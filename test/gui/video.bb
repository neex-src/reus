;; bb gui, tested the way a person uses it, and filmed (Linux; the host has no Xvfb, so Xvnc):
;;
;;   bb test/gui/video.bb prepare [CLAUDE-PROJECT-DIR-NAME... -- COPILOT-SESSION-ID...]
;;        copies a few Claude projects (their transcripts and file backups) and Copilot sessions of this machine
;;        into .scratch/gui-video/home, and the Claude settings without the reus hooks (trace.install/off on the copy)
;;   bb test/gui/video.bb copy      (in a worktree: it starts by removing that worktree's var/replay and var/copilot-replay)
;;                                  the whole flow on the copy: bb gui typed into a terminal, the found list, build from
;;                                  the old Claude and Copilot logs, a replayed session opened in the viewer and back,
;;                                  the hooks turned on and off (the copied settings.json and Copilot home)
;;   bb test/gui/video.bb real      the machine's own ~/.claude and ~/.copilot, LOOKING only: nothing is pressed that
;;                                  writes; checks that the real settings and hook files are byte for byte the same after
;;
;; Its own X display (Xvnc, in a tmux window, its /tmp bound to .scratch by bwrap so the X lock file lands there; no
;; TCP, no file socket, VNC only on a unix socket in .scratch; i3 as the window manager, every window floating where
;; it asks to be, focus follows the pointer), a real headful Chromium on it, the input real X events
;; (xdotool: mouse moves, clicks, wheel, keys), the whole display recorded by ffmpeg x11grab into
;; .scratch/evidence/gui-<run>-<time>.mp4, with a log of every step next to it. Chromium's DevTools port is used only to
;; LOOK (where is the button, what does the page say now), never to click or type.
;; Everything it writes is under <reus>/.scratch.
(require '[babashka.fs :as fs]
         '[babashka.http-client.websocket :as ws]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def root (str (fs/canonicalize (fs/parent (fs/parent (fs/parent *file*))))))
(def work (str (fs/path root ".scratch" "gui-video")))
(def home (str (fs/path work "home")))
(def evidence (str (fs/path root ".scratch" "evidence")))
(def W 1920) (def H 1080) (def browser-h 850)

(def stamp (.format (java.time.LocalDateTime/now) (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")))
(def ^:dynamic *log* nil)
(def t-start (atom (System/currentTimeMillis)))

(defn log [& xs]
  (let [l (format "%7.1fs  %s" (/ (- (System/currentTimeMillis) @t-start) 1000.0) (str/join " " xs))]
    (println l)
    (when *log* (spit *log* (str l "\n") :append true))))

(def failures (atom []))
(defn check [ok? what] (log (if ok? "ok  " "FAIL") what) (when-not ok? (swap! failures conj what)) ok?)

(defn sleep [ms] (Thread/sleep (long ms)))

(defn wait-for
  "Poll f every 250 ms until it is truthy, at most ms; its value or nil."
  [ms f]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop [] (or (try (f) (catch Exception _ nil))
                 (when (< (System/currentTimeMillis) end) (sleep 250) (recur))))))

;; --- the display --------------------------------------------------------------------------------

(defn free-display
  "A display number nobody uses: no lock, no socket (a file one or an abstract one)."
  []
  (let [socks (:out (p/shell {:out :string} "cat" "/proc/net/unix"))]
    (first (remove #(or (fs/exists? (str "/tmp/.X" % "-lock")) (fs/exists? (str "/tmp/.X11-unix/X" %))
                        (str/includes? socks (str "/tmp/.X11-unix/X" % "\n")))
                   (range 70 99)))))

(defn start-x!
  "Xvnc on :n in a tmux window; {:n :env :stop}."
  [n]
  (let [dir (str (fs/path work "x" (str n)))
        xtmp (str (fs/path dir "tmp"))
        auth (str (fs/path dir "xauth"))
        cookie (apply str (map #(format "%02x" (bit-and % 0xff)) (repeatedly 16 #(rand-int 256))))]
    (fs/delete-tree dir)
    (fs/create-dirs xtmp)
    (p/shell {:out :string :err :string} "xauth" "-f" auth "add" (str ":" n) "." cookie)
    (let [cmd (str "exec bwrap --dev-bind / / --bind " xtmp " /tmp --die-with-parent -- "
                   "Xvnc :" n " -geometry " W "x" H " -depth 24 -rfbport -1 -rfbunixpath " dir "/vnc.sock"
                   " -SecurityTypes None -nolisten tcp -nolisten unix -auth " auth " > " dir "/xvnc.log 2>&1")
          win (str "reus-x" n)
          env {"DISPLAY" (str ":" n) "XAUTHORITY" auth}]
      (p/shell "tmux" "new-window" "-d" "-n" win cmd)
      (check (wait-for 10000 #(zero? (:exit (p/shell {:extra-env env :out :string :err :string :continue true} "xdotool" "getdisplaygeometry"))))
             (str "X display :" n " is up (tmux window " win ", vnc socket " dir "/vnc.sock)"))
      (spit (str (fs/path dir "i3.conf"))
            (str "# i3 config file (v4)\nfont pango:monospace 9\ndefault_border none\ndefault_floating_border none\n"
                 "focus_follows_mouse yes\nfor_window [class=\".*\"] floating enable\n"
                 "for_window [class=\"(?i)chromium\"] move position 0 px 0 px\nipc-socket " dir "/i3.sock\n"))
      (fs/create-dirs (fs/path dir "run"))
      (let [wm (p/process {:extra-env (merge env {"TMPDIR" xtmp "XDG_RUNTIME_DIR" (str (fs/path dir "run"))})
                           :out (str (fs/path dir "i3.log")) :err (str (fs/path dir "i3.err"))}
                          "i3" "-c" (str (fs/path dir "i3.conf")))]
        (sleep 1000)
        {:n n :env env :dir dir
         :stop (fn []
                 (p/destroy-tree wm)
                 (let [pid (str/trim (:out (p/shell {:out :string :continue true} "tmux" "list-panes" "-t" win "-F" "#{pane_pid}")))]
                   (when (seq pid) (p/shell {:continue true} "kill" "-TERM" pid))
                   (sleep 800)
                   (p/shell {:continue true :err :string} "tmux" "kill-window" "-t" win)))}))))

(defn xdo [x & args] (apply p/shell {:extra-env (:env x) :out :string :err :string :continue true} "xdotool" (map str args)))

(defn record!
  "ffmpeg x11grab of the whole display into file; the stop fn."
  [x file]
  (fs/create-dirs (fs/parent file))
  (let [pr (p/process {:extra-env (:env x) :in :pipe :out :string :err (str file ".ffmpeg.log")}
                      "ffmpeg" "-y" "-loglevel" "warning" "-f" "x11grab" "-draw_mouse" "1" "-framerate" "15"
                      "-video_size" (str W "x" H) "-i" (str ":" (:n x))
                      "-c:v" "libx264" "-preset" "veryfast" "-crf" "25" "-pix_fmt" "yuv420p" (str file))]
    (fn []
      (let [in (:in pr)] (.write in (.getBytes "q")) (.flush in) (.close in))
      (deref pr 20000 nil)
      (log "video" file (str (quot (or (some-> (fs/file file) .length) 0) 1024) " KB")))))

;; --- looking at the page (DevTools, read only) ---------------------------------------------------

(def cdp-port 9377)

(defn- page-ws []
  (some #(when (= "page" (get % "type")) (get % "webSocketDebuggerUrl"))
        (json/parse-string (slurp (str "http://127.0.0.1:" cdp-port "/json")))))

(def ^:private conn (atom nil))
(def ^:private replies (atom {}))
(def ^:private ids (atom 0))

(defn- connect! []
  (when-let [c @conn] (try (ws/close! c) (catch Exception _)))
  (let [buf (StringBuilder.)]
    (reset! conn (ws/websocket {:uri (page-ws)
                                :on-message (fn [_ data last?]
                                              (.append buf (str data))
                                              (when last?
                                                (let [m (try (json/parse-string (str buf)) (catch Exception _ {}))]
                                                  (.setLength buf 0)
                                                  (when-let [id (get m "id")] (deliver (get @replies id (promise)) m)))))}))))

(defn js
  "The value of a JS expression in the page (never an input: only reads)."
  [expr]
  (let [id (swap! ids inc) pr (promise)]
    (swap! replies assoc id pr)
    (try
      (ws/send! @conn (json/generate-string {:id id :method "Runtime.evaluate"
                                             :params {:expression expr :returnByValue true :awaitPromise true}}))
      (let [r (deref pr 8000 nil)]
        (get-in r ["result" "result" "value"]))
      (catch Exception _ (connect!) nil)
      (finally (swap! replies dissoc id)))))

(def helpers
  "window.__t = (sel, text) => [...document.querySelectorAll(sel)].find(e => e.offsetParent !== null && e.textContent.trim().startsWith(text));
   window.__mouse = window.__mouse || {x: -1, y: -1};
   if (!window.__hooked) { window.__hooked = 1; addEventListener('mousemove', e => { window.__mouse = {x: e.clientX, y: e.clientY, sx: e.screenX, sy: e.screenY}; }, true); }
   true")

(defn ready! [] (wait-for 15000 #(and (= "complete" (js "document.readyState")) (js helpers))))

;; --- real input -------------------------------------------------------------------------------

(def offset (atom nil))
(def pos (atom [960 500]))

(defn move!
  "Move the real pointer to screen x,y in small steps (it is seen moving in the video)."
  [x [tx ty]]
  (let [[fx fy] @pos n 18]
    (doseq [i (range 1 (inc n))]
      (let [t (/ i n) e (- 1 (* (- 1 t) (- 1 t)))]
        (xdo x "mousemove" (Math/round (double (+ fx (* e (- tx fx))))) (Math/round (double (+ fy (* e (- ty fy))))))
        (sleep 16)))
    (reset! pos [tx ty])))

(defn calibrate!
  "Where the page's (0,0) is on the screen: move the pointer over the page and read the client position it got."
  [x]
  (js helpers)
  (move! x [790 490])
  (move! x [800 500])
  (sleep 200)
  (when-let [m (wait-for 3000 #(let [m (js "window.__mouse")] (when (>= (get m "x" -1) 0) m)))]
    (reset! offset [(- 800 (get m "x")) (- 500 (get m "y"))])
    (log "page origin on screen" @offset)))

(defn rect
  "Center and edges of the element the JS expression finds, in client coordinates; nil when none."
  [el-js]
  (js (str "(() => { const el = " el-js "; if (!el) return null; const r = el.getBoundingClientRect();"
           " return {x: r.left + r.width / 2, y: r.top + r.height / 2, top: r.top, bottom: r.bottom, vh: innerHeight, sy: scrollY}; })()")))

(defn wheel!
  "Real wheel clicks over the page: positive down."
  [x n]
  (when-not @offset (calibrate! x))
  (dotimes [_ (Math/abs (long n))]
    (xdo x "click" (if (pos? n) 5 4))
    (sleep 45)))

(defn into-view!
  "Wheel until the element is well inside the window (or the page scrolls no further)."
  [x el-js]
  (loop [k 0 last-y nil]
    (when-let [r (rect el-js)]
      (let [{:strs [top bottom vh sy]} r
            tall? (> (- bottom top) (* 0.6 vh))
            steps #(min 6 (max 1 (long (/ % 100))))]
        (cond (or (> k 60) (= sy last-y)) r
              (if tall? (> top (* 0.5 vh)) (> bottom (- vh 60)))
              (do (wheel! x (steps (- (if tall? top bottom) (if tall? (* 0.25 vh) (* 0.55 vh))))) (sleep 300) (recur (inc k) sy))
              (< top 70) (do (wheel! x (- (steps (- (* 0.3 vh) top)))) (sleep 300) (recur (inc k) sy))
              :else r)))))

(defn click!
  "Find, scroll to, move to and click (a real button press) the element; what it said."
  [x what el-js & {:keys [pause] :or {pause 500}}]
  (when-not @offset (calibrate! x))
  (if-let [r (into-view! x el-js)]
    (let [[ox oy] @offset
          r (or (rect el-js) r)]
      (move! x [(+ ox (Math/round (double (get r "x")))) (+ oy (Math/round (double (get r "y"))))])
      (sleep 250)
      (xdo x "click" 1)
      (log "click" what)
      (sleep pause)
      true)
    (check false (str "found on the page: " what))))

(defn type!
  "Real key presses: text, then keys."
  [x text & keys]
  (when (seq text) (xdo x "type" "--delay" "70" text))
  (doseq [k keys] (xdo x "key" k) (sleep 100)))

(defn page-text [] (js "document.body.innerText"))

(defn saw [what f & {:keys [ms] :or {ms 15000}}]
  (check (wait-for ms f) what))

;; --- the parts ----------------------------------------------------------------------------------

(defn browser-cmd [profile]
  (str "chromium --user-data-dir=" profile " --remote-debugging-port=" cdp-port
       " --window-position=0,0 --window-size=" W "," browser-h " --force-device-scale-factor=1"
       " --no-first-run --no-default-browser-check --disable-sync --password-store=basic --disable-gpu"
       " --disable-features=Translate,MediaRouter,OptimizationHints --hide-crash-restore-bubble --disable-session-crashed-bubble"))

(defn terminal!
  "An xterm at the bottom of the display, a plain bash in the reus checkout with env; the process."
  [x env]
  (let [tmp (str (fs/path work "tmp"))]
    (fs/create-dirs tmp)
    (p/process {:extra-env (merge (:env x) env {"TMPDIR" tmp "PS1" "\\[\\e[1;32m\\]reus\\[\\e[0m\\] $ "})
                :dir root :out :discard :err :discard}
               "xterm" "-geometry" (str "236x13+0+" (+ browser-h 2)) "-fa" "Monospace" "-fs" "10"
               "-bg" "#15171a" "-fg" "#e8e6e1" "-T" "terminal"
               "-e" "bash" "--norc" "--noprofile")))

(defn- focused [x] (str/trim (str (:out (xdo x "getwindowfocus" "getwindowname")))))

(defn in-terminal!
  "Type a command into the terminal (the pointer goes there first: the window under it has the keyboard)."
  [x cmd]
  (move! x [700 (+ browser-h 120)])
  (when-not (wait-for 5000 #(= "terminal" (focused x)))
    (move! x [710 (+ browser-h 110)]))
  (check (wait-for 5000 #(= "terminal" (focused x))) "the terminal has the keyboard (the pointer is over it)")
  (sleep 300)
  (type! x cmd "Return"))

(defn- digest [f] (when (fs/exists? f) (str (fs/size f) ":" (hash (slurp (str f))))))

;; --- prepare: a copy of a few projects and sessions ------------------------------------------------

(def default-claude ["-home-user-projects-feed" "-home-user-projects-repls" "-home-user-projects-reus-sane"])
(def default-copilot [[(str (fs/path (fs/home) ".copilot")) "fb2b78b4-af0d-45ac-bfc8-363fe33fa98d"]
                      [(str (fs/path (fs/home) "projects" "copilot-lab" ".copilot-home")) "62198236-f91e-45fa-b9a2-c61699432f6d"]
                      [(str (fs/path (fs/home) "projects" "copilot-lab" ".copilot-home")) "6d44b492-e2e8-445c-8f02-691c9191c9dd"]])

(defn prepare! [claude-dirs copilot-ids]
  (let [real (str (fs/path (fs/home) ".claude"))
        claude (str (fs/path home "claude"))
        copilot (str (fs/path home "copilot"))]
    (fs/delete-tree home)
    (doseq [d claude-dirs]
      (fs/copy-tree (fs/path real "projects" d) (fs/path claude "projects" d))
      (doseq [f (fs/glob (fs/path real "projects" d) "*.jsonl")
              :let [sid (str/replace (str (fs/file-name f)) #"\.jsonl$" "")
                    hist (fs/path real "file-history" sid)]
              :when (fs/directory? hist)]
        (fs/copy-tree hist (fs/path claude "file-history" sid))))
    (fs/copy (fs/path real "settings.json") (fs/path claude "settings.json"))
    (p/shell {:dir root :extra-env {"CLAUDE_CONFIG_DIR" claude}} "bb" "-e" "(require 'trace.install) (trace.install/off)")
    (doseq [[h id] copilot-ids]
      (fs/copy-tree (fs/path h "session-state" id) (fs/path copilot "session-state" id)))
    (fs/create-dirs (fs/path copilot "hooks"))
    (println "copied into" home ":" (count claude-dirs) "Claude projects," (count copilot-ids) "Copilot sessions")))

;; --- the runs -----------------------------------------------------------------------------------

(defn with-stage
  "Display, recording, terminal, then (f x); everything stopped after, the video path returned."
  [run env f]
  (let [file (str (fs/path evidence (str "gui-" run "-" stamp ".mp4")))]
    (fs/create-dirs evidence)
    (binding [*log* (str file ".log")]
      (reset! t-start (System/currentTimeMillis))
      (log "run" run "->" file)
      (let [x (start-x! (free-display))
            stop-rec (record! x file)
            profile (str (fs/path work "chrome-" run))
            _ (fs/delete-tree profile)
            term (terminal! x (merge env {"BROWSER" (browser-cmd profile)}))]
        (try
          (check (wait-for 15000 #(seq (str/trim (str (:out (xdo x "search" "--name" "^terminal$")))))) "the terminal window is up")
          (sleep 2000)
          (f x)
          (catch Throwable e (check false (str "the run went on: " (ex-message e))))
          (finally
            (sleep 1500)
            (stop-rec)
            (try (some-> @conn ws/close!) (catch Exception _))
            (p/destroy-tree term)
            (sleep 300)
            ((:stop x))))
        (log (if (empty? @failures) "PASSED" (str "FAILED: " (count @failures))) "-" file)
        file))))

(defn start-gui! [x]
  (in-terminal! x "bb gui")
  (saw "bb gui opened the browser on its page" #(and (page-ws) (do (connect!) true)) :ms 30000)
  (ready!)
  (saw "the page says what it found" #(str/includes? (str (page-text)) "Found "))
  (calibrate! x))

(defn found-line [] (js "document.querySelector('.found').innerText"))

(defn- branches?
  "Was repo built in this run (after it started), with a session branch for each of projects (any, for nil)?"
  [repo projects]
  (let [refs (str/split-lines (:out (p/shell {:dir (str repo) :out :string :err :string :continue true}
                                             "git" "for-each-ref" "--format=%(refname:short)" "refs/heads")))
        built (some-> (fs/path repo ".git" "HEAD") fs/last-modified-time .toMillis)]
    (and built (> built @t-start)
         (if projects
           (every? (fn [pr] (some #(str/starts-with? % (str pr "/s/")) refs)) projects)
           (some #(re-find #"/s/" %) refs)))))

(defn copy-run [x]
  (let [claude (str (fs/path home "claude"))
        copilot (str (fs/path home "copilot"))
        settings (str (fs/path claude "settings.json"))
        before (slurp settings)]
    (start-gui! x)
    (log "found:" (found-line))
    (check (str/includes? (found-line) "Hooks: Claude not installed, Copilot not installed") "hooks off at the start")
    (sleep 2500)
    ;; a look at what was found
    (move! x [300 260]) (sleep 800) (move! x [960 260]) (sleep 800) (move! x [1600 260]) (sleep 800)
    ;; 1. build from the old Claude logs
    (click! x "Build Claude traces" "__t('button', 'Build Claude traces')" :pause 900)
    (click! x "Yes, build (Claude)" "document.querySelector('[data-build-go=claude]')")
    (saw "the progress shows" #(not (js "document.getElementById('job').hidden")) :ms 5000)
    (let [seen (atom #{})]
      (saw "the Claude build finished" #(let [t (str (js "document.getElementById('job-text') && document.getElementById('job-text').innerText"))]
                                          (swap! seen conj (first (str/split t #" \d")))
                                          (and (re-find #"recorded" t) (not (js "document.getElementById('job').dataset.state === 'recording'"))))
           :ms 240000)
      (log "progress said:" (str/join " | " (remove str/blank? @seen))))
    (wait-for 10000 #(do (ready!) (str/includes? (str (js "location.hash")) "browse")))
    (ready!) (calibrate! x)
    (log "job:" (js "document.getElementById('job-text').innerText"))
    (check (branches? (fs/path root "var" "replay") nil) "var/replay was built now, with session branches")
    (sleep 2000)
    ;; 1b. build from the old Copilot logs
    (click! x "Build Copilot traces" "__t('button', 'Build Copilot traces')" :pause 900)
    (click! x "Yes, build (Copilot)" "document.querySelector('[data-build-go=copilot]')")
    (saw "the Copilot build finished" #(re-find #"recorded" (str (js "document.getElementById('job-text').innerText"))) :ms 240000)
    (wait-for 10000 #(do (ready!) (str/includes? (str (js "location.hash")) "browse")))
    (ready!) (calibrate! x)
    (log "job:" (js "document.getElementById('job-text').innerText"))
    (check (branches? (fs/path root "var" "copilot-replay") nil) "var/copilot-replay was built now, with session branches")
    (sleep 1500)
    ;; 2. open a replayed Claude session in the viewer
    (into-view! x "document.getElementById('repo-claude-replay')")
    (sleep 1200)
    (click! x "a session list of the Claude replay" "document.querySelector('#repo-claude-replay tr.sessions summary')" :pause 1200)
    (click! x "a replayed Claude session" "document.querySelector('#repo-claude-replay ul.sess a')" :pause 1500)
    (saw "the viewer opened the session" #(do (ready!) (and (str/includes? (str (js "location.href")) "/p/")
                                                             (js "!!document.querySelector('.step')"))))
    (calibrate! x)
    (log "viewer:" (js "location.href") "-" (js "document.title"))
    (sleep 1500)
    (click! x "a tool call of the session" "document.querySelector('.step summary')" :pause 1500)
    (wheel! x 6) (sleep 1200) (wheel! x -6) (sleep 600)
    (click! x "back to the dashboard" "document.querySelector('a.dash')" :pause 1200)
    (saw "back on the dashboard" #(do (ready!) (str/includes? (str (page-text)) "Build traces from the old logs")))
    (calibrate! x)
    ;; 2b. a replayed Copilot session
    (click! x "a session list of the Copilot replay" "document.querySelector('#repo-copilot-replay tr.sessions summary')" :pause 1200)
    (click! x "a replayed Copilot session" "document.querySelector('#repo-copilot-replay ul.sess a')" :pause 1500)
    (saw "the viewer opened the Copilot session" #(do (ready!) (and (str/includes? (str (js "location.href")) "/p/")
                                                                     (js "!!document.querySelector('.step')"))))
    (calibrate! x)
    (log "viewer:" (js "location.href") "-" (js "document.title"))
    (sleep 1200)
    (wheel! x 5) (sleep 1000)
    (click! x "back to the dashboard" "document.querySelector('a.dash')" :pause 1200)
    (saw "back on the dashboard again" #(do (ready!) (str/includes? (str (page-text)) "Record from now on")))
    (calibrate! x)
    ;; 3. the hooks: on, then off
    (click! x "What on writes (Claude)" "__t('#hooks summary', 'What on writes')" :pause 1500)
    (click! x "Turn on the Claude hook" "__t('#hooks button', 'Turn on the Claude hook')" :pause 900)
    (click! x "Yes, write it" "[...document.querySelectorAll('[data-hook-go]')].find(e => e.offsetParent)" :pause 2500)
    (saw "the page came back with the Claude hook on" #(do (ready!) (str/includes? (str (found-line)) "Hooks: Claude on")))
    (calibrate! x)
    (check (str/includes? (slurp settings) "-m trace.hook") "the copied settings.json has the hook")
    (check (str/includes? (slurp settings) (str "--config '" root "/bb.edn'")) "pointing at this checkout")
    (sleep 1500)
    (click! x "Turn on the Copilot hooks" "__t('#hooks button', 'Turn on the Copilot hooks')" :pause 900)
    (click! x "Yes, write it" "[...document.querySelectorAll('[data-hook-go]')].find(e => e.offsetParent)" :pause 2500)
    (saw "the page came back with the Copilot hooks on" #(do (ready!) (str/includes? (str (found-line)) "Copilot on")))
    (calibrate! x)
    (check (fs/exists? (fs/path copilot "hooks" "reus.json")) "the copied Copilot home has hooks/reus.json")
    (sleep 1500)
    (click! x "Turn off the Claude hook" "__t('#hooks button', 'Turn off the Claude hook')" :pause 900)
    (click! x "Yes, write it" "[...document.querySelectorAll('[data-hook-go]')].find(e => e.offsetParent)" :pause 2500)
    (saw "the Claude hook is off again" #(do (ready!) (str/includes? (str (found-line)) "Hooks: Claude not installed")))
    (calibrate! x)
    (click! x "Turn off the Copilot hooks" "__t('#hooks button', 'Turn off the Copilot hooks')" :pause 900)
    (click! x "Yes, write it" "[...document.querySelectorAll('[data-hook-go]')].find(e => e.offsetParent)" :pause 2500)
    (saw "the Copilot hooks are off again" #(do (ready!) (str/includes? (str (found-line)) "Copilot not installed")))
    (calibrate! x)
    (check (not (str/includes? (slurp settings) "trace.hook")) "the copied settings.json has no reus hook again")
    (check (= (json/parse-string before) (json/parse-string (slurp settings))) "and is the settings it was before (same JSON)")
    (check (not (fs/exists? (fs/path copilot "hooks" "reus.json"))) "hooks/reus.json is gone again")
    (sleep 1500)
    (into-view! x "document.getElementById('running')")
    (sleep 2500)
    (in-terminal! x "")
    (type! x "" "ctrl+c")
    (sleep 1200)))

(def real-files
  (let [c (str (fs/path (fs/home) ".claude")) cp (str (fs/path (fs/home) ".copilot"))]
    [(str (fs/path c "settings.json")) (str (fs/path cp "hooks" "reus.json"))
     (str (fs/path cp "skills" "reus-compare" "SKILL.md"))]))

(defn real-run [x]
  (let [before (mapv digest real-files)
        var-before (set (map str (fs/list-dir (fs/path root "var"))))]
    (start-gui! x)
    (log "found:" (found-line))
    (sleep 3000)
    (move! x [300 260]) (sleep 900) (move! x [960 260]) (sleep 900) (move! x [1600 260]) (sleep 900)
    (into-view! x "document.querySelector('#build table.pick')")
    (sleep 1500)
    (click! x "the project filter" "document.querySelector('.filter')" :pause 400)
    (type! x "reus")
    (sleep 1800)
    (type! x "" "ctrl+a" "BackSpace")
    (sleep 800)
    (into-view! x "document.getElementById('browse')") (sleep 2000)
    (when (js "!!document.querySelector('#repo-live tr.sessions summary')")
      (click! x "the live trace's session list" "document.querySelector('#repo-live tr.sessions summary')" :pause 2500))
    (into-view! x "document.getElementById('hooks')") (sleep 2500)
    (click! x "What on writes (Claude), only to read it" "__t('#hooks summary', 'What on writes')" :pause 2500)
    (into-view! x "document.getElementById('running')") (sleep 3000)
    (in-terminal! x "")
    (type! x "" "ctrl+c")
    (sleep 1000)
    (check (= before (mapv digest real-files)) (str "the real settings and hook files are as they were: " (str/join ", " (map #(str/replace % (str (fs/home)) "~") real-files))))
    (check (= var-before (set (map str (fs/list-dir (fs/path root "var"))))) "nothing new in var/")))

(let [[cmd & args] *command-line-args*]
  (case cmd
    "prepare" (let [[c [_ & cp]] (split-with #(not= "--" %) args)]
                (prepare! (or (seq c) default-claude)
                          (if (seq cp) (map #(vector (str (fs/path (fs/home) ".copilot")) %) cp) default-copilot)))
    "copy" (do (when (fs/directory? (fs/path root ".git"))
                 (println "run it from a worktree of reus: the copy run builds over" (str root "/var/replay") "and var/copilot-replay")
                 (System/exit 2))
               (doseq [d ["replay" "copilot-replay"]] (fs/delete-tree (fs/path root "var" d))) ; this worktree's own, from an earlier run
               (with-stage "copy" {"CLAUDE_CONFIG_DIR" (str (fs/path home "claude")) "COPILOT_HOME" (str (fs/path home "copilot"))} copy-run)
               (System/exit (if (empty? @failures) 0 1)))
    "real" (do (with-stage "real" {"TRACE_REPO" (str (fs/path (fs/home) "projects" "reus" "var" "traces"))} real-run)
               (System/exit (if (empty? @failures) 0 1)))
    (println "usage: bb test/gui/video.bb prepare [claude-dir... -- copilot-id...] | copy | real")))
