(ns trace.page
  "The look of the pages meant for people who do not read traces (/call, /calls, /mirrors, /mirror/...): one
  inline stylesheet, one shell with the top bar, a few formatters. Self-contained on purpose: no script, no
  font or library from elsewhere (these pages are filmed in a box with no business reaching the internet), and
  nothing scrolls inside a box: a pre is as tall as its text, a long text is cut in the markup and the rest sits
  behind a <details>. Never give a block here a max-height or an overflow other than visible: a box that is
  shorter than its text paints the text over whatever comes next (that was the mirror view's bug)."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]))

(defn thou
  "n with thousands separators, an en dash for nil."
  [n]
  (if n (str/replace (str n) #"\B(?=(\d{3})+(?!\d))" ",") "–"))

(defn clip
  "s on one line, at most n characters, an ellipsis when it was longer."
  [s n]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(def css
  "body{margin:0;font:16px/1.5 system-ui,-apple-system,'Segoe UI',sans-serif;color:#1b1f24;background:#f6f7f9}
header.top{background:#1b1f24;color:#fff;padding:10px 24px;display:flex;flex-wrap:wrap;gap:4px 20px;align-items:baseline}
header.top a{color:#cfd6df;text-decoration:none}header.top a.brand{color:#fff;font-weight:600}
header.top a.on{color:#fff;border-bottom:2px solid #fff}
main{max-width:60rem;margin:0 auto;padding:24px 24px 64px}
main.wide{max-width:84rem}
h1{font-size:1.5rem;margin:.2em 0 .1em}h2{font-size:1.1rem;margin:2em 0 .4em;border-bottom:1px solid #d5dae1;padding-bottom:.2em}
h3{font-size:1.05rem;margin:0}
.lede{font-size:1.15rem;margin:.4em 0 1em}
.nums{display:flex;flex-wrap:wrap;gap:12px;margin:12px 0}
.num{background:#fff;border:1px solid #d5dae1;border-radius:8px;padding:10px 16px;min-width:8.5rem}
.num b{display:block;font-size:1.6rem;line-height:1.2}.num span{color:#59636e;font-size:.85rem}
table{border-collapse:collapse;width:100%;background:#fff;border:1px solid #d5dae1}
th,td{text-align:left;padding:6px 10px;border-bottom:1px solid #e6e9ee;vertical-align:top;overflow-wrap:break-word}th{background:#eef1f5;font-weight:600}
td.n,th.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#fff;border:1px solid #d5dae1;border-radius:6px;padding:10px 12px;margin:.4em 0;font:13px/1.45 ui-monospace,Menlo,Consolas,monospace}
.quote{background:#fff;border-left:4px solid #3b6fd4;padding:8px 14px;margin:.4em 0;white-space:pre-wrap;overflow-wrap:anywhere}
.muted{color:#59636e}.ok{color:#17803d}.bad{color:#b42318}
ol.steps{margin:.3em 0;padding-left:1.4em}ol.steps li{margin:.15em 0}
code{font:13px ui-monospace,Menlo,Consolas,monospace;overflow-wrap:anywhere}
details summary{cursor:pointer;color:#3b6fd4}
a{color:#3b6fd4}
.src{color:#59636e;font-size:.85rem;margin-top:2.5em}
.crumb{color:#59636e;font-size:.9rem;margin:0}
.sess{font:12px ui-monospace,Menlo,Consolas,monospace;color:#59636e}
.badge{font-size:.72rem;padding:1px 7px;border-radius:9px;background:#eef1f5;color:#59636e;margin-left:6px}
.state{font-size:.7rem;font-weight:700;padding:1px 6px;border-radius:4px;letter-spacing:.03em;color:#fff;white-space:nowrap}
.state.bad{background:#b42318}.state.warn{background:#b7791f}.state.good{background:#e6f4ea;color:#17803d}
.legend{color:#59636e;font-size:.9rem;margin:.2em 0 1em;max-width:60rem}
table.list th,table.list td{padding:6px 8px}
table.list thead th{font-size:.8rem;color:#2f3a46;white-space:normal;vertical-align:bottom}
table.list thead tr.groups th{text-align:center;border-bottom:1px solid #c5ccd6;color:#59636e;font-weight:600}
table.list thead th a{text-decoration:none;color:inherit}table.list thead th a:hover{text-decoration:underline}
table.list td{vertical-align:middle}table.list td.n,table.list td.when{white-space:nowrap}
table.list tbody tr:hover td{background:#f3f6fa}
.mono{font:12px ui-monospace,Menlo,Consolas,monospace}
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(14rem,1fr));gap:12px;margin:12px 0}
.tile{background:#fff;border:1px solid #d5dae1;border-radius:8px;padding:10px 16px}
.tile .lab{display:block;color:#59636e;font-size:.8rem;text-transform:uppercase;letter-spacing:.04em}
.tile .two{display:flex;flex-wrap:wrap;gap:4px 22px;margin-top:2px}
.tile .two b{display:block;font-size:1.35rem;line-height:1.25;font-variant-numeric:tabular-nums}
.tile .two span,.tile .note{color:#59636e;font-size:.85rem}
.tile .note{display:block;margin-top:2px}
.card{background:#fff;border:1px solid #d5dae1;border-radius:8px;padding:14px 18px;margin:16px 0}
.card h3{display:flex;flex-wrap:wrap;gap:2px 14px;align-items:baseline}
.card h3 a{text-decoration:none;color:#1b1f24}
.card .verdict{margin:.3em 0 .5em}
.said{margin:.4em 0;padding:6px 12px;border-left:4px solid #c5ccd6;background:#f6f7f9;white-space:pre-wrap;overflow-wrap:anywhere}
table.sides{table-layout:fixed;margin:.6em 0}
table.sides.cmp{max-width:64rem}
table.sides th:first-child{width:12.5rem;background:#fff;font-weight:500;color:#2f3a46}
table.sides thead th{background:#eef1f5;font-weight:600}
table.sides td,table.sides tbody th{font-variant-numeric:tabular-nums;overflow-wrap:anywhere}
table.sides .hint{display:block;color:#59636e;font-size:.78rem;font-weight:400}
table.sides td.vs{color:#59636e}
table.sides td.empty{color:#8a94a3}
.chips{display:flex;flex-wrap:wrap;gap:3px 6px}
.chip{font:12px ui-monospace,Menlo,Consolas,monospace;background:#eef1f5;border-radius:4px;padding:0 6px;white-space:nowrap}
.line{display:flex;flex-wrap:wrap;gap:6px 14px;align-items:baseline;margin:.5em 0}
details.fold{margin:.5em 0}
details.fold>summary{padding:2px 0}
details.fold[open]>summary{font-weight:600}
details.call{border:1px solid #d5dae1;border-left:4px solid #c5ccd6;border-radius:6px;margin:6px 0;background:#fff}
details.call.nod{border-left-color:#7a4fb5}
details.call.failed{border-color:#b42318;border-left-color:#b42318;background:#fdf3f1}
details.call.refused{border-left-color:#b7791f}
details.call.sub{margin-left:18px}
details.call>summary{display:flex;flex-wrap:wrap;gap:2px 10px;align-items:baseline;padding:6px 10px;color:#1b1f24;font-size:.88rem}
details.call .i{font:600 12px ui-monospace,Menlo,Consolas,monospace;color:#59636e;min-width:1.8rem}
details.call .tool{font:600 12px ui-monospace,Menlo,Consolas,monospace;color:#59636e;white-space:nowrap}
details.call .lbl{font:12px ui-monospace,Menlo,Consolas,monospace;flex:1 1 16rem;min-width:0;overflow-wrap:anywhere}
details.call .meta{font:11px ui-monospace,Menlo,Consolas,monospace;color:#59636e;white-space:nowrap;margin-left:auto}
.call-body{padding:2px 12px 12px}
.call-body .tag{font:600 11px ui-monospace,Menlo,Consolas,monospace;text-transform:uppercase;color:#59636e;margin-top:8px}
.call-body .tag.err{color:#b42318}.call-body .tag.add{color:#17803d}.call-body .tag.del{color:#b42318}
.call-body pre.err,.call-body pre.outerr{border-color:#b42318}
.call-body table.kv{width:auto;border:0;background:none}.kv th{background:none;border:0;padding:1px 12px 1px 0;color:#59636e;font:12px ui-monospace,Menlo,Consolas,monospace}
.kv td{border:0;padding:1px 0;font:12px ui-monospace,Menlo,Consolas,monospace}
.cols2{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:18px;align-items:start}
.cols2 h2{margin-top:.8em}
@media (max-width:1100px){.cols2{grid-template-columns:minmax(0,1fr)}}
ul.plain{margin:.3em 0;padding-left:1.3em}ul.plain li{margin:.2em 0;overflow-wrap:anywhere}
.reason{color:#59636e}
.filters{display:flex;flex-wrap:wrap;gap:6px 16px;margin:12px 0}.filters b{border-bottom:2px solid #1b1f24}")

(defn shell
  "A whole page: title, the top bar (current: which link is lit, :prompts :mirrors :calls :runs; top: hiccup that
  replaces the whole top bar, for pages that are files and have no server to link to), body. wide: more room for tables."
  [title body & {:keys [current wide top extra-css]}]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head [:meta {:charset "utf-8"}] [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title] [:style (h/raw (str css extra-css))]]
         [:body
          (or top
              [:header.top
               [:a.brand {:href "/"} "reus trace"]
               (for [[k href t] [[:prompts "/" "Prompts"] [:mirrors "/mirrors" "Mirrors"] [:calls "/calls" "Model calls"] [:runs "/runs" "Runs"]]]
                 [:a (cond-> {:href href} (= k current) (assoc :class "on")) t])])
          [:main (when wide {:class "wide"}) body]]])))
