// trace view: lazy sections, highlighting, markdown, filters, live poll.
(function () {
  const $$ = (sel, root) => Array.from((root || document).querySelectorAll(sel));
  const store = {
    get(k) { try { return localStorage.getItem(k); } catch (e) { return null; } },
    set(k, v) { try { localStorage.setItem(k, v); } catch (e) {} },
  };
  const stripAnsi = (s) => s.replace(/\x1b\[[0-9;?]*[A-Za-z]/g, '');

  function highlight(root) {
    if (!window.hljs) return;
    $$('code[data-hl]', root).forEach((el) => {
      if (el.dataset.done) return;
      el.dataset.done = '1';
      if (el.textContent.length > 300000) return;
      try { hljs.highlightElement(el); } catch (e) {}
    });
  }

  function diffView(text) {
    const box = document.createElement('div');
    sandbox.className = 'diff';
    const lines = text.replace(/\n$/, '').split('\n');
    if (!text.trim()) { box.innerHTML = '<div class="dl">(no changes)</div>'; return box; }
    for (const l of lines) {
      if (/^(index |--- |\+\+\+ |new file mode|deleted file mode|similarity|rename )/.test(l)) continue;
      const d = document.createElement('div');
      let cls = 'dl';
      let t = l;
      const m = l.match(/^diff --git a\/(.+?) b\/(.+)$/);
      if (m) { cls += ' f'; t = m[2]; }
      else if (l.startsWith('@@')) cls += ' h';
      else if (l.startsWith('+')) cls += ' a';
      else if (l.startsWith('-')) cls += ' d';
      d.className = cls;
      d.textContent = t;
      sandbox.appendChild(d);
    }
    return box;
  }

  function codeView(text, lang) {
    const pre = document.createElement('pre');
    pre.className = 'code';
    const code = document.createElement('code');
    code.className = 'language-' + (lang || 'plaintext');
    code.dataset.hl = '1';
    code.textContent = text;
    pre.appendChild(code);
    return pre;
  }

  async function load(det) {
    if (det.dataset.loaded) return;
    det.dataset.loaded = '1';
    const body = det.querySelector(':scope > .lazy-body');
    try {
      const r = await fetch(det.dataset.src);
      const text = stripAnsi(await r.text());
      if (!r.ok) throw new Error(text || r.status);
      body.innerHTML = '';
      const kind = det.dataset.kind;
      if (kind === 'diff') body.appendChild(diffView(text));
      else if (kind === 'code') body.appendChild(codeView(text, det.dataset.lang));
      else {
        // tool output: highlight when it plainly is JSON or EDN-ish, else leave as text
        const t = text.trimStart();
        const lang = /^[\[{]"/.test(t) ? 'json' : (/^[({\[]/.test(t) || /^:/.test(t)) ? 'clojure' : 'plaintext';
        body.appendChild(codeView(text, lang));
      }
      highlight(body);
    } catch (e) {
      body.innerHTML = '';
      const n = document.createElement('div');
      n.className = 'note';
      n.textContent = 'Could not load this: ' + e.message;
      body.appendChild(n);
      delete det.dataset.loaded;
    }
  }

  // a details opening: load its lazy children that are open, highlight inline code
  document.addEventListener('toggle', (ev) => {
    const det = ev.target;
    if (!(det instanceof HTMLDetailsElement) || !det.open) return;
    if (det.classList.contains('lazy')) load(det);
    $$('details.lazy[open]', det).forEach(load);
    highlight(det);
  }, true);

  // lazy sections that sit open outside any closed step (net change, file page)
  $$('details.lazy[open]').forEach((d) => { if (!d.closest('details:not([open])')) load(d); });

  // markdown for prompts and replies
  $$('.md').forEach((el) => {
    if (!window.marked) return;
    const src = el.textContent;
    const htmlStr = marked.parse(src, { breaks: true });
    el.innerHTML = window.DOMPurify ? DOMPurify.sanitize(htmlStr) : htmlStr;
    $$('pre code', el).forEach((c) => { c.dataset.hl = '1'; });
    highlight(el);
  });

  // shell commands: one command per line instead of one long line
  $$('pre.cmd code.language-bash').forEach((c) => {
    if (!window.shell) return;
    c.dataset.raw = c.textContent;
    try { c.textContent = shell.format(c.textContent); } catch (e) {}
  });

  // output per command (experimental): each command of a shell step with what it printed
  const splitBox = document.getElementById('splitout');
  function splitView(raw, output) {
    const { segs, lines, parts } = shell.assign(raw, output);
    const box = document.createElement('div');
    sandbox.className = 'split';
    const out = (from, to) => (to > from ? codeView(lines.slice(from, to).join('\n'), 'plaintext')
                                         : Object.assign(document.createElement('div'), { className: 'sc-none', textContent: 'no output' }));
    const cmd = (i) => { const pre = codeView(segs[i].cmd + (segs[i].op && segs[i].op !== ';' ? '  ' + segs[i].op : ''), 'bash'); pre.classList.add('sc-cmd'); return pre; };
    for (const p of parts) {
      const row = document.createElement('div');
      if (p.owner != null) {
        row.className = 'sc';
        row.append(cmd(p.owner), out(p.from, p.to));
      } else if (p.cmds.length) {
        row.className = 'sc grp';
        const h = document.createElement('div');
        h.className = 'grp-h';
        h.textContent = 'These ' + p.cmds.length + ' commands printed the lines below together; nothing in the output shows where one ends.';
        row.append(h, ...p.cmds.map(cmd), out(p.from, p.to));
      } else {
        row.className = 'sc stray';
        const h = document.createElement('div');
        h.className = 'grp-h';
        h.textContent = 'Output none of the commands was expected to print';
        row.append(h, out(p.from, p.to));
      }
      sandbox.appendChild(row);
    }
    return box;
  }
  async function splitStep(body) {
    if (!window.shell || body.dataset.split) return;
    const code = body.querySelector(':scope > pre.cmd code.language-bash');
    const outDet = body.querySelector(':scope > details.lazy[data-kind="text"]');
    if (!code || !outDet || shell.split(code.dataset.raw || code.textContent).length < 2) return;
    body.dataset.split = '1';
    try {
      const r = await fetch(outDet.dataset.src);
      const text = stripAnsi(await r.text());
      if (!r.ok) throw new Error(text || r.status);
      const view = splitView(code.dataset.raw, text);
      body.querySelector(':scope > pre.cmd').after(view);
      body.classList.add('has-split');
      highlight(view);
    } catch (e) { delete body.dataset.split; }
  }
  function splitOpen() {
    if (!splitBox || !splitBox.checked) return;
    $$('details.step-d[open] > .step-body').forEach(splitStep);
  }
  if (splitBox) {
    const q = new URLSearchParams(location.search).get('split');
    splitBox.checked = q != null ? q === '1' : store.get('trace.split') === '1';
    document.body.classList.toggle('split-on', splitBox.checked);
    splitBox.addEventListener('change', () => {
      store.set('trace.split', splitBox.checked ? '1' : '0');
      document.body.classList.toggle('split-on', splitBox.checked);
      splitOpen();
    });
    document.addEventListener('toggle', (ev) => {
      if (splitBox.checked && ev.target.classList && ev.target.classList.contains('step-d') && ev.target.open)
        splitStep(ev.target.querySelector(':scope > .step-body'));
    }, true);
  }

  // project picker
  $$('select[data-nav]').forEach((sel) => sel.addEventListener('change', () => { location.href = sel.value; }));

  // expand / collapse all steps
  $$('[data-act]').forEach((b) => b.addEventListener('click', () => {
    const open = b.dataset.act === 'open';
    $$('.step:not(.hidden) > details.step-d').forEach((d) => { d.open = open; });
  }));

  // kind filters
  const filters = $$('[data-filter]');
  function applyFilters() {
    const on = new Set(filters.filter((f) => f.checked).map((f) => f.dataset.filter));
    $$('.step').forEach((s) => s.classList.toggle('hidden', !on.has(s.dataset.kind)));
    store.set('trace.filters', JSON.stringify([...on]));
  }
  try {
    const saved = JSON.parse(store.get('trace.filters') || 'null');
    if (Array.isArray(saved)) filters.forEach((f) => { f.checked = saved.includes(f.dataset.filter); });
  } catch (e) {}
  filters.forEach((f) => f.addEventListener('change', applyFilters));
  if (filters.length) applyFilters();

  // tape cell or #hash: open that step
  function openHash() {
    const id = decodeURIComponent(location.hash.slice(1));
    const li = id && document.getElementById(id);
    if (!li) return;
    li.classList.remove('hidden');
    const d = li.querySelector('details.step-d');
    if (d) d.open = true;
    // a step of an agent run sits inside its Agent step: open every step around it too
    for (let p = li.parentElement; p; p = p.parentElement) {
      if (p.tagName === 'DETAILS') p.open = true;
      if (p.classList && p.classList.contains('step')) p.classList.remove('hidden');
    }
    li.scrollIntoView({ block: 'start' });
  }
  window.addEventListener('hashchange', openHash);
  openHash();
  splitOpen();

  // keep the current prompt visible in the index
  const cur = document.querySelector('.pr.cur');
  const idx = document.querySelector('.index');
  if (cur && idx) idx.scrollTop = cur.offsetTop - idx.clientHeight / 2;

  // live: poll branch tips; reload when they move (follow) or say so
  const follow = document.getElementById('follow');
  const state = document.getElementById('livestate');
  if (follow) {
    follow.checked = store.get('trace.follow') === '1';
    follow.addEventListener('change', () => store.set('trace.follow', follow.checked ? '1' : '0'));
    let last = null;
    const reload = () => {
      store.set('trace.scroll', String(window.scrollY));
      location.reload();
    };
    const saved = store.get('trace.scroll');
    if (saved) { store.set('trace.scroll', ''); window.scrollTo(0, +saved); }
    state.addEventListener('click', () => { if (state.classList.contains('fresh')) reload(); });
    async function poll() {
      try {
        const m = new URLSearchParams(location.search).get('m');
        const r = await fetch('/x/tips' + (m ? '?m=' + encodeURIComponent(m) : ''), { cache: 'no-store' });
        const j = await r.json();
        const sig = JSON.stringify(j);
        if (last && sig !== last) {
          const newest = (j.live || []).join(', ');
          if (follow.checked) {
            const here = decodeURIComponent(location.pathname.replace(/^\/p\//, ''));
            if (j.latest && location.pathname.startsWith('/p/') && here !== j.latest) {
              location.href = '/p/' + encodeURIComponent(j.latest) + location.search;
            } else reload();
            return;
          }
          state.textContent = 'new steps: reload';
          state.classList.add('fresh');
          state.title = newest;
        } else if (!last) {
          state.textContent = (j.live || []).length ? 'running: ' + j.live.join(', ') : 'idle';
        }
        last = sig;
      } catch (e) {
        state.textContent = 'server down';
      }
      setTimeout(poll, 2500);
    }
    poll();
  }
})();
