// bb gui's page (trace.dash): pickers, the two-step buttons, the build's progress. No library, no dialog box.
(() => {
  const token = document.querySelector('meta[name=reus-token]').content;
  const $ = (s, el = document) => el.querySelector(s);
  const $$ = (s, el = document) => [...el.querySelectorAll(s)];
  const fmt = n => n >= 1e9 ? (n / 1e9).toFixed(1) + ' GB' : n >= 1e6 ? Math.round(n / 1e6) + ' MB' : n >= 1e3 ? Math.round(n / 1e3) + ' KB' : n + ' B';
  const post = (url, body) =>
    fetch(url, { method: 'POST', headers: { 'content-type': 'application/json', 'x-reus-token': token }, body: JSON.stringify(body) })
      .then(r => r.json().catch(() => ({})).then(j => ({ ok: r.ok, status: r.status, j })));

  // --- show all / filter ---------------------------------------------------------------------
  $$('.showall').forEach(b => b.addEventListener('click', () => {
    const box = b.closest('.panel, .repo');
    $$('.more', box).forEach(r => r.classList.remove('more'));
    b.remove();
  }));

  // --- pickers ---------------------------------------------------------------------------------
  $$('.panel[data-source]').forEach(panel => {
    const boxes = () => $$('tbody input[type=checkbox]', panel);
    const btn = $('[data-build]', panel);
    const count = $('.count', panel);
    const update = () => {
      if (!btn) return;
      const on = boxes().filter(b => b.checked);
      const n = on.reduce((a, b) => a + +b.dataset.n, 0);
      const bytes = on.reduce((a, b) => a + +b.dataset.bytes, 0);
      count.textContent = on.length ? `${n} session${n === 1 ? '' : 's'} picked, ${fmt(bytes)}` : 'nothing picked';
      btn.disabled = !on.length;
    };
    panel.addEventListener('change', update);
    const filter = $('.filter', panel);
    if (filter) filter.addEventListener('input', () => {
      const q = filter.value.trim().toLowerCase();
      $$('tbody tr', panel).forEach(tr => {
        tr.hidden = q && !(tr.dataset.q || '').includes(q);
        if (q) tr.classList.add('shown'); else tr.classList.remove('shown');
      });
    });
    $$('[data-pick]', panel).forEach(b => b.addEventListener('click', () => {
      const k = b.dataset.pick;
      boxes().forEach(x => {
        const tr = x.closest('tr');
        x.checked = k === 'all' ? !tr.hidden : k === 'week' ? tr.dataset.week === '1' : false;
      });
      update();
    }));
    update();
  });

  // --- the two-step buttons ----------------------------------------------------------------------
  const go = el => el.closest('.go');
  const ask = (btn) => {
    const g = go(btn);
    $$(':scope > button', g).forEach(b => b.hidden = true);
    $('.count', g) && ($('.count', g).hidden = true);
    $('.confirm', g).hidden = false;
  };
  const unask = (g) => {
    $$(':scope > button', g).forEach(b => b.hidden = false);
    $('.count', g) && ($('.count', g).hidden = false);
    $('.confirm', g).hidden = true;
  };
  $$('[data-cancel]').forEach(b => b.addEventListener('click', () => unask(go(b))));

  $$('[data-build]').forEach(b => b.addEventListener('click', () => ask(b)));
  $$('[data-build-go]').forEach(b => b.addEventListener('click', () => {
    const source = b.dataset.buildGo;
    const panel = b.closest('.panel');
    const keys = $$('tbody input[type=checkbox]', panel).filter(x => x.checked).map(x => x.value);
    b.disabled = true;
    post('/x/build', { source, keys }).then(({ ok, j }) => {
      b.disabled = false;
      unask(go(b));
      if (!ok) { showJob({ state: 'failed', source, error: j.error || 'refused' }); return; }
      watching = true;
      showJob(j);
      poll();
    });
  }));

  $$('[data-hook]').forEach(b => b.addEventListener('click', () => {
    const c = $('.confirm', go(b));
    c.dataset.which = b.dataset.hook;
    c.dataset.to = b.dataset.to;
    $('p', c).dataset.base = $('p', c).dataset.base || $('p', c).innerHTML;
    $('p', c).innerHTML = (b.dataset.to === 'on' ? 'Turn on: ' : 'Turn off: ') + $('p', c).dataset.base;
    ask(b);
  }));
  $$('[data-hook-go]').forEach(b => b.addEventListener('click', () => {
    const c = b.closest('.confirm');
    b.disabled = true;
    post('/x/hooks', { which: c.dataset.which, to: c.dataset.to }).then(({ ok, j }) => {
      const out = $('#hook-out');
      out.hidden = false;
      out.textContent = ok ? (j.out || 'done') : ('refused: ' + (j.error || ''));
      if (ok) setTimeout(() => { location.hash = 'hooks'; location.reload(); }, 1500);
      else b.disabled = false;
    });
  }));

  // --- the build's progress ----------------------------------------------------------------------
  const jobEl = $('#job');
  let watching = false;
  const running = j => j && ['reading', 'recording', 'swapping'].includes(j.state);
  const secs = j => Math.round(((j.ended || Date.now()) - (j.started || Date.now())) / 1000);
  function showJob(j) {
    if (!j || !j.state) { jobEl.hidden = true; return; }
    jobEl.hidden = false;
    jobEl.dataset.state = j.state;
    const who = j.source === 'copilot' ? 'Copilot' : 'Claude';
    $('#job-title').textContent = (running(j) ? 'Building ' : j.state === 'done' ? 'Built ' : 'Could not build ') + who + ' traces';
    let pct = 0, text = '';
    if (j.state === 'reading') { pct = j.files ? 30 * j.read / j.files : 2; text = `reading log ${j.read} of ${j.files}`; }
    else if (j.state === 'recording') {
      pct = 30 + (j.events ? 66 * j.done / j.events : 0);
      text = j.done >= j.events ? `writing the repo (${j.events} events)` : `recording event ${j.done} of ${j.events}`;
    }
    else if (j.state === 'swapping') { pct = 98; text = 'putting it in place'; }
    else if (j.state === 'done') { pct = 100; text = j.summary || 'done'; }
    else { pct = 100; text = j.error || 'failed'; }
    $('#job-bar').style.width = pct.toFixed(1) + '%';
    $('#job-state').textContent = j.state === 'failed' ? 'failed' : `${secs(j)} s`;
    const t = $('#job-text');
    t.textContent = text;
    if (j.state === 'done') {
      const key = j.source === 'copilot' ? 'copilot-replay' : 'claude-replay';
      const a = document.createElement('a');
      a.className = 'button'; a.href = '/browse?r=' + key; a.textContent = 'Browse it';
      t.append(' ', a);
    }
  }
  function poll() {
    fetch('/x/job', { cache: 'no-store' }).then(r => r.json()).then(j => {
      showJob(j);
      if (running(j)) { watching = true; setTimeout(poll, 500); }
      else if (watching && j.state === 'done') { location.hash = 'browse'; location.reload(); }
    }).catch(() => setTimeout(poll, 2000));
  }
  const first = JSON.parse(jobEl.dataset.job || '{}');
  showJob(first);
  if (running(first)) poll();
})();
