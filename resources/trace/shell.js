// shell commands as the trace view shows them: split into the commands that run one
// after the other, laid out one per line, and (a guess) which part of the output each
// one printed. Plain functions; the browser gets window.shell, node gets module.exports.
(function (root) {
  const OPENERS = new Set(['for', 'while', 'until', 'if', 'case', 'select']);
  const CLOSERS = new Set(['done', 'fi', 'esac']);
  const STARTERS = new Set(['do', 'then', 'else', 'elif', '!', 'time']);

  // Top-level commands of src: split on ; && || & and newlines that sit outside quotes,
  // $( ), ( ), { }, heredoc bodies and for/while/if/case blocks.
  // -> [{cmd, op}] where op is the operator that followed the command ('' at the end).
  function split(src) {
    const segs = [];
    let cur = '', i = 0, depth = 0, kw = 0, atStart = true;
    const n = src.length, docs = [];
    const push = (op) => {
      const t = cur.trim();
      if (t) segs.push({ cmd: t, op });
      else if (segs.length && op) segs[segs.length - 1].op = op;
      cur = ''; atStart = true;
    };
    const take = (s) => { cur += s; i += s.length; };
    while (i < n) {
      const c = src[i], rest = src.slice(i);
      if (c === '\\') { take(src.slice(i, i + 2)); continue; }
      if (c === "'") { const j = src.indexOf("'", i + 1); take(src.slice(i, j < 0 ? n : j + 1)); atStart = false; continue; }
      if (c === '"' || c === '`') {
        let j = i + 1;
        while (j < n && src[j] !== c) j += src[j] === '\\' ? 2 : 1;
        take(src.slice(i, j + 1)); atStart = false; continue;
      }
      if (c === '#' && (i === 0 || /\s/.test(src[i - 1]))) {
        const j = src.indexOf('\n', i); take(src.slice(i, j < 0 ? n : j)); continue;
      }
      const hd = rest.match(/^<<-?\s*(['"]?)([A-Za-z_][A-Za-z0-9_]*)\1/);
      if (hd) { docs.push(hd[2]); take(hd[0]); continue; }
      if (c === '\n') {
        take(c);
        for (const d of docs.splice(0)) {
          const m = new RegExp('^\\t*' + d + '[ \\t]*$', 'm').exec(src.slice(i));
          take(m ? src.slice(i, i + m.index + m[0].length) : src.slice(i));
        }
        if (depth === 0 && kw === 0) push(''); else atStart = true;
        continue;
      }
      if (rest.startsWith('$(')) { depth++; take('$('); atStart = true; continue; }
      if (c === '(') { depth++; take(c); atStart = true; continue; }
      if (c === ')') { if (depth > 0) depth--; take(c); continue; }
      if (atStart && c === '{' && /\s/.test(src[i + 1] || ' ')) { depth++; take(c); continue; }
      if (atStart && c === '}') { if (depth > 0) depth--; take(c); atStart = false; continue; }
      const top = depth === 0 && kw === 0;
      let op = ['&&', '||', ';;', ';', '&'].find((o) => rest.startsWith(o));
      if (op === '&' && (/[<>]/.test(src[i - 1] || '') || src[i + 1] === '>')) op = null; // 2>&1, &>
      if (op) {
        if (top && op !== ';;') { i += op.length; push(op); }
        else { take(op); atStart = true; }
        continue;
      }
      if (c === '|') { take(c); atStart = true; continue; }
      if (/[A-Za-z_!]/.test(c) && atStart) {
        const w = rest.match(/^[A-Za-z_!]+/)[0];
        if (OPENERS.has(w)) kw++;
        else if (CLOSERS.has(w) && kw > 0) kw--;
        take(w);
        atStart = STARTERS.has(w);
        continue;
      }
      if (!/\s/.test(c)) atStart = false;
      take(c);
    }
    push('');
    return segs;
  }

  // One command per line; && and || stay at the end of the line they follow.
  function format(src) {
    const segs = split(src);
    if (segs.length < 2) return src.trim();
    return segs.map((s) => s.cmd + (s.op && s.op !== ';' ? ' ' + s.op : '')).join('\n');
  }

  // --- which output belongs to which command (a guess) ------------------------------

  const SILENT = /^(cd|mkdir|cp|mv|rm|rmdir|touch|ln|chmod|export|set|unset|sleep|true|:|source|\.)\b/;
  const ONE_LINE = /^(pwd|date|whoami|hostname)\b|^git (rev-parse|branch --show-current|rev-list --count)\b/;

  const unquote = (s) => s.replace(/^'(.*)'$/s, '$1').replace(/^"(.*)"$/s, '$1');
  const reEsc = (s) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

  // the last command of a pipeline, without its redirections' targets
  const lastOf = (cmd) => cmd.split(/\s\|\s/).pop().trim();

  // regex for the first line an echo/printf prints, or null when it is all variables
  function echoPattern(cmd) {
    let m = cmd.match(/^echo\s+(?:-[neE]+\s+)*(.*)$/s);
    let text;
    if (m) text = m[1].split(/\s+(?=(?:[^'"]|'[^']*'|"[^"]*")*$)/).map(unquote).join(' ');
    else if ((m = cmd.match(/^printf\s+(['"])(.*?)\1/s))) text = m[2].replace(/%-?\d*[sdf]/g, '\u0000').split('\\n').find((l) => l.trim()) || '';
    else return null;
    if (/[<>|]/.test(cmd.replace(/(['"]).*?\1/gs, ''))) return null; // redirected or piped away
    const parts = text.split(/\$\{[^}]*\}|\$\([^)]*\)|\$[A-Za-z_0-9@*#?]+|\u0000/);
    const re = parts.map(reEsc).join('.*');
    if (!parts.join('').trim()) return null;
    return new RegExp('^' + re + '$');
  }

  // words of a command, quotes removed (good enough to read its arguments)
  const words = (cmd) => (cmd.match(/'[^']*'|"(?:[^"\\]|\\.)*"|\S+/g) || []).map(unquote);

  // stages after the first that keep each line's start: filters, cut from column 1
  const KEEPS = /^(grep|head|tail|sort|uniq(?! -c)|less|cat|cut -c ?1-\d*)\b/;

  // A test that every output line of the command passes, from what the command is, or null.
  function linePred(cmd) {
    const stages = cmd.split(/\s\|\s/).map((s) => s.trim());
    if (stages.slice(1).some((s) => !KEEPS.test(s))) return null;
    const w = words(stages[0].replace(/\s\d?>&?\d?\s*\S+$/, '').replace(/\s2>\/dev\/null/g, ''));
    const args = w.slice(1);
    const nonFlags = args.filter((a) => !a.startsWith('-'));
    const starts = (prefixes) => {
      const ps = prefixes.map((p) => p.replace(/[*?[].*$/, '').replace(/^\.\//, '')).filter(Boolean);
      return ps.length ? (l) => ps.some((p) => l.startsWith(p)) : null;
    };
    if (w[0] === 'grep' && !args.some((a) => /^-[a-zA-Z]*h/.test(a))) {
      const files = nonFlags.slice(1); // the first non-flag is the pattern
      if (args.some((a) => /^-[a-zA-Z]*[rR]/.test(a)) || files.length > 1) return starts(files);
      if (files.length === 1 && args.some((a) => /^-[a-zA-Z]*n/.test(a)) && !args.some((a) => /^-[a-zA-Z]*[cl]/.test(a)))
        return (l) => /^\d+[:-]/.test(l) || l === '--';
    }
    if (w[0] === 'wc' && nonFlags.length > 1) return (l) => /^\s*\d+(\s+\d+)*\s+\S/.test(l);
    if (w[0] === 'find' && nonFlags[0]) return starts([nonFlags[0]]);
    if (w[0] === 'git') {
      const sub = args.filter((a) => !a.startsWith('-')).join(' ');
      if (/worktree list/.test(sub)) return (l) => /^\/\S*\s+[0-9a-f]{7,40}\s/.test(l);
      if (/\blog\b/.test(sub) && args.includes('--oneline'))
        return (l) => /^[0-9a-f]{7,12} /.test(l);
      if (/\bstatus\b/.test(sub) && args.some((a) => /^-[a-z]*s[a-z]*$|^--short$|^--porcelain$/.test(a)))
        return (l) => /^([ MADRCU?!]{2} |## )/.test(l);
      if (/^branch$/.test(sub) || /^branch\b/.test(sub) && args.some((a) => /^(-a|-r|--list|-v+)$/.test(a)))
        return (l) => /^[*+ ] \S/.test(l);
    }
    if (w[0] === 'ls' && args.some((a) => /^-[a-zA-Z]*l/.test(a))) return (l) => /^(total \d+|[-dlbcps][rwx-]{9})/.test(l);
    if (w[0] === 'tmux' && /^list-/.test(args[0] || '')) {
      const i = args.indexOf('-F');
      if (i >= 0 && args[i + 1]) {
        const re = new RegExp('^' + args[i + 1].split(/#\{[^}]*\}/).map(reEsc).join('.*') + '$');
        return (l) => re.test(l);
      }
    }
    return null;
  }

  function classify(raw) {
    const cmd = raw.replace(/^git((?:\s+-[Cc]\s+\S+)+)/, 'git');
    const last = lastOf(cmd);
    if (SILENT.test(cmd) || /^git (add|commit|checkout|switch|stash|worktree add)\b.*(-q\b|--quiet)/.test(cmd)
        || /\bsed -i\b/.test(cmd) || /(^|[^>&0-9])>{1,2}\s*[^&\s]/.test(last.replace(/(['"]).*?\1/gs, '')))
      return { kind: 'silent' };
    const pat = echoPattern(cmd);
    if (pat) return { kind: 'anchor', re: pat };
    if (ONE_LINE.test(last) || /^wc(\s+-[lwc]+)?\s+[^\s|*]+$/.test(last) || /\bwc -l\s*$/.test(last))
      return { kind: 'fixed', lines: 1 };
    if (/^git commit\b/.test(cmd) && !/\s(-q|--quiet)\b/.test(cmd))
      return { kind: 'anchor', re: /^\[\S+( \(root-commit\))? [0-9a-f]{7,}\] /,
               cont: (l) => /^ (\d+ files? changed|(create|delete|rename|mode change) )/.test(l) };
    if (/^grep\b/.test(cmd) && !/\|/.test(cmd) && /\s-[a-zA-Z]*c\b/.test(cmd) && linePred(cmd) === null) return { kind: 'fixed', lines: 1 };
    const pred = linePred(cmd);
    if (pred) return { kind: 'shaped', pred };
    return { kind: 'unknown' };
  }

  // Output text -> {segs, lines, parts}; parts cover every output line in order, each
  // {from, to, cmds: [i...], owner}: owner is the one command that printed lines
  // [from, to); without an owner the commands in cmds printed them together and nothing
  // tells where one stops (cmds empty: output none of them was expected to print).
  // Where a command's output starts is found from echo/printf markers, commands that
  // print one line, and commands whose lines have a known shape (grep -r: path:..).
  function assign(src, output) {
    const segs = split(src).map((s) => Object.assign(s, classify(s.cmd)));
    const lines = output.replace(/\n$/, '').split('\n');
    const parts = [];
    const own = (i, from, to) => parts.push({ from, to, cmds: [i], owner: i });
    // the commands in pending printed [from, to)
    const flush = (pending, from, to) => {
      const loud = pending.filter((i) => segs[i].kind !== 'silent');
      if (loud.length === 1) {
        pending.forEach((i) => (i === loud[0] ? own(i, from, to) : own(i, from, from)));
      } else if (!pending.length) {
        if (to > from) parts.push({ from, to, cmds: [], owner: null });
      } else if (to === from) {
        pending.forEach((i) => own(i, from, from));
      } else {
        parts.push({ from, to, cmds: pending, owner: null });
      }
    };
    // segs [s0, s1) printed lines [a, b)
    const block = (s0, s1, a, b) => {
      let i = s0, j = s1 - 1;
      const tail = [];
      for (; i <= j && segs[i].kind === 'fixed'; i++) { const k = Math.min(1, b - a); own(i, a, a + k); a += k; }
      for (; j >= i && segs[j].kind === 'fixed'; j--) { const k = Math.min(1, b - a); tail.unshift([j, b - k, b]); b -= k; }
      let pending = [];
      for (let x = i; x <= j; x++) {
        const s = segs[x];
        if (s.kind !== 'shaped') { pending.push(x); continue; }
        let st = a;
        while (st < b && !s.pred(lines[st])) st++;
        if (st === b) { pending.push(x); continue; }
        let en = st;
        while (en < b && s.pred(lines[en])) en++;
        flush(pending, a, st);
        own(x, st, en);
        a = en; pending = [];
      }
      flush(pending, a, b);
      tail.forEach(([x, f, t]) => own(x, f, t));
    };
    // echo/printf markers, each matched after the previous one, cut the output in blocks
    let lo = 0, segLo = 0, p = 0;
    segs.forEach((s, i) => {
      if (s.kind !== 'anchor') return;
      let at = -1;
      for (let j = p; j < lines.length; j++) if (s.re.test(lines[j])) { at = j; break; }
      if (at < 0) return;
      block(segLo, i, lo, at);
      let en = at + 1;
      while (s.cont && en < lines.length && s.cont(lines[en])) en++;
      own(i, at, en);
      lo = p = en; segLo = i + 1;
    });
    block(segLo, segs.length, lo, lines.length);
    return { segs, lines, parts };
  }

  const api = { split, format, assign, classify };
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
  else root.shell = api;
})(typeof window !== 'undefined' ? window : globalThis);
