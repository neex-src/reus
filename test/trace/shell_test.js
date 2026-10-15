// node test/trace/shell_test.js -- resources/trace/shell.js: split, format, assign
const assert = require('node:assert/strict');
const sh = require('../../resources/trace/shell.js');

const cmds = (s) => sh.split(s).map((x) => x.cmd);
let n = 0;
const t = (name, f) => { f(); n++; };

t('top-level operators split; pipes and 2>&1 do not', () => {
  assert.deepEqual(cmds("cd /x; cat a.edn | head -3 && echo '== b; c' || ls 2>&1"),
    ['cd /x', 'cat a.edn | head -3', "echo '== b; c'", 'ls 2>&1']);
  assert.deepEqual(sh.split('a && b; c').map((x) => x.op), ['&&', ';', '']);
});

t('loops, subshells, $( ) and heredocs stay whole', () => {
  assert.deepEqual(cmds('for f in a b; do echo "== $f"; head -2 $f; done; echo end'),
    ['for f in a b; do echo "== $f"; head -2 $f; done', 'echo end']);
  assert.deepEqual(cmds('x=$(ls; pwd); (cd a; ls); { a; b; }'), ['x=$(ls; pwd)', '(cd a; ls)', '{ a; b; }']);
  assert.deepEqual(cmds("cat > f <<'EOF'\na; b\nEOF\nls"), ["cat > f <<'EOF'\na; b\nEOF", 'ls']);
  assert.deepEqual(cmds('if [ -f a ]; then cat a; fi; ls'), ['if [ -f a ]; then cat a; fi', 'ls']);
});

t('format: one command per line, && kept at the line end', () => {
  assert.equal(sh.format('cd x; git status && git log -1'), 'cd x\ngit status &&\ngit log -1');
  assert.equal(sh.format('ls'), 'ls');
});

t('assign: echo markers cut the output', () => {
  const { parts } = sh.assign('cat a; echo ---; cat b; echo "== $f"; ls',
    'a1\na2\n---\nb1\n== x\nf1\nf2\n');
  const by = parts.map((p) => [p.cmds, p.from, p.to, p.owner]);
  assert.deepEqual(by, [[[0], 0, 2, 0], [[1], 2, 3, 1], [[2], 3, 4, 2], [[3], 4, 5, 3], [[4], 5, 7, 4]]);
});

t('assign: silent and one-line commands, a group when nothing separates', () => {
  const { parts } = sh.assign('cd x; cat a; cat b; pwd', 'a\nb\n/x\n');
  const by = parts.map((p) => [p.cmds, p.from, p.to, p.owner]);
  assert.deepEqual(by, [[[0, 1, 2], 0, 2, null], [[3], 2, 3, 3]]);
});

t('assign: line shapes find where a command starts', () => {
  const cmd = "cd x; cat a.edn; grep -rn foo src lib/*.clj | cut -c1-80; cat .gitignore; git worktree list";
  const out = '{:a 1}\n{:b 2}\nsrc/x.clj:3:foo\nlib/y.clj:9:foo\n/target\n/home/r  9cf63a3 [main]\n';
  const by = sh.assign(cmd, out).parts.map((p) => [p.cmds, p.from, p.to, p.owner]);
  assert.deepEqual(by, [[[0], 0, 0, 0], [[1], 0, 2, 1], [[2], 2, 4, 2], [[3], 4, 5, 3], [[4], 5, 6, 4]]);
});

t('classify', () => {
  assert.equal(sh.classify('mkdir -p x').kind, 'silent');
  assert.equal(sh.classify('cat a > b').kind, 'silent');
  assert.equal(sh.classify('ls 2>&1').kind, 'unknown');
  assert.equal(sh.classify('echo $x').kind, 'unknown');
  assert.equal(sh.classify('git log | wc -l').kind, 'fixed');
});

console.log(`shell.js: ${n} tests passed`);
