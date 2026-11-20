# reus

All the code here is written by AI (Claude Code) and has not been reviewed.

reus records what an AI coding agent does, as git history. Every prompt, every tool call (a file
read, an edit, a command) and every answer becomes one commit in a separate git repo. Later you go
through a session step by step with plain `git log -p`, or in the browser.

It works with Claude Code and with GitHub Copilot (the CLI and the IntelliJ plugin).

## What you need

- git
- bb ([babashka](https://babashka.org)): `brew install borkdude/brew/babashka`

reus installs nothing.

## Start

    bb gui        # opens http://127.0.0.1:7350, Ctrl-C stops it

At first the page only looks: which Claude Code and Copilot sessions are on this machine, and
whether reus records them. Then, with buttons:

1. **Build traces from the old logs.** Every session you already ran becomes git history. This
   writes only into reus's own `var/` folder.
2. **Browse them** in the viewer.
3. **Turn recording on**, so every new session is recorded while it happens. The page shows the
   exact text it will add before you click. Turning it off takes it out again; what was recorded
   stays.

The same from a terminal:

    bb trace:on           # record Claude Code sessions
    bb copilot:install    # record Copilot sessions
    bb trace:health       # "trace ok", or what is broken
    bb trace:gui          # only the viewer, http://127.0.0.1:7360
    bb trace:off          # stop (and bb copilot:uninstall)

`bb tasks` lists everything else.

## What it does on your machine

- Reads the logs Claude Code and Copilot already keep (`~/.claude/projects/`, `~/.copilot/`).
  Read only.
- Writes git repos into `var/` inside the reus folder (or wherever `TRACE_REPO` points).
- Only when you turn recording on: adds hook lines to `~/.claude/settings.json` and writes
  `~/.copilot/hooks/reus.json`.
- Listens on 127.0.0.1 only.
- Sends nothing over the network. A few developer tasks (`bb runs:go`, the end-to-end test) start
  `claude -p` or Copilot on purpose; you never need them to use reus.

The traces are copies of what the agent saw and did: code, prompts, answers. They stay on your
machine, in `var/`.

## How it works

The agent runs a hook on every prompt, every tool call and every answer. The hook takes a snapshot
of the project folder into the trace repo, writes the event to a spool file and exits. A worker in
the background turns the spool into commits. The hook always exits 0, so a broken reus never stops
the agent; `bb trace:health` says what went wrong.

Each project gets branches in the trace repo:

    <project>/main                  your real main, copied commit for commit
    <project>/s/<session>           one session
    <project>/a/<session>-<agent>   one subagent run of that session

A session branch reads like this:

    prompt 03: fix the login test
    03.1 Read: src/login.clj
    03.2 Bash: bb test
    03.3 Edit: src/login.clj
    03.4 Bash: bb test
    response 03

Every commit holds the whole project folder as it was right after that call. A read gives an empty
diff, an edit gives exactly what it changed. The commit message has the full command, and trailers
you can grep: the tool, the file, the exit code, how long it took, the model, the tokens.
