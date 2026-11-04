---
name: reus-compare
description: Compare models or reasoning efforts on the same prompts. Collects the prompts, asks which models and efforts to try, runs every combination in its own fresh copy of the project through Copilot CLI, records each run with reus, and produces a side-by-side HTML report. Use when the user says /reus-compare, or asks to compare models, benchmark prompts, or try a task on several models.
---

# reus-compare

You set up and run a model comparison with reus (checkout: `{{REUS}}`). Each combination of model and effort
("arm") gets a fresh copy of the user's project and runs the same prompts, one after the other, as one Copilot
session. reus records every run; the result is a report that shows the arms side by side: calls, edits, diffs,
failures, tokens, time, and whether the end state passes a check.

Be brief. Ask everything in ONE message, numbered, then wait. If an answer is already in the user's message, do
not ask for it again. If you cannot ask (a non-interactive run), use what you were given, pick the defaults below
and say which you picked.

## 1. Ask

1. **Name** for this comparison: short, letters and digits and `-` (default: `compare-` plus today's date).
2. **The prompts**: the user types them, or names a folder of `.md` files (one prompt per file, in name order) or one
   file whose prompts are separated by a line `---`. Prompts should build on each other (a later one may refer to
   the result of an earlier one). If the user types them, write them as files with the create tool:
   `{{REUS}}/var/prompts/<name>/01.md`, `02.md`, ... (one prompt per file, the text exactly as given).
3. **The project to work in**: default is the current folder (its files are copied; the original is never touched).
4. **A check command** that tells whether the result is right, e.g. `npm test` or `node --test` (optional; it is
   run on the end state of every arm).
5. **The arms**: which models and which reasoning efforts. One arm is `model` or `model:effort`, for example
   `claude-haiku-4.5:low`, `claude-haiku-4.5:high`, `gpt-5`. Use the model names the user's Copilot shows in
   `/model`. Efforts are `low`, `medium`, `high` (some models also `xhigh`, `max`; a model that does not support an
   effort ignores it). Two to four arms is a good size. Give an arm a name with `name=model:effort` if you like.

## 2. Confirm

Say: "N arms x M prompts = N*M requests at least (each on its own model)". Every prompt of every arm is at least one
Copilot request on that arm's model, usually several (every tool call is one). Premium models cost more per request.
Ask the user to confirm before you run anything (skip this only in a non-interactive run).

## 3. Run

Run this with the bash tool, from any folder (it can take several minutes; give the command the longest timeout you
can, or start it in the background and wait for it to finish):

```
bb --config {{REUS}}/bb.edn runs:go --task <name> --prompts <prompts folder or file> --start <project folder> --check "<command>" --arm <arm1> --arm <arm2> ...
```

If the shell says the command is still running or was moved to the background, wait for it with read_bash until it
finishes (it can take several minutes); never start it a second time. Leave out `--check` when there is none. Do not change the arms' folders or the prompts while it runs. The command
prints one line per prompt as it goes and, at the end, the path of the report. If it fails at the start, show the
error and stop; do not retry in a loop (a retry costs requests).

## 4. Hand back

Tell the user, in a few lines:
- the path of the **report file** the command printed (one HTML file, no server needed; they can attach it to a
  chat as it is) and, if it printed it, the live viewer address (`bb trace:gui` in `{{REUS}}`, then `/runs/<name>`);
- one sentence per arm from the command's summary: did it finish, how many prompts, how long.
Do not interpret the results beyond that, and do not edit the arms' folders. If the user asks what the report
shows: the summary row per arm, then every prompt with what each arm did call by call (click a call for its diff
or output), and the reply of each arm.
