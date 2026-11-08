# CORRECTIONS.md — simpler_descriptive_app

Project: simpler4health/simpler_descriptive_app
Base branch analyzed: main
Destination fork: https://github.com/admeduardovieira/simpler_descriptive_app

## Corrections proposed

| ID | Title | Severity | Status | Risk | Files involved |
|----|-------|----------|--------|------|----------------|
| REA-simpler-001 | No automated tests for Shiny app | P3 | PROPOSING | Medium | `tests/` (new) |
| REA-simpler-002 | Data key read from local file without commit protection | P3 | PROPOSING | Low | `app.R`, `.gitignore` |

## Details of proposed corrections

### REA-simpler-001 — No automated tests
**Problem:** No tests validate the Shiny app UI, data export, or variable selection.
**Fix:** Add testthat-based tests for core modules.
**Validation:** Run tests; confirm no regressions.

### REA-simpler-002 — Data key commit protection
**Problem:** `app.R` reads key from `overview_data/key_code.txt`; no `.gitignore` entry prevents accidental commit.
**Fix:** Add `overview_data/key_code.txt` to `.gitignore`.
**Validation:** Confirm file is ignored by git.

## Not implemented
None implemented in this execution; proposals for maintainers.

## Publication plan
- Fork: `admeduardovieira/simpler_descriptive_app` (created).
- No code changes (only audit reports).
- Commit identity: `admeduardovieira <admeduardovieira@users.noreply.github.com>`.
- PR to `simpler4health/simpler_descriptive_app` branch `main`.

Переклад технічний, виконаний автоматизовано; рекомендується перегляд носієм мови перед використанням якнормативного посилання.
