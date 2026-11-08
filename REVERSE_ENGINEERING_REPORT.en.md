# REVERSE_ENGINEERING_REPORT — simpler_descriptive_app

## A. General diagnosis

### Architecture identified
R Shiny application for researchers to explore SIMPLER (Swedish Infrastructure for Medical Population-Based Life-Course and Environmental Research) variables, Uppsala University. Structure:

- `app.R`: UI, server, usage logging, optional login
- `R/`: modules (explore_variable_module.R, faq_search_block.R, _disable_autoload.R)
- `meta_data/`: variable lists per dataset (11 CSV files)
- `faq/`: FAQ content (simpler_faq.csv)
- `renv.lock`: exact R package versions
- `Dockerfile`: image for SciLifeLab Serve
- `.github/workflows/docker.yml`: CI for Docker image build and publish

### Technologies and dependencies
- R 4.3.2
- Packages: shiny, shinyjs, shinyWidgets, shinymanager, DT, compareGroups, ggplot2, Hmisc, ComplexUpset, openxlsx, digest, openssl, rgeolocate, jsonlite, htmltools
- Docker

### Maintenance state
Default branch: main. Repository: https://github.com/simpler4health/simpler_descriptive_app. Automatic build via GitHub Actions + GHCR.

### Existing testing mechanisms
No automated tests found.

### Investigation limitations
- R not installed; app not executed.
- Participant data not included in repository (encrypted, provided externally).
- REA 6.7.0 not operational; static analysis only.

## B. Problems found

| ID | Title | Severity | Status | Location |
|----|-------|----------|--------|----------|
| REA-simpler-001 | No automated tests for Shiny app | P3 | CONFIRMED | repo root |
| REA-simpler-002 | Data key read from local file without commit protection | P3 | PROVÁVEL | `app.R:87-92` |

### Details of confirmed problems

**REA-simpler-001 (P3, CONFIRMED)**: No automated tests for the R Shiny application (UI tests, export tests, variable validation tests).

### Details of the probable problem

**REA-simpler-002 (P3, PROVÁVEL)**: `app.R:87-92` reads data key from `overview_data/key_code.txt` if environment variable is not set. Although README warns "Never commit the key", there is no code protection against accidental commit.

## C. Recurring problems
No recurring public issues found.

## D. Undocumented errors
All items in section B are problems not previously documented.

## E. Performance
Single data loading at startup (not reactive). Usage logs retained 90 days with automatic purge.

## F. Technical improvements
- Add tests for Shiny app (REA-simpler-001)
- Add gitignore/check to prevent key commit (REA-simpler-002)

## G. Execution plan
All independent and low risk.

## H. Expected outcome
More testable app, with protection against credential leakage.

**Tools used:** git, gh, PowerShell, static code inspection.  
**Limitation:** R not installed.  
**Environment:** Windows, PowerShell 5.1, `C:\Users\User\audit-repos\simpler_descriptive_app` (clone of upstream, branch main).
