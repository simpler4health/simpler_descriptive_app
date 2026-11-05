# SIMPLER Descriptive App

An R Shiny application for researchers who want to use data from
[SIMPLER](https://www.simpler4health.se/w/sh/en) (Swedish Infrastructure for Medical
Population-Based Life-Course and Environmental Research), Uppsala University.

The app helps researchers prepare a data application. They can:

- **Explore Variable** – browse the variables available in each dataset, generate a list of
  the variables to request, see an estimated cost, and download the selection as an Excel file ready to upload to
  [iLab](https://uu.ilab.agilent.com/service_center/show_external/3659).
- **Table** – get descriptive tables (optionally stratified) to estimate how many participants
  match a combination of variables; download as Word.
- **Combination Plot** – see the overlap between variables with an UpSet plot; download as PDF.
- **Search FAQ** – a keyword-based FAQ assistant (no AI involved).

**No participant data are included in this repository.**

---

## Before using the app

Before using this app, we strongly encourage all SIMPLER users to visit the
[SIMPLER website](https://www.simpler4health.se/w/sh/en/) and read the available information
about SIMPLER, including the
[Rules and Guidelines](https://www.simpler4health.se/w/sh/en/researchers/rules-and-guidelines).
This will help you understand the study, how to request data, and the proper channels to
follow — saving time for both users and the project.

To better understand the study and the data available, we also encourage users to read the
SIMPLER white paper (Warensjö Lemming E, et al. *Eur J Epidemiol* 2026.
doi:[10.1007/s10654-026-01455-6](https://doi.org/10.1007/s10654-026-01455-6)), also available
as a PDF in [`SIMPLER_white_paper/`](SIMPLER_white_paper/SIMPLER_European_Journal_of_Epidemiology.pdf).

> **Important — data protection notice.** The data available in this application is intended
> solely for the preparation of research proposals and not for research purposes. SIMPLER does
> not permit any misuse of this application, including any attempt to re-identify participants,
> share data with unauthorized third parties, or use the data for purposes other than preparing
> a research proposal.

**Requesting data.** To request data, please follow the instructions available on the
[SIMPLER website](https://www.simpler4health.se/w/sh/en/researchers/data-access), and submit
your application through the
[iLab system](https://uu.ilab.agilent.com/service_center/show_external/3659).

**Data anonymization.** To ensure participant confidentiality, the dataset has been modified.
In the current version, a subset of 7,669 participants out of 107,339 has been removed so that
any possible combination of variables contains at least 10 individuals. The numbers shown in
the app are therefore approximate.

**Usage statistics.** This application collects anonymous usage statistics (such as visits,
interaction events, and country-level geographic information) to improve performance and
usability. No personal data or identifiable information is stored. Usage logs are retained for
90 days and then automatically deleted (details in the section *Privacy and usage logging* below).

---

## Repository structure

```
.
├── app.R                              UI, server, usage logging and optional login
├── R/
│   ├── _disable_autoload.R            disables Shiny's automatic loading of R/
│   ├── explore_variable_module.R      'Explore Variable' tab
│   └── faq_search_block.R             keyword-based FAQ search
├── faq/
│   └── simpler_faq.csv                FAQ content (keywords, answer)
├── meta_data/                         variable lists shown in 'Explore Variable' (metadata only)
│   ├── datasets_appv01.csv
│   └── var.list*.csv                  one file per dataset (11 files)
├── www/                               logos
├── SIMPLER_white_paper/
│   └── SIMPLER_European_Journal_of_Epidemiology.pdf   (CC BY)
├── renv.lock                          exact R package versions (used to build the Docker image)
├── Dockerfile                         Docker image for SciLifeLab Serve (code only, no data)
├── .dockerignore                      files that must never enter the image
├── shiny-server.conf                  Shiny Server settings (app logs written to the data volume)
├── .github/workflows/docker.yml       builds and publishes the image automatically (GitHub Actions)
├── CITATION.cff                       citation metadata (authors, licence)
├── LICENSE
└── README.md
```

Files and folders that are **not** in the repository and must be provided at deployment:

| Path | Content | Notes |
|---|---|---|
| `overview_data/underlying_overview_datav03.enc.rds` | anonymised participant-level data, encrypted (AES-256-GCM) | provided by SIMPLER; decrypted in memory at start-up |
| `overview_data/key_code.txt` | data key file | see *Configuration* |
| `manifests/` | Illumina GSA manifest ZIPs (GWAS tab) | optional; without it the manifest links do not work |
| `GeoLite2-Country/GeoLite2-Country.mmdb` | MaxMind GeoLite2 Country database | optional; see below |
| `logs/` | usage logs | created automatically when running locally; must exist in the Serve volume |

---

## Requirements

- R 4.3.2 (the version recorded in `renv.lock`) with the packages:
  `shiny`, `shinyjs`, `shinyWidgets`, `shinymanager`, `DT`, `compareGroups`, `ggplot2`, 
  `Hmisc`, `ComplexUpset`, `openxlsx`, `digest`, `openssl`,
  `rgeolocate`, `jsonlite`, `htmltools`
- [pandoc](https://pandoc.org/) ≥ 2 (Word export of the Table tab)

The exact package versions are recorded in `renv.lock`. To install them:

```r
install.packages("renv")
renv::restore()
```

> **Note:** the Combination Plot (ComplexUpset 1.3.3) needs **ggplot2 3.4.x** and the matching
> versions of `patchwork`, `scales` and `gtable` recorded in `renv.lock`. It does not work with
> ggplot2 ≥ 4.0.

---

## Configuration

The app needs a key to decrypt the data file. It is read from the environment variable
`SIMPLER_DATA_KEY` or from a key file stored with the data . **Never commit the key.**

The app reads its settings from environment variables (never commit their values):

| Variable | Required | Purpose |
|---|---|---|
| `SIMPLER_DATA_KEY` | yes | key used to decrypt the data file |
| `SIMPLER_USER`, `SIMPLER_PASS` | no | if both are set, a login screen is shown (not 
used on Serve, where access is controlled by the app's visibility setting)|

---

## Country lookup (MaxMind GeoLite2)

Usage logs store the visitor's country, looked up with the free MaxMind GeoLite2 Country
database. Its licence does not allow redistribution, so it is not included here:

1. Create a free account at <https://www.maxmind.com/en/geolite2/signup>.
2. Download **GeoLite2 Country** (`.mmdb`) and place it at
   `GeoLite2-Country/GeoLite2-Country.mmdb`.

Without the database the app works normally and the country is logged as `Unknown`.

---

## Deployment on SciLifeLab Serve

1. **Image.** Every push to `main` that changes the app builds a Docker image with GitHub
   Actions and publishes it on the GitHub Container Registry with a unique date-time tag,
   e.g. `ghcr.io/simpler4health/simpler_descriptive_app:20261103-1500`. The address of each
   image is shown in the summary of the run (tab *Actions*). The image is public and
   contains only code.
2. **Data.** Upload the data files (see the table in *Repository structure*) to the project
   volume (`project-vol`) with the Serve File Manager, including an empty `logs/` folder.
3. **App settings on Serve.** Image: the address from step 1 (never `latest`);
   Port: `3838`; Mount path: `/srv/shiny-server/data`.

---

## Privacy and usage logging

How the anonymous usage statistics are collected:

- Each visitor gets a pseudonymous id: an HMAC-SHA256 of IP address + user agent, using a
  random key generated **in memory** every day. The key is never written to disk, so ids
  cannot be linked across days or reversed.
- Only the pseudonymous id, session id, country and event name are stored. The IP address
  is never stored.
- Logs (`logs/log_visits_*.csv`, `logs/log_events_*.csv`) are deleted automatically after
  **90 days**.

This design was approved by the SIMPLER Data Office.

---

## Citation

**This app.** To cite the SIMPLER Descriptive App, use the **"Cite this repository"** button
on the GitHub page (generated from [`CITATION.cff`](CITATION.cff)):

> Sayols-Baixeras S, Al Jewari C, Håkansson N, Mola-Caminal M, Kolseth AK, Michaëlsson K.
> SIMPLER Descriptive App.

**SIMPLER.** To describe or cite SIMPLER, please use the white paper:

> Warensjö Lemming E, et al. The Swedish infrastructure for medical population-based
> life-course and environmental research. *Eur J Epidemiol*. 2026 Oct 12.
> doi:[10.1007/s10654-026-01455-6](https://link.springer.com/article/10.1007/s10654-026-01455-6)

Please also follow the SIMPLER
[publication policy](https://www.simpler4health.se/w/sh/en/researchers/publication-policy).

The PDF in `SIMPLER_white_paper/` is an open access article distributed under the terms of
the Creative Commons Attribution (CC BY) licence.

---

## Licence

The **source code** is released under the [MIT License](LICENSE).

The following are **not** covered by the MIT License:

- the SIMPLER data (not included in this repository);
- the logos in `www/`, which belong to their respective institutions;
- the white paper in `SIMPLER_white_paper/` (CC BY, see above);
-  the content of `faq/simpler_faq.csv`, which may be reused and adapted under
  [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), provided that the SIMPLER white
  paper (see *Citation*) is cited as the source.

---

## Contact

SIMPLER, Uppsala University – [simpler@surgsci.uu.se](mailto:simpler@surgsci.uu.se)
