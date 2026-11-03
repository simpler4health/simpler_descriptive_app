# ==============================================================================
# SIMPLER Descriptive App - Docker image for SciLifeLab Serve
# ==============================================================================
# The image contains ONLY code and public files. The data, the key, the GWAS
# manifests, the GeoLite2 database and the logs live in the project volume,
# which Serve mounts at /srv/shiny-server/data when the app starts
# (Mount path = /srv/shiny-server/data, as recommended by the Serve team).
#
# Build:   docker build --platform linux/amd64 -t simpler4health/simpler-descriptive-app:v1.0.0 .
# Test:    docker run with the local data folder mounted at /srv/shiny-server/data
# ==============================================================================

# Base image: same R version as the one in renv.lock (the app is tested with it)
FROM rocker/shiny:4.3.2

# System libraries needed to compile some R packages
#   libssl-dev: openssl | libxml2-dev, libcurl4-openssl-dev: general R package deps
#   libuv1-dev: httpuv  | libharfbuzz-dev, libfribidi-dev: text rendering
#   pandoc: Word export of the Table tab (compareGroups::export2word)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        libssl-dev \
        libxml2-dev \
        libcurl4-openssl-dev \
        libuv1-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        pandoc && \
    rm -rf /var/lib/apt/lists/*

# Install the exact package versions from renv.lock into the system library.
# Posit Package Manager provides pre-built Linux binaries, so the build is much faster.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/jammy/latest
COPY renv.lock /tmp/renv.lock
RUN Rscript -e 'install.packages("renv")' && \
    Rscript -e 'renv::restore(lockfile = "/tmp/renv.lock", library = .libPaths()[1], prompt = FALSE)' && \
    rm /tmp/renv.lock

# Copy the app code and public files (nothing else: no data, no key, no logs)
RUN rm -rf /srv/shiny-server/*
COPY app.R /srv/shiny-server/
COPY R/ /srv/shiny-server/R/
COPY faq/ /srv/shiny-server/faq/
COPY meta_data/ /srv/shiny-server/meta_data/
COPY www/ /srv/shiny-server/www/
COPY SIMPLER_white_paper/ /srv/shiny-server/SIMPLER_white_paper/

# Folders that come from the mounted volume (/srv/shiny-server/data): the app keeps
# using its usual relative paths (overview_data/, manifests/, GeoLite2-Country/, logs/).
RUN ln -s data/overview_data    /srv/shiny-server/overview_data && \
    ln -s data/manifests        /srv/shiny-server/manifests && \
    ln -s data/GeoLite2-Country /srv/shiny-server/GeoLite2-Country && \
    ln -s data/logs             /srv/shiny-server/logs

# Make sure the 'shiny' user has UID 999, as required by SciLifeLab Serve
RUN if id shiny >/dev/null 2>&1 && [ "$(id -u shiny)" -ne 999 ]; then \
        userdel -r shiny; \
        id -u 999 >/dev/null 2>&1 && userdel -r "$(id -un 999)"; \
    fi; \
    id shiny >/dev/null 2>&1 || useradd -u 999 -m -s /bin/bash shiny; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/

USER shiny
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
