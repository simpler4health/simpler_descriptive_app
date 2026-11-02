# ============================================================
# EXPLORE VARIABLE MODULE v9
# ============================================================
# Explore Variable module for the SIMPLER Descriptive App.
#
# What it does:
#   - The user picks datasets in the sidebar ('Datasets:' picker) and clicks 'Explore variable'.
#   - One tab per dataset is rendered with a DT table of the available variables.
#     Cells/rows/columns can be clicked to add variables to a per-dataset 'basket'.
#   - A 'Cost' tab estimates the cost of the request from the datasets in the baskets.
#   - The baskets can be downloaded as an Excel file (one sheet per dataset + Cost sheet).
#
# Public entry points (called from app.R):
#   explore_variable_sidebar_ui(dataset_choices)   sidebar inputs
#   explore_variable_tab_ui()                      main-panel placeholder (uiOutput)
#   explore_variable_server(input, output, session, log_event_fn, session_info)
#
# NOTE: not a namespaced Shiny module: input/output ids are global, and
# show()/hide() (loading overlay) are expected to be available when called.

# openxlsx: used only for the Excel download
library(openxlsx)

# ============================================================
# COST COMPUTATION
# ============================================================
# Builds the cost table from the current baskets (rv$baskets).
# Returns list(table = data.frame(Dataset, Cost), total = numeric).
# Prices are in SEK and apply to academic users (see the Cost tab text).
ev_compute_cost <- function(rv) {
  rows  <- list()
  total <- 0

  # Application fee: always included
  rows[["application"]] <- data.frame(Dataset="Application fee", Cost="5,000 SEK", stringsAsFactors=FALSE)
  total <- total + 5000

  # Labels of the datasets that have at least one selected variable
  active_labels <- names(rv$baskets)[sapply(names(rv$baskets), function(l) length(rv$baskets[[l]])>0)]

  # Questionnaires, registries, demographics, clinical chemistry and metals are billed together
  q_labels <- c("Questionnaires","Disease registry","Basic demographic", "Clinical chemistry","Metals")
  if (any(active_labels %in% q_labels)) {
    rows[["questionnaires"]] <- data.frame(Dataset="Questionnaires & Register data", Cost="20,000 SEK", stringsAsFactors=FALSE)
    total <- total + 20000
  }
  # Each of the following groups is billed once, however many variables are selected
  if ("Proteomics" %in% active_labels) {
    rows[["proteomics"]] <- data.frame(Dataset="Proteome Profile", Cost="20,000 SEK", stringsAsFactors=FALSE)
    total <- total + 20000
  }
  if (any(active_labels %in% c("NMR metabolomics","LC-MS metabolomics"))) {
    rows[["metabolomics"]] <- data.frame(Dataset="Metabolome Profile", Cost="20,000 SEK", stringsAsFactors=FALSE)
    total <- total + 20000
  }
  if ("GWAS" %in% active_labels) {
    rows[["gwas"]] <- data.frame(Dataset="GWAS", Cost="20,000 SEK", stringsAsFactors=FALSE)
    total <- total + 20000
  }
  # Metagenomics: price depends on the profiler (MetaPhlAn4 / CHAMP) of the selected files
  if ("Metagenomics" %in% active_labels) {
    raw_meta    <- rv$datasets[["Metagenomics"]]
    basket_meta <- rv$baskets[["Metagenomics"]]
    profilers   <- character(0)
    if (!is.null(raw_meta) && "Profiler" %in% names(raw_meta) && "File" %in% names(raw_meta)) {
      sel_rows  <- raw_meta[!is.na(raw_meta$File) & raw_meta$File %in% basket_meta, ]
      profilers <- unique(sel_rows$Profiler)
    }
    if ("MetaPhlAn4" %in% profilers) {
      rows[["meta4"]] <- data.frame(Dataset="Metagenome Profile (MetaPhlAn4)", Cost="20,000 SEK", stringsAsFactors=FALSE)
      total <- total + 20000
    }
    if ("CHAMP" %in% profilers) {
      rows[["champ"]] <- data.frame(Dataset="Metagenome Profile (CHAMP)", Cost="50,000 SEK", stringsAsFactors=FALSE)
      total <- total + 50000
    }
    # Selected files with no known profiler: shown as a range, not added to the total
    if (length(profilers)==0 && length(basket_meta)>0) {
      rows[["meta_unk"]] <- data.frame(Dataset="Metagenome Profile (profiler not determined)", Cost="20,000-50,000 SEK", stringsAsFactors=FALSE)
    }
  }

  # Final TOTAL row
  rows[["total"]] <- data.frame(Dataset="TOTAL", Cost=paste0(formatC(total,format="d",big.mark=",")," SEK"), stringsAsFactors=FALSE)
  cost_df <- do.call(rbind, rows); rownames(cost_df) <- NULL
  list(table=cost_df, total=total)
}

# ============================================================
# GWAS MANIFEST MAP
# ============================================================
# Maps the name in the 'Manifest' column of the GWAS table to the ZIP file to download.
# The ZIP files are served through the 'manifests' resource path registered in app.R
# (addResourcePath('manifests', 'manifests')).
ev_gwas_manifests <- list(
  "GSAV3"     = "manifests/GSA-24v3-0-A1-manifest-file-csv.zip",
  "GSA-MD_v1" = "manifests/GSAMD-24v1-0_20011747_A1.zip"
)

# ============================================================
# QUESTIONNAIRE CLICK COLUMNS
# Must match names(read.csv(...)) exactly (spaces become dots,
# and note "Toilet", not "Toilte")
# ============================================================
ev_questionnaire_cols <- c("FFQ1987","FFQ1997","HQ2008","FFQ2009",
                            "HQ.cycle1","Toilet.Habits.cycle1",
                            "HQ2019","FFQ2019",
                            "HQ.cycle2","Standr..Health.Interview.cycle2",
                            "Toilet.Habits.cycle2")


# Custom reader for the drug registry file: values are quoted and separated by spaces,
# so each line is parsed by extracting the quoted fields.
# NOTE: currently not referenced by ev_dataset_config (drug registry is not offered).
ev_read_drug_registry <- function(f) {
  
  lines <- readLines(f, encoding = "latin1", warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  
  extract_fields <- function(line) {
    m       <- gregexpr('"([^"]*)"', line)
    matches <- regmatches(line, m)[[1]]
    trimws(gsub('^"|"$', '', matches))
  }
  
  header <- extract_fields(lines[1])
  ncols  <- length(header)
  
  body <- lapply(lines[-1], function(l) {
    fields <- extract_fields(l)
    length(fields) <- ncols   # pad/truncate defensively
    fields
  })
  
  df <- as.data.frame(do.call(rbind, body), stringsAsFactors = FALSE)
  names(df) <- header
  df
}

# ============================================================
# DATASET CONFIGURATION
# ============================================================
# One entry per dataset name available in the 'Datasets:' picker.
# The names must match the column names of the app data (`dades`) and the names listed in
# meta_data/datasets_appv01.csv (spelling included).
#
# Fields:
#   file         CSV with the variable list shown in the table
#   read_fn      function that reads that CSV (separator/quote/encoding differ per file)
#   click_cols   columns whose cells become clickable buttons (they feed the basket)
#   label        tab title; entries sharing the same label + file are shown in ONE tab
#   gwas / health_exam / registry / proteomics   flags that switch the table layout
#   prot_filter_col  (proteomics only) column used to keep the rows of the chosen Olink panel
ev_dataset_config <- list(

  # --- Questionnaires (all share the same file and the same set of clickable columns) ---
  "FFQ1987"                         = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "FFQ1997"                         = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "HQ2008"                          = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "FFQ2009"                         = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "HQ cycle1"                       = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "Toilte Habits cycle1"            = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "HQ2019"                          = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "FFQ2019"                         = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "HQ cycle2"                       = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "Standr. Health Interview cycle2" = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "Toilte Habits cycle2"            = list(file="meta_data/var.list_app02.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols=ev_questionnaire_cols, label="Questionnaires", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),

  # --- Basic demographic ---
  "participant_sex"    = list(file="meta_data/var.list.basic_demographicv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols="variable", label="Basic demographic", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  
  # --- Genotyping arrays (the 'Manifest' column becomes a download link) ---
  "GWAS" = list(file="meta_data/var.list.gwasv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols="Array", label="GWAS", gwas=TRUE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),

  # --- Clinical chemistry ---
  "clinical_chemistry_cycl1" = list(file="meta_data/var.list.clin.chemv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Clinical chemistry", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "clinical_chemistry_cycl2" = list(file="meta_data/var.list.clin.chemv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Clinical chemistry", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),

  # --- Metals ---
  "metals_cycl1" = list(file="meta_data/var.list.metalsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Metals", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "metals_cycl2" = list(file="meta_data/var.list.metalsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Metals", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),

  # --- Proteomics (Olink): rows are filtered by the selected panel ---
  "Olink.target96_cycl1" = list(file="meta_data/var.list.proteomicsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("UniProt.ID.target96.cycle1","UniProt.ID.explore.cycle1"), label="Proteomics", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=TRUE, prot_filter_col="UniProt.ID.target96.cycle1"),
  "Olink.explore_cycl1"  = list(file="meta_data/var.list.proteomicsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("UniProt.ID.target96.cycle1","UniProt.ID.explore.cycle1"), label="Proteomics", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=TRUE, prot_filter_col="UniProt.ID.explore.cycle1"),

  # --- Disease registry (one entry per outcome available in the data) ---
  "dead"              = list(file="meta_data/var.list.registryv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols="Disease", label="Disease registry", gwas=FALSE, health_exam=FALSE, registry=TRUE, proteomics=FALSE),
  
  "any_cancer"   = list(file="meta_data/var.list.registryv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols="Disease", label="Disease registry", gwas=FALSE, health_exam=FALSE, registry=TRUE, proteomics=FALSE),
  "cvd"               = list(file="meta_data/var.list.registryv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols="Disease", label="Disease registry", gwas=FALSE, health_exam=FALSE, registry=TRUE, proteomics=FALSE),
  "diabetes"          = list(file="meta_data/var.list.registryv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols="Disease", label="Disease registry", gwas=FALSE, health_exam=FALSE, registry=TRUE, proteomics=FALSE),
  "fracture"          = list(file="meta_data/var.list.registryv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols="Disease", label="Disease registry", gwas=FALSE, health_exam=FALSE, registry=TRUE, proteomics=FALSE),
  "hip_frac"          = list(file="meta_data/var.list.registryv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,stringsAsFactors=FALSE), click_cols="Disease", label="Disease registry", gwas=FALSE, health_exam=FALSE, registry=TRUE, proteomics=FALSE),
  
  # --- Metabolomics and metagenomics ---
  "metabolomics_nmr_cycl1"   = list(file="meta_data/var.list.nmr_metabolomicsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="NMR metabolomics", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "metabolomics_nmr_cycl2"   = list(file="meta_data/var.list.nmr_metabolomicsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="NMR metabolomics", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "metabolomics_lc_ms_cycl1" = list(file="meta_data/var.list.lc_ms_metabolomicsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols="Mode", label="LC-MS metabolomics", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),
  "Fecal metagenomics"       = list(file="meta_data/var.list.metagenomicsv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols="File", label="Metagenomics", gwas=FALSE, health_exam=FALSE, registry=FALSE, proteomics=FALSE),

  # --- Health examination (shown with sub-tabs: All / DXA / Tanita) ---
  "dxa_cycl1" = list(file="meta_data/var.list.health.examv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Health examination", gwas=FALSE, health_exam=TRUE, registry=FALSE, proteomics=FALSE),
  "dxa_cycl2" = list(file="meta_data/var.list.health.examv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Health examination", gwas=FALSE, health_exam=TRUE, registry=FALSE, proteomics=FALSE),
  "tanita"    = list(file="meta_data/var.list.health.examv01.appv01.csv", read_fn=function(f) read.csv(f,header=TRUE,sep=" ",quote='"',stringsAsFactors=FALSE,fileEncoding="latin1"), click_cols=c("cycle1","cycle2"), label="Health examination", gwas=FALSE, health_exam=TRUE, registry=FALSE, proteomics=FALSE)
)

# ============================================================
# DESCRIPTIONS
# ============================================================
# Short text shown in the blue box at the top of each dataset tab (keyed by `label`).
ev_dataset_descriptions <- list(
  "Questionnaires"    = "Variables available in questionnaires across different collection waves (FFQ and HQ). Only rows with at least one non-missing value in the selected questionnaires are shown.",
  "Basic demographic" = "Basic demographic variables available for all participants.",
  "GWAS"              = "Genotyping arrays available in SIMPLER. Click on Array to select. Click the Manifest link to download the corresponding manifest ZIP file.",
  "Clinical chemistry"= "Clinical chemistry measurements available across collection cycles.",
  "Metals"            = "Metal measurements available across collection cycles.",
  "Proteomics"        = "Proteomics data from Olink panels. Rows are filtered based on the selected panel (target96 or explore).",
  "Disease registry"  = "Disease outcomes available in SIMPLER. Click a disease to select it. ICD codes for selected diseases appear below.",
  "Drug registry"     = "Drug registry variables available in SIMPLER.",
  "NMR metabolomics"  = "NMR metabolomics variables available across collection cycles.",
  "LC-MS metabolomics"= "LC-MS metabolomics variables by acquisition mode.",
  "Metagenomics"      = "Metagenomic data files available in SIMPLER. MetaPhlAn4 files cost 20,000 SEK; CHAMP files cost 50,000 SEK (cumulative if both selected).",
  "Health examination"= "Health examination variables. Sub-tabs filter by instrument: All variables, DXA body composition, and Tanita measurements."
)

# ============================================================
# HELPERS
# ============================================================
# Distinct datasets (by label + file) for the selected picker values; used to load each file once.
ev_unique_datasets <- function(selected_vals) {
  seen <- character(0); out <- list()
  for (v in selected_vals) {
    cfg <- ev_dataset_config[[v]]; if (is.null(cfg)) next
    key <- paste0(cfg$label,"|",cfg$file)
    if (!key %in% seen) { seen <- c(seen,key); out[[cfg$label]] <- cfg }
  }
  out
}

# Names of the proteomics columns to filter on, for the selected Olink panels.
ev_proteomics_filter_cols <- function(selected_vals) {
  cols <- c()
  for (v in selected_vals) {
    cfg <- ev_dataset_config[[v]]
    if (!is.null(cfg) && isTRUE(cfg$proteomics) && !is.null(cfg$prot_filter_col))
      cols <- unique(c(cols, cfg$prot_filter_col))
  }
  cols
}

# Questionnaires: keep only rows with at least one non-missing value in the selected questionnaires.
ev_filter_questionnaire_rows <- function(df, selected_vals, all_click_cols) {
  sel <- intersect(intersect(selected_vals, all_click_cols), names(df))
  if (length(sel)==0) return(df)
  df[rowSums(!is.na(df[,sel,drop=FALSE]))>0,,drop=FALSE]
}

# All distinct non-empty values of one column (used by the 'select column' buttons).
ev_all_col_vals <- function(raw, col) {
  if (is.null(raw)||!col %in% names(raw)) return(character(0))
  unique(na.omit(as.character(raw[[col]])))
}

# Compact text of the basket for the sidebar (first 20 values, then '...').
ev_basket_display <- function(vals) {
  if (length(vals)==0) return(tags$small(style="color:#aaa;","empty"))
  display <- if (length(vals)>20) c(vals[1:20],"...") else vals
  tags$small(style="color:#555;word-break:break-all;", paste(display, collapse=", "))
}

# HTML for one clickable cell button. Blue (btn-primary) = in the basket, grey = not selected.
ev_make_btn <- function(val, label, col_name, basket) {
  if (is.na(val)||val=="") return("")
  cls <- if (val %in% basket) "btn-primary" else "btn-secondary"
  sprintf('<button class="ev-var-btn btn btn-sm %s" data-ds="%s" data-col="%s" data-val="%s">%s</button>',
          cls, htmltools::htmlEscape(label), htmltools::htmlEscape(col_name),
          htmltools::htmlEscape(val), htmltools::htmlEscape(val))
}

# Prepares the data.frame shown by DT:
#   - adds a '.sel' column with a row checkbox
#   - replaces each clickable column by buttons
#   - (GWAS) turns the Manifest column into a download link
ev_build_dt_df <- function(raw, label, click_c, basket, is_gwas) {
  df <- raw
  if (length(click_c)>0) {
    row_vals <- apply(df[,click_c,drop=FALSE],1,function(r) paste(r[!is.na(r)&r!=""],collapse="|||"))
  } else row_vals <- rep("",nrow(df))

  chk <- sapply(row_vals, function(vals) {
    parts   <- strsplit(vals,"\\|\\|\\|")[[1]]
    any_sel <- length(basket)>0 && any(parts %in% basket)
    sprintf('<input type="checkbox" class="ev-row-check" data-ds="%s" data-vals="%s"%s>',
            htmltools::htmlEscape(label), htmltools::htmlEscape(vals),
            if (any_sel) " checked" else "")
  })
  df <- cbind(.sel=chk, df)

  for (col in click_c) {
    if (!col %in% names(df)) next
    df[[col]] <- sapply(df[[col]], function(v) ev_make_btn(v, label, col, basket))
  }

  if (is_gwas && "Manifest" %in% names(df)) {
    df[["Manifest"]] <- sapply(df[["Manifest"]], function(v) {
      if (is.na(v)||v=="") return("")
	path <- ev_gwas_manifests[[v]]
      if (is.null(path)) return(htmltools::htmlEscape(v))
      url <- file.path("manifests", basename(path))
      sprintf('<a href="%s" download target="_blank">%s <i class="fa fa-download"></i></a>',
              htmltools::htmlEscape(url), htmltools::htmlEscape(v))
    })
  }
  df
}

# JavaScript run by DT after every redraw (paging/filtering/sorting): re-applies the
# basket state (blue buttons, ticked checkboxes) stored in window.ev_selected.
ev_dt_draw_callback <- function(label, click_c) {
  sprintf("
    function(settings) {
      var ds = %s;
      var sel = window.ev_selected || {};
      var basket = sel[ds] || [];
      if (basket.length === 0) return;
      $(settings.nTable).find('.ev-var-btn[data-ds=\"' + ds + '\"]').each(function() {
        var val = String($(this).data('val'));
        if (basket.indexOf(val) >= 0) {
          $(this).removeClass('btn-secondary').addClass('btn-primary');
        } else {
          $(this).removeClass('btn-primary').addClass('btn-secondary');
        }
      });
      $(settings.nTable).find('.ev-row-check[data-ds=\"' + ds + '\"]').each(function() {
        var vals = String($(this).data('vals')).split('|||');
        var anySelected = vals.some(function(v){ return basket.indexOf(v) >= 0; });
        $(this).prop('checked', anySelected);
      });
    }
  ", jsonlite::toJSON(label))
}

# DT table with the settings shared by all dataset tabs.
ev_datatable <- function(df, label, click_c) {
  DT::datatable(df, escape=FALSE, selection="none", filter="top", rownames=FALSE,
    options=list(
      pageLength=4, autoWidth=FALSE, dom="ftip", scrollX=TRUE,
      initComplete=DT::JS("function(settings,json){var api=this.api();setTimeout(function(){api.columns.adjust().draw(false);},150);}"),
      drawCallback=DT::JS(ev_dt_draw_callback(label, click_c)),
      columnDefs=list(list(width="35px",targets=0))
    )
  )
}

# Blue info box shown above each table.
ev_info_box <- function(label) {
  txt <- ev_dataset_descriptions[[label]]
  if (is.null(txt)) txt <- paste0("Variables available in ",label,".")
  div(style="background:#e8f4fd;border-left:4px solid #3c8dbc;padding:10px 14px;border-radius:4px;font-size:13px;margin-bottom:12px;",
      icon("info-circle",style="color:#3c8dbc;margin-right:6px;"), txt)
}

# ============================================================
# SIDEBAR UI
# ============================================================
# Sidebar: dataset picker, 'Explore variable' button, Excel download and the basket summary.
explore_variable_sidebar_ui <- function(dataset_choices) {
  tagList(
    pickerInput("main_var","Datasets:",choices=dataset_choices,multiple=TRUE,
                options=list(`actions-box`=TRUE,`live-search`=TRUE,
                             `selected-text-format`="count > 2",
                             `count-selected-text`="{0} datasets selected")),
    div(style="display:flex;align-items:center;gap:6px;",
        actionButton("exp_variable","Explore variable",icon=icon("table"),class="btn-primary",style="flex-grow:1;"),
        info_button("exp_variable","Displays variables in selected datasets. Click cells, use row checkboxes, or column/dataset buttons.","Explore variable")),
    br(),br(),
    div(style="display:flex;align-items:center;gap:6px;",
        downloadButton("ev_download_excel","Download variable information"),
        info_button("ev_download_excel","Download selected variables as Excel with one sheet per dataset plus a Cost sheet. Only datasets with selected variables are included.","Download")),
    br(),br(),
    tags$hr(style="margin:8px 0;"),
    tags$p(style="font-size:12px;font-weight:bold;margin-bottom:4px;","Selected variables:"),
    uiOutput("ev_basket_sidebar"),
    br()
  )
}

# IMPORTANT: ev_gwas_dl must always be present in the DOM (not inside
# renderUI) so that clicking a manifest link can trigger it at any time.
# Main-panel placeholder: the tabs are generated on the server side (output$ev_dataset_tabs).
explore_variable_tab_ui <- function() {
  tagList(
    uiOutput("ev_dataset_tabs")
   
  )
}

# ============================================================
# SERVER
# ============================================================
# Server logic of the module. Returns the reactiveValues `rv` invisibly.
#   log_event_fn  function(event, session, info) used for usage logging (optional)
#   session_info  reactiveVal with the anonymous user info (optional)
explore_variable_server <- function(input, output, session, log_event_fn=NULL, session_info=NULL) {

  # rv$datasets   raw data.frame of each loaded dataset (by label)
  # rv$configs    its configuration entry
  # rv$baskets    selected values per dataset (character vectors)
  # rv$selected   values currently chosen in the 'Datasets:' picker
  # rv$load_tick  incremented on every load to force the tabs to re-render
  rv <- reactiveValues(datasets=list(), configs=list(), baskets=list(),
                       selected=list(), load_tick=0, gwas_manifest=NULL)

  # Sends the baskets to the browser (window.ev_selected) so DT can colour the buttons
  ev_sync_js_basket <- function() {
    basket_js <- rv$baskets
    basket_js <- lapply(basket_js, function(v) if (length(v)==0) list() else as.list(v))
    session$sendCustomMessage("ev_update_basket", basket_js)
  }

  # 'Explore variable' button: load the files of the selected datasets (once per label),
  # drop the ones that are no longer selected and refresh the tabs.
  observeEvent(input$exp_variable, {
    if (!is.null(log_event_fn)) log_event_fn("exp_variable",session, info = if (!is.null(session_info)) session_info() else NULL)
    req(input$main_var)
    show(); on.exit(hide(),add=TRUE)

    rv$selected <- input$main_var
    unique_ds   <- ev_unique_datasets(input$main_var)

    for (label in names(unique_ds)) {
      cfg <- unique_ds[[label]]
      tryCatch({
        if (is.null(rv$datasets[[label]])) {
          df <- cfg$read_fn(cfg$file); df[df==""] <- NA
          rv$datasets[[label]] <- df
        }
        rv$configs[[label]] <- cfg
        if (is.null(rv$baskets[[label]])) rv$baskets[[label]] <- character(0)
      }, error=function(e) showNotification(paste("Error loading",label,":",e$message),type="error"))
    }

    current_labels <- names(unique_ds)
    for (label in names(rv$datasets)) {
      if (!label %in% current_labels) {
        rv$datasets[[label]] <- NULL; rv$configs[[label]] <- NULL; rv$baskets[[label]] <- NULL
      }
    }
    rv$load_tick <- rv$load_tick + 1
    ev_sync_js_basket()
  })

  # Click on a single cell button: toggle that value in the basket
  observeEvent(input$ev_cell_click, {
    ds <- input$ev_cell_click$ds; val <- input$ev_cell_click$val
    cur <- rv$baskets[[ds]]; if (is.null(cur)) cur <- character(0)
    rv$baskets[[ds]] <- if (val %in% cur) setdiff(cur,val) else unique(c(cur,val))
    ev_sync_js_basket()
  })

  # Row checkbox: add/remove all the clickable values of that row
  observeEvent(input$ev_row_check, {
    ds <- input$ev_row_check$ds
    vals <- input$ev_row_check$vals; vals <- vals[!is.na(vals)&vals!=""]
    cur <- rv$baskets[[ds]]; if (is.null(cur)) cur <- character(0)
    rv$baskets[[ds]] <- if (isTRUE(input$ev_row_check$checked)) unique(c(cur,vals)) else setdiff(cur,vals)
    ev_sync_js_basket()
  })

  # 'Select column' button: select/deselect every value of one column
  observeEvent(input$ev_col_select, {
    ds  <- input$ev_col_select$ds; col <- input$ev_col_select$col
    raw <- rv$datasets[[ds]]
    all_vals <- ev_all_col_vals(raw, col)
    cur <- rv$baskets[[ds]]; if (is.null(cur)) cur <- character(0)
    all_selected <- length(all_vals)>0 && all(all_vals %in% cur)
    rv$baskets[[ds]] <- if (all_selected) setdiff(cur,all_vals) else unique(c(cur,all_vals))
    esc_ds <- gsub('"','\\\\"',ds); esc_col <- gsub('"','\\\\"',col)
    action <- if (all_selected) 'removeClass("btn-primary").addClass("btn-secondary")' else 'removeClass("btn-secondary").addClass("btn-primary")'
    runjs(sprintf('$(".ev-var-btn[data-ds=\\"%s\\"][data-col=\\"%s\\"]").%s;',esc_ds,esc_col,action))
    ev_sync_js_basket()
  })

  # 'Select / Deselect all' button: toggle every clickable value of the dataset
  observeEvent(input$ev_select_all, {
    ds  <- input$ev_select_all$ds
    raw <- rv$datasets[[ds]]; cfg <- rv$configs[[ds]]
    if (is.null(raw)||is.null(cfg)) return()
    click_c  <- intersect(cfg$click_cols,names(raw))
    all_vals <- unique(unlist(lapply(click_c, function(col) ev_all_col_vals(raw,col))))
    cur <- rv$baskets[[ds]]; if (is.null(cur)) cur <- character(0)
    all_selected <- length(all_vals)>0 && all(all_vals %in% cur)
    rv$baskets[[ds]] <- if (all_selected) setdiff(cur,all_vals) else unique(c(cur,all_vals))
    esc_ds <- gsub('"','\\\\"',ds)
    action <- if (all_selected) 'removeClass("btn-primary").addClass("btn-secondary")' else 'removeClass("btn-secondary").addClass("btn-primary")'
    runjs(sprintf('$(".ev-var-btn[data-ds=\\"%s\\"]").%s;',esc_ds,action))
    ev_sync_js_basket()
  })

  # ----------------------------------------------------------
  # GWAS manifest download
  # ----------------------------------------------------------


  # ----------------------------------------------------------
  # Dataset tabs
  # ----------------------------------------------------------
  # Renders all the tabs. For every dataset a DT output is created; the layout depends on
  # the flags of its config: health_exam (sub-tabs), proteomics (filtered rows),
  # registry (extra ICD table of the selected diseases) or the generic table.
  output$ev_dataset_tabs <- renderUI({
    rv$load_tick
    labels <- names(rv$datasets)

    if (length(labels)==0) return(div(style="padding:40px;color:#888;text-align:center;",
                                      icon("info-circle")," Select datasets and click Explore variable."))

    # One renderer per dataset label
    for (label in labels) {
      local({
        lbl <- label; cfg <- rv$configs[[lbl]]
        if (is.null(cfg)) return()

        if (isTRUE(cfg$health_exam)) {
          # Health examination: one table per instrument (All / DXA / Tanita)
          for (s in c("all","DXA","Tanita")) {
            local({
              suffix <- s; instr <- list(all=NULL,DXA="DXA",Tanita="Tanita BC-418")[[suffix]]
              oid <- paste0("ev_dt_",make.names(lbl),"_",suffix)
              output[[oid]] <- DT::renderDT({
                raw <- rv$datasets[[lbl]]; if (is.null(raw)||nrow(raw)==0) return(NULL)
                basket  <- isolate(rv$baskets[[lbl]]); if (is.null(basket)) basket <- character(0)
                click_c <- intersect(cfg$click_cols,names(raw))
                df_filt <- if (!is.null(instr)&&"Instrument"%in%names(raw))
                  raw[!is.na(raw$Instrument)&raw$Instrument==instr,,drop=FALSE] else raw
                if (nrow(df_filt)==0) return(NULL)
                ev_datatable(ev_build_dt_df(df_filt,lbl,click_c,basket,FALSE), lbl, click_c)
              },server=TRUE)
            })
          }

        } else if (isTRUE(cfg$proteomics)) {
          # Proteomics: keep only the rows of the selected Olink panel(s)
          oid <- paste0("ev_dt_",make.names(lbl))
          output[[oid]] <- DT::renderDT({
            raw <- rv$datasets[[lbl]]; if (is.null(raw)||nrow(raw)==0) return(NULL)
            basket  <- isolate(rv$baskets[[lbl]]); if (is.null(basket)) basket <- character(0)
            click_c <- intersect(cfg$click_cols,names(raw))
            fc <- intersect(ev_proteomics_filter_cols(isolate(rv$selected)),names(raw))
            df_filt <- if (length(fc)>0) raw[rowSums(!is.na(raw[,fc,drop=FALSE]))>0,,drop=FALSE] else raw
            ev_datatable(ev_build_dt_df(df_filt,lbl,click_c,basket,FALSE), lbl, click_c)
          },server=TRUE)

        } else if (isTRUE(cfg$registry)) {
          # Disease registry: table of diseases + table with the ICD codes of the selected ones
          oid     <- paste0("ev_dt_",make.names(lbl))
          oid_icd <- paste0(oid,"_icd")
          output[[oid]] <- DT::renderDT({
            raw <- rv$datasets[[lbl]]; if (is.null(raw)||nrow(raw)==0) return(NULL)
            basket  <- isolate(rv$baskets[[lbl]]); if (is.null(basket)) basket <- character(0)
            click_c <- intersect(cfg$click_cols,names(raw))
            ev_datatable(ev_build_dt_df(raw,lbl,click_c,basket,FALSE), lbl, click_c)
          },server=TRUE)
          output[[oid_icd]] <- DT::renderDT({
            raw <- rv$datasets[[lbl]]; basket <- rv$baskets[[lbl]]
            if (is.null(raw)||length(basket)==0) return(NULL)
            click_c <- intersect(cfg$click_cols,names(raw)); if (length(click_c)==0) return(NULL)
            sel <- raw[!is.na(raw[[click_c[1]]])&raw[[click_c[1]]]%in%basket,,drop=FALSE]
            if (nrow(sel)==0) return(NULL)
            DT::datatable(sel,escape=TRUE,selection="none",rownames=FALSE,
                          options=list(pageLength=25,dom="tip"))
          },server=TRUE)

        } else {
          # Generic table (questionnaires are additionally filtered by the selected questionnaires)
          oid <- paste0("ev_dt_",make.names(lbl))
          output[[oid]] <- DT::renderDT({
            raw <- rv$datasets[[lbl]]; if (is.null(raw)||nrow(raw)==0) return(NULL)
            basket  <- isolate(rv$baskets[[lbl]]); if (is.null(basket)) basket <- character(0)
            click_c <- intersect(cfg$click_cols,names(raw))
            df_show <- if (lbl=="Questionnaires"&&length(rv$selected)>0)
              ev_filter_questionnaire_rows(raw,rv$selected,cfg$click_cols) else raw
            ev_datatable(ev_build_dt_df(df_show,lbl,click_c,basket,isTRUE(cfg$gwas)), lbl, click_c)
          },server=TRUE)
        }
      })
    }

    # Cost table and total (recomputed whenever the baskets change)
    cost_result <- reactive({ ev_compute_cost(rv) })

    output$ev_cost_table <- renderTable({
      cost_result()$table
    }, striped=TRUE, hover=TRUE, bordered=TRUE, rownames=FALSE)

    output$ev_cost_total <- renderUI({
      res <- cost_result()
      div(style="background:#fff3cd;border-left:4px solid #ffc107;padding:10px 14px;border-radius:4px;font-size:15px;font-weight:bold;margin-top:8px;",
          icon("coins",style="color:#ffc107;margin-right:6px;"),
          paste0("Estimated total cost: ",formatC(res$total,format="d",big.mark=",")," SEK"))
    })

    # Build the tab of each dataset: info box, column buttons, table and info banner
    tab_list <- lapply(labels, function(label) {
      cfg     <- rv$configs[[label]]
      ds_key  <- make.names(label)
      click_c <- if (!is.null(cfg)) intersect(cfg$click_cols,names(rv$datasets[[label]])) else character(0)

      col_btns <- if (length(click_c)>0) {
        div(style="margin-bottom:8px;display:flex;flex-wrap:wrap;align-items:center;gap:4px;",
            tags$small(style="color:#666;margin-right:4px;","Select column:"),
            lapply(click_c, function(col) {
              tags$button(class="ev-col-select-btn btn btn-sm btn-outline-secondary",
                          style="font-size:11px;padding:2px 8px;",
                          `data-ds`=label,`data-col`=col, col)
            }),
            tags$button(class="ev-select-all-btn btn btn-sm btn-outline-primary",
                        style="font-size:11px;padding:2px 8px;margin-left:8px;",
                        `data-ds`=label, icon("check-double")," Select / Deselect all")
        )
      } else NULL

      # Table(s) of the tab, depending on the dataset type
      content <- if (!is.null(cfg)&&isTRUE(cfg$health_exam)) {
        tabsetPanel(
          tabPanel("All",    DT::DTOutput(paste0("ev_dt_",ds_key,"_all"))),
          tabPanel("DXA",    DT::DTOutput(paste0("ev_dt_",ds_key,"_DXA"))),
          tabPanel("Tanita", DT::DTOutput(paste0("ev_dt_",ds_key,"_Tanita")))
        )
      } else if (!is.null(cfg)&&isTRUE(cfg$registry)) {
        tagList(
          DT::DTOutput(paste0("ev_dt_",ds_key)),
          br(),
          tags$p(style="font-size:12px;font-weight:bold;margin-top:10px;margin-bottom:4px;",
                 "Selected diseases - ICD codes:"),
          DT::DTOutput(paste0("ev_dt_",ds_key,"_icd"))
        )
      } else {
        DT::DTOutput(paste0("ev_dt_",ds_key))
      }

      tabPanel(label, br(), ev_info_box(label), col_btns, br(), content,dataset_info_banner())
    })

    # Last tab: cost estimate
    cost_tab <- tabPanel("Cost", br(),
      div(style="background:#e8f4fd;border-left:4px solid #3c8dbc;padding:10px 14px;border-radius:4px;font-size:13px;margin-bottom:16px;box-sizing:border-box;max-width:100%;",
          icon("info-circle",style="color:#3c8dbc;margin-right:6px;"),
          "The estimated cost of your project is calculated based on the datasets from which you have selected variables. The application fee (5,000 SEK) is always included, and will be deducted from the total cost once the project is approved. Costs are charged per project, not per variable.",
          tags$br(), tags$br(),
          tags$em("Please note that these fees apply to researchers in academia. For projects within the business sector, full cost coverage applies for data.")),
      tableOutput("ev_cost_table"),
      uiOutput("ev_cost_total")
    )

    # Dataset tabs + Cost tab
    do.call(tabsetPanel, c(list(id="ev_main_tabs"), tab_list, list(cost_tab)))
  })

  # ----------------------------------------------------------
  # Basket sidebar
  # ----------------------------------------------------------
  # Sidebar summary: selected values per dataset, with a Reset button each
  output$ev_basket_sidebar <- renderUI({
    labels <- names(rv$baskets)
    if (length(labels)==0) return(tags$em(style="color:#888;","None selected"))
    items <- lapply(labels, function(label) {
      vals <- rv$baskets[[label]]
      div(style="margin-bottom:10px;",
          tags$strong(style="font-size:12px;",label), br(),
          ev_basket_display(vals), br(),
          actionButton(paste0("ev_reset_",make.names(label)),label="Reset",
                       icon=icon("trash"),class="btn-xs btn-outline-danger",
                       style="margin-top:3px;font-size:11px;"))
    })
    do.call(tagList,items)
  })

  # Reset buttons are created dynamically: register one observer per new dataset label (only once)
  observed_reset_btns <- reactiveVal(character(0))

  observe({
    new_labels <- setdiff(names(rv$baskets), observed_reset_btns())
    if (length(new_labels) == 0) return()

    for (label in new_labels) {
      local({
        lbl <- label; btn_id <- paste0("ev_reset_",make.names(lbl))
        observeEvent(input[[btn_id]], {
          rv$baskets[[lbl]] <- character(0)
          esc <- gsub('"','\\\\"',lbl)
          runjs(sprintf('$(".ev-var-btn[data-ds=\\"%s\\"]").removeClass("btn-primary").addClass("btn-secondary");$(".ev-row-check[data-ds=\\"%s\\"]").prop("checked",false);',esc,esc))
          ev_sync_js_basket()
        },ignoreInit=TRUE)
      })
    }

    observed_reset_btns(union(observed_reset_btns(), new_labels))
  })

  # ----------------------------------------------------------
  # Excel download
  # ----------------------------------------------------------
  output$ev_download_excel <- downloadHandler(
    filename=function() paste0("simpler_variables_",Sys.Date(),".xlsx"),
    content=function(file) {
      if (!is.null(log_event_fn)) log_event_fn("download_explore_variable",session, info = if (!is.null(session)) session_info() else NULL)
      wb <- createWorkbook(); written <- FALSE

      # One sheet per dataset that has selected variables
      for (label in names(rv$datasets)) {
        raw    <- rv$datasets[[label]]
        basket <- rv$baskets[[label]]
        cfg    <- rv$configs[[label]]
        if (is.null(cfg)||is.null(raw)||nrow(raw)==0) next
        if (length(basket)==0) next

        # Keep rows with at least one selected value; unselected values in those rows are blanked
        click_c <- intersect(cfg$click_cols,names(raw))

        if (length(click_c)==0) {
          sheet_df <- raw
        } else {
          row_match <- rowSums(
            sapply(click_c,function(col) !is.na(raw[[col]])&raw[[col]]%in%basket),
            na.rm=TRUE)>0
          sheet_df <- raw[row_match,,drop=FALSE]
          for (col in click_c)
            sheet_df[[col]] <- ifelse(!is.na(sheet_df[[col]])&sheet_df[[col]]%in%basket,
                                      sheet_df[[col]],NA)
          still_ok <- rowSums(!is.na(sheet_df[,click_c,drop=FALSE]))>0
          sheet_df <- sheet_df[still_ok,,drop=FALSE]
        }

        if (nrow(sheet_df)==0) next

        # Excel sheet names are limited to 31 characters
        sname <- substr(label,1,31)
        addWorksheet(wb,sname)
        writeData(wb,sname,sheet_df,headerStyle=createStyle(textDecoration="bold"))
        setColWidths(wb,sname,cols=seq_len(ncol(sheet_df)),widths="auto")
        written <- TRUE
      }

      # Cost sheet (always included)
      cost_res <- ev_compute_cost(rv)
      cost_df  <- cost_res$table
      addWorksheet(wb,"Cost")
      writeData(wb,"Cost",cost_df,headerStyle=createStyle(textDecoration="bold"))
      total_row <- nrow(cost_df)+1
      addStyle(wb,"Cost",style=createStyle(textDecoration="bold"),
               rows=total_row+1,cols=1:2,gridExpand=TRUE)
      setColWidths(wb,"Cost",cols=1:2,widths="auto")

      # Fallback sheet when nothing was selected
      if (!written) {
        addWorksheet(wb,"Empty")
        writeData(wb,"Empty",data.frame(Note="No variables selected. Please select variables before downloading."))
      }

      saveWorkbook(wb,file,overwrite=TRUE)
    }
  )

  invisible(rv)
}
