# ==============================================================================
# SIMPLER Descriptive App  (R Shiny)
# ==============================================================================
# Lets researchers explore the SIMPLER variables, estimate how many participants
# match a combination of variables, and select what to request in a data application.
#
# Tabs:
#   Homepage          general information and usage notes
#   Explore Variable  variables per dataset + estimated cost (explore_variable_module.R)
#   Table             descriptive table, optionally stratified (compareGroups)
#   Combination Plot  UpSet plot of the overlap between variables (ComplexUpset)
# Plus a floating 'Search FAQ' assistant (keyword search, no AI; faq_search_block.R).
#
# Run (from the app folder):   shiny::runApp('app.R')
#
# Expected folder layout (paths are relative to the working directory):
#   meta_data/            datasets_appv01.csv, var.list_app02.csv, manifests
#   overview_data/        underlying_overview_datav03.rds  (anonymised participant-level data)
#   www/                  logos and images (served as 'assets')
#   SIMPLER_white_paper/  the white paper PDF (served as 'whitepaper')
#   GeoLite2-Country/     GeoLite2-Country.mmdb (MaxMind licence: do NOT redistribute)
#   faq/                  simpler_faq.csv
#   logs/                 created automatically (usage logs, deleted after 90 days)
#
# Optional environment variables (login screen, while the app is not public):
#   SIMPLER_USER, SIMPLER_PASS   if both are set, shinymanager asks for them
# ==============================================================================


# ============================================================
# Libraries
# ============================================================
library(shiny)
library(shinyjs)
library(compareGroups)    # statistical summary tables
library(shinyWidgets)     # nicer UI inputs
library(ggplot2)
library(Hmisc)            # cut2()
library(ComplexUpset)     # upset plots
library(DT)
# digest + openssl + rgeolocate: pseudonymised usage logging (see 'Random daily key' below)
# shinymanager: optional login screen (only active when SIMPLER_USER / SIMPLER_PASS are set)
library(digest)
library(rgeolocate)
library(openxlsx)
library(openssl)
library(shinymanager)

# ============================================================
# DATA LOADING (single source of truth)
# IMPORTANT:
# - Loaded once at startup (not reactive)
# - Avoids reloading for every user interaction
# ============================================================

# Defines explore_variable_sidebar_ui(), explore_variable_tab_ui() and explore_variable_server()
# (the whole 'Explore Variable' tab). NOTE: relative path, adjust it to the repository layout.
source("R/explore_variable_module.R", local = TRUE)

# dataset_map: one column per dataset group; each column lists the variable names of that group
dataset_map <- read.csv("meta_data/datasets_appv01.csv", stringsAsFactors = FALSE)

# dataset_choices: list(group name -> named vector of variables) for the Explore Variable picker.
# A leading non-breaking space is added to the entry that has the same name as its group.
dataset_choices <- list()

for (col_name in colnames(dataset_map)) {
  
  vals <- dataset_map[[col_name]]
  vals <- vals[!is.na(vals) & vals != ""]
  
  if (length(vals) > 0) {
    clean_name <- gsub("\\.", " ", col_name)
    
    display_names <- ifelse(
      vals == clean_name,
      paste0("\u00a0", vals),  
      vals
    )
    
    dataset_choices[[clean_name]] <- setNames(vals, display_names)
    
  }
}

# Anonymised participant-level data, stored encrypted (AES-256-GCM) and decrypted
data_key_file <- "overview_data/key_code.txt"
if (!nzchar(Sys.getenv("SIMPLER_DATA_KEY")) && file.exists(data_key_file)) {
  readRenviron(data_key_file)
}
data_key_hex <- Sys.getenv("SIMPLER_DATA_KEY")
if (nchar(data_key_hex) != 64) stop("Data key not found: set SIMPLER_DATA_KEY or provide overview_data/key_code.txt")
data_key <- as.raw(strtoi(substring(data_key_hex, seq(1, 63, 2), seq(2, 64, 2)), 16L))
data_enc <- readRDS("overview_data/underlying_overview_datav03.enc.rds")
dades <- unserialize(openssl::aes_gcm_decrypt(data_enc, key = data_key, iv = attr(data_enc, "iv")))
Sys.unsetenv("SIMPLER_DATA_KEY")   # the key is no longer needed once the data is loaded
rm(data_key_file, data_key_hex, data_key, data_enc)   # remove the key from memory once the data is loaded

# Size of the original cohort before anonymisation; used to report how many participants were removed
n_original_cohort <- 107339


# Builds the grouped choices (group -> variables) for the pickerInputs of Table and Combination Plot.
# Only variables that exist in `dades` are kept.
build_var_choices <- function(dataset_map, dades_names) {
  
  grouped <- list()
  used    <- character(0)
  
  for (col_name in colnames(dataset_map)) {
    
    vals <- dataset_map[[col_name]]
    vals <- trimws(vals)
    vals <- vals[!is.na(vals) & vals != ""]
    vals <- vals[vals %in% dades_names]   # keep only variables present in dades
    
    if (length(vals) > 0) {
      clean_name <- gsub("\\.", " ", col_name)
      
      display_names <- ifelse(
        vals == clean_name,
        paste0("\u00a0", vals),
        vals
      )
      
      grouped[[clean_name]] <- setNames(vals, display_names)
      used <- c(used, vals)
    }
  }
  
  # Variables of `dades` that are not listed in any group go to an 'Other' group
  # dades variables not covered by any dataset_map group
  remaining <- setdiff(dades_names, used)
  if (length(remaining) > 0) {
    grouped[["Other"]] <- setNames(remaining, remaining)
  }
  
  grouped
}

# Grouped choices used by all variable pickers in the sidebar
var_choices_grouped <- build_var_choices(dataset_map, names(dades))

# ------------------------------------------------------------
# Random daily key
# ------------------------------------------------------------

# The daily key is generated in memory when the app starts (and when the day changes). It is never
# written to disk or to the code. Consequence: the same visitor gets the same pseudonymous id during
# one day, but ids cannot be linked across days (and cannot be reversed once the process stops).
# Method: HMAC-SHA256(daily key, ip | user agent).
.salt_state <- new.env()
.salt_state$day  <- Sys.Date()
.salt_state$salt <- paste(as.character(openssl::rand_bytes(32)), collapse = "")

# Returns today's key; creates a new random one when the date has changed
get_daily_key <- function() {
  if (Sys.Date() != .salt_state$day) {
    .salt_state$day  <- Sys.Date()
    .salt_state$salt <- paste(as.character(openssl::rand_bytes(32)), collapse = "")
  }
  .salt_state$salt
}

# Pseudonymous visitor id: keyed hash (HMAC-SHA256) of IP + user agent.
# The raw IP and user agent are removed from memory right after hashing.
generate_user_id <- function(session) {
  
  ip <- isolate(session$clientData$remoteAddr)
  ua <- isolate(session$clientData$userAgent)
  
  if (is.null(ip) || ip == "") ip <- "unknown"
  if (is.null(ua) || ua == "") ua <- "unknown"
  
  input_string <- paste(ip, tolower(ua), sep = "|")
  
  user_id <- digest::hmac(
    key    = get_daily_key(),
    object = input_string,
    algo   = "sha256"
  )
  
  rm(ip, ua)
  user_id
}

# Country of the visitor from the IP address (MaxMind GeoLite2 database).
# Only the country is kept; the IP itself is never stored.
# Returns "Internal / Unknown" for private/proxy addresses and NA when the lookup fails.
get_country <- function(ip) {   
  
  # -------- detectar IP interna (HPC / proxy) --------
  if (is.null(ip) || ip == "" ||
      grepl("^(127\\.|10\\.|192\\.168\\.|172\\.)", ip)) {
    return("Internal / Unknown")
  }
  
  # -------- lookup con rgeolocate --------
  res <- tryCatch(
    rgeolocate::maxmind(
      ip,
      "GeoLite2-Country/GeoLite2-Country.mmdb"
    ),
    error = function(e) return(NULL)
  )
  
  # -------- chequeos de seguridad --------
  if (is.null(res)) return(NA)
  if (nrow(res) == 0) return(NA)
  if (is.na(res$country_name)) return(NA)
  
  return(res$country_name)
}

# Everything we keep about a visitor: the pseudonymous id and the country (never the IP)
get_user_info <- function(session) {
  
  ip <- isolate(session$clientData$remoteAddr)
  
  user_id <- generate_user_id(session)
  country <- get_country(ip)
  
  if (is.na(country)) country <- "Unknown"
  
  rm(ip)
  
  list(
    user_id = user_id,
    country = country
  )
}

# Writes one interaction event (e.g. run_table, download_table) to today's event log.
# `info` is the pre-computed list(user_id, country) from get_user_info(); when NULL it is computed here.
log_event <- function(event_name, session, info=NULL){
  
  if (is.null(info)) info <- get_user_info(session) 
  
  log <- data.frame(
    time = Sys.time(),
    user_id = info$user_id,
    session_id = session$token,
    country = info$country,
    event = event_name
  )
  
# Logging must never stop the app: if the log file cannot be written, only a warning is printed
  tryCatch(
    write.table(
      log,
      file = paste0("logs/log_events_", format(Sys.Date(), "%Y-%m-%d"),".csv"),
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(paste0("logs/log_events_", format(Sys.Date(), "%Y-%m-%d"),".csv")),
      append = TRUE
    ),
    error = function(e) warning("Could not write the event log: ", conditionMessage(e))
  )
}

# Writes one row per session to today's visit log (logs/log_visits_YYYY-MM-DD.csv)
log_visit <- function(session) {
  
  info <- get_user_info(session)
  
  log <- data.frame(
    time = Sys.time(),
    user_id = info$user_id,
    session_id = session$token,
    country = info$country
  )
  
# Logging must never stop the app: if the log file cannot be written, only a warning is printed
  tryCatch(
    write.table(
      log,
      file = paste0("logs/log_visits_", format(Sys.Date(), "%Y-%m-%d"),".csv"),
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(paste0("logs/log_visits_", format(Sys.Date(), "%Y-%m-%d"),".csv")),
      append = TRUE
    ),
    error = function(e) warning("Could not write the visit log: ", conditionMessage(e))
  )
}

# Small blue 'i' icon with a help text. The text is stored in a hidden .tooltip-box and the
# JavaScript at the end of the <head> shows it in a floating #global-tooltip on hover/focus.
# NOTE: `id` is currently not used by the function (only kept for readability at the call sites).
info_button <- function(id, text, title = NULL) {
  title_html <- if (!is.null(title)) paste0("<strong>", title, "</strong><br>") else ""
  
  tags$span(
    class = "info-tooltip",
    tabindex = "0",
    icon("info-circle"),
    tags$span(
      class = "tooltip-box",
      HTML(paste0(title_html, text))
    )
  )
}

# ------------------------------------------------------------
# Log retention: delete log files older than `keep_days` days.
# The date is taken from the file name (log_*_YYYY-MM-DD.csv).
# ------------------------------------------------------------
LOG_RETENTION_DAYS <- 90
# Folder for the usage logs (log_visits_*.csv and log_events_*.csv)
dir.create("logs", showWarnings = FALSE)

# Remembers the day of the last purge so it runs at most once per day
.purge_state <- new.env()
.purge_state$last_run <- as.Date(NA)

purge_old_logs <- function(dir = "logs", keep_days = LOG_RETENTION_DAYS) {
  # Run at most once per day
  if (!is.na(.purge_state$last_run) && .purge_state$last_run == Sys.Date()) return(invisible(NULL))
  .purge_state$last_run <- Sys.Date()
  
  files <- list.files(dir, pattern = "^log_.*_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(invisible(NULL))
  
  file_date <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", basename(files)))
  old <- files[!is.na(file_date) & file_date < Sys.Date() - keep_days]
  if (length(old) > 0) unlink(old)
  invisible(length(old))
}

# ============================================================
# DATA DISCLAIMER BANNER
# Reused on Table and Variable Combinations tabs
# ============================================================
data_disclaimer_banner <- function() {
  div(
    style = "background:#fff3cd; border-left:4px solid #ffc107;
              padding:10px 14px; border-radius:4px; font-size:13px;
              margin-bottom:12px; box-sizing:border-box; max-width:100%;",
    icon("exclamation-triangle", style="color:#856404; margin-right:6px;"),
    tags$b("Note: "),
    sprintf(
      "To protect participant confidentiality, %s participants have been removed from this dataset. ",
      format(n_original_cohort - nrow(dades), big.mark=",")
    ),
    "The numbers shown below are therefore approximate and should be used for orientation purposes only."
  )
}

# Defines simpler_faq (FAQ table read from faq/simpler_faq.csv) and search_faq()
source("R/faq_search_block.R", local = TRUE)

# Converts character columns to factors. For columns coded NO/YES the level order is NO, YES, others
# (so tables show YES as the 'event' level); other columns use the default alphabetical order.
chr_to_factor_no_yes <- function(df) {
  
  chr_cols <- names(df)[sapply(df, is.character)]
  
  for (col in chr_cols) {
    
    x <- df[[col]]
    lvls_present <- unique(x[!is.na(x)])
    
    # only reorder if the column actually uses NO/YES coding
    if (all(c("NO", "YES") %in% lvls_present)) {
      
      other_lvls <- setdiff(sort(lvls_present), c("NO", "YES"))
      ordered_lvls <- c("NO", "YES", other_lvls)
      
      df[[col]] <- factor(x, levels = ordered_lvls)
      
    } else {
      
      # not a NO/YES variable -> just convert to factor with default order
      df[[col]] <- factor(x)
    }
  }
  
  df
}

# Applied once at startup
dades <- chr_to_factor_no_yes(dades)

# ============================================================
# DATASET INFO BANNER
# Reused on Explore Variable, Table, and Variable Combinations tabs
# ============================================================
dataset_info_banner <- function() {
  div(
    style = "background:#e8f4fd; border-left:4px solid #3c8dbc;
              padding:10px 14px; border-radius:4px; font-size:13px;
              margin-top:12px; box-sizing:border-box; max-width:100%;",
    icon("info-circle", style="color:#3c8dbc; margin-right:6px;"),
    "For more detailed information about the datasets shown above, please visit the ",
    tags$a(href="https://www.simpler4health.se/w/sh/en/researchers/available-data", target="_blank", "SIMPLER website", .noWS="after"),
    ".",
    tags$br(),
    "For a full description of the cohort, read the SIMPLER white paper: ",
    tags$a(
      href     = "whitepaper/SIMPLER_European_Journal_of_Epidemiology.pdf",
      download = "SIMPLER_European_Journal_of_Epidemiology.pdf",
      target   = "_blank",
      "download PDF", .noWS = "after"
    ),
    " or ",
    tags$a(
      href   = "https://link.springer.com/article/10.1007/s10654-026-01455-6",
      target = "_blank",
      "view the article (doi:10.1007/s10654-026-01455-6)", .noWS = "after"
    ),
    "."
  )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  
  # Spinner animation definition
  tags$head(
    # ---- CSS ----
    # spinner animation; DataTables font/padding (smaller on phones); selected-cell colour;
    # info icon + tooltip styles (the tooltip text itself is shown by the JS at the end of <head>);
    # stacking order of the sidebar (keeps the 'i' icons clickable); clickable pickerInput group headers
    tags$style(HTML("@keyframes spin { to { transform: rotate(360deg); } }
                    
          table.dataTable td {
          padding: 8px !important;
          }
          table.dataTable {
            font-size: 12px;
          }
          .dataTables_wrapper {
            overflow-x: auto;
            width: 100%;
          }
          @media (max-width: 768px) {
            table.dataTable {
              font-size: 8px;
            }
             table.dataTable td, table.dataTable th {
              padding: 4px !important;
            }
          }
          
          
          
table.dataTable tbody td.selected {
      background-color: #ffe082 !important;
}


 .fa-info-circle {
      color: #3c8dbc !important;
      font-size: 16px !important;
      opacity: 1 !important;
    }

    .btn {
      opacity: 1 !important;
    }


    .info-tooltip {
      position: relative;
      display: inline-block;
      cursor: pointer;
      color: #3c8dbc;
      font-size: 16px;
    }

    .info-tooltip .tooltip-box { display: none !important; }

    #global-tooltip {
      display: none;
      position: fixed;
      z-index: 100000;
      width: 320px;
      background-color: #fff;
      color: #333;
      border: 1px solid #ccc;
      border-radius: 8px;
      padding: 10px 14px;
      font-size: 13px;
      line-height: 1.5;
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      white-space: normal;
      word-break: break-word;
      pointer-events: none;
    }
    
     .col-sm-2 > form.well { position: relative; z-index: 10; }
    
     .dropdown-header {
      cursor: pointer;
      user-select: none;
    }
    .dropdown-header:hover {
      background-color: #f0f0f0;
    }
    
")),
    
    # ---- JS 1: click on a group header of a pickerInput selects/deselects the whole group ----
    tags$script(HTML("
 
  document.addEventListener('click', function(e) {
 
    // Find the clicked optgroup header, if any
    var header = e.target.closest('.dropdown-header');
    if (!header) return;
 
    // The header's class looks like 'dropdown-header optgroup-N' (1-indexed)
    var classes = header.className || '';
    var m = classes.match(/optgroup-(\\d+)/);
    if (!m) return;
    var groupIndex = parseInt(m[1], 10);
 
    // The toggle button that opened this dropdown carries data-id = the
    // id of the underlying <select> element (even when the menu itself
    // has been detached and appended to <body> by bootstrap-select)
    var toggle = document.querySelector('.dropdown-toggle[aria-expanded=\"true\"]');
    if (!toggle) return;
 
    var selectId = toggle.getAttribute('data-id');
    if (!selectId) return;
 
    var $select = $('#' + selectId);
    if ($select.length === 0) return;
 
    var $optgroup = $select.find('optgroup').eq(groupIndex - 1);
    if ($optgroup.length === 0) return;
 
    var groupValues = $optgroup.find('option').map(function() {
      return $(this).val();
    }).get();
 
    var currentValues = $select.val() || [];
 
    // If every value in the group is already selected, this click deselects
    // the group; otherwise it selects the whole group (union with current selection)
    var allSelected = groupValues.every(function(v) {
      return currentValues.indexOf(v) !== -1;
    });
 
    var newValues;
    if (allSelected) {
      newValues = currentValues.filter(function(v) {
        return groupValues.indexOf(v) === -1;
      });
    } else {
      newValues = currentValues.slice();
      groupValues.forEach(function(v) {
        if (newValues.indexOf(v) === -1) newValues.push(v);
      });
    }
 
    // Prevent bootstrap-select's own header click behaviour, then apply.
    // selectpicker('val', ...) updates the UI AND fires the change event Shiny listens to.
    e.preventDefault();
    e.stopPropagation();
    $select.selectpicker('val', newValues);
    $select.selectpicker('refresh'); // force redraw of button text/count and checkmarks
 
  }, true); // capture phase: run before bootstrap-select's own handlers can stop the event
 
")),
    # ---- JS 2: toggle of variable buttons (sends input$clicked_var) and closing of popovers on outside click ----
    tags$script(HTML("
    $(document).on('click', '.var-btn', function() {
 
      var val = $(this).data('val');
 
      // enviar a Shiny
      Shiny.setInputValue('clicked_var', val, {priority: 'event'});
 
      // toggle visual
      $(this).toggleClass('btn-primary btn-light');
    });
    
    
 
$(document).on('click', function(e) {
 
    $('[data-toggle=\"popover\"]').each(function () {
 
      var trigger = $(this);
 
      if (
        trigger.is(e.target) ||                     
        trigger.has(e.target).length > 0 ||         
        $('.popover').has(e.target).length > 0      
      ) {
        return;  
        }
 
      trigger.popover('hide');
    });
 
  });
 
 
  ")),
    # ---- JS 3: Explore Variable tab: variable buttons, row/column/select-all toggles, manifest link
    # and GWAS download trigger. Each action sets an input value that the module observes. ----
    tags$script(HTML('
      $(document).on("click", ".ev-var-btn", function(e) {
        e.stopPropagation();
        var ds  = $(this).data("ds");
        var val = String($(this).data("val"));
        $(this).toggleClass("btn-primary btn-secondary");
        Shiny.setInputValue("ev_cell_click", {ds:ds, val:val, ts:Date.now()}, {priority:"event"});
      });
 
      $(document).on("change", ".ev-row-check", function() {
        var ds   = $(this).data("ds");
        var vals = String($(this).data("vals")).split("|||");
        var chk  = $(this).is(":checked");
        $.each(vals, function(i,v) {
          if (!v) return;
          var btn = $(".ev-var-btn[data-ds=\\""+ds+"\\"][data-val=\\""+v+"\\"]");
          if (chk) { btn.removeClass("btn-secondary").addClass("btn-primary"); }
          else     { btn.removeClass("btn-primary").addClass("btn-secondary"); }
        });
        Shiny.setInputValue("ev_row_check", {ds:ds, vals:vals, checked:chk, ts:Date.now()}, {priority:"event"});
      });
 
      $(document).on("click", ".ev-col-select-btn", function(e) {
        e.stopPropagation();
        var ds   = $(this).data("ds");
        var col  = String($(this).data("col"));
        var btns = $(".ev-var-btn[data-ds=\\""+ds+"\\"][data-col=\\""+col+"\\"]");
        var vals = [];
        btns.each(function(){ var v=String($(this).data("val")); if(v) vals.push(v); });
        var nSel     = btns.filter(".btn-primary").length;
        var deselect = (nSel === btns.length && btns.length > 0);
        btns.each(function(){
          if (deselect) { $(this).removeClass("btn-primary").addClass("btn-secondary"); }
          else          { $(this).removeClass("btn-secondary").addClass("btn-primary"); }
        });
        Shiny.setInputValue("ev_col_select", {ds:ds, col:col, vals:vals, deselect:deselect, ts:Date.now()}, {priority:"event"});
      });
 
      $(document).on("click", ".ev-select-all-btn", function(e) {
        e.stopPropagation();
        var ds   = $(this).data("ds");
        var btns = $(".ev-var-btn[data-ds=\\""+ds+"\\"]");
        var vals = [];
        btns.each(function(){ var v=String($(this).data("val")); if(v) vals.push(v); });
        var nSel     = btns.filter(".btn-primary").length;
        var deselect = (nSel === btns.length && btns.length > 0);
        btns.each(function(){
          if (deselect) { $(this).removeClass("btn-primary").addClass("btn-secondary"); }
          else          { $(this).removeClass("btn-secondary").addClass("btn-primary"); }
        });
        Shiny.setInputValue("ev_select_all", {ds:ds, vals:vals, deselect:deselect, ts:Date.now()}, {priority:"event"});
      });
      
      $(document).on("click", ".ev-manifest-link", function(e) {
  e.preventDefault();
  Shiny.setInputValue("ev_manifest_download", {manifest:$(this).data("manifest"), ts:Date.now()}, {priority:"event"});
});
Shiny.addCustomMessageHandler("ev_trigger_gwas_dl", function(msg) {
  var dl = document.getElementById("ev_gwas_dl");
  if (!dl) { console.error("ev_gwas_dl not found"); return; }
  var link = dl.tagName === "A" ? dl : dl.querySelector("a");
  if (!link) { console.error("No <a> inside ev_gwas_dl"); return; }
  link.click();
});
                     ')),
    # ---- JS 4: keeps the current variable selection (basket) in window.ev_selected ----
    tags$script(HTML('
  Shiny.addCustomMessageHandler("ev_update_basket", function(basket) {
    window.ev_selected = basket;
  });
')),
    
    # ---- JS 5: info-icon tooltips. A single body-level #global-tooltip (position: fixed) is filled and
    # positioned on mouseover/focus, so it is never hidden by the layout of the page. ----
    tags$script(HTML("
  (function() {
    var tip = null;
    function getTip() {
      if (!tip) {
        tip = document.createElement('div');
        tip.id = 'global-tooltip';
        document.body.appendChild(tip);
      }
      return tip;
    }
    function show(el) {
      var box = el.querySelector('.tooltip-box');
      if (!box) return;
      var t = getTip();
      t.innerHTML = box.innerHTML;
      t.style.display = 'block';
      var r = el.getBoundingClientRect();
      var w = t.offsetWidth, h = t.offsetHeight;
      // Prefer the right side of the icon; flip to the left if it does not fit
      var left = r.right + 8;
      if (left + w > window.innerWidth - 8) left = Math.max(8, r.left - w - 8);
      // Keep it inside the viewport vertically
      var top = r.top - 6;
      if (top + h > window.innerHeight - 8) top = Math.max(8, window.innerHeight - h - 8);
      t.style.left = left + 'px';
      t.style.top  = top + 'px';
    }
    function hide() { if (tip) tip.style.display = 'none'; }
    function tipEl(e) { return e.target.closest ? e.target.closest('.info-tooltip') : null; }

    document.addEventListener('mouseover', function(e) { var el = tipEl(e); if (el) show(el); });
    document.addEventListener('mouseout',  function(e) {
      var el = tipEl(e);
      if (el && !el.contains(e.relatedTarget)) hide();
    });
    document.addEventListener('focusin',  function(e) { var el = tipEl(e); if (el) show(el); });
    document.addEventListener('focusout', function(e) { if (tipEl(e)) hide(); });
  })();
"))
  ),
  
  
  # ------------------------------------------------------------
  
  # Top bar: SIMPLER logo (link to the website) on the left, visit counter on the right
  tags$div(
    style="display:flex; align-items:center; justify-content:space-between;
           padding:10px 20px; background-color:#f8f9fa;
           border-bottom:1px solid #ddd;",
    
    # LEFT: LOGO
    # NOTE: place logo.png inside www/ folder
    tags$a(
      href="https://www.simpler4health.se/w/sh/en", 
      target="_blank",
      title= "Go to SIMPLER website",
      
      tags$div(
        style="height:60px; display:flex; align-items:center; justify-content:flex-start;",
        
        tags$img(
          src="assets/simpler.jpg",
          
          style="height:45px; width:auto; object-fit:contain; cursor:pointer;"
        )
      )
    ),
    
    # RIGHT: COUNTER
    tags$div(
      style="font-size:14px;",
      textOutput("visit_counter")
    )
  ),
  
  # ------------------------------------------------------------
  # GLOBAL LOADING OVERLAY
  # IMPORTANT:
  # Controlled via show()/hide() in server
  # Must NOT include "display:flex" here
  # ------------------------------------------------------------
  tags$div(
    id = "loading-overlay",
    style="display:none; position:fixed; inset:0;
           background:rgba(255,255,255,0.75); z-index:9999;
           align-items:center; justify-content:center;
           flex-direction:column; gap:12px;",
    tags$div(style="width:40px;height:40px;border:4px solid #ddd;
                    border-top-color:#3c8dbc;border-radius:50%;
                    animation: spin 0.8s linear infinite;"),
    tags$div(style="font-weight:bold;","Running calculations...")
  ),
  
  # ------------------------------------------------------------
  # LAYOUT
  # 2-column UI:
  # - sidebar (inputs)
  # - mainPanel (outputs)
  # ------------------------------------------------------------
  sidebarLayout(
    
    # Sidebar: the controls shown depend on the active tab (conditionalPanel on input.tabs)
    sidebarPanel(width=2,
                 
                 # ---------------- TABLE CONTROLS ----------------
                 conditionalPanel("input.tabs=='Table'",
                                  pickerInput("main_var_tab","Stratify by (optional):",
                                              choices=c("No stratification" = "",var_choices_grouped)),
                                  pickerInput("var_tab","Variables (required):",
                                              choices=var_choices_grouped,
                                              multiple=TRUE,
                                              options=list(`actions-box`=TRUE, `live-search` = TRUE)),
                                  
                                  # Hint under the pickers (text generated in the server)
                                  uiOutput("table_hint"),
                                  
                                  div(style="display:flex; align-items:center; gap:6px;",
                                      
                                      actionButton("run_table","Run table",
                                                   icon=icon("table"),
                                                   class="btn-primary",
                                                   style="flex-grow:1;"),
                                      
                                      info_button(
                                        "run_table",
                                        "Generate a frequency table using the selected variables. Optionally stratify by a selected variable or view results for the full cohort. Combinations with fewer than 10 participants per cell are excluded.",
                                        "Run table"
                                      )
                                  )
                                  ,
                                  br(),br(),
                                  div(style="display:flex; align-items:center; gap:6px;",
                                      
                                      downloadButton("download_table","Download table"),
                                      
                                      info_button(
                                        "download_table",
                                        "Download the table as a Word document (.docx), ready to use or share.",
                                        "Download"
                                      )
                                  )
                 ),
                 conditionalPanel("input.tabs=='Explore Variable'",
                                  explore_variable_sidebar_ui(dataset_choices)
                 ),
                 
                 # ---------------- PLOT 2 CONTROLS ----------------
                 conditionalPanel("input.tabs=='Combination Plot'",
                                  pickerInput("over_var","Variables (max 10):",
                                              choices=var_choices_grouped,
                                              multiple=TRUE,
                                              options=list(`actions-box`=TRUE, `live-search` = TRUE,
                                                           `max-options`=10,
                                                           `max-options-text`="No more than 10 variables allowed")),
                                  
                                  uiOutput("plot2_var_hint"),
                                  pickerInput("over_row","Row (optional):",
                                              choices=c("No selection" = "", var_choices_grouped),
                                              selected = ""),
                                  pickerInput("over_col","Column (optional):",
                                              choices=c("No selection" = "",var_choices_grouped),
                                              selected = ""),
                                  numericInput(
                                    "min_size_plot2",
                                    "Minimum intersection size:",
                                    value = 10,
                                    min = 10
                                  ),
                                  numericInput(
                                    "min_degree_plot2",
                                    "Minimum sets combined:",
                                    value = 1,
                                    min = 1
                                  ),
                                  
                                  div(style="display:flex; align-items:center; gap:6px;",
                                      
                                      actionButton("run_plot2","Run combination plot",
                                                   icon=icon("project-diagram"),
						   class="btn-primary",
                                                   style="flex-grow:1;"),
                                      
                                      info_button(
                                        "run_plot2",
                                        "Generate an intersection plot (UpSet plot) to visualize the overlap between variables. Use the ‘Variables’ selection to choose which variables to include (maximum 10 variables). Use the ‘Row’ dropdown to select the variable for the stacked bar plot across rows. Use the ‘Column’ dropdown to select the variable for the stacked bar plot across columns. Set the minimum intersection size to control the smallest number of participants an intersesction must have to be displayed. Set the minimum sets combined to control the smallest number of variables that must overlap together to be displayed.",
                                        "Combination Plot"
                                      )
                                  ),
                                  
                                  br(),br(),
                                  div(style="display:flex; align-items:center; gap:6px;",
                                      
                                      downloadButton("download_plot2","Download combination plot"),
                                      
                                      info_button(
                                        "download_plot2",
                                        "Download the plot in high-quality PDF format.",
                                        "Download"
                                      )
                                  )
                 )
                 
                 
    ),
    
    mainPanel(width=10,
              
              # Tabs. input$tabs is used by the conditionalPanels of the sidebar.
              tabsetPanel(
                id="tabs",
                
                # ============================================================
                # HOMEPAGE TAB
                # ============================================================
                tabPanel(
                  "Homepage",
                  div(style="max-width:900px; margin:auto;",
                      
                      tags$h2("SIMPLER Descriptive App", style="margin-bottom:20px;"),
                      
                      tags$p(style="font-size:14pt;",
                             "This application allows users to explore the SIMPLER dataset, estimate the number of participants eligible for a potential research project, and select the variables to be requested. ",
                             "Users can review variable frequencies and assess participant availability based on the selected variables."
                      ),
                      
                      tags$hr(),
                      
                      tags$h3("Important information"),
                      tags$ul(style="font-size:13pt;",
                              tags$li(
                                "Before using this app, we strongly encourage all SIMPLER users to visit the ",
                                tags$a(href="https://www.simpler4health.se/w/sh/en/", target="_blank", "SIMPLER website"),
                                " and read the available information about SIMPLER, including the ",
                                tags$a(href="https://www.simpler4health.se/w/sh/en/researchers/rules-and-guidelines", target="_blank", "Rules and Guidelines", .noWS="after"),
                                ". This will help you understand the study, how to request data, and the proper channels to follow — saving time for both users and the project."
                              ),
                              tags$li(
                                "To better understand the study and the data available, we also encourage the users to read the SIMPLER white paper: ",
                                tags$a(
                                  href     = "whitepaper/SIMPLER_European_Journal_of_Epidemiology.pdf",
                                  download = "SIMPLER_European_Journal_of_Epidemiology.pdf",   # click downloads the manuscript
                                  target   = "_blank",
                                  "The Swedish infrastructure for medical population-based life-course and environmental research"
                                ),
                                ". Warensjö Lemming E, et al. ",
                                tags$i("Eur J Epidemiol"),
                                ". 2026 Oct 12. doi: ",
                                tags$a(
                                  href   = "https://link.springer.com/article/10.1007/s10654-026-01455-6",
                                  target = "_blank",
                                  "10.1007/s10654-026-01455-6",
                                  .noWS  = "after"
                                ),
                                "."
                              ),
                              tags$li(tags$b("Important — data protection notice: "),
                                      "The data available in this application is intended solely for the preparation of research proposals and not for research purposes. ",
                                      "SIMPLER does not permit any misuse of this application, including any attempt to re-identify participants, share data with unauthorized third parties, or use the data for purposes other than preparing a research proposal. "
                              ),
                              tags$li(
                                "To request data, please follow the instructions available on the ",
                                tags$a(href="https://www.simpler4health.se/w/sh/en/researchers/data-access", target="_blank", "SIMPLER website", .noWS="after"),
                                ", and submit your application through the ",
                                tags$a(href="https://uu.ilab.agilent.com/service_center/show_external/3659", target="_blank", "iLab system", .noWS="after"),
                                "."
                              ),
                              tags$li(tags$b("Data anonymization:"),
                                      "To ensure participant confidentiality, the dataset has been modified. ",
                                      sprintf(
                                        "A subset of %s participants out of %s has been removed so that any possible combination of variables contains at least 10 individuals.",
                                        format(n_original_cohort - nrow(dades), big.mark=","),
                                        format(n_original_cohort, big.mark=",")
                                      )
                              ),
                              
                              tags$li(
                                
                                "This application collects anonymous usage statistics (such as visits, interaction events, and country-level geographic information) ",
                                "to improve performance and usability. No personal data or identifiable information is stored. Usage logs are retained for 90 days and then automatically deleted."
                                
                              ),
                              tags$li(
                                "For getting exact numbers, studying rare conditions or if you encounter any bugs or have feedback, please contact: ",
                                tags$a(href="mailto:simpler@surgsci.uu.se", target="_blank","simpler@surgsci.uu.se", .noWS="after"),
                                "."
                              )
                              
                      ),
                      
                      tags$hr(),
                      
                      tags$h3("How to use the app"),
                      tags$ul(style="font-size:13pt;",
                              tags$li("Use the ", tags$b("Explore Variable"), " tab to view variables within each dataset and the estimated cost of a data request."),
                              tags$li("Use the ", tags$b("Table"), " tab for descriptive summaries at dataset level, not at variable level."),
                              tags$li("Use the ", tags$b("Combination Plot"), " tab for overlap analysis at dataset level, not at variable level."),
                              tags$li("Download results using the available buttons."),
                              tags$li("Click the search FAQ icon in the top-right corner to open a ",
                                      tags$b("non-AI assistant"),
                                      " that answers common questions based on a predefined list of FAQs (frequently asked questions).")
                      ),
                      
                      tags$hr(),
                      tags$h3("Acknowledgements"),
                      
                      # -------- FIRST ROW --------
                      fluidRow(
                        
                        column(3,
                               tags$div(
                                 style="height:120px; width:200px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.uu.se/en",
                                   target="_blank",
                                   title="Go to Uppsala University website",
                                   tags$img(
                                     src="assets/uppsala_university.jpg",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(3,
                               tags$div(
                                 style="height:120px; width:200px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.chalmers.se/en/",
                                   target="_blank",
                                   title="Go to Chalmers website",
                                   tags$img(
                                     src="assets/chalmers.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(3,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.oru.se/english/",
                                   target="_blank",
                                   title="Go to Örebro University website",
                                   tags$img(
                                     src="assets/orebro.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(3,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://ki.se/en",
                                   target="_blank",
                                   title="Go to Karolinska Institutet website",
                                   tags$img(
                                     src="assets/ki.jpg",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        )
                        
                      ),
                      
                      # -------- SECOND ROW --------
                      fluidRow(
                        
                        column(4,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://regionuppsala.se/",
                                   target="_blank",
                                   title="Go to Region Uppsala website",
                                   tags$img(
                                     src="assets/region_uppsala.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(4,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://regionvastmanland.se/",
                                   target="_blank",
                                   title="Go to Region Västmanland website",
                                   tags$img(
                                     src="assets/region_vastmanland.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(4,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.regionorebrolan.se/",
                                   target="_blank",
                                   title="Go to Region Örebro län website",
                                   tags$img(
                                     src="assets/region_orebro_lan.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        )
                        
                      ),
                      
                      # -------- THIRD ROW (former "Funders") --------
                      fluidRow(
                        
                        column(3,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.vr.se/english.html",
                                   target="_blank",
                                   title="Go to Swedish Research Council",
                                   tags$img(
                                     src="assets/vr.jpg",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(3,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://oes.se/",
                                   target="_blank",
                                   title="Go to Stiftelsen Olle Engkvist Byggmästare",
                                   tags$img(
                                     src="assets/olle_logo.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        
                        column(3,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.naiss.se/",
                                   target="_blank",
                                   title="Go to National Academic Infrastructure for Supercomputing in Sweden",
                                   tags$img(
                                     src="assets/naiss.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        ),
                        column(3,
                               tags$div(
                                 style="height:120px; display:flex; align-items:center; justify-content:center;",
                                 tags$a(
                                   href="https://www.uu.se/centrum/uppmax/",
                                   target="_blank",
                                   title="Go to Uppsala Multidisciplinary Center for Advanced Computational Science",
                                   tags$img(
                                     src="assets/uppmax_logo.png",
                                     style="max-height:90px; max-width:180px; object-fit:contain;"
                                   )
                                 )
                               )
                        )
                        
                      ),
                  )
                ),
                
                # ============================================================
                # OTHER TABS
                # Their content is generated by the server (uiOutput) or by the module
                # ============================================================
                
                tabPanel(
                  "Explore Variable",
                  
                  # DTOutput("explore_table")
                  explore_variable_tab_ui()
                ),
                # Table tab: disclaimer, table (HTML) and dataset info banner, all rendered by the server
                tabPanel("Table", br(), uiOutput("table_disclaimer"), uiOutput("restab"), uiOutput("table_info_banner")),
                tabPanel("Combination Plot", br(), uiOutput("plot2_disclaimer"), uiOutput("plot2_error_msg"), plotOutput("plot2", height="600px"), uiOutput("plot2_info_banner"))
              )
    )
    
  ),
  # ============================================================
  # CHATBOT 
  # ============================================================
  # Styles of the floating buttons (Search FAQ / How to apply / Apply) and of the chat panel
  tags$head(tags$style(HTML("
 
    #chat-fab {
      position: fixed;
      top: 8px;
      right: 15px;
      width: auto;
      height: 36px;
      padding: 0 16px;
      border-radius: 18px;
      background-color: #3c8dbc;
      color: white;
      border: none;
      font-size: 14px;
      font-weight: bold;
      cursor: pointer;
      box-shadow: 0 4px 12px rgba(0,0,0,0.25);
      z-index: 10001;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 6px;
    }
    
    #chat-fab:hover { background-color: #2e6da4; }
    #chat-panel {
      display: none;
      position: fixed;
      top: 136px;
      right: 15px;
      width: 360px;
      max-width: calc(100vw - 30px);
      max-height: 70vh;
      background: white;
      border-radius: 12px;
      box-shadow: 0 8px 24px rgba(0,0,0,0.2);
      z-index: 10000;
      flex-direction: column;
      overflow: hidden;
    }
    

    #howto-apply-btn, #apply-btn {
      position: fixed;
      right: 15px;
      width: auto;
      height: 36px;
      padding: 0 16px;
      border-radius: 18px;
      color: white;
      border: none;
      font-size: 14px;
      font-weight: bold;
      cursor: pointer;
      box-shadow: 0 4px 12px rgba(0,0,0,0.25);
      z-index: 10001;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 6px;
      text-decoration: none;
    }
    #howto-apply-btn { top: 52px; background-color: #78909c; }
    #howto-apply-btn:hover { background-color: #607d8b; color: white; }
    #apply-btn { top: 94px; background-color: #37474f; }
    #apply-btn:hover { background-color: #263238; color: white; }

    #chat-header {
      background-color: #3c8dbc;
      color: white;
      padding: 12px 16px;
      font-weight: bold;
      font-size: 14px;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    #chat-messages {
      flex: 1;
      overflow-y: auto;
      padding: 12px;
      max-height: 340px;
      font-size: 13px;
    }
    .chat-msg-user {
      background: #e3f2fd;
      border-radius: 8px 8px 0 8px;
      padding: 8px 10px;
      margin: 6px 0 6px 40px;
      color: #1a1a1a;
    }
    .chat-msg-bot {
      background: #f5f5f5;
      border-radius: 8px 8px 8px 0;
      padding: 8px 10px;
      margin: 6px 40px 6px 0;
      color: #1a1a1a;
      white-space: pre-wrap;
    }
    .chat-msg-bot a { color: #3c8dbc; }
    #chat-input-area {
      display: flex;
      padding: 8px;
      border-top: 1px solid #eee;
      gap: 6px;
    }
    #chat-input {
      flex: 1;
      border: 1px solid #ddd;
      border-radius: 6px;
      padding: 6px 10px;
      font-size: 13px;
      outline: none;
    }
    #chat-send {
      background-color: #3c8dbc;
      color: white;
      border: none;
      border-radius: 6px;
      padding: 6px 12px;
      cursor: pointer;
      font-size: 13px;
    }
    #chat-send:hover { background-color: #2e6da4; }
    
    @media (max-width: 768px) {
      #chat-fab, #howto-apply-btn, #apply-btn {
        font-size: 9px !important;
        padding: 0 8px !important;
        height: 24px !important;
        gap: 4px !important;
      }
      #chat-fab i, #howto-apply-btn i, #apply-btn i {
        font-size: 10px !important;
      }
      #chat-fab span, #howto-apply-btn, #apply-btn { white-space: nowrap; }
    }
 
  "))),
  
  # Chat JavaScript: open/close the panel; the button or Enter sends the text to input$chat_input
  tags$script(HTML("
 
    $(document).on('click', '#chat-fab', function() {
      var panel = $('#chat-panel');
      if (panel.css('display') === 'none') {
        panel.css('display', 'flex');
        $('#chat-input').focus();
      } else {
        panel.css('display', 'none');
      }
    });
 
    $(document).on('click', '#chat-send', function() {
      sendChatMessage();
    });
 
    $(document).on('keypress', '#chat-input', function(e) {
      if (e.which === 13) sendChatMessage();
    });
 
    function sendChatMessage() {
      var msg = $('#chat-input').val().trim();
      if (msg === '') return;
      Shiny.setInputValue('chat_input', msg, {priority: 'event'});
      $('#chat-input').val('');
    }
 
  ")),
  
  # tags$button(id = "chat-fab", HTML('<i class="fa fa-comments"></i>')),
  tags$button(id = "chat-fab", HTML('<i class="fa fa-search"></i> Search FAQ')),
  
  # Shortcut buttons to the data-access instructions and to the application system (iLab)
  tags$a(id = "howto-apply-btn",
         href = "https://www.simpler4health.se/w/sh/en/researchers/data-access",
         target = "_blank",
         HTML('<i class="fa fa-question-circle"></i> How to apply for data')),
  
  tags$a(id = "apply-btn",
         href = "https://uu.ilab.agilent.com/service_center/show_external/3659",
         target = "_blank",
         HTML('<i class="fa fa-file-signature"></i> Apply')),
  
  
  # Chat panel: header with close button, message area (starts with a greeting) and text input
  tags$div(id = "chat-panel",
           tags$div(id = "chat-header",
                    tags$span("SIMPLER Assistant"),
                    tags$span(style = "cursor:pointer; font-size:16px;",
                              onclick = "$('#chat-panel').css('display','none')",
                              HTML("&times;"))
           ),
           tags$div(id = "chat-messages",
                    tags$div(class = "chat-msg-bot",
                             "Hi! I can help you with questions about SIMPLER, the cohorts, questionnaires, variables, and how to use this app. What would you like to know?")
           ),
           tags$div(id = "chat-input-area",
                    tags$input(id = "chat-input", type = "text",
                               placeholder = "Search a question..."),
                    tags$button(id = "chat-send", "Send")
           )
  )
)





# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session){
  
  # Delete old log files (at most once per day, see purge_old_logs)
  purge_old_logs()
  
  
  # Public resource paths. NOTE: these folders are served WITHOUT login, even when shinymanager is active,
  # so only public files should live there (assets = logos, manifests = manifest files,
  # whitepaper = white paper PDF).
  addResourcePath(
    "assets",
    "www"
  )
  
  addResourcePath(
    "manifests",
    "manifests"
  ) 
  addResourcePath("whitepaper", "SIMPLER_white_paper")
  
  
  # Visitor id + country, computed once per session and reused by every log_event() call
  session_info <- reactiveVal(NULL)
  # ============================================================
  # INTERNAL VISIT COUNTER (GDPR SAFE)
  # ============================================================
  observe({
    session_info(get_user_info(session))
  }, priority = 1000)  # run early, before other observers that may log events
  
  # Register the visit once the first page has been rendered
  session$onFlushed(function() {
    log_visit(session)
  }, once = TRUE)
  
  # ------------------------------------------------------------
  # LOADING OVERLAY CONTROL
  # ------------------------------------------------------------
  # Local helpers (they take precedence over shinyjs::show/hide inside server)
  show <- function(){ runjs("$('#loading-overlay').css('display','flex')") }
  hide <- function(){ runjs("$('#loading-overlay').css('display','none')") }
  observe({ hide() })  # ensure hidden on startup
  
  # ------------------------------------------------------------
  # REACTIVE STORAGE
  # rv = current results
  # cache = memoization (avoid recomputation)
  # ------------------------------------------------------------
  
  # rv: current results of each tab; cache: results already computed for a given selection
  rv <- reactiveValues(
    table = NULL,
    explore_table = NULL,
    plot2 = NULL,
    plot2_error = NULL
    
  )
  cache <- reactiveValues(table=list(), 
                          plot2=list())
  
  # ============================================================
  # TABLE
  # ============================================================
  observeEvent(input$run_table,{
    log_event("run_table", session, info=session_info())
    
    req(input$var_tab)
    
    # Optional stratification variable ("" = whole cohort)
    main_var <- input$main_var_tab
    if (main_var == "") main_var <- NULL
    
    vars <- input$var_tab
    
    
    # ------------------------------------------------------------
    # CACHE KEY (IMPORTANT)
    # - must be a single unique string
    # - collapse used to avoid list index errors
    # ------------------------------------------------------------
    
    key <- paste(
      ifelse(is.null(main_var) || main_var == "", "NONE", main_var),
      paste(sort(vars), collapse="|"),
      sep="§"
    )
    
    
    # Cache hit → return immediately
    if(!is.null(cache$table[[key]])){
      rv$table <- cache$table[[key]]
      return()
    }
    
    show(); on.exit(hide(), add=TRUE)
    
    if (is.null(main_var) || main_var == "") {
      
      dat <- dades[, vars, drop = FALSE]
      
    } else {
      
      dat <- dades[, c(main_var, vars), drop = FALSE]
      
    }
    
    # Only the analysed variables (without the stratification variable)
    tmp <- dat[, vars, drop=FALSE]
    
    # ------------------------------------------------------------
    # FAST FILTERING
    # Removes rows where all variables are NO/NA
    # Key performance optimization
    # ------------------------------------------------------------
    if(ncol(tmp)==1){
      keep <- !is.na(tmp[[1]]) & tmp[[1]]!="NO"
    } else {
      keep <- rowSums(as.matrix(!(tmp %in% c("NO", NA)))) > 0
    }
    
    dat <- dat[keep,,drop=FALSE]
    
    if (!is.null(main_var) && main_var != "") {
      dat <- dat[!is.na(dat[[main_var]]),]
    }
    
    # Stops the calculation with a message when nothing is left after filtering
    validate(need(nrow(dat)>0,"No data left"))
    
    # ------------------------------------------------------------
    # PREPROCESSING MAIN VARIABLE
    # Numeric → grouped into quantiles
    # ------------------------------------------------------------
    if (!is.null(main_var) && main_var != "" && is.numeric(dat[[main_var]])) {
      dat[[main_var]] <- cut2(dat[[main_var]], g=5)
    }
    
    # ------------------------------------------------------------
    # compareGroups (computational bottleneck)
    # max.xlev = 4 → reduces factor complexity → faster
    # ------------------------------------------------------------
    
    formula_cg <- if (is.null(main_var) || main_var == "") {
      as.formula("~ .")
    } else {
      as.formula(paste(main_var, "~ ."))
    }
    
    
    res <- compareGroups(
      formula_cg,
      data = dat,
      max.xlev = 4,
      compute.p=FALSE,
      compute.ratio=F
    )
    
    tbl <- createTable(res, show.p.overall=FALSE, show.n = TRUE, hide.no="NO")
    
    # Save to cache
    cache$table[[key]] <- tbl
    rv$table <- tbl
  })
  
  # Render table (HTML)
  output$restab <- renderUI({
    req(rv$table)
    HTML(export2md(rv$table,strip=TRUE,first.strip=TRUE,format="html"))
  })
  
  output$table_disclaimer <- renderUI({
    req(rv$table)
    data_disclaimer_banner()
  })
  
  output$table_info_banner <- renderUI({
    req(rv$table)
    dataset_info_banner()
  })
  
  # Hint shown under the pickers: whole cohort vs stratified by the chosen variable
  output$table_hint <- renderUI({
    
    if (is.null(input$main_var_tab) || input$main_var_tab == "") {
      
      div(
        style = "color: #666; font-size: 13px;",
        icon("info-circle"),
        " Summary for the whole cohort"
      )
      
    } else {
      
      div(
        style = "color: #666; font-size: 13px;",
        icon("info-circle"),
        paste(" Stratified by", input$main_var_tab)
      )
      
    }
  })
  
  # Counter below the 'Variables' picker of the Combination Plot (max 10)
  output$plot2_var_hint <- renderUI({
    
    n_sel <- length(input$over_var)
    
    if (n_sel == 0) {
      
      div(
        style = "color: #666; font-size: 13px;",
        icon("info-circle"),
        " Select up to 10 variables"
      )
      
    } else if (n_sel > 10) {
      
      div(
        style = "color: #d9534f; font-size: 13px; font-weight: bold;",
        icon("exclamation-triangle"),
        sprintf(" %d variables selected — please select 10 or fewer", n_sel)
      )
      
    } else {
      
      div(
        style = "color: #666; font-size: 13px;",
        icon("info-circle"),
        sprintf(" %d of 10 variables selected", n_sel)
      )
      
    }
  })
  
  
  # ------------------------------------------------------------
  # TABLE DOWNLOAD (Word / .docx)
  # ------------------------------------------------------------
  output$download_table <- downloadHandler(
    filename = function(){ paste0("table_", Sys.Date(), ".docx") },
    content = function(file){
      
      log_event("download_table", session, info=session_info())
      req(rv$table)
      
      # export2word() writes the compareGroups table directly to a .docx file
      export2word(
        rv$table,
        file = file,
        strip = TRUE,
        first.strip = TRUE
      )
    }
  )
  
  # ============================================================
  # Explore variable table
  # ============================================================
  
  # NOTE: rv$explore_table and rv$selected_vars are not used anywhere else in app.R (leftover).
  rv$explore_table <- NULL
  rv$selected_vars <- character(0)
  
  # Server logic of the Explore Variable tab (explore_variable_module.R).
  # It receives the logging function and the per-session user info.
  ev_rv <- explore_variable_server(
    input, output, session,
    log_event_fn = log_event,
    session_info = session_info
  )
  
  # ============================================================
  # CHATBOT SERVER
  # ============================================================
  observeEvent(input$chat_input, {
    
    req(input$chat_input)
    query <- input$chat_input
    
    # Show the user message (safe escaping via JSON)
    runjs(sprintf("
      $('#chat-messages').append($('<div class=\"chat-msg-user\"></div>').text(%s));
      $('#chat-messages').scrollTop($('#chat-messages')[0].scrollHeight);
    ", jsonlite::toJSON(query, auto_unbox = TRUE)))
    
    # Best matching FAQ entries, or NULL when nothing is relevant
    results <- search_faq(query, simpler_faq)
    
    if (is.null(results)) {
      # Fallback message when no FAQ matches
      response <- "I don't have specific information about that. For detailed questions, please contact the SIMPLER office at simpler@surgsci.uu.se"
    } else {
      response <- paste(sapply(results, function(r) r$answer), collapse = "\n\n---\n\n")
    }
    
    # Literal "\n" sequences stored in the CSV become real line breaks
    response <- gsub("\\n", "\n", response, fixed = TRUE)
    
    runjs(sprintf("
      $('#chat-messages').append($('<div class=\"chat-msg-bot\"></div>').text(%s).css('white-space','pre-wrap'));
      $('#chat-messages').scrollTop($('#chat-messages')[0].scrollHeight);
    ", jsonlite::toJSON(response, auto_unbox = TRUE)))
    
    log_event("chat_query", session, info = session_info())
  })
  
  
  
  # ============================================================
  # COMBINATION PLOT (UpSet)
  # Checks the selection, uses the cache when possible, otherwise builds the plot
  # ============================================================
  observeEvent(
    input$run_plot2, {
      
      log_event("run_plot2", session, info = session_info())
      
      # Validation: at most 10 variables, and min. sets combined <= number of variables
      if (length(input$over_var) > 10) {
        rv$plot2 <- NULL
        rv$plot2_error <- "Please select 10 variables or fewer for the Combination Plot."
        return()
      }
      
      if (input$min_degree_plot2 > length(input$over_var)) {
        rv$plot2 <- NULL
        rv$plot2_error <- sprintf(
          "Minimum sets combined (%d) cannot be greater than the number of selected variables (%d).",
          input$min_degree_plot2, length(input$over_var)
        )
        return()
      }
      rv$plot2_error <- NULL
      
      # Row / column variables are optional ("" -> NULL)
      row_var <- if (is.null(input$over_row) || input$over_row == "") NULL else input$over_row
      col_var <- if (is.null(input$over_col) || input$over_col == "") NULL else input$over_col
      
      # ------------------------------------------------------------
      # CACHE KEY FIX (CRITICAL)
      # collapse required to avoid indexing crash
      # ------------------------------------------------------------
      key <- paste(
        paste(sort(input$over_var), collapse = "|"),
        ifelse(is.null(row_var), "NONE", row_var),
        ifelse(is.null(col_var), "NONE", col_var),
        input$min_size_plot2, 
        input$min_degree_plot2,
        sep = "§"
      )
      
      if (!is.null(cache$plot2[[key]])) {
        rv$plot2 <- cache$plot2[[key]]
        return()
      }
      
      show(); on.exit(hide(), add = TRUE)
      
      # Variables that define the intersections (row/column variables are excluded)
      vars <- setdiff(input$over_var,
                      c(row_var, col_var))
      
      validate(need(length(vars) > 0, "Select valid overlap variables"))
      
      dat  <- dades
      vars <- vars[vars %in% names(dat)]
      
      # Binary encoding for intersection variables
      dat[, vars] <- lapply(dat[, vars, drop = FALSE],
                            function(x) ifelse(x == "NO", 0, 1))
      
      tmp <- dat[, vars, drop = FALSE]
      
      # Safe filtering
      if (ncol(tmp) == 1) {
        keep <- !is.na(tmp[[1]]) & tmp[[1]] == 1
      } else {
        keep <- rowSums(as.matrix(tmp), na.rm = TRUE) > 0
      }
      
      dat <- dat[keep, , drop = FALSE]
      
      validate(need(nrow(dat) > 0, "No valid data"))
      
      # Factor conversion for grouping (only if selected)
      if (!is.null(row_var)) dat[[row_var]] <- factor(dat[[row_var]])
      if (!is.null(col_var)) dat[[col_var]] <- factor(dat[[col_var]])
      
      # ------------------------------------------------------------
      # set_sizes layer: colored by row_var if selected, dark grey otherwise
      # ------------------------------------------------------------
      set_sizes_layer <- if (!is.null(row_var)) {
        upset_set_size(
          geom = geom_bar(aes(fill = .data[[row_var]]), width = 0.4)
        ) + labs(fill = row_var)
      } else {
        upset_set_size(
          geom = geom_bar(fill = "grey20", width = 0.4)
        )
      }
      
      # ------------------------------------------------------------
      # intersection_size layer: colored by col_var if selected, black otherwise
      # ------------------------------------------------------------
      intersection_layer <- if (!is.null(col_var)) {
        intersection_size(
          aes(fill = .data[[col_var]]),
          width = 0.75,
          text = list(size = 4)
        )
      } else {
        intersection_size(
          fill = "black",
          width = 0.75,
          text = list(size = 4)
        )
      }
      
      
      # Build plot
      p <- ComplexUpset::upset(
        dat, vars, name = "Variable",
        min_degree = input$min_degree_plot2,
        min_size = input$min_size_plot2,
        
        set_sizes = set_sizes_layer +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(linetype=2), 
            panel.grid.minor = element_blank(),
            text = element_text(size = 20),
            axis.ticks.x = element_blank(),
            legend.position = "top"
          ),
        
        base_annotations = list(
          "Intersection size" = intersection_layer +
            theme(
              panel.grid.major.y = element_line(linetype=2), 
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              text = element_text(size = 20),
              axis.line = element_line(colour = "black"),
              axis.ticks.x = element_blank(),
              panel.grid.major = element_blank(),
              legend.position = "top"
            )
        ),
        
        stripes = upset_stripes(
          geom = geom_segment(linewidth = 12),
          colors = c("grey95", "white")
        )
        
      ) +
        theme(
          panel.grid.major.y = element_line(linetype=2), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 20),
          axis.line = element_line(colour = "black"),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "top"
        )
      
      cache$plot2[[key]] <- p
      rv$plot2 <- p
    })
  
  # Red error box shown above the plot when the selection is not valid
  output$plot2_error_msg <- renderUI({
    req(rv$plot2_error)
    div(
      style = "background:#f8d7da; border-left:4px solid #dc3545;
                padding:10px 14px; border-radius:4px; font-size:13px;
                margin-bottom:12px; box-sizing:border-box; max-width:100%;
                color:#721c24;",
      icon("exclamation-circle", style="margin-right:6px;"),
      rv$plot2_error
    )
  })
  
  # The plot object is stored in rv$plot2 and rendered here
  output$plot2 <- renderPlot({ req(rv$plot2); rv$plot2 })
  
  
  output$plot2_disclaimer <- renderUI({
    req(rv$plot2)
    data_disclaimer_banner()
  })
  
  output$plot2_info_banner <- renderUI({
    req(rv$plot2)
    dataset_info_banner()
  })
  
  # Download of the current plot as PDF
  output$download_plot2 <- downloadHandler(
    filename = "plot2.pdf",
    content = function(file) {
      log_event("download_plot2",session)
      
      ggsave(file, rv$plot2, width = 12, height = 8)
    }
  )
  
  
}


# ============================================================
# RUN APP
# ============================================================

# Optional login: only when both environment variables are defined.
# Example:  SIMPLER_USER=simpler SIMPLER_PASS='long-password' Rscript -e "shiny::runApp('app.R')"
auth_user <- Sys.getenv("SIMPLER_USER")
auth_pass <- Sys.getenv("SIMPLER_PASS")
auth_on   <- nzchar(auth_user) && nzchar(auth_pass)

if (auth_on) {
  ui_final <- shinymanager::secure_app(ui)
  server_final <- function(input, output, session) {
    shinymanager::secure_server(
      check_credentials = function(user, password) {
        list(result = identical(user, auth_user) && identical(password, auth_pass))
      }
    )
    server(input, output, session)
  }
} else {
  ui_final     <- ui
  server_final <- server
}

# Without credentials in the environment the app starts without login
shinyApp(ui_final, server_final)











