# ============================================================
# SIMPLER FAQ CHATBOT DATA
# Keyword-based FAQ search used by the 'Search FAQ' assistant (no AI involved).
#
# How it works:
#   1. faq/simpler_faq.csv has two columns: `keywords` (comma-separated) and `answer`.
#   2. At startup every keyword is tokenised (lower-case, accents folded, stopwords removed,
#      plurals stemmed, synonyms mapped to one canonical word) and an IDF weight is computed,
#      so rare words count more than words that appear in many FAQs.
#   3. For every question, each FAQ gets a score (exact token matches + multi-word keyword
#      bonuses + typo tolerance). The best one or two answers are returned, or NULL when
#      nothing is relevant enough (app.R then shows the 'contact us' message).
#
# Used from app.R:   source('R/faq_search_block.R')   and   search_faq(query, simpler_faq)
# Debugging:         faq_debug('your question', simpler_faq)   (run in the R console)
# ============================================================
# Reads the FAQ CSV into a list of list(keywords = <character vector>, answer = <text>).
# Answers may contain the literal two characters \n for line breaks (converted in app.R).
load_faq <- function(path = "faq/simpler_faq.csv") {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  lapply(seq_len(nrow(df)), function(i) {
    list(
      keywords = trimws(unlist(strsplit(df$keywords[i], ","))),
      answer   = df$answer[i]
    )
  })
}

# ------------------------------------------------------------
# Text normalisation helpers
# ------------------------------------------------------------
# Words ignored when matching: too common to be informative
faq_stopwords <- c("a","an","the","is","are","was","were","be","do","does","did",
                   "can","could","should","would","i","we","you","my","our","your",
                   "me","to","of","in","on","at","for","from","with","and","or",
                   "how","what","where","when","which","who","why","about","this",
                   "that","there","it","its","any","some","get","have","has","need",
                   "want","please","tell","much","many")

# canonical word = variants users might type (the canonical word is added automatically)
faq_synonyms <- list(
  cost        = c("price","pricing","fee","sek","charge","expensive","payment","invoice"),
  apply       = c("application","request","submit","ilab","applying","applied"),
  participant = c("people","subject","individual","person"),
  sex         = c("gender","sexes"),
  women       = c("female","woman"),
  men         = c("male","man"),
  register    = c("registry","registries"),
  drug        = c("medication","medicine","prescription","prescribed"),
  protein     = c("proteomic","proteome"),
  metabolomic = c("metabolome"),
  genetic     = c("genetics","genomic","genomics","genome","genotype","genotyping","snp","gwas"),
  fecal       = c("faecal","feces","faeces"),
  analysis    = c("analyse","analyze","analysing","analyzing"),
  anonymous   = c("anonymization","anonymisation","anonymised","anonymized",
                  "anonymity","identifiable","identify"),
  death       = c("mortality","died","dead"),
  microbiome  = c("microbiota")
)

# Normalises free text: lower-case, fold accented letters, punctuation -> space
faq_normalize <- function(x) {
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- chartr("\u00e5\u00e4\u00f6\u00e9\u00e8", "aaoee", x)   # fold accented letters
  x <- gsub("[^a-z0-9]+", " ", x)                             # punctuation -> space
  x <- gsub("\\b(ffq|hq|cycle)\\s+([0-9]+)", "\\1\\2", x, perl = TRUE)  # "ffq 1997" -> "ffq1997"
  trimws(x)
}

# Light stemming (plurals only); short words/acronyms are left untouched
faq_stem <- function(w) {
  long <- nchar(w) > 3
  w[long] <- sub("ies$", "y", w[long])
  w[long] <- sub("([^s])s$", "\\1", w[long])
  w
}

# stemmed variant -> canonical word (canonical word maps to itself)
faq_syn_lookup <- unlist(lapply(names(faq_synonyms), function(canon) {
  v <- unique(c(canon, faq_synonyms[[canon]]))
  setNames(rep(canon, length(v)), faq_stem(v))
}))

# Text -> vector of unique tokens: normalised, stopwords removed, stemmed, synonyms replaced by their canonical word
faq_tokens <- function(x) {
  x <- faq_normalize(x)
  if (!nzchar(x)) return(character(0))
  w <- unlist(strsplit(x, " ", fixed = TRUE))
  w <- w[!(w %in% faq_stopwords) & nchar(w) >= 2]
  w <- faq_stem(w)
  hit <- w %in% names(faq_syn_lookup)
  w[hit] <- unname(faq_syn_lookup[w[hit]])
  unique(w)
}

# ------------------------------------------------------------
# Index: tokenise keywords once at startup and compute IDF
# (tokens used by many FAQs weigh less than tokens used by few)
# ------------------------------------------------------------
faq_prepare <- function(faq_data) {
  faq_data <- lapply(faq_data, function(item) {
    kws <- item$keywords[nzchar(trimws(item$keywords))]   # drop empty keywords
    item$kw_tokens  <- Filter(length, lapply(kws, faq_tokens))
    item$all_tokens <- unique(unlist(item$kw_tokens))
    # multi-word keywords, normalised, to detect contiguous phrases in the query
    kw_norm <- unique(faq_normalize(kws))
    item$kw_phrases <- Filter(function(p) length(strsplit(p, " ", fixed = TRUE)[[1]]) > 1, kw_norm)
    item
  })
  n    <- length(faq_data)
  dfq  <- table(unlist(lapply(faq_data, function(i) i$all_tokens)))
  idf  <- setNames(as.numeric(log(1 + n / dfq)), names(dfq))
  attr(faq_data, "idf") <- idf
  faq_data
}

# Loaded and indexed ONCE when the app starts.
# NOTE: the path is relative to the working directory, and the app will not start if the CSV is missing.
simpler_faq <- faq_prepare(load_faq("faq/simpler_faq.csv"))

# ============================================================
# FAQ SEARCH FUNCTION
# ============================================================
# Scores every FAQ entry against the query (one number per FAQ, higher = better match).
# Used by search_faq() and faq_debug().
faq_score <- function(query, faq_data) {
  if (is.null(attr(faq_data, "idf"))) faq_data <- faq_prepare(faq_data)
  idf      <- attr(faq_data, "idf")
  q_tokens <- faq_tokens(query)
  if (length(q_tokens) == 0) return(rep(0, length(faq_data)))
  q_alpha  <- q_tokens[!grepl("[0-9]", q_tokens)]
  qn       <- faq_normalize(query)
  
  vapply(faq_data, function(item) {
    toks <- item$all_tokens
    if (length(toks) == 0) return(0)
    
    # 1) exact token matches, each token counted once, weighted by IDF
    hit   <- toks[toks %in% q_tokens]
    score <- sum(idf[hit])
    
    # 2) multi-word keywords: +1 if all their tokens are in the query (any order),
    #    +1 more if the phrase appears contiguously in the query
    score <- score + sum(vapply(item$kw_tokens, function(kt) {
      length(kt) > 1 && all(kt %in% q_tokens)
    }, logical(1)))
    score <- score + sum(vapply(item$kw_phrases, function(p) {
      grepl(paste0("\\b", p, "\\b"), qn, perl = TRUE)
    }, logical(1)))
    
    # 3) typo tolerance: long alphabetic tokens with edit distance <= 1
    miss <- setdiff(toks, hit)
    miss <- miss[nchar(miss) >= 5 & !grepl("[0-9]", miss)]
    if (length(miss) > 0 && length(q_alpha) > 0) {
      near <- vapply(miss, function(m) any(adist(m, q_alpha) <= 1), logical(1))
      score <- score + 0.75 * sum(idf[miss[near]])
    }
    score
  }, numeric(1))
}

# Returns the best-matching FAQ entries (a list of list(keywords, answer)),
# or NULL when no entry is relevant enough.
# min_score: below this the bot answers "I don't have information..."
# rel_cutoff: extra answers must score at least this fraction of the best one
search_faq <- function(query, faq_data, n_results = 2,
                       min_score = 1.2, rel_cutoff = 0.3) {
  scores <- faq_score(query, faq_data)
  top    <- max(scores)
  if (top < min_score) return(NULL)
  
  keep <- which(scores >= max(min_score, rel_cutoff * top))
  keep <- keep[order(scores[keep], decreasing = TRUE)]
  faq_data[head(keep, n_results)]
}

# Debug helper (run in the console, not used by the app)
faq_debug <- function(query, faq_data, n = 5) {
  s <- faq_score(query, faq_data)
  o <- head(order(s, decreasing = TRUE), n)
  data.frame(score = round(s[o], 2), faq_row = o,
             answer = substr(vapply(faq_data[o], function(i) i$answer, character(1)), 1, 70))
}
