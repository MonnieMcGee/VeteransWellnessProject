## ============================================================
## Project: Dallas Veteran Wellbeing Analysis
## Script: 03_build_analysis_corpus.R
## Purpose: Import, preprocess, quality-check, and freeze the Reddit
##          corpus used for ESFI development and downstream analyses.
## July 2026
## ============================================================

source("Code/00_project_setup.R")

## ============================================================
## 1) Define and verify raw input files
## ============================================================

vet_file <- file.path(
  rawdata_dir,
  "veteranThreads_January.csv"
)

dallas_file <- file.path(
  rawdata_dir,
  "dallasThreads_January.csv"
)

finance_file <- file.path(
  rawdata_dir,
  "financePostsOct.csv"
)

stopifnot(
  file.exists(vet_file),
  file.exists(dallas_file),
  file.exists(finance_file)
)

## ============================================================
## 2) Data ingestion
## ============================================================

vet_total <- readtext::readtext(
  vet_file,
  encoding = "UTF-8"
)

dallas_total <- readtext::readtext(
  dallas_file,
  encoding = "UTF-8"
)

finance_total <- readtext::readtext(
  finance_file,
  encoding = "UTF-8"
)

posts <- dplyr::bind_rows(
  vet_total,
  dallas_total,
  finance_total
)

raw_input_counts <- tibble::tibble(
  input = c("Veteran", "Dallas", "Finance"),
  rows = c(
    nrow(vet_total),
    nrow(dallas_total),
    nrow(finance_total)
  )
)

print(raw_input_counts)

stopifnot(
  nrow(vet_total) == 2493L,
  nrow(dallas_total) == 1581L,
  nrow(finance_total) == 171L,
  nrow(posts) == 4245L
)

## ============================================================
## 3) Construct analysis text and metadata
## ============================================================

df_vet <- posts |>
  dplyr::mutate(
    imported_doc_id = doc_id,
    timestamp = as.POSIXct(
      timestamp,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    year = lubridate::year(timestamp),
    full_text = dplyr::case_when(
      is.na(comment_id) ~ stringr::str_c(
        text,
        text.1,
        sep = " "
      ),
      TRUE ~ comment
    ),
    full_text = stringr::str_squish(full_text),
    source = dplyr::case_when(
      stringr::str_detect(
        imported_doc_id,
        "veteranThreads_January"
      ) ~ "Veteran",
      stringr::str_detect(
        imported_doc_id,
        "dallasThreads_January"
      ) ~ "Dallas",
      TRUE ~ "Finance"
    )
  ) |>
  dplyr::filter(year %in% 2020:2025) |>
  dplyr::mutate(
    document_id = sprintf(
      "reddit_%04d",
      dplyr::row_number()
    )
  )

message(
  "Rows after 2020-2025 filtering: ",
  nrow(df_vet)
)

print(
  table(
    df_vet$source,
    useNA = "ifany"
  )
)

source_counts <- table(df_vet$source)

stopifnot(
  nrow(df_vet) == 3361L,
  unname(source_counts["Dallas"]) == 949L,
  unname(source_counts["Finance"]) == 159L,
  unname(source_counts["Veteran"]) == 2253L,
  !anyDuplicated(df_vet$document_id),
  !anyNA(df_vet$document_id),
  !anyNA(df_vet$source)
)

text_diagnostics <- df_vet |>
  dplyr::summarise(
    missing_text = sum(is.na(full_text)),
    empty_text = sum(
      is.na(full_text) |
        stringr::str_squish(full_text) == ""
    )
  )

print(text_diagnostics)

if (text_diagnostics$empty_text > 0L) {
  warning(
    "The filtered data contain missing or empty full_text values."
  )
}

## ============================================================
## 4) Create corpus
## ============================================================

corp_vet <- quanteda::corpus(
  df_vet,
  text_field = "full_text",
  docid_field = "document_id"
)

stopifnot(
  quanteda::ndoc(corp_vet) == 3361L,
  identical(
    quanteda::docnames(corp_vet),
    df_vet$document_id
  )
)

## ============================================================
## 5) Tokenization
## ============================================================

toks_vet_raw <- quanteda::tokens(
  corp_vet,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE
) |>
  quanteda::tokens_tolower()

## ============================================================
## 6) Compound common multiword place-name patterns
## ============================================================

# Always compound "fort <next>".
toks_vet_raw <- quanteda::tokens_compound(
  toks_vet_raw,
  pattern = quanteda::phrase("fort *")
)

# Always compound "san <next>".
toks_vet_raw <- quanteda::tokens_compound(
  toks_vet_raw,
  pattern = quanteda::phrase("san *")
)

# Compound "st <next>" and "saint <next>".
toks_vet_raw <- quanteda::tokens_compound(
  toks_vet_raw,
  pattern = quanteda::phrase(
    c("st *", "saint *")
  )
)

## ============================================================
## 7) Define frozen custom stopword list
## ============================================================

custom_stop <- unique(c(
  quanteda::stopwords("en"),

  # Conversational and platform noise
  "t", "s", "m", "re", "ve", "ll",
  "don", "don_t", "im", "ive", "youre", "theyre", "weve", "youve",
  "isnt", "arent", "didnt", "doesnt", "couldnt", "wouldnt", "wont",
  "just", "like", "really", "think",
  "also", "even", "one", "now", "still", "much", "right", "things",
  "got", "going", "will", "want", "https", "http", "www", "rt",

  # AutoMod and moderation artifacts
  "action_performed", "performed_automatically", "automatically_please",
  "please_contact", "contact_subreddit", "subreddit_message", "message_compose",
  "questions_concerns", "compose_veterans", "veterans_questions",
  "contact_moderators", "moderators", "moderators_subreddit",
  "bot", "bot_action", "friendly_reminder", "comment_removed"
))

junk_stop <- c(
  # HTML and URL artifacts
  "amp", "mailto", "png", "webp", "jpeg", "jpg", "gif", "svg",
  "href", "src", "utm", "utm_source", "utm_medium", "utm_campaign",
  "5bhttps", "3a", "2f", "5d",

  # Bot and moderation leftovers
  "remindmebot", "automatically", "performed", "compose", "subreddit",
  "moderators", "commenter", "comment", "removed", "delete", "deleted",
  "reminder", "subject", "message", "link", "click"
)

semantic_glue <- c(
  "good", "time", "make", "take", "year", "way", "well",
  "new", "lot", "sure", "said", "see", "day", "something", "people",
  "life", "look", "never", "someone", "anything",
  "thing", "went", "may", "us", "post", "thanks", "thank", "etc",
  "might", "told", "ago"
)

civility_stop <- c(
  "attacks", "attacks_slurs", "slurs", "bigotry", "bigotry_etc", "civil",
  "civil_disagreements", "disagreements", "contribute",
  "contribute_discussion", "discussion", "fine", "wrong", "calling",
  "calling_poopy-head", "yet"
)

custom_stop_final <- unique(c(
  custom_stop,
  junk_stop,
  semantic_glue,
  civility_stop,
  "can", "pm", "am",
  "dallas", "veteran", "veterans"
))

## ============================================================
## 8) Remove stopwords
## ============================================================

toks_vet_clean <- toks_vet_raw |>
  quanteda::tokens_remove(custom_stop_final) |>
  quanteda::tokens_remove(
    pattern = "^[a-z]$",
    valuetype = "regex"
  )

## ============================================================
## 9) Create unigrams and bigrams
## ============================================================

toks_vet_12 <- quanteda::tokens_ngrams(
  toks_vet_clean,
  n = 1:2
)

## ============================================================
## 10) Create and trim DFM
## ============================================================

dfm_vet <- quanteda::dfm(toks_vet_12) |>
  quanteda::dfm_trim(min_docfreq = 10) |>
  quanteda::dfm_trim(
    max_docfreq = 0.5,
    docfreq_type = "prop"
  )

nonempty <- quanteda::ntoken(dfm_vet) > 0

dfm_vet2 <- dfm_vet[nonempty, ]

message(
  "Documents after DFM trimming: ",
  quanteda::ndoc(dfm_vet2)
)

message(
  "Features after DFM trimming: ",
  quanteda::nfeat(dfm_vet2)
)

dfm_source_counts <- table(
  quanteda::docvars(dfm_vet2, "source")
)

print(dfm_source_counts)

stopifnot(
  length(nonempty) == 3361L,
  sum(nonempty) == 3076L,
  sum(!nonempty) == 285L,
  quanteda::ndoc(dfm_vet2) == 3076L,
  unname(dfm_source_counts["Dallas"]) == 867L,
  unname(dfm_source_counts["Finance"]) == 146L,
  unname(dfm_source_counts["Veteran"]) == 2063L
)

## ============================================================
## 11) Pre-freeze diagnostics
## ============================================================

bigram_count_pre_exclusion <- sum(
  grepl(
    "_",
    quanteda::featnames(dfm_vet2)
  )
)

message(
  "Bigram features retained: ",
  bigram_count_pre_exclusion
)

print(
  head(
    quanteda::featnames(dfm_vet2)[
      grepl(
        "_",
        quanteda::featnames(dfm_vet2)
      )
    ],
    30
  )
)

print(
  quanteda::topfeatures(
    dfm_vet2,
    50
  )
)

message(
  "Documents before empty-document removal: ",
  quanteda::ndoc(toks_vet_12)
)

message(
  "Nonempty documents: ",
  sum(nonempty)
)

message(
  "Empty documents removed: ",
  sum(!nonempty)
)

print(
  table(
    quanteda::docvars(toks_vet_12, "source"),
    useNA = "ifany"
  )
)

dfm_before_max <- quanteda::dfm(toks_vet_12) |>
  quanteda::dfm_trim(min_docfreq = 10)

removed_highfreq <- setdiff(
  quanteda::featnames(dfm_before_max),
  quanteda::featnames(dfm_vet2)
)

print(removed_highfreq)

## ============================================================
## 12) Identify potential event-calendar posts
## ============================================================

event_terms <- c(
  "comedy", "music", "theatre", "theater",
  "art", "arts", "show", "club",
  "performance", "event", "museum",
  "comedy_club", "live_music", "event_venue",
  "performing_arts", "music_hall", "arts_center",
  "comedy_show", "museum_art"
)

event_terms <- intersect(
  event_terms,
  quanteda::featnames(dfm_vet2)
)

if (length(event_terms) == 0L) {
  stop(
    "None of the event-calendar terms survived DFM trimming.",
    call. = FALSE
  )
}

event_score <- quanteda::rowSums(
  dfm_vet2[, event_terms]
)

summary(event_score)

event_docs <- data.frame(
  document_id = quanteda::docnames(dfm_vet2),
  source = quanteda::docvars(dfm_vet2, "source"),
  event_score = event_score,
  stringsAsFactors = FALSE
)

event_docs <- event_docs[
  order(
    event_docs$event_score,
    decreasing = TRUE
  ),
]

print(
  head(
    event_docs,
    30
  )
)

## ============================================================
## 13) Flag likely event-calendar posts
## ============================================================

event_threshold <- 250

event_post <- (
  quanteda::docvars(dfm_vet2, "source") == "Dallas" &
    event_score >= event_threshold
)

quanteda::docvars(dfm_vet2, "event_score") <- event_score
quanteda::docvars(dfm_vet2, "event_post") <- event_post

flagged_ids <- quanteda::docnames(dfm_vet2)[event_post]

print(flagged_ids)

print(
  table(
    source = quanteda::docvars(dfm_vet2, "source"),
    event_post = event_post
  )
)

stopifnot(
  length(flagged_ids) == 10L,
  all(
    quanteda::docvars(dfm_vet2, "source")[event_post] == "Dallas"
  )
)

## ============================================================
## 14) Create event-post review file
## ============================================================

corp_text <- as.character(corp_vet)

stopifnot(
  all(flagged_ids %in% quanteda::docnames(corp_vet))
)

flagged_texts <- corp_text[flagged_ids]

flagged_review <- data.frame(
  document_id = flagged_ids,
  source = quanteda::docvars(corp_vet, "source")[
    match(
      flagged_ids,
      quanteda::docnames(corp_vet)
    )
  ],
  event_score = event_score[
    match(
      flagged_ids,
      quanteda::docnames(dfm_vet2)
    )
  ],
  excerpt = substr(
    flagged_texts,
    1,
    1500
  ),
  stringsAsFactors = FALSE
)

## These documents are retained in the review file because they are
## weekly event calendars rather than organic community discussion.
## They share a templated structure containing dates, events, venues,
## cities, times, and descriptions. Their exclusion is based on document
## genre rather than substantive content.

## ============================================================
## 15) Create analysis corpus after event-calendar exclusion
## ============================================================

dfm_vet_analysis <- dfm_vet2[!event_post, ] |>
  quanteda::dfm_trim(min_docfreq = 10) |>
  quanteda::dfm_trim(
    max_docfreq = 0.5,
    docfreq_type = "prop"
  )

message(
  "Final analysis documents: ",
  quanteda::ndoc(dfm_vet_analysis)
)

message(
  "Final analysis features: ",
  quanteda::nfeat(dfm_vet_analysis)
)

print(
  table(
    quanteda::docvars(dfm_vet_analysis, "source"),
    useNA = "ifany"
  )
)

stopifnot(
  quanteda::ndoc(dfm_vet_analysis) == 3066L,
  quanteda::nfeat(dfm_vet_analysis) == 1681L
)

## ============================================================
## 16) Create aligned corpus and token objects
## ============================================================

analysis_ids <- quanteda::docnames(dfm_vet_analysis)

corp_vet_analysis <- corp_vet[analysis_ids]
toks_vet_analysis <- toks_vet_12[analysis_ids]

stopifnot(
  identical(
    quanteda::docnames(corp_vet_analysis),
    quanteda::docnames(toks_vet_analysis)
  ),
  identical(
    quanteda::docnames(toks_vet_analysis),
    quanteda::docnames(dfm_vet_analysis)
  ),
  quanteda::ndoc(dfm_vet_analysis) ==
    nrow(quanteda::docvars(dfm_vet_analysis))
)

print(
  table(
    quanteda::docvars(corp_vet_analysis, "source")
  )
)

print(
  table(
    quanteda::docvars(toks_vet_analysis, "source")
  )
)

print(
  table(
    quanteda::docvars(dfm_vet_analysis, "source")
  )
)

print(
  quanteda::topfeatures(
    dfm_vet_analysis,
    50
  )
)

bigram_features <- quanteda::featnames(dfm_vet_analysis)[
  grepl(
    "_",
    quanteda::featnames(dfm_vet_analysis)
  )
]

if (length(bigram_features) > 0L) {
  print(
    quanteda::topfeatures(
      dfm_vet_analysis[, bigram_features],
      50
    )
  )
}

## ============================================================
## 17) Create separate analysis metadata table
## ============================================================

analysis_metadata <- quanteda::docvars(dfm_vet_analysis) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    document_id = quanteda::docnames(dfm_vet_analysis),
    .before = 1
  )

stopifnot(
  nrow(analysis_metadata) == quanteda::ndoc(dfm_vet_analysis),
  identical(
    analysis_metadata$document_id,
    quanteda::docnames(dfm_vet_analysis)
  )
)

## ============================================================
## 18) Save derived data objects
## ============================================================

readr::write_csv(
  flagged_review,
  file.path(
    deriveddata_dir,
    "event_calendar_posts_removed.csv"
  )
)

saveRDS(
  flagged_review,
  file.path(
    deriveddata_dir,
    "event_calendar_posts_removed.rds"
  )
)

saveRDS(
  dfm_vet2,
  file.path(
    deriveddata_dir,
    "reddit_dfm_pre_event_exclusion.rds"
  )
)

saveRDS(
  dfm_vet_analysis,
  file.path(
    deriveddata_dir,
    "reddit_dfm_analysis_corpus.rds"
  )
)

saveRDS(
  toks_vet_analysis,
  file.path(
    deriveddata_dir,
    "reddit_tokens_analysis_corpus.rds"
  )
)

saveRDS(
  corp_vet_analysis,
  file.path(
    deriveddata_dir,
    "reddit_corpus_analysis_corpus.rds"
  )
)

saveRDS(
  analysis_metadata,
  file.path(
    deriveddata_dir,
    "reddit_analysis_metadata.rds"
  )
)

readr::write_csv(
  analysis_metadata,
  file.path(
    deriveddata_dir,
    "reddit_analysis_metadata.csv"
  )
)

saved_files <- c(
  "event_calendar_posts_removed.csv",
  "event_calendar_posts_removed.rds",
  "reddit_dfm_pre_event_exclusion.rds",
  "reddit_dfm_analysis_corpus.rds",
  "reddit_tokens_analysis_corpus.rds",
  "reddit_corpus_analysis_corpus.rds",
  "reddit_analysis_metadata.rds",
  "reddit_analysis_metadata.csv"
)

missing_files <- saved_files[
  !file.exists(file.path(deriveddata_dir, saved_files))
]

stopifnot(length(missing_files) == 0)

## ============================================================
## 19) Completion summary
## ============================================================

message("03_build_analysis_corpus.R completed successfully.")
message("Raw rows imported: ", nrow(posts))
message("Documents in 2020-2025 corpus: ", quanteda::ndoc(corp_vet))
message("Event-calendar posts removed: ", length(flagged_ids))
message("Documents in analysis corpus: ", quanteda::ndoc(dfm_vet_analysis))
message("Derived-data directory: ", deriveddata_dir)
