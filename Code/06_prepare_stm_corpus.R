###############################################################
## 06_prepare_stm_corpus.R
##
## Create the STM analysis corpus from the frozen ESFI corpus.
##
## This script:
##   1. Loads the frozen ESFI corpus.
##   2. Removes AutoModerator and moderator-generated artifacts.
##   3. Optionally removes pinned recurring informational threads.
##   4. Retrims the document-feature matrix.
##   5. Removes newly empty documents.
##   6. Saves STM-ready corpus objects.
##
## The frozen ESFI corpus is never modified.
###############################################################
## NOTE
## The STM corpus differs from the frozen ESFI corpus only by the
## removal of moderator-generated artifacts and recurring automated
## informational posts. The ESFI dictionary was developed and
## validated on the frozen corpus and was not altered after this point.

source("Code/00_project_setup.R")
##-------------------------------------------------------------
## 0. Load frozen analysis objects
##-------------------------------------------------------------

corpus_file <- file.path(
  deriveddata_dir,
  "reddit_corpus_analysis_corpus.rds"
)

tokens_file <- file.path(
  deriveddata_dir,
  "reddit_tokens_analysis_corpus.rds"
)

dfm_file <- file.path(
  deriveddata_dir,
  "reddit_dfm_analysis_with_esfi.rds"
)

reddit_corpus_analysis <- readRDS(corpus_file)
reddit_tokens_analysis <- readRDS(tokens_file)
reddit_dfm_analysis <- readRDS(dfm_file)

stopifnot(
  quanteda::is.corpus(reddit_corpus_analysis),
  quanteda::is.tokens(reddit_tokens_analysis),
  quanteda::is.dfm(reddit_dfm_analysis)
)

stopifnot(
  identical(
    quanteda::docnames(reddit_corpus_analysis),
    quanteda::docnames(reddit_tokens_analysis)
  ),
  identical(
    quanteda::docnames(reddit_tokens_analysis),
    quanteda::docnames(reddit_dfm_analysis)
  )
)

message(
  "Frozen analysis documents: ",
  quanteda::ndoc(reddit_dfm_analysis)
)

message(
  "Frozen analysis features: ",
  quanteda::nfeat(reddit_dfm_analysis)
)
stopifnot(
  quanteda::ndoc(reddit_dfm_analysis) == 3066,
  quanteda::nfeat(reddit_dfm_analysis) == 1681,
  all(
    c("source", "year", "esfi_hits", "esfi_present") %in%
      names(quanteda::docvars(reddit_dfm_analysis))
  ),
  sum(
    as.numeric(
      quanteda::docvars(
        reddit_dfm_analysis,
        "esfi_hits"
      )
    )
  ) == 428,
  sum(
    as.logical(
      quanteda::docvars(
        reddit_dfm_analysis,
        "esfi_present"
      )
    )
  ) == 268
)

# Create audit-output directory
stm_output_dir <- file.path(output_dir, "stm_corpus_preparation")
dir.create(stm_output_dir, recursive = TRUE, showWarnings = FALSE)

## 1. Extract text and metadata
doc_ids <- quanteda::docnames(
  reddit_corpus_analysis)

reddit_text <- as.character(
  reddit_corpus_analysis)

names(reddit_text) <- doc_ids

reddit_meta <- quanteda::docvars(
  reddit_corpus_analysis
) |>
  as.data.frame()

reddit_meta$esfi_hits <- as.numeric(
  quanteda::docvars(
    reddit_dfm_analysis,
    "esfi_hits"
  )
)

reddit_meta$esfi_present <- as.logical(
  quanteda::docvars(
    reddit_dfm_analysis,
    "esfi_present"
  )
)

stopifnot(
  nrow(reddit_meta) ==
    quanteda::ndoc(reddit_dfm_analysis)
)

stopifnot(
  identical(
    quanteda::docnames(reddit_corpus_analysis),
    quanteda::docnames(reddit_dfm_analysis)
  )
)
## Ensure optional metadata variables exist
if (!"author" %in% names(reddit_meta)) {
  reddit_meta$author <- NA_character_
}

if (!"title" %in% names(reddit_meta)) {
  reddit_meta$title <- NA_character_
}

reddit_meta <- reddit_meta |>
  mutate(
    doc_id = as.character(doc_ids),
    full_text_for_screening = stringr::str_squish(reddit_text),
    author_screen = stringr::str_to_lower(
      tidyr::replace_na(as.character(author), "")
    ),
    title_screen = stringr::str_squish(
      tidyr::replace_na(as.character(title), "")
    )
  )

## 2. Conservative artifact flags
reddit_meta <- reddit_meta |>
  mutate(
    flag_automod_author =
      str_detect(author_screen, "^automoderator$") |
      str_detect(author_screen, "remindmebot") |
      str_detect(author_screen, "moderatorbot"),

    flag_wiki_message =
      str_detect(
        full_text_for_screening,
        regex("^['\"“”‘’]*have you looked in the\\s+\\*{0,2}\\[?wiki",
              ignore_case = TRUE)
      ) |
      str_detect(
        full_text_for_screening,
        regex("^['\"“”‘’]*have you looked in the wiki for an answer",
              ignore_case = TRUE)
      ),

    flag_comment_removed =
      str_detect(
        full_text_for_screening,
        regex("^your comment was removed because", ignore_case = TRUE)
      ) |
      str_detect(
        full_text_for_screening,
        regex("^this comment (has been|was) removed", ignore_case = TRUE)
      ),

    flag_post_removed =
      str_detect(
        full_text_for_screening,
        regex("^(your|this) (post|submission) (has been|was) removed",
              ignore_case = TRUE)
      ),

    flag_locked_thread =
      str_detect(
        full_text_for_screening,
        regex("^(this thread has been locked|locked[.! ]|comments (are|have been) locked)",
              ignore_case = TRUE)
      ),

    flag_bot_signature =
      str_detect(
        full_text_for_screening,
        regex("i am a bot,? and this action was performed automatically",
              ignore_case = TRUE)
      ) |
      str_detect(
        full_text_for_screening,
        regex("this action was performed automatically",
              ignore_case = TRUE)
      ),

    flag_moderator_boilerplate =
      str_detect(
        full_text_for_screening,
        regex("please contact the moderators of this subreddit",
              ignore_case = TRUE)
      ) &
      str_detect(
        full_text_for_screening,
        regex("(bot|automatically|removed|questions or concerns)",
              ignore_case = TRUE)
      )
  )

## 3. Pinned informational candidates
## These are flagged for manual review and are not removed by default.
reddit_meta <- reddit_meta |>
  mutate(
    flag_pinned_candidate =
      str_detect(
        title_screen,
        regex("^(weekly|monthly|daily|official).*(thread|megathread|discussion|questions)",
              ignore_case = TRUE)
      ) |
      str_detect(
        title_screen,
        regex("(megathread|pinned thread|official discussion thread)",
              ignore_case = TRUE)
      ) |
      str_detect(
        full_text_for_screening,
        regex("^(welcome to the (weekly|monthly)|this is the (weekly|monthly).*(thread|megathread))",
              ignore_case = TRUE)
      )
  )

## Change to TRUE only after reviewing the pinned candidates.
remove_pinned_candidates <- FALSE

reddit_meta <- reddit_meta |>
  mutate(
    artifact_reason = case_when(
      flag_automod_author        ~ "AutoModerator or bot author",
      flag_wiki_message          ~ "Recurring Wiki informational message",
      flag_comment_removed       ~ "Comment-removal notice",
      flag_post_removed          ~ "Post/submission-removal notice",
      flag_locked_thread         ~ "Locked-thread notice",
      flag_bot_signature         ~ "Automated bot signature",
      flag_moderator_boilerplate ~ "Moderator-contact boilerplate",
      remove_pinned_candidates & flag_pinned_candidate
                                ~ "Pinned or recurring informational thread",
      TRUE                       ~ NA_character_
    ),
    remove_reddit_artifact = !is.na(artifact_reason)
  )

## 4. Review counts before removal
artifact_counts <- reddit_meta |>
  count(artifact_reason, sort = TRUE, name = "documents") |>
  mutate(artifact_reason = replace_na(artifact_reason, "Retained"))

print(artifact_counts)
message("Documents flagged for removal: ",
        sum(reddit_meta$remove_reddit_artifact))
message("Pinned candidates requiring review: ",
        sum(reddit_meta$flag_pinned_candidate))

pinned_review <- reddit_meta |>
  filter(flag_pinned_candidate) |>
  transmute(
    doc_id,
    source = if ("source" %in% names(reddit_meta)) source else NA_character_,
    year = if ("year" %in% names(reddit_meta)) year else NA_integer_,
    author = if ("author" %in% names(reddit_meta)) author else NA_character_,
    title = if ("title" %in% names(reddit_meta)) title else NA_character_,
    excerpt = str_sub(full_text_for_screening, 1, 800)
  )

write.csv(
  pinned_review,
  file.path(stm_output_dir,"pinned_informational_candidates_for_review.csv"),
  row.names = FALSE
)

## 5. Audit log of removed documents
removed_reddit_artifacts <- reddit_meta |>
  filter(remove_reddit_artifact) |>
  transmute(
    doc_id,
    source = if ("source" %in% names(reddit_meta)) source else NA_character_,
    year = if ("year" %in% names(reddit_meta)) year else NA_integer_,
    subreddit = if ("subreddit" %in% names(reddit_meta)) subreddit else NA_character_,
    author = if ("author" %in% names(reddit_meta)) author else NA_character_,
    title = if ("title" %in% names(reddit_meta)) title else NA_character_,
    esfi_hits = esfi_hits,
    esfi_present = esfi_present,
    artifact_reason,
    excerpt = str_sub(full_text_for_screening, 1, 1200)
  ) |>
  arrange(artifact_reason, doc_id)

write.csv(
  removed_reddit_artifacts,
  file.path(stm_output_dir,"removed_reddit_artifacts.csv"),
  row.names = FALSE
)

saveRDS(
  removed_reddit_artifacts,
  file.path(stm_output_dir,"removed_reddit_artifacts.rds")
)

## 6. Create STM-specific filtered objects
keep_docs <- !reddit_meta$remove_reddit_artifact
reddit_corpus_stm <- reddit_corpus_analysis[keep_docs]
reddit_tokens_stm <- reddit_tokens_analysis[keep_docs]
reddit_dfm_stm <- reddit_dfm_analysis[keep_docs,]
stopifnot(identical(docnames(reddit_corpus_stm),
                    docnames(reddit_tokens_stm)))
stopifnot(identical(docnames(reddit_tokens_stm),
                    docnames(reddit_dfm_stm)))

## 7. Retrim after removing repeated artifacts
reddit_dfm_stm <- reddit_dfm_stm |>
  dfm_trim(min_docfreq = 10) |>
  dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")

## Remove documents that become empty after retrimming
stm_nonempty <- ntoken(reddit_dfm_stm) > 0

empty_after_artifact_removal <- tibble(
  doc_id = docnames(reddit_dfm_stm)[!stm_nonempty],
  source = docvars(reddit_dfm_stm, "source")[!stm_nonempty],
  year = docvars(reddit_dfm_stm, "year")[!stm_nonempty],
  esfi_hits = docvars(reddit_dfm_stm, "esfi_hits")[!stm_nonempty],
  esfi_present = docvars(reddit_dfm_stm, "esfi_present")[!stm_nonempty],
  removal_reason = "Empty after STM vocabulary trimming"
)

reddit_dfm_stm <- reddit_dfm_stm[stm_nonempty, ]

final_stm_ids <- docnames(reddit_dfm_stm)
reddit_corpus_stm <- reddit_corpus_stm[final_stm_ids]
reddit_tokens_stm <- reddit_tokens_stm[final_stm_ids]

##-------------------------------------------------------------
## STM corpus summaries
##-------------------------------------------------------------

document_summary <- tibble::tibble(
  stage = c(
    "Frozen analysis corpus",
    "Removed Reddit artifacts",
    "Removed after vocabulary trimming",
    "Final STM corpus"
  ),
  documents = c(
    quanteda::ndoc(reddit_dfm_analysis),
    sum(reddit_meta$remove_reddit_artifact),
    nrow(empty_after_artifact_removal),
    quanteda::ndoc(reddit_dfm_stm)
  )
)

vocabulary_summary <- tibble::tibble(
  measure = c(
    "Frozen analysis features",
    "Final STM features",
    "Features removed during STM trimming"
  ),
  features = c(
    quanteda::nfeat(reddit_dfm_analysis),
    quanteda::nfeat(reddit_dfm_stm),
    quanteda::nfeat(reddit_dfm_analysis) -
      quanteda::nfeat(reddit_dfm_stm)
  )
)

cat("\n")
cat("Document Summary\n")
cat("------------------------------------------\n")
print(document_summary)

cat("\n")
cat("Vocabulary Summary\n")
cat("------------------------------------------\n")
print(vocabulary_summary)

write.csv(
  document_summary,
  file.path(
    stm_output_dir,
    "stm_document_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  vocabulary_summary,
  file.path(
    stm_output_dir,
    "stm_vocabulary_summary.csv"
  ),
  row.names = FALSE
)
## 8. Complete removal audit
removed_documents_audit <- dplyr::bind_rows(
  removed_reddit_artifacts |>
    dplyr::transmute(
      doc_id, source, year, esfi_hits, esfi_present,
      removal_reason = artifact_reason
    ),
  empty_after_artifact_removal |>
    dplyr::select(
      doc_id, source, year, esfi_hits, esfi_present, removal_reason
    )
) |>
  dplyr::arrange(removal_reason, doc_id)

write.csv(
  removed_documents_audit,
  file.path(stm_output_dir, "stm_removed_documents_audit.csv"),
  row.names = FALSE
)

saveRDS(
  removed_documents_audit,
  file.path(stm_output_dir, "stm_removed_documents_audit.rds")
)

## 9. Final checks
stopifnot(identical(docnames(reddit_corpus_stm),
                    docnames(reddit_tokens_stm)))
stopifnot(identical(docnames(reddit_tokens_stm),
                    docnames(reddit_dfm_stm)))
stopifnot(ndoc(reddit_dfm_stm) ==
            nrow(docvars(reddit_dfm_stm)))

message("Final STM documents: ", ndoc(reddit_dfm_stm))
message("Final STM features: ", nfeat(reddit_dfm_stm))

print(table(docvars(reddit_dfm_stm, "source"), useNA = "ifany"))
print(table(docvars(reddit_dfm_stm, "year"), useNA = "ifany"))

## Verify
document_summary
vocabulary_summary
artifact_counts
empty_after_artifact_removal
sum(reddit_meta$remove_reddit_artifact) +
  nrow(empty_after_artifact_removal)
table(
  removed_reddit_artifacts$source,
  removed_reddit_artifacts$artifact_reason,
  useNA = "ifany"
)
stopifnot(
  sum(table(docvars(reddit_dfm_stm, "source"))) ==
    ndoc(reddit_dfm_stm)
)

stopifnot(
  sum(table(docvars(reddit_dfm_stm, "year"))) ==
    ndoc(reddit_dfm_stm)
)

stopifnot(
  all(ntoken(reddit_dfm_stm) > 0)
)

stopifnot(
  ndoc(reddit_dfm_stm) == 3008,
  nrow(removed_documents_audit) == 58,
  ndoc(reddit_dfm_analysis) - nrow(removed_documents_audit) ==
    ndoc(reddit_dfm_stm),
  all(c("esfi_hits", "esfi_present") %in%
        names(quanteda::docvars(reddit_dfm_stm)))
)
## 10. Save cleaned STM objects
write.csv(
  artifact_counts,
  file.path(stm_output_dir, "reddit_artifact_counts.csv"),
  row.names = FALSE)

reddit_metadata_stm <- quanteda::docvars(
  reddit_dfm_stm
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    doc_id = quanteda::docnames(
      reddit_dfm_stm
    ),
    .before = 1
  )

saveRDS(reddit_meta, file.path(stm_output_dir, "reddit_artifact_screening_metadata.rds"))
saveRDS(reddit_metadata_stm, file.path(deriveddata_dir, "reddit_metadata_stm.rds"))
saveRDS(reddit_corpus_stm, file.path(deriveddata_dir, "reddit_corpus_stm.rds"))
saveRDS(reddit_tokens_stm, file.path(deriveddata_dir, "reddit_tokens_stm.rds"))
saveRDS(reddit_dfm_stm, file.path(deriveddata_dir, "reddit_dfm_stm.rds"))
saveRDS(empty_after_artifact_removal,
        file.path(stm_output_dir,"empty_documents_after_artifact_removal.rds"))

reddit_stm_input <- quanteda::convert(
  reddit_dfm_stm,
  to = "stm"
)

reddit_stm_input$meta <- reddit_metadata_stm
stopifnot(
  length(reddit_stm_input$documents) ==
    quanteda::ndoc(reddit_dfm_stm)
)

stopifnot(
  all(
    c("esfi_hits", "esfi_present") %in%
      names(reddit_stm_input$meta)
  )
)

stopifnot(
  nrow(reddit_stm_input$meta) ==
    quanteda::ndoc(reddit_dfm_stm)
)

if ("doc_id" %in% names(reddit_stm_input$meta)) {
  stopifnot(
    identical(
      as.character(reddit_stm_input$meta$doc_id),
      as.character(quanteda::docnames(reddit_dfm_stm))
    )
  )
}

saveRDS(
  reddit_stm_input,
  file.path(deriveddata_dir, "reddit_stm_input.rds")
)
saveRDS(
  reddit_stm_input$vocab,
  file.path(deriveddata_dir, "reddit_stm_vocab.rds")
)

message("STM-specific cleaned objects saved successfully.")
message("The frozen ESFI corpus objects were not modified.")

message("")

message("06_prepare_stm_corpus.R completed successfully.")

message("------------------------------------------")

message("Final STM documents: ", ndoc(reddit_dfm_stm))

message("Final STM features : ", nfeat(reddit_dfm_stm))

message("------------------------------------------")
