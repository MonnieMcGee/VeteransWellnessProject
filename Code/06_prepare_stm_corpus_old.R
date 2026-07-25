###############################################################
## 04_prepare_stm_corpus.R
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

source("00_project_setup.R")

## 0. Load frozen ESFI corpus objects
corp_vet_analysis <- readRDS("./Data/corp_vet_analysis_final.rds")
toks_vet_analysis <- readRDS("./Data/toks_vet_analysis_final.rds")
dfm_vet_analysis  <- readRDS("./Data/dfm_vet_analysis_final.rds")

stopifnot(inherits(corp_vet_analysis, "corpus"))
stopifnot(inherits(toks_vet_analysis, "tokens"))
stopifnot(inherits(dfm_vet_analysis, "dfm"))
stopifnot(identical(docnames(corp_vet_analysis), docnames(toks_vet_analysis)))
stopifnot(identical(docnames(toks_vet_analysis), docnames(dfm_vet_analysis)))

message("Frozen ESFI documents: ", ndoc(dfm_vet_analysis))
message("Frozen ESFI features: ", nfeat(dfm_vet_analysis))

# Create output directory
output_dir <- file.path("Output", "stm_corpus")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

## 1. Extract text and metadata
doc_ids <- docnames(corp_vet_analysis)
reddit_text <- as.character(corp_vet_analysis)
names(reddit_text) <- doc_ids

reddit_meta <- quanteda::docvars(corp_vet_analysis) |>
  as.data.frame()

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
  file.path(output_dir,"pinned_informational_candidates_for_review.csv"),
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
    artifact_reason,
    excerpt = str_sub(full_text_for_screening, 1, 1200)
  ) |>
  arrange(artifact_reason, doc_id)

write.csv(
  removed_reddit_artifacts,
  file.path(output_dir,"removed_reddit_artifacts.csv"),
  row.names = FALSE
)

saveRDS(
  removed_reddit_artifacts,
  file.path(output_dir,"removed_reddit_artifacts.rds")
)

## 6. Create STM-specific filtered objects
keep_docs <- !reddit_meta$remove_reddit_artifact

corp_vet_stm_final <- corp_vet_analysis[keep_docs]
toks_vet_stm_final <- toks_vet_analysis[keep_docs]
dfm_vet_stm_final  <- dfm_vet_analysis[keep_docs, ]

stopifnot(identical(docnames(corp_vet_stm_final),
                    docnames(toks_vet_stm_final)))
stopifnot(identical(docnames(toks_vet_stm_final),
                    docnames(dfm_vet_stm_final)))

## 7. Retrim after removing repeated artifacts
dfm_vet_stm_final <- dfm_vet_stm_final |>
  dfm_trim(min_docfreq = 10) |>
  dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")

## Remove documents that become empty after retrimming
stm_nonempty <- ntoken(dfm_vet_stm_final) > 0

empty_after_artifact_removal <- tibble(
  doc_id = docnames(dfm_vet_stm_final)[!stm_nonempty],
  source = docvars(dfm_vet_stm_final, "source")[!stm_nonempty],
  year = docvars(dfm_vet_stm_final, "year")[!stm_nonempty]
)

dfm_vet_stm_final <- dfm_vet_stm_final[stm_nonempty, ]

final_stm_ids <- docnames(dfm_vet_stm_final)
corp_vet_stm_final <- corp_vet_stm_final[final_stm_ids]
toks_vet_stm_final <- toks_vet_stm_final[final_stm_ids]

stm_summary <- tibble(
  
  frozen_documents = ndoc(dfm_vet_analysis),
  
  removed_artifacts = sum(reddit_meta$remove_reddit_artifact),
  
  removed_empty_documents = nrow(empty_after_artifact_removal),
  
  final_documents = ndoc(dfm_vet_stm_final),
  
  final_features = nfeat(dfm_vet_stm_final)
  
)

print(stm_summary)

write.csv(
  stm_summary,
  file.path(output_dir,"stm_corpus_summary.csv"),
  row.names = FALSE
)

## 8. Final checks
stopifnot(identical(docnames(corp_vet_stm_final),
                    docnames(toks_vet_stm_final)))
stopifnot(identical(docnames(toks_vet_stm_final),
                    docnames(dfm_vet_stm_final)))
stopifnot(ndoc(dfm_vet_stm_final) ==
            nrow(docvars(dfm_vet_stm_final)))

message("Final STM documents: ", ndoc(dfm_vet_stm_final))
message("Final STM features: ", nfeat(dfm_vet_stm_final))

print(table(docvars(dfm_vet_stm_final, "source"), useNA = "ifany"))
print(table(docvars(dfm_vet_stm_final, "year"), useNA = "ifany"))

## Verify
stm_summary
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
  sum(table(docvars(dfm_vet_stm_final, "source"))) ==
    ndoc(dfm_vet_stm_final)
)

stopifnot(
  sum(table(docvars(dfm_vet_stm_final, "year"))) ==
    ndoc(dfm_vet_stm_final)
)

stopifnot(
  all(ntoken(dfm_vet_stm_final) > 0)
)
## 10. Save cleaned STM objects
write.csv(
  artifact_counts,
  file.path(output_dir, "reddit_artifact_counts.csv"),
  row.names = FALSE)

reddit_meta_stm_final <- quanteda::docvars(
  corp_vet_stm_final
) |>
  as.data.frame()
saveRDS(reddit_meta, file.path(output_dir, "reddit_artifact_screening_metadata.rds"))
saveRDS(reddit_meta_stm_final,file.path(output_dir,"reddit_meta_stm_final.rds"))
saveRDS(corp_vet_stm_final, file.path(output_dir,"corp_vet_stm_final.rds"))
saveRDS(toks_vet_stm_final, file.path(output_dir,"toks_vet_stm_final.rds"))
saveRDS(dfm_vet_stm_final, file.path(output_dir,"dfm_vet_stm_final.rds"))
saveRDS(empty_after_artifact_removal,
        file.path(output_dir,"empty_documents_after_artifact_removal.rds"))

out <- quanteda::convert(
  dfm_vet_stm_final,
  to = "stm"
)
stopifnot(
  length(out$documents) ==
    quanteda::ndoc(dfm_vet_stm_final)
)

stopifnot(
  nrow(out$meta) ==
    quanteda::ndoc(dfm_vet_stm_final)
)

if ("doc_id" %in% names(out$meta)) {
  stopifnot(
    identical(
      as.character(out$meta$doc_id),
      as.character(quanteda::docnames(dfm_vet_stm_final))
    )
  )
}

saveRDS(
  out,
  file.path(output_dir,"reddit_stm_input_final.rds")
)
saveRDS(
  out$vocab,
  file.path(output_dir,"reddit_stm_vocab_final.rds")
)

message("STM-specific cleaned objects saved successfully.")
message("The frozen ESFI corpus objects were not modified.")

message("")

message("Corpus successfully frozen for STM analysis.")

message("------------------------------------------")

message("Documents: ", ndoc(dfm_vet_stm_final))

message("Features : ", nfeat(dfm_vet_stm_final))

message("------------------------------------------")
