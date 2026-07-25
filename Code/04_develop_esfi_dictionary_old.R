###############################################################
## ESFI Dictionary Development
##
## Purpose
## -------
## This script documents the development and refinement of the
## Experienced System Fragmentation Index (ESFI) dictionary.
##
## The workflow is intentionally separated from the production
## validation script. It preserves an audit trail showing:
##   1. the broad candidate dictionary considered initially;
##   2. keyword-in-context (KWIC) review of ambiguous terms;
##   3. empirical matching of candidate terms in the frozen corpus;
##   4. the contribution of terms and conceptual domains;
##   5. the decisions that led to the final ESFI dictionary.
##
## Required frozen objects
## -----------------------
## 1. toks_vet_clean
##    Tokens after stop-word removal but before unigram/bigram
##    expansion and document-frequency trimming. These tokens are
##    used for KWIC review because they preserve the local context
##    needed to evaluate candidate terms.
##
## 2. dfm_vet_analysis
##    Final frozen document-feature matrix containing unigrams and
##    bigrams after removal of the ten templated Dallas event-calendar
##    posts and final document-frequency trimming.
##
## Expected file names
## -------------------
##   data/toks_vet_clean.rds
##   data/dfm_vet_analysis.rds
##
## Alternative paths can be supplied below.
###############################################################

##-------------------------------------------------------------
## 0. Packages and file locations
##-------------------------------------------------------------

source("00_project_setup.R")

## Change these paths if the frozen objects are stored elsewhere.
tokens_file <- file.path("Data", "toks_vet_analysis_final.rds")
dfm_file    <- file.path("Data", "dfm_vet_analysis_final.rds")
output_dir  <- file.path("Output", "esfi_dictionary_development")

## Set to TRUE to print all KWIC diagnostics to the console.
## KWIC output can be lengthy, so FALSE is the safer default when
## rerunning the complete script.
RUN_KWIC <- FALSE

## Helper: search a few plausible locations for an input file.
## This allows the script to run from either the project root or a
## scripts subdirectory without relying on objects in the workspace.
locate_input <- function(path) {
  candidates <- unique(c(
    path,
    file.path("..", path),
    basename(path),
    file.path("..", basename(path))
  ))

  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0L) {
    stop(
      "Could not find required input file: ", path, "\n",
      "Checked: ", paste(candidates, collapse = ", ")
    )
  }

  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

##-------------------------------------------------------------
## 1. Import and verify the frozen analysis objects
##-------------------------------------------------------------

tokens_path <- locate_input(tokens_file)
dfm_path    <- locate_input(dfm_file)

message("Reading tokens from: ", tokens_path)
message("Reading DFM from:    ", dfm_path)

toks_vet_clean   <- readRDS(tokens_path)
dfm_vet_analysis <- readRDS(dfm_path)



if (!quanteda::is.tokens(toks_vet_clean)) {
  stop("toks_vet_clean.rds does not contain a quanteda tokens object.")
}

if (!quanteda::is.dfm(dfm_vet_analysis)) {
  stop("dfm_vet_analysis.rds does not contain a quanteda dfm object.")
}

## Confirm that source metadata required for the known-groups summaries
## is present in the final DFM.
if (!"source" %in% names(quanteda::docvars(dfm_vet_analysis))) {
  stop("dfm_vet_analysis must contain a document variable named 'source'.")
}

message("KWIC token documents: ", quanteda::ndoc(toks_vet_clean))
message("Final DFM documents:  ", quanteda::ndoc(dfm_vet_analysis))
message("Final DFM features:   ", quanteda::nfeat(dfm_vet_analysis))

## The token object can contain more documents than the final DFM because
## the DFM excludes empty documents and the ten Dallas event calendars.
## Therefore, equality of document counts is not required here.

## Create the output directory only after the input checks succeed.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

##-------------------------------------------------------------
## 2. Broad candidate dictionary considered during development
##-------------------------------------------------------------

## This broad list was intended to cover multiple manifestations of
## experienced system fragmentation. It includes explicit barriers,
## repeated administrative action, waiting, coordination failures,
## structural fragmentation, and communication failures.
##
## Several entries were later removed because they were ambiguous,
## too rare to contribute, absent from the frozen corpus, or reflected
## researcher terminology rather than the language used by veterans.
esfi_dict_broad <- c(
  ## Access barriers and adverse decisions
  "denied", "denial*", "reject*", "ineligible",

  ## Repeated administrative action
  "paperwork", "form", "claim*", "appeal*",
  "refile*", "reopen*", "resubmit*", "reapply*",

  ## Delay and unresolved status
  "wait*", "backlog*", "pending", "delay*",
  "still_waiting", "long_wait", "taking_forever",

  ## Coordination and routing failures
  "transfer*", "redirect*", "handoff*", "bounced",
  "bounced_around", "sent_elsewhere", "referred_back",
  "back_and_forth", "start_over",

  ## Structural fragmentation
  "runaround", "silo*", "red_tape",
  "multiple_offices", "different_departments",

  ## Communication and information failures
  "misinform*", "unresponsive", "ghosted",
  "no_response", "never_called", "callback",
  "conflicting_information", "different_answers",
  "nobody_knows"
)

##-------------------------------------------------------------
## 3. KWIC review of ambiguous and candidate terms
##-------------------------------------------------------------

## KWIC review was used to distinguish language that actually described
## navigation through fragmented systems from words that were generic,
## semantically diffuse, or matched unrelated stems.
##
## Key decisions from the review:
##   form*       -> remove: matched format, former, formed, formula, etc.
##   claim*      -> retain: overwhelmingly referred to VA claims processes.
##   transfer*   -> retain: included genuine routing and coordination failures,
##                  despite some college and financial-transfer noise.
##   pending     -> remove: only two matches, neither relevant to ESFI.
##   reject*     -> initially retained conceptually, later removed because it
##                  produced no matches in the final DFM after trimming.
##   resubmit*, reapply*, refile* -> remove: conceptually relevant but only
##                  one or two occurrences each.

kwic_patterns <- c(
  "form*",
  "claim*",
  "transfer*",
  "pending",
  "reject*",
  "resubmit*",
  "reapply*",
  "refile*"
)

## Helper that writes each KWIC result to a CSV file. Saving the output
## creates a reproducible record without requiring very long console output.
write_kwic_result <- function(tokens, pattern, directory) {
  result <- quanteda::kwic(
    tokens,
    pattern = pattern,
    valuetype = "glob",
    window = 5
  )

  safe_name <- gsub("[^A-Za-z0-9]+", "_", pattern)
  safe_name <- gsub("_+$", "", safe_name)

  out_file <- file.path(directory, paste0("kwic_", safe_name, ".csv"))
  utils::write.csv(as.data.frame(result), out_file, row.names = FALSE)

  result
}

kwic_results <- lapply(
  kwic_patterns,
  function(pat) write_kwic_result(toks_vet_clean, pat, output_dir)
)
names(kwic_results) <- kwic_patterns

if (isTRUE(RUN_KWIC)) {
  for (pat in names(kwic_results)) {
    cat("\n", paste(rep("=", 72), collapse = ""), "\n", sep = "")
    cat("KWIC pattern: ", pat, "\n", sep = "")
    print(kwic_results[[pat]])
  }
}

## Summarize the number of KWIC matches for the development record.
kwic_match_summary <- tibble(
  pattern = names(kwic_results),
  matches = vapply(kwic_results, nrow, integer(1))
)

write.csv(
  kwic_match_summary,
  file.path(output_dir, "kwic_match_summary.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 4. Intermediate candidate dictionary used for contribution analysis
##-------------------------------------------------------------

## After the KWIC review, broad or ambiguous terms such as form and pending
## were removed. The following intermediate dictionary retained theoretically
## central terms so that their empirical contribution could be assessed.
esfi_dictionary_candidate <- quanteda::dictionary(list(
  access_barriers = c(
    "denied",
    "denial*",
    "reject*"
  ),

  administrative_processes = c(
    "paperwork",
    "claim*",
    "appeal*"
  ),

  waiting_delay = c(
    "wait*",
    "backlog*",
    "delay*"
  ),

  coordination_failures = c(
    "transfer*",
    "redirect*",
    "handoff*"
  )
))

candidate_patterns <- unlist(esfi_dictionary_candidate, use.names = FALSE)

## Identify the exact frozen-corpus features captured by the candidate
## dictionary. This reveals which stems are operationally active.
actual_candidate_matches <- quanteda::featnames(
  quanteda::dfm_select(
    dfm_vet_analysis,
    pattern = candidate_patterns,
    valuetype = "glob",
    selection = "keep"
  )
)

message(
  "Candidate dictionary matched ", length(actual_candidate_matches),
  " distinct DFM features."
)
print(actual_candidate_matches)

write.csv(
  tibble(matched_feature = actual_candidate_matches),
  file.path(output_dir, "candidate_actual_matches.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 5. Contribution of actual matched features
##-------------------------------------------------------------

## Select all features matching the intermediate candidate dictionary.
dfm_esfi_terms <- quanteda::dfm_select(
  dfm_vet_analysis,
  pattern = candidate_patterns,
  valuetype = "glob",
  selection = "keep"
)

term_hits <- quanteda::colSums(dfm_esfi_terms)
total_term_hits <- sum(term_hits)

if (total_term_hits == 0L) {
  stop("The candidate ESFI dictionary produced no hits in the final DFM.")
}

term_contribution <- tibble(
  matched_feature = names(term_hits),
  hits = as.numeric(term_hits)
) |>
  mutate(
    percent_of_all_hits = 100 * hits / total_term_hits
  ) |>
  arrange(desc(hits)) |>
  mutate(
    cumulative_percent = cumsum(percent_of_all_hits)
  )

print(term_contribution)

write.csv(
  term_contribution,
  file.path(output_dir, "term_contribution.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 6. Document prevalence of ESFI indicators
##-------------------------------------------------------------

## Convert the ESFI feature matrix to a binary presence/absence matrix.
## A document is considered ESFI-positive if it contains at least one
## ESFI feature.

dfm_esfi_binary <- quanteda::dfm_weight(
  dfm_esfi_terms,
  scheme = "boolean"
)

esfi_present <- quanteda::rowSums(dfm_esfi_binary) > 0

##-------------------------------------------------------------
## Save document-level ESFI scores
##-------------------------------------------------------------

esfi_document_scores <- quanteda::docvars(dfm_vet_analysis) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    original_document_number = seq_len(n()),
    document_name = quanteda::docnames(dfm_vet_analysis),
    esfi_hits = as.numeric(esfi_hits),
    esfi_present = esfi_present
  )

saveRDS(
  esfi_document_scores,
  file.path(
    output_dir,
    "esfi_document_scores.rds"
  )
)

readr::write_csv(
  esfi_document_scores,
  file.path(
    output_dir,
    "esfi_document_scores.csv"
  )
)


document_prevalence <- tibble(
  documents = quanteda::ndoc(dfm_vet_analysis),
  documents_with_esfi = sum(esfi_present),
  percent_with_esfi =
    100 * mean(esfi_present)
)

print(document_prevalence)

write.csv(
  document_prevalence,
  file.path(output_dir, "document_prevalence.csv"),
  row.names = FALSE
)

## For a little richer data - prevalence by source.
document_prevalence_by_source <- tibble(
  source = quanteda::docvars(dfm_vet_analysis, "source"),
  esfi_present = esfi_present
) |>
  group_by(source) |>
  summarise(
    documents = n(),
    documents_with_esfi = sum(esfi_present),
    percent_with_esfi = 100 * mean(esfi_present),
    .groups = "drop"
  )

print(document_prevalence_by_source)

write.csv(
  document_prevalence_by_source,
  file.path(output_dir, "document_prevalence_by_source.csv"),
  row.names = FALSE
)

## Number of ESFI matches in each document
esfi_hits <- quanteda::rowSums(dfm_esfi_terms)

esfi_summary_by_source <- tibble(
  source = quanteda::docvars(dfm_vet_analysis, "source"),
  esfi_hits = esfi_hits,
  esfi_present = esfi_hits > 0
) |>
  group_by(source) |>
  summarise(
    documents = n(),
    documents_with_esfi = sum(esfi_present),
    percent_with_esfi = 100 * mean(esfi_present),
    total_hits = sum(esfi_hits),
    mean_hits_per_document = mean(esfi_hits),
    mean_hits_among_positive = mean(esfi_hits[esfi_present]),
    median_hits_among_positive = median(esfi_hits[esfi_present]),
    maximum_hits = max(esfi_hits),
    .groups = "drop"
  )

print(esfi_summary_by_source)

##-------------------------------------------------------------
## 6. ESFI summary by source corpus
##-------------------------------------------------------------

## Number of ESFI feature matches in each document
esfi_hits <- quanteda::rowSums(dfm_esfi_terms)

## Indicator that a document contains at least one ESFI feature
esfi_present <- esfi_hits > 0

esfi_summary_by_source <- tibble(
  source = quanteda::docvars(dfm_vet_analysis, "source"),
  esfi_hits = esfi_hits,
  esfi_present = esfi_present
) |>
  group_by(source) |>
  summarise(
    documents = n(),
    documents_with_esfi = sum(esfi_present),
    percent_with_esfi = 100 * mean(esfi_present),
    total_esfi_hits = sum(esfi_hits),
    mean_hits_per_document = mean(esfi_hits),
    mean_hits_positive_documents =
      ifelse(
        sum(esfi_present) > 0,
        mean(esfi_hits[esfi_present]),
        NA_real_
      ),
    median_hits_positive_documents =
      ifelse(
        sum(esfi_present) > 0,
        median(esfi_hits[esfi_present]),
        NA_real_
      ),
    maximum_hits = max(esfi_hits),
    percent_of_all_hits =
      100 * total_esfi_hits / sum(total_esfi_hits),
    .groups = "drop"
  ) |>
  arrange(desc(total_esfi_hits))

## Round for presentation
esfi_summary_by_source <- esfi_summary_by_source |>
  mutate(
    percent_with_esfi = round(percent_with_esfi, 2),
    mean_hits_per_document = round(mean_hits_per_document, 3),
    mean_hits_positive_documents =
      round(mean_hits_positive_documents, 2),
    percent_of_all_hits = round(percent_of_all_hits, 1)
  )

print(esfi_summary_by_source)

write.csv(
  esfi_summary_by_source,
  file.path(output_dir, "esfi_summary_by_source.csv"),
  row.names = FALSE
)
##-------------------------------------------------------------
## 7. Contribution of dictionary stems and conceptual domains
##-------------------------------------------------------------

## Rather than manually mapping every observed inflection, create a separate
## dictionary key for each candidate stem. dfm_lookup then combines all
## matching features (e.g., claim and claims) under the corresponding stem.
stem_dictionary_list <- list(
  denied     = "denied",
  `denial*`  = "denial*",
  `reject*`  = "reject*",
  paperwork  = "paperwork",
  `claim*`   = "claim*",
  `appeal*`  = "appeal*",
  `wait*`    = "wait*",
  `backlog*` = "backlog*",
  `delay*`   = "delay*",
  `transfer*` = "transfer*",
  `redirect*` = "redirect*",
  `handoff*`  = "handoff*"
)

esfi_stem_dictionary <- quanteda::dictionary(stem_dictionary_list)

dfm_esfi_stems <- quanteda::dfm_lookup(
  dfm_vet_analysis,
  dictionary = esfi_stem_dictionary,
  valuetype = "glob",
  exclusive = TRUE
)

stem_hits <- quanteda::colSums(dfm_esfi_stems)
total_stem_hits <- sum(stem_hits)

## Inventory maps each candidate stem to one of the four conceptual domains.
dictionary_inventory <- tribble(
  ~dictionary_stem, ~domain,
  "denied",     "access_barriers",
  "denial*",    "access_barriers",
  "reject*",    "access_barriers",
  "paperwork",  "administrative_processes",
  "claim*",     "administrative_processes",
  "appeal*",    "administrative_processes",
  "wait*",      "waiting_delay",
  "backlog*",   "waiting_delay",
  "delay*",     "waiting_delay",
  "transfer*",  "coordination_failures",
  "redirect*",  "coordination_failures",
  "handoff*",   "coordination_failures"
)

stem_contribution_complete <- dictionary_inventory |>
  left_join(
    tibble(
      dictionary_stem = names(stem_hits),
      hits = as.numeric(stem_hits)
    ),
    by = "dictionary_stem"
  ) |>
  mutate(
    hits = tidyr::replace_na(hits, 0)
  )

## Use ordinary if/else rather than dplyr::if_else because the condition is
## scalar while the percentage result is a vector.
if (total_stem_hits > 0L) {
  stem_contribution_complete <- stem_contribution_complete |>
    mutate(percent_of_all_hits = 100 * hits / total_stem_hits)
} else {
  stem_contribution_complete <- stem_contribution_complete |>
    mutate(percent_of_all_hits = 0)
}

stem_contribution_complete <- stem_contribution_complete |>
  arrange(desc(hits)) |>
  mutate(cumulative_percent = cumsum(percent_of_all_hits))

print(stem_contribution_complete)

write.csv(
  stem_contribution_complete,
  file.path(output_dir, "stem_contribution_complete.csv"),
  row.names = FALSE
)

## Aggregate stem contributions into the four conceptual domains.
domain_contribution <- stem_contribution_complete |>
  group_by(domain) |>
  summarise(
    hits = sum(hits),
    .groups = "drop"
  )

total_domain_hits <- sum(domain_contribution$hits)

if (total_domain_hits > 0L) {
  domain_contribution <- domain_contribution |>
    mutate(percent_of_all_hits = 100 * hits / total_domain_hits)
} else {
  domain_contribution <- domain_contribution |>
    mutate(percent_of_all_hits = 0)
}

domain_contribution <- domain_contribution |>
  arrange(desc(hits)) |>
  mutate(cumulative_percent = cumsum(percent_of_all_hits))

print(domain_contribution)

write.csv(
  domain_contribution,
  file.path(output_dir, "domain_contribution.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 8. Candidate-term counts by source corpus
##-------------------------------------------------------------

## Group the selected candidate features by source. This diagnostic was used
## to verify that claims, denials, transfers, and paperwork were concentrated
## in Veteran discourse, while Dallas and Finance provided comparison groups.
term_by_source_dfm <- quanteda::dfm_group(
  dfm_esfi_terms,
  groups = quanteda::docvars(dfm_esfi_terms, "source")
)

term_by_source <- quanteda::convert(
  term_by_source_dfm,
  to = "data.frame"
)

print(term_by_source)

write.csv(
  term_by_source,
  file.path(output_dir, "term_by_source.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 9. Decision log for candidate dictionary terms
##-------------------------------------------------------------

## This table records why terms were retained or removed. It is useful for
## maintaining an audit trail and for describing construct-informed dictionary
## development in the Methods or supplementary materials.
decision_log <- tribble(
  ~candidate, ~decision, ~rationale,
  "denied", "retain", "Direct expression of an adverse access decision; frequent and specific.",
  "denial*", "retain", "Captures denial and denial_letter; direct evidence of an access barrier.",
  "reject*", "remove", "Conceptually relevant but produced no matches in the final frozen DFM.",
  "paperwork", "retain", "Specific administrative-process language with observed matches.",
  "form", "remove", "Ambiguous stem matched format, former, formed, formula, and other unrelated forms.",
  "claim*", "retain", "KWIC review showed use overwhelmingly concerned VA claims navigation.",
  "appeal*", "retain", "Signals an adverse decision or repeated administrative action.",
  "wait*", "retain", "Captures wait and waiting; direct evidence of delay and unresolved processes.",
  "backlog*", "remove", "Theoretically relevant but produced no matches in the final frozen DFM.",
  "pending", "remove", "Only two KWIC matches and neither represented system fragmentation.",
  "delay*", "remove", "Theoretically relevant but produced no matches in the final frozen DFM.",
  "transfer*", "retain", "KWIC showed genuine routing failures and repeated transfers despite some unrelated uses.",
  "redirect*", "remove", "Produced no matches in the final frozen DFM.",
  "handoff*", "remove", "Produced no matches and reflected professional rather than participant language.",
  "runaround", "remove", "Highly specific but absent from the final frozen DFM.",
  "silo*", "remove", "Absent and more characteristic of researcher or policy terminology.",
  "red_tape", "remove", "Highly specific but absent from the final frozen DFM.",
  "misinform*", "remove", "Absent from the final frozen DFM.",
  "unresponsive", "remove", "Absent from the final frozen DFM.",
  "ghosted", "remove", "Absent from the final frozen DFM.",
  "resubmit*", "remove", "Only two KWIC occurrences; too rare to contribute meaningfully.",
  "reapply*", "remove", "Only one KWIC occurrence; too rare to contribute meaningfully.",
  "refile*", "remove", "Only two KWIC occurrences; too rare to contribute meaningfully."
)

write.csv(
  decision_log,
  file.path(output_dir, "dictionary_decision_log.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 10. Freeze the final ESFI dictionary
##-------------------------------------------------------------

## Final operational dictionary used in subsequent ESFI scoring and
## validation analyses. Every retained stem is both conceptually defensible
## and empirically active in the frozen corpus.
esfi_dict_final <- c(
  ## Access barriers
  "denied",
  "denial*",

  ## Administrative processes
  "paperwork",
  "claim*",
  "appeal*",

  ## Waiting and delay
  "wait*",

  ## Coordination failures
  "transfer*"
)

## Confirm that the final dictionary produces the expected observed features.
actual_final_matches <- quanteda::featnames(
  quanteda::dfm_select(
    dfm_vet_analysis,
    pattern = esfi_dict_final,
    valuetype = "glob",
    selection = "keep"
  )
)

message(
  "Final ESFI dictionary matched ", length(actual_final_matches),
  " distinct DFM features:"
)
print(actual_final_matches)

## Save the final dictionary in both RDS and plain-text forms so that the
## production validation script can import exactly the frozen instrument.
saveRDS(
  esfi_dict_final,
  file.path(output_dir, "esfi_dict_final.rds")
)

writeLines(
  esfi_dict_final,
  con = file.path(output_dir, "esfi_dict_final.txt")
)

write.csv(
  tibble(matched_feature = actual_final_matches),
  file.path(output_dir, "final_actual_matches.csv"),
  row.names = FALSE
)

## Save scored DFM.
quanteda::docvars(dfm_vet_analysis, "esfi_hits") <- as.numeric(esfi_hits)
quanteda::docvars(dfm_vet_analysis, "esfi_present") <- esfi_present

saveRDS(
  dfm_vet_analysis,
  file.path(
    output_dir,
    "dfm_vet_analysis_with_esfi.rds"
  )
)
##-------------------------------------------------------------
## 11. Internal consistency checks
##-------------------------------------------------------------

## Counts should agree across feature-, stem-, and domain-level summaries.
stopifnot(sum(term_contribution$hits) == sum(stem_contribution_complete$hits))
stopifnot(sum(stem_contribution_complete$hits) == sum(domain_contribution$hits))

## Percentages should sum to 100, within numerical tolerance.
stopifnot(isTRUE(all.equal(sum(term_contribution$percent_of_all_hits), 100)))
stopifnot(isTRUE(all.equal(sum(stem_contribution_complete$percent_of_all_hits), 100)))
stopifnot(isTRUE(all.equal(sum(domain_contribution$percent_of_all_hits), 100)))

## Verify that all retained final patterns match at least one feature.
final_pattern_hits <- vapply(
  esfi_dict_final,
  function(pattern) {
    selected <- quanteda::dfm_select(
      dfm_vet_analysis,
      pattern = pattern,
      valuetype = "glob",
      selection = "keep"
    )
    sum(quanteda::colSums(selected))
  },
  numeric(1)
)

stopifnot(all(final_pattern_hits > 0))

final_pattern_summary <- tibble(
  dictionary_pattern = names(final_pattern_hits),
  hits = as.numeric(final_pattern_hits)
)

write.csv(
  final_pattern_summary,
  file.path(output_dir, "final_pattern_summary.csv"),
  row.names = FALSE
)

##-------------------------------------------------------------
## 12. Save a compact development bundle
##-------------------------------------------------------------

## The bundle collects the principal development outputs without storing the
## large token or DFM objects again.
esfi_development_bundle <- list(
  broad_dictionary = esfi_dict_broad,
  candidate_dictionary = esfi_dictionary_candidate,
  final_dictionary = esfi_dict_final,
  kwic_match_summary = kwic_match_summary,
  actual_candidate_matches = actual_candidate_matches,
  actual_final_matches = actual_final_matches,
  term_contribution = term_contribution,
  stem_contribution = stem_contribution_complete,
  domain_contribution = domain_contribution,
  term_by_source = term_by_source,
  decision_log = decision_log,
  final_pattern_summary = final_pattern_summary
)

saveRDS(
  esfi_development_bundle,
  file.path(output_dir, "esfi_dictionary_development_bundle.rds")
)

message("ESFI dictionary development completed successfully.")
message("Outputs written to: ", normalizePath(output_dir, winslash = "/", mustWork = TRUE))
