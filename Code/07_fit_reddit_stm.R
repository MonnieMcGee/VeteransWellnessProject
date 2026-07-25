###############################################################
## 07_fit_reddit_stm.R
##
## Purpose
## -------
## Select the number of topics and fit the final structural topic
## model using the frozen STM corpus produced by
## 06_prepare_stm_corpus.R.
###############################################################

source("Code/00_project_setup.R")
run_search_k <- FALSE
refit_final_stm <- TRUE

##-------------------------------------------------------------
## 1. Load frozen STM objects
##-------------------------------------------------------------

reddit_stm_input <- readRDS(
  file.path(
    deriveddata_dir,
    "reddit_stm_input.rds"
  )
)

reddit_corpus_stm <- readRDS(
  file.path(
    deriveddata_dir,
    "reddit_corpus_stm.rds"
  )
)

reddit_dfm_stm <- readRDS(
  file.path(
    deriveddata_dir,
    "reddit_dfm_stm.rds"
  )
)

reddit_metadata_stm <- readRDS(
  file.path(
    deriveddata_dir,
    "reddit_metadata_stm.rds"
  )
)

##-------------------------------------------------------------
## 2. Verify STM input alignment
##-------------------------------------------------------------

stopifnot(
  length(reddit_stm_input$documents) ==
    quanteda::ndoc(reddit_dfm_stm),
  nrow(reddit_stm_input$meta) ==
    quanteda::ndoc(reddit_dfm_stm),
  nrow(reddit_metadata_stm) ==
    quanteda::ndoc(reddit_dfm_stm),
  identical(
    quanteda::docnames(reddit_corpus_stm),
    quanteda::docnames(reddit_dfm_stm)
  ),
  identical(
    as.character(reddit_stm_input$meta$doc_id),
    as.character(
      quanteda::docnames(reddit_dfm_stm)
    )
  ),
  identical(
    as.character(reddit_metadata_stm$doc_id),
    as.character(
      quanteda::docnames(reddit_dfm_stm)
    )
  ),
  all(
    c("source", "year", "esfi_hits", "esfi_present") %in%
      names(reddit_stm_input$meta)
  )
)

message(
  "STM documents: ",
  length(reddit_stm_input$documents)
)

message(
  "STM vocabulary: ",
  length(reddit_stm_input$vocab)
)

print(
  table(
    reddit_stm_input$meta$source,
    useNA = "ifany"
  )
)

print(
  table(
    reddit_stm_input$meta$year,
    useNA = "ifany"
  )
)

##-------------------------------------------------------------
## 3. Standardize prevalence covariates
##-------------------------------------------------------------

reddit_stm_input$meta <- reddit_stm_input$meta |>
  dplyr::mutate(
    doc_id = as.character(doc_id),
    source = factor(
      source,
      levels = c(
        "Dallas",
        "Finance",
        "Veteran"
      )
    ),
    year = as.integer(year),
    esfi_hits = as.numeric(esfi_hits),
    esfi_present = as.logical(esfi_present)
  )

stopifnot(
  identical(
    reddit_stm_input$meta$doc_id,
    as.character(
      quanteda::docnames(reddit_dfm_stm)
    )
  ),
  identical(
    as.character(
      reddit_stm_input$meta$source
    ),
    as.character(
      reddit_metadata_stm$source
    )
  ),
  identical(
    as.integer(
      reddit_stm_input$meta$year
    ),
    as.integer(
      reddit_metadata_stm$year
    )
  )
)

##-------------------------------------------------------------
## 4. Select the number of topics
##-------------------------------------------------------------

if (run_search_k) {

  set.seed(1234)

  k_candidates <- c(10, 12, 15, 18, 20)

  k_result <- stm::searchK(
    documents = reddit_stm_input$documents,
    vocab = reddit_stm_input$vocab,
    K = k_candidates,
    prevalence = ~ year + source,
    data = reddit_stm_input$meta,
    init.type = "Spectral",
    heldout.seed = 1234
  )

  k_diagnostics <- k_result$results |>
    tibble::as_tibble() |>
    dplyr::transmute(
      K = as.integer(K),
      exclusivity = as.numeric(exclus),
      semantic_coherence = as.numeric(semcoh),
      heldout_likelihood = as.numeric(heldout),
      residual = as.numeric(residual),
      lower_bound = purrr::map_dbl(
        lbound,
        \(x) as.numeric(x)[1]
      ),
      em_iterations = purrr::map_int(
        em.its,
        \(x) as.integer(x)[1]
      )
    ) |>
    dplyr::arrange(K)

  print(k_diagnostics)

  plot(k_result)

  write.csv(
    k_diagnostics,
    file.path(
      reddit_stm_output_dir,
      "reddit_stm_k_diagnostics.csv"
    ),
    row.names = FALSE
  )

  saveRDS(
    k_result,
    file.path(
      reddit_stm_output_dir,
      "reddit_searchK.rds"
    )
  )
}

##-------------------------------------------------------------
## 5. Fit the final STM
##-------------------------------------------------------------

## Update only if the new diagnostics support a different choice.
K_final <- 12

if (refit_final_stm) {
  set.seed(1234)

  reddit_stm_model <- stm::stm(
    documents = reddit_stm_input$documents,
    vocab = reddit_stm_input$vocab,
    K = K_final,
    prevalence = ~ year + source,
    data = reddit_stm_input$meta,
    init.type = "Spectral",
    seed = 1234,
    max.em.its = 250
  )
}

##-------------------------------------------------------------
## 6. Check convergence and dimensions
##-------------------------------------------------------------

print(reddit_stm_model$convergence)

stopifnot(
  nrow(reddit_stm_model$theta) ==
    length(reddit_stm_input$documents)
)

stopifnot(
  ncol(reddit_stm_model$theta) ==
    K_final
)

stopifnot(
  all(
    abs(
      rowSums(reddit_stm_model$theta) - 1
    ) < 1e-6
  )
)

##-------------------------------------------------------------
## 7. Inspect topic terms
##-------------------------------------------------------------

topic_labels <- stm::labelTopics(
  reddit_stm_model,
  n = 20
)

print(topic_labels)

# Examine correlation among topics
stm::topicCorr(reddit_stm_model)

capture.output(
  topic_labels,
  file = file.path(
    reddit_stm_output_dir,
    "reddit_stm_topic_labels.txt"
  )
)

##-------------------------------------------------------------
## 8. Create aligned text vector for representative documents
##-------------------------------------------------------------

texts_stm <- as.character(
  reddit_corpus_stm
)

names(texts_stm) <- quanteda::docnames(
  reddit_corpus_stm
)

texts_stm <- texts_stm[
  match(
    reddit_stm_input$meta$doc_id,
    names(texts_stm)
  )
]

stopifnot(!anyNA(texts_stm))
stopifnot(
  length(texts_stm) ==
    nrow(reddit_stm_model$theta)
)

## Do not hard-code substantive topic numbers until the newly fitted
## model has been inspected. Start by reviewing all topics or specify
## selected topics after labeling the final solution.

thoughts_all_topics <- stm::findThoughts(
  reddit_stm_model,
  texts = texts_stm,
  topics = seq_len(K_final),
  n = 10
)

capture.output(
  thoughts_all_topics,
  file = file.path(
    reddit_stm_output_dir,
    "reddit_stm_representative_documents.txt"
  )
)

##-------------------------------------------------------------
## 9. Save final STM objects
##-------------------------------------------------------------

saveRDS(
  reddit_stm_model,
  file.path(
    reddit_stm_output_dir,
    "reddit_stm_model.rds"
  )
)

saveRDS(
  texts_stm,
  file.path(
    reddit_stm_output_dir,
    "reddit_stm_texts.rds"
  )
)

saveRDS(
  k_result,
  file.path(
    reddit_stm_output_dir,
    "reddit_searchK.rds"
  )
)

saveRDS(
  list(
    K_final = K_final,
    k_candidates = k_candidates,
    k_diagnostics = k_diagnostics,
    convergence = reddit_stm_model$convergence,
    topic_labels = topic_labels
  ),
  file.path(
    reddit_stm_output_dir,
    "reddit_stm_fit_summary.rds"
  )
)

message("07_fit_reddit_stm.R completed successfully.")
message(
  "Outputs written to: ",
  normalizePath(
    reddit_stm_output_dir,
    winslash = "/",
    mustWork = TRUE
  )
)
