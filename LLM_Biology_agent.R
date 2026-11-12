############################################################
# PDAB_2026 — LLM-Assisted Biological Interpretation
#
# Script: Biology Agent
# Author: Melline Fontes Noronha
# Affiliation: Wolfe Lab
#              Loyola University Chicago
#              Stritch School of Medicine
#
# Description:
# Summarizes previously calculated taxon–pathway
# associations and supporting KO-level statistical evidence
# using a locally deployed Llama 3.1 8B model via Ollama.
#
# Associated manuscript:
# Long-read shotgun metagenomics identifies distinct
# urogenital microbial community states associated with
# functional potential and urinary tract infection
# during pregnancy.
#
# Year: 2026
#
# Input:
#   Evidence Agent object created by run_evidence_agent()
#
# Example:
#   gardnerella_abc <- run_evidence_agent(
#     taxon = "Gardnerella vaginalis",
#     pathway = "map02010"
#   )
#
#   biology_abc <- run_biology_agent(gardnerella_abc)
############################################################


############################################################
# 1. LOAD PACKAGES
############################################################

library(dplyr)
library(stringr)
library(httr)
library(jsonlite)


############################################################
# 2. SETTINGS
############################################################

ollama_url <- "http://127.0.0.1:11434/api/generate"
ollama_model <- "llama3.1:8b"

biology_output_directory <- "LLM_biology_reports"

if (!dir.exists(biology_output_directory)) {
  dir.create(
    biology_output_directory,
    recursive = TRUE
  )
}


############################################################
# 3. SAFE FILE-NAME FUNCTION
############################################################

safe_filename <- function(x) {
  
  x %>%
    stringr::str_replace_all(
      "[^A-Za-z0-9_-]+",
      "_"
    ) %>%
    stringr::str_replace_all(
      "_+",
      "_"
    ) %>%
    stringr::str_remove("^_") %>%
    stringr::str_remove("_$")
}


############################################################
# 4. LOCAL OLLAMA CALL
############################################################
#
# You can skip redefining this function if call_ollama()
# already exists in your R session.
#
############################################################

call_ollama <- function(
    prompt,
    model = ollama_model,
    timeout_seconds = 900,
    maximum_tokens = 900,
    temperature = 0.1
) {
  
  response <- httr::POST(
    url = ollama_url,
    
    body = list(
      model = model,
      prompt = prompt,
      stream = FALSE,
      
      options = list(
        num_predict = maximum_tokens,
        num_ctx = 8192,
        temperature = temperature
      )
    ),
    
    encode = "json",
    
    httr::timeout(timeout_seconds)
  )
  
  if (response$status_code != 200) {
    
    error_text <- httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
    
    stop(
      "Ollama request failed. HTTP status: ",
      response$status_code,
      "\n",
      error_text
    )
  }
  
  parsed_response <- httr::content(
    response,
    as = "parsed",
    encoding = "UTF-8"
  )
  
  if (
    is.null(parsed_response$response) ||
    parsed_response$response == ""
  ) {
    stop("Ollama returned an empty response.")
  }
  
  parsed_response$response
}


############################################################
# 5. MICROBIOME BIOLOGY AGENT
############################################################

run_biology_agent <- function(
    evidence_object,
    model = ollama_model,
    timeout_seconds = 900,
    maximum_tokens = 1000,
    temperature = 0.1,
    save_output = TRUE
) {
  
  ##########################################################
  # VALIDATE EVIDENCE OBJECT
  ##########################################################
  
  if (!is.list(evidence_object)) {
    stop(
      "evidence_object must be the list returned by ",
      "run_evidence_agent()."
    )
  }
  
  required_elements <- c(
    "pathway_result",
    "ko_evidence",
    "evidence_summary"
  )
  
  missing_elements <- setdiff(
    required_elements,
    names(evidence_object)
  )
  
  if (length(missing_elements) > 0) {
    stop(
      "The evidence object is missing: ",
      paste(
        missing_elements,
        collapse = ", "
      )
    )
  }
  
  
  ##########################################################
  # EXTRACT EVIDENCE
  ##########################################################
  
  pathway_result <- evidence_object$pathway_result
  ko_evidence <- evidence_object$ko_evidence
  evidence_report <- evidence_object$evidence_summary
  
  if (
    is.null(evidence_report) ||
    length(evidence_report) == 0 ||
    evidence_report == ""
  ) {
    stop(
      "The evidence object does not contain a valid ",
      "evidence_summary."
    )
  }
  
  
  ##########################################################
  # EXTRACT IDENTIFIERS AND COUNTS
  ##########################################################
  
  taxon_name <- as.character(
    pathway_result$Taxon[1]
  )
  
  pathway_id <- as.character(
    pathway_result$KEGG_Pathway[1]
  )
  
  pathway_name <- as.character(
    pathway_result$Pathway_Name[1]
  )
  
  expected_ko_count <- as.integer(
    pathway_result$Number_of_KOs[1]
  )
  
  recovered_ko_count <- nrow(
    ko_evidence
  )
  
  positive_ko_count <- sum(
    ko_evidence$Direction == "Positive",
    na.rm = TRUE
  )
  
  negative_ko_count <- sum(
    ko_evidence$Direction == "Negative",
    na.rm = TRUE
  )
  
  mixed_directions <- (
    positive_ko_count > 0 &&
      negative_ko_count > 0
  )
  
  exact_ko_match <- (
    expected_ko_count ==
      recovered_ko_count
  )
  
  ko_ids <- paste(
    ko_evidence$KO,
    collapse = "; "
  )
  
  
  ##########################################################
  # CREATE MACHINE-CALCULATED CONTEXT
  ##########################################################
  
  calculated_context <- paste0(
    "R-CALCULATED VALIDATION CONTEXT\n",
    "Taxon: ", taxon_name, "\n",
    "Pathway ID: ", pathway_id, "\n",
    "Pathway name: ", pathway_name, "\n",
    "Pathway-level supporting KO count: ",
    expected_ko_count, "\n",
    "Recovered KO records: ",
    recovered_ko_count, "\n",
    "Positive KO associations: ",
    positive_ko_count, "\n",
    "Negative KO associations: ",
    negative_ko_count, "\n",
    "Mixed KO directions: ",
    mixed_directions, "\n",
    "KO-count agreement: ",
    exact_ko_match, "\n",
    "Exact supporting KO identifiers: ",
    ko_ids, "\n"
  )
  
  
  ##########################################################
  # BUILD CONSERVATIVE BIOLOGY PROMPT
  ##########################################################
  
  biology_prompt <- paste0(
    "You are a senior microbiologist specializing in:\n",
    "- urogenital microbiomes\n",
    "- microbial ecology\n",
    "- bacterial physiology\n",
    "- metagenomic functional annotation\n",
    "- KEGG Orthology\n",
    "- microbial metabolism\n\n",
    
    "Your goal is to produce the most conservative biological ",
    "interpretation possible for expert peer review.\n\n",
    
    "This is a urogenital metagenomic study.\n",
    "Never substitute gut, oral, or another body site.\n\n",
    
    "The statistical analysis was performed in R and has already ",
    "been validated. Do not recalculate or change any numerical ",
    "result.\n\n",
    
    "Every substantive statement must begin with exactly one ",
    "of these tags:\n",
    "[EVIDENCE]\n",
    "[ESTABLISHED BIOLOGY]\n",
    "[HYPOTHESIS]\n",
    "[LIMITATION]\n\n",
    
    "Definitions:\n",
    "[EVIDENCE] = directly supported by the supplied study result.\n",
    "[ESTABLISHED BIOLOGY] = general biological knowledge that ",
    "does not claim that the process occurred in this study.\n",
    "[HYPOTHESIS] = a plausible but untested explanation.\n",
    "[LIMITATION] = a restriction on interpretation.\n\n",
    
    "Never mix evidentiary categories within the same statement.\n",
    "Never present a hypothesis as study evidence.\n",
    "Never present general biology as an observation from this ",
    "dataset.\n\n",
    "Never use the phrase 'pathway activity' or 'predicted activity'. ",
    "Use 'abundance of KOs assigned to the pathway' instead.\n",
    
    "Do not say that a taxon contributes to a function. ",
    "State only that taxon abundance co-varied with functional-feature abundance.\n",
    
    "Avoid words such as 'essential', 'crucial', and 'key' unless necessary ",
    "to define universally accepted pathway biology.\n",
    
    "Testable hypotheses must first test taxonomic attribution before ",
    "proposing expression, activity, proteomic, or metabolomic validation.\n",
    
    "ABSOLUTE RULES\n\n",
    
    "1. Do not say that the named taxon possesses, contains, ",
    "encodes, expresses, uses, activates, performs, produces, ",
    "increases, or drives a function unless taxon-resolved gene ",
    "attribution or functional activity was directly measured.\n\n",
    
    "2. Describe the study result as co-variation between taxon ",
    "abundance and annotated functional-feature abundance across ",
    "samples.\n\n",
    
    "3. Do not claim direct functional attribution of a KO or ",
    "pathway to the named taxon.\n\n",
    
    "4. Do not infer:\n",
    "- gene expression\n",
    "- transcription\n",
    "- protein abundance\n",
    "- pathway activity\n",
    "- enzyme activity\n",
    "- metabolic flux\n",
    "- metabolite abundance\n",
    "- substrate concentration\n",
    "- nutrient competition\n",
    "- ecological dominance\n",
    "- clinical effects\n",
    "- host interaction\n",
    "- causation\n\n",
    
    "5. Do not assign substrate specificity unless the supplied ",
    "KO description explicitly identifies that substrate.\n\n",
    
    "6. Interpret individual KOs only using their supplied ",
    "Description, Preferred_name, and EC fields. If an annotation ",
    "is vague, state that its specific contribution cannot be ",
    "resolved from the supplied annotation.\n\n",
    
    "7. Preserve the exact number of supporting KOs reported by ",
    "the Evidence Agent. Do not omit, add, or replace KO IDs.\n\n",
    
    "8. Do not invent citations, publications, clinical outcomes, ",
    "additional taxa, mechanisms, or experimental results.\n\n",
    
    "9. Do not say that the data show an increase or decrease in ",
    "gene abundance unless the evidence explicitly reports a ",
    "directional comparison. For correlation results, use wording ",
    "such as 'greater taxon abundance co-varied with greater ",
    "functional-feature abundance.'\n\n",
    
    "10. If the evidence is insufficient to support a conclusion, ",
    "state exactly:\n",
    "'The available evidence is insufficient to support this ",
    "conclusion.'\n\n",
    
    "11. Metagenomic annotation reflects predicted genomic ",
    "functional potential. It does not demonstrate expression, ",
    "activity, metabolic flux, or metabolite production.\n\n",
    
    "12. Correlation does not establish that the functional ",
    "features originated from the named taxon. Co-occurring ",
    "organisms, shared community structure, genome properties, ",
    "and compositional effects may contribute.\n\n",
    
    "----------------------------------------\n",
    "R-CALCULATED CONTEXT\n",
    "----------------------------------------\n\n",
    
    calculated_context,
    "\n",
    
    "----------------------------------------\n",
    "EVIDENCE AGENT REPORT\n",
    "----------------------------------------\n\n",
    
    evidence_report,
    "\n\n",
    
    "Return exactly these sections:\n\n",
    
    "### Study-supported observations\n",
    "Report only study-supported observations. Preserve the exact ",
    "taxon, pathway, supporting-KO count, direction, and important ",
    "evidence-consistency information.\n\n",
    
    "### Established biological context\n",
    "Briefly explain the pathway or functional class as general ",
    "biology. Do not claim it occurred in this study. Do not assign ",
    "specific substrates beyond the supplied annotations.\n\n",
    
    "### Conservative interpretation\n",
    "Explain only what the observed co-variation may indicate at ",
    "the level of predicted functional representation. Explicitly ",
    "state that direct taxonomic attribution and functional ",
    "activity were not established.\n\n",
    
    "### Alternative explanations\n",
    "Discuss plausible alternatives, including co-occurring taxa, ",
    "shared ecological structure, genome-size or copy-number ",
    "effects, annotation ambiguity, and compositional-data ",
    "interdependence. Label each statement as a hypothesis or ",
    "limitation.\n\n",
    
    "### Testable hypotheses\n",
    "Provide no more than three cautious hypotheses. Each must be ",
    "clearly labeled [HYPOTHESIS] and paired with a suitable ",
    "validation approach such as taxon-resolved read assignment, ",
    "MAG analysis, isolate genomics, metatranscriptomics, ",
    "proteomics, or metabolomics.\n\n",
    
    "### Biological limitations\n",
    "State the major limitations of inferring biology from ",
    "taxon-function correlations and metagenomic annotations.\n\n",
    
    "### Overall biological confidence\n",
    "Separate confidence in the statistical association from ",
    "confidence in direct biological attribution or mechanism.\n\n",
    
    "Keep the response concise, cautious, and suitable for ",
    "scientific review."
  )
  
  
  ##########################################################
  # RUN BIOLOGY AGENT
  ##########################################################
  
  message(
    "Running Microbiome Biology Agent for ",
    taxon_name,
    " / ",
    pathway_name,
    "..."
  )
  
  biology_text <- call_ollama(
    prompt = biology_prompt,
    model = model,
    timeout_seconds = timeout_seconds,
    maximum_tokens = maximum_tokens,
    temperature = temperature
  )
  
  
  ##########################################################
  # CREATE REPORT
  ##########################################################
  
  report <- paste0(
    "# Microbiome Biology Agent report\n\n",
    
    "## Analysis target\n\n",
    "- Taxon: ", taxon_name, "\n",
    "- Pathway: ", pathway_name,
    " (", pathway_id, ")\n",
    "- Expected supporting KOs: ",
    expected_ko_count, "\n",
    "- Recovered supporting KOs: ",
    recovered_ko_count, "\n",
    "- Positive KOs: ",
    positive_ko_count, "\n",
    "- Negative KOs: ",
    negative_ko_count, "\n\n",
    
    "## Evidence Agent input\n\n",
    evidence_report,
    "\n\n",
    
    "## Microbiome Biology Agent output\n\n",
    biology_text,
    "\n\n",
    
    "---\n\n",
    "**Model:** ", model, "\n\n",
    "**Role:** Conservative biological interpretation of an ",
    "R-generated evidence report.\n\n",
    
    "**Important:** This report must be reviewed against the ",
    "underlying data, KO annotations, and peer-reviewed ",
    "literature before manuscript use.\n"
  )
  
  
  ##########################################################
  # SAVE OUTPUT
  ##########################################################
  
  output_file <- NULL
  
  if (save_output) {
    
    output_file <- file.path(
      biology_output_directory,
      paste0(
        safe_filename(taxon_name),
        "__",
        safe_filename(pathway_name),
        "__",
        safe_filename(pathway_id),
        "__biology.md"
      )
    )
    
    writeLines(
      report,
      con = output_file
    )
    
    message(
      "Saved: ",
      output_file
    )
  }
  
  
  ##########################################################
  # PRINT AND RETURN
  ##########################################################
  
  cat(
    "\n",
    biology_text,
    "\n"
  )
  
  invisible(
    list(
      pathway_result = pathway_result,
      ko_evidence = ko_evidence,
      calculated_context = calculated_context,
      evidence_summary = evidence_report,
      biology_prompt = biology_prompt,
      biology_text = biology_text,
      output_file = output_file
    )
  )
}
