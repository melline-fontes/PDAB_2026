############################################################
# PDAB_2026 — LLM-Assisted Biological Interpretation
#
# Script: Biological Evidence Agent
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
############################################################
############################################################
# EVIDENCE AGENT
#
# Purpose:
#   Summarize one taxon–pathway association using only:
#   - pathway-level statistics
#   - supporting KO-level statistics
#   - supplied KO annotations
#
# It must not:
#   - explain mechanisms
#   - infer pathway activity
#   - discuss literature
#   - generate hypotheses
############################################################

library(dplyr)
library(stringr)
library(httr)
library(jsonlite)


############################################################
# 1. SETTINGS
############################################################

ollama_url <- "http://127.0.0.1:11434/api/generate"
ollama_model <- "llama3.1:8b"

evidence_output_directory <- "LLM_evidence_reports"

if (!dir.exists(evidence_output_directory)) {
  dir.create(
    evidence_output_directory,
    recursive = TRUE
  )
}


############################################################
# 2. SAFE FILE NAME
############################################################

safe_filename <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9_-]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_remove("^_") %>%
    stringr::str_remove("_$")
}


############################################################
# 3. CALL OLLAMA
############################################################

call_ollama <- function(
    prompt,
    model = ollama_model,
    timeout_seconds = 900,
    maximum_tokens = 500,
    temperature = 0
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
    stop(
      "Ollama request failed. HTTP status: ",
      response$status_code,
      "\n",
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    )
  }
  
  parsed <- httr::content(
    response,
    as = "parsed",
    encoding = "UTF-8"
  )
  
  if (
    is.null(parsed$response) ||
    parsed$response == ""
  ) {
    stop("Ollama returned an empty response.")
  }
  
  parsed$response
}


############################################################
# 4. EVIDENCE AGENT FUNCTION
############################################################

run_evidence_agent <- function(
    taxon,
    pathway,
    pathway_table = taxa_pathway_network_edges,
    ko_table = taxa_ko_connections_complete,
    save_output = TRUE
) {
  
  ##########################################################
  # FIND PATHWAY-LEVEL RESULT
  ##########################################################
  
  selected_pathway <- pathway_table %>%
    dplyr::filter(
      Taxon == taxon,
      KEGG_Pathway == pathway
    )
  
  if (nrow(selected_pathway) == 0) {
    stop(
      "No retained association found for:\n",
      "Taxon: ", taxon, "\n",
      "Pathway: ", pathway
    )
  }
  
  selected_pathway <- selected_pathway[1, ]
  
  
  ##########################################################
  # PARSE SUPPORTING KO LIST
  ##########################################################
  
  supporting_kos <- unlist(
    strsplit(
      selected_pathway$KO_List,
      split = ";",
      fixed = TRUE
    )
  )
  
  supporting_kos <- stringr::str_trim(supporting_kos)
  supporting_kos <- supporting_kos[supporting_kos != ""]
  
  
  ##########################################################
  # COLLECT TAXON-SPECIFIC KO EVIDENCE
  ##########################################################
  
  ko_evidence <- ko_table %>%
    dplyr::filter(
      Taxon == taxon,
      KO %in% supporting_kos
    ) %>%
    dplyr::select(
      KO,
      Description,
      Preferred_name,
      EC,
      Rho,
      Absolute_Rho,
      P_value,
      Padj,
      Direction
    ) %>%
    dplyr::distinct(
      KO,
      .keep_all = TRUE
    ) %>%
    dplyr::arrange(
      dplyr::desc(Absolute_Rho)
    )
  
  
  ##########################################################
  # VALIDATION CHECKS
  ##########################################################
  
  if (nrow(ko_evidence) == 0) {
    stop(
      "The pathway result was found, but no supporting KOs ",
      "were found in the KO table."
    )
  }
  
  if (nrow(ko_evidence) != selected_pathway$Number_of_KOs) {
    warning(
      "Pathway summary reports ",
      selected_pathway$Number_of_KOs,
      " supporting KOs, but ",
      nrow(ko_evidence),
      " were recovered from the KO table."
    )
  }
  
  
  ##########################################################
  # FORMAT KO TABLE AS TEXT
  ##########################################################
  
  ko_lines <- apply(
    ko_evidence,
    1,
    function(x) {
      
      description <- x[["Description"]]
      
      if (
        is.na(description) ||
        description == ""
      ) {
        description <- "No description supplied"
      }
      
      preferred_name <- x[["Preferred_name"]]
      
      if (
        is.na(preferred_name) ||
        preferred_name == ""
      ) {
        preferred_name <- "NA"
      }
      
      ec_value <- x[["EC"]]
      
      if (
        is.na(ec_value) ||
        ec_value == ""
      ) {
        ec_value <- "NA"
      }
      
      paste0(
        "- KO: ", x[["KO"]],
        "\n  Description: ", description,
        "\n  Preferred name: ", preferred_name,
        "\n  EC: ", ec_value,
        "\n  Spearman rho: ",
        round(as.numeric(x[["Rho"]]), 3),
        "\n  Absolute rho: ",
        round(as.numeric(x[["Absolute_Rho"]]), 3),
        "\n  Raw p-value: ",
        format(
          as.numeric(x[["P_value"]]),
          scientific = TRUE,
          digits = 3
        ),
        "\n  FDR-adjusted p-value: ",
        format(
          as.numeric(x[["Padj"]]),
          scientific = TRUE,
          digits = 3
        ),
        "\n  Direction: ", x[["Direction"]]
      )
    }
  )
  
  ko_text <- paste(
    ko_lines,
    collapse = "\n\n"
  )
  
  
  ##########################################################
  # BUILD STRICT EVIDENCE PROMPT
  ##########################################################
  
  prompt_text <- paste0(
    "You are an evidence summarization agent for a urogenital ",
    "metagenomic functional analysis.\n\n",
    
    "Your task is to report only the supplied evidence. ",
    "Do not interpret biological meaning. ",
    "Do not explain mechanisms. ",
    "Do not discuss pathway activity, gene expression, ",
    "metabolite production, clinical relevance, causation, ",
    "ecology, literature, or hypotheses.\n\n",
    
    "Do not add facts from your background knowledge.\n",
    "Do not assign functions beyond the supplied descriptions.\n",
    "Do not change the body site: this is a urogenital metagenomic study.\n",
    "Do not use the words 'activity', 'enriched', 'drives', ",
    "'causes', 'contributes', or 'produces' unless those exact ",
    "claims appear in the supplied evidence.\n\n",
    
    "PATHWAY-LEVEL EVIDENCE\n",
    "Taxon: ", selected_pathway$Taxon, "\n",
    "KEGG pathway ID: ",
    selected_pathway$KEGG_Pathway, "\n",
    "Pathway name: ",
    selected_pathway$Pathway_Name, "\n",
    "Number of supporting KOs: ",
    selected_pathway$Number_of_KOs, "\n",
    "Mean Spearman rho: ",
    round(selected_pathway$Mean_Rho, 3), "\n",
    "Mean absolute Spearman rho: ",
    round(selected_pathway$Mean_Absolute_Rho, 3), "\n",
    "Minimum FDR-adjusted p-value: ",
    format(
      selected_pathway$Minimum_Padj,
      scientific = TRUE,
      digits = 3
    ),
    "\n",
    "Summary direction: ",
    selected_pathway$Direction, "\n\n",
    
    "SUPPORTING KO-LEVEL EVIDENCE\n",
    ko_text,
    "\n\n",
    
    "Return exactly these sections:\n",
    "### Quantitative summary\n",
    "State only the taxon, pathway, direction, number of KOs, ",
    "mean rho, mean absolute rho, and minimum FDR.\n\n",
    
    "### Supporting KO evidence\n",
    "List each KO with its supplied description, preferred name, ",
    "EC number, rho, and FDR. Do not reinterpret the annotation.\n\n",
    
    "### Evidence consistency check\n",
    "State whether the number of recovered KO records matches ",
    "the pathway-level Number_of_KOs value. ",
    "State whether all KO-level directions agree with the ",
    "pathway-level summary direction.\n\n",
    
    "### Evidence boundaries\n",
    "State that these data show statistical co-variation between ",
    "taxon abundance and annotated functional abundance. ",
    "State that no biological mechanism, pathway activity, gene ",
    "expression, metabolite production, or causation can be ",
    "concluded from this evidence alone.\n"
  )
  
  
  ##########################################################
  # RUN AGENT
  ##########################################################
  
  message(
    "Running evidence agent for ",
    taxon,
    " / ",
    selected_pathway$Pathway_Name,
    "..."
  )
  
  evidence_summary <- call_ollama(
    prompt = prompt_text,
    temperature = 0,
    maximum_tokens = 600
  )
  
  
  ##########################################################
  # BUILD REPORT
  ##########################################################
  
  report <- paste0(
    "# Evidence report\n\n",
    
    "## Source result\n\n",
    "- Taxon: ", selected_pathway$Taxon, "\n",
    "- Pathway: ", selected_pathway$Pathway_Name,
    " (", selected_pathway$KEGG_Pathway, ")\n",
    "- Supporting KOs: ",
    selected_pathway$Number_of_KOs, "\n",
    "- Mean rho: ",
    round(selected_pathway$Mean_Rho, 3), "\n",
    "- Mean absolute rho: ",
    round(selected_pathway$Mean_Absolute_Rho, 3), "\n",
    "- Minimum FDR: ",
    format(
      selected_pathway$Minimum_Padj,
      scientific = TRUE,
      digits = 3
    ),
    "\n\n",
    
    "## Evidence-agent output\n\n",
    evidence_summary,
    "\n\n",
    
    "---\n\n",
    "**Model:** ", ollama_model, "\n\n",
    "**Role:** Evidence extraction only; no biological ",
    "interpretation was requested.\n"
  )
  
  
  ##########################################################
  # SAVE REPORT
  ##########################################################
  
  output_file <- NULL
  
  if (save_output) {
    
    output_file <- file.path(
      evidence_output_directory,
      paste0(
        safe_filename(taxon),
        "__",
        safe_filename(selected_pathway$Pathway_Name),
        "__",
        pathway,
        "__evidence.md"
      )
    )
    
    writeLines(
      report,
      con = output_file
    )
    
    message("Saved: ", output_file)
  }
  
  
  ##########################################################
  # PRINT AND RETURN
  ##########################################################
  
  cat("\n", evidence_summary, "\n")
  
  invisible(
    list(
      pathway_result = selected_pathway,
      ko_evidence = ko_evidence,
      prompt = prompt_text,
      evidence_summary = evidence_summary,
      output_file = output_file
    )
  )
}
