# PDAB_2026

## LLM-Assisted Interpretation of Taxon–Pathway Associations

This repository provides supporting materials for the large language model (LLM)-assisted interpretation performed in the manuscript:

**"Long-read shotgun metagenomics identifies distinct urogenital microbial community states associated with functional potential and urinary tract infection during pregnancy."**

**Wolfe Lab — Loyola University Chicago**

## Overview

A locally deployed **Llama 3.1 8B** model, accessed through **Ollama**, was used to support the interpretation of selected taxon–pathway associations identified through statistical analyses of urogenital shotgun metagenomic data.

The workflow consists of two agents:

### 1. Evidence Agent

The Evidence Agent summarizes previously calculated statistical evidence, including:

- Spearman correlation coefficients
- FDR-adjusted p-values
- Number of supporting KEGG Orthologs (KOs)
- Direction and consistency of associations
- Functional annotations

The agent does not perform statistical calculations or independently infer biological mechanisms.

### 2. Biology Agent

The Biology Agent receives the Evidence Agent report and supporting KO-level information to generate conservative biological interpretations.

Its instructions distinguish between:

- Study-supported evidence
- Established biological knowledge
- Testable hypotheses
- Methodological limitations

The agents do not establish causation, direct taxonomic attribution, or experimentally validated pathway activity.

## Model Configuration

| Parameter | Evidence Agent | Biology Agent |
|---|---|---|
| Model | Llama 3.1 8B | Llama 3.1 8B |
| Platform | Ollama (local) | Ollama (local) |
| Temperature | 0 | 0.1 |
| Context window | 8,192 tokens | 8,192 tokens |
| Maximum output tokens | 600 | 1,000 |

## Repository Contents

The repository is intended to provide documentation and materials related specifically to the LLM workflow, including agent instructions, prompts, and generated reports.

## Important Considerations

All statistical associations were calculated independently of the LLM workflow.

LLM-generated interpretations were intended to support biological discussion and hypothesis generation. They should not be considered independent experimental validation or evidence of causal relationships.

## Citation

**Fontes Noronha et al.**

*Long-read shotgun metagenomics identifies distinct urogenital microbial community states associated with functional potential and urinary tract infection during pregnancy.*

Manuscript submitted to *Frontiers in Microbiology*.

## Contact

**Melline Fontes Noronha**  
Senior Bioinformatician | Head of Bioinformatics  
Wolfe Lab — Loyola University Chicago  
Stritch School of Medicine
