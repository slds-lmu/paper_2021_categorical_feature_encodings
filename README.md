# Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features

This repository contains code and results for the paper "Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features".

DESCRIPTION OF FOLDERS AND FILES:

**analysis**
- **high_cardinality_benchmark/**:
  - *main.R* builds a batchtools registry containing all computational jobs; sources most other scripts.
  - after jobs have been run on some compute cluster, *collect_results.R* extracts the results from the registry; saves the preprocessed results in *results.rds*

- **upload_datasets/**:
  - scripts were used to upload some benchmark datasets to OpenML

- **install.R**:
  - Install all relevant pacakges for running the experiments

**publication/**:
- *manuscript.Rmd* is a reproducible script to build the *manuscript.pdf* submitted for publication to CSDA
- *supplementary_material.Rmd* / *supplementary_material.pdf* contains additional information not included in the manuscript
- *references.bib* contains all references used in the manuscript (and some more)

For questions and remarks feel free to contact the manuscript authors.