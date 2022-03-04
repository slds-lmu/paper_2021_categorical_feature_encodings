# Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features

This repository contains code and results for the paper "Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features" published in the journal Computational Statistics:

Pargent, F., Pfisterer, F., Thomas, J. et al. Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features. Comput Stat (2022). https://doi.org/10.1007/s00180-022-01207-6

DESCRIPTION OF FOLDERS AND FILES:

**analysis/**:

- **high_cardinality_benchmark/**:
  - *main.R* builds the batchtools *registry/* containing all computational jobs; sources most other .R scripts
  - after jobs have been run on some compute cluster, *collect_results.R* extracts the results from the registry; saves the preprocessed results in *results.rds*

- **upload_datasets/**:
  - scripts were used to upload some benchmark datasets to OpenML

- *install.R*:
  - was used to install packages on the compute cluster before running the benchmark

**publication/**:

- *manuscript.Rmd* is a reproducible script to build the *manuscript.pdf*; loads some .rds files from the analysis folder
- *supplementary_material.Rmd* builds *supplementary_material.pdf* which contains additional information not included in the manuscript
- *references.bib* contains all references used in the manuscript

For questions and remarks feel free to contact the manuscript authors.
