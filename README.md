# Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features

This repository contains code and results for the paper "Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features" published in *Computational Statistics*. Please cite:

Pargent, F., Pfisterer, F., Thomas, J., & Bischl, B. Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features. *Computational Statistics* (2022). https://doi.org/10.1007/s00180-022-01207-6

**NOTES:** (*relevant for people trying to reproduce our results*)

- Unfortunately, many benchmark datasets we uploaded to the OpenML platform ourselves are not correctly displayed on the OpenML homepage. We do not know how to fix this without changing the datasets that were actually used (and also downloaded from OpenML), when running the benchmark experiment.
- Those datasets are labeled on the OpenML homepage with status *"in_preparation"* and are thus not found when searching only for *"active"* datasets (the default).
- For some datasets, the OpenML homepage reports errors which apparently prevent it to correctly display feature statistics.
- Despite these inconveniences, to the best of our knowledge (10.03.2022) **all datasets are functional and can be downloaded** with the R code reported in this repository!

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
