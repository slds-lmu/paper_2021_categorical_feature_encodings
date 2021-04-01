This repository contains code and results for the paper "Regularized target encoding outperforms traditional methods in supervised machine learning with high cardinality features", submitted for publication at *Computational Statistics and Data Analysis (CSDA)*.
A preprint is available on arXiv under <INSERTLINK>.

DESCRIPTION OF FOLDERS AND FILES: 

**upload_datasets/**:
- scripts were used to upload some benchmark datasets to OpenML

**analysis/high_cardinality_benchmark/**:
- *main.R* builds a batchtools registry containing all computational jobs; sources most other scripts.
- after jobs have been run on some compute cluster, *collect_results.R* extracts the results from the registry; saves the preprocessed results in *results.rds*

**doc/**:
- *high_card_final_datasets.Rmd* documents all datasets used in the benchmark along with some remarks on why they were included; outputs *high_card_final_datasets.html* as well as *analysis/high_cardinality_benchmark/oml_ids.rds* and *analysis/high_cardinality_benchmark/descr_dat.rds* which are used in the benchmark and the manuscript
- *sessionInfo_220319* is a text file documenting the package versions used to run the benchmark analysis on the Linux Cluster of the Leibniz Supercomputing Centre in Garching

**publication/**:
- *manuscript.Rmd* is a reproducible script to build the *manuscript.pdf* submitted for publication to CSDA
- *supplementary_material.Rmd* / *supplementary_material.pdf* contains additional information not included in the manuscript
- *references.bib* contains all references used in the manuscript (and some more)

For questions and remarks feel free to contact the manuscript authors.