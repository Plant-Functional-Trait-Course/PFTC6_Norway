
# PFTC6 data paper README file

This repository contains the cleaning code for the morphological and
chemical traits (dataset i) from the data paper: Vandvik et al. Plant
trait, carbon flux, reflectance and climate data from global change
experiments and gradients in Norway.

**This repository contains:**

- code to download the raw trait data from [OSF](https://osf.io/fcbw4/)

- code to clean the data and create clean data files

- code to create trait data dictionaries

- code to create a species list, showing the species sampled in the
  different datasets.

Note that the cleaning code for other datasets is in other repositories
(for more details see the paper).

### Reproduce the cleaning code

The cleaning code in this repository is based on a [targets
pipeline](https://books.ropensci.org/targets/) in a [renv
environment](https://rstudio.github.io/renv/articles/renv.html).

To reproduce the cleaning code follow these steps:

1.  Clone this GitHub repository to your local machine.

2.  Run `renv::restore()` to reproduce the environment and download and
    install all R packages that are needed.

3.  Open the `run.R` file and run `library(targets)` and
    `targets::tar_make()` to reproduce the code.

### Data file naming convention

The file names have the following convention:

Project_Status_Experiment_Response_Year(s).Extension

E.g. PFTC_clean_Global_Change_traits_2022.csv
