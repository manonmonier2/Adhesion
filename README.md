# Adhesion analysis
Workflow to perform the analysis of the adhesion data generated by @Manon Monnier.

## Requirements

R (version ?)

### R package

_Manon écrit la version des packages_

readxl, cli, config

## Script description

_concatenation_data.R_ 

Merges all the batch files present in the input directory (**batches**). 

The merged files are return in **concatenate_file** for the data file and **concatenate_id_file** for the id file.

No id correction is done at this stage.

## USAGE

### Preparation
All the parameters must be provided in the _config.yml_ file.

Multiple config can be saved, the **"default"** configuration is currently used.

#### Concatenation
``` shell
Rscript concatenation_data.R
```
