# SNP_2022
 Workflow allowing to performed the preliminary SNP analysis

## requirements

R (version ?)

### R package

_Manon écrit la version des packages_

readxl

cli

optparse

config

## script description

_All the path must be provided in the config.yml file_
Multiple config can be saved, the **"default"** configuration is currently used

#### concatenation
``` shell
Rscript concatenation_data.R -i [rep with all batch] -o [path to concatenate data]
```
