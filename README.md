# Adhesion analysis
Workflow to perform the analysis of the adhesion data generated by @Manon Monnier.

## Requirements

R version 4.1.2 (2021-11-01)

### R package


eadxl 1.3.1, cli 3.1.0, config, svglite 2.0.0, ggplot2 3.3.6, dplyr 1.0.8

## Script description

**_concatenation_data.R_**

Merges all the batch files present in the input directory (**[batches]**). 

The merged files are return in **[concatenate_file]** for the data file and **[concatenate_id_file]** for the id file.

No id correction is done at this stage.


**_load_data.R_**

Selects data according to the variable 'comments' in **[nom du fichier avec les donnees espèces, temperature etc]**

Creates a unique file per id

Recalibrate curves at axis y=0

Creates variable colums for each different protocol

Homogenization of species and strain names

**_index_definition.R_**

Segmentation of curves in six parts

**_interpolation_complete.R_**

Interpolation of the curve from XX to XX

Integrate calculation on the negative part of the curve

Uncertainty calculation

Distance calculation between index5 and index1

**_represention_especes.R_**

Curves represented by group of species




## USAGE

### Preparation
All the parameters must be provided in the _config.yml_ file.

Multiple config can be saved, the **"default"** configuration is currently used.

#### Concatenation
``` shell
Rscript concatenation_data.R
```
