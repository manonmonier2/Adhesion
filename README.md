# Adhesion analysis
Workflow to perform the analysis of the adhesion data generated by @Manon Monnier.

## Requirements

R version 4.1.2 (2021-11-01)

### R package


eadxl 1.3.1, cli 3.1.0, config, svglite 2.0.0, ggplot2 3.3.6, dplyr 1.0.8

## Script description
All the word between "[" "]" reference to a parameter in the _config.yml_ file.

**_concatenation_batch.R_**

Merges all the batch files present in the input directory **[batches]**. 

The merged files are returned in **[concatenate_file]** for the data file and **[concatenate_id_file]** for the id file.

No id correction is done at this stage.

MM: 2022032405 : changer commentaire par pb_machine


**_concatenation_metadata.R_**

Merges all the metadata files present in the input directory **[metadata]**.
The merged file is returned in **[concatenate_metadata]**.

At this stage, corrections are made to species names, comments, protocols and stocks.

This script also contains the correspondence table between the protocols and the conditions in the form of a boolean table. This table is not written for the moment.

@ Add a table of conversions in the readme (line 22 to 37 of concatenation_metadata.R)

**_load_data.R_**

From the concatenated batches file **[concatenate_file]**, the corresponding id file **[concatenate_id_file]** and the concatenated metadata file **[concatenate_metadata]**,
creates an unique batch file per id.

!!! For now, this id must be shared and unique in **[concatenate_id_file]** and **[concatenate_metadata]** !!!
Without this condition, we have no explicit link between batches and metadata.

Recalibrate curves at axis y=0

Creates variable colums for each different protocol

Homogenization of species and strain names

**_index_definition.R_**

Selects data according to the variable 'comments' in **[nom du fichier avec les donnees espèces, temperature etc]**

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
