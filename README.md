# SNP_2022
 Workflow allowing to performed the preliminary SNP analysis

## requirements

PATRIC Command Line Interface (https://docs.patricbrc.org//cli_tutorial/cli_installation.html#cli-installation)

python >= 3.8.8

bash

### _Python_ mods
os

sys

argparse

re

## script description

#### prepare Rast launch command line
``` shell
python annot_fasta.py -f /perso/lorenzi/Documents/SNP_2022/data/genome -g /perso/lorenzi/Documents/SNP_2022/data/gbk -r /perso/lorenzi/Documents/SNP_2022/data/rast -t 1883
```

#### Launch Rast run
``` shell
path=~/Documents/SNP_2022/data/rast/run
for file in $path/*
do
  bash $file
done
```

#### Prepare Rast retrieve command line
``` shell
python annot_fasta.py -f /perso/lorenzi/Documents/SNP_2022/data/genome -g /perso/lorenzi/Documents/SNP_2022/data/gbk -r /perso/lorenzi/Documents/SNP_2022/data/rast -t 1883
```

#### retrieve bash rast run
``` shell
path=~/Documents/SNP_2022/data/rast/retrieve
for file in $path/*
do
  bash $file
done
```

#### generate json file from gbk
``` shell
python genbank_to_json.py -p /perso/lorenzi/Documents/SNP_2022/data/gbk/ -o /perso/lorenzi/Documents/SNP_2022/data/json/ -a gbk
```

#### run blast
``` shell
python run_blast.py -i /perso/lorenzi/Documents/SNP_2022/data/json/ -b /perso/lorenzi/Documents/SNP_2022/data/blast/run_1/ -n /perso/lorenzi/Documents/SNP_2022/data/species_run_1.tab
```

#### core genome
``` shell
python make_core_genome.py -b /perso/lorenzi/Documents/SNP_2022/data/blast/run_1/blast_res/ -r run_1 -o /perso/lorenzi/Documents/SNP_2022/data/
```

#### persistance

``` shell
python make_persistence.py -r run_1 -p /perso/lorenzi/Documents/SNP_2022/data/ -b /perso/lorenzi/Documents/SNP_2022/data/blast/run_1/blast_res/
```

#### sumup file
``` shell
python species_sumup.py -i /perso/lorenzi/Documents/SNP_2022/data/json/ -o /perso/lorenzi/Documents/SNP_2022/data/ -j txt
```

### dN/dS (en stand by, combinatoire trop importante) le script dnds_by_ort.py ne fait que preparer les donnees pour l'alignement proteique pour l'instant
``` shell
python snp_by_ort.py -o /perso/lorenzi/Documents/SNP_2022/data/ortholog_run_1.txt -i /perso/lorenzi/Documents/SNP_2022/data/json/ -a /perso/lorenzi/Documents/SNP_2022/data/mafft/run_1/
```

### prepare the sumup file for the database
``` shell
python database_creation/prep_data_database.py -j /perso/lorenzi/Documents/SNP_2022/data/json/ -s /perso/lorenzi/Documents/SNP_2022/data/
```

## database construction (on local PC, path change)
### create database
# in sql shell
``` sql shell
CREATE DATABASE snp_2022;
```

### create table
``` shell
cd database_creation
python create_table.py
```

### insert data in the database
``` shell
python remove_database.py && python create_table.py && python insert_data.py -r C:/Users/jean-/OneDrive/Documents/SNP_2022/data/species_run_1.tab -j C:/Users/jean-/OneDrive/Documents/SNP_2022/data/json/ -s C:/Users/jean-/OneDrive/Documents/SNP_2022/data/table.csv -o C:/Users/jean-/OneDrive/Documents/SNP_2022/data/ortholog_fam_run_1.txt -c C:/Users/jean-/OneDrive/Documents/SNP_2022/data/core_run_1.txt -p C:/Users/jean-/OneDrive/Documents/SNP_2022/data/persistence/
```

``` sql
SELECT cds.cds_id
FROM cds
JOIN replicon ON cds.replicon_id = replicon.replicon_id
WHERE replicon_type = 'plasmid' and
cds.genome_id = 'RLA212_072020'
```

# launch mauve command line
progressiveMauve --output=/perso/lorenzi/Documents/SNP_2022/data/mauve/test_vm /perso/lorenzi/Documents/SNP_2022/data/gbk/RLA212_072020.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RLB133_072020.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RLB1-8.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RLB1-9.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RLB3-17.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RLB3-6.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RPA42_072020.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/RPA45_072020.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/S1A1-3.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/S1A1-7.gbk  /perso/lorenzi/Documents/SNP_2022/data/gbk/S1A1-8.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/S1D411_072020.gbk /perso/lorenzi/Documents/SNP_2022/data/gbk/S1D4-14.gbk 

# launch mauve snp exporter command line (require .jar executable)
java org.gel.mauve.analysis.SnpExporter -f test_vm -o aln.snp
