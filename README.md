# Economic analyses of respiratory tract infection diagnostics: a systematic review 

[![DOI](https://zenodo.org/badge/363175133.svg)](https://zenodo.org/badge/latestdoi/363175133)

This GitHub repository contains code to create the tables as presented in the manuscript mentioned above.

## Use of the script

To get started you will need a recent version of R. The code uses the *checkpoint* package to make the code better reproducible and portable. Upon running the *table_create.R* script, a folder (*.checkpoint*) will be created in the working directory containing the required packages. The script will then create the tables and save them in the *output* folder.

## Packages required

- checkpoint
- gt
- dplyr
- tidyr
- stringr
- scales
- readr

## License

The code is licensed under GNU General Public License v3.0.

## Funding

[![Alt text](https://www.value-dx.eu/wp-content/uploads/2019/07/Logo-300x124.png)](https://www.value-dx.eu/)

This project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking under grant agreement No 820755. This Joint Undertaking receives support from the European Union’s Horizon 2020 research and innovation programme and EFPIA and bioMérieux SA, Janssen Pharmaceutica NV, Accelerate Diagnostics S.L., Abbott, Bio-Rad Laboratories, BD Switzerland Sàrl, and The Wellcome Trust Limited.
