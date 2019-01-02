# A new perspective on global renewable energy systems: why trade in energy carriers matters - Scripts for the Paper

by
Johannes Schmidt, Katharina Gruber, Michael Klingler, Claude Klöckl, Luis Ramirez Camargo, Peter Regner, Olga Turkovska, Sebastian Wehrle, Elisabeth Wetterlund
https://refuel.world


# How to run

The scripts need to be run from RStudio. If not run from R-Studio, the working directory has to be set manually at the top of each file.

First run 00_reFUEL_Download.R to download necessary files and read the instructions in 00_reFUEL_Download.R on how to download files that cannot be downloaded automatically.

# Requirements

The easiest way to install the requirements in Ubuntu/Debian or a similar Debian derivative is to run the following command:

    $ sudo apt install r-base r-cran-formatr r-cran-shiny r-cran-rstudioapi r-cran-readxl r-cran-openxlsx r-cran-rlang r-cran-zoo libudunits2-dev libgeos-dev r-cran-raster libgdal-dev libgdal-dev littler libxml2-dev libcurl4-openssl-dev libssl-dev

Then install two more packages using the R package manager in an R prompt:

    > install.packages("tidyverse")
    > install.packages("ggrepel")

Tested using Xubuntu 18.10, minor adaptations might be necessary for a different OS/version.
