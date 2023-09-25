# Notes:

The code in this repository is intended to be run on an HPC. 

## Set up

1) In an interactive session, Install packages found in R/load.R, particularly INLA and scoringutils. On the shell, run:

> salloc

> module load R/4.2.0-foss-2020b
 
> R

> install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

> library(INLA)

> options(timeout=300)

> inla.binary.install()

> library(INLA)

> install.packages('scoringutils')

if there are other packages that need to be installed, instal them here as well.

2) if mod1.sh is created or modified on a Windows machine, you will get an error. In the terminal on the cluster, run:
> dos2unix mod1.sh

3) Run the program by opening the shell, change directory to the directory containing the .sh file using 
> cd XX

4) run salloc mod1.sh
