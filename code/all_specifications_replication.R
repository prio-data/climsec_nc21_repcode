library(groundhog)
groundhog.day="2021-02-20"
pkgs=c('tidyverse','rsample','recipes','parsnip','yardstick','Amelia','ranger','iml','patchwork','skimr','svglite','ggthemes','egg','cshapes', 'crayon', 'DBI', 'dplyr', 'ggplot2', 'hms', 'lifecycle',
       'Rcpp', 'rlang', 'tibble', 'tictoc','sf','zoo','janitor','texreg')
groundhog.library(pkgs, groundhog.day)  


root_path <- "</path/to/replication code/>code"

setwd(root_path)

TRAINING_PERIOD <- 4
LAG_PREDICTORS <- TRUE
NLAG <- 1
DEPVAR <- "asylum seekers" # "asylum seekers" | "asylum seekers (global stock)" | "refugees (global stock)"
LOG_TRANSFORM_DEPVAR <- TRUE
PER_CAPITA_TRANSFORM_DEPVAR <- TRUE
SPEI_MOVING_AVERAGES_3_6 <- FALSE
NSTEP_AHEAD <- 1
CALCULATE_INTERACTIONS <- TRUE # If TRUE, the script will calculate variable interaction levels. This is VERY time consuming (we only do for select global parameters).

tic()
source("asylum_analysis_main_replication.R")
toc()


CALCULATE_INTERACTIONS <- FALSE # If TRUE, the script will calculate variable interaction levels. This is VERY time consuming (we only do for select global parameters).
NLAG <- 2
tic()
source("asylum_analysis_main_replication.R")
toc()

NLAG <- 1
LAG_PREDICTORS <- FALSE
tic()
source("asylum_analysis_main_replication.R")
toc()

LAG_PREDICTORS <- TRUE
TRAINING_PERIOD <- 8
tic()
source("asylum_analysis_main_replication.R")
toc()

TRAINING_PERIOD <- 12
tic()
source("asylum_analysis_main_replication.R")
toc()

TRAINING_PERIOD <- 4
NSTEP_AHEAD <- 8
tic()
source("asylum_analysis_main_replication.R")
toc()

NSTEP_AHEAD <- 3
tic()
source("asylum_analysis_main_replication.R")
toc()

NSTEP_AHEAD <- 1
LOG_TRANSFORM_DEPVAR <- FALSE
PER_CAPITA_TRANSFORM_DEPVAR <- FALSE
tic()
source("asylum_analysis_main_replication.R")
toc()

LOG_TRANSFORM_DEPVAR <- TRUE
PER_CAPITA_TRANSFORM_DEPVAR <- TRUE
SPEI_MOVING_AVERAGES_3_6 <- TRUE 
tic()
source("asylum_analysis_main_replication.R")
toc()

