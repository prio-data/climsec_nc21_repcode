# climsec_nc21_repcode
Replication code for Schutte, Vestby, Carling, and Buhaug "Climatic conditions are weak predictors of asylum migration", forthcoming in Nature Communications (2021).

To keep the code repicable, we rely on the groundhog package in R: https://cran.r-project.org/web/packages/groundhog/index.html

Groundhog loads library versions used at the date of publication of the code. This ensures that future changes to underlying libraries do not affect the replication of results. However, we have experienced problems with resolving underlying package dependencies in groundhog. Associated error messages will mention unknown function calls and can usually be resolved manually by installing associated libraries with install.packages().

One quirk with the code is that we use lz4 compression of our .rds files to shave off some time and space. The program that does lz4 compression must be installed with the following command line (on a .deb-based Linux distribution):

sudo apt-get install liblz4-tool

When piping .rds files to the lz4 compression tool, we use tee. tee should be installed on Ubuntu by default. (See lines 650-657 in the "asylum_analysis_main_replication.R" for the exact code we run.)
Since the script uses the Linux command line and a Linux command line program, we highly recommend running the code on Linux. Windows users might want to bypass this by changing lines 670-673 in the following way:

      fname <- here(RESULT_FOLDER, "models", glue::glue("model_fit_IMP{imp_num}_CV{cv_num}.rds"))
      # con <- lz4_pipe(fname, mode = "write")
      saveRDS(mfit, fname)
      # close(con)

To run replications, download the code into a local directory with a subfolder called "code". Then, download the replication data from Harvard Dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6WRMCO. Replication data should go into a subfolder called "data". 
 
Then, adapt line 8 in all_specifications_replication.R to point to you local code folder. 

To run the estimations (after installing all required libraries and dependencies), ensure that you have about 200GB of disk space available for all estimated models. Alternatively, you can generate main results first by only excuting lines 1-25 in all_specifications_replication.R. Then remove estimated models by cd'ing into the results folder and running the following command:

find . -name *model_fit* -exec rm -rf{} \;

If the full results are to be replicated, simply execute "all_specifications_replication.R". Several hours of CPU time will be required.

R sessionInfo():

R version 4.0.2 (2020-06-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=nb_NO.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=nb_NO.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=nb_NO.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=nb_NO.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggthemes_4.2.0  here_0.1        skimr_2.1.1     patchwork_1.0.1 iml_0.10.0      ranger_0.12.1   Amelia_1.7.6    Rcpp_1.0.4.6    yardstick_0.0.6 parsnip_0.1.1  
[11] recipes_0.1.13  rsample_0.0.6   forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.4     readr_1.3.1     tidyr_1.1.0     tibble_3.0.1    ggplot2_3.3.2  
[21] tidyverse_1.3.0

loaded via a namespace (and not attached):
 [1] colorspace_1.4-1   ellipsis_0.3.1     class_7.3-17       rprojroot_1.3-2    snakecase_0.11.0   base64enc_0.1-3    fs_1.4.1           rstudioapi_0.11   
 [9] listenv_0.8.0      furrr_0.1.0        prodlim_2019.11.13 fansi_0.4.1        lubridate_1.7.9    xml2_1.3.2         codetools_0.2-16   splines_4.0.2     
[17] knitr_1.29         texreg_1.37.5      jsonlite_1.7.0     pROC_1.16.2        Metrics_0.1.4      packrat_0.5.0      broom_0.5.6        dbplyr_1.4.4      
[25] compiler_4.0.2     httr_1.4.1         backports_1.1.8    assertthat_0.2.1   Matrix_1.2-18      cli_2.0.2          htmltools_0.5.0    tools_4.0.2       
[33] gtable_0.3.0       glue_1.4.1         reshape2_1.4.4     cellranger_1.1.0   vctrs_0.3.1        nlme_3.1-147       timeDate_3043.102  gower_0.2.2       
[41] xfun_0.15          globals_0.12.5     rvest_0.3.5        lifecycle_0.2.0    future_1.17.0      MASS_7.3-51.6      zoo_1.8-8          scales_1.1.1      
[49] ipred_0.9-9        hms_0.5.3          parallel_4.0.2     gridExtra_2.3      rpart_4.1-15       stringi_1.4.6      maptools_1.0-1     e1071_1.7-3       
[57] checkmate_2.0.0    lava_1.6.7         repr_1.1.0         rlang_0.4.6        pkgconfig_2.0.3    lattice_0.20-41    prediction_0.3.14  sf_0.9-4          
[65] tidyselect_1.1.0   plyr_1.8.6         magrittr_1.5       R6_2.4.1           generics_0.0.2     DBI_1.1.0          pillar_1.4.4       haven_2.3.1       
[73] foreign_0.8-79     withr_2.2.0        units_0.6-7        survival_3.1-12    sp_1.4-2           nnet_7.3-14        janitor_2.0.1      modelr_0.1.8      
[81] crayon_1.3.4       KernSmooth_2.23-17 grid_4.0.2         readxl_1.3.1       data.table_1.12.8  blob_1.2.1         reprex_0.3.0       digest_0.6.25     
[89] classInt_0.4-3     xtable_1.8-4       munsell_0.5.0  
