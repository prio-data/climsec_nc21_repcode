#### GLOBAL PARAMETERS, to be set in all_specifications_replication.R ####
#TRAINING_PERIOD <- 4
#LAG_PREDICTORS <- TRUE
#LAG <- 1
#DEPVAR <- "asylum seekers" # "asylum seekers" | "asylum seekers (global stock)" | "refugees (global stock)"
#LOG_TRANSFORM_DEPVAR <- TRUE
#PER_CAPITA_TRANSFORM_DEPVAR <- TRUE
#SPEI_MOVING_AVERAGES_3_6 <- FALSE#TRUE
#NSTEP_AHEAD <- 1
#CALCULATE_INTERACTIONS <- FALSE # If TRUE, the script will calculate variable interaction levels. This is VERY time consuming (we only do for select global parameters).

N_IMPUTATIONS <- 10
NUM_THREADS <- 15
N_TREES <- 1000
CALCULATE_IMPORTANCE <- TRUE

TRAIN_MODELS <- TRUE

DEPVAR_UNDERSCORE <- gsub(" ", "_", DEPVAR)
DEPVAR_TC <- paste0(toupper(substring(DEPVAR, 1, 1)), substring(DEPVAR, 2))
DEPVAR_TC_BREAKS <- sub(" ", "\n", sub(" ", "\n", DEPVAR_TC)) # replace first and second occurence

#### DEFINE RESULTS FOLDER ####
RESULT_FOLDER <- "results"
RESULT_FOLDER <- paste0(RESULT_FOLDER, "_TP", TRAINING_PERIOD)

if(!LOG_TRANSFORM_DEPVAR){ RESULT_FOLDER <- paste(RESULT_FOLDER, "NOLOGDEP", sep = "_") }
if(!PER_CAPITA_TRANSFORM_DEPVAR){ RESULT_FOLDER <- paste(RESULT_FOLDER, "NOPCDEP", sep = "_") }
if(LAG_PREDICTORS){ RESULT_FOLDER <- paste(RESULT_FOLDER, paste0("LAG", NLAG), sep = "_") }
if(NSTEP_AHEAD>1){ RESULT_FOLDER <- paste(RESULT_FOLDER, paste0("NAHEAD", NSTEP_AHEAD), sep = "_") }
if(SPEI_MOVING_AVERAGES_3_6){ RESULT_FOLDER <- paste(RESULT_FOLDER, "SPEIR36", sep = "_") }

#### READ DATA ####
eucountries <- 
  "year, n
1958, 6
1973, 3
1981, 1
1986, 2
1995, 3
2004, 10
2007, 2
2013, 1
"
eucountries <- readr::read_csv(eucountries)

get_numeu <- function(myyear){
  filter(eucountries, year <= myyear) %>% summarize(n = sum(n)) %>% pull(n)
}

numeu <- tibble("year" = 1958:2019)
numeu$numeu <-  sapply(numeu$year, get_numeu)

original_dataset <- read_csv("../data/dataset/replication_data.csv")
original_dataset$n_asylum[is.na(original_dataset$n_asylum)] <- 0
original_dataset$n_asylum_eu28[is.na(original_dataset$n_asylum_eu28)] <- 0
original_dataset$n_refugees[is.na(original_dataset$n_refugees)] <- 0
original_dataset$spei3_gs_neg <- as.numeric(original_dataset$spei3_gs_neg)*12 # multiply with 12 because we erroneously calculated the average over each year, rather than the sum.
original_dataset$spei3_gs_pos <- as.numeric(original_dataset$spei3_gs_pos)*12 # multiply with 12 because we erroneously calculated the average over each year, rather than the sum.
original_dataset <- dplyr::rename(original_dataset, physical_integrity = civil_liberties_combined)

# remove microstates from the dataset
microstates <- read.csv("../data/microstates.dat", header = F, sep = "\t")
original_dataset <- dplyr::filter(original_dataset, !(gwcode %in% c(microstates$V1)))

dataset <- original_dataset


#### DEFINE DEPENDENT VARIABLE ####
if(DEPVAR == "asylum seekers"){
  dataset$orig_depvar <- dataset$n_asylum_eu28
  million <- 1000000
  dataset$ln_depvar_pop <- if_else(dataset$n_asylum_eu28 > 0, dataset$n_asylum_eu28 / (dataset$wdi_pop/ million), 0)
  dataset$ln_depvar_pop <- log(dataset$ln_depvar_pop + 1)
  RESULT_FOLDER <- paste(RESULT_FOLDER, "ASYLUM", sep = "_")
} else if (DEPVAR == "refugees (global stock)") {
  dataset$orig_depvar <- dataset$n_refugees
  million <- 1000000
  dataset$ln_depvar_pop <- if_else(dataset$n_refugees > 0, dataset$n_refugees / (dataset$wdi_pop/ million), 0)
  dataset$ln_depvar_pop <- log(dataset$ln_depvar_pop + 1)
  RESULT_FOLDER <- paste(RESULT_FOLDER, "REFUGEES", sep = "_")
} else if (DEPVAR == "asylum seekers (global stock)") {
  dataset$orig_depvar <- dataset$n_asylum
  million <- 1000000
  dataset$ln_depvar_pop <- if_else(dataset$n_asylum > 0, dataset$n_asylum / (dataset$wdi_pop/ million), 0)
  dataset$ln_depvar_pop <- log(dataset$ln_depvar_pop + 1)
  RESULT_FOLDER <- paste(RESULT_FOLDER, "GSTOCK", sep = "_")
} else {
  stop("DEPVAR must be one of: asylum seekers, asylum seekers (global stock), or refugees (global stock).")
}

dataset$depvar <- dataset$orig_depvar

if(PER_CAPITA_TRANSFORM_DEPVAR){
  million <- 1000000
  dataset$depvar <- if_else(dataset$depvar > 0, dataset$depvar / (dataset$wdi_pop/ million), 0)
}

if(LOG_TRANSFORM_DEPVAR){
  dataset$depvar <- log(dataset$depvar + 1)
}

#Load cshapes for mapping
cshapes <- cshapes::cshp() %>% sf::st_as_sf() 

#### CREATE RESULTS FOLDER ####
results_path <- paste0(getwd(),"/../results/",RESULT_FOLDER)
models_path <- paste0(results_path,"/models")

dir.create(results_path)
dir.create(models_path)

#### Descriptive statistics ####

if(DEPVAR == "asylum seekers" & SPEI_MOVING_AVERAGES_3_6 == FALSE){
  calculate_moving_averages <- function(df, calc_6){
    df <- group_by(df, country) %>%
      arrange(year) %>%
      mutate(spei3_gs_pos_r3 = zoo::rollmean(spei3_gs_pos, k = 3, fill = NA, align = "right"),
             spei3_gs_neg_r3 = zoo::rollmean(spei3_gs_neg, k = 3, fill = NA, align = "right"),
             spei3_gs_pos_r6 = zoo::rollmean(spei3_gs_pos, k = 6, fill = NA, align = "right"),
             spei3_gs_neg_r6 = zoo::rollmean(spei3_gs_neg, k = 6, fill = NA, align = "right"))
    if(calc_6){
      df <- dplyr::select(df, -spei3_gs_pos, -spei3_gs_neg)
    } else {
      df <- dplyr::select(df, -spei3_gs_pos_r6, -spei3_gs_neg_r6)
    }
  }
  
  
  log_vars <- c("wdi_pop", "area", "wdi_gdppc", "casualties_brd", "annually_affected_20k", "homicide")
  

  to_descriptive_stat <- dataset %>%
    dplyr::select(country, year, n_asylum_eu28, depvar, highest_neighbor_dem, area, wdi_pop, wdi_urban_pop, distance_to_eu,
           wdi_gdppc_growth, wdi_gdppc, perc_post_secondary, kof_index, wdi_imr,
           tmp_pop, spei3_gs_pos, spei3_gs_neg, casualties_brd, annually_affected_20k, physical_integrity, free_movement, homicide) %>%
    recipe(.) %>%
    update_role(!!log_vars, new_role = "log_transform") %>%
    step_log(has_role("log_transform"), offset = 1) %>%
    prep() %>%
    juice()
  
  to_descriptive_stat <- calculate_moving_averages(to_descriptive_stat, calc_6 = SPEI_MOVING_AVERAGES_3_6) %>% ungroup()
  
  to_descriptive_stat <- to_descriptive_stat %>% filter(year >=1999)
  
  descriptives <- to_descriptive_stat %>% skim()
  
  main_table <- descriptives %>% as_tibble() %>% janitor::clean_names() %>% 
    dplyr::select(skim_variable, n_missing,
           numeric_mean, numeric_sd, 
           numeric_p50, numeric_p0, numeric_p100) %>%
    rename(Variable = skim_variable,
           Mean = numeric_mean,
           SD = numeric_sd,
           Median = numeric_p50,
           Min = numeric_p0,
           Max = numeric_p100,
           Missing = n_missing) %>% filter(Variable != "country")
  
  v_baseline <- c("highest_neighbor_dem", "area", "wdi_pop", "wdi_urban_pop", "distance_to_eu")
  v_econ <- c("wdi_gdppc_growth", "wdi_gdppc", "kof_index", "perc_post_secondary", "wdi_imr") 
  v_climate <- c("tmp_pop", "spei3_gs_pos", "spei3_gs_neg", "spei3_gs_pos_r3", "spei3_gs_neg_r3")
  v_violence <- c("casualties_brd", "annually_affected_20k", "physical_integrity", "free_movement", "homicide")
  all_vars <- c("year", "n_asylum_eu28", "depvar",  v_baseline, v_violence, v_econ, v_climate)
  
  main_table <- main_table[match(all_vars, main_table$Variable),]
  
  
  ncountries <- descriptives  %>% as_tibble() %>% filter(skim_variable == "country") %>% pull(factor.n_unique)
  nobs <- attributes(descriptives)$data_rows
  
  command <- paste("<tfoot><tr><td colspan='7'>The dataset has", 
                    nobs,"observations and", 
                    ncountries, "countries.</td></tr></tfoot>", sep = " ")
  
  footer <- list("pos" = list(nrow(main_table)), "command" = command)
  
  # Create in print xtable in html
  print(xtable::xtable(main_table, 
                       caption="Descriptive statistics",
                       digits = 2),
        type = "html",
        file = paste0(results_path,"/","descriptive_statistics.html"),
        include.rownames=FALSE, caption.placement='top',
        html.table.attributes='align="left"',
        add.to.row = footer)
  
  cormat <- round(cor(to_descriptive_stat %>% dplyr::select(all_vars[2:23]), use = "pairwise.complete.obs"),2)
  cormat[upper.tri(cormat)] <- NA
  cormat <- reshape2::melt(cormat, na.rm = TRUE)
    
  g_cor <- ggplot(data = cormat, aes(x=Var1, y=Var2, color=-value, fill = value, label = value)) + 
    geom_tile() + 
    geom_text(size = 1.7) +
    scale_color_viridis_c(guide = FALSE, limits = c(-1, 1)) + 
    scale_fill_viridis_c("Pearson's\nCorrelation", limits = c(-1, 1)) +
    coord_fixed() +
    theme_minimal(base_size = 10) +
    theme(legend.position = c(0.25,.70),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.title = element_blank())
  
  ggsave(paste0(results_path,"/", glue::glue("{DEPVAR_UNDERSCORE}_cormat.pdf")), plot = g_cor, device="pdf", width = 12, height = 12, units = "cm")
  ggsave(paste0(results_path,"/", glue::glue("{DEPVAR_UNDERSCORE}_cormat.svg")), plot = g_cor, device="svg", width = 12, height = 12, units = "cm")
  
  
}


cshapes <- cshapes[cshapes$COWEYEAR >= 1999,]
cshapes$orig_depvar <- 0

for (i in 1:nrow(cshapes)){
  country_gw <- cshapes$GWCODE[i]
  cshapes$orig_depvar[i] <- sum(dataset$orig_depvar[dataset$gwcode == country_gw])
}

cshapes <- sf::st_as_sf(cshapes)

cshapes <- cshapes %>%
  mutate(orig_depvar_cat = case_when(orig_depvar <= 1000 ~ "0-1K",
                                     orig_depvar > 1000 & orig_depvar <= 50000 ~ "1K-50K",
                                     orig_depvar > 50000 & orig_depvar <= 500000 ~ "50K-500K",
                                     orig_depvar > 500000 ~ "500K-"))
cshapes$orig_depvar_cat <- ordered(cshapes$orig_depvar_cat, levels = c("0-1K", "1K-50K", "50K-500K", "500K-"))

g_map <- ggplot(cshapes, aes(fill=orig_depvar_cat)) + geom_sf(color="gray40", lwd = 0.1) + 

  scale_fill_manual(glue::glue("{DEPVAR_TC} \n1999-2018"), values = c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"),
                    guide = guide_legend(direction = "vertical", title.position = "top", label.position = "right")) +

  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_bw(base_size = 9) + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.title = element_text(size = 9),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        legend.key.width = unit(0.8,"line"),
        legend.key.height = unit(0.3,"line"),
        legend.position = c(0.15,0.245),
        legend.box.background = element_rect(colour = "black")) +
  
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
ggsave(paste0("../results/",RESULT_FOLDER, "/map_origin_discrete.pdf"), plot = g_map, device="pdf", width = 20, height = 8.5, units = "cm")
ggsave(paste0("../results/",RESULT_FOLDER, "/map_origin_discrete.svg"), plot = g_map, device="svg", width = 20, height = 8.5, units = "cm")


ggplot(cshapes, aes(fill=orig_depvar_cat)) + geom_sf(color="gray40", lwd = 0.1) 

ggplot(cshapes) + geom_sf(color="gray40", lwd = 0.1) + coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) 


plot_df <- group_by(dataset, country) %>%
  arrange(year) %>%
  mutate(spei3_gs_pos_r3 = zoo::rollmean(spei3_gs_pos, k = 3, fill = NA, align = "right"),
         spei3_gs_neg_r3 = zoo::rollmean(spei3_gs_neg, k = 3, fill = NA, align = "right")) %>%
  ungroup() %>%
  subset(year >= 1999)

plot_df$gwregion <- ifelse(plot_df$gwcode %in% 1:199, "America", NA)
plot_df$gwregion <- ifelse(plot_df$gwcode %in% 200:399, "Europe", plot_df$gwregion)
plot_df$gwregion <- ifelse(plot_df$gwcode %in% 400:599, "Sub-Saharan Africa", plot_df$gwregion)
plot_df$gwregion <- ifelse(plot_df$gwcode %in% 600:699, "MENA", plot_df$gwregion)
plot_df$gwregion <- ifelse(plot_df$gwcode %in% 700:860, "Asia", plot_df$gwregion)
plot_df$gwregion <- ifelse(plot_df$gwcode %in% 900:999, "Oceania", plot_df$gwregion)
plot_df$gwregion <- factor(plot_df$gwregion, levels = c("America", "Europe", "Sub-Saharan Africa", "MENA", "Asia", "Oceania"))

annotations <- group_by(plot_df, year) %>% 
  filter(year %in% c(2002, 2016)) %>% 
  top_n(n=3, wt = orig_depvar) %>% 
  dplyr::select(gwcode, country, year, orig_depvar)

to_plot <- plot_df %>% dplyr::select(gwcode, gwregion, year, orig_depvar) %>% na.omit()

g1 <- ggplot(to_plot, aes(x=year, y=orig_depvar, fill=gwregion, color=gwregion, group=gwcode)) +
  geom_area(size=.05, alpha=.5, position="stack") +
  scale_fill_colorblind("Region") +
  scale_color_colorblind("Region") +
  scale_y_continuous(labels = paste0(c(0,300,600,900), "K"), breaks = 10^5 * c(0,3,6,9)) +
  annotate("text", x = 2015.5, y = 675000, label = "Iraq", size = 2.5) +
  annotate("text", x = 2015.5, y = 150000, label = "Afghanistan", size = 2.5) +
  annotate("text", x = 2015.5, y = 400000, label = "Syria", size = 2.5) +
  theme_bw(base_size = 9) + ylab("Number of asylum seekers") + xlab("Year") +
  theme(legend.title = element_text(size = 9),
        legend.key.size = unit(0.5,"line"),
        axis.text.y = element_text(angle = 90, hjust = 0.5, color = "black"),
        axis.text.x = element_text(color = "black"),
        legend.position = c(0.155,0.729),#"right",#c(0.22,0.72),
        legend.box.background = element_rect(colour = "black")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

ggsave(paste0(results_path,"/", glue::glue("{DEPVAR_UNDERSCORE}_by_country_and_region.pdf")), plot = g1, device="pdf")
ggsave(paste0(results_path,"/", glue::glue("{DEPVAR_UNDERSCORE}_by_country_and_region.svg")), plot = g1, device="svg")


figure1 <- ggarrange(g1,g_map, heights = c(0.38,1),ncol = 1)

ggsave(paste0(results_path,"/", glue::glue("{DEPVAR_UNDERSCORE}_figure1.pdf")), plot = figure1, device="pdf")
ggsave(paste0(results_path,"/", glue::glue("{DEPVAR_UNDERSCORE}_figure1.svg")), plot = figure1, device="svg")


v_baseline <- c("highest_neighbor_dem", "area", "wdi_pop", "wdi_urban_pop", "distance_to_eu")
v_econ <- c("wdi_gdppc_growth", "wdi_gdppc", "kof_index", "perc_post_secondary", "wdi_imr") 
v_climate <- c("tmp_pop", "spei3_gs_pos", "spei3_gs_neg", "spei3_gs_pos_r3", "spei3_gs_neg_r3")
v_violence <- c("casualties_brd", "annually_affected_20k", "physical_integrity", "free_movement", "homicide")
all_vars <- c(v_baseline, v_violence, v_econ, v_climate)

plot_sum <- dplyr::select(plot_df, gwregion, year, gwcode, c("wdi_pop", "area", "wdi_gdppc", "casualties_brd", "annually_affected_20k", "homicide"))
plot_sum <- gather(plot_sum, "variable", "value", -gwregion, -year, -gwcode)
plot_sum$geom <- "area"


plot_mean <- dplyr::group_by(plot_df, gwregion, year) %>% 
  summarize_at(c("highest_neighbor_dem", "kof_index", "wdi_gdppc_growth", "wdi_urban_pop", "perc_post_secondary", "tmp_pop", 
                 "spei3_gs_pos", "spei3_gs_neg", "spei3_gs_pos_r3", "spei3_gs_neg_r3", "physical_integrity", 
                 "free_movement", "distance_to_eu", "wdi_imr"), mean, na.rm = T)
plot_mean <- gather(plot_mean, "variable", "value", -gwregion, -year)
plot_mean$geom <- "line"
plot_mean$gwcode <- NA
plot_mean <- dplyr::select(plot_mean, gwregion, year, gwcode, variable, value, geom)

to_plot <- bind_rows(plot_sum, plot_mean)
to_plot$variable <- factor(to_plot$variable, levels = c(v_baseline, v_violence, v_econ, v_climate))

my_breaks <- function(x) {
  halfbillion <- 500000000
  billion <- 1000000000
  million <- 1000000
  thousand <- 1000
  if(max(x, na.rm = T) > (halfbillion)){
    paste0(x/billion, "B")
  }  else if (max(x, na.rm = T) > million) {
    paste0(x/million, "M")
  } else if (max(x, na.rm = T) > thousand) {
    paste0(x/thousand, "K")
  } else{x}
}

g2 <- ggplot(to_plot) +
  facet_wrap(~variable, ncol = 5, scales="free_y") +
  geom_area(data = dplyr::filter(to_plot, geom == "area"), 
            mapping = aes(x=year, y=value, fill=gwregion, color=gwregion, group=gwcode), 
            size=.05, alpha=.5, position="stack")  +
  geom_line(data = dplyr::filter(to_plot, geom == "line"),
            mapping = aes(x=year, y=value, color=gwregion, group=gwregion), size = 0.2)  +
  scale_fill_colorblind("Region", guide=guide_legend(ncol = 6)) +
  scale_color_colorblind("Region") +
  scale_x_continuous(breaks = c(2000, 2008, 2016), expand = c(0.02,0.02)) +
  scale_y_continuous(breaks = scales::pretty_breaks(3, min.n = 2), labels = my_breaks) +
  ylab("") + xlab("Year") +
  theme_bw(base_size = 10) + 
  theme(strip.text.x = element_text(size=4.6, margin = margin(0.1,0.1,0.1,0.1, unit = "line")),
        axis.text.y = element_text(size = 6, color = "black", angle = 90, vjust = 1, hjust = 0.5, margin = ggplot2::margin(t=0,r=0,b=0,l=0)),
        axis.text.x = element_text(size = 6, color = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5,"line"),
        legend.box.background = element_rect(colour = "black"))
ggsave(paste0(results_path,"/","input_variables_by_country_and_region.pdf"), plot = g2, device="pdf", width = 12, height = 10, units = "cm")
ggsave(paste0(results_path,"/","input_variables_by_country_and_region.svg"), plot = g2, device="svg", width = 12, height = 10, units = "cm")



#### PREPROCESS DATASET ####
to_imputation <- dataset %>%   
  dplyr::select(country, year, depvar, highest_neighbor_dem, area, wdi_pop, wdi_urban_pop, distance_to_eu,
         wdi_gdppc_growth, wdi_gdppc, perc_post_secondary, kof_index, wdi_imr,
         tmp_pop, spei3_gs_pos, spei3_gs_neg, casualties_brd, annually_affected_20k, physical_integrity, free_movement, homicide)

log_vars <- c("wdi_pop", "area", "wdi_gdppc", "casualties_brd", "annually_affected_20k", "homicide")
to_imputation <- to_imputation %>%
  recipe(.) %>%
  update_role(!!log_vars, new_role = "log_transform") %>%
  step_log(has_role("log_transform"), offset = 1) %>%
  prep() %>%
  juice()

# multiple imputation
set.seed(12345)
imputations <- amelia(x = to_imputation %>% as.data.frame(),
                      m = N_IMPUTATIONS,
                      idvars = "depvar",
                      cs = "country",
                      ts = "year",
                      logs = log_vars
)$imputations  

constrain_imputed <- function(df) {
  df_names <- names(df)
  if("spei3_gs_neg" %in% df_names){df$spei3_gs_neg <- if_else(df$spei3_gs_neg < 0, 0, df$spei3_gs_neg)}
  if("spei3_gs_pos" %in% df_names){df$spei3_gs_pos <- if_else(df$spei3_gs_pos < 0, 0, df$spei3_gs_pos)}
  if("kof_index" %in% df_names){df$kof_index <- if_else(df$kof_index < 0, 0, df$kof_index)}
  if("wdi_urban_pop" %in% df_names){df$wdi_urban_pop <- if_else(df$wdi_urban_pop < 0, 0, df$wdi_urban_pop)}
  if("perc_post_secondary" %in% df_names){df$perc_post_secondary <- if_else(df$perc_post_secondary < 0, 0, df$perc_post_secondary)}
  if("wdi_imr" %in% df_names){df$wdi_imr <- if_else(df$wdi_imr < 0, 0, df$wdi_imr)}
  if("distance_to_eu" %in% df_names){df$distance_to_eu <- if_else(df$distance_to_eu < 0, 0, df$distance_to_eu)}
  
  return(df)
}
imputations <- lapply(imputations, constrain_imputed)

lag_all_predictors <- function(df){
  lag_text <- paste0("lag_", NLAG)
  df <- df %>%
    group_by(country) %>%
    arrange(year) %>%
    recipe(depvar ~ .) %>%
    remove_role(country, year, old_role = "predictor") %>% 
    step_lag(all_predictors(), lag = NLAG) %>%
    step_rm(all_predictors(), -starts_with(lag_text)) %>%
    step_naomit(all_predictors()) %>% # remove missing created from lagging
    prep() %>%
    juice() %>%
    recipe(depvar ~ .) %>%
    prep() %>%
    juice()
  names(df) <- sub(paste0(lag_text, "_"), "", names(df))
  return(df)
}

if(LAG_PREDICTORS){
  imputations <- map(imputations, lag_all_predictors)  
}

calculate_moving_averages <- function(df, calc_6){
  df <- group_by(df, country) %>%
    arrange(year) %>%
    mutate(spei3_gs_pos_r3 = zoo::rollmean(spei3_gs_pos, k = 3, fill = NA, align = "right"),
           spei3_gs_neg_r3 = zoo::rollmean(spei3_gs_neg, k = 3, fill = NA, align = "right"),
           spei3_gs_pos_r6 = zoo::rollmean(spei3_gs_pos, k = 6, fill = NA, align = "right"),
           spei3_gs_neg_r6 = zoo::rollmean(spei3_gs_neg, k = 6, fill = NA, align = "right"))
  if(calc_6){
    df <- dplyr::select(df, -spei3_gs_pos, -spei3_gs_neg)
  } else {
    df <- dplyr::select(df, -spei3_gs_pos_r6, -spei3_gs_neg_r6)
  }
}
imputations <- map(imputations, calculate_moving_averages, calc_6 = SPEI_MOVING_AVERAGES_3_6)

filter_years <- function(df){ df %>% filter(year >= 1999) }
imputations <- map(imputations, filter_years)

imputations <- lapply(imputations, na.omit)  # missing on dependent variable



##### Short detour for the OLS-FE models ####
if(SPEI_MOVING_AVERAGES_3_6 == FALSE){
  ols_v_climate <- c(v_climate, "tmp_pop_sq")
  
  #Pooled OLS specification (dunno if we need that at all?):
  f_all <- paste0("depvar ~ ", paste(c(v_baseline, v_econ, ols_v_climate, v_violence), collapse = " + "))
  f_baseline <- paste0("depvar ~ ", paste(c(v_baseline), collapse = " + "))
  f_econ <- paste0("depvar ~ ", paste(c(v_baseline, v_econ), collapse = " + "))
  f_climate <- paste0("depvar ~ ", paste(c(v_baseline, ols_v_climate), collapse = " + "))
  f_violence <- paste0("depvar ~ ", paste(c(v_baseline, v_violence), collapse = " + "))
  
  
  #2-way FE specification:
  f_all_fe <- paste(c(f_all, "factor(country) + factor(year)"), collapse = " + ")
  f_baseline_fe <- paste(c(f_baseline, "factor(country) + factor(year)"), collapse = " + ")
  f_econ_fe <- paste(c(f_econ, "factor(country) + factor(year)"), collapse = " + ")
  f_climate_fe <- paste(c(f_climate, "factor(country) + factor(year)"), collapse = " + ")
  f_violence_fe <- paste(c(f_violence, "factor(country) + factor(year)"), collapse = " + ")
  
  
  to_ols <- imputations$imp1
  to_ols$tmp_pop_sq <- to_ols$tmp_pop ^ 2
  
  m_all_fe <- lm(f_all_fe, data = to_ols)
  m_econ_fe <- lm(f_econ_fe, data = to_ols)
  m_climate_fe <- lm(f_climate_fe, data = to_ols)
  m_violence_fe <- lm(f_violence_fe, data = to_ols)
  
  texreg::screenreg(list( m_climate_fe, m_econ_fe, m_violence_fe, m_all_fe), omit.coef = "factor")
  texreg::htmlreg(list( m_climate_fe, m_econ_fe, m_violence_fe, m_all_fe), omit.coef = "factor", file = paste0(results_path,"/2way_fe_lm.html"))

}


# Split data in training and testing

split_into_training_and_testing <- function(df, training_period){
  train_test <- df %>%
    nest(data = -year) %>%
    rolling_origin(initial = training_period, cumulative = FALSE) 
  
  train = map(train_test$splits, ~ bind_rows(analysis(.x)$data))
  test = map(train_test$splits, ~ bind_rows(assessment(.x)$data))
  
  
  insert_year_column <- function(split, training = TRUE){
    if(training){
      years <- analysis(split)$year
      nobs_per_year <- sapply(analysis(split)$data, nrow)
    } else {
      years <- assessment(split)$year
      nobs_per_year <- sapply(assessment(split)$data, nrow)
      
    }
    years <- tibble("year" = rep(years, nobs_per_year))
    return(years)
  }
  
  train_years <- map(train_test$splits, insert_year_column)
  train <- map2(train, train_years, bind_cols)
  test_years <- map(train_test$splits, insert_year_column, training = FALSE)
  test <- map2(test, test_years, bind_cols)

  return(tibble("train" = train, "test" = test))  
}

imputations <- map(imputations, split_into_training_and_testing, training_period = TRAINING_PERIOD)

if(NSTEP_AHEAD > 1){
  
  lead_test_set <- function(df, nstep){
    df <- mutate(df, test = lead(test, nstep-1)) # -1 because test is already 1 step ahead
    df <- head(df, n = nrow(df) - nstep + 1) # drop the observation without any data (the last nstep-1 observations)
    return(df)
  }
  
  imputations <- map(imputations, lead_test_set, nstep = NSTEP_AHEAD)
}

#### Train models ####
update_recipe <- function(train, calc_6){
  if(calc_6){
    my_recipe <- train %>%
      recipe(depvar ~ .) %>%
      add_role(highest_neighbor_dem, area, wdi_pop, wdi_urban_pop, distance_to_eu, new_role = "baseline") %>%
      add_role(wdi_gdppc_growth, wdi_gdppc, perc_post_secondary, kof_index, wdi_imr, new_role = "economy") %>%
      add_role(tmp_pop, spei3_gs_pos_r3, spei3_gs_neg_r3, spei3_gs_pos_r6, spei3_gs_neg_r6, new_role = "climate") %>%
      add_role(casualties_brd, annually_affected_20k, physical_integrity, free_movement, homicide, new_role = "violence") %>%
      step_rm(country, year) %>%
      step_dummy(all_nominal()) %>% 
      prep()
  } else {
    my_recipe <- train %>%
      recipe(depvar ~ .) %>%
      add_role(highest_neighbor_dem, area, wdi_pop, wdi_urban_pop, distance_to_eu, new_role = "baseline") %>%
      add_role(wdi_gdppc_growth, wdi_gdppc, perc_post_secondary, kof_index, wdi_imr, new_role = "economy") %>%
      add_role(tmp_pop, spei3_gs_pos, spei3_gs_neg, spei3_gs_pos_r3, spei3_gs_neg_r3, new_role = "climate") %>%
      add_role(casualties_brd, annually_affected_20k, physical_integrity, free_movement, homicide, new_role = "violence") %>%
      step_rm(country, year) %>%
      step_dummy(all_nominal()) %>% 
      prep()
  }
  
  climate <- my_recipe %>% step_rm(has_role("economy"), has_role("violence")) %>% prep() 
  economy <- my_recipe %>% step_rm(has_role("climate"), has_role("violence")) %>% prep() 
  violence <- my_recipe %>% step_rm(has_role("climate"), has_role("economy")) %>% prep() 
  
  all_nb <- my_recipe %>% step_rm(has_role("baseline")) %>% prep() 
  climate_nb <- my_recipe %>% step_rm(has_role("baseline"), has_role("economy"), has_role("violence")) %>% prep() 
  economy_nb <- my_recipe %>% step_rm(has_role("baseline"), has_role("climate"), has_role("violence")) %>% prep() 
  violence_nb <- my_recipe %>% step_rm(has_role("baseline"), has_role("climate"), has_role("economy")) %>% prep() 
  
  climate_economy <- my_recipe %>% step_rm(has_role("violence")) %>% prep() 
  economy_violence <- my_recipe %>% step_rm(has_role("climate")) %>% prep() 
  violence_climate <- my_recipe %>% step_rm(has_role("economy")) %>% prep() 
  
  all_recipes <- list("climate" = climate, 
                      "economy" = economy, 
                      "violence" = violence, 
                      "climate_nb" = climate_nb, 
                      "economy_nb" = economy_nb, 
                      "violence_nb" = violence_nb, 
                      "climate_economy" = climate_economy, 
                      "economy_violence" = economy_violence, 
                      "violence_climate" = violence_climate,
                      "all_nb" = all_nb,
                      "all" = my_recipe)
  return(all_recipes)  
}

lr <- linear_reg()
rf <- rand_forest(mode = "regression", trees = N_TREES, min_n = 5)

predictions_from_recipe <- function(recip, recipe_name, num_threads, calculate_importance){
  train_final <- bake(recip, train)
  test_final <- bake(recip, test)
  
  
  train_final_z <- train_final %>%
    recipe(depvar ~ .) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    prep() %>% juice()
  
  test_final_z <- test_final %>%
    recipe(depvar ~ .) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    prep() %>% juice()
  
  ranger_params <- list("object" = rf, "engine" = "ranger", "num.threads" = num_threads, "save.memory" = FALSE, "seed" = 54636)
  if(calculate_importance){ranger_params <- append(ranger_params, list("importance" = "permutation", "scale.permutation.importance" = TRUE))}
  rf_fit <- do.call(set_engine, ranger_params) %>% fit(depvar ~ ., data = train_final)

    
  # Add second-order temperature polynomial to the linear model when tmp_pop is part of the recipe
  if("tmp_pop" %in% names(train_final)){
    train_final <- train_final %>% mutate(tmp_pop_sq = tmp_pop^2)
    test_final <- test_final %>% mutate(tmp_pop_sq = tmp_pop^2)
    train_final_z <- train_final_z %>% mutate(tmp_pop_sq = tmp_pop^2)
    test_final_z <- test_final_z %>% mutate(tmp_pop_sq = tmp_pop^2)
  }
  lr_fit <-  lr %>% set_engine("lm") %>% fit(depvar ~ ., data = train_final)
  lrz_fit <-  lr %>% set_engine("lm") %>% fit(depvar ~ ., data = train_final_z)

  predictions <- dplyr::select(test, country, year, depvar)
  predictions$pred_rf <- rf_fit %>% predict(test_final) %>% pull(.pred)
  predictions$pred_lr <- lr_fit %>% predict(test_final) %>% pull(.pred)
  predictions$pred_lrz <- lrz_fit %>% predict(test_final_z) %>% pull(.pred)
  
  predictions$recipe <- recipe_name
  predictions <- pivot_longer(predictions, cols = starts_with("pred"), names_to = "mclass", values_to = ".pred", names_prefix = "pred_")

  return(tibble("rf_fit" = list(rf_fit), 
              "lr_fit" = list(lr_fit), 
              "lrz_fit" = list(lrz_fit),              
              "predictions" = list(predictions), 
              "recipe" = recipe_name))
}

# https://rpubs.com/jeffjjohnston/rds_compression
# This shaves 40 percent off the time used to estimate the model... A bit more demanding on space
# sudo apt-get install liblz4-tool
lz4_pipe <- function(filename, mode="read") {
  if(mode == "read") {
    con <- pipe(paste0("cat ", filename, " | lz4 -d -c"), "rb")
  } else {
    con <- pipe(paste0("lz4 -z -f | tee >/dev/null ", paste0(filename, ".lz4")), "wb") # tee is used to fix file ownership (pipes file to dev/null and to new file)
  }
  con
}

if(TRAIN_MODELS){
  #model_fits <- tibble()
  for(imp_num in 1:length(imputations)){
    for(cv_num in 1:nrow(imputations[[imp_num]])){
      train <- imputations[[imp_num]]$train[[cv_num]]
      test <- imputations[[imp_num]]$test[[cv_num]]
      all_recipes <- update_recipe(train, calc_6 = SPEI_MOVING_AVERAGES_3_6)
      mfit <- map2_dfr(all_recipes, names(all_recipes), predictions_from_recipe, num_threads = NUM_THREADS, calculate_importance = CALCULATE_IMPORTANCE)
      mfit$imputation <- imp_num
      mfit$cv_num <- cv_num
      #    model_fits <- bind_rows(model_fits, mfit)
      #fname <- here(RESULT_FOLDER, "models", glue::glue("model_fit_IMP{imp_num}_CV{cv_num}.rds"))
      fname <- paste0(models_path,"/",glue::glue("model_fit_IMP{imp_num}_CV{cv_num}.rds")) 
      # here(RESULT_FOLDER, "models", glue::glue("model_fit_IMP{imp_num}_CV{cv_num}.rds"))
      
      con <- lz4_pipe(fname, mode = "write")
      saveRDS(mfit, con)
      close(con)
    }
  }
}




model_files <- list.files(paste0(models_path,"/"),pattern = "*models*", full.names = TRUE)
  #paste0(models_path,"/",glue::glue("model_fit_IMP{imp_num}_CV{cv_num}.rds")) 
  #list.files(here(RESULT_FOLDER, "models"), full.names = TRUE)
predictions <- tibble()
for(fname in model_files){
  con <- lz4_pipe(fname, mode = "read")
  model_fit <- readRDS(con)
  close(con)
  prediction <- bind_rows(model_fit$predictions)
  prediction$imputation <- first(model_fit$imputation)
  prediction$cv_num <- first(model_fit$cv_num)
  predictions <- bind_rows(predictions, prediction)
}

predictions <- dplyr::select(dataset, country, year, orig_depvar, wdi_pop) %>% right_join(predictions, by = c("country", "year"))

exponentiate <- function(variable, offset){exp(variable) - offset}
if(LOG_TRANSFORM_DEPVAR){ 
  predictions$pred_orig_depvar <- exponentiate(predictions$.pred, 1) 
} else {
  predictions$pred_orig_depvar <- predictions$.pred 
  }
if(PER_CAPITA_TRANSFORM_DEPVAR){ predictions$pred_orig_depvar <- predictions$pred_orig_depvar * predictions$wdi_pop/million }

my_metrics <- metric_set(rmse, mae, huber_loss)

performance_metrics <- predictions %>%
  group_by(recipe, mclass) %>%
  my_metrics(truth = depvar, estimate = .pred)
  
orig_depvar_metrics <- predictions %>%
  group_by(recipe, mclass) %>%
  my_metrics(truth = orig_depvar, estimate = pred_orig_depvar)

# other metrics: "rmse", "huber_loss"

model_sets <- list(
  "main" = c("all", "climate", "economy", "violence"), 
  "no_baseline" = c("all_nb", "climate_nb", "economy_nb", "violence_nb"),
  "leave_one_out" = c("all", "climate_economy", "economy_violence", "violence_climate")
)

recipe_palette <- rev(c("#0f2080", "#a95aa1", "#85c0f9", "#f5793a"))

for(i in 1:length(model_sets)){
  
  model_set_name <- names(model_sets)[i]

  orig_depvar_min_mse_by_year <- predictions %>%
    filter(recipe %in% tail(model_sets[[i]], -1)) %>% # remove the "all" element
    group_by(recipe, mclass, country, year) %>%
    mae(truth = orig_depvar, estimate = pred_orig_depvar) %>%
    left_join(dataset, by = c("country", "year")) %>%
    group_by(country, year) %>%
    filter(.estimate == min(.estimate)) %>%
    group_by(recipe, year) %>%
    summarize(orig_depvar = sum(orig_depvar)) %>%
    ungroup() %>%
    mutate(recipe = fct_reorder(recipe, desc(recipe)))
  
  g_main_c <- ggplot(orig_depvar_min_mse_by_year, aes(x=year, y=orig_depvar, fill=recipe, color=recipe, group=recipe)) +
    geom_area(size=.05, position="stack") +
    scale_fill_manual("Model", values=head(recipe_palette, -1)) +
    scale_color_manual("Model", values=head(recipe_palette, -1)) +
    scale_y_continuous(labels = paste0(c(0, 300, 600, 900), "K"), breaks = 10^5 * c(0, 3, 6, 9)) +
    scale_x_continuous(breaks = c(2003, 2006, 2009, 2012, 2015, 2018)) +
    theme_bw(base_size = 6) + 
    ylab(glue::glue("{DEPVAR_TC}")) + xlab("Year") +
    theme(axis.text.y = element_text(color = "black", angle = 90, hjust = 0.5),
          axis.text.x = element_text(color = "black"),
          legend.position = c(0.25, 0.8),
          legend.box.background = element_rect(fill = "black"))
  
  ggsave(paste0(results_path,"/", glue::glue("main_c_{model_set_name}.pdf")), plot = g_main_c, device="pdf")
  ggsave(paste0(results_path,"/", glue::glue("main_c_{model_set_name}.svg")), plot = g_main_c, device="svg")
  

  plot_df <- predictions %>%
    filter(mclass == "rf" & recipe %in% model_sets[[i]]) %>%
    mutate(recipe = fct_reorder(recipe, desc(recipe)))
  
  g_alt_b <- ggplot(plot_df, aes(x=orig_depvar/1000, y = pred_orig_depvar/1000, group = recipe, color = recipe)) + 
    geom_point(alpha = 0.05, shape = ".") +
    geom_smooth(method = "lm", size = 0.4) + 
    geom_abline(intercept = 0, slope = 1, size = 0.4) +
    scale_color_manual("Model", values = recipe_palette) +
    scale_y_continuous(limits = c(0, 300), labels = function(x) paste0(x, "K")) +
    scale_x_continuous(labels = function(x) paste0(x, "K")) +
    annotate("text", x = 100, y = 120, label = "Perfect prediction", angle = 45, size = 2) + 
    theme_bw(base_size = 6) + ylab( glue::glue("Predicted {DEPVAR_TC}")) + xlab( glue::glue("{DEPVAR_TC}")) +
    theme(axis.text.y = element_text(color = "black", angle = 90, hjust = 0.5),
          axis.text.x = element_text(color = "black"),
          legend.position = c(0.15, 0.80),
          legend.box.background = element_rect(fill = "black")) + 
    coord_equal()
  ggsave(paste0(results_path,"/", glue::glue("predicted_on_actual_{model_set_name}.pdf")), plot = g_alt_b, device="pdf")
  ggsave(paste0(results_path,"/", glue::glue("predicted_on_actual_{model_set_name}.svg")), plot = g_alt_b, device="svg")
  

  to_plot <- filter(performance_metrics, mclass == "rf", .metric == "mae", recipe %in% model_sets[[i]])
  
  to_plot <- to_plot %>% mutate(recipe_short = case_when(recipe == "climate_economy" ~ "c-e",
                                                         recipe == "economy_violence" ~ "e-v",
                                                         recipe == "violence_climate" ~ "v-c",
                                                         recipe == "climate_nb" ~ "c-nb",
                                                         recipe == "economy_nb" ~ "e-nb",
                                                         recipe == "violence_nb" ~ "v-nb",
                                                         TRUE ~ recipe))
  
  ordered_levels <- to_plot %>% arrange(-.estimate) %>% pull(recipe_short)
  to_plot$recipe_short <- factor(to_plot$recipe_short, levels = ordered_levels)
  
  to_nudge <- (max(to_plot$.estimate) - min(to_plot$.estimate)) / 40
  if(max(to_plot$.estimate) > 1000){
    to_nudge <- (max(to_plot$.estimate) - min(to_plot$.estimate)) / 30
  }
  
  g_main_a <- ggplot(to_plot, aes(x = recipe_short, y = .estimate, label = round(.estimate, 2))) +
    geom_point(size = 0.8) +
    geom_text(nudge_y = to_nudge, size = 1.4) +
    coord_flip() +
    ylab("Mean prediction error (#)") +
    xlab("Model") + 
    theme_bw(base_size = 6) +
    theme(axis.text.y = element_text(color = "black"),
          axis.text.x = element_text(color = "black"))
  ggsave(paste0(results_path,"/", glue::glue("main_a_{model_set_name}.pdf")), plot = g_main_a, device="pdf")
  ggsave(paste0(results_path,"/", glue::glue("main_a_{model_set_name}.svg")), plot = g_main_a, device="svg")
  

    
  if(LOG_TRANSFORM_DEPVAR){    
    xlab_label <- glue::glue("{DEPVAR_TC} (log scale)")
    ylab_label <- glue::glue("Predicted {DEPVAR_TC} (log scale)")
    
    if(PER_CAPITA_TRANSFORM_DEPVAR){
      xlab_label <- glue::glue("{DEPVAR_TC} per million (log scale)")
      ylab_label <- glue::glue("Predicted {DEPVAR} per million (log scale)")
    }
    
    logp1 <- function(x) {log(x+1)}
    expp1 <- function(x) {exp(x)-1}
    
    
    annotate_x <- 500
    annotate_y <- 750
    if(max(expp1(plot_df$.pred)) < 1500){
      annotate_x <- 100
      annotate_y <- 150
    }
    
    g_main_b <- ggplot(plot_df, aes(x=expp1(depvar), y = expp1(.pred), group = recipe, color = recipe)) + 
      geom_point(alpha = 0.05, shape = ".") +
      geom_smooth(method = "lm", size = 0.4) + 
      geom_abline(intercept = 0, slope = 1, size = 0.4) +
      scale_color_manual("Model", values = recipe_palette) +
      #scale_y_continuous(limits = c(0, 11), trans = "log10") +
      scale_y_continuous(breaks = c(0, 10, 100, 1000, 10000), trans =  scales::trans_new("custom", logp1, expp1), labels = scales::comma) +
      scale_x_continuous(breaks = c(0, 10, 100, 1000, 10000), trans =  scales::trans_new("custom", logp1, expp1), labels = scales::comma) +
      annotate("text", x = annotate_x, y = annotate_y, label = "Perfect prediction", angle = 45, size = 2) + 
      theme_bw(base_size = 6) + ylab(ylab_label) + xlab(xlab_label) +
      theme(axis.text.y = element_text(color = "black", angle = 90, hjust = 0.5),
            axis.text.x = element_text(color = "black"),
            legend.position = c(0.8, 0.2),
            legend.box.background = element_rect(fill = "black")) + 
      coord_equal()
  }
  
  if(!LOG_TRANSFORM_DEPVAR){
    g_main_b <- g_alt_b
  }
  
  ggsave(paste0(results_path,"/", glue::glue("main_b_{model_set_name}.pdf")), plot = g_main_b, device="pdf")
  ggsave(paste0(results_path,"/", glue::glue("main_b_{model_set_name}.svg")), plot = g_main_b, device="svg")
  

  
  
  # Patchwork
  main_plot <-  g_main_a / (g_main_b + g_main_c) +
    plot_layout(heights = c(1, 5)) &
    theme(legend.key.size = unit(0.3,"line"))
  
  ggsave(paste0(results_path,"/", glue::glue("main_plot_{model_set_name}.pdf")), plot = main_plot, device="pdf", width = 12, height = 7, units = "cm")
  ggsave(paste0(results_path,"/", glue::glue("main_plot_{model_set_name}.svg")), plot = main_plot, device="svg", width = 12, height = 7, units = "cm")
  

  # predicted on actual in specific countries
  plot_df <- predictions %>%
    filter(mclass == "rf", recipe %in% model_sets[[i]], 
           country %in% c("syria", "yemen", "afghanistan", "iraq", "guatemala", "el salvador", "venezuela", "somalia")) %>%
    group_by(recipe, country, year) %>% # average over all imputations
    summarize(pred_orig_depvar = mean(pred_orig_depvar, na.rm = T),
              orig_depvar = mean(orig_depvar, na.rm = T))
  
  plot_df <- plot_df %>% mutate(recipe_short = case_when(recipe == "climate_economy" ~ "c-e",
                                                         recipe == "economy_violence" ~ "e-v",
                                                         recipe == "violence_climate" ~ "v-c",
                                                         recipe == "climate_nb" ~ "c-nb",
                                                         recipe == "economy_nb" ~ "e-nb",
                                                         recipe == "violence_nb" ~ "v-nb",
                                                         TRUE ~ recipe))
  plot_df$recipe_short <- factor(plot_df$recipe_short)
  
  plot_df <- plot_df %>% ungroup() %>% mutate(recipe_short = fct_reorder(recipe_short, desc(recipe_short)))
  
  g1 <- ggplot(plot_df, aes(x = year, y = pred_orig_depvar/1000, group = recipe_short, color = recipe_short)) +
    geom_line(size=0.7) +
    geom_line(aes(x = year, y = orig_depvar/1000), color = "black", size = 0.7) + 
    facet_wrap(~country, ncol = 2, scales = "free_y") +
    scale_color_manual("Model", values = recipe_palette) +
    scale_y_continuous(labels = function(x) paste0(x, "K")) +
    scale_x_continuous(breaks = c(2008, 2012, 2016)) +
    ylab( glue::glue("Predicted {DEPVAR_TC} (black line is actual)")) + xlab("Year") +
    theme_bw(base_size = 10) + 
    theme(axis.text.y = element_text(color = "black", angle = 90, hjust = 0.5),
          axis.text.x = element_text(color = "black"),
          legend.position = "bottom")

  ggsave(paste0(results_path,"/", glue::glue("predicted_over_time_selected_countries_{model_set_name}.pdf")), plot = g1, device="pdf", width = 12, height = 16, units = "cm")
  ggsave(paste0(results_path,"/", glue::glue("predicted_over_time_selected_countries_{model_set_name}.svg")), plot = g1, device="svg", width = 12, height = 16, units = "cm")
  

}

# ale plots
get_ale_from_model <- function(feature, iml_predictor){
  eff <- FeatureEffect$new(iml_predictor, feature = feature)
  eff <- eff$results
  eff$feature <- feature
  names(eff) <- c(".type", ".ale", "value", "feature")
  return(eff)
}

ranger_predict_fun <- function(model, newdata){
  return(predict(model, newdata)$predictions)
}


model_files <- tibble("full_path" = list.files(paste0(models_path,"/"),pattern = "*models*", full.names = TRUE))
model_files$fname = sapply(str_split(model_files$full_path, "/"), tail, n = 1)
model_files$imputation = sapply(str_split(model_files$fname, "_"), function(x) x[3])
model_files$imputation = sub("IMP", "", model_files$imputation)
model_files$cv_num = sapply(str_split(model_files$fname, "_"), function(x) x[4])
model_files$cv_num = sapply(str_split(model_files$cv_num, "\\."), head, n = 1)
model_files$cv_num = sub("CV", "", model_files$cv_num)

ales <- tibble()
for(imp_num in 1:length(imputations)){
  for(cv in 1:nrow(imputations[[imp_num]])){
    train <- imputations[[imp_num]]$train[[cv]]
    #test <- imputations[[imp_num]]$test[[cv]] # ales can also be calculated for test data
    
    fname <- filter(model_files, imputation == imp_num, cv_num == cv) %>% pull(full_path)
    con <- lz4_pipe(fname, mode = "read")
    model_fit <- readRDS(con)
    close(con)
    
    model_fit <- filter(model_fit, recipe == "all") %>% pull(rf_fit) %>% .[[1]] # Only one observation, but wrapped in a list.
    features <- model_fit$fit$forest$independent.variable.names 
    mod <- Predictor$new(model_fit$fit, data = train, predict.fun = ranger_predict_fun)
    ale <- map_df(features, get_ale_from_model, iml_predictor = mod)
    ale$imputation <- imp_num
    ale$cv_num <- cv
    ales <- bind_rows(ales, ale)
  }
}

ales$groups <- paste(ales$imputation, ales$cv_num, sep = "-")

variable_order <- all_vars
if(SPEI_MOVING_AVERAGES_3_6){
  variable_order[variable_order == "spei3_gs_pos_r3"] <- "spei3_gs_pos_r6"
  variable_order[variable_order == "spei3_gs_neg_r3"] <- "spei3_gs_neg_r6"
  variable_order[variable_order == "spei3_gs_pos"] <- "spei3_gs_pos_r3"
  variable_order[variable_order == "spei3_gs_neg"] <- "spei3_gs_neg_r3"
}

ales$feature <- factor(ales$feature, levels = variable_order)
g_ale <- ggplot(ales, aes(x=value, y =.ale, group = groups)) + 
  facet_wrap(~feature, scales = "free_x") + 
  geom_line(alpha = 0.1) +
  ylab("Accumulated Local Effects (ALE)") + xlab("Observed variable values") +
  scale_x_continuous(breaks = scales::pretty_breaks(3, min.n = 2)) +
  theme_bw(base_size = 10) + 
  theme(strip.text.x = element_text(size=5.4),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.text.x = element_text(size = 6, color = "black"))

ggsave(paste0(results_path,"/","ales.pdf"), plot = g_ale, device="pdf", width = 12, height = 12, units = "cm")
ggsave(paste0(results_path,"/","ales.svg"), plot = g_ale, device="svg", width = 12, height = 12, units = "cm")



# linear model effect plot
linear_effects <- tibble()
for(imp_num in 1:length(imputations)){
  for(cv in 1:nrow(imputations[[imp_num]])){
    fname <- filter(model_files, imputation == imp_num, cv_num == cv) %>% pull(full_path)
    con <- lz4_pipe(fname, mode = "read")
    model_fit <- readRDS(con)
    close(con)
    
    model_fit <- filter(model_fit, recipe == "all") %>% pull(lrz_fit) %>% .[[1]] # Only one observation, but wrapped in a list.
    coefs <- coef(summary(model_fit$fit)) %>% reshape2::melt(na.rm = TRUE) %>% 
      dplyr::filter(Var2 %in% c("Estimate", "Std. Error")) %>%
      tidyr::pivot_wider(names_from = Var2, values_from = value)
    names(coefs) <- c("variable", "eff", "se")
    
    coefs$imp_num <- imp_num
    coefs$cv_num <- cv
    linear_effects <- bind_rows(linear_effects, coefs)
  }
}

#https://stackoverflow.com/questions/23480529/r-function-to-generate-a-mixture-distribution
norm_mixture = function(n, means, sds){
  mymat <- matrix(nrow = n, ncol = length(means))
  for(i in 1:length(means)){
    mymat[,i] <- rnorm(n, means[i], sds[i])
  }
  id = sample(1:length(means),n,rep = T,prob = rep(1/length(means), length(means)))  
  id = cbind(1:n,id)
  mymat[id]
}


#https://stackoverflow.com/questions/49689927/unnest-a-list-column-directly-into-several-columns
to_plot <- linear_effects %>%
  filter(variable != "(Intercept)") %>%
  group_by(variable) %>%
  summarize(quantiles = list(quantile(norm_mixture(1e6, eff, se), c(0.025, 0.5, 0.975)))) %>%
  unnest_wider(quantiles)

names(to_plot) <- c("variable", "q025", "median", "q975")

to_plot <- to_plot %>%
  mutate(variable = fct_reorder(variable, median))

g_leff <- ggplot(to_plot, aes(x = variable, y = median, ymin = q025, ymax = q975)) + 
  geom_linerange() + geom_point() + 
  ylab("Average effect (simulated 95% confidence interval)") +
  coord_flip() +
  theme_bw(base_size = 10) +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_blank())

ggsave(paste0(results_path,"/", "average_linear_effect_allmodel.pdf"), plot = g_leff, device="pdf", width = 12, height = 7, units = "cm")
ggsave(paste0(results_path,"/", "average_linear_effect_allmodel.svg"), plot = g_leff, device="svg", width = 12, height = 7, units = "cm")



# variable_importance
if(CALCULATE_IMPORTANCE){
  all_importance <- tibble()
  for(i in 1:nrow(model_files)){
    con <- lz4_pipe(model_files$full_path[i], mode = "read")
    model_fit <- readRDS(con)
    close(con)
    
    model_fit <- filter(model_fit, recipe == "all") %>% pull(rf_fit) %>% .[[1]] # Only one observation, but wrapped in a list.
    
    imp <- importance(model_fit$fit)
    imp <- tibble("variable" = names(imp), 
                  "importance" = as.numeric(imp))
    all_importance <- bind_rows(all_importance, imp)
  }
  
  all_importance$variable <- reorder(all_importance$variable, all_importance$importance)
  g_imp <- ggplot(all_importance, aes(x = variable, y = importance)) + 
    geom_boxplot() +
    coord_flip() +
    ylab("Variable importance (% Increased MSE)") + 
    theme_bw(base_size = 10) + 
    theme(axis.text.y = element_text(color = "black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(color = "black"))

  ggsave(paste0(results_path,"/","variable_importance.pdf"), plot = g_imp, device="pdf", width = 12, height = 12, units = "cm")
  ggsave(paste0(results_path,"/","variable_importance.svg"), plot = g_imp, device="svg", width = 12, height = 12, units = "cm")
  

  
  all_importance <- tibble()
  for(i in 1:nrow(model_files)){
    con <- lz4_pipe(model_files$full_path[i], mode = "read")
    model_fit <- readRDS(con)
    close(con)
    
    model_fit <- filter(model_fit, recipe == "all_nb") %>% pull(rf_fit) %>% .[[1]] # Only one observation, but wrapped in a list.
    
    imp <- importance(model_fit$fit)
    imp <- tibble("variable" = names(imp), 
                  "importance" = as.numeric(imp))
    all_importance <- bind_rows(all_importance, imp)
  }
  
  all_importance$variable <- reorder(all_importance$variable, all_importance$importance)
  g_imp <- ggplot(all_importance, aes(x = variable, y = importance)) + 
    geom_boxplot() +
    coord_flip() +
    ylab("Variable importance (% Increased MSE)") + 
    theme_bw(base_size = 10) + 
    theme(axis.text.y = element_text(color = "black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(color = "black"))

  ggsave(paste0(results_path,"/", "variable_importance_no_baseline.pdf"), plot = g_imp, device="pdf", width = 12, height = 12, units = "cm")
  ggsave(paste0(results_path,"/", "variable_importance_no_baseline.svg"), plot = g_imp, device="svg", width = 12, height = 12, units = "cm")
  
  
  # Additional exploration with varying future data
  all_importance <- tibble()
  for(i in 1:nrow(model_files)){
    con <- lz4_pipe(model_files$full_path[i], mode = "read")
    model_fit <- readRDS(con)
    close(con)
    
    pred_year <- filter(model_fit, recipe == "all") %>% pull(predictions) %>% .[[1]] %>% pull(year) %>% unique() %>% as.character() %>% as.numeric()
    model_fit <- filter(model_fit, recipe == "all") %>% pull(rf_fit) %>% .[[1]] # Only one observation, but wrapped in a list.
    
    future_data <- bind_rows(imputations[[1]]$test) %>% filter(year > pred_year+5)
    
    predict_parsnip <- function(object, newdata){
      predict.model_fit(object, new_data = newdata, type = "numeric")
    }
    mod <- Predictor$new(model_fit, data = future_data, y = "depvar", predict.fun = predict_parsnip)
    p1 <- FeatureImp$new(mod, loss = "mse")
    p1 <- p1$results
    p1$feature <- feature
    names(p1) <- c("feature_value", ".value", ".type", ".id", "feature")
    
    imp <- importance(model_fit$fit)
    imp <- tibble("variable" = names(imp), 
                  "importance" = as.numeric(imp))
    all_importance <- bind_rows(all_importance, imp)
  }
}

# variable interactions
if(CALCULATE_INTERACTIONS){
  interactions <- tibble()
  for(imp_num in 1:length(imputations)){
    for(cv in 1:nrow(imputations[[imp_num]])){
      train <- imputations[[imp_num]]$train[[cv]]

      fname <- filter(model_files, imputation == imp_num, cv_num == cv) %>% pull(full_path)
      con <- lz4_pipe(fname, mode = "read")
      model_fit <- readRDS(con)
      close(con)
      
      model_fit <- filter(model_fit, recipe == "all") %>% pull(rf_fit) %>% .[[1]] # Only one observation, but wrapped in a list.
      
      features <- model_fit$fit$forest$independent.variable.names
      mod <- Predictor$new(model_fit$fit, data = train, predict.fun = ranger_predict_fun)
      intr <- Interaction$new(mod)
      intr <- intr$results
      intr$imputation <- imp_num
      intr$cv_num <- cv
      interactions <- bind_rows(interactions, intr)
    }
  }
  interactions$.feature <- reorder(interactions$.feature, interactions$.interaction)
  interactions <- interactions %>% filter(!(.feature %in% c("year", "depvar", "country"))) # calculates for all variables in train, even if they are not part of model.
  g_interaction <- ggplot(interactions, aes(x=.feature, y =.interaction)) + 
    geom_boxplot() +
    coord_flip() +
    ylab("Variable interaction strength\n(Friedman's H-statistic)") + 
    xlab("Variable") + 
    theme_bw(base_size = 10) + 
    theme(axis.text.y = element_text(color = "black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(color = "black"))

  ggsave(paste0(results_path,"/", "variable_interaction_strength.pdf"), plot = g_interaction, device="pdf", width = 12, height = 12, units = "cm")
  ggsave(paste0(results_path,"/", "variable_interaction_strength.svg"), plot = g_interaction, device="svg", width = 12, height = 12, units = "cm")
  
}