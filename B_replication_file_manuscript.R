
#############################################################################################
###                                  REPLICATION FILES FOR                                ###
###                                                                                       ###
###  A Costly Commitment: Populism, Economic Performance, and the Quality of Bureaucracy  ###
###                                                                                       ###
###                       Luca Bellodi, Massimo Morelli, Matia Vannoni                    ###
###                                                                                       ###
###                     For questions: luca.bellodi@unibocconi.it                         ###
###                                                                                       ###
#############################################################################################


# Install Packages if not installed or if installed with different version
library(tidyverse)
install.packages("rdpower")
install.packages("rddensity")
install.packages("webshot")
webshot::install_phantomjs()
library(rdpower)
library(rddensity)
library(webshot)

# Data.frame with packages and version
pk = data.frame(
  package = c("devtools", "tidyverse", "rdrobust", "rdpower", "kableExtra", 
              "scales", "rddensity", "plm", "rvest", "lubridate", "readxl"),
  version = c("2.4.3", "1.3.0", "2.0.2", "2.2", "1.3.4", 
              "1.2.0", "2.2", "2.6.1", "1.0.2", "1.8.0", "1.4.0")
  )

# Function to install the appropriate version of package
# This is important for the RD packages, for we noticed that different versions produce minimally different results
pk_fun = function(package, version){
  if(!package %in% installed.packages() | packageVersion(package) != version){
devtools::install_version(package, version = version)}
}

# Apply to each package
for(i in 1:nrow(pk)) pk_fun(package = pk$package[i], version = pk$version[i])

# Load packages
lapply(pk$package, library, character.only = TRUE)

# To save tables it's necessary to install phantomjs 
#webshot::install_phantomjs()

# Load Analysis Dataset, rows are municipality-calendar year pairs
rdd_data = readRDS("dataset.Rds")

# Sequence of Tables and Figures as they appear in manuscript

#####################################
## Table 1: Descriptive Statistics ##
#####################################

# Group data at the municipality-election year level

# Full sample
dataset_ey <- rdd_data %>% group_by(ISTAT_CODICE_COMUNE, year_election) %>% filter(row_number()==1) %>% ungroup()
# Sampe where a populist ran
data_ey <- rdd_data %>% filter(!is.na(margin)) %>% group_by(ISTAT_CODICE_COMUNE, year_election) %>% filter(row_number()==1) %>% ungroup()


# Function to create table
ds_fun <- function(x, y){
  bind_rows(
    x %>%
      summarise_at(
        c("debt_accumulation", "debt_repayment", "cost_overrun_dummy",
          "bur_turnover", "bur_degree"),  mean, na.rm = TRUE) %>%
      pivot_longer(cols = debt_accumulation:bur_degree, names_to = "Variable", values_to = "mean"),
    y %>%
      summarise_at(
        c("age_mayor", "degree_mayor", "mayor_secondary_education", "female_mayor", "winning_candidate_was_incumbent",
          "white_collar_dummy", "population_istat", "ISTAT_surface_sq_km", "north_region"),  mean, na.rm = TRUE) %>%
      pivot_longer(cols = age_mayor:north_region, names_to = "Variable", values_to = "mean"),
    # Observations
    data.frame(Variable = c("Observations", "Unique Municipalities"),
               mean = c(round(nrow(x), 0), length(unique(x$ISTAT_CODICE_COMUNE))))
  )
}


# Apply function to get four columns
# Full Dataset
all <- ds_fun(x = rdd_data, y = dataset_ey) %>% rename("Full Dataset" = mean)
# Analysis Dataset (where a populis ran and margin of victory is not NA) 
analysis <- ds_fun(x = rdd_data %>% filter(!is.na(margin)), y = data_ey) %>% rename("Analysis Dataset" = mean)
# Municipalities with populist mayors
tr_data <- ds_fun(x = rdd_data %>% filter(margin > 0), y = data_ey %>% filter(margin > 0)) %>% rename("Populist Mayor" = mean)
# Municipalities without populist mayors
con_data <- ds_fun(x = rdd_data  %>% filter(margin < 0), y = data_ey %>% filter(margin < 0)) %>% rename("Non-Populist Mayor" = mean)

variables_names = c("Debt Accumulation",  "Debt Repayment", "Cost Overrun (%)", "Managers' Turnover", "Postgraduate Managers (%)",
                    "Age", "Graduate (%)", "Secondary Education (%)", "Female (%)", "Incumbents (%)", "White Collar (%)",
                    "Resident Population", "Surface (sq.km)", "In Northern Regions (%)", "Observations", "Unique Municipalities")

# Left join columns
dstab <- list(all, analysis, con_data, tr_data) %>%
  reduce(left_join, by = "Variable") %>%
  mutate(Variable = variables_names)

# Adjust digits
dstab <- dstab %>% 
  mutate_at(c("Full Dataset", "Analysis Dataset", "Non-Populist Mayor", "Populist Mayor"),
            function(x) if_else(x > 1000,
                                x %>% round(0) %>% format(big.mark = ",", nsmall = 0),
                                x %>% round(2) %>% format(nsmall = 2)))
# Final Table
dstab %>%
  kableExtra::kable(format.args = list(big.mark = ","),
                    digits = 2,
                    align = c("l", rep("r", 4)),
                    caption = "Descriptive Statistics of Main Variables in the Full Dataset and in the Municipalities Where a Populist Ran") %>%
  kableExtra::kable_styling(full_width = F, position = "center") %>%
  kableExtra::row_spec(0, bold = T) %>%
  kableExtra::pack_rows("Economic Performance", 1, 3) %>%
  kableExtra::pack_rows("Quality of Bureaucrats", 4, 5) %>%
  kableExtra::pack_rows("Mayors", 6, 11) %>%
  kableExtra::pack_rows("Municipalities", 12, 16) %>%
  kableExtra::kable_styling(font_size = 10) %>%
  kableExtra::save_kable(file = "Table_1.html", self_contained = T)

# PS. When saving html tables, kableExtra might create a lib folder by default

########################
## Figure 1: RD Plots ##
########################

# Set default ggplot theme (settings suitable for conversion into MS Word)
theme_ggplot = theme(strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"), color = "black"),
                     strip.text.y = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"), color = "black"),
                     panel.border = element_rect(fill=NA,size=0.3),
                     strip.background.x = element_blank(),
                     strip.background.y = element_blank(),
                     panel.grid = element_blank(),
                     panel.spacing = unit(0.1, "lines"),
                     strip.text = element_text(size = 8, color = "black"),
                     axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 7, color = "black"),
                     axis.title = element_text(size = 8, color = "black")) 

fig1_data <- rdd_data %>% filter(abs(margin) < 8) %>%
  mutate(mayor_populist = margin > 0) %>%
  gather(key = dv, value = variable, 
         c("bur_turnover", "bur_degree", "debt_repayment", "debt_accumulation", "cost_overrun_dummy")) %>%
  filter(!is.na(variable)) %>%
  mutate(dv = case_when(str_detect(dv, "turnover") ~ "Turnover\nManagers",
                        str_detect(dv, "repayment") ~ "Debt\nRepayment",
                        str_detect(dv, "overrun") ~ "Percent Cost\nOverruns",
                        str_detect(dv, "accumulation") ~ "Debt\nAccumulation",
                        str_detect(dv, "degree") ~ "Percent Postgraduate\nManagers"))

fig1_data$dv = factor(fig1_data$dv, 
                      levels = c("Debt\nAccumulation", "Debt\nRepayment", "Percent Cost\nOverruns", "Turnover\nManagers", "Percent Postgraduate\nManagers"))

fig1 = ggplot(fig1_data, aes(x = margin, y = variable, shape = mayor_populist, 
                             color = as.factor(mayor_populist), linetype = mayor_populist)) +
  stat_summary_bin(fun = "mean", binwidth = 0.5, geom = "point", size = 0.4) +
  theme_bw() +
  scale_color_manual(values = c("black", "grey60")) +
  geom_vline(xintercept = 0, size = 0.1, linetype = 2) +
  guides(shape = "none", linetype = "none", color = "none") +
  geom_smooth(size = 0.5, method = "lm", formula = y ~ poly(x,2), se = F) +
  labs(x = "Margin of Victory", y = NULL) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  facet_wrap(~dv, ncol = 5, scales = "free_y") +
  theme_ggplot

# Save
ggsave("figure_1.png", fig1, width=7.2, height=2.2)

#########################
## Table 2: RD Results ##
#########################

# Specify outcome variables as list
dv <- with(rdd_data, list(
  # Performance
  debt_accumulation,
  debt_repayment,
  cost_overrun_dummy,
  # Bureaucrats
  bur_turnover,
  bur_degree
))

# Names of outcome variables
dv_names_main <- c("Debt Accumulation", "Debt Repayment", "Percent Cost Overruns", 
                   "Turnover Managers", "Percent Postgraduate Managers")

# Type of outcomes
type <- c(rep("Government\nPerformance", 3), rep("Quality of\nBureaucrats", 2))

# Data.frame with results
main_results <- data.frame()

for(i in 1:length(dv)){
  
  # RDD without covariates
  rd <- with(rdd_data, rdrobust(y = dv[[i]],
                                x = margin,
                                all = T,
                                cluster = ISTAT_CODICE_COMUNE))
  
  # RDD with covariates
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE))
  
  # results without covariates
  results_no_cov <- data.frame(dv = dv_names_main[i],
                               estimate = rd$coef[1],
                               se = rd$se[3],
                               p.value = rd$pv[3],
                               h = rd$bws[1],
                               conf.low = rd$ci[3],
                               conf.high = rd$ci[6],
                               `Obs. used` = sum(rd$N_h),
                               cov = "No Covariates",
                               type = type[i])
  
  # results with covariates
  results_cov <- data.frame(dv = dv_names_main[i],
                            estimate = rd_covs$coef[1],
                            se = rd_covs$se[3],
                            p.value = rd_covs$pv[3],
                            h = rd_covs$bws[1],
                            conf.low = rd_covs$ci[3],
                            conf.high = rd_covs$ci[6],
                            `Obs. used` = sum(rd_covs$N_h),
                            cov = "With Covariates",
                            type = type[i])
  
  # merge
  main_results <- rbind(main_results,
                        results_no_cov,
                        results_cov)

  
}


# Function to convert data.frame to regression table
turn_reg_table <- function(results, covariates){
  
  cova = if_else(covariates == T, "With Covariates", "No Covariates")
  
  results %>% filter(cov == cova) %>%
    select(-type, -conf.low, -conf.high) %>%
    mutate_at(c("estimate", "se", "h"), 
              function(x) x %>% round(3) %>% format(nsmall = 3) %>%
                as.character %>% str_squish %>% str_trim) %>%
    mutate(Obs..used = format(round(Obs..used, 0), big.mark = ",")) %>%
    mutate(dv = dv %>% str_replace(" (?=(Man)|(Acc)|(Rep)|(Over))", "\n")) %>%
    mutate(estimate = case_when(p.value < 0.001 ~ paste0(estimate, "**"),
                                p.value < 0.05 ~ paste0(estimate, "*"),
                                p.value < 0.1 ~ paste0(estimate, "†"),
                                TRUE ~ estimate)) %>%
    select(dv, estimate, se, h, Obs..used) %>%
    rename("Outcomes" = dv,
           "Estimate" = estimate,
           "Robust SE" = se,
           "Obs. Used" = Obs..used) %>%
    t()
}

# create input for regression table
table_2 <- turn_reg_table(results = main_results, covariates = T) 

kableExtra::kable(table_2,
                  format.args = list(big.mark = ","), 
                  digits = 3,
                  align = c("c", "c", "c", "c", "c"),
                  linesep = "") %>%
  kableExtra::add_header_above(c(" " = 1, "Economic Performance" = 3, "Quality of Bureaucrats" = 2), bold = T) %>%
  kableExtra::kable_styling(full_width = F, position = "center",
                            bootstrap_options = "condensed") %>%
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "Table_2.html", self_contained = T)


###############################################################
## Table 3: Types of departure categories of top bureaucrats ##
###############################################################

table_3 = tribble(
  ~`Departure Category`, ~Forced, ~Voluntary, ~Total,
  "Other Reasons", "?", "?", "2,341",
  "Resignation", "", "X","1,800",
  "Transfer to Other Administrations", "", "X", "863",
  "Forced Retirement for Age Limits", "X", "", "763",
  "Contract Termination", "X", "", "182",
  "Contract Termination, 40 years of contributions", "", "", "71",
  "Firing", "X", "", "60",
  "Transfer due to Outsourcing", "", "", "25")

table_3 %>% kableExtra::kable(align = c("l", "c", "c", "r")) %>% 
  kableExtra::kable_paper(full_width = F) %>%
  kableExtra::kable_classic() %>%
  kableExtra::kable_styling(font_size = 11) %>%
  kableExtra::save_kable(file = "Table_3.html", self_contained = T)


#####################################################
## Figure 2: Forced or voluntary departure results ##
#####################################################

# List of outcomes
dv <- with(rdd_data, list(voluntary_departures,
                          forced_departures,
                          other_reasons,
                          turn_fired))

type_dep = "As Percentage of the Total Number of Managers"

dv_names <- c("Voluntary Departures",
              "Forced Departures",
              "Other Reasons",
              "Total Departures")

# save results in data.frame
results_departure <- data.frame()

for(i in 1:length(dv)){
  
  rd <- rdrobust(y = dv[[i]],
                 x = rdd_data$margin,
                 all = T,
                 cluster = rdd_data$ISTAT_CODICE_COMUNE)
  
  rd_covs <- with(rdd_data, rdrobust(y = dv[[i]],
                                     x = margin,
                                     covs = cbind(log(population_istat),
                                                  councillors,
                                                  degree_mayor,
                                                  female_mayor,
                                                  factor(year),
                                                  factor(ISTAT_CODICE_COMUNE),
                                                  factor(year_election),
                                                  ISTAT_surface_sq_km,
                                                  idro_risk_surface,
                                                  white_collar_dummy,
                                                  mayor_secondary_education),
                                     all = T,
                                     cluster = ISTAT_CODICE_COMUNE))
  
  results_no_cov <- data.frame(dv = dv_names[i],
                               type = type_dep,
                               estimate = rd$coef[1],
                               se = rd$se[3],
                               p.value = rd$pv[3],
                               h = rd$bws[1],
                               conf.low = rd$ci[3],
                               conf.high = rd$ci[6],
                               `Obs. used` = sum(rd$N_h),
                               cov = "No Covariates")
  
  results_cov <- data.frame(dv = dv_names[i],
                            type = type_dep,
                            estimate = rd_covs$coef[1],
                            se = rd_covs$se[3],
                            p.value = rd_covs$pv[3],
                            h = rd_covs$bws[1],
                            conf.low = rd_covs$ci[3],
                            conf.high = rd_covs$ci[6],
                            `Obs. used` = sum(rd_covs$N_h),
                            cov = "With Covariates")
  
  results_departure <- rbind(results_departure, results_no_cov, results_cov)
  
}

results_departure$dv <- factor(results_departure$dv, levels = rev(unique(results_departure$dv)))
results_departure$lab = if_else(results_departure$dv == "Total Departures", "Total", "Categories")

figure_2 = ggplot(results_departure, aes(x = estimate, y = dv)) +
  geom_point(aes(shape = cov), size = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, linetype = cov), height = 0, size = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.3) +
  theme_bw() +
  xlim(-0.04, 0.105) +
  theme_ggplot +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.length=unit(.05, "cm"),
        strip.text.y = element_blank()) +
  facet_grid(lab~cov, space = "free", scales = "free")+
  guides(shape = "none", linetype = "none") +
  labs(x = NULL, y = " ")

ggsave("figure_2.png", figure_2, width=4.5, height= 2)

#################################################################
## Figure 3: Dealing with challenger status of populist mayors ##
#################################################################

# Remove obs where mayor was incumbent
no_incumbent <- rdd_data %>%
  filter(winning_candidate_was_incumbent == 0)

# Remove obs where mayor and at least one party supporting mayor were incumbent
no_incumbent_cand_and_party_1 <- rdd_data %>%
  filter(winning_candidate_was_incumbent == 0,
         winning_party_was_incumbent_at_least_one == 0)

# three datasets on which to perform RDD
data_incumbent <- list(no_incumbent,
                       no_incumbent_cand_and_party_1)
                 
types_of_incumbent = c("Challenger Candidates",
                       "Challenger Candidates and Parties")

# data.frame where to save results
results_no_incumbent <- data.frame()

for(k in 1:length(data_incumbent)){
  
  # select dataset
  no_incumbent = data_incumbent[[k]]
  
  # List of DVs
  dv <- with(no_incumbent, list(
    # Performance
    debt_accumulation,
    debt_repayment,
    cost_overrun_dummy,
    # Bureaucrats
    bur_turnover,
    bur_degree
  ))
  
  for(i in 1:length(dv)){
    
    # RDD no covariates
    rd <- with(no_incumbent, rdrobust(y = dv[[i]],
                                      x = margin,
                                      all = T,
                                      cluster = ISTAT_CODICE_COMUNE))
    
    # RDD with covariates
    rd_covs <- with(no_incumbent, rdrobust(y = dv[[i]],
                                           x = margin,
                                           covs = cbind(log(population_istat),
                                                        councillors,
                                                        degree_mayor,
                                                        female_mayor,
                                                        factor(year),
                                                        factor(ISTAT_CODICE_COMUNE),
                                                        factor(year_election),
                                                        ISTAT_surface_sq_km,
                                                        idro_risk_surface,
                                                        white_collar_dummy,
                                                        mayor_secondary_education),
                                           all = T,
                                           cluster = ISTAT_CODICE_COMUNE))
    
    results_no_cov <- data.frame(dv = dv_names_main[i],
                                 estimate = rd$coef[1],
                                 se = rd$se[3],
                                 p.value = rd$pv[3],
                                 h = rd$bws[1],
                                 conf.low = rd$ci[3],
                                 conf.high = rd$ci[6],
                                 `Obs. used` = sum(rd$N_h),
                                 cov = "No Covariates",
                                 type = type[i],
                                 incumbent = types_of_incumbent[k])
    
    results_cov <- data.frame(dv = dv_names_main[i],
                              estimate = rd_covs$coef[1],
                              se = rd_covs$se[3],
                              p.value = rd_covs$pv[3],
                              h = rd_covs$bws[1],
                              conf.low = rd_covs$ci[3],
                              conf.high = rd_covs$ci[6],
                              `Obs. used` = sum(rd_covs$N_h),
                              cov = "With Covariates",
                              type = type[i],
                              incumbent = types_of_incumbent[k])
    
    results_no_incumbent <- rbind(results_no_incumbent,
                                  results_no_cov,
                                  results_cov)
    
  }
}

# Prepare data.frame for figure
no_incumbent_plot <- bind_rows(# Add main results from table 2
                              main_results %>% mutate(incumbent = "All Mayors"),
                              # Add results without incumbent
                              results_no_incumbent) %>%
  # Adjust name of outcomes
  mutate(dv = str_replace(dv, "\\s(?=(Acc|Re|Ov|Man))", "\\\n"))


# Re-level variables
no_incumbent_plot$dv <- factor(no_incumbent_plot$dv, level = rev(unique(no_incumbent_plot$dv)))
no_incumbent_plot$incumbent <- factor(no_incumbent_plot$incumbent, level = unique(no_incumbent_plot$incumbent))

figure_3 = ggplot(no_incumbent_plot, aes(x = estimate, y = dv, color = incumbent, shape = incumbent, linetype = incumbent)) +
  geom_point(size = 1, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.4, height = 0, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.3) +
  theme_bw() +
  theme_ggplot +
  theme(
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    panel.spacing = unit(0.5, "lines")) +
  facet_grid(cols = vars(cov)) +
  scale_color_grey() +
  labs(x = NULL, y = " ", color = "Sample of Mayors", shape = "Sample of Mayors", linetype = "Sample of Mayors") +
  guides(color = guide_legend(byrow = TRUE))

ggsave("figure_3.png", figure_3, width = 6.7, height= 3.5)


############################################################################
## Figure 4, sequentially removing right-wing party from populist parties ##
############################################################################

# Load different datasets where set of populist parties changed omitting one right-wing party at time 
load("dataset_right_wing_parties_removed_sequentially.Rdata")

# Analysis different parties
party_removed <- c("LEGA", "FDI", "FI", "PDL", "FI and PDL")

results_right_wing_remove <- data.frame()

for(j in 1:length(party_removed)){
  
  # Select dataset where mayors supported by X are not consider populists, 
  # where X is a set of right wing populist parties 
  rwdata <- datasets_right_wing[[j]]
  
  # List of outcomes for the selected dataset
  dvs_rw <- with(rwdata, list(
    # Performance
    debt_accumulation,
    debt_repayment,
    cost_overrun_dummy,
    # Bureaucrats
    bur_turnover,
    bur_degree
  ))
  
  for(i in 1:length(dvs_rw)){
    
    # RDD with covariates
    rd_covs <- with(rwdata, rdrobust(y = dvs_rw[[i]],
                                       x = margin,
                                       covs = cbind(log(population_istat),
                                                    councillors,
                                                    degree_mayor,
                                                    female_mayor,
                                                    factor(year),
                                                    factor(ISTAT_CODICE_COMUNE),
                                                    factor(year_election),
                                                    ISTAT_surface_sq_km,
                                                    idro_risk_surface,
                                                    white_collar_dummy,
                                                    mayor_secondary_education),
                                       all = T,
                                       cluster = ISTAT_CODICE_COMUNE))
    
    results_cov <- data.frame(dv = dv_names_main[i],
                              estimate = rd_covs$coef[1],
                              se = rd_covs$se[3],
                              p.value = rd_covs$pv[3],
                              h = rd_covs$bws[1],
                              conf.low = rd_covs$ci[3],
                              conf.high = rd_covs$ci[6],
                              `Obs. used` = sum(rd_covs$N_h),
                              cov = "With Covariates",
                              type = type[i],
                              party_removed = party_removed[j])
    
    results_right_wing_remove <- rbind(results_right_wing_remove, results_cov)
    
  }
  print(j)
}

# Adjust names of variables for figure
results_right_wing_remove = results_right_wing_remove %>%
  mutate(dv = dv %>% str_replace(" (?=(Acc)|(Repay)|(Over)|(Manag))", "\n"))

# Relevel outcomes and party label
results_right_wing_remove$dv <- factor(results_right_wing_remove$dv, 
                                       levels = rev(unique(results_right_wing_remove$dv)))

results_right_wing_remove$party_removed <- factor(results_right_wing_remove$party_removed, 
                                                  levels = c("LEGA", "FDI", "FI", "PDL", "FI and PDL"))

figure_4 = ggplot(results_right_wing_remove, aes(estimate, dv, color = party_removed)) + 
  geom_point(size = 1) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 0.4) +
  theme_bw() +
  theme_ggplot +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        axis.ticks.y = element_blank()) +
  labs(x = NULL, y = NULL) +
  facet_grid(cols = vars(party_removed)) +
  scale_x_continuous(breaks = c(-0.15, 0, 0.15)) +
  scale_color_grey() +
  guides(color = "none")

ggsave("figure_4.png", figure_4, width=7, height= 2.8)

