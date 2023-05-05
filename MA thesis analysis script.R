#
#
# MA thesis R script
# 
# David Klug
#
#
#



# Packages ----------------------------------------------------------------
library(renv)
renv::init()
library(here)
library(rio)
library(MASS)
library(stargazer) # for R >= 4.2.2 run this bugfix https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
library(tidyverse)
library(AER)
library(readtext)
library(quanteda)
library(ggplot2)
library(stringr)
library(scales)
library(lemon)
library(ggpattern)
library(flextable)
library(ggcorrplot)
library(devtools)
#devtools::install_github("caijun/ggcorrplot2")
library(ggcorrplot2)
library(ggforce) #implicit dependency of ggcorrplot2
library(performance)
library(mice)
library(pscl)
library(poissonreg)
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")


# Setting up the environment and data import-------------------------------
# Clear the R environment
rm(list=ls())

# Set the working directory (should be redundant thanks to .Rproj)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import data
country_coding <- import("country_coding.xlsx")
d <- import("coding_sheet.xlsx")

# Data manipulation -------------------------------------------------------

# defining factor levels in correct order for HR categories
d$coding_category <- factor(
  d$coding_category,
  levels = c(
    "Investment HR incorporation",
    "Supplier HR screening",
    "Employee HR training",
    "Non-discrimination",
    "Freedom of association",
    "Child labor",
    "Forced labor",
    "Security practices",
    "Indigenous rights",
    "HR impact assessment",
    "Remediation"
    ))

# adjusting the data type of some variables and adding country level variables
d <- d %>% 
  mutate(across(assurance:hr_statement, ~as.numeric(.))) %>% 
  # there is no distinction between mainland China and HK possible, so the two are merged
  mutate(headquarters_country = if_else(headquarters_country=="Hong Kong","China",headquarters_country)) %>%
  left_join(country_coding,by=c("headquarters_country"="Country")) %>%
  mutate(mandatory_regulation_level = as.numeric(mandatory_regulation_level), 
         soft_law_level = as.numeric(soft_law_level),
         ROA = Profit/Assets)
  

# Tables and plots --------------------------------------------------------

dir.create("Outputs")
  
## Histogram for dep variables ---------------------------------------------
d_hist <- d %>%
  group_by(company_name) %>%
  summarise(sd_score = sum(sd_score, na.rm=T),
                          hd_score = sum(
                            sum(hd_data_present, na.rm=T),
                            sum(hd_rel_to_peers, na.rm=T),
                            sum(hd_rel_to_prev_period, na.rm=T),
                            sum(hd_rel_to_targets, na.rm=T),
                            sum(hd_abs_and_normalized, na.rm=T),
                            sum(hd_disaggregated, na.rm=T))) %>%
  select(sd_score, hd_score) %>%
  pivot_longer(c(sd_score, hd_score),
                             names_to = "disclosure_type",
                             values_to = "disclosure_score") %>%
  group_by(disclosure_type, disclosure_score) %>%
  summarise(count = n())

p_hist <- ggplot() +
  geom_col(aes(x=disclosure_score, y=count),
                         fill="lightgrey",
                         color="black",
                         data=d_hist) +
  geom_vline(aes(xintercept=mean_value),
                           linetype = "11",
                           linewidth=1.2,
                           data=d_hist %>% mutate(mean_value=sum(disclosure_score*count)/sum(count))) +
  geom_label(aes(x=disclosure_score, y=count, label=count,),
                           size=3,
                           fill="white",
                           vjust=-.2,
                           data=d_hist) +
  lemon::facet_rep_grid(
    . ~ disclosure_type,
    labeller = labeller(
      disclosure_type = c("hd_score"= "Hard human rights disclosure score (max. possible: 66)",
                                        "sd_score" ="Soft human rights disclosure score (max. possible: 11)")),
    repeat.tick.labels=T) +
  scale_x_continuous(
    breaks = 0:12) +
  scale_y_continuous(
    breaks = scales::breaks_width(5),
    limits = c(0,40),
    expand = c(0.02,0)) +
  labs(title = "Univariate distribution of hard and soft human rights disclosure scores",
                     subtitle = "The vertical line marks the overall average",
                     x = "disclosure score",
                     y = "count of companies") +
  theme(
    panel.grid.major.y = element_line(color="grey40"),
    axis.ticks.y = element_line(color="grey40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(size = 10, face="italic"),
    strip.background = element_blank(),
    plot.margin = margin(.2,.2,.2,.2, "inches"))

ggsave("Outputs/plot_depvar_hist.png", plot=p_hist, device="png", width=9, height=5)
  

## Average scores by category ---------------------------------------------------------

d_cat <- d %>%
  select(company_name, coding_category:hd_disaggregated) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  group_by(coding_category, company_name) %>%
  mutate(hd_score = sum(
    sum(hd_data_present, na.rm=T),
    sum(hd_rel_to_peers, na.rm=T),
    sum(hd_rel_to_prev_period, na.rm=T),
    sum(hd_rel_to_targets, na.rm=T),
    sum(hd_abs_and_normalized, na.rm=T),
    sum(hd_disaggregated, na.rm=T))) %>%
  select(-c(hd_data_present:hd_disaggregated)) %>%
  mutate(hd_score_binary = if_else(hd_score==0, 0, 1)) %>%
  group_by(coding_category) %>%
  summarise(mean_disclosure_sd = mean(sd_score),
            mean_disclosure_hd = mean(hd_score),
            mean_disclosure_hd_binary = mean(hd_score_binary)) %>%
  pivot_longer(mean_disclosure_sd:mean_disclosure_hd_binary,
               names_to = "disclosure_type", values_to = "disclosure_value")

p_cat <- ggplot(data=d_cat) +
  geom_col_pattern(
    aes(x=str_wrap(coding_category, width = 15),
        y=disclosure_value,
        pattern = disclosure_type),
    pattern_spacing = 0.02,
    fill="white",
    color= "black",
    width=.75,
    position=position_dodge(width=.75)) +
  scale_y_continuous(
    limits = c(0,1),
    breaks=seq(0, 1, by=.1),
    expand = c(0.02,0)) +
  scale_x_discrete(
    position="bottom",
    guide = guide_axis(angle = 45),
    limits = str_wrap(levels(d$coding_category), width=15)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey40"),
    axis.ticks.y = element_line(color="grey40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",  #c(.9, 1.14)
    legend.title = element_blank(),
    axis.text.x = element_text(size=11),
    axis.title.x = element_blank(),
    plot.margin = margin(.2,.2,.1,.2, "inches")) +
  scale_pattern_discrete(
    breaks=c("mean_disclosure_hd","mean_disclosure_hd_binary", "mean_disclosure_sd"),
    labels=c("hard disclosure (total)", "hard disclosure (yes/no)","soft disclosure")) +
  labs(title = "Average hard and soft disclosure scores by human rights category",
       subtitle = "Maximum for 'soft disclosure' is 1\nMaximum for 'hard disclosure (total)' is 6\nMaximum for 'hard disclosure (yes/no)' is 1 (all scores above 0 are changed to 1)",
       y = "average disclosure score")

ggsave("Outputs/plot_depvar_categories.png", plot=p_cat, device="png", width=9, height=5)



## Average scores by disclosure criteria ---------------------------------------------------------

d_criteria <- d %>%
  select(hd_data_present:hd_disaggregated, company_name) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  group_by(company_name) %>%
  summarise(across(hd_data_present:hd_disaggregated, ~sum(.))) %>%
  select(-company_name) %>%
  pivot_longer(hd_data_present:hd_disaggregated,
                             names_to = "criterium",
                             values_to = "score") %>%
  mutate(score_binary = if_else(score==0, 0, 1)) %>%
  group_by(criterium) %>%
  summarise(score = mean(score),
                          score_binary = mean(score_binary)) %>%
  pivot_longer(score:score_binary,
                             names_to = "score_type",
                             values_to = "score_value") %>%
  mutate(criterium = factor(criterium)) %>%
  mutate(criterium = fct_relevel(
    fct_recode(criterium,
               "data present" = "hd_data_present",
               "rel. to peers" = "hd_rel_to_peers",
               "rel. to prev. period" = "hd_rel_to_prev_period",
               "rel. to targets" = "hd_rel_to_targets",
               "abs. and normalized" = "hd_abs_and_normalized",
               "disaggregated" = "hd_disaggregated"),
    "data present",
    "rel. to peers",
    "rel. to prev. period",
    "rel. to targets",
    "abs. and normalized",
    "disaggregated"))
  
p_crit <-  ggplot(data=d_criteria) +
  geom_col_pattern(
    aes(
      x=criterium,
      y=score_value,
      pattern=score_type),
    position="dodge",
    #width=.85,
      pattern_spacing = 0.02,
    fill="white",
    color= "black") +
  scale_y_continuous(
    limits = c(0,.72),
    breaks=seq(0, .7, by=.1),
    expand = c(0.02,0)) +
  scale_x_discrete(
    limits = levels(d_criteria)) +
  scale_pattern_discrete(labels = c("average (total)", "average (yes/no)")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey40"),
    axis.ticks.y = element_line(color="grey40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(.9, 1.08),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_blank(),
    plot.margin = margin(.2,.2,.2,.2, "inches")) +
  labs(
    title = "Average hard disclosure scores by disclosure criterium",
    subtitle = "Maximum for 'average (total)' is 11\nMaximum for 'average (yes/no)' is 1 (all scores above 0 are changed to 1)",
    y = "average hard disclosure score")

ggsave("Outputs/plot_depvar_criteria.png", plot=p_crit, device="png", width=9, height=4.5)  




## Frequency and prop table ------------------------------------------------

d_depvar_table <- d %>%
  select(coding_category:hd_disaggregated) %>%
  group_by(coding_category) %>%
  reframe(across(
    everything(),
    # display the sum of companies on the left and the percentage on the right
    ~paste0(
      sum(., na.rm=T),
      " / ",
      round(mean(., na.rm=T)*100, digits = 1),
      "%"
      )))

d_depvar_table %>%
  flextable::as_flextable() %>%
  flextable::save_as_docx(path="Outputs/table_depvar.docx")
  
  
# Bringing the data into its final shape ----------------------------------


## Summarise disclosure scores ---------------------------------------------
d <- d %>%
  group_by(company_name) %>%
  mutate(sd_score = sum(!is.na(sd_score)),
                       hd_score = sum(
                         sum(!is.na(hd_data_present)),
                         sum(!is.na(hd_rel_to_peers)),
                         sum(!is.na(hd_rel_to_prev_period)),
                         sum(!is.na(hd_rel_to_targets)),
                         sum(!is.na(hd_abs_and_normalized)),
                         sum(!is.na(hd_disaggregated)))) %>%
  select(
    -coding_category,
    -c(hd_data_present:hd_disaggregated)) %>%
  filter(if_any(c(assurance,hr_report,hr_policy,hr_statement), ~!is.na(.))) %>%
  distinct() %>%
  mutate(HR_report = ifelse(hr_report==1|hr_policy==1|hr_statement==1, 1, 0))


d <- d %>%
  select(
    company_name,
    HR_incident_count,
    HRP_index,
    headquarters_country,
    sd_score,
    hd_score,
    assurance,
    HR_report,
    ROA,
    logged_Assets,
    soft_law_level,
    mandatory_regulation_level)



## Add report length variable ----------------------------------------------

# first, unzip the "Folders_CSR reports.zip" file
# using unzip() is unreliable, so better do it within the OS's file explorer

# the code below was used to change the file names in such a way that
# they contain the company name to enable merging with the full data set
# downstream. however, as I have already taken care of this step, the code
# below should not be executed anymore
# # first, get list of paths to csr reports
# oldnames <- list.files("./Folders_CSR reports/", full.names = T, recursive = T)[-51] # R thinks theres a desktop.ini file even though I can't see it
# # construct new filepaths where the pdfs are renamed as reprisk companies
# newnames <- file.path(
#   list.files("./Folders_CSR reports/", full.names = T),
#   paste0(str_extract(list.files("./Folders_CSR reports/"), "(?<=---\\s).*"), ".pdf"))
# # notet that there is a problem with Zurich and JPMC because file paths got too long
# # these have to be renamed manually
# # as I've already done this step, the code below is commented out
# # file.rename(oldnames, newnames)
  
# read in all pdfs, will take about 3 minutes
# 'invalid font weight' errors can be safely ignored


txt <- readtext::readtext("./Folders_CSR reports*/")

txt <- txt[1:50, ] # get rid of desktop.ini

# delete the .pdf file endings
# now the doc_ids match the company names exactly
txt$doc_id <- str_sub(txt$doc_id, 0, -5)
intersect(txt$doc_id, d$company_name)

# get number of words
txt <- corpus(txt)
txt <- tokens(txt)
txt <- ntoken(txt)

# join with full data set
txt <- data.frame(company_name = names(txt),
                  report_length = txt)
d <- d %>%
  left_join(txt, by="company_name") %>% 
  ungroup() %>%
  mutate(report_length = report_length/1000)

# remove txt object because it takes up a lot of space
rm(txt)

# shorten variable names for the upcoming plots and analysis
d <- d %>%
  rename(
    hD = hd_score,
    sD = sd_score,
    HRP = HRP_index,
    RL = report_length,
    ASSUR = assurance,
    HRR = HR_report,
    lnA = logged_Assets,
    ROA = ROA,
    mREG = mandatory_regulation_level,
    sLAW = soft_law_level,
    CNTRY = headquarters_country
  )


# More plots and tables ----------------------------------------------------

## List of companies -------------------------------------------------------

d %>% select(company_name) %>% distinct() %>% stargazer(summary=F, type="html", out="Outputs/company_list.html")


## Scatterplot of DV and IV ------------------------------------------------

d_scatter <- d %>% 
  select(sD, hD, HRP) %>% 
  pivot_longer(c(sD, hD), names_to = "disclosure_type", values_to = "disclosure_score")

p_scatter <- ggplot(data=d_scatter) +
  geom_point(aes(x=HRP, y=disclosure_score), size=3, alpha=.4, position = position_jitter(.05)) +
  facet_grid(disclosure_type~., 
             labeller = labeller(disclosure_type = c("hD"="hard disclosure", "sD"="soft disclosure"))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=10, face="italic"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_line(color="grey"),
        axis.ticks = element_blank(),
        plot.margin = margin(.2,.2,.2,.2, "inches")) + # using inches because that's what ggsave() uses as well
  scale_x_continuous(breaks = c(seq(-60,0, by=10))) +
  labs(title="Scatterplot of dependent and independent variables",
       subtitle="A small degree of noise is added to the position of the points to mitigate overplotting",
       y="disclosure score",
       x="human rights performance index")

ggsave("Outputs/plot_scatter.png", plot=p_scatter, device="png", width=9, height=5)

  

## Disclosure levels by country --------------------------------------------

  d_countries <- d %>%
    select(CNTRY, sD, hD) %>%
    mutate(hd_mean = mean(hD),
                         sd_mean = mean(sD)) %>%
    add_count(CNTRY) %>%
    group_by(CNTRY) %>%
    mutate(sD = mean(sD),
                         hD = mean(hD)) %>%
    pivot_longer(c(sD, hD), names_to = "vartype", values_to = "value") %>%
    distinct() %>%
    mutate(CNTRY = case_when(
      CNTRY=="United States of America" ~ "USA",
      CNTRY=="United Kingdom of Great Britain and Northern Ireland" ~ "UK",
      CNTRY=="Russian Federation" ~ "Russia",
      TRUE ~ CNTRY))
  
  p_countries <- ggplot(data=d_countries) +
    geom_col_pattern(
      aes(
        x=reorder(CNTRY, n, max),
        y=value,
        pattern=vartype),
      pattern_spacing = 0.02,
      fill="white",
      color= "black",
      width=.75,
      position=position_dodge(width=.75))+
    geom_label(
      aes(
        x=CNTRY,
        y=-.45,
        label=n),
      data=d_countries) +
    geom_hline(aes(yintercept=hd_mean), color="black", linewidth=1.2) +
    geom_hline(aes(yintercept=sd_mean), color ="black", linewidth=1.2) +
    scale_x_discrete(
      guide = guide_axis(angle = 45)) +
    scale_y_continuous(
      breaks = scales::breaks_width(1)) +
    scale_pattern_discrete(
      labels=c("hard disclosure", "soft disclosure")) +
    theme(
      panel.background = element_blank(),
      legend.title = element_blank(),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color="grey40"),
      axis.ticks.y = element_line(color="grey40"),
      axis.text.x = element_text(size=11, margin = margin(t=-5)),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(.2,.2,.1,.2, "inches")) + # using inches because that's what ggsave() uses as well
    labs(
      title="Number of companies and average human rights disclosure score by headquarters country",
      subtitle="The two lines show the average disclosure score. The one above represents soft disclosure, the one below hard disclosure.",
      y="average disclosure score")
  ggsave("Outputs/plot_depvar_countries.png", plot=p_countries, device="png", width=9, height=5)
  
  

## Histogram independent variable ------------------------------------------

  p_hrp <- ggplot(data=d) +
    geom_histogram(aes(x=HRP), binwidth = 1, fill="grey") +
    geom_vline(aes(xintercept=mean(HRP)),linetype = "11",linewidth=1.2)+
    scale_x_continuous(breaks=seq(-70,0, by=5)) +
    labs(title="Distribution of the independent variable",
                       subtitle="The vertical line marks the overall average",
                       x="human rights performance index") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(.1,.1,.1,.1, "inches"),
      panel.grid.major.y = element_line(color="grey40"),
      axis.ticks.y = element_line(color="grey40"))
  
  ggsave("Outputs/plot_indep.png", plot=p_hrp, device="png", width=7, height=4)
  

  
## Summary table -----------------------------------------------------------
d_summary <- d %>%
    as.data.frame() %>%
    select(
      hD,
      sD,
      HRP,
      RL,
      ASSUR,
      HRR,
      lnA,
      ROA,
      mREG,
      sLAW
      )
  
  stargazer(
    d_summary,
    type="html",
    out="Outputs/summary_table.html")


## Correlation matrix ------------------------------------------------------

d_corrplot <- d %>% select(hD, sD, HRP, RL, ASSUR, HRR, lnA, ROA, mREG, sLAW)
  
corr <- cor(d_corrplot, use = "pairwise.complete.obs")
corr_pmat <- cor_pmat(d_corrplot)

p_corr <- ggcorrplot2::ggcorrplot.mixed(
corr,
upper = "circle",
lower = "number",
p.mat = corr_pmat,
insig = "label_sig",
sig.lvl = c(0.1, 0.05, 0.01)) +
theme(panel.background = element_rect(hsv(0,0,0.97)),
      plot.margin = margin(.15,0,.15,0, "inches")) +
scale_fill_gradientn(colours = c("red","orange","limegreen", "green"),
                                   limits = c(-1, 1),
                                   guide = guide_colorbar(
                                     direction = "vertical",
                                     title = "",
                                     nbin = 1000,
                                     ticks.colour = "black",
                                     frame.colour = "black",
                                     barwidth = 1.5,
                                     barheight = 15)) +
scale_colour_gradientn(colours = "black", guide = "none") +
labs(title="Correlation matrix for all variables",
                   subtitle = "Confidence levels: p=0.1 *, p=0.05 **, p=0.01 ***\nVariable names: hD=hard disclosure, sD=soft disclosure, HRP=human rights performance, \nRL=report length, ASSUR=assurance, HRR=human rights report, lnA=logged total assets, \nROA=return on assets, mREG=level of mandatory regulation, sLAW=level of soft regulation")
ggsave("Outputs/plot_corr.png", plot=p_corr, device="png", width=7.5, height=6)





# Regression assumption tests ----------------------------------------------

## Assumption tests: overdispersion ----------------------------------------

# the tests below show overdispersion for hD, but no overdispersion for sD
# therefore, the poisson glm can be used for sD, but for hD, a negative binomial model is preferred

# create models used in tests
poisson_model_hd_minimal <- glm(hD ~ HRP, data = d, family = "poisson") 
poisson_model_hd_maximal <- glm(hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW, data = d, family = "poisson")
poisson_model_sd_minimal <- glm(sD ~ HRP, data = d, family = "poisson") 
poisson_model_sd_maximal <- glm(sD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW, data = d, family = "poisson")

# check dispersion of hard disclosure models using AER::dispersiontest()
poisson_model_hd_minimal %>% AER::dispersiontest()
poisson_model_hd_maximal %>% AER::dispersiontest()

# check dispersion of soft disclosure models using AER::dispersiontest()
poisson_model_sd_minimal %>% AER::dispersiontest()
poisson_model_sd_minimal %>% AER::dispersiontest()

# check dispersion of hard disclosure models using performance::check_overdispersion()
poisson_model_hd_minimal %>% performance::check_overdispersion()
poisson_model_hd_maximal %>% performance::check_overdispersion()

# check dispersion of soft disclosure models using performance::check_overdispersion()
poisson_model_sd_minimal %>% performance::check_overdispersion()
poisson_model_sd_maximal %>% performance::check_overdispersion()



## Assumption tests: VIF and collinearity ----------------------------------

# tests for the neg binomial hD model and the poisson sD model showed no collinearity issues based on VIF

# create neg binominal hD model for testing
negbin_model_hd_maximal <- glm.nb(hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW, data = d)

# check for collinearity issues
negbin_model_hd_maximal %>% performance::check_collinearity()
poisson_model_sd_maximal %>% performance::check_collinearity()

# save VIFs to txt file
negbin_model_hd_maximal %>% vif() %>% capture.output(file="Outputs/vif_hd.txt")
poisson_model_sd_maximal %>% vif() %>% capture.output(file="Outputs/vif_sd.txt")





# Regression analysis ---------------------------------------------------

## Preparing the data ------------------------------------------------------

# Formulas
fhd_max <- as.formula("hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW")
fsd_max <- as.formula("sD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW")

# generate imputed data due to missing reporting length
m <- 30
d_imp <- mice::mice(data = d, m = m, maxit = 10, seed = 1, print = F)

## Soft disclosure models --------------------------------------------------

# Note: MICE imputation is only necessary for those models that contain report length (RL)

# create minimal sD model
model_sd_min <- glm(sD ~ HRP, data = d, family = "poisson")

# create sD model with firm-level control variables
model_sd_firmlvl <- glm(sD ~ HRP + lnA + ROA, data = d, family = "poisson")

# create sD model with country-level control variables
model_sd_cntrylvl <- glm(sD ~ HRP + mREG + sLAW, data = d, family = "poisson")

# create sD model with document-level control variables
models_sd_documentlvl <- lapply(1:m, function(i) {
  glm(sD ~ HRP + RL + ASSUR + HRR,
      data = mice::complete(data = d_imp, i), family = "poisson")
})

# create maximal sD model with imputed datasets
models_sd_max <- lapply(1:m, function(i) {
  glm(sD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW,
         data = mice::complete(data = d_imp, i), family = "poisson")
})

# create stepwise sD model with imputed datasets
stepwise_regression_sd <- function(data) {
  step(glm(sD ~ HRP, data = data, family = "poisson"),
       direction = "forward",
       scope = fsd_max) }
models_sd_stepwise <- lapply(1:m, function(i) {
  stepwise_regression_sd(mice::complete(data = d_imp, i))
})



## Hard disclosure models --------------------------------------------------

# create minimal hD model
model_hd_min <- glm.nb(hD ~ HRP, data = d)

# create hD model with firm-level control variables
model_hd_firmlvl <- glm.nb(hD ~ HRP + lnA + ROA, data = d)

# create hD model with country-level control variables
model_hd_cntrylvl <- glm.nb(hD ~ HRP + mREG + sLAW, data = d)

# create hD model with document-level control variables
models_hd_documentlvl <- lapply(1:m, function(i) {
  glm.nb(hD ~ HRP + RL + ASSUR + HRR,
      data = mice::complete(data = d_imp, i))
})

# create maximal hD model with imputed datasets
models_hd_max <- lapply(1:m, function(i) {
  glm.nb(hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW,
         data = mice::complete(data = d_imp, i))
})

# create stepwise hD model with imputed datasets
stepwise_regression_hd <- function(data) {
  step(glm.nb(hD ~ HRP, data = data),
       direction = "forward",
       scope = fhd_max) }
models_hd_stepwise <- lapply(1:m, function(i) {
  stepwise_regression_hd(mice::complete(data = d_imp, i))
})


## Pooling the MICE regression results -------------------------------------

# extract MICE pooled regression results for soft disclosure models
models_sd_max_pooled <- mice::pool(models_sd_max)
models_sd_stepwise_pooled <- mice::pool(models_sd_stepwise)
models_sd_documentlvl_pooled <- mice::pool(models_sd_documentlvl)

# extract MICE pooled regression results for hard disclosure models
models_hd_max_pooled <- mice::pool(models_hd_max)
models_hd_stepwise_pooled <- mice::pool(models_hd_stepwise)
models_hd_documentlvl_pooled <- mice::pool(models_hd_documentlvl)


## Exporting regression model results -------------------------------------

# for R >= 4.2.2 run this bugfix https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

# create big regression table with soft and hard disclosure regression results
stargazer(
  # specify models (these mostly serve as placeholders because the coefficents etc. need to be extracted manually for pooled regression)
  model_sd_min, models_sd_documentlvl[[1]], model_sd_firmlvl, model_sd_cntrylvl, models_sd_stepwise[[1]], models_sd_max[[1]], 
  model_hd_min, models_hd_documentlvl[[1]], model_hd_firmlvl, model_hd_cntrylvl, models_hd_stepwise[[1]], models_hd_max[[1]],
  
  dep.var.caption = "Dependent variable: human rights disclosure score",
  dep.var.labels = c("soft disclosure", "hard disclosure"),
  title = "Poisson and negative binomial GLM regression about the relationship between human rights disclosure and performance",
  type = "html",
  out = "Outputs/reg_table_main.html",
  omit.stat = "theta",

  coef = list(coef(model_sd_min),
              summary(models_sd_documentlvl_pooled)$estimate,
              coef(model_sd_firmlvl),
              coef(model_sd_cntrylvl),
              summary(models_sd_stepwise_pooled)$estimate,
              summary(models_sd_max_pooled)$estimate,
              coef(model_hd_min),
              summary(models_hd_documentlvl_pooled)$estimate,
              coef(model_hd_firmlvl),
              coef(model_hd_cntrylvl),
              summary(models_hd_stepwise_pooled)$estimate,
              summary(models_hd_max_pooled)$estimate),
  
  se = list(summary(model_sd_min)$coefficients[, "Std. Error"],
            summary(models_sd_documentlvl_pooled)$std.error,
            summary(model_sd_firmlvl)$coefficients[, "Std. Error"],
            summary(model_sd_cntrylvl)$coefficients[, "Std. Error"],
            summary(models_sd_stepwise_pooled)$std.error,
            summary(models_sd_max_pooled)$std.error,
            summary(model_hd_min)$coefficients[, "Std. Error"],
            summary(models_hd_documentlvl_pooled)$std.error,
            summary(model_hd_firmlvl)$coefficients[, "Std. Error"],
            summary(model_hd_cntrylvl)$coefficients[, "Std. Error"],
            summary(models_hd_stepwise_pooled)$std.error,
            summary(models_hd_max_pooled)$std.error),
  
  t = list(coef(model_sd_min)/summary(model_sd_min)$coefficients[, "Std. Error"],
           summary(models_sd_documentlvl_pooled)$statistic,
           coef(model_sd_firmlvl)/summary(model_sd_firmlvl)$coefficients[, "Std. Error"],
           coef(model_sd_cntrylvl)/summary(model_sd_cntrylvl)$coefficients[, "Std. Error"],
           summary(models_sd_stepwise_pooled)$statistic,
           summary(models_sd_max_pooled)$statistic,
           coef(model_hd_min)/summary(model_hd_min)$coefficients[, "Std. Error"],
           summary(models_hd_documentlvl_pooled)$statistic,
           coef(model_hd_firmlvl)/summary(model_hd_firmlvl)$coefficients[, "Std. Error"],
           coef(model_hd_cntrylvl)/summary(model_hd_cntrylvl)$coefficients[, "Std. Error"],
           summary(models_hd_stepwise_pooled)$statistic,
           summary(models_hd_max_pooled)$statistic),
  
  p = list(coef(summary(model_sd_min))[,4],
           summary(models_sd_documentlvl_pooled)$p.value,
           coef(summary(model_sd_firmlvl))[,4],
           coef(summary(model_sd_cntrylvl))[,4],
           summary(models_sd_stepwise_pooled)$p.value,
           summary(models_sd_max_pooled)$p.value,
           coef(summary(model_hd_min))[,4],
           summary(models_hd_documentlvl_pooled)$p.value,
           coef(summary(model_hd_firmlvl))[,4],
           coef(summary(model_hd_cntrylvl))[,4],
           summary(models_hd_stepwise_pooled)$p.value,
           summary(models_hd_max_pooled)$p.value)
  )



# Robustness tests --------------------------------------------------------


## Hurdle models for hard disclosure ---------------------------------------

# create minimal hD hurdle model
model_hurdle_min <- hurdle(hD ~ HRP, data = d, dist = "negbin", zero.dist = "negbin")
hurdletest(model_hurdle_min)

# create hD hurdle model with firm-level control variables
model_hurdle_firmlvl <- hurdle(hD ~ HRP + lnA + ROA, data = d, dist = "negbin", zero.dist = "negbin")

# create hD hurdle model with country-level control variables
model_hurdle_cntrylvl <- hurdle(hD ~ HRP + mREG + sLAW, data = d, dist = "negbin", zero.dist = "negbin")

# create hD hurdle model with document-level control variables
models_hurdle_documentlvl <- lapply(1:m, function(i) {
  hurdle(hD ~ HRP + RL + ASSUR + HRR,
         data = mice::complete(data = d_imp, i), dist = "negbin", zero.dist = "negbin")
})

# create maximal hD hurdle model with imputed datasets
models_hurdle_max <- lapply(1:m, function(i) {
  hurdle(hD ~ HRP + RL + ASSUR + HRR + lnA + ROA + mREG + sLAW,
         data = mice::complete(data = d_imp, i), dist = "negbin", zero.dist = "negbin")
})

# create stepwise hD hurdle model with imputed datasets
stepwise_regression_hurdle <- function(data) {
  step(hurdle(hD ~ HRP, data = data),
       direction = "forward",
       scope = fhd_max) }
models_hurdle_stepwise <- lapply(1:m, function(i) {
  stepwise_regression_hurdle(mice::complete(data = d_imp, i))
})


## Pooling the hurdle MICE models --------------------------------------------

# The approach for extracting hurdle model information is based on an SO post here:
# https://stackoverflow.com/questions/75717211/problem-with-pooling-estimates-from-multiple-imputed-datasets-using-mice-in-r-z

# Step 1
# Convert to MICE models to tidy format
models_hurdle_max_tidy <- mice::getfit(models_hurdle_max) %>%
  lapply(function(fit) poissonreg::tidy(fit, type="all")) %>% 
  bind_rows()
models_hurdle_stepwise_tidy <- mice::getfit(models_hurdle_stepwise) %>%
  lapply(function(fit) poissonreg::tidy(fit, type="all")) %>% 
  bind_rows()
models_hurdle_documentlvl_tidy <- mice::getfit(models_hurdle_documentlvl) %>%
  lapply(function(fit) poissonreg::tidy(fit, type="all")) %>% 
  bind_rows()

# Step 2
# Convenience wrapper function around pool.scalar.
# pool.scalar also returns a "qhat" and "u" which are vectors, 
# and we don't need them. Those vectors mess up the format of
# the summary that we want to compute later.
wrap.pool.scalar <- function(estimates, variances, n, k) {
  pool_res <- mice::pool.scalar(estimates, variances, n = n, k = k)
  return(as_tibble(list(
    qbar = pool_res$qbar, 
    ubar = pool_res$ubar, 
    b = pool_res$b, 
    t = pool_res$t, 
    df = pool_res$df, 
    r = pool_res$r, 
    fmi = pool_res$fmi)))
}

# For each (term,type) pair, compute pooled univariate estimates using 
# wrap.pool.scalar 
models_hurdle_max_tidy_pooled <- models_hurdle_max_tidy %>% 
  group_by(term, type) %>% 
  reframe(wrap.pool.scalar(estimate, std.error^2, n=60, k=1)) %>% 
  mutate(estimate = qbar)
models_hurdle_stepwise_tidy_pooled <- models_hurdle_stepwise_tidy %>% 
  group_by(term, type) %>% 
  reframe(wrap.pool.scalar(estimate, std.error^2, n=60, k=1)) %>% 
  mutate(estimate = qbar)
models_hurdle_documentlvl_tidy_pooled <- models_hurdle_documentlvl_tidy %>% 
  group_by(term, type) %>% 
  reframe(wrap.pool.scalar(estimate, std.error^2, n=60, k=1)) %>% 
  mutate(estimate = qbar)

# Step 3
# Copy the pooled estimate calculations from the the MICE package
# https://github.com/amices/mice/blob/master/R/mipo.R#L69-L71
models_hurdle_max_tidy_smry <- models_hurdle_max_tidy_pooled %>% mutate(
  std.error = sqrt(t), 
  statistic = estimate / std.error,
  p.value = 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))) %>% 
  dplyr::select(term, type, estimate, std.error, statistic, df, p.value)
models_hurdle_stepwise_tidy_smry <- models_hurdle_stepwise_tidy_pooled %>% mutate(
  std.error = sqrt(t), 
  statistic = estimate / std.error,
  p.value = 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))) %>% 
  dplyr::select(term, type, estimate, std.error, statistic, df, p.value)
models_hurdle_documentlvl_tidy_smry <- models_hurdle_documentlvl_tidy_pooled %>% mutate(
  std.error = sqrt(t), 
  statistic = estimate / std.error,
  p.value = 2 * (pt(abs(statistic), pmax(df, 0.001), lower.tail = FALSE))) %>% 
  dplyr::select(term, type, estimate, std.error, statistic, df, p.value)

# extract zero or count part of the tidy hurdle models
models_hurdle_max_tidy_smry_zero <- models_hurdle_max_tidy_smry[models_hurdle_max_tidy_smry$type=="zero",]
models_hurdle_max_tidy_smry_count <- models_hurdle_max_tidy_smry[models_hurdle_max_tidy_smry$type=="count",]
models_hurdle_stepwise_tidy_smry_zero <- models_hurdle_stepwise_tidy_smry[models_hurdle_stepwise_tidy_smry$type=="zero",]
models_hurdle_stepwise_tidy_smry_count <- models_hurdle_stepwise_tidy_smry[models_hurdle_stepwise_tidy_smry$type=="count",]
models_hurdle_documentlvl_tidy_smry_zero <- models_hurdle_documentlvl_tidy_smry[models_hurdle_documentlvl_tidy_smry$type=="zero",]
models_hurdle_documentlvl_tidy_smry_count <- models_hurdle_documentlvl_tidy_smry[models_hurdle_documentlvl_tidy_smry$type=="count",]

# also tidy up non MICE models for consistency
model_hurdle_min_zero <- poissonreg::tidy(model_hurdle_min, type="all") %>% filter(type=="zero")
model_hurdle_min_count <- poissonreg::tidy(model_hurdle_min, type="all") %>% filter(type=="count")
model_hurdle_firmlvl_zero <- poissonreg::tidy(model_hurdle_firmlvl, type="all") %>% filter(type=="zero")
model_hurdle_firmlvl_count <- poissonreg::tidy(model_hurdle_firmlvl, type="all") %>% filter(type=="count")
model_hurdle_cntrylvl_zero <- poissonreg::tidy(model_hurdle_cntrylvl, type="all") %>% filter(type=="zero")
model_hurdle_cntrylvl_count <- poissonreg::tidy(model_hurdle_cntrylvl, type="all") %>% filter(type=="count")



## Exporting hurdle model results ------------------------------------------

# for R >= 4.2.2 run this bugfix https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

stargazer(
  # specify models (these mostly serve as placeholders because the coefficents etc. need to be extracted manually for pooled regression)
  model_hurdle_min, models_hurdle_documentlvl[[1]], model_hurdle_firmlvl,
  model_hurdle_cntrylvl, models_hurdle_stepwise[[1]], models_hurdle_max[[1]],
  model_hurdle_min, models_hurdle_documentlvl[[1]], model_hurdle_firmlvl,
  model_hurdle_cntrylvl, models_hurdle_stepwise[[1]], models_hurdle_max[[1]],
  
  dep.var.caption = "Dependent variable: hard human rights disclosure score",
  dep.var.labels = "",
  title = "Neg. binomial hurdle regression that models the relationship between human rights disclosure and performance<br>(1-6 are zero models, 7-12 are count models)",
  type = "html",
  out = "Outputs/reg_table_hurdle.html",
  omit.stat = "theta",
  
  coef = list(model_hurdle_min_zero$estimate,
              models_hurdle_documentlvl_tidy_smry_zero$estimate,
              model_hurdle_firmlvl_zero$estimate,
              model_hurdle_cntrylvl_zero$estimate,
              models_hurdle_stepwise_tidy_smry_zero$estimate,
              models_hurdle_max_tidy_smry_zero$estimate,
              model_hurdle_min_count$estimate,
              models_hurdle_documentlvl_tidy_smry_count$estimate,
              model_hurdle_firmlvl_count$estimate,
              model_hurdle_cntrylvl_count$estimate,
              models_hurdle_stepwise_tidy_smry_count$estimate,
              models_hurdle_max_tidy_smry_count$estimate),
  
  se = list(model_hurdle_min_zero$std.error,
            models_hurdle_documentlvl_tidy_smry_zero$std.error,
            model_hurdle_firmlvl_zero$std.error,
            model_hurdle_cntrylvl_zero$std.error,
            models_hurdle_stepwise_tidy_smry_zero$std.error,
            models_hurdle_max_tidy_smry_zero$std.error,
            model_hurdle_min_count$std.error,
            models_hurdle_documentlvl_tidy_smry_count$std.error,
            model_hurdle_firmlvl_count$std.error,
            model_hurdle_cntrylvl_count$std.error,
            models_hurdle_stepwise_tidy_smry_count$std.error,
            models_hurdle_max_tidy_smry_count$std.error),
  
  t = list(model_hurdle_min_zero$statistic,
           models_hurdle_documentlvl_tidy_smry_zero$statistic,
           model_hurdle_firmlvl_zero$statistic,
           model_hurdle_cntrylvl_zero$statistic,
           models_hurdle_stepwise_tidy_smry_zero$statistic,
           models_hurdle_max_tidy_smry_zero$statistic,
           model_hurdle_min_count$statistic,
           models_hurdle_documentlvl_tidy_smry_count$statistic,
           model_hurdle_firmlvl_count$statistic,
           model_hurdle_cntrylvl_count$statistic,
           models_hurdle_stepwise_tidy_smry_count$statistic,
           models_hurdle_max_tidy_smry_count$statistic),
  
  p = list(model_hurdle_min_zero$p.value,
           models_hurdle_documentlvl_tidy_smry_zero$p.value,
           model_hurdle_firmlvl_zero$p.value,
           model_hurdle_cntrylvl_zero$p.value,
           models_hurdle_stepwise_tidy_smry_zero$p.value,
           models_hurdle_max_tidy_smry_zero$p.value,
           model_hurdle_min_count$p.value,
           models_hurdle_documentlvl_tidy_smry_count$p.value,
           model_hurdle_firmlvl_count$p.value,
           model_hurdle_cntrylvl_count$p.value,
           models_hurdle_stepwise_tidy_smry_count$p.value,
           models_hurdle_max_tidy_smry_count$p.value)
)
