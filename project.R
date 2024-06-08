{\rtf1\ansi\ansicpg1252\cocoartf2757
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 options(repos = "https://cran.r-project.org/")\
# Load the dplyr package\
library(dplyr)\
library(stats)\
library(corrplot)\
library(readr)\
library(parallel)\
library(doParallel)\
library(foreach)\
library(ggplot2)\
\
# Import Data\
df16_17<- read_csv("Datasets/CAHMI-Data-Resource-Center-for-Child-and-Adolescent-Health/2016-2017 NSCH_Topical_CSV_CAHMI_DRCv2/2016_17 NSCH_Topical_Implicate_CAHMI_DRCv2.csv")\
\
# Serialize data to disk\
saveRDS( df16_17 , file = "df16_17.rds")\
\
# Load data from disk when needed\
df<- readRDS("df16_17.rds")\
\
# Clean Data, remove irrelevant values \
df <- df16_17 %>% filter(ACE2more_1617 != 99)\
df <- df16_17 %>% filter(ADHDind_1617 != 99)\
df <- df16_17 %>% filter(ADHDind_1617 != 95)\
df <- df16_17 %>% filter(ADHDSevInd_1617 != 99)\
df <- df16_17 %>% filter(ADHDSevInd_1617 != 95)\
\
# Construct correlation matrix\
corr_matrix <- cor(df[,c("ACE2more_1617", "ADHDind_1617", "ADHDSevInd_1617",\
                         "ADHDMed_1617", "ADHDBehTreat_1617", "InsGap_1617",\
                         "MotherMH_1617", "FatherMH_1617", "flrish0to5_1617",\
                         "flrish6to17_1617", "ParAggrav_1617", "CurrIns_1617",\
                         "bully_1617", "bullied_1617", "MakeFriend_1617", "EmSupport_1617",\
                         "SLEEPPOS", "BedTime_1617", "HrsSleep_1617", "FoodSit_1617",\
                         "NbhdSupp_1617", "NbhdSafe_1617", "NbhdAmenities_1617", "NbhdDetract_1617",\
                         "SchlSafe_1617", "BedTime_1617", "age3_1617", "famstruct_1617", "povlev4_1617"),])\
\
# Print correlation matrix values\
print(corr_matrix)\
# Visualize correlation matrix\
corrplot(corr_matrix, method = "color")\
\
\
corr_matrix <- cor(df)\
# Heatmap\
heatmap(corr_matrix, annot = TRUE, main = "Correlation Heatmap")\
\
\
#++++++++++++++++++++++\
\
corr_1_n <- cor(df16_17)\
corrplot(corr_1_n , method = "color")\
heatmap(corr_1_n, annot = TRUE, main = "Correlation Heatmap")\
\
#++++++++++++++++++++++\
\
#write.csv(corr_matrix, file = 'corr.csv')\
\
# Perform linear regression to model the relationship between \
# ACE2more_1617(One or more ACE) and ADHDind_1617 (Existance of ADHD)\
model <- lm(ADHDind_1617 ~ ACE2more_1617, data = df)\
\
# View the regression summary\
summary(model)\
\
\
# ANOVA Test\
anova_test <- aov(ADHDind_1617 ~ ACE2more_1617, data = df)\
\
# View summary of ANOVA test\
summary(anova_test)\
\
#=========================\
# Number of resamples\
n_resamples <- 100\
\
# Initialize storage for coefficient estimates\
coefficient_estimates <- numeric(n_resamples)\
\
# Create a parallel backend for foreach\
cl <- makeCluster(detectCores())\
registerDoParallel(cl)\
\
# Parallelize the bootstrapping process using foreach\
resampled_datasets <- foreach(i = 1:n_resamples, .combine = 'list') %dopar% \{\
  # Randomly draw samples with replacement from observed data\
  resampled_data <- df[sample(nrow(df), replace = TRUE), ]\
  \
  # Fit linear regression to the resampled data\
  simulated_model <- lm(ADHDind_1617 ~ ACE2more_1617, data = resampled_data)\
  \
  # Extract and store the coefficient for ACE2more_1617\
  coefficient_estimate <- coef(simulated_model)["ACE2more_1617"]\
  \
  # Return the coefficient estimate as a numeric value\
  coefficient_estimate\
\}\
\
# Close the parallel backend\
stopCluster(cl)\
\
# Convert the list of coefficient estimates to a numeric vector\
coefficient_estimates <- unlist(resampled_datasets)\
\
# Check for and remove NA values\
coefficient_estimates <- coefficient_estimates[!is.na(coefficient_estimates)]\
\
# Calculate quantiles for the confidence interval\
confidence_interval <- quantile(coefficient_estimates, c(0.025, 0.975))\
\
# Print results\
cat("Coefficient Estimates:\\n")\
print(coefficient_estimates)\
cat("95% Confidence Interval:\\n")\
print(confidence_interval)\
}