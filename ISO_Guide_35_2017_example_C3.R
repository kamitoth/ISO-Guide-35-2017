#ISO Guide 35 2017 example C3
library(readr)  # for read_csv
library(lme4) # CRAN package for linear mixed effect models (LMEs)

data <- "https://raw.githubusercontent.com/kamitoth/ISO-Guide-35-2017/main/ISO_Guide_35_Example_C3.csv" #defining data url
C3 <- read_csv(url(data)) #reading of data
View(C3)

tf_anova <- aov(result ~ as.factor(unit) + as.factor(run), C3) # model definition; 
# categorical variables needs to be defined as categorical variables!!! see ?as.factor
sum <- summary(tf_anova) # the summary function provides MSs and F-statistics
sum
sqrt(sum[[1]]['Residuals','Mean Sq']) #swb, residual or within bottle SD
sqrt((sum[[1]]['as.factor(unit)','Mean Sq']-sum[[1]]['Residuals','Mean Sq'])/3) #sbb, between bottle SD

# Beyond two-factor ANOVA
# Linear mixed effect models (LMEs) or hierarchical models by frequentists
lmer(result ~ (1|unit) + (1|run), C3)
