#----------PROJECT STATS IN THE SOCIAL SCIENCES----------
# Hypothesis: People with higher education levels view vaccines more favorably than those with lower levels of educational attainent.
# Null: People with higher education levels do not view vaccines any more or less favorably than those with lower levels of educational attainment.
# ctrl + L to clear console

#```{r tinytex}
#tinytex::install_tinytex()
#```
#Import libraries
#library(ggfortify) do I need this?
tinytex::install_tinytex()
install.packages("margins")
library(haven) # to read SAS
library(skimr)
library(Hmisc)
library(tidyverse) #ggplot2, dplyr, readr, tibble, 
library(car)
library(stargazer)
library(boot)#bootstrapping
library(margins) #binary logistic regression
library(ggeffects) #binary logistic regresssion




#loading in sas document 
data <- read_sas("/Users/Anne/OneDrive/Documents/UoE/2023-24 Postgrad/Statistical Analysis/Project Stats 23/2022/gss7222_r2.sas7bdat") #NORC's GSS 2022
dim(data) #72390 cases (rows) by 6693 variables (columns)

#keep necessary variables
data <- data %>% select(EDUC, AGE, VAXSAFE, VAXKIDS, COVID12, VAXDOHARM)
dim(data)#72,390 rows x 7 columns

# EDUC - respondent's education, VAXSAFE - vaccines are safe (agree disagree), AXKIDS - vaccines are important for children to have (agree/disagree)
# COVID12 - received a COVID-19 vaccine?, 
# VAXDOHARM - how much agree with vax do more harm than good

str(data) #check the structure of the dataframe, the dataset contains 72,390 rows and 7 columns

#The columns of interest (VAXSAFE, VAXKIDS, COVID12, VAXDOHARM) all seem to have a large number of NA (missing) values.

summary(data) #summary statistics
describe(data) #hmisc describe dataset

#frequency tables
table(data$EDUC) #frequency table of EUDC (values 0-20, ordinal categories)
table(data$AGE) #frequency table of AGE (values 18-89, discrete)
table(data$VAXSAFE) #freq VAXSAFE (values 1-5 likert scale, strongly agree-strongly disagree)
table(data$VAXKIDS) #freq VAXKIDS (values 1-5 likert scale, strongly agree-strongly disagree)
table(data$COVID12) #freq COVID12 (values 1 yes, 2 no)
table(data$VAXDOHARM) #freq VAXDOHARM (values 1-5 likert scale, strongly agree- strongl disagree)


#-----------------Cleaning data/ missing data---------------

#check for missing values
sum(is.na(data)) #365,967 missing values in the data frame

sum(is.na(data$VAXSAFE))  #number of missing values in VAXSAFE

# Calculate the percentage of missing data for VAXSAFE
percentage_missing_VAXSAFE <- sum(is.na(data$VAXSAFE)) / nrow(data) * 100
print(percentage_missing_VAXSAFE) #98.29811

#VAXKIDS
percentage_missing_VAXKIDS <- sum(is.na(data$VAXKIDS)) / nrow(data) * 100
print(percentage_missing_VAXKIDS) #98.29673

#COVID12
percentage_missing_COVID12 <- sum(is.na(data$COVID12)) / nrow(data) * 100
print(percentage_missing_COVID12) #98.3064


#VAXDOHARM
percentage_missing_VAXDOHARM <- sum(is.na(data$VAXDOHARM)) / nrow(data) * 100
print(percentage_missing_VAXDOHARM) #98.45835 :(((( so much missing


#REMOVING NA VALUES
data_complete <- na.omit(data)

sum(is.na(data_complete))#how many NA cases are there? (0)

describe(data_complete)#descritpive stats for new dataset  378 cases for all variables

data <- data_complete #rename this small dataframe to data
describe(data)#double check

#explain in essay why I chose to delete NA (imputing techniques wouldnt work with this dataset consideirng the percentage of missing data)



#------------ Univariate analysis-----------------

#check the type of each remaining variable EDUC, AGE, VAXSAFE, VAXDOHARM, COVID12



# - EDUC - 
#checking the structure of EDUC
str(data$EDUC) #currently a numeric variable, not ordinal factor, metadata comes from sas, need to change
#make EDUC labels to correspond to the education levels in GSS code book
EDUC_LABELS <- c("0" = "No Formal Schooling",
                 "1" = "1st Grade",
                 "2" = "2nd Grade",
                 "3" = "3rd Grade",
                 "4" = "4th Grade",
                 "5" = "5th Grade",
                 "6" = "6th Grade",
                 "7" = "7th Grade",
                 "8" = "8th Grade",
                 "9" = "9th Grade",
                 "10" = "10th Grade",
                 "11" = "11th Grade",
                 "12" = "12th Grade",
                 "13" = "1 Year of College",
                 "14" = "2 Years of College",
                 "15" = "3 Years of College",
                 "16" = "4 Years of College",
                 "17" = "5 Years of College",
                 "18" = "6 Years of College",
                 "19" = "7 Years of College",
                 "20" = "8 Years or More of College")
#convert EDUC to an ordnal factor
data$EDUC_ORDINAL <- factor(data$EDUC, levels = 0:20, labels = EDUC_LABELS, ordered = TRUE) #important to have order here
#check if ordinal now
str(data$EDUC_ORDINAL)
#make barchart with EBUC_LABEL
ggplot(data, aes(x = EDUC_ORDINAL)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Education Levels", x = "Education Level", y = "Count")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x labels for readability
#calculate and print summary stats for EDUC USING NUMERIC EDUC FOR THSE STATS
educ_summary <- data %>%
  summarise(
    count = sum(!is.na(EDUC)),
    mean = mean(EDUC, na.rm = TRUE), 
    median = median(EDUC, na.rm = TRUE),
    mode = as.numeric(names(sort(table(EDUC), decreasing = TRUE)[1])), # mode
    sd = sd(EDUC, na.rm = TRUE),
    min = min(EDUC, na.rm = TRUE),
    max = max(EDUC, na.rm = TRUE),
  )

print(educ_summary) #mean 14.7, median 14, mode 16, mean and mode better for Ordinal data


# - AGE - 
#check structure of AGE
str(data$AGE)#it is numeric, probably best to keep it as such for hist, boxplot, and qq plot to understand distribution
#Histogram for AGE
ggplot(data, aes(x = AGE)) +
  geom_histogram(binwidth = 1.5, fill = "lightblue", color = "black") + 
  labs(title = "Distribution of Respondents Age", x = "Age", y = "Frequency") +
  theme_minimal()#bin side 2 for enhanced clarity MAKE SURE TO MAKE OTHERS 2 FOR COMPARISON
#boxplot for AGE
ggplot(data, aes(y = AGE)) +
  geom_boxplot() +
  labs(title = "Boxplot of Respondents' Ages", y = "Age")
#Q-Q plot for AGE
qqPlot(data$AGE, main = "Q-Q Plot for AGE")
#summary statistics for AGE
age_summary <- data %>%
  summarise(
    count = sum(!is.na(AGE)),
    mean = mean(AGE, na.rm = TRUE),
    median = median(AGE, na.rm = TRUE),
    sd = sd(AGE, na.rm = TRUE),
    min = min(AGE, na.rm = TRUE),
    max = max(AGE, na.rm = TRUE),
    mode = as.numeric(names(sort(table(AGE), decreasing = TRUE)[1]))
  )
# Print the summary statistics
print(age_summary) #mean 50.2, median 51, sd 17.9, min 19, max 89, mode 32
 

# - VAXSAFE -
#check structure of VAXSAFE
str(data$VAXSAFE) #it is numeric
#make VAXSAFE labels to correspond to the VAXSAFE labels in the codebook
VAXSAFE_LABELS <- c("1" = "Strongly Agree",
                    "2" = "Agree",
                    "3" = "Neither Agree Nor Disagree",
                    "4" = "Disagree",
                    "5" = "Strongly Disagree")
#convert VAXSAFE to an ordinal factor (VAXSAFE_ORDINAL)
data$VAXSAFE_ORDINAL <- factor(data$VAXSAFE, levels = c(1, 2, 3, 4, 5), 
                               labels = VAXSAFE_LABELS, ordered = TRUE)
#check the structure for ordinal
str(data$VAXSAFE_ORDINAL)#YAY IT'S ORDINAL
#barchart for VAXSAFE_LABEL
ggplot(data, aes(x = factor(VAXSAFE_ORDINAL))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Prompt: Vaccines Are Safe", x = "Response Category", y = "Count") #need to re label the responses 
#frequency table for VAXSAFE_ORDINAL
VAXSAFE_freq <- table(data$VAXSAFE_ORDINAL)
print(VAXSAFE_freq)
#summary statistics for VAXSAFE
VAXSAFE_summary <- data %>%
  summarise(
    count = sum(!is.na(VAXSAFE_ORDINAL)),
    mean = mean(VAXSAFE, na.rm = TRUE),
    mode = as.numeric(names(sort(table(VAXSAFE), decreasing = TRUE)[1]))
  )
# Print the summary statistics
print(VAXSAFE_summary) #mean 2.01, mode 1


# - VAXKIDS -
#check structure of VAXKIDS
str(data$VAXKIDS) #it is numeric
#make VAXKIDS labels to correspond to the VAXKIDS labels in the codebook
VAXKIDS_LABELS <- c("1" = "Strongly Agree",
                    "2" = "Agree",
                    "3" = "Neither Agree Nor Disagree",
                    "4" = "Disagree",
                    "5" = "Strongly Disagree")
#convert VAXKIDS to an ordinal factor (VAXKIDS_ORDINAL)
data$VAXKIDS_ORDINAL <- factor(data$VAXKIDS, levels = c(1, 2, 3, 4, 5), 
                               labels = VAXKIDS_LABELS, ordered = TRUE)
#check for ordinal
str(data$VAXKIDS_ORDINAL) #YAY
#barchart for VAXKIDS_ORDINAL
ggplot(data, aes(x = factor(VAXKIDS_ORDINAL))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Prompt: Vaccines Are Important For Kids to Have", x = " Response Category", y = "Count")
#frequency table for VAXKIDS_ORDINAL
VAXKIDS_freq <- table(data$VAXKIDS_ORDINAL)
print(VAXKIDS_freq)
#summary statistics for VAXKIDS
VAXKIDS_summary <- data %>%
  summarise(
    count = sum(!is.na(VAXKIDS_ORDINAL)),
    mean = mean(VAXKIDS, na.rm = TRUE),
    mode = as.numeric(names(sort(table(VAXKIDS), decreasing = TRUE)[1]))
  )
# Print the summary statistics
print(VAXKIDS_summary) #mean 1.67, mode 1



# - COVID12 -
#check structure of COVID12
str(data$COVID12) #numeric
#make COVID12 labels to correspond to the COVID12 labels in the codebook
COVID12_LABELS <- c("1" = "Agree",
                    "2" = "Disagree")#checking the table
#make COVID12_LABELS factor variable from COVID12
data$COVID12_LABELS <- factor(data$COVID12, levels = names(COVID12_LABELS), labels = COVID12_LABELS)
#recoding COVID_LABELS into COVID_BINARY such that 0=disagree (no vaccine) and 1= agree(vaccine)
data$COVID12_BINARY <- ifelse(data$COVID12_LABELS == "Agree", 1, 0)
#check binary
str(data$COVID12_BINARY)
#barchart for COVID12
ggplot(data, aes(x = factor(COVID12_LABELS))) +
  geom_bar(fill = "darkred") +
  labs(title = "Prompt: Have You Ever Recieved a Covid-19 Vaccine?", x = " Response Category", y = "Count")
#frequency table for COVID_12
COVID12_freq <- table(data$COVID12_BINARY)
print(COVID12_freq) # 81 "0"=disagree/not vaccinated, 329 "1"=agree/were vaccinated 
#summary statistics for COVID12
COVID12_summary <- data %>%
  summarise(
    count = sum(!is.na(COVID12)),
    mean = mean(COVID12, na.rm = TRUE),
    mode = as.numeric(names(sort(table(COVID12), decreasing = TRUE)[1]))
  )
# Print the summary statistics
print(COVID12_summary) #mean 1.2, mode 1


# - VAXDOHARM - 
#check structure of VAXDOHARM
str(data$VAXDOHARM) #it is numeric
#make VAXDOHARM labels to correspond to the VAXDOHARM labels in the codebook
VAXDOHARM_LABELS <- c("1" = "Strongly Agree",
                    "2" = "Agree",
                    "3" = "Neither Agree Nor Disagree",
                    "4" = "Disagree",
                    "5" = "Strongly Disagree")
#convert VAXDOHARM to an ordinal factor (VAXDOHARM_ORDINAL)
data$VAXDOHARM_ORDINAL <- factor(data$VAXDOHARM, levels = c(1, 2, 3, 4, 5), 
                               labels = VAXDOHARM_LABELS, ordered = TRUE)
#check for ordinal
str(data$VAXDOHARM_ORDINAL) #YAY
#barchart for VAXDOHARM
ggplot(data, aes(x = factor(VAXDOHARM_ORDINAL))) +
  geom_bar(fill = "yellow", color = "black") +
  labs(title = "Prompt: Overally, Vaccines Do More Harm Than Good", x = " Response Category", y = "Count")
#frequency table for VAXDOHARM_ORDINAL
VAXDOHARM_freq <- table(data$VAXDOHARM_ORDINAL)
print(VAXDOHARM_freq)
#summary statistics for VAXDOHARM
VAXDOHARM_summary <- data %>%
  summarise(
    count = sum(!is.na(VAXDOHARM_ORDINAL)),
    mean = mean(VAXDOHARM, na.rm = TRUE),
    mode = as.numeric(names(sort(table(VAXDOHARM), decreasing = TRUE)[1]))
  )
# Print the summary statistics
print(VAXDOHARM_summary) #mean 3.98, mode 5, count 410




# -------------------------Bivariate analysis - analyse the relationships between two variables--------------------


#Check the class of EDUC and EDUC_ORDINAL
class(data$EDUC)#numeric, ok to use for regression/correlation
class(data$EDUC_ORDINAL)#factor- DO NOT USE IN EQUATIONS
#classes of VAXSAFE and VAXSAFE_ORDINAL
class(data$VAXSAFE)#numeric, ok to use for regression/correlation
class(data$VAXSAFE_ORDINAL) #factor, ordinal, not good for equations?
#classes of VAXDOHARM and VAXDOHARM_ORDINAL
class(data$VAXDOHARM)#numeric, ok to use for regression/correlation
class(data$VAXDOHARM_ORDINAL) #facto, ordinal, not good for equations
#class of AGE
class(data$AGE)#numeric, ok for regression 


# - correlation tests - 

#EDUC & VAXSAFE (better better reflects the ordinal nature of the Likert scale for rho)
#calculate the initial Spearmans cor 
correlation_educ_vaxsafe <- cor.test(data$EDUC, data$VAXSAFE, method = "spearman")#get error "Cannot compute exact p-value with ties"
#print
print(correlation_educ_vaxsafe)# 0.263, which indicates a weak negative association, p-value 6.3e-8 suggests it is super signifiant (p should be less than .05)
#function to calculate Spearmans cor
spearman_cor <- function(data, indices) {
  d <- data[indices, ]  # Allows boot to select sample
  cor(d$EDUC, d$VAXSAFE, method = "spearman")
}
#set up data for bootstrapping
data_boot <- data.frame(EDUC = data$EDUC, VAXSAFE = data$VAXSAFE)
#number of bootstrap replications
R <- 500
#running the bootstrapping
set.seed(123)  #setting the seed so if this is reproduced it gives the same result despite irl being randomized
boot_result <- boot(data_boot, spearman_cor, R = R)
#getting the bootstrapped p-value with the cor estimate from the cor.test() result
boot_p_value_educ_vaxsafe <- sum(boot_result$t >= correlation_educ_vaxsafe$estimate) / R
#print the bootstrapped p-value
print(boot_p_value_educ_vaxsafe) #Bootstrapped p-value .476


#EDUC & VAXDOHARM
#calculate the initial Spearmans cor 
correlation_educ_vaxdoharm <- cor.test(data$EDUC, data$VAXDOHARM, method = "spearman")#get error "Cannot compute exact p-value with ties"
#print
print(correlation_educ_vaxdoharm)# rho .315, which indicates a moderate positive association, p-value 6.6e-11 suggests it is super signifiant (p should be less than .05)
#function to calculate Spearmans cor
spearman_cor <- function(data, indices) {
  d <- data[indices, ]  # Allows boot to select sample
  cor(d$EDUC, d$VAXDOHARM, method = "spearman")
}
#set up data for bootstrapping
data_boot <- data.frame(EDUC = data$EDUC, VAXDOHARM = data$VAXDOHARM)
#number of bootstrap replications
R <- 500
#running the bootstrapping
set.seed(123)  #setting the seed so if this is reproduced it gives the same result despite irl being randomized
boot_result <- boot(data_boot, spearman_cor, R = R)
#getting the bootstrapped p-value with  the cor estimate from the cor.test() result
boot_p_value_educ_vaxdoharm <- sum(boot_result$t >= correlation_educ_vaxdoharm$estimate) / R
#print the bootstrapped p-value
print(boot_p_value_educ_vaxdoharm) #Bootstrapped p-value .498, should be less than .05
#InterpretationA bootstrapped p-value of 0.498 is quite high, suggesting that in nearly half of the bootstrap samples, a correlation as strong as (or stronger than) 0.3153252 is observed.
#This implies that the observed correlation could happen fairly frequently by chance, given the variability in your data. It's a more conservative estimate of significance compared to the very low p-value from the original Spearman's correlation test.


#AGE & VAXSAFE
#calculate the initial Spearmans cor 
correlation_age_vaxsafe <- cor.test(data$AGE, data$VAXSAFE, method = "spearman")#get error "Cannot compute exact p-value with ties"
#print
print(correlation_age_vaxsafe)# rho -.05, which indicates a v weak negative association, p-value .2456 suggest no signficance (p should be less than .05)
#function to calculate Spearmans cor
spearman_cor <- function(data, indices) {
  d <- data[indices, ]  # Allows boot to select sample
  cor(d$AGE, d$VAXSAFE, method = "spearman")
}
#set up data for bootstrapping
data_boot <- data.frame(AGE = data$AGE, VAXSAFE = data$VAXSAFE)
#number of bootstrap replications
R <- 500
#running the bootstrapping
set.seed(123)  #setting the seed so if this is reproduced it gives the same result despite irl being randomized
boot_result <- boot(data_boot, spearman_cor, R = R)
#getting the bootstrapped p-value with  the cor estimate from the cor.test() result
boot_p_value_age_vaxsafe <- sum(boot_result$t >= correlation_age_vaxsafe$estimate) / R
#print the bootstrapped p-value
print(boot_p_value_age_vaxsafe)#bootstrapped p = .53 no statiscial signficance


#AGE & VAXDOHARM
#calculate the initial Spearmans cor 
correlation_age_vaxdoharm <- cor.test(data$AGE, data$VAXDOHARM, method = "spearman")#get error "Cannot compute exact p-value with ties"
#print
print(correlation_age_vaxdoharm)# rho -.14, which indicates a v weak negative association, p-value .004 suggest some signficance (p should be less than .05)

#function to calculate Spearmans cor
spearman_cor <- function(data, indices) {
  d <- data[indices, ]  # Allows boot to select sample
  cor(d$AGE, d$VAXDOHARM, method = "spearman")
}
#set up data for bootstrapping
data_boot <- data.frame(AGE = data$AGE, VAXDOHARM = data$VAXDOHARM)
#number of bootstrap replications
R <- 500
#running the bootstrapping
set.seed(123)  #setting the seed so if this is reproduced it gives the same result despite irl being randomized
boot_result <- boot(data_boot, spearman_cor, R = R)
#getting the bootstrapped p-value with  the cor estimate from the cor.test() result
boot_p_value_age_vaxdoharm <- sum(boot_result$t >= correlation_age_vaxdoharm$estimate) / R
#print the bootstrapped p-value
print(boot_p_value_age_vaxdoharm)#bootstrapped p = .48 no statiscial signficance

#visual display to compare all of these results
# dataget data ready for the chart, 4 values for each
results_df <- data.frame(
  Test = c("EDUC & VAXSAFE", "EDUC & VAXDOHARM", "AGE & VAXSAFE", "AGE & VAXDOHARM"),
  Spearman_Rho = c(correlation_educ_vaxsafe$estimate, correlation_educ_vaxdoharm$estimate,
                   correlation_age_vaxsafe$estimate, correlation_age_vaxdoharm$estimate),
  Spearman_P_Value = c(correlation_educ_vaxsafe$p.value, correlation_educ_vaxdoharm$p.value,
                       correlation_age_vaxsafe$p.value, correlation_age_vaxdoharm$p.value),
  Bootstrap_P_Value = c(boot_p_value_educ_vaxsafe, boot_p_value_educ_vaxdoharm,
                        boot_p_value_age_vaxsafe, boot_p_value_age_vaxdoharm)
)

print(results_df)
#FROM THESE RESULTS: focus on EDUC&VAXSAFE,AGE&VAXDOHARM,maybe EDUC&VAXDOHARM ignore AGE&VAXSAFE


#Binary logistic  Regression, EDUC&COVID12_BINARY
#fit a logistic regression model
covid_model <- glm(COVID12_BINARY ~ EDUC, family = binomial(link = "logit"), data = data)
summary(covid_model)# coeffcient ofIntercept 2.14881 = negative log-odds of being agree when EDUC at baseline(lowest)
#suggests that negative value shows it is low log-odd of being agree and low education
#coef of EDUC is .251, positive suggests as educ goes up so do log odds of being in vaccinated category
#both p-values of statistical significance
#calculate and interpret odds ratios
exp(covid_model$coefficients)#odds ratio for EDUC is 1.29, for each aditioal unit of education odds of being vaccinated(/agreeing) go up by ~29%
#predicted probabilities and marginal effects
ggpredict(covid_model, terms = "EDUC[all]") %>%
  plot() +
  labs(title = "Predicted Probabilities of Vaccination Across Eduation Level") 
#Shows probability of being vaccinated changes across different levels of education,positive relationship, as educ increases so does likelihood of being vaccinated
#average marginal effects
covid_model_ave <- margins(covid_model, type = "response")
summary(covid_model_ave) #AME of 0.0370 , on avg each additional unit of education increases the probability of being vaccinated by 3.7 percent

#bootstrapping for logistic regression  EDUC&COVID12_BINARY (BCa) bias and skewness
#define function to get coeffs
boot_coef <- function(data, indices) {
  # Fit the model to the bootstrap sample
  model <- glm(COVID12_BINARY ~ EDUC, family = binomial(link = "logit"), data = data[indices, ])
  return(coef(model))
}
#number of bootstrap variations
n_boot <- 1000
#do the bootstrap
set.seed(123)#like last time, so that though it is randomized, results can be recreated
boot_results <- boot(data, boot_coef, R = n_boot)
#calc bootstrap confidence intervals
boot_ci <- boot.ci(boot_results, type = "bca")
# Print the bootstrap confidence intervals
print(boot_ci)



#Binary logistic regression AGE&COVID12_BINARY
#fit a binary logistic regression model
covid_model <- glm(COVID12_BINARY ~ AGE, family = binomial(link = "logit"), data = data)
summary(covid_model)# coeffcient ofIntercept -0.468 = negative log-odds of being agree when AGE at baseline(lowest)
#negative value shows it is low log-odd of being agree and low AGE
#coef of AGE is .041, positive suggests as AGE goes up so do log odds of being in vaccinated category
#both p-values of statistical significance
#calculate and interpret odds ratios
exp(covid_model$coefficients)#odds ratio for AGE is 1.04, for each additional unit of AGE odds of being vaccinated(/agreeing) go up by ~4%
#predicted probabilities and marginal effects
ggpredict(covid_model, terms = "AGE[all]") %>%
  plot() +
  labs(title = "Predicted Probabilities of Vaccination Across Respondent Age") 
#Shows probability of being vaccinated changes across different levels of AGE,positive relationship, as AGE increases so does likelihood of being vaccinated
#average marginal effects
covid_model_ave <- margins(covid_model, type = "response")
summary(covid_model_ave) #AME of 0.006 , on avg each additional unit of AGE increases the probability of being vaccinated by 3.7 percent, p value significant
#bootstrapping for logistic regression  AGE&COVID12_BINARY (BCa) bias and skewness
#define function to get coeffs
boot_coef <- function(data, indices) {
  # Fit the model to the bootstrap sample
  model <- glm(COVID12_BINARY ~ AGE, family = binomial(link = "logit"), data = data[indices, ])
  return(coef(model))
}
#number of bootstrap variations
n_boot <- 1000
#do the bootstrap
set.seed(123)#like last time, so that though it is randomized, results can be recreated
boot_results <- boot(data, boot_coef, R = n_boot)
#calc bootstrap confidence intervals
boot_ci <- boot.ci(boot_results, type = "bca")
# Print the bootstrap confidence intervals
print(boot_ci) #95% between -1.135, .209 BCa)


#----------------------------------multivariate analysis----------------------------------- analyse relationship between multiple variables
 
#multiple ordinal logistic regression (OLR)
#install here bc it causes issues elsehwere
library(MASS)#for multiple ordinal logistic regression
#ordinal Logistic Regression (OLR, best for likert scale )
#check structure of VAXSAFE, need it to be ordinal
str(data$VAXSAFE)
str(data$VAXSAFE_ORDINAL)#use the ordinal
#multivariate ordinal logistic regression
olr_model <- polr(data$VAXSAFE_ORDINAL ~ EDUC + AGE, data = data)
#check the summary
summary(olr_model) #Coefficients: EDUC -.18 std er .035  as education increases, the log odds of being in a higher category of agreement that vaccines are safe (moving from disagree to agree) decreases
#t-value of -5.224 = statistically signficiant
#coefficient for AGE is -.006, strd error .005, negative relationship (smaller and not as signficicant ad EDUC)
#t value of -1.211 not really staitsically sig
#resid dev: 988.8 lower indicates better model fit... yikes
#AIC akaike information criterion 1000.8, lower indicates better model quality

