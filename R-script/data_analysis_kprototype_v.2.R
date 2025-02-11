library(tidyverse)
library(xlsx)
library(naniar)
library(gridExtra)
library(factoextra)
library(clustMixType)

## read the data in a csv format (traditional way)

data_thesis = read.csv("../complete analysis/final_data.csv")  # Read character variables as factors # all variables 96 observations, 33 variables
str(data_thesis)
rownames(data_thesis) = make.names(data_thesis$Country, unique = TRUE)
## Remove variable

data_thesis = data_thesis[, -14]

## Identify character variables to transform as factors

data_fac <- data_thesis
char_cols <- sapply(data_thesis, is.character)
char_cols  # 10 character variables

# Character variables to factors

data_fac$Mean.Employment.Tenure <- as.factor(data_fac$Mean.Employment.Tenure)
data_fac$Main.Method.of.Skill.acquisition = as.factor(data_fac$Main.Method.of.Skill.acquisition) 
data_fac$Union.Type = as.factor(data_fac$Union.Type)
data_fac$Main.source.of.Funding = as.factor(data_fac$Main.source.of.Funding)
data_fac$Fund.Allocation.Criteria = as.factor(data_fac$Fund.Allocation.Criteria)
data_fac$Main.Criteria.for.pay.raise...promotion = as.factor(data_fac$Main.Criteria.for.pay.raise...promotion)
data_fac$Main.ownership.of.large.firms = as.factor(data_fac$Main.ownership.of.large.firms)
data_fac$main.controlling.owner = as.factor(data_fac$main.controlling.owner)
data_fac$Type = as.factor(data_fac$Type)
data_fac$state.decision.making = as.factor(data_fac$state.decision.making)



## describe the dataframe
skimr::skim(data_fac)  # More complete description of the variables

## check missing values

anyNA(data_fac) # Logical function
sum(is.na(data_fac))  # 36 missing values

## Check pattern of missing values with naniar package

prop_complete(data_fac)
pct_miss(data_thesis)  ## 1.1 % missing values
missing_table = miss_var_summary(data_fac[, c(3:32)])  # I excluded country and year
missing_table  # Describe the missing pattern in the variables (rule of law, interfirm relations, and confidence in labour unions)

## Replace by zeros NAs in numerical variables

library(dplyr)
data_fac1 <- data_fac %>%
  mutate(rule.of.law1 = if_else(is.na(rule.of.law), 0, rule.of.law ))  

data_fac1 = data_fac1 %>% 
mutate(Confidence.in.labor.unions1 = ifelse(is.na(Confidence.in.labor.unions), 0, Confidence.in.labor.unions))


data_fac1 = data_fac1 %>% 
  mutate(Interfirm.Relations1 = ifelse(is.na(Interfirm.Relations), which.max(Interfirm.Relations), Interfirm.Relations))


## Remove original variables

data_fac1$Confidence.in.labor.unions <- NULL
data_fac1$rule.of.law <-NULL
data_fac1$Interfirm.Relations <- NULL

summary(data_fac1)
anyNA(data_fac1)

# Number of countries included in the sample (24 countries)

countrylist = unique(data_fac1$Country)
length(countrylist)

# Number of time periods (4 time periods)

yearlist = unique(data_fac1$Attribute)
length(yearlist)

## 
set.seed(1234)


kp <- kproto(data_fac1[,3:31], k = 4, diss = "gower")
print(kp)
class(kp)
lambdaest(data_fac1[, 3:31])
summary(kp)
cluster.assignments = kp$cluster
cluster.assignments
#Choosing optimal K
data <- data_fac1[,3:31]
  # Elbow Method for finding the optimal number of clusters
  set.seed(1234)
  
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
#data <- na.omit(data) # to remove the rows with NA's
wss <- sapply(1:k.max, 
              function(k){kproto(data, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")> clprofiles(kp, na.omit(data_fac[,3:32]), vars = NULL, col = NULL)

clprofiles(kp, data_fac1[,3:31], vars = NULL, col = NULL)

###
## country and cluster assignment
## merge 4
data_fac1$cluster <- kp$cluster

write.csv(data_fac1, file="cluster_kproto1.csv")

