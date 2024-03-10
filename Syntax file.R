###############################
# Analysis for Machado et al. #
###############################

# required libraries
library(tidyverse)
library(sjPlot)
library(psych)
library(car)

# set working directory to load the file

setwd("/Users/Arthur/Google Drive/LPfC/Doutorado/Dados Doutorado/Psicovida I/Resultados")

# set seed
set.seed(123)

# open file
df2 <- read.csv(file="Study data.csv")

# see data structure
str(df2)

# col index
data.frame(colnames(df2))

# set categorical variables into factors
col <- c(2,4:11,14:26)
df2[,col] <- lapply(df2[,col],factor)

str(df2)

# observe contrasts
contrasts(df2$ptsd.factor)

# observe contrasts
contrasts(df2$phq.factor)

# set no depression as reference
df2 <- within(df2, phq.factor <- relevel(phq.factor, ref = 2))

# observe contrasts
contrasts(df2$sex)

# set male as reference
df2 <- within(df2, sex <- relevel(sex, ref = 2))

# observe contrasts
contrasts(df2$race)

# set white as reference
df2 <- within(df2, race <- relevel(race, ref = 2))

# observe contrasts
contrasts(df2$category)

# set physician as reference
df2 <- within(df2, category <- relevel(category, ref = 5))

# observe contrasts
contrasts(df2$ppe)

# observe contrasts
contrasts(df2$mental.disorder)

# n and percentages (or median and iqr) for whole sample

lapply(df2[, c(3,12:13)],
                  FUN=function(x) c(median=median(x),
                                    iqr=quantile(x)))

df2 %>%
  count(ptsd.factor) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(phq.factor) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(sex) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(race) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(ppe) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(category) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(mental.disorder) %>%
  mutate(prop = prop.table(n))

df2 %>%
  count(region) %>%
  mutate(prop = prop.table(n))

df2 %>%
  group_by(ppe, category) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

# n and percentage (or median and iqr) by ptsd.factor

aggregate(age~ptsd.factor,data=df2,FUN=function(x) c(median=median(x),
                                                     iqr=quantile(x)))

aggregate(pcl~ptsd.factor,data=df2,FUN=function(x) c(median=median(x),
                                                     iqr=quantile(x)))

aggregate(phq~ptsd.factor,data=df2,FUN=function(x) c(median=median(x),
                                                     iqr=quantile(x)))

tbp1 <- df2 %>%
  group_by(sex, ptsd.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbp1

tbp2 <- df2 %>%
  group_by(race, ptsd.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbp2

tbp3 <- df2 %>%
  group_by(ppe, ptsd.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbp3

tbp4 <- df2 %>%
  group_by(category, ptsd.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbp4

tbp5 <- df2 %>%
  group_by(mental.disorder, ptsd.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbp5

# plots for ptsd factor

theme_set(theme_bw())

ggplot(df2, aes(x=ptsd.factor,y=age))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width=0.2)

ggplot(tbp1, aes(x=sex,y=prop, fill=ptsd.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable PTSD") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbp2, aes(x=race,y=prop, fill=ptsd.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable PTSD") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbp3, aes(x=ppe,y=prop, fill=ptsd.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable PTSD") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbp4, aes(x=category,y=prop, fill=ptsd.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable PTSD") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbp5,aes(x=mental.disorder,y=prop, fill=ptsd.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable PTSD") +
  scale_y_continuous(limits = c(0,1))

# bivariate analyses for ptsd.factor

# mann-whitney u test for age and ptsd.factor
umod1 <- wilcox.test(age~ptsd.factor,df2)
umod1

# chi-square test for sex and ptsd.factor
umod2 <- chisq.test(df2$sex,df2$ptsd.factor)
umod2

# chi-square test for race and ptsd.factor
umod3 <- chisq.test(df2$race,df2$ptsd.factor)
umod3

# chi-square test for ppe and ptsd.factor
umod4 <- chisq.test(df2$ppe,df2$ptsd.factor)
umod4

# chi-square test for category and ptsd.factor
umod5 <- chisq.test(df2$category,df2$ptsd.factor)
umod5

# chi-square test for mental.disorder and ptsd.factor
umod6 <- chisq.test(df2$mental.disorder, df2$ptsd.factor)
umod6

# logistic regression with ptsd as dependent variable
mlog1 <- glm(ptsd.factor~age+category+sex+ppe+
               mental.disorder,
             family=binomial,data=df2)

summary(mlog1)

# plot model
mlog1_plot <- plot_model(mlog1, show.values = TRUE,
                         value.offset = 0.4,
                         dot.size = 4,
                         colors = "bw",
                         axis.labels = c("Mental health disorder","Inadequate PPE",
                                         "Female","Physical therapists",
                                         "Nurses","Nurse technicians",
                                         "Other healthcare workers",
                                         "Age"),
                         title = "",
                         vline.color = "gray")+
  ylim(0,4)+
  theme_sjplot()

mlog1_plot

# check for multicollinearity with variance inflation factors
vif(mlog1)

# check for linearity assumption between outcome and continuous predictor

# predict the probability (p) of PTSD
predicted1 <- predict(mlog1, type = "response")

# get the log odds
logodds1 <- log(predicted1/(1-predicted1))

# select numerical independent variables
linearcheck1 <- df2["age"]

# bind the log-odds
linearcheck1 <- data.frame(cbind(linearcheck1,logodds1))

# plot logit against the predictor
ggplot(linearcheck1, aes(x=age,y=logodds1))+
  geom_point(alpha=0.6,size=2,color="deepskyblue3") +
  ylab("Log-odds\n") +
  xlab("\nAge")

# check for outliers and influential values
influenceIndexPlot(mlog1)

# bonferroni outlier test of the studentized residuals
outlierTest(mlog1)

# check if influential values change model coefs
mlog1_35_332 <- update(mlog1,subset=c(-35,-332))

compareCoefs(mlog1,mlog1_35_332)

# odds ratio coef and 95% CI
round(exp(cbind("Odds ratio" = coef(mlog1),
                confint.default(mlog1, level = 0.95))),3)

#######################
# Depression analyses #
#######################

# remove previous depression and bipolar diagnosis from sample

df3 <- df2[- grep("Bipolar", df2$disorders.names),]
df3$disorders.names <- droplevels(df3$disorders.names)

df3 <- df3[- grep("Depress", df3$disorders.names),]
df3$disorders.names <- droplevels(df3$disorders.names)

levels(df3$disorders.names)

# n and percentage (or median and iqr) by depression

aggregate(age~phq.factor,data=df3,FUN=function(x) c(median=median(x),
                                                    iqr=quantile(x)))

aggregate(pcl~phq.factor,data=df3,FUN=function(x) c(median=median(x),
                                                    iqr=quantile(x)))

aggregate(phq~phq.factor,data=df3,FUN=function(x) c(median=median(x),
                                                    iqr=quantile(x)))

tbd1 <- df3 %>%
  group_by(sex, phq.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbd1

tbd2 <- df3 %>%
  group_by(race, phq.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbd2

tbd3 <- df3 %>%
  group_by(ppe, phq.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbd3

tbd4 <- df3 %>%
  group_by(category, phq.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbd4

tbd5 <- df3 %>%
  group_by(mental.disorder, phq.factor) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

tbd5

# plots for phq factor

ggplot(df3, aes(x=phq.factor,y=age))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width=0.2)

ggplot(tbd1, aes(x=sex, y=prop, fill=phq.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable depression") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbd2, aes(x=race, y=prop, fill=phq.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable depression") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbd3, aes(x=ppe, y=prop, fill=phq.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable depression") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbd4, aes(x=category, y=prop, fill=phq.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable depression") +
  scale_y_continuous(limits = c(0,1))

ggplot(tbd5,aes(x=mental.disorder, y=prop, fill=phq.factor)) +
  geom_col(position = "dodge") +
  ylab("proportion of probable depression") +
  scale_y_continuous(limits = c(0,1))

# bivariate analyses for phq.factor

# mann-whitney u test for age and phq.factor
umod11 <- wilcox.test(age~phq.factor,df3)
umod11

# chi-square test for sex and phq.factor
umod12 <- chisq.test(df3$sex,df3$phq.factor)
umod12

# chi-square test for race and phq.factor
umod13 <- chisq.test(df3$race,df3$phq.factor)
umod13

# chi-square test for ppe and phq.factor
umod14 <- chisq.test(df3$ppe,df3$phq.factor)
umod14

# chi-square test for category and phq.factor
umod15 <- chisq.test(df3$category,df3$phq.factor)
umod15

# chi-square test for mental.disorder and phq.factor
umod16 <- chisq.test(df3$mental.disorder, df3$phq.factor)
umod16

# logistic regression with depression as dependent variable
mlog2 <- glm(phq.factor~age+category+sex+ppe+
               mental.disorder,
             family=binomial,data=df3)

summary(mlog2)

# plot model
mlog2_plot <- plot_model(mlog2, show.values = TRUE,
                         value.offset = 0.4,
                         dot.size = 4,
                         colors = "bw",
                         axis.labels = c("Mental health disorder","Inadequate PPE",
                                         "Female","Physical therapists",
                                         "Nurses","Nurse technicians",
                                         "Other healthcare workers",
                                         "Age"),
                         title = "",
                         vline.color = "gray")+
  ylim(0,4)+
  theme_sjplot()

mlog2_plot

# check for multicollinearity with variance inflation factors
vif(mlog2)

# check for linearity assumption between outcome and continuous predictor

# predict the probability (p) of Depression
predicted2 <- predict(mlog2, type = "response")

# get the log odds
logodds2 <- log(predicted2/(1-predicted2))

# select numerical independent variables
linearcheck2 <- df3["age"]

# bind the log-odds
linearcheck2 <- data.frame(cbind(linearcheck2,logodds2))

# plot logit against the predictor
ggplot(linearcheck2, aes(x=age,y=logodds2))+
  geom_point(alpha=0.6,size=2,color="deepskyblue3") +
  ylab("Log-odds\n") +
  xlab("\nAge")

# check for outliers and influential values
influenceIndexPlot(mlog2)

# bonferroni outlier test of the studentized residuals
outlierTest(mlog2)

# check if influential values change model coefs
mlog2_332_439 <- update(mlog2,subset=c(-332,-439))

compareCoefs(mlog2,mlog2_332_439)

# odds ratio coef and 95% CI
round(exp(cbind("Odds ratio" = coef(mlog2),
                confint.default(mlog2, level = 0.95))),3)


########################
# Internal consistency #
########################

# subset pcl items
consist_pcl <- df2[,27:46]

# Cronbach's alpha for pcl items
alpha(consist_pcl)$total$std.alpha

# subset phq items
consist_phq <- df3[,47:56]

# Cronbach's alpha for phq items
alpha(consist_phq)$total$std.alpha

# Cronbach's alpha for theoretical dimension of DSM-5 PTSD

# subset subscales
criterionB <- consist_pcl[,1:5]

criterionC <- consist_pcl[,6:7]

criterionD <- consist_pcl[,8:14]

criterionE <- consist_pcl[15-20]

# alphas

alpha(criterionB)$total$std.alpha

alpha(criterionC)$total$std.alpha

alpha(criterionD)$total$std.alpha

alpha(criterionE)$total$std.alpha
