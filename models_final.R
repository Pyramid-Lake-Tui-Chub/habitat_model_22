### INSTALL PACKAGES ----
library(DHARMa)
library(glmmTMB)
library(compositions)
library(robustbase)
library(car)
library(effects)
library(MuMIn)
library(emmeans)
library(performance)
library(ggplot2)
--------------------------------------------------------------------------------
### DATASET GENERATION ----

## GENERAL
# subset data just for substrate data (aka no Hoop net)
sub_only <- subset(master, perc_clay != "NA")

# add a logCpue column for log-normal regression
logCpue <- log(sub_only$matureallCpue + 0.08)
sub_only <- cbind(sub_only, logCpue)

## COMPLETE SUBSETS
# all months, depths 1 & 2 only
sub_depths <- subset(sub_only, depth_bin != 3)

#all depths, months 5 & 6 only
sub_months <- subset(sub_only, month != 7)

## VEGETATION
# subset to July for vegetation ( a full dataset)
sub_june <- subset(sub_only, avg_vegheight != "NA" & month == 6)

--------------------------------------------------------------------------------
  
### DEPTH MODELS ----

## (1) depth subset (all months, depths 1 & 2 only)

# the model
dep1 <- glmmTMB(logCpue ~ as.factor(depth_bin) + (1|site) + (1|site:depth_bin),
                #offset = log(effort_hours),
              family = gaussian,
              data = sub_depths)
summary(dep1)

#QQ plot residuals
dep1_resids <- simulateResiduals(dep1)
plot(dep1_resids)

# ANOVA results
car::Anova(dep1)

# check R2
performance::r2(dep1)

# predict
pdep1 <- predict(dep1, re.form=~0, type="link")

# create data frame for plotting
dep1_data <- data.frame(sub_depths, pdep1)

# plots of predicted values against response (how well does the model predict?)
ggplot(dep1_data,
       aes(logCpue, pdep1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(dep1_data,
       aes(as.factor(depth_bin), pdep1)) +
  geom_boxplot()

## (2) depth subset (all depths, months 5 & 6)

# the model
dep2 <- glmmTMB(logCpue ~ as.factor(depth_bin) + (1|site)+ (1|site:depth_bin),
                #offset = log(effort_hours),
                family = gaussian,
                data = sub_months)
summary(dep2)

#QQ plot residuals
dep2_resids <- simulateResiduals(dep2)
plot(dep2_resids)

# ANOVA results
car::Anova(dep2)

# check R2
performance::r2(dep2)
performance::check_model(dep2)
MuMIn::r.squaredGLMM(dep2)
# predict
pdep2 <- predict(dep2, re.form=~0, type="link")

# create data frame for plotting
dep2_data <- data.frame(sub_months, pdep2)

# plots of predicted values against response (how well does the model predict?)
ggplot(dep2_data,
       aes(logCpue, pdep2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(dep2_data,
       aes(as.factor(depth_bin), pdep2)) +
  geom_boxplot()

### SUBSTRATE AND TEMP

## all data, partial July set with 5, 30 M Gills missing

# the model
sub_temp_1 <- glmmTMB(logCpue ~ perc_clay + perc_silt + temp_avg + (1|site) + (1|site:depth_bin),
                #offset = log(effort_hours),
                family = gaussian,
                data = sub_only)
summary(sub_temp_1)

#QQ plot residuals
sub_temp_1_resids <- simulateResiduals(sub_temp_1)
plot(sub_temp_1_resids)

#ANOVA results
car::Anova(sub_temp_1)

# check R2
performance::r2(sub_temp_1)

# predict
psub_temp_1 <- predict(sub_temp_1, re.form=~0, type="link")

# create data frame for plotting
sub_temp_1_data <- data.frame(sub_only, psub_temp)

# plots of predicted values against response (how well does the model predict?)
ggplot(sub_temp_1_data,
       aes(logCpue, psub_temp_1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(sub_temp_1_data,
       aes(perc_silt, psub_temp_1)) +
  geom_point() +
  geom_smooth()

ggplot(sub_temp_1_data,
       aes(perc_clay, psub_temp_1)) +
  geom_point() +
  geom_smooth()

ggplot(sub_temp_1_data,
       aes(temp_avg, psub_temp_1)) +
  geom_point() +
  geom_smooth()

## subsetted on depth (aka all months but only depths 1 & 2)

# the model
sub_temp_2 <- glmmTMB(logCpue ~ perc_clay + perc_silt + temp_avg + (1|site) + (1|site:depth_bin),
                    #offset = log(effort_hours),
                    family = gaussian,
                    data = sub_depths)
summary(sub_temp_2)

#QQ plot residuals
sub_temp_2_resids <- simulateResiduals(sub_2_temp)
plot(sub_temp_2_resids)

# ANOVA results
car::Anova(sub_temp_2)

# check R2
performance::r2(sub_temp_2)

# predict
psub_temp <- predict(sub_temp_2, re.form=~0, type="link")

# create data frame for plotting
sub_temp_2_data <- data.frame(sub_depths, psub_temp_2)

# plots of predicted values against response (how well does the model predict?)
ggplot(sub_temp_2_data,
       aes(logCpue, psub_temp_2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(sub_temp_2_data,
       aes(perc_silt, psub_temp_2)) +
  geom_point() +
  geom_smooth()

ggplot(sub_temp_2_data,
       aes(perc_clay, psub_temp_2)) +
  geom_point() +
  geom_smooth()

ggplot(sub_temp_2_data,
       aes(temp_avg, psub_temp_2)) +
  geom_point() +
  geom_smooth()

## subsetted on month (aka all depths but only months 5&6)

## I think the reason this is not significant but the predictors make sense is that
## without the last month the data is much less zero-inflated so it does not detect significance
## as readily

# the model
sub_temp_3 <- glmmTMB(logCpue ~ perc_clay + perc_silt + temp_avg + (1|site) + (1|site:depth_bin),
                    #offset = log(effort_hours),
                    family = gaussian,
                    data = sub_depths)
summary(sub_temp_3)

#QQ plot residuals
sub_temp_3_resids <- simulateResiduals(sub_temp_3)
plot(sub_temp_3_resids)

# ANOVA results
car::Anova(sub_temp_3)

# check R2
performance::r2(sub_temp_3)

# predict
psub_temp_3 <- predict(sub_temp_3, re.form=~0, type="link")

# create data frame for plotting
sub_temp_3_data <- data.frame(sub_depths, psub_temp_3)

# plots of predicted values against response (how well does the model predict?)
ggplot(sub_temp_3_data,
       aes(logCpue, psub_temp_3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(sub_temp_3_data,
       aes(perc_silt, psub_temp_3)) +
  geom_point() +
  geom_smooth()

ggplot(sub_temp_3_data,
       aes(perc_clay, psub_temp_3)) +
  geom_point() +
  geom_smooth()

ggplot(sub_temp_3_data,
       aes(temp_avg, psub_temp_3)) +
  geom_point() +
  geom_smooth()

### SUBSTRATE, TEMP & VEGETATION (june only)

## I feel like there should be a quadratic function for avg vegetation height
## but the residuals don't seem to support it

# the model
june <- glmmTMB(logCpue ~ perc_clay + perc_silt + temp_avg + avg_vegheight + (1|site) + (1|site:depth_bin),
                      family = gaussian,
                      data = sub_june)
summary(june)

#QQ plot residuals
june_resids <- simulateResiduals(june)
plot(june_resids)
plotResiduals(june_resids, form=sub_june$temp_avg)

# ANOVA results
car::Anova(june)

# check R2
performance::r2(june)
MuMIn::r.squaredGLMM(june)

# predict
pJune <- predict(june, re.form=~0, type="link")

# create data frame for plotting
june_data <- data.frame(sub_june, pJune)

# plots of predicted values against response (how well does the model predict?)
ggplot(june_data,
       aes(logCpue, pJune)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(june_data,
       aes(perc_silt, pJune)) +
  geom_point() +
  geom_smooth()

ggplot(june_data,
       aes(perc_clay, pJune)) +
  geom_point() +
  geom_smooth()

ggplot(june_data,
       aes(temp_avg, pJune)) +
  geom_point() +
  geom_smooth()

ggplot(june_data,
       aes(avg_vegheight, pJune)) +
  geom_point() +
  geom_smooth()
