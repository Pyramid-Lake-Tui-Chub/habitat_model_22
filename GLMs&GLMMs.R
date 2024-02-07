### INSTALL PACKAGES ----
library(DHARMa)
library(glmmTMB)
library(compositions)
library(robustbase)
library(car)
library(effects)
library(MuMIn)
library(emmeans)

### DATA VISUALIAZTION ----
# look at raw distribution of data
hist(master$matureAll_num, breaks=20)
hist(master$matureallCpue, breaks=20)


### DATA SUBSETTING
# subset data just for substrate data (aka no Hoop net)
sub_only <- subset(master, perc_clay != "NA")
logCpue <- log(sub_only$matureallCpue + 0.08)
sub_only <- cbind(sub_only, ilrSC, logCpue)

# subset to July for vegetation ( a full dataset)
sub_june <- subset(sub_only, avg_vegheight != "NA" & month == 6)

### MODELS WITHOUT RANDOM EFFECTS ----
-------------------------------------------------------------------------------------------------------------------------------
### % SUBSTRATE GLM ----
options(na.action = "na.fail")

# the model
M1 <- glmmTMB(logCpue ~ perc_silt + perc_clay,
              #zi= ~1,
          family = gaussian,
          data = sub_only)
summary(M1)

# test model assumptions with package DHARMa
par(mfrow=c(1,1))

# overdispersion
testDispersion(M1)

# calculate residuals, should range from 0.25-0.75 homogeneously
# positive results are bad and highlighted in red
# indicate heteroskedasticity/outliers
simulationOutput <- simulateResiduals(fittedModel = M1, plot = T)

# predict
pM1 <- predict(M1, re.form=~0, type="link")

# create data frame for plotting
M1_data <- data.frame(sub_only, pM1)

# plots of predicted values against response (how well does the model predict?)
ggplot(M1_data,
       aes(logCpue, pM1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(M1_data,
       aes(perc_silt, pM3)) +
  geom_point() +
  geom_smooth(se = F)

### DEPTH GLM ----
# the model 
M2 <- glmmTMB(logCpue ~ as.factor(depth_bin),
              #zi= ~1,
          family = gaussian,
          data = sub_only)
summary(M2)

#use package DHARMa to test for overdispersion, takes a simulation approach
testDispersion(M2)

#QQ plot residuals
M2_resids <- simulateResiduals(M2)
plot(M2_resids)

# predict
pM2 <- predict(M2, re.form=~0, type="link")

# create data frame for plotting
M2_data <- data.frame(sub_only, pM2)

# plots of predicted values against response (how well does the model predict?)
ggplot(M2_data,
       aes(logCpue, pM2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(M2_data,
       aes(as.factor(depth_bin), pM2)) +
  geom_boxplot()

### SUBSTRATE AND DEPTH GLM, ALL AVAILABLE DATA ----

# the model
M3 <- glmmTMB(matureAll_num ~ perc_clay + perc_silt + as.factor(depth_bin), 
              offset = log(effort_hours),
              #zi=~1,
              family = nbinom2,
              data = sub_only)
summary(M3)

M3_resids <- simulateResiduals(M3)
plot(M3_resids)

#use package DHARMa for model checking
par(mfrow= c(1,1))
testDispersion(M3)

# predict
pM3 <- predict(M3, re.form=~0, type="link")

# create data frame for plotting
M3_data <- data.frame(sub_only, pM3)

# plots of predicted values against response (how well does the model predict?)
ggplot(M3_data,
       aes(matureallCpue, pM3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(M3_data,
       aes(as.factor(depth_bin), pM3)) +
  geom_boxplot()

ggplot(s2,
       aes(ilrSC, pM3)) +
  geom_point() +
  geom_smooth(se = F)

### SUBSTRATE, PLANT COVER, TEMP GLM, JUNE ONLY (aka the only full dataset) ----

# the model
M4 <- glmmTMB(matureAll_num ~ perc_clay + perc_silt + avg_vegheight + temp_avg,
              offset = log(effort_hours),
              #zi=~1,
              family = nbinom2,
              data=sub_june)
summary(M4)

#use package DHARMa for model checking
par(mfrow= c(1,1))
testDispersion(M4)

#QQ plot residuals
M4_resids <- simulateResiduals(M4)
plot(M4_resids)
plotResiduals(M4_resids, form=sub_june$perc_silt)
plotResiduals(M4_resids, form=sub_june$avg_vegheight)

# model checking with ANOVA
car::Anova(M4)

# predict
pM4 <- predict(M4, re.form=~0, type="link")

# create data frame for plotting
M4_data <- data.frame(sub_june, pM4)

# plots of predicted values against response (how well does the model predict?)
ggplot(M4_data,
       aes(logCpue, pM4)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(M4_data,
       aes(as.factor(depth_bin), pM4)) +
  geom_boxplot()

ggplot(M4_data,
       aes(avg_vegheight, pM4)) +
  geom_point() +
  geom_smooth(se = T)

### SUBSTRATE & DEPTH GLM, JUNE ONLY ---- 

# the model
M5 <- glmmTMB(logCpue ~ perc_clay + perc_silt + as.factor(depth_bin),
              #zi= ~1,
              family = gaussian,
              data=sub_june)
summary(M5)

#use package DHARMa for model checking
par(mfrow= c(1,1))
testDispersion(M5)

#QQ plot residuals
M5_resids <- simulateResiduals(M5)
plot(M5_resids)

# predict
pM5 <- predict(M5, re.form=~0, type="link")

# create data frame for plotting
M5_data <- data.frame(sub_june, pM5)

# plots of predicted values against response (how well does the model predict?)
ggplot(M5_data,
       aes(matureallCpue, pM5)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each predictor
ggplot(M5_data,
       aes(as.factor(depth_bin), pM5)) +
  geom_boxplot()

ggplot(M5_data,
       aes(ilrSC, pM5)) +
  geom_point() +
  geom_smooth(se = F)
------------------------------------------------------------------------------------------------
#### MODELS WITH RANDOM EFFECTS, MOSTLY JUST SEE FINAL SCRIPT, THIS IS PRACTICE

# substrate w/ random effect for site
M6 <- glmmTMB(matureAll_num ~ perc_clay + perc_silt + (1|site), 
                offset = log(effort_hours),
                #zi=~1,
                family = nbinom2,
                data = sub_only)
summary(M6)

M6_resids <- simulateResiduals(M6)
plot(M6_resids)

# substrate w/ random effect for site and location within site
M7 <- glmmTMB(matureAll_num ~ perc_clay + perc_silt + (1|site) + (1|site:depth_bin), 
                offset = log(effort_hours),
                #zi=~1,
                family = nbinom2,
                data = sub_only)
summary(M7)

M7_resids <- simulateResiduals(M7)
plot(M7_resids)

car::Anova(M7)
emmeans(M7, pairwise~perc_clay)
emmeans(M7, ~perc_clay, type="response")
