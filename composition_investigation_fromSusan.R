## INSTAL PACKAGES ----
library(tidyverse)
library(compositions)
library(colorspace)
library(glmmTMB)

## INVESTIGATE THE DATA STRUCTURE ----

# distribution of depth bins by month for each site 
with(master, table(depth_bin, month, site, useNA = "ifany"))

# distribution of sets by month and site (different way to look at the same thing)
with(master, table(month, site, depth_bin, useNA = "ifany"))

# number of nets per depth bin
with(master, table(depth_bin, net, useNA = "ifany"))

# with hoop nets removed, look at the just the compositions of substrate and check if they sum to 1
soilDat <- master %>%
  filter(!net_type == "HOP") %>%
  select(perc_clay, perc_rock, perc_sand, perc_silt) %>%
  mutate(soilSum = perc_clay + perc_rock + perc_sand + perc_silt)
soilDat

# how many zeros in each column?
colSums(soilDat == 0)

# summary stats for this data
summary(soilDat)

# histogram of percent clay distribution
ggplot(soilDat,
       aes(x = perc_clay)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)

# histogram of percent rock distribution
ggplot(soilDat,
       aes(x = perc_rock)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)

# histogram of percent sand distribution
ggplot(soilDat,
       aes(x = perc_sand)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)

# histogram of percent silt distribution
ggplot(soilDat,
       aes(x = perc_silt)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)

## SEQUENCE TO DROP ONE SUBSTRATE AND RE-CLOSE THE DATA aka sensitivity analyses ----

# Order data in ascending order by clay to determine 
# "what is the minimum detection limit for percent clay?" 
# (using lowest detected point as a proxy)
head(arrange(soilDat, perc_clay))
#0.0072

# same with sand
head(arrange(soilDat, perc_sand))
#0.0043

# and silt
head(arrange(soilDat, perc_silt))
#0.0882

# and rock, so many 0s!
rock_0 <- subset(soilDat, perc_rock != 0)
head(arrange(rock_0, perc_rock))
#0.0047

# Drop substrate 
# Close composition
# Replace zeros with the lowest detected point
# Re-close composition
soilDat3 <- soilDat %>%
  select(-perc_rock, -soilSum) %>%
  clo() %>%
  zeroreplace(d = c(0.0072*2/3, 0.0043* 2/3, 0.0882*2/3)) %>%
  clo()

# check that the compositions sum to 1 now
rowSums(soilDat3)  

# create the response variable
y0 <- master %>%
  filter(!net_type == "HOP") %>%
  select(matureallCpue, matureAll_num, effort_hours)

# histogram of the response variable (cpue here)
ggplot(y0,
       aes(x = matureallCpue)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)

# create variable for all possible responses
yCpue <- y0$matureallCpue
yAll <- y0$matureAll_num
effort_hours <- y0$effort_hours

# data frame with composition data and responses  
soilDat3 <- data.frame(soilDat3, y0)

# transform the composition into relative geometry aka the Aitchison Simplex
X <- acomp(soilDat3[,1:3])
X

# check the correlation of the isometric log-ratio transformed acomp data
# what is ilr? who knows, but basically the data is transformed from proportion into vector
# I think the correlation shows the correlation between the different vectors
stats::cor(ilr(X), method = "spearman") 

# adjusted log-tranformation for the response data, use a small number to not make large over-dispersion problem
# although you should probably use poisson with offset
# still could be zero-inflated
Y <- log(soilDat3$matureallCpue + 0.08)

# function to set colors for the plots
rescale = function(xin, rout, rin=range(xin, na.rm = F)) {
  xout = rout[1] + (xin-rin[1])*(rout[2]-rout[1])/(rin[2]-rin[1])
  return(xout)
} 
(Ycex = rescale(Y, c(0.5,2)))
Ycol = grey(rescale(Y,c(0.1,0.8)))

#plot with scaled colors
plot(X,cex=Ycex)
plot(X,pch=19,col=Ycol)

# add sizes to indicate magnitude of CPUE
opar <- par(mar=c(3,3,0,0))
plot(X,pch=21,bg=Ycol,cex=Ycex)
(Ylev <- pretty(Y))

# add a legend, negatives because it's on the log scale
legend(x=0,y=0.97,pch=21,legend=Ylev,
       pt.cex=rescale(Ylev,c(0.5,2),range(Y)),
       pt.bg=grey(rescale(Ylev,c(0.1,0.9),range(Ylev)))) # to keep scale in range for grey()
par(opar)

# paired scatterplots for all of the pairwise log-ratios for each combination of substrate against response
# response variable on the Y axis
opar <- par(mfrow=c(3,3),
            mar=c(2,2,0.5,0.5),
            oma=c(3,3,0,0))
for(i in 1:3){
  for(j in 1:3){
    plot(log(X[,i]/X[,j]),Y,pch=ifelse(i!=j,19,""))
    if(i==j){text(x=0,y=mean(range(Y)),
                  labels=colnames(X)[i], cex=1.5)}
  }
}
mtext(text=c("pairwise log-ratio","dependent variable"),
      side=c(1,2),at=0.5,line=2,outer=T)
par(opar)

# ILR for the dataset, X1 is one pairwise comparison and X2 is the other? Shouldn't there by 3?
ilr(X)
ilrX1 <- ilr(X)[,1]
ilrX2 <- ilr(X)[,2]
#ilrSC <- ilr(X)[,2], for modelling beyond
(test <- cbind(Y, X, ilrX1, ilrX2))

# scatterplot matrix of acomp substrates, ilrs and response
pairs(test)

# normal model (log-transformed)
(model = lm(Y~ilr(X)))
summary(model)

# not entirely sure what these coefficients mean? They might be intercepts? They sum to 1
(a = coef(model)[1])
(b = ilrInv(coef(model)[-1],orig=X))
barplot(b, las=2,xlim=c(0,11))
cor(ilr(X)[,1], ilr(X)[,2])

pNorm <- predict(model)

# lighter grey curved line is less CPUE, black is high CPUE
par(mfrow=c(1,1))
plot(X)
straight(mean(X),b,lwd=2,col="black",lty=2)
myY = pretty(Y)
refX = mean(X) + ((myY - a)/norm(b)^2)*b
plot(refX, add= T, pch = 19)

orthoComp = function(x){
  ilrInv(ilr(x) %*% matrix(c(0,1,-1,0), ncol=2))
}
mygreyscale = grey((0:nrow(refX))/nrow(refX))
for(i in 1:nrow(refX)) {
  straight(acomp(refX[i,]), orthoComp(b),
           col = mygreyscale[i], lwd=2)
}

# check model assumptions
Predicted <- predict(model)
Residuals <- resid(model)
plot(Predicted, Residuals)

par(mfrow=c(2,2))
plot(model, add.smooth = F)
par(mfrow=c(1,1))

s2 <- data.frame(soilDat3, Predicted, ilrX1, ilrX2)

# negative binomial and zero inflation (all fixed effects) with offset
useData <- data.frame(yAll, X, ilrX1, ilrX2)
(model2 = glmmTMB(yAll~ ilrX1 + ilrX2, 
                          offset = log(effort_hours), 
                          ziformula = ~ 1,
                          data = useData,
                          family = nbinom2()))
summary(model2)

# predict for nbinom model, re.form specifies to set all random effects to zero, type = link means conditional mean
# on the scale of the link function, or the linear predictor of the conditional model
pNB <- predict(model2, re.form=~0, type="link")

# see how normal and nbinom models go together? DONT UNDERSTAND THIS
plot(exp(pNorm), pNB)
plot(pNorm, pNB)

# might not work with nbinom?
(a = coef(model)[1])
(b = ilrInv(coef(model2)[-1],orig=X))

#check model assumptions
# pretty bangin'
DHARMa::simulateResiduals(model2, plot =T)

# create data frame for plotting
s3 <- data.frame(soilDat3, pNB, ilrX1, ilrX2)

## NORMAL MODEL PREDICTED PLOTS ----
# plots of predicted values against response (how well does the model predict?)
ggplot(s2,
       aes(log(matureallCpue + 0.008), Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each used substrate category
ggplot(s2,
       aes(perc_clay, Predicted)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(s2,
       aes(perc_sand, Predicted)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(s2,
       aes(perc_silt, Predicted)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(s2,
       aes(perc_rock, Predicted)) +
  geom_point() +
  geom_smooth(se = F)

# plots of predicted against the log-ratio variables
# ilr 1
ggplot(s2,
       aes(ilrX1, Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# ilr 2
ggplot(s2,
       aes(ilrX2, Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

## NBINOM MODEL PREDICTED PLOTS ----

# plots of predicted values against response (how well does the model predict?)
ggplot(s3,
       aes(matureAll_num, pNB)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# plots of predicted values against each used substrate category
ggplot(s3,
       aes(perc_clay, pNB)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(s3,
       aes(perc_rock, pNB)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(s3,
       aes(perc_silt, pNB)) +
  geom_point() +
  geom_smooth(se = F)

# plots of predicted against the log-ratio variables
ggplot(s3,
       aes(ilrX1, pNB)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# freaking bangin'!
ggplot(s3,
       aes(ilrX2, pNB)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

