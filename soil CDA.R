load("C:/Users/A00017434/Client work/Barnes Sarah/habitat_model/.RData")

library(tidyverse)
library(compositions)

with(master, table(depth_bin, month, site, useNA = "ifany"))
with(master, table(month, site, depth_bin, useNA = "ifany"))

with(master, table(depth_bin, net, useNA = "ifany"))

soilDat <- master %>%
  filter(!net_type == "HOP") %>%
  select(perc_clay, perc_rock, perc_sand, perc_silt) %>%
  mutate(soilSum = perc_clay + perc_rock + perc_sand + perc_silt)
soilDat

colSums(soilDat == 0)
summary(soilDat)

ggplot(soilDat,
       aes(x = perc_clay)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)
ggplot(soilDat,
       aes(x = perc_rock)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)
ggplot(soilDat,
       aes(x = perc_sand)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)
ggplot(soilDat,
       aes(x = perc_silt)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)

ggplot(soilDat3,
       aes(x = matureallCpue)) +
  geom_histogram(fill = "cornsilk3", color = "black", bins = 40)


# Drop perc_rock 
# Close composition
# Replace zeros
# Re-close composition

head(arrange(soilDat, perc_clay))
head(arrange(soilDat, perc_sand))
head(arrange(soilDat, perc_silt))

soilDat3 <- soilDat %>%
  select(-perc_rock, -soilSum) %>%
  clo() %>%
  zeroreplace(d = c(0.0072, 0.0043, 0.088)) %>%
  clo()

rowSums(soilDat3)  

y0 <- master %>%
  filter(!net_type == "HOP") %>%
  select(matureallCpue, matureAll_num, effort_hours)
yCpue <- y0$matureallCpue
yAll <- y0$matureAll_num
effort_hours <- y0$effort_hours
  
soilDat3 <- data.frame(soilDat3, y0)



library(colorspace)

Strawberries <- soilDat3
Strawberries

X <- acomp(Strawberries[,1:3])
X
stats::cor(ilr(X), method = "spearman") # cor = 0.352

Y <- log(Strawberries$matureallCpue + 0.08)
# should probably use poisson with offset
# still could be zero-inflated

rescale = function(xin, rout, rin=range(xin, na.rm = F)) {
  xout = rout[1] + (xin-rin[1])*(rout[2]-rout[1])/(rin[2]-rin[1])
  return(xout)
} 
(Ycex = rescale(Y, c(0.5,2)))
Ycol = grey(rescale(Y,c(0.1,0.8)))
plot(X,cex=Ycex)
plot(X,pch=19,col=Ycol)

opar <- par(mar=c(3,3,0,0))
plot(X,pch=21,bg=Ycol,cex=Ycex)
(Ylev <- pretty(Y))
# legend(x=0,y=0.97,pch=21,legend=Ylev,
#        pt.cex=rescale(Ylev,c(0.5,2),range(Y)),
#        pt.bg=grey(rescale(Ylev,c(0.1,0.8),range(Ylev))))
legend(x=0,y=0.97,pch=21,legend=Ylev,
       pt.cex=rescale(Ylev,c(0.5,2),range(Y)),
       pt.bg=grey(rescale(Ylev,c(0.1,0.9),range(Ylev)))) # to keep scale in range for grey()
par(opar)

# pairs(X)

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

ilr(X)
ilrX1 <- ilr(X)[,1]
ilrX2 <- ilr(X)[,2]
(test <- cbind(Y, X, ilrX1, ilrX2))
pairs(test)

# normal
(model = lm(Y~ilr(X)))
summary(model)
(a = coef(model)[1])
(b = ilrInv(coef(model)[-1],orig=X))
cor(ilr(X)[,1], ilr(X)[,2])

pNorm <- predict(model)

# poisson with offset
library(glmmTMB)
useData <- data.frame(yAll, X, ilrX1, ilrX2)
(model = glmmTMB(yAll~ ilrX1 + ilrX2, 
                          offset = log(effort_hours), 
                          ziformula = ~ 1,
                          data = useData,
                          family = nbinom2()))
summary(model)

pNB <- predict(model)

plot(exp(pNorm), pNB)
plot(pNorm, pNB)

(a = coef(model)[1])
(b = ilrInv(coef(model)[-1],orig=X))
cor(ilr(X)[,1], ilr(X)[,2])
DHARMa::simulateResiduals(model, plot =T)

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

Predicted <- predict(model, re.form=~0, type = "link")
Residuals <- resid(model)
plot(Predicted, Residuals)

par(mfrow=c(2,2))
plot(model, add.smooth = F)
par(mfrow=c(1,1))

s2 <- data.frame(Strawberries, Predicted, ilrX1, ilrX2)
# library(tidyverse)
ggplot(s2,
       aes(log(matureallCpue + 0.008), Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
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
       aes(ilrX1, Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
ggplot(s2,
       aes(ilrX2, Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

