### R code from vignette source 'ecoreg-guide.Rnw'

###################################################
### code chunk number 1: ecoreg-guide.Rnw:19-21
###################################################
options(width = 60)
version <- gsub("Version: +", "", help(package="ecoreg")$info[[1]][3])


###################################################
### code chunk number 2: ecoreg-guide.Rnw:26-27
###################################################
cat(version)


###################################################
### code chunk number 3: ecoreg-guide.Rnw:384-385
###################################################
library(ecoreg)


###################################################
### code chunk number 4: ecoreg-guide.Rnw:404-411
###################################################
ng <- 50
N <- rep(100, ng)
set.seed(31412) 
ctx <- cbind(deprivation = rnorm(ng), mean.income = rnorm(ng))
phi <- cbind(nonwhite = runif(ng), smoke = runif(ng))
sim.df <- as.data.frame(cbind(ctx, phi))
sim.df[1:5,]


###################################################
### code chunk number 5: ecoreg-guide.Rnw:418-426
###################################################
mu <- qlogis(0.05)
alpha.c <- log(c(1.01, 1.02))
alpha <- log(c(1.5, 2))
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, 
                data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, 
                isam=10)
sim1$y[1:5]
aggdata <- as.data.frame(cbind(y=sim1$y, N=N, sim.df))


###################################################
### code chunk number 6: ecoreg-guide.Rnw:442-443
###################################################
aggdata[1:5,]


###################################################
### code chunk number 7: ecoreg-guide.Rnw:458-459
###################################################
indivdata <- sim1$idata


###################################################
### code chunk number 8: ecoreg-guide.Rnw:468-469
###################################################
indivdata[1:15,]


###################################################
### code chunk number 9: ecoreg-guide.Rnw:495-497
###################################################
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,  data = aggdata)


###################################################
### code chunk number 10: ecoreg-guide.Rnw:529-530
###################################################
agg.eco


###################################################
### code chunk number 11: ecoreg-guide.Rnw:541-544
###################################################
aggdata.sub <- aggdata
aggdata.sub$y <- aggdata$y - tapply(indivdata$y, indivdata$group, sum)
aggdata.sub$N <- aggdata.sub$N - 10


###################################################
### code chunk number 12: ecoreg-guide.Rnw:552-557
###################################################
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,
               iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
               data = aggdata.sub, idata=sim1$idata)
agg.indiv.eco


###################################################
### code chunk number 13: ecoreg-guide.Rnw:576-586
###################################################
phi <- cbind(nonwhite = runif(ng, 0, 0.2), smoke = runif(ng, 0.1, 0.3))
sim.df <- as.data.frame(cbind(ctx, phi))
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, 
                data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, 
                isam=10)
aggdata <- as.data.frame(cbind(y=sim1$y, N=N, sim.df))
indivdata <- sim1$idata
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco


###################################################
### code chunk number 14: ecoreg-guide.Rnw:590-598
###################################################
aggdata.sub <- aggdata
aggdata.sub$y <- aggdata$y - tapply(indivdata$y, indivdata$group, sum)
aggdata.sub$N <- aggdata.sub$N - 10
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,
               iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
               data = aggdata.sub, idata=indivdata)
agg.indiv.eco


###################################################
### code chunk number 15: ecoreg-guide.Rnw:605-608
###################################################
indiv.eco <- eco(iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
               idata=indivdata)
indiv.eco


###################################################
### code chunk number 16: ecoreg-guide.Rnw:622-632
###################################################
phi <- cbind(nonwhite = runif(ng), smoke = runif(ng))
sim.df <- as.data.frame(cbind(ctx, phi))
sim.df$poll <- rnorm(ng, 1.24, 0.1)
sim.df$poll.sd <- rep(0.2, ng)
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke,
                m=sim.df["poll"], S=sim.df["poll.sd"], beta=log(2), 
                data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, 
                isam=10)
aggdata <- as.data.frame(cbind(y=sim1$y, N=N, sim.df))
aggdata[1:5,]


###################################################
### code chunk number 17: ecoreg-guide.Rnw:639-643
###################################################
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income, 
               normal= ~ poll, norm.var=poll.sd, 
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco


###################################################
### code chunk number 18: ecoreg-guide.Rnw:659-669
###################################################
sim.df$poll <- rnorm(ng, 1.24, 0.1)
sim.df$poll.sd <- rep(0.2, ng)
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke,
                m=sim.df["poll"], S=sim.df["poll.sd"], beta=log(2), 
                data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, isam=10)
aggdata <- as.data.frame(cbind(y=sim1$y, N=N, sim.df))
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income, 
               normal= ~ poll, norm.var=poll.sd, 
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco


###################################################
### code chunk number 19: ecoreg-guide.Rnw:677-690
###################################################
indivdata <- sim1$idata
indivdata[1:5,]
aggdata.sub <- aggdata
aggdata.sub$y <- aggdata$y - tapply(indivdata$y, indivdata$group, sum)
aggdata.sub$N <- aggdata.sub$N - 10
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income, 
                     normal= ~ poll, norm.var=poll.sd, 
                     binary = ~ nonwhite + smoke,  data = aggdata.sub,
                     iformula = y ~ deprivation + mean.income + 
                            nonwhite + smoke + poll, 
                     idata=indivdata
                     )
agg.indiv.eco


###################################################
### code chunk number 20: ecoreg-guide.Rnw:740-749
###################################################
aggdata$soclass <- plogis(qlogis(aggdata$smoke) + runif(ng, -1, 1))
pa <- aggdata$smoke
pb <- aggdata$soclass
p11 <- pa*pb* 2 /(1 + pb)
p10 <- pa - p11
p01 <- pb - p11
p00 <- 1 - (p01 + p10 + p11)
cross <- cbind(p00, p10, p01, p11)
cross[1:5,]


###################################################
### code chunk number 21: ecoreg-guide.Rnw:754-755
###################################################
eco(cbind(y, N) ~ 1, binary = ~ smoke + soclass, cross=cross, data=aggdata)


###################################################
### code chunk number 22: ecoreg-guide.Rnw:874-875 (eval = FALSE)
###################################################
## help(eco)


###################################################
### code chunk number 23: ecoreg-guide.Rnw:881-882 (eval = FALSE)
###################################################
## help.start()


