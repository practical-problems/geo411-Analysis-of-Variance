# Geography 411 – Homework 2
# Name: Steven Shi
# Description: Analysis of variance 

#GEO 411
#Script for Homework #2

###Read Data
precipSample <- read.csv("precipSample.csv")
precipSample

###Create Time Period Variable
precipSample$Period[(precipSample$Year <= 1967)] <- "Period1" 
precipSample$Period[(precipSample$Year > 1967 & precipSample$Year < 1997)] <- "Period2"
precipSample$Period[(precipSample$Year >= 1997)] <- "Period3"
precipSample

table(precipSample$Period)
n1 <- sum(precipSample$Period == "Period1")
n2 <- sum(precipSample$Period == "Period2")
n3 <- sum(precipSample$Period == "Period3")
n <- length(precipSample$Buffalo)

k <- 3


###Buffalo Analyses

##ANOVA Section

##Using R to calculate means
mean1 <- mean(precipSample$Buffalo[precipSample$Period == "Period1"])
mean2 <- mean(precipSample$Buffalo[precipSample$Period == "Period2"])
mean3 <- mean(precipSample$Buffalo[precipSample$Period == "Period3"])

print("sample means")
print(paste("Overall",mean(precipSample$Buffalo)))
print(paste("Period1",mean1))
print(paste("Period2",mean2))
print(paste("Period3",mean3))

###Visualizing ANOVA

boxplot(Buffalo ~ Period, data = precipSample, ylab = "annual precipitation (in.)")

layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$Buffalo),max(precipSample$Buffalo)+1)
BuffaloPrecipBreaks <- seq(22,54,2)
hist(precipSample$Buffalo[precipSample$Period == "Period1"], breaks = BuffaloPrecipBreaks, xlim = minMax, ylim = c(0,5), main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
hist(precipSample$Buffalo[precipSample$Period == "Period2"], breaks = BuffaloPrecipBreaks, xlim = minMax, ylim = c(0,5), main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
hist(precipSample$Buffalo[precipSample$Period == "Period3"], breaks = BuffaloPrecipBreaks, xlim = minMax, ylim = c(0,5), main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
layout(matrix(c(1)))

###Doing ANOVA the long way

###Total Sum of Squares

#The "easy" way
s2 <- var(precipSample$Buffalo)
xBar <- mean(precipSample$Buffalo)
TSS <- (n - 1)*s2
TSS

#The hard way 
TSS2 <- 0
for (i in 1:n){
	TSS2 <- TSS2 + (precipSample$Buffalo[i] - xBar)^2
	}
TSS2	

###Between Sum of Squares
xBar.js <- c(mean1, mean2, mean3)
n.js <- c(n1, n2, n3)

BSS <- 0
for (j in 1:k){
	BSS <- BSS + n.js[j]*(xBar.js[j] - xBar)^2
	}
BSS

###Within Sum of Squares

#The "easy" way
s2.js <- c(var(precipSample$Buffalo[precipSample$Period == "Period1"]), 
		var(precipSample$Buffalo[precipSample$Period == "Period2"]),
		var(precipSample$Buffalo[precipSample$Period == "Period3"]))

WSS <- 0
for (j in 1:k){
	WSS <- WSS + (n.js[j] - 1)*s2.js[j]
	}
WSS

#The hard way - double loop
WSS2 <- 0
for (j in 1:k){
	periodPrecip <- subset(precipSample, Period == paste("Period",j,sep = ""))
	for (i in 1:n.js[j]){
		WSS2 <- WSS2 + (periodPrecip$Buffalo[i] - xBar.js[j])^2
		}
	}
WSS2

F <- (BSS/(k-1))/(WSS/(n-k))
F

#Find the critical value (alpha = 0.05, df = k-1, n-k)
Fcrit <- qf(0.05, (k-1), (n-k), lower.tail = FALSE)
Fcrit

#The Quick Way to do ANOVA
modelBuffalo <- lm(Buffalo ~ as.factor(Period), data = precipSample)
anova(modelBuffalo)

###Levene Test
precipSample$BuffaloAbsoluteDeviations <- abs(resid(modelBuffalo))
precipSample
boxplot(precipSample$BuffaloAbsoluteDeviations ~ Period, data = precipSample)
anova(lm(precipSample$BuffaloAbsoluteDeviations ~ as.factor(precipSample$Period)))

###Normality Plots
layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$Buffalo),max(precipSample$Buffalo)+1)
BuffaloPrecipBreaks <- seq(22,54,2)
hist(precipSample$Buffalo[precipSample$Period == "Period1"], freq = FALSE, breaks = BuffaloPrecipBreaks, xlim = minMax, main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period1"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period1"])), add=TRUE)
hist(precipSample$Buffalo[precipSample$Period == "Period2"], freq = FALSE, breaks = BuffaloPrecipBreaks, xlim = minMax, main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period2"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period2"])), add=TRUE)
hist(precipSample$Buffalo[precipSample$Period == "Period3"], freq = FALSE, breaks = BuffaloPrecipBreaks, xlim = minMax, main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period3"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period3"])), add=TRUE)
layout(matrix(c(1)))

### K-S test
#Period 1
period1ECDF <- ecdf(precipSample$Buffalo[precipSample$Period == "Period1"])
plot(period1ECDF)
curve(pnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period1"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period1"])), add=TRUE)
ks.test(precipSample$Buffalo[precipSample$Period == "Period1"], "pnorm", mean(precipSample$Buffalo[precipSample$Period == "Period1"]), sd(precipSample$Buffalo[precipSample$Period == "Period1"]))

#Period 2
period2ECDF <- ecdf(precipSample$Buffalo[precipSample$Period == "Period2"])
plot(period2ECDF)
curve(pnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period2"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period2"])), add=TRUE)
ks.test(precipSample$Buffalo[precipSample$Period == "Period2"], "pnorm", mean(precipSample$Buffalo[precipSample$Period == "Period2"]), sd(precipSample$Buffalo[precipSample$Period == "Period2"]))

#Period 3
period3ECDF <- ecdf(precipSample$Buffalo[precipSample$Period == "Period3"])
plot(period3ECDF)
curve(pnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period3"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period3"])), add=TRUE)
ks.test(precipSample$Buffalo[precipSample$Period == "Period3"], "pnorm", mean(precipSample$Buffalo[precipSample$Period == "Period3"]), sd(precipSample$Buffalo[precipSample$Period == "Period3"]))

###Kruskal-Wallis 
kruskalWallis <- kruskal.test(Buffalo ~ as.factor(Period), data = precipSample)
kruskalWallis

qchisq(0.05, k-1, lower.tail = FALSE)

###Median Test
medianPrecipBuffalo <- median(precipSample$Buffalo)
medianPrecipBuffalo

boxplot(Buffalo ~ Period, data = precipSample, ylab = "annual precipitation (in.)")
abline(h = medianPrecipBuffalo, lwd = 2)

precipSample$BuffaloMedian <- ifelse(precipSample$Buffalo > medianPrecipBuffalo, "Greater than median", "Less than or equal")
precipSample
medianTable <- table(precipSample$BuffaloMedian, precipSample$Period)
medianTable
chisq <- chisq.test(medianTable)
chisq$expected
chisq





###San Diego Analyses

##ANOVA Section

##Using R to calculate means
mean1 <- mean(precipSample$SanDiego[precipSample$Period == "Period1"])
mean2 <- mean(precipSample$SanDiego[precipSample$Period == "Period2"])
mean3 <- mean(precipSample$SanDiego[precipSample$Period == "Period3"])

print("sample means")
print(paste("Overall",mean(precipSample$SanDiego)))
print(paste("Period1",mean1))
print(paste("Period2",mean2))
print(paste("Period3",mean3))

###Visualizing ANOVA

boxplot(SanDiego ~ Period, data = precipSample, ylab = "annual precipitation (in.)")

layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$SanDiego),max(precipSample$SanDiego))
SanDiegoPrecipBreaks <- seq(3,25,2)
hist(precipSample$SanDiego[precipSample$Period == "Period1"], breaks = SanDiegoPrecipBreaks, xlim = minMax, ylim = c(0,6), main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
hist(precipSample$SanDiego[precipSample$Period == "Period2"], breaks = SanDiegoPrecipBreaks, xlim = minMax, ylim = c(0,6), main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
hist(precipSample$SanDiego[precipSample$Period == "Period3"], breaks = SanDiegoPrecipBreaks, xlim = minMax, ylim = c(0,6), main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
layout(matrix(c(1)))

#Find the critical value (alpha = 0.05, df = k-1, n-k)
Fcrit <- qf(0.05, (k-1), (n-k), lower.tail = FALSE)
Fcrit

#The Quick Way to do ANOVA
modelSanDiego <- lm(SanDiego ~ as.factor(Period), data = precipSample)
anova(modelSanDiego)


###Levene Test
precipSample$SanDiegoAbsoluteDeviations <- abs(resid(modelSanDiego))
precipSample
boxplot(precipSample$SanDiegoAbsoluteDeviations ~ Period, data = precipSample)
anova(lm(precipSample$SanDiegoAbsoluteDeviations ~ as.factor(precipSample$Period)))

###Normality Plots
layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$SanDiego),max(precipSample$SanDiego))
SanDiegoPrecipBreaks <- seq(3,25,2)
hist(precipSample$SanDiego[precipSample$Period == "Period1"], freq = FALSE, breaks = SanDiegoPrecipBreaks, xlim = minMax, main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period1"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period1"])), add=TRUE)
hist(precipSample$SanDiego[precipSample$Period == "Period2"], freq = FALSE, breaks = SanDiegoPrecipBreaks, xlim = minMax, main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period2"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period2"])), add=TRUE)
hist(precipSample$SanDiego[precipSample$Period == "Period3"], freq = FALSE, breaks = SanDiegoPrecipBreaks, xlim = minMax, main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period3"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period3"])), add=TRUE)
layout(matrix(c(1)))

### K-S test
#Period 1
period1ECDF <- ecdf(precipSample$SanDiego[precipSample$Period == "Period1"])
plot(period1ECDF)
curve(pnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period1"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period1"])), add=TRUE)
ks.test(precipSample$SanDiego[precipSample$Period == "Period1"], "pnorm", mean(precipSample$SanDiego[precipSample$Period == "Period1"]), sd(precipSample$SanDiego[precipSample$Period == "Period1"]))

#Period 2
period2ECDF <- ecdf(precipSample$SanDiego[precipSample$Period == "Period2"])
plot(period2ECDF)
curve(pnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period2"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period2"])), add=TRUE)
ks.test(precipSample$SanDiego[precipSample$Period == "Period2"], "pnorm", mean(precipSample$SanDiego[precipSample$Period == "Period2"]), sd(precipSample$SanDiego[precipSample$Period == "Period2"]))

#Period 3
period3ECDF <- ecdf(precipSample$SanDiego[precipSample$Period == "Period3"])
plot(period3ECDF)
curve(pnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period3"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period3"])), add=TRUE)
ks.test(precipSample$SanDiego[precipSample$Period == "Period3"], "pnorm", mean(precipSample$SanDiego[precipSample$Period == "Period3"]), sd(precipSample$SanDiego[precipSample$Period == "Period3"]))

###Kruskal-Wallis 
kruskalWallis <- kruskal.test(SanDiego ~ as.factor(Period), data = precipSample)
kruskalWallis

qchisq(0.05, k-1, lower.tail = FALSE)

###Median Test
medianPrecipSanDiego <- median(precipSample$SanDiego)
medianPrecipSanDiego

boxplot(SanDiego ~ Period, data = precipSample, ylab = "annual precipitation (in.)")
abline(h = medianPrecipSanDiego, lwd = 2)

precipSample$SanDiegoMedian <- ifelse(precipSample$SanDiego > medianPrecipSanDiego, "Greater than median", "Less than or equal")
precipSample
medianTable <- table(precipSample$SanDiegoMedian, precipSample$Period)
medianTable
chisq <- chisq.test(medianTable)
chisq$expected
chisq


#Remember to upload your work to GitHub 


