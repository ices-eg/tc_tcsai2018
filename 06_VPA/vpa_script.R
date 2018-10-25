source("vpa_function.R")

## Read catch at age, change units
catage <- read.table("bluefin.dat", header=TRUE, check.names=FALSE, row.names=1)
catage <- catage / 1000

## Run model
model <- vpa(catage, M=0.14, Fterm=0.1, Fages=5)

## View results
par(mfrow=c(2,2))

## 1 Population
round(model$N)
Year <- as.integer(rownames(model$N))
plot(apply(model$N, 2, median), ylim=c(0,120), yaxs="i", type="l", lty=3,
     main="Population in 2013 (bars) vs.\n median population (line)",
     xlab="Age", ylab="Individuals")
points(c(tail(model$N, 1)), type="h", lwd=6)

## 2 Recruitment
barplot(model$N[,1], ylab="Individuals at age 1", main="Recruitment")

## 3 Selectivity
round(model$F, 2)
plot(colMeans(model$F)/max(colMeans(model$F)), ylim=c(0,1.05), yaxs="i",
     type="l", main="Selectivity", xlab="Age", ylab="Average F at age")

## 4 Fbar
Fbar2.5 <- rowMeans(model$F[,2:5])
plot(Year, Fbar2.5, ylim=c(0,0.35), yaxs="i", main="Fbar (2-5)",
     ylab="Average F at ages 2-5", type="l")
