source("09_sca_function.R")

## Read data

C <- as.matrix(read.table("nscod_catage.dat", header=TRUE,
                          check.names=FALSE, row.names=1))
I <- as.matrix(read.table("nscod_survey.dat", header=TRUE,
                          check.names=FALSE, row.names=1))
M <- as.matrix(read.table("nscod_natmort.dat", header=TRUE,
                          check.names=FALSE, row.names=1))
data <- list(C=C, I=I, M=M)

## Set initial parameter values

logNa <- rep(8, ncol(C))
logNt <- rep(8, nrow(C))
logFa <- rep(0, ncol(C)-1)
logFt <- rep(0, nrow(C))
logQ <- rep(-5, ncol(I))
par <- c(logNa=logNa, logNt=logNt, logFa=logFa, logFt=logFt, logQ=logQ)

################################################################################

## Fit model

sca(par, data, full=TRUE)
sca(par, data)

run <- optim(f=sca, par=par, data=data, method="BFGS",
             control=list(maxit=1000))

model <- sca(run$par, data, full=TRUE)

## View results

par(mfrow=c(2,2))

## 1 Population
round(model$N)
Year <- as.integer(rownames(model$N))
plot(apply(model$N, 2, median), ylim=c(0,400), yaxs="i", type="l", lty=3,
     main="Population in 2016 (bars) vs.\n median population (line)",
     xlab="Age", ylab="Individuals")
points(c(tail(model$N, 1)), type="h", lwd=6)

## 2 Recruitment
barplot(model$N[,1], ylab="Individuals at age 1", main="Recruitment")

## 3 Selectivity
round(model$F, 2)
plot(colMeans(model$F)/max(colMeans(model$F)), ylim=c(0,1.05), yaxs="i",
     type="l", main="Selectivity", xlab="Age", ylab="Average F at age")

## 4 Fbar
Fbar2.4 <- rowMeans(model$F[,2:4])
plot(Year[-length(Year)], Fbar2.4, ylim=c(0,1.2), yaxs="i", main="Fbar (2-4)",
     ylab="Average F at ages 2-4", type="l")
