Schaefer <- function(par, data, verbose=FALSE)
{
  r <- exp(par[["logr"]])
  K <- exp(par[["logK"]])
  Binit <- exp(par[["logBinit"]])
  q <- exp(par[["logq"]])
  year <- data$Year
  C <- data$Catch
  I <- data$Index
  n <- length(year)
  B <- numeric(n)
  B[1] <- Binit
  for(i in 1:(n-1))
  {
    B[i+1] <- max(B[i] + r*B[i]*(1-B[i]/K) - C[i], 1)
  }
  Ifit <- q * B

  res <- log(I) - log(Ifit)
  RSS <- sum(res^2)

  pars <- c(r=r, K=K, Binit=Binit, q=q)
  refpts <- c(HRmsy=0.5*r, Bmsy=0.5*K, MSY=0.25*r*K)

  if(verbose)
    list(B=B, HR=C/B, Ifit=Ifit, res=res, pars=pars, refpts=refpts, RSS=RSS)
  else
    RSS
}

################################################################################
## South Atlantic albacore

albacore <- read.table("albacore.dat", header=TRUE)
init <- c(logr=log(0.5), logK=log(200), logBinit=log(100), logq=log(0.5))

Schaefer(par=init, albacore)
optim(init, Schaefer, data=albacore)
est <- optim(init, Schaefer, data=albacore)$par
fit <- Schaefer(est, albacore, verbose=TRUE)

par(mfrow=c(2,2))

plot(albacore$Year, fit$Ifit, ylim=c(0,90), yaxs="i", type="l", lwd=4,
     col="gray", xlab="Year", ylab="Biomass index",
     main="Albacore: Fit to data")
points(Index~Year, albacore)

plot(albacore$Year, fit$B, type="l", ylim=c(0,300), yaxs="i", lwd=2,
     xlab="Year", ylab="Biomass and catch", main="Albacore: Biomass and catch")
points(Catch~Year, albacore, type="h", lwd=6)

plot(albacore$Year, fit$HR, ylim=c(0,0.35), yaxs="i", type="l",
     lwd=2, xlab="Year", ylab="Harvest rate", main="Albacore: Harvest rate")

fit$pars
fit$refpts

################################################################################
## Georges Bank winter flounder

flounder <- read.table("flounder.dat", header=TRUE)

K.init <- 8 * mean(flounder$Catch)
B.init <- 0.5 * K.init
q.init <- flounder$Index[1] / B.init
init <- c(logr=log(0.5), logK=log(K.init),
          logBinit=log(B.init), logq=log(q.init))

Schaefer(par=init, flounder)
optim(init, Schaefer, data=flounder)
optim(init, Schaefer, data=flounder, method="Nelder-Mead",
      control=list(maxit=1e5, reltol=1e-10))
nlminb(init, Schaefer, data=flounder, control=list(eval.max=1e4, iter.max=1e4))
est <- nlminb(init, Schaefer, data=flounder,
              control=list(eval.max=1e4, iter.max=1e4))$par
fit <- Schaefer(est, flounder, verbose=TRUE)

par(mfrow=c(2,2))

plot(flounder$Year, fit$Ifit, ylim=c(0,8), yaxs="i", lwd=4, col="gray",
     type="l", xlab="Year", ylab="Biomass index", main="Flounder: Fit to data")
points(Index~Year, flounder)

plot(flounder$Year, fit$B, type="l", ylim=c(0,15), yaxs="i", lwd=2,
     xlab="Year", ylab="Biomass and catch", main="Flounder: Biomass and catch")
points(Catch~Year, flounder, type="h", lwd=6)

plot(flounder$Year, fit$HR, ylim=c(0,0.6), yaxs="i", type="l",
     lwd=2, xlab="Year", ylab="Harvest rate", main="Flounder: Harvest rate")

t(fit$pars)
t(fit$refpts)
