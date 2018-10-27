source("09_sca_function.R")

C <- as.matrix(read.table("nscod_catage.dat", header = TRUE, check.names = FALSE, row.names = 1))
I <- as.matrix(read.table("nscod_survey.dat", header = TRUE, check.names = FALSE, row.names = 1))
M <- as.matrix(read.table("nscod_natmort.dat", header = TRUE, check.names = FALSE, row.names= 1))
data <- list(C = C, I = I, M = M)

logNa <- rep(8, ncol(C))
logNt <- rep(8, nrow(C))
logFa <- rep(0, ncol(C) - 1)
logFt <- rep(0, nrow(C))
logQ <- rep(-5, ncol(I))
par <- c(logNa = logNa, logNt = logNt, logFa = logFa, logFt = logFt, logQ = logQ)

################################################################################

sca(par, data, full=TRUE)
sca(par, data)

opt1 <- optim(par, sca, data = data)
opt1

opt2 <- optim(par, sca, data = data, method = "BFGS")
opt2

opt3 <- optim(par, sca, data = data, method = "BFGS", control = list(maxit=1000))
opt3

# or we can use nlminb
# ?nlminb
opt4 <- nlminb(par, sca, data = data)
opt4

opt5 <- nlminb(par, sca, data = data, control = list(eval.max = 1000, iter.max = 1000))
opt5

# summarise fits
opt4$value <- opt4$objective
opt5$value <- opt5$objective
opts <- list(opt1 = opt1, opt2 = opt2, opt3 = opt3, opt4 = opt4, opt5 = opt5)
sapply(opts,
       function(x) {
         c(value = x$value,
           convergence = x$convergence)
       })

