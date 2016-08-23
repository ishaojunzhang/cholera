library(mgcv)

setwd("~/Documents/MStat")
load("Data/cholera_132_weeks.RData")


# choose k for temperal trend ---------------------------------------------
cvgp <- 10
groups <- list(1:13, 14:26, 27:39, 40:52, 53:65, 66:78, 79:91, 92:104, 105:117, 118:132)

# copy the dataset for each department
dat <- list()
for (i in 1:10) {
  dat[[i]] <- data.frame(week = 1:132, cholera = round(cholera_xls[, i]))
}

# compute minimum mspe for all departments
k <- 11:41
mspe <- rep(NA, length(k))
gam.control(epsilon = 1e-06, maxit = 1000000)
for (d in seq_along(k)) {
  s = 0
  for (gp in 2:(cvgp - 1)) {
      for (i in 1:10) {
      fit <- gam(cholera ~ s(week, bs = "cr", k = k[d]), 
                 family = "poisson",
                 data = dat[[i]][- groups[[gp]], ])
      s <- s + sum((predict(fit, list(week = groups[[gp]]), type = "response")
                    - dat[[i]]$cholera[groups[[gp]]]) ^ 2)
    }
  }
  mspe[d] <- s / 1040
}
min_k <- which.min(mspe) + 9
pdf("Images/df_temporal.pdf")
plot(k - 1, log(mspe), xlab = "K", ylab = "log MSPE", type = "l")
dev.off()

m <- rep(NA, length(k))
for (i in seq_along(k)) {
  m[i] <- log(min(mspe[1:i]))
}
pdf("Images/df_temporal2.pdf")
plot(k - 1, m, type = "l", xlab = "K", ylab = "cumulative minimum of log MSPE")
dev.off()

# fit gam separately for each department  -----------------------------------------
fit <- list()
week <- 1:132
# gam.control(epsilon = 1e-06, maxit = 1000000)
pdf("Images/gam_week.pdf", width=8, height=6)
par(mfrow=c(3, 4))
for (i in 1:10) {
  fit[[i]] <- gam(cholera_web[, i] ~ s(week, bs = "cr", k = 20), family = "poisson", method = "REML")
  plot(fit[[i]], residuals = FALSE, xlab = "week", main = names(cholera_web)[i], se = FALSE)
}
dev.off()

# overall temporal trend --------------------------------------------------
pdf("Images/overall_trend.pdf")
data_web <- data.frame(week = rep(1:132, 10), department = rep(names(cholera_web), each = 132), cholera = as.vector(as.matrix(cholera_web)))
fit_single <- gam(cholera ~ department + s(week, bs = "cr", k = 20) , family = "poisson", method = "REML", data = data_web)
plot(fit_single)
dev.off()
summary(fit_single) # 78.5%

# plot proportion of zero cases
zero_num <- rep(0, 10)
for (i in 1:10) {
  zero_num[i] <- sum(cholera_web[, i] == 0)
}
pdf("Images/bar_zero.pdf", width = 8, height = 4)
barplot(rbind(zero_num, 132 - zero_num), ylim = c(0, 140), names.arg = names (cholera_web), cex.names = 0.5, legend.text = c("zero", "nonzero"), xlim = c(0, 15))
dev.off()

# zero-inflated poisson ---------------------------------------------------
require(rje)
zero_mass <- rep(0, 10)
pdf("Images/zi_poisson.pdf",width=8,height=6)
par(mfrow=c(3, 4))
library(VGAM, warn.conflicts=FALSE)
for (i in 1:10) {
  virtual.design.matrix<-model.matrix.gam(fit[[i]])
  formula.str<-paste("formula <- cholera_web[, i] ~ virtual.design.matrix[, 2]",sep="")
  for (j in 3:20) {
    formula.str<-paste(formula.str, paste("virtual.design.matrix[," , j, "]", sep=""), sep="+")
  }
  eval(parse(text = formula.str))
  fit.zi.sim<-vglm(formula, family = zipoissonff, maxit = 100000)
  zero_mass[i] <- 1-expit(fit.zi.sim@coefficients[2])
  plot(week, scale(log(fitted(fit.zi.sim)), scale = FALSE), type = "l", xlab = "week", main = names(cholera_web)[i], ylab = "")
}
detach(package:VGAM)
dev.off()

# test whether two models are indistinguishible
library(pscl)
i <- 10 # mannully set which department to test from 2 to 10
formula.str<-paste("formula <- cholera_web[, i] ~ virtual.design.matrix[, 2]",sep="")
for (j in 3:20) {
  formula.str<-paste(formula.str, paste("virtual.design.matrix[," , j, "]", sep=""), sep="+")
}
eval(parse(text = formula.str))
glmFitP <- glm(formula, family = poisson(link = "log"), data = cholera_web)
formula.str <- paste(formula.str, "| 1") 
eval(parse(text = formula.str))
virtual.design.matrix <- model.matrix.gam(fit[[i]])
ziFitP <- zeroinfl(formula, dist = "poisson", data = cholera_web)
vuong(ziFitP, glmFitP)


# inla --------------------------------------------------------------------
library(INLA)
pdf("Images/inla_temporal.pdf", width = 8, height = 6)
par(mfrow=c(3,4))
week_duplicate <- 1:132
fit.inla.time <- list()
for (i in 1:10) {
  formula <- cholera ~ f(week, model = "rw2") + f(week_duplicate, model = "iid")
  fit.inla.time[[i]] <- inla(formula, family="poisson", data = dat[[i]])
  
  posterior.mean <- posterior.upper <- posterior.lower <- rep(0, 132)
  for (t in 1:132)
  {
    posterior.mean[t] <- inla.emarginal(function(x) x, fit.inla.time[[i]]$marginals.random$week[[t]])
    posterior.upper[t] <- inla.qmarginal(0.975, fit.inla.time[[i]]$marginals.random$week[[t]])
    posterior.lower[t] <- inla.qmarginal(0.025, fit.inla.time[[i]]$marginals.random$week[[t]])
  }
  plot(0, type="n", xlim = c(1, 132), ylim = c(min(posterior.lower), max(posterior.upper)), xlab="week", ylab="rw2", main = names(cholera_web)[i])
  
  lines(week, posterior.mean)
  # lines(week, posterior.upper,lty=2)
  # lines(week, posterior.lower,lty=2)
}
dev.off()


# Bayesian ----------------------------------------------------------------
library(rjags)
fit <- gam(cholera_web[, 1] ~ s(week, bs = "cr", k = 20), family = "poisson", method = "REML")
Z.block <- model.matrix.gam(fit) # design matrix of gam
Z.block <- Z.block[, -1]
data <- list(cholera_web = cholera_web, Z.block = Z.block, k = 19)
jags.m <- jags.model(file = "Jags/model1.jag", data = data, n.chains = 3, n.adapt = 5000)
params <- c("beta0", "u")
samps <- coda.samples(jags.m, params, n.iter = 5000)
pdf("Images/samples.pdf")
plot(samps)
dev.off()

pdf("Images/bayesian.pdf", width = 8, height = 6)
par(mfrow = c(3, 4))
for (d in 1:10) {
  y <- matrix(NA, nrow = 5000, ncol = 132)
  for (i in 1:5000) {
    beta <- samps[[1]][i, d]
    u <- samps[[1]][i, seq(10 + d, 190 + d, 10)]
    y[i, ] <- Z.block %*% u
  }
  plot(apply(y, 2, mean), type = "l", 
       xlab = "week", ylab = "", main = names(cholera_web)[d])
}
dev.off()

jags.m2 <- jags.model(file = "Jags/model2.jag", data = data, n.chains = 3, n.adapt = 5000)
params2 <- c("beta0", "u")
samps2 <- coda.samples(jags.m2, params2, n.iter = 5000)
pdf("Images/samples2.pdf")
plot(samps2)
dev.off()
