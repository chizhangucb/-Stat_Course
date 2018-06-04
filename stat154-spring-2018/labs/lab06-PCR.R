## 2) Principal Components Regression (PCR)
library(ISLR)
library(pls)
str(Hitters, vec.len = 1)
Hitters <- na.omit(Hitters)
Hitters_mat <- model.matrix(Salary ~ ., Hitters)
X <- scale(Hitters_mat[, -1], center = T, scale = T)
Y <- Hitters$Salary

# 2.1) Start with PCA
# principal component regression (without CV)
pcr_fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "none") 
names(pcr_fit)
scores <- matrix(scores(pcr_fit), nrow = NROW(X), ncol = NCOL(X))
svd_Hitters <- svd(X)
svd_Z <- X %*% svd_Hitters$v 

# 2.2) PC Regression on the first component
z1 <- svd_Z[, 1]
b1 <- as.numeric(solve(t(z1) %*% z1) %*% t(z1) %*% Y)
yhat_PC1 <- as.vector(b1 * z1) + mean(Y)
fitted_pcr_PC1 <- as.vector(pcr_fit$fitted.values[ , , 1])
all.equal(yhat_PC1, fitted_pcr_PC1)
# The fitted response using PC1 provided by pcr() is the same as 
# the result using svd() and some linear algebra

# 2.3) PC Regression on all PCs
bPCR <- solve(t(svd_Z) %*% svd_Z) %*% t(svd_Z) %*% Y
yhat_full <- as.numeric(svd_Z %*% bPCR + mean(Y))
fitted_pcr_full <- as.numeric(pcr_fit$fitted.values[ , , 19])
all.equal(yhat_full, fitted_pcr_full)

# 2.4) PCR coefficients in terms of the predictor variables
# i.e., verify if Vt %*% betaOLS = betaPCR

# First PC
b1_star <- b1 * svd_Hitters$v[, 1]
coef_pcr_PC1 <- as.numeric(pcr_fit$coefficients[ , , 1])
all.equal(b1_star, coef_pcr_PC1)

# First and second PCs
b12_star <- as.numeric(svd_Hitters$v[, 1:2] %*% solve(diag((svd_Hitters$d[1:2]))) 
                       %*% t(svd_Hitters$u[, 1:2]) %*% Y)
coef_pcr_PC12 <- as.numeric(pcr_fit$coefficients[ , , 2])
all.equal(b12_star, coef_pcr_PC12)

# Alternative (sum them up)
z2 <- svd_Z[, 2]
b2 <- as.numeric(solve(t(z2) %*% z2) %*% t(z2) %*% Y)
b2_star <- b2 * svd_Hitters$v[, 2]
all.equal(b12_star, b1_star + b2_star )

# All possible sets of PCs
for (i in 2:19) {
  b_star <- as.numeric(svd_Hitters$v[, 1:i] %*% solve(diag((svd_Hitters$d[1:i]))) 
                       %*% t(svd_Hitters$u[, 1:i]) %*% Y)
  coef_pcr <- as.numeric(pcr_fit$coefficients[ , , i])
  print(paste0("when using ", i, " PCs, the comparison is ", 
               all.equal(b_star, coef_pcr)))
}

# bOLS <- solve(t(X) %*% X) %*% t(X) %*% Y
# all.equal(t(svd_Hitters$v) %*% bOLS, bPCR)
