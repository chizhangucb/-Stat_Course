dataset <- read.csv('../data/nba-teams-2017.csv', stringsAsFactors = F)
str(dataset, vec.len = 1)

######################################################
########### I. Transformation of Variables ###########
######################################################
ActVars <- c("wins", "losses", "points", "field_goals",
             "assists", "turnovers", "steals", "blocks")
dat <- dataset[, ActVars]
summary(dat)
boxplot(dat)
pairs(dat)

########## 0. PCA with prcomp()
# In order to perform PCA on standardized data, use the argument scale. = TRUE.
# PCA with prcomp()
pca_prcomp <- prcomp(dat, scale. = TRUE)
# what's in a prcomp object?
names(pca_prcomp)

# stdevs <- apply(dat, 2, sd)
# datSD <- scale(dat, center = T, scale = stdevs)
datSD <- scale(dat, center = T, scale = T)
boxplot(datSD)

########## 1. PCA via EVD
R_datSD <- 1 / (NROW(datSD) - 1) * t(datSD) %*% datSD
# Note: If X is mean-centered, 1/n * t(X) %*% X is a covariance matrix 
# Here, if X is standarized, 1/n * t(X) %*% X is a correlation matrix, so 
# R_datSD <- cov(datSD)
EVD_R_datSD <- eigen(R_datSD)

#### 1.1 Eigenvalues
# whether this lambda equals to square of d
(pca_prcomp$sdev)^2
all.equal(EVD_R_datSD$values, (pca_prcomp$sdev)^2)  # True

# the sum of eigenvalues equals the number of variables
round(sum(EVD_R_datSD$values)) == ncol(R_datSD)

# Compute the prop of of variation captured by each PC (i.e., lambda / p)
round(EVD_R_datSD$values / ncol(R_datSD), digits = 4)
# The first PC captures 46% of variation, the second 20%, etc

########## 2. How many PCs to retain?
## Criteria 1. Screeplot (see if there’s an “elbow”)
eigenvalues <- round(EVD_R_datSD$values, digits = 2)
proportion <- round(EVD_R_datSD$values / ncol(R_datSD), digits = 4) * 100
cum_prop <- cumsum(proportion)
# Table of Eigenvalues
Eigval.Tb <- cbind(eigenvalues, proportion, cum_prop)
rownames(Eigval.Tb) <- paste0("comp", 1:8)
plot(x = 1:8, y = eigenvalues, xlab = "number of components", 
     ylab = "values", type = "b")

## Criteria 2. Kaiser’s Rule
# i.e., retain those PCs with eigenvalues λk > 1
Eigval.Tb[eigenvalues > 1, ]

## Criteria 3. Jollife’s Rule
# i.e., retain those PCs with eigenvalues λk > 0.7
Eigval.Tb[eigenvalues > 0.7, ]

######################################################
############ II. Studying the Individuals ############
######################################################

########## 1. Eigenvectors
# We can construct a PCA matrix Z = XV
Z_datSD <- datSD %*% EVD_R_datSD$vectors
colnames(Z_datSD) <- ActVars
head(Z_datSD, 10)
head(datSD, 10)  # compare

# Loadings in PCA: eigenvectors of R_datSD
# E.g., the first element in the first loading v1 reflects 
# 41.19% of the variable X1 will be loaded in the first PC
EVD_R_datSD$vectors[ ,1]

# whether the loadings from eigen() equal to loadings from prcomp()?
EVD_R_datSD$vectors
pca_prcomp$rotation
# Actually opposite, why??
# https://stats.stackexchange.com/questions/233869/why-do-prcomp-
# and-eigencov-in-r-return-different-signs-of-pca-eigenvectors?
# utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

# Because prcomp() uses singular value decomposition at its core, rather than 
# eigendecomposition of the variance-covariance matrix. In general, the computed
# signs of eigenvectors can be very SENSITIVE to small computational differences.
# The moral is, don't rely on the signs of eigenvectors!

########## 2. PCA via SVD
SVD_R_datSD <- svd(datSD)

# whether the square of this d equals to eigenvalues of XtX
SVD_R_datSD$d
XtX <- t(datSD) %*% datSD
all.equal((SVD_R_datSD$d)^2, eigen(XtX)$value)  # True

# whether this rotation matrix equals to the rotation in prcomp()
SVD_R_datSD$v
pca_prcomp$rotation
all(sapply(1:ncol(dat), function(i) {
  all.equal(as.vector(SVD_R_datSD$v[, i]), as.vector(pca_prcomp$rotation[, i]))
}))  # True

########## 3. Contribution of Individuals
library("FactoMineR")
res.pca <- PCA(dat, ncp = 8, graph = FALSE)
print(res.pca)
Var.ctr <- res.pca$var$contrib
Ind <- res.pca$ind
rownames(Ind$contrib) <- dataset$team
colnames(Ind$contrib) <- paste0("PC", 1:NCOL(Ind$contrib))
head(Ind$contrib[, 1:4], n = 6)
# Warriors has a large contribution to PC1, and Raptors has a value close to zero on PC1.

######################################################
###### III. Alternative Way to Visualize PCA #########
######################################################
# http://www.sthda.com/english/articles/31-principal-component-methods-
# in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# install.packages("factoextra")
library(factoextra)
library("FactoMineR")
rownames(dat) <- dataset$team
res.pca <- PCA(dat, ncp = 8, graph = FALSE)
Ind <- res.pca$ind

# 1. Circle of correlations
fviz_pca_var(res.pca, col.var = "black")

# 2. Contributions of individuals to PCs
library("corrplot")
corrplot(Ind$contrib, is.corr=FALSE)   

# 3. Variable Correlation and Representativeness 
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping (slow if many points)

######################################################
###### IV. Studying Supplementary Individuals ########
######################################################
# datsup <- read.csv('../data/nba-teams-2016.csv')