dataset <- read.csv('data/nba-teams-2017.csv', stringsAsFactors = F)
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


######################################################
###################### III. Lab ######################
######################################################
ActVars <- c("wins", "losses", "points", "field_goals", "points3", "free_throws", 
             "off_rebounds", "def_rebounds", "assists", "steals", "blocks", "personal_fouls")
dat <- dataset[, ActVars]

########## 1. PCA with prcomp()
pca_prcomp <- prcomp(dat, scale. = TRUE)
(eigenvalues <- round((pca_prcomp$sdev)^2, digits = 3))
loadings <- pca_prcomp$rotation
scores <- pca_prcomp$x
# scores <- datSD %*% loadings

########## 2. PCA with princomp()
# to perform PCA on standardized data, use cor = TRUE, meaning that 
# the analysis is performed using the correlation matrix.
pca_princomp <- princomp(dat, cor = TRUE)
# what's in a princomp object?
names(pca_princomp)
loadings <- pca_princomp$loadings  # some values are left in blank
# How would you retrieve just the matrix of loadings?
print(pca_princomp$loadings, cutoff = 0)

########## 3. Stages of a Principal Components Analysis

## 3.1 Eigenvalues and Proportion of Variance Explained
(eigenvalues <- round((pca_princomp$sdev)^2, digits = 4))
proportion <- round(eigenvalues / ncol(dat), digits = 4) * 100
cum_prop <- cumsum(proportion)
(Eigval.Tb <- cbind(eigenvalues, proportion, cum_prop))
row.names(Eigval.Tb) <- paste0("PC", 1:12)
barplot(Eigval.Tb[, 1])

## 3.2 Choosing the number of components
# eg 1. capture 70% of the total variation
sum(cum_prop <= 70)  # the first 3 PCs

# eg 2. Kaiser’s rule: Exclude PCs whose eigenvalues are less than the average
ave.egval <- round(sum(eigenvalues) / ncol(dat))  # If X standardized, always 1
sum(eigenvalues >= 1)  # the first 4 PCs

# eg 3. Jollife rule: Exclude PCs whose eigenvalues are less than 0.7
sum(eigenvalues >= 0.7)  # the first 6 PCs

# eg 4. looking for an “elbow” in the screenplot
# a point where “large” eigenvalues cease and “small” eigenvalues begin
plot(x = 1:12, y = eigenvalues, xlab = "number of components", 
     ylab = "values", type = "b")
# With this rule, I will retain 3 PCs

## 3.3 Variable Loadings and Correlations with PCs
# The larger the loading of a variable in a given PC, 
# the more associated the variable is with that PC.
scores <- as.data.frame(pca_princomp$scores)
colnames(scores) <- paste0("PC", 1:12)

# What are the correlations of the variables with the PC
cor(dat, scores)

# What variables seem to be more correlated with PC1? "points"
which.max(cor(dat, scores[, 1]))  # 3

# What variables seem to be more correlated with PC2? "points3"
which.max(cor(dat, scores[, 2]))  # 5

# Circle of correlations
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(dat, scores))
  
# data frame with arrows coordinates
arrows = data.frame(x1 = c(0, 0, 0, 0), y1 = c(0, 0, 0, 0),
                    x2 = correlations$PC1, y2 = correlations$PC2)
# geom_path will do open circles
library(ggplot2)
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, colour = "gray65") + 
  xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs", y = "pc2 axis") + 
  ggtitle("Circle of correlations")

## 3.4 Visualizing observations
rownames(scores) <- dataset$team

# plot of observations
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PC Plot of Teams")

# install.packages("plotly")
library(plotly)

# data frame for plot_ly()
scores_df <- cbind.data.frame(
  scores, team = dataset$team, stringsAsFactors = FALSE)
colnames(scores_df) <- c(paste0("PC", 1:12), "team")

# Here’s a scatterplot of PC1 and PC2
# scatter plot
plot_ly(data = scores_df, x = ~PC1, y = ~PC2, 
        type = 'scatter', mode = 'markers',
        text = ~team, marker = list(size = 10))

# Here’s a 3D-scatterplot of PC1, PC2, and PC3
# 3d scatter plot
plot_ly(data = scores_df, x = ~PC1, y = ~PC2, z = ~PC3, 
        type = 'scatter3d', mode = 'markers', text = ~team)

## 3.5 Biplot: Another visual display
# biplot
# The scale = 0 argument to biplot() ensures that the arrows are 
# scaled to represent the loadings, while the PCs are scaled to unit variance
biplot(pca_prcomp, scale = 0)
biplot(pca_prcomp, scale = 1)
