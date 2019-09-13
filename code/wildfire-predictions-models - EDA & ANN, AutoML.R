
## processed data


## EDA on data
setwd("D:\\MS Data Analytics\\Sem 2\\ADM\\ADM Project\\Git\\wildfires-adm\\Random Forest")
wildfire_data <- read.delim("newWildFireData.csv", header = TRUE, sep = ",")

summary(wildfire_data)

# class balance: imbalanced!!!
table(wildfire_data$fire)

#check for missing values
sapply(wildfire_data,function(x) sum(is.na(x)))


#### univariate analysis using graphs
# central tendency 
summary(wildfire_data$ndvi)
boxplot(wildfire_data$ndvi)

# spread
hist(wildfire_data$ndvi)


#outliers
boxplot(subset(wildfire_data, select=c(2:8)))
boxplot(subset(wildfire_data, select=c(6,8)))
boxplot(wildfire_data$precipitation) # huge outlier!
boxplot(wildfire_data$wind) # outlier!!!!
boxplot(wildfire_data$relativeHuditiy)

boxplot(wildfire_data$ndvi ~ wildfire_data$fire, col=c("grey","gold"))

spineplot(wildfire_data$ndvi, wildfire_data$fire) # lower the values of ndvi shows higher chances of fire.

scatter.smooth(wildfire_data$ndvi, wildfire_data$maxTemp, main="Relation between Max Temp Vs NDVI")

wildfire_data.cor <- cor(subset(wildfire_data, select=c(2:8)), method = c("spearman"))

###

#install.packages("corrplot")
library(corrplot)
corrplot(wildfire_data.cor)

wildfire_data$avgTemp <- rowMeans(wildfire_data[ , c("minTemp", "maxTemp")] ) 
wildfire_data$minTemp = NULL
wildfire_data$maxTemp = NULL
write.csv(wildfire_data, "scaledData.csv", row.names = FALSE)
###################################### ANN ##########################################

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/master/4644/R")

# Finally, let's load H2O and start up an H2O cluster

.rs.restartR()

library(h2o)
h2o.init()

####### PCA in R #######

#normalise numeric data
summary(wildfire_data)
wildfire_ann <- wildfire_data
summary(wildfire_ann)
wildfire_ann[,-1] <-scale(wildfire_ann[,-1])

summary(wildfire_ann)

library(e1071)
library(ggfortify)
library(caret)
set.seed(1337)
index <- createDataPartition(wildfire_ann$fire, p = .80, list = FALSE)
pca <- prcomp(wildfire_ann[index, -1], scale. = T, center = F)

autoplot(pca, data=wildfire_ann[index,], colour='fire')

screeplot(pca, type="lines", npcs = 8)

var.pca <- pca$sdev ^ 2
x.var.pca <- var.pca / sum(var.pca)
cum.var.pca <- cumsum(x.var.pca)
plot(cum.var.pca[1:8],xlab="No. of principal components",
     ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')

## indicates 2 to 4 pca required


h2o.wildfire <- h2o.importFile(path="scaledData.csv", header=T)
t2 <- Sys.time()
pca.h2o <- h2o.prcomp(h2o.wildfire[, -1], k=6)


plot(x=t(pca.h2o@model$importance[1,]), xlab="Principal Component",
     ylab="Proportion of variance explained")

plot(x=t(pca.h2o@model$importance[3,]), xlab="No. of principal components",
     ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')


wildfire_ann.pca <- as.matrix(wildfire_ann[,-1]) %*% pca$rotation[, 1:4]
wildfire_ann.h2o.pca <- as.matrix(wildfire_ann[,-1]) %*%
  as.matrix(pca.h2o@model$eigenvectors[, 1:4])


library(caret)

set.seed(2018)
#stratified sampling
index <- createDataPartition(wildfire_ann$fire, p = .75, list = FALSE)

h2o.training <- h2o.wildfire[index, -1]
h2o.testing <- h2o.wildfire[-index, -1]

pca.h2o.training <- h2o.prcomp(h2o.training, x=names(h2o.training),
                               k=6, ignore_const_cols = FALSE)

pca.train <- as.matrix(wildfire_ann[index,-1]) %*%
  as.matrix(pca.h2o.training@model$eigenvectors[, 1:4])

pca.test <- as.matrix(wildfire_ann[-index,-1]) %*%
  as.matrix(pca.h2o.training@model$eigenvectors[, 1:4])

h2o.training.pca <- as.h2o(pca.train)
h2o.testing.pca <- as.h2o(pca.test)

factor(wildfire_ann$fire)
h2o.training.pca <- h2o.cbind(h2o.training.pca, as.h2o(factor(wildfire_ann[index,1])))
h2o.testing.pca <- h2o.cbind(h2o.testing.pca, as.h2o(factor(wildfire_ann[-index,1])))
names(h2o.training.pca)

#rename the dependent (it's current called x)
names(h2o.training.pca)[5] <- "fire"
names(h2o.testing.pca)[5] <- "fire"

t3 <- Sys.time()
res.dl <- h2o.deeplearning(x = 1:4,
                           y = "fire",
                           training_frame = h2o.training.pca,
                           activation = "Tanh",
                           hidden=rep(160,3),
                           epochs = 20)

print( difftime( Sys.time(), t3, units = 'sec'))

(pref<-h2o.performance(model=res.dl, newdata=h2o.testing.pca, xval = TRUE))

pred.dl<-h2o.predict(object=res.dl, newdata=h2o.testing.pca[,-5])

summary(pred.dl)

predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(wildfire_ann$fire),
                    labels = levels(wildfire_ann$fire))
confusionMatrix(predicted, wildfire_ann[-index, 1])
## 0.9805

validation <- createDataPartition(wildfire_ann[-index,1], p = 1/3, list = FALSE)
h2o.validation <- h2o.testing.pca[validation, ]

hidden_opt <- list(rep(150,3), c(200, 150, 75, 50), 100)
l1_opt <- c(1e-5,1e-7)
activation <- c("Tanh", "RectifierWithDropout", "Maxout")
hyper_params <- list(hidden = hidden_opt,
                     l1 = l1_opt, activation = activation)
h2o.grid("deeplearning",
         hyper_params = hyper_params,
         x = 1:4,
         y = "fire",
         grid_id = "ADM_ann_grid",
         training_frame = h2o.training.pca,
         validation_frame = h2o.validation)

grid <- h2o.getGrid(grid_id = "ADM_ann_grid",
                    sort_by = "accuracy",
                    decreasing = TRUE)
# Grab the top model, chosen by accuracy
best <- h2o.getModel(grid@model_ids[[1]])
print(best)

h2o.performance(best, newdata=h2o.testing.pca[-validation, ], xval=TRUE)

pred.dl<-h2o.predict(object=best, newdata=h2o.testing.pca[-validation,-5])

predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(wildfire_ann$fire),
                    labels = levels(wildfire_ann$fire))
testingLabels <- wildfire_ann[-index, 1]
testingLabels <- testingLabels[-validation]
confusionMatrix(predicted, testingLabels)

## 0.9771


automl <- h2o.automl(x = 1:4, y = "fire",
                     training_frame = h2o.training.pca,
                     validation_frame = h2o.validation,
                     project_name = "wildfire_AutoML",
                     max_runtime_secs = 180)

h2o.performance(automl@leader, newdata=h2o.testing.pca[-validation, ], xval=TRUE)

pred.dl<-h2o.predict(object=automl@leader, newdata=h2o.testing.pca[-validation,-5])

predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(wildfire_ann$fire),
                    labels = levels(wildfire_ann$fire))
testingLabels <- wildfire_ann[-index, 1]
testingLabels <- testingLabels[-validation]
confusionMatrix(predicted, testingLabels)

### 0.9706


