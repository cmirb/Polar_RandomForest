#### Description ####
# This script is used to train and test a random model. 
# The goal is find out the climatic variables that best explain the distribution of Salix arctica in the Arctic.
# The predictor variables are in the form of bioclimatic variables from the CHELSA dataset, as raster files.
# The target variable (presence or absene of the species) is in the form of a dataframe with presence and background points at lat/lon locations. 
# The model is trained using the randomForest package in R.
# The model is tested using a test dataset and evaluated using the confusion matrix and the AUC score.

# The random forest model can be applied to future projections of the same variables, provided that the same variables are used as predictors.

# @author: Charlotta Mirbach

#### Script: ####


# selected variables
# bio05	-0.53temp of warmest month
# bio02	-0.45 mean diurnal range, used to calc bio03
# bio03	-0.26isothermality
# bio12	0.21 annual precipitation
# bio19	0.26 prec coldest quarter


rm(list = ls(all = TRUE)) 

# load packages -----------------------------------------------------------


if (!require("rspat")) remotes::install_github("rspatial/rspat")
## Loading required package: rspat
if (!require("geodata")) remotes::install_github("rspatial/geodata")
## Loading required package: geodata
if (!require("predicts")) remotes::install_github("rspatial/predicts")
## Loading required package: predicts

library(rspat)
library(tidyverse)
#library(knitr)
# library(kableExtra)


# load occurences ---------------------------------------------------------


occs <- read.csv('D:/01-msc/01-thesis/01-data/01-raw/01-occurences/occurence data/inaturalist_gbif_rs_field.csv', header= T, sep =";")
salix <- occs
# 2172 obs

salix <- dplyr::select(salix,
                       longitude,
                       latitude,
                       scientific_name)
colnames(salix) <- c('lon', 'lat', 'Class')

# remove duplicates
dups <- duplicated(salix[c("lon","lat")])
dim(dups)

# subset occs to domain ---------------------------------------------------
# subset the data to spatial extend of domain
e <- ext(25, 160,60,82)

salix <- subset(salix, lat>60 & lat<82) # 2167 observations
salix <- subset(salix, lon>25 & lon<160) # 1776 observations
# 1776 obs

dev.off()
plot(salix[,1:2], cex=0.5, col="red")



# load climate variables --------------------------------------------------

# climate variables

chelsa <- list.files(('D:/01-msc/01-thesis/01-data/02-output/02-climate/01_current/clim_cur_all_vars'), full.names = T)
chelsa <- rast(chelsa)
names(chelsa)

chelsa <- rast('D:/01-msc/01-thesis/01-data/02-output/02-climate/01_current/01_file.tif')
# selected variables
# bio05	-0.53temp of warmest month
# bio02	-0.45 mean diurnal range, used to calc bio03
# bio03	-0.26isothermality
# bio12	0.21 annual precipitation
# bio19	0.26 prec coldest quarter

bio <- subset(chelsa, c(2,5,12,19))
names(bio)
chelsa <- bio

# extract climate var at occ points ---------------------------------------
# extract raster values at occurrence points

salix_c <- extract(chelsa, salix[,1:2])
head(salix_c, 3)

salix_c <- na.omit(salix_c)
# 1429 obs
# 1757 obs if only bioclim vars are included

# remove first column
salix_c <- salix_c[,-1]

# export dataframe
write.csv(salix_c, 'D:/01-msc/01-thesis/01-data/02-output/05-presence-absence/values_at_occ_points.csv')

# vectorize salix extent to create mask -----------------------------------

# vectorize (as points shapefile)
ext_salix <- ext(vect(salix[, 1:2])) + 5
ext_salix


# create background points ------------------------------------------------

# take sample
set.seed(0)
window(chelsa) <- ext_salix
bg <- spatSample(chelsa, 5000, "random", na.rm=TRUE, xy=TRUE)
head(bg)
tbs_bg <- bg

# remove ID 
bg <- bg[, -c(1:2)]
# 5000 background points

# plot 

names(bg)

plot(bg[,2], bg[,4], xlab="Max temp of warmest month (?C)",
     ylab="Precipitation of coldest quarter (mm)", cex=.8)
points(salix_c[,2], salix_c[,4], col="red", cex=.6, pch="+")
legend("topright", c("observed", "background"), col=c("red", "black"), pch=c("+", "o"), pt.cex=c(.6, .8))


# create presence-background df -------------------------------------------

# combine presence and background data
dw <- rbind(cbind(pa=1, salix_c), cbind(pa=0, bg))
df <- dw
# 6429 obs, 5000 being bg points

write.csv(df, 'E:/01-msc/01-thesis/01-data/02-output/05-presence-absence/01-presence-background/presence-background-chelsa-bioclim-4vars.csv', row.names = F, sep=';')
df <- read.csv('D:/01-msc/01-thesis/01-data/02-output/05-presence-absence/01-presence-background/presence-background-chelsa-bioclim-4vars.csv')



# random forest model -----------------------------------------------------


# rf-data splitting -------------------------------------------------------


# create random train & test data

set.seed(123)
i <- sample(nrow(df), 0.2 * nrow(df))
test <- df[i,] # 1351 obs
train <- df[-i,] # 5406 obs

# make presence-background categorical variable
fpa <- as.factor(train[, 'pa'])


# train random forest -----------------------------------------------------


library(randomForest)
crf <- randomForest(train[, 2:ncol(train)], fpa, importance = T)

# inspect random forest
# especially out-of-bag error
crf # llist of 18
plot(crf)

# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 11.14%
# Confusion matrix:
#     0    1   class.error
# 0 3751  262  0.06528781
# 1  340 1053  0.24407753


# determine importance of input variables
png("D:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/variable_importance.png",
    width = 900, height = 900, pointsize = 18) 
varImpPlot(crf)
dev.off()

save(crf, file="E:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/crf.RData", overwrite=T)
load('D:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/crf.RData')


# tune random forest ------------------------------------------------------

# convert presence-absence to factor to allow for classification rather than regression
train$pa <- as.factor(train$pa)
test$pa <- as.factor(test$pa)
# use regression and tune parameters
trf <- tuneRF(train[, 2:ncol(train)], train[, "pa"])
trf
# mtry   OOBError
# 1    1 0.08431804
# 2    2 0.08351611

# mtry: Number of variables randomly sampled as candidates at each split.
# determine no of vars with smalles out of bag error

mt <- trf[which.min(trf[,2]), 1]
mt
# note this changes every time the rf is created


# run random forest (train) w/ mtry ---------------------------------------

rrf <- randomForest(train[, 2:ncol(df)], train[, "pa"], mtry=mt, ntree=250)
rrf # list of 17
plot(rrf)
varImpPlot(rrf)

save(rrf, file='E:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/rf_tuned.RData')
load('D:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/rf_tuned.RData')


# predict to raster -------------------------------------------------------


# make prediction based on RF
gc() # take out the trash
# need to rewrite data.frame (df) 
# if it is loaded from disk, an error occurs
rp <- predict(chelsa, rrf, na.rm=TRUE) # takes forever, 15 min or so
# 12:33 -
writeRaster(rp, 'E:/01-msc/01-thesis/01-data/02-output/06-predictions/05-rf-4vars/rf_chelsa_current_4vars.tif')

rp_full <- rast('D:/01-msc/01-thesis/01-data/02-output/06-predictions/03-rf-4vars/rf_chelsa_current_4vars_resampled.tif')

par(mfrow=c(1,2))
windows()
plot(rp, main='new projection')
plot(rp_full, main='old projection')

# evaluate random forest (test) --------------------------------------------------
library(predicts)
eva <- pa_evaluate(predict(rrf, test[test$pa==1, ]), predict(rrf, test[test$pa==0, ]))
pred_1 <- predict(rrf, test[test$pa==1, ])
pred_0 <- predict(rrf, test[test$pa==0, ])
str(pred_1)
str(pred_0)


eva
#' @stats
#' np  na prevalence   auc   cor pcor   ODP
#' 1 364 987      0.269 0.935 0.755    0 0.731
#' 
#' @thresholds
#' max_kappa max_spec_sens no_omission equal_prevalence equal_sens_spec
#' 1     0.458         0.237       0.019             0.27           0.253
#' 
#' @tr_stats
#' treshold kappa  CCR  TPR TNR FPR  FNR  PPP  NPP  MCR  OR
#' 1           0     0 0.27    1   0   1    0 0.27  NaN 0.73 NaN
#' 2           0     0 0.27    1   0   1    0 0.27  NaN 0.73 NaN
#' 3           0     0 0.27    1   0   1    0 0.27  NaN 0.73 NaN
#' 4         ...   ...  ...  ... ... ...  ...  ...  ...  ... ...
#' 1078        1  0.01 0.73 0.01   1   0 0.99    1 0.73 0.27 Inf
#' 1079        1  0.01 0.73 0.01   1   0 0.99    1 0.73 0.27 Inf
#' 1080        1     0 0.73    0   1   0    1  NaN 0.73 0.27 NaN
#' 
#' # Load necessary libraries
# install.packages("pROC")
library(pROC) # for ROC curve and AUC i.e. signal vs noise

# Assuming rrf is the random forest model and test is test data
predicted_probs <- predict(rrf, test) # probabilities of the positive class
str(predicted_probs)
# Compute ROC curve
roc_obj <- roc(test$pa, predicted_probs)

# Compute AUC
auc(roc_obj)

# To get coordinates of the ROC curve (to select thresholds)
roc_coords <- coords(roc_obj, "all")

# To get the threshold that maximizes sensitivity + specificity:
best_threshold <- coords(roc_obj, "best")
# Extract the actual threshold value
best_threshold_value <- best_threshold[1, "threshold"]

# Compute predictions using the threshold
predictions <- ifelse(predicted_probs >= best_threshold_value, 1, 0)
predictions <- as.numeric(predictions)
str(predictions)

png("D:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/roc_curve.png")
plot.roc(roc_obj, main="ROC Curve", col="blue")
abline(h=0, v=1, col="gray")
dev.off()


library(caret)
length(predictions)
length(test$pa)

cm <- confusionMatrix(as.factor(predictions), as.factor(test$pa))

library(xtable)
latex_table <- xtable(cm$table)
print(latex_table, type = "latex", file = "D:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/model_evaluation.tex")

stats_df <- as.data.frame(as.table(cm$overall))
latex_stats <- xtable(stats_df)
print(latex_stats, type = "latex", file = "D:/01-msc/01-thesis/01-data/02-output/07-models/01_rf_4vars/model_stats.tex")

library(grid)
library(gridExtra)

# Confusion matrix as a graphical table
cm_gtable <- tableGrob(cm$table)

# Model statistics as a graphical table
stats_gtable <- tableGrob(as.data.frame(as.table(cm$overall)))


png("D:/01-msc/01-thesis/09-graphics/04_model_metrics/confusion_matrix_rf_4_vars.png", width=800, height=400)
grid.draw(cm_gtable)
dev.off()

png("D:/01-msc/01-thesis/09-graphics/04_model_metrics/model_stats_rf_4_vars.png", width=800, height=400)
grid.draw(stats_gtable)
dev.off()

# install.packages(c("ggplot2", "ggtext", "dplyr"))
library(ggplot2)
library(ggtext)
library(dplyr)

# Confusion matrix to dataframe
cm_df <- as.data.frame(as.table(cm$table))

# Model statistics to dataframe
stats_df <- as.data.frame(as.table(cm$overall))

plot_nature_style_table <- function(data, title) {
  ggplot(data, aes(x = Var1, y = reorder(Var2, desc(Var2)), label = Freq)) +
    geom_tile(fill = "white", color = "black", size = 1.5) +
    geom_text(size = 5, family = "Arial") +
    theme_minimal() +
    labs(title = title, x = NULL, y = NULL) +
    theme(axis.text = element_textbox_simple(size = 12, box.color = "transparent", color = "black", family = "Arial"),
          plot.title = element_textbox_simple(size = 14, halign = 0.5, box.color = "transparent", color = "black", family = "Arial"),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
}

plot_nature_style_1D_table <- function(data, title) {
  ggplot(data, aes(x = "", y = reorder(Var1, desc(Var1)), label = round(Freq, 2))) +
    geom_tile(fill = "white", color = "black", size = 1.5) +
    geom_text(size = 5, family = "sans") +
    theme_minimal() +
    labs(title = title, x = NULL, y = NULL) +
    theme(axis.text = element_textbox_simple(size = 12, box.color = "transparent", color = "black", family = "sans"),
          plot.title = element_textbox_simple(size = 14, halign = 0.5, box.color = "transparent", color = "black", family = "sans"),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
}


colnames(cm_df) <- c("Var1", "Var2", "Freq")

# Usage:
cm_plot <- plot_nature_style_table(cm_df, "Confusion Matrix")
stats_plot <- plot_nature_style_1D_table(stats_df, "Model Statistics")
stats_plot


ggsave("D:/01-msc/01-thesis/09-graphics/04_model_metrics/confusion_matrix_rf_4_vars.png", cm_plot, width = 5, height = 4, dpi = 300)
ggsave("D:/01-msc/01-thesis/09-graphics/04_model_metrics/model_stats_rf_4_vars.png", stats_plot, width = 5, height = 4, dpi = 300)


#  evaluate variable importance----------------
# variable importance
library(randomForest)
importance_scores <- importance(rrf)


importance_df <- as.data.frame(importance_scores)
importance_df$Variable <- rownames(importance_df)
colnames(importance_df)


library(ggplot2)

ggplot(importance_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variable", y = "Importance (Mean Decrease Gini)") +
  theme_minimal()


# AUC and ROC---------------------

# AUC
library(pROC)

# Compute ROC curve (assuming we have already done this)
roc_obj <- roc(test$pa, predicted_probs)

# Plot ROC curve
auc_plot <- ggroc(roc_obj) + 
  ggtitle(paste("ROC Curve\nAUC =", round(auc(roc_obj), 2))) + 
  theme_minimal()

print(auc_plot)


# Kappa statistic
# Extract Kappa from confusion matrix
kappa_value <- cm$overall["Kappa"]

kappa_df <- data.frame(
  Model = "Random Forest",
  Kappa = kappa_value
)

kappa_plot <- ggplot(kappa_df, aes(x = Model, y = Kappa)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  ggtitle("Kappa Statistic for Random Forest Model") + 
  theme_minimal()

print(kappa_plot)

#library(knitr)
#library(kableExtra)
#library(magick)
#library(webshot)
webshot::install_phantomjs()


kbl(eva@stats, caption =  "Evaluation of Random Forest",booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "hold_position"))
# receiver operating characteristic curve
# measure of true positives vs false postives
plot(eva, "ROC")
dev.off()
par(mfrow=c(1,2))
plot(eva, "boxplot")
plot(eva, "density")


# create threshold (sensitivity vs specificity) ----------------------------------------------------


# set thresholds
tr <- eva@thresholds
tr
# max_kappa max_spec_sens no_omission equal_prevalence equal_sens_spec
# 1    0.4389        0.3635       9e-04           0.2709       0.2714333
# plot binary representation
dev.off()
windows()
plot(rp > tr$max_spec_sens)
plot(chelsa,2)

# project future scenarios --------------------------------------------------------


# make future prediction
# download all variables (global)
# load files (global)

# rasters were cropped in a different script on a different machine
# predictions were performed remotely

# get packages citations --------------------------------------------------
library(caret)

packages <- grep("^package:", search(), value = TRUE)
packages <- gsub("^package:", "", packages)


get_citation <- function(pkg_name) {
  cit <- tryCatch({
    cit_obj <- citation(pkg_name)
    capture.output(print(cit_obj, bibtex = TRUE))
  }, error = function(e) {
    return(NA)
  })
  return(cit)
}

get_description <- function(pkg_name) {
  desc <- tryCatch({
    pkg_desc <- packageDescription(pkg_name)
    return(pkg_desc$Description)
  }, error = function(e) {
    return(NA)
  })
}

descriptions <- sapply(packages, get_description)

citations_list <- lapply(packages, get_citation)
names(citations_list) <- packages

citations_df <- data.frame(Package = names(citations_list), 
                           Description = descriptions,
                           Citation = sapply(citations_list, paste, collapse = "\n"),
                           stringsAsFactors = FALSE)
print(citations_df)


# Replace line breaks with spaces in the Description column
citations_df$Description <- gsub("\n", " ", citations_df$Description)

write.csv(citations_df, 'D:/01-msc/01-thesis/06-references/packages_df.csv', row.names = F)

packages %>% 
  map(citation) %>% 
  print(style = 'text')


