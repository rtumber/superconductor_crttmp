#packages
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(pdftools)) install.packages("pdftools", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(bst)) install.packages("bst", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(mboost)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(earth)) install.packages("earth", repos = "http://cran.us.r-project.org")
if(!require(brnn)) install.packages("brnn", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(import)) install.packages("import", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")

#NOTE tabulizer and rjava require Java installation. for 64bit systems this needs to be done manually, web 
#installer only gives 32 bit version
if(!require(rJava)) install.packages("rJava", repos = "http://cran.us.r-project.org")
if(!require(tabulizer)) install.packages("tabulizer", repos = "http://cran.us.r-project.org")

library(rvest)
library(data.table)
library(tidyverse)
library(pdftools)
library(rJava)
library(tabulizer)
library(patchwork)
library(caret)
library(bst)
library(gam)
library(kernlab)
library(mboost)
library(earth)
library(brnn)
library(randomForest)
library(import)
library(xgboost)
library(doParallel)
library(kknn)
library(Rborist)
library(pls)

#Data Import
#datafile address https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip

data_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip", data_file)

zip_contents<- unzip(data_file, list = TRUE)
zip_contents

#read data description from webpage to ascertain how the zip contents should be processed
html <- read_html("https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data")
html


#extract tables
tabs <- html %>% html_nodes("table")
#retain table 2
tab_2 <- tabs[[2]]
#extract text
tab_2_text <- html_text(tab_2)
tab_2_text
#split on \n
tab_2_split <- as.data.table(strsplit(tab_2_text, "\n"))
#data information
data_set_inf <- tab_2_split[33]
#citation reference
cit_ref <- tab_2_split[42]
#relevant paper linked on page is behind a paywall, free version available at https://arxiv.org/abs/1803.10260, 
#download link https://arxiv.org/pdf/1803.10260.pdf

#Examine data description
print(data_set_inf)
#Web link updated to https://supercon.nims.go.jp/en/

#Open zip and assign references to each csv in relation to purpose, then tidy dataframes, etc that will no longer be used 
unzip(data_file)
superconductor_ref <- read.csv("unique_m.csv")
feature_data <- read.csv("train.csv")

rm(data_file)
rm(zip_contents)
rm(html)
rm(tabs)
rm(tab_2)
rm(tab_2_text)
rm(tab_2_split)


#Data exploration

#The objective is to determine the critical temperature from the features in feature_data and the data in 
#superconductor_ref. Superconductors in superconductor_ref share the same row index as the data in feature_data.
#The relevant paper state these data are already cleaned and includes an explanation of the process with which the
#features that may be used to predict Critical temperature are calculated.
#pdf downloaded
pdf_file <- tempfile()
download.file("https://www.arxiv.org/pdf/1803.10260.pdf", pdf_file, mode = "wb")

#The below table extracted from the PDF describes some of the elemental properties used.
pdf_txt_extr <- pdf_text(pdf_file)
ele_prop <- pdf_txt_extr[4] %>% str_trim %>% str_split("\r\n")
ele_prop

#attempt tidier extraction using tabulizer
ele_prop_tabl <- extract_tables(pdf_file, pages=4, method="stream") %>% as.data.table()

#tidy extraction
colnames(ele_prop_tabl) <- as.character(ele_prop_tabl[1])
ele_prop_tabl <- ele_prop_tabl[-1]
ele_prop_tabl$Description[2] <- paste(ele_prop_tabl$Description[2], ele_prop_tabl$Description[3], sep =" ")
ele_prop_tabl <- ele_prop_tabl[-3,]
ele_prop_tabl$Description[4] <- paste(ele_prop_tabl$Description[4], ele_prop_tabl$Description[5], sep =" ")
ele_prop_tabl <- ele_prop_tabl[-5,]
ele_prop_tabl$Description[5] <- paste(ele_prop_tabl$Description[5], ele_prop_tabl$Description[6], sep =" ")
ele_prop_tabl <- ele_prop_tabl[-6,]
ele_prop_tabl$Description[6] <- paste(ele_prop_tabl$Description[6], ele_prop_tabl$Description[7], sep =" ")
ele_prop_tabl <- ele_prop_tabl[-7,]
ele_prop_tabl$Description[8] <- paste(ele_prop_tabl$Description[8], ele_prop_tabl$Description[9], sep =" ")
ele_prop_tabl <- ele_prop_tabl[-9,]

Encoding(ele_prop_tabl$Variable) <- "UTF-8"
Encoding(ele_prop_tabl$Units) <- "UTF-8"
Encoding(ele_prop_tabl$Description) <- "UTF-8"


#To begin we will examine the feature data
#Examination of feature_data
feat_str <- as.data.frame(summary.default(feature_data)) %>% group_by(Var1) %>% spread(key = Var2, value = Freq) %>%
  select(Var1, Mode)
colnames(feat_str) <- c("feature_name", "class")

#An explanation of the features is obtained from the original paper
#pdf text extracted and relevant entry located
feature_explanation <- pdf_txt_extr[9] %>% str_split("\r\n")
feature_explanation <- feature_explanation[[1]][1:11] %>% str_trim %>% str_split("\\s{2,}", simplify = TRUE) %>% as.data.table() 
colnames(feature_explanation) <- as.character(feature_explanation[1])
feature_explanation <- feature_explanation[-1]

#The example values in the table are based on superconductor Re7Zr1.
#To briefly summarise some of the undefined equation components in the above table
#t1 & t2 refer to the thermal conductivities of the two elements
#p1 & p2 are the ratios of the two elements
#w1 & w2 are fractions of thermal conductivities, t1/(t1+t2) & t2/(t1+t2)
#A & B are intermediate values calculated from components p & w. A = p1w1/(p1w1+p2w2) & B = p2w2/(p1w1+p2w2)

#81 predictors, mostly numeric, three integers. critical_temp is the target.
summary(feature_data)

#Examination of the effect of each feature on critical temperature
#Start with correlation of all features against critical temperature
cor(feature_data)
x<- ncol(feature_data) -1
cor_rng <- seq(1, x, 1)

feat_cor <- cor(feature_data[,cor_rng],feature_data$critical_temp) %>% as.data.frame()
colnames(feat_cor) <- "correlation"
feat_cor <- feat_cor %>% mutate(feature = colnames(feature_data[1:x])) %>% arrange(desc(correlation))

feat_cor %>% slice_max(correlation, n = 20)
feat_cor %>% slice_min(correlation, n = 20)
ind_tst <- c(1,2,5)
model_features <- feat_train %>% select(-all_of(ind_tst))

###################

#Generally good correlation in sufficient features to proceed
#Number of elements have a coefficient above 0.60 however this may simply be a result of the sample of 
#superconductors taken and the number of superconductors of specific certain crystal structures (perovskite, for
#example). This may be worth returning to if appropriate data is available.

##################

#Plotting each feature against critical temperature to look for non-linear relationships
#Split feature_data into similar properties
feat_atomic_mass <- feature_data %>% select(c(ends_with("atomic_mass"), "critical_temp"))
feat_fie <- feature_data %>% select(c(ends_with("fie"), "critical_temp"))
feat_atomic_radius <- feature_data %>% select(c(ends_with("atomic_radius"), "critical_temp"))
feat_density <- feature_data %>% select(c(ends_with("Density"), "critical_temp"))
feat_electron_affinity <- feature_data %>% select(c(ends_with("ElectronAffinity"), "critical_temp"))
feat_fusion_heat <- feature_data %>% select(c(ends_with("FusionHeat"), "critical_temp"))
feat_thermal_cond <- feature_data %>% select(c(ends_with("ThermalConductivity"), "critical_temp"))
feat_valence <- feature_data %>% select(c(ends_with("Valence"), "critical_temp"))

#graph plotting features against critical temperature (low alpha used due to volume of data)
#number of elements
feature_data %>%
  ggplot(aes(number_of_elements,critical_temp)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", method.args = list(degree=1))

#Atomic Mass
am_x<- ncol(feat_atomic_mass) - 1
am_col_ind <- seq(1, am_x, 1)

feat_am_plot <- lapply(am_col_ind, function(am_feat){
  feat_am_point <- feat_atomic_mass %>%
    ggplot(aes(feat_atomic_mass[,am_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_atomic_mass[am_feat])) +
    xlab(colnames(feat_atomic_mass[am_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})

(feat_am_plot[[1]]|feat_am_plot[[2]])/(feat_am_plot[[3]]|feat_am_plot[[4]])/(feat_am_plot[[5]]|feat_am_plot[[6]])/(feat_am_plot[[7]]|feat_am_plot[[8]])/(feat_am_plot[[9]]|feat_am_plot[[10]])
ggsave("images/feat_am_plot.png")

#First Ionisation Energy
fie_x<- ncol(feat_fie) - 1
fie_col_ind <- seq(1, fie_x, 1)

feat_fie_plot <- lapply(fie_col_ind, function(fie_feat){
  feat_fie %>%
    ggplot(aes(feat_fie[,fie_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_fie[fie_feat])) +
    xlab(colnames(feat_fie[fie_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_fie_plot[[1]]|feat_fie_plot[[2]])/(feat_fie_plot[[3]]|feat_fie_plot[[4]])/(feat_fie_plot[[5]]|feat_fie_plot[[6]])/(feat_fie_plot[[7]]|feat_fie_plot[[8]])/(feat_fie_plot[[9]]|feat_fie_plot[[10]])
ggsave("images/feat_fie_plot.png")

#Atomic Radius
ar_x<- ncol(feat_atomic_radius) - 1
ar_col_ind <- seq(1, ar_x, 1)

feat_ar_plot <- lapply(ar_col_ind, function(ar_feat){
  feat_atomic_radius %>%
    ggplot(aes(feat_atomic_radius[,ar_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_atomic_radius[ar_feat])) +
    xlab(colnames(feat_atomic_radius[ar_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_ar_plot[[1]]|feat_ar_plot[[2]])/(feat_ar_plot[[3]]|feat_ar_plot[[4]])/(feat_ar_plot[[5]]|feat_ar_plot[[6]])/(feat_ar_plot[[7]]|feat_ar_plot[[8]])/(feat_ar_plot[[9]]|feat_ar_plot[[10]])
ggsave("images/feat_ar_plot.png")

#Density
den_x<- ncol(feat_density) - 1
den_col_ind <- seq(1, den_x, 1)

feat_den_plot <- lapply(den_col_ind, function(den_feat){
  feat_density %>%
    ggplot(aes(feat_density[,den_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_density[den_feat])) +
    xlab(colnames(feat_density[den_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_den_plot[[1]]|feat_den_plot[[2]])/(feat_den_plot[[3]]|feat_den_plot[[4]])/(feat_den_plot[[5]]|feat_den_plot[[6]])/(feat_den_plot[[7]]|feat_den_plot[[8]])/(feat_den_plot[[9]]|feat_den_plot[[10]])
ggsave("images/feat_den_plot.png")

#Electron Affinity
ea_x<- ncol(feat_electron_affinity) - 1
ea_col_ind <- seq(1, ea_x, 1)

feat_ea_plot <- lapply(ea_col_ind, function(ea_feat){
  feat_electron_affinity %>%
    ggplot(aes(feat_electron_affinity[,ea_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_electron_affinity[ea_feat])) +
    xlab(colnames(feat_electron_affinity[ea_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_ea_plot[[1]]|feat_ea_plot[[2]])/(feat_ea_plot[[3]]|feat_ea_plot[[4]])/(feat_ea_plot[[5]]|feat_ea_plot[[6]])/(feat_ea_plot[[7]]|feat_ea_plot[[8]])/(feat_ea_plot[[9]]|feat_ea_plot[[10]])
ggsave("images/feat_ea_plot.png")

#Fusion Heat
fh_x<- ncol(feat_fusion_heat) - 1
fh_col_ind <- seq(1, fh_x, 1)

feat_fh_plot <- lapply(fh_col_ind, function(fh_feat){
  feat_fusion_heat %>%
    ggplot(aes(feat_fusion_heat[,fh_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_fusion_heat[fh_feat])) +
    xlab(colnames(feat_fusion_heat[fh_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_fh_plot[[1]]|feat_fh_plot[[2]])/(feat_fh_plot[[3]]|feat_fh_plot[[4]])/(feat_fh_plot[[5]]|feat_fh_plot[[6]])/(feat_fh_plot[[7]]|feat_fh_plot[[8]])/(feat_fh_plot[[9]]|feat_fh_plot[[10]])
ggsave("images/feat_fh_plot.png")

#Thermal Conductivity
tc_x<- ncol(feat_thermal_cond) - 1
tc_col_ind <- seq(1, tc_x, 1)

feat_tc_plot <- lapply(tc_col_ind, function(tc_feat){
  feat_thermal_cond %>%
    ggplot(aes(feat_thermal_cond[,tc_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_thermal_cond[tc_feat])) +
    xlab(colnames(feat_thermal_cond[tc_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_tc_plot[[1]]|feat_tc_plot[[2]])/(feat_tc_plot[[3]]|feat_tc_plot[[4]])/(feat_tc_plot[[5]]|feat_tc_plot[[6]])/(feat_tc_plot[[7]]|feat_tc_plot[[8]])/(feat_tc_plot[[9]]|feat_tc_plot[[10]])
ggsave("images/feat_tc_plot.png")

#Valence
val_x<- ncol(feat_valence) - 1
val_col_ind <- seq(1, val_x, 1)

feat_val_plot <- lapply(val_col_ind, function(val_feat){
  feat_valence %>%
    ggplot(aes(feat_valence[,val_feat],critical_temp)) +
    geom_point(alpha = 0.05) +
    ggtitle(colnames(feat_valence[val_feat])) +
    xlab(colnames(feat_valence[val_feat])) +
    geom_smooth(method = "loess", method.args = list(degree=1))
})
(feat_val_plot[[1]]|feat_val_plot[[2]])/(feat_val_plot[[3]]|feat_val_plot[[4]])/(feat_val_plot[[5]]|feat_val_plot[[6]])/(feat_val_plot[[7]]|feat_val_plot[[8]])/(feat_val_plot[[9]]|feat_val_plot[[10]])
ggsave("images/feat_val_plot.png")


#############
#With this in mind, models will be constructed using caret.
#These will at first use a range of modeling types, linear and non linear to determine best approach before 
#focusing on the more successful

#The features used in the model will be added stepwise
#Model accuracy will be assessed by calculation of RMSE

###############
#Housekeeping
rm(am_col_ind)
rm(am_x)
rm(ar_col_ind)
rm(ar_x)
rm(cor_rng)
rm(den_col_ind)
rm(den_x)
rm(ea_col_ind)
rm(ea_x)
rm(fh_col_ind)
rm(fh_x)
rm(fie_col_ind)
rm(fie_x)
rm(tc_col_ind)
rm(tc_x)
rm(val_col_ind)
rm(val_x)
rm(x)

###############
#Splitting the data for training and testing
#75% of the feature_data will be used for training, the remaining 25% will be used to test model accuracy

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = feature_data$critical_temp, times = 1, p = 0.25, list = FALSE)
feat_train <- feature_data[-test_index,]
feat_test <- feature_data[test_index,]

###############
#10 fold cross validation will be used in training
control <- trainControl(method = "cv", number = 10, p = 0.8)

#Linear models
lm_types <- c("lm", "BstLm", "gamLoess", "glmboost", "knn", "svmLinear")

set.seed(1, sample.kind="Rounding")
lm_fits <- lapply(lm_types, function(model){
  print(model)
  train(critical_temp ~ wtd_std_ThermalConductivity, method = model, data = feat_train, trControl = control)
})

#RMSE gathering (minimum RMSE retained from models that record more than one since this is a comparative test and 
#does not represent the final model)
n_models <- seq(1:length(lm_types))
lm_rmses <- sapply(n_models, function(m_number){
  min(lm_fits[[m_number]][["results"]][["RMSE"]])
})

lm_rmses <- as.data.frame(lm_rmses) %>%
  mutate(model_name = lm_types)
colnames(lm_rmses) <- c("RMSE", "model_name")

print(lm_rmses)

#Non-linear models
nlm_types <- c("rf", "gamboost", "bagEarth", "brnn", "xgbTree")

set.seed(1, sample.kind="Rounding")
nlm_fits <- lapply(nlm_types, function(model){
  print(model)
  train(critical_temp ~ wtd_std_ThermalConductivity, method = model, data = feat_train, trControl = control)
})

#RMSE gathering (minimum RMSE retained from models that record more than one since this is a comparative test and 
#does not represent the final model)
 n_models <- seq(1:length(nlm_types))
nlm_rmses <- sapply(n_models, function(m_number){
  min(nlm_fits[[m_number]][["results"]][["RMSE"]])
})

nlm_rmses <- as.data.frame(nlm_rmses) %>%
  mutate(model_name = nlm_types)
colnames(nlm_rmses) <- c("RMSE", "model_name")

print(nlm_rmses)

#compare lm and nlm
rbind(lm_rmses, nlm_rmses)

#The knn, random forest and tree based models are most accurate at this stage, a second feature is added to confirm this
#before further random forest and tree based models are added and performance assessed in processing extra features

set.seed(1, sample.kind="Rounding")
lm_fits_2 <- lapply(lm_types, function(model){
  print(model)
  train(critical_temp ~ wtd_std_ThermalConductivity + range_ThermalConductivity, method = model, data = feat_train, trControl = control)
})

n_models <- seq(1:length(lm_types))
lm_rmses_2 <- sapply(n_models, function(m_number){
  min(lm_fits_2[[m_number]][["results"]][["RMSE"]])
})

lm_rmses_2 <- as.data.frame(lm_rmses_2) %>%
  mutate(model_name = lm_types)
colnames(lm_rmses_2) <- c("RMSE", "model_name")

print(lm_rmses_2)

set.seed(1, sample.kind="Rounding")
nlm_fits_2 <- lapply(nlm_types, function(model){
  print(model)
  train(critical_temp ~ wtd_std_ThermalConductivity + range_ThermalConductivity, method = model, data = feat_train, trControl = control)
})

n_models <- seq(1:length(nlm_types))
nlm_rmses_2 <- sapply(n_models, function(m_number){
  min(nlm_fits_2[[m_number]][["results"]][["RMSE"]])
})

nlm_rmses_2 <- as.data.frame(nlm_rmses_2) %>%
  mutate(model_name = nlm_types)
colnames(nlm_rmses_2) <- c("RMSE", "model_name")

print(nlm_rmses_2)

#compare lm and nlm
rbind(lm_rmses_2, nlm_rmses_2)

#Once again, the knn, random forest and tree based models produce the most accurate results.

#parallel processing set up to expedite process
#time elapsed working sequentially
t1 <- system.time(train(critical_temp ~ wtd_std_ThermalConductivity + range_ThermalConductivity, method = "rf", data = feat_train, trControl = control))

#set 4 clusters
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
t4 <- system.time(train(critical_temp ~ wtd_std_ThermalConductivity + range_ThermalConductivity, method = "rf", data = feat_train, trControl = control))
stopCluster(cl)
registerDoSEQ()

print(t1)
print(t4)

#significant improvement in processing time using four cores. Implement in further training

cl <- makePSOCKcluster(4)
registerDoParallel(cl)
#Addition of different knn, random forest and tree based models to training model evaluation

further_types <- c("knn", "kknn", "rf", "ranger", "Rborist", "xgbTree", "xgbDART")
set.seed(1, sample.kind="Rounding")
further_fits <- lapply(further_types, function(model){
  print(model)
  train(critical_temp ~ wtd_std_ThermalConductivity + range_ThermalConductivity + range_atomic_radius, method = model, data = feat_train, trControl = control)
})

n_models <- seq(1:length(further_types))
further_rmses <- sapply(n_models, function(m_number){
  min(further_fits[[m_number]][["results"]][["RMSE"]])
})

further_rmses <- as.data.frame(further_rmses) %>%
  mutate(model_name = further_types)
colnames(further_rmses) <- c("RMSE", "model_name")

print(further_rmses)

stopCluster(cl)
registerDoSEQ()

#All models show improvement, top 10 +ve correlated features modelled. 
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(1, sample.kind="Rounding")
t10pve_fits <- lapply(further_types, function(model){
  print(model)
  train(critical_temp ~ wtd_std_ThermalConductivity + range_ThermalConductivity + range_atomic_radius + std_ThermalConductivity + wtd_entropy_atomic_mass + wtd_entropy_atomic_radius + number_of_elements + range_fie + wtd_std_atomic_radius + entropy_Valence, method = model, data = feat_train, trControl = control)
})

n_models <- seq(1:length(further_types))
t10pve_rmses <- sapply(n_models, function(m_number){
  min(t10pve_fits[[m_number]][["results"]][["RMSE"]])
})

t10pve_rmses <- as.data.frame(t10pve_rmses) %>%
  mutate(model_name = further_types)
colnames(t10pve_rmses) <- c("RMSE", "model_name")

print(t10pve_rmses)

stopCluster(cl)
registerDoSEQ()


#typing each feature name into the model is tedious, tidier/quicker method is to create a duplicate training data 
#table based on row number in the feat_cor table and critical temperature column.
#First nineteen rows show correlation > 0.50

n_cor_feat <- seq(1,19,1)

feature_correlation_index <- sapply(n_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

model_features <- feat_train %>% select(c(all_of(feature_correlation_index), critical_temp))

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(1, sample.kind="Rounding")
t19pve_fits <- lapply(further_types, function(model){
  print(model)
  train(critical_temp ~ ., method = model, data = model_features, trControl = control)
})

n_models <- seq(1:length(further_types))
t19pve_rmses <- sapply(n_models, function(m_number){
  min(t19pve_fits[[m_number]][["results"]][["RMSE"]])
})

t19pve_rmses <- as.data.frame(t19pve_rmses) %>%
  mutate(model_name = further_types)
colnames(t19pve_rmses) <- c("RMSE", "model_name")

print(t19pve_rmses)

stopCluster(cl)
registerDoSEQ()



#Addition of -ve correlations to determine effect of all features with correlations < -0.5 and > 0.5

n_neg_cor_feat <- seq(75,81,1)

feature_neg_correlation_index <- sapply(n_neg_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

model_features <- feat_train %>% select(c(all_of(feature_correlation_index), all_of(feature_neg_correlation_index), critical_temp))

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(1, sample.kind="Rounding")
point5_fits <- lapply(further_types, function(model){
  print(model)
  train(critical_temp ~ ., method = model, data = model_features, trControl = control)
})

n_models <- seq(1:length(further_types))
point5_rmses <- sapply(n_models, function(m_number){
  min(point5_fits[[m_number]][["results"]][["RMSE"]])
})

point5_rmses <- as.data.frame(point5_rmses) %>%
  mutate(model_name = further_types)
colnames(point5_rmses) <- c("RMSE", "model_name")

print(point5_rmses)

stopCluster(cl)
registerDoSEQ()

#knn and xgb models lose accuracy compared to random forest.
#continue using random forest models only.

#adjust to include correlations > 0.4 and < -0.4

rf_types <- c("rf", "ranger", "Rborist")

n_cor_feat <- seq(1,24,1)

feature_correlation_index <- sapply(n_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

n_neg_cor_feat <- seq(70,81,1)

feature_neg_correlation_index <- sapply(n_neg_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

model_features <- feat_train %>% select(c(all_of(feature_correlation_index), all_of(feature_neg_correlation_index), critical_temp))


cl <- makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(1, sample.kind="Rounding")
point4_fits <- lapply(rf_types, function(model){
  print(model)
  train(critical_temp ~ ., method = model, data = model_features, trControl = control)
})

n_models <- seq(1:length(rf_types))
point4_rmses <- sapply(n_models, function(m_number){
  min(point4_fits[[m_number]][["results"]][["RMSE"]])
})

point4_rmses <- as.data.frame(point4_rmses) %>%
  mutate(model_name = rf_types)
colnames(point4_rmses) <- c("RMSE", "model_name")

print(point4_rmses)

stopCluster(cl)
registerDoSEQ()

#Further improvement, add more features
n_cor_feat <- seq(1,33,1)

feature_correlation_index <- sapply(n_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

n_neg_cor_feat <- seq(58,81,1)

feature_neg_correlation_index <- sapply(n_neg_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

model_features <- feat_train %>% select(c(all_of(feature_correlation_index), all_of(feature_neg_correlation_index), critical_temp))

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(1, sample.kind="Rounding")
point3_fits <- lapply(rf_types, function(model){
  print(model)
  train(critical_temp ~ ., method = model, data = model_features, trControl = control)
})

n_models <- seq(1:length(rf_types))
point3_rmses <- sapply(n_models, function(m_number){
  min(point3_fits[[m_number]][["results"]][["RMSE"]])
})

point3_rmses <- as.data.frame(point3_rmses) %>%
  mutate(model_name = rf_types)
colnames(point3_rmses) <- c("RMSE", "model_name")

print(point3_rmses)

stopCluster(cl)
registerDoSEQ()

#rmse improvements continue, but processing time increased and improvements levelling off, drop rf and Rborist models.
#Since boosted models showed early success, construction of dmatrix using xgboost and modelling undertaken to see if
#this or pca will provide more accurate predictions.

feat_train_dmatr <- xgb.DMatrix(data = as.matrix(feat_train[1:81]), label = feat_train$critical_temp)
train_xgb_fit <- xgboost(data = feat_train_dmatr, subsample = 0.70, nrounds = 50, num_parallel_tree = 10)
print(train_xgb_fit)

#Large improvement in RMSE over ranger model.
#Continue with ranger model to see how much RMSE can be improved and allow direct comparison of predictions to assess
#performance at range of critical temperatures 

###############################

#Principal Component Analysis

feat_train_matr <- as.matrix(feat_train[1:81])

feature_data_pca <- prcomp(feat_train_matr, scale = TRUE)
feature_data_pca$rotation

biplot(feature_data_pca, scale = 0)

feature_data_pca_variance <- (feature_data_pca$sdev)^2
feature_data_pca_variance[1:10]
feature_data_pca_variance_prop <- feature_data_pca_variance/sum(feature_data_pca_variance)
feature_data_pca_variance_prop[1:10]

#plot proportion of variance to determine principal components 
plot(feature_data_pca_variance_prop, xlab = "Principal Component", ylab = "Proportion of Variance", type = "b")

plot(cumsum(feature_data_pca_variance_prop), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance", type = "b")

cumsum(feature_data_pca_variance_prop)

#First 12 of the 81 features for predicting critical temperature account for around 90% of the variance

feat_pca_train <- data.table(critical_temp = feat_train$critical_temp, feature_data_pca$x)
feat_pca_train_12 <- feat_pca_train[,1:13]
pca_train_12_ranger <- train(critical_temp ~ ., method = "ranger", data = feat_pca_train_12)
min(pca_train_12_ranger[["results"]][["RMSE"]])
pca_train_12_xgb <- train(critical_temp ~ ., method = "xgbTree", data = feat_pca_train_12)
min(pca_train_12_xgb[["results"]][["RMSE"]])
#
#feat_pca_train <- data.table(feature_data_pca$x[,1:12])
#pred_tar <- factor(feat_train$critical_temp)
#
#fit <- ranger::ranger(critical_temp ~ ., data = feat_pca_train)
#
#feat_test_matr <- as.matrix(feat_test[1:81])
#feat_test_pca <- sweep(feat_test_matr, 2, colMeans(feat_test_matr)) %*% feature_data_pca$rotation
#feat_test_pca <- feat_test_pca[,1:12]

#feat_test_pred <- ranger::predictions(fit, feat_test_pca, class = "class")
#RMSE(feat_test_pred, feat_test$critical_temp)

#high RMSE do not pursue this method any further

#returning to ranger model, add more features to determine if xgboosted matrix can be bettered

n_cor_feat <- seq(1,38,1)

feature_correlation_index <- sapply(n_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

n_neg_cor_feat <- seq(54,81,1)

feature_neg_correlation_index <- sapply(n_neg_cor_feat, function(feat_ind){
  which(colnames(feat_train) == feat_cor[feat_ind,2])
})

model_features <- feat_train %>% select(c(all_of(feature_correlation_index), all_of(feature_neg_correlation_index), critical_temp))


cl <- makePSOCKcluster(4)
registerDoParallel(cl)

point2_ranger_fit <- train(critical_temp ~ ., method = "ranger", data = model_features, trControl = control)

point2_ranger_rmse <- min(point2_ranger_fit[["results"]][["RMSE"]])

point2_ranger_rmse <- as.data.frame(point2_ranger_rmse) %>%
  mutate(model_name = "ranger")
colnames(point2_ranger_rmse) <- c("RMSE", "model_name")

print(point2_ranger_rmse)

stopCluster(cl)
registerDoSEQ()

#Run against full training feature set
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

full_ranger_fit <- train(critical_temp ~ ., method = "ranger", data = feat_train, trControl = control)

full_ranger_rmse <- min(full_ranger_fit[["results"]][["RMSE"]])

full_ranger_rmse <- as.data.frame(full_ranger_rmse) %>%
  mutate(model_name = "ranger")
colnames(full_ranger_rmse) <- c("RMSE", "model_name")

print(full_ranger_rmse)

stopCluster(cl)
registerDoSEQ()

#Doubtful ranger performance would outstrip xgboosted model given change across features and rmse obtained modelling 
#the full feature set

#Predict using xgboosted model on the test set for RMSE calculation
feat_test_dmatr <- xgb.DMatrix(data = as.matrix(feat_test[1:81]), label = feat_test$critical_temp)
xgb_pred <- predict(train_xgb_fit, feat_test_dmatr)
xgb_RMSE <- RMSE(xgb_pred, feat_test$critical_temp)
print(xgb_RMSE)

#Trained Ranger model has lower RMSE than RMSE calculated using xgb on test set. Predict on test set using Ranger 
#model
ranger_pred <- predict(full_ranger_fit, feat_test)
ranger_RMSE <- RMSE(ranger_pred, feat_test$critical_temp)
print(ranger_RMSE)

#Ranger model returns better RMSE than xgb model.
fin_RMSEs <- as.data.table(rbind(ranger_RMSE, xgb_RMSE)) %>% mutate(model_name = c("ranger", "xgBoost")) %>% select(model_name, V1)
colnames(fin_RMSEs) <- c("Model Name", "RMSE")

#Plot predicted temperature against critical temperature for each to determine if critical temperature magnitude is a factor 
#influencing accuracy

ggplot() +
  geom_point(aes(feat_test$critical_temp, ranger_pred), alpha = 0.05) +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Ranger Predicted Critical Temperature over Actual Critical Temperature") +
  xlab("Actual Critical Temperature") +
  ylab("Predicted Critical Temperature")


ggplot() +
  geom_point(aes(feat_test$critical_temp, xgb_pred), alpha = 0.05) +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("xgb Predicted Critical Temperature over Actual Critical Temperature") +
  xlab("Actual Critical Temperature") +
  ylab("Predicted Critical Temperature")

#Ranger model looks to be more accurate at critical temperatures close to zero.

#Can we predict elemental composition of a superconductor of critical temperature x by relating the feature data
#directly to elements/element pairings/clusters themselves and can this lead to determination of structure?

#tidy workspace
rm(t10pve_fits)
rm(t19pve_fits)
rm(feat_pca_train)
rm(feat_pca_train_12)
rm(feat_train_matr)
rm(feat_train_dmatr)
rm(further_fits)
rm(lm_fits)
rm(lm_fits_2)
rm(nlm_fits)
rm(nlm_fits_2)
rm(point2_ranger_fit)
rm(point3_fits)
rm(point4_fits)
rm(point5_fits)

save.image(file = "fin_scr.RData")
