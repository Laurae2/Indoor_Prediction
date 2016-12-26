# Où sauvegarder les fichiers ?
file_tag <- "4_data/"

# Initialisation de la variable qui accueillera la précision
accuracy <- data.frame(matrix(nrow = 16, ncol = 13))
colnames(accuracy) <- c("Fold", "xgb_LinearModel", "xgb_DecisionTree", "xgb_RandomForest", "xgb_GradientBoosting", "h2o_LinearModel", "h2o_DecisionTree", "h2o_RandomForest", "h2o_GradientBoosting", "h2o_NN_32x6_ReLU", "h2o_NN_32x6_Soft", "h2o_NN_16x16x6_ReLU", "h2o_NN_16x16x6_Soft")
accuracy[, 1] <- c("Fold_1v2", "Fold_1v3", "Fold_2v1", "Fold_2v3", "Fold_3v1", "Fold_3v2", "Fold_1v23", "Fold_2v13", "Fold_3v12", "Fold_12v3", "Fold_13v2", "Fold_23v1", "Moyenne_1c1", "Moyenne_1c2", "Moyenne_2c1", "Moyenne")

# Initialisation des folds pour la cross-validation
folds_train <- list()
folds_test <- list()
training_data <- list()
testing_data <- list()
training_xgb <- list()
testing_xgb <- list()
training_h2o <- list()
testing_h2o <- list()
combinations_train <- c(list(1, 1, 2, 2, 3, 3), combn(3, 1, simplify = FALSE), combn(3, 2, simplify = FALSE))
combinations_test <- c(list(2, 3, 1, 3, 1, 2), rev(combn(3, 2, simplify = FALSE)), rev(combn(3, 1, simplify = FALSE)))
temp_factors <- as.factor(group_path$path_ID)

# Création des données d'entrainement et de validation
for (i in 1:12) {
  
  # Création des folds d'entrainement et de validation
  folds_train[[i]] <- which(group_room[["dataset_ID"]] %in% combinations_train[[i]])
  folds_test[[i]] <- which(group_room[["dataset_ID"]] %in% combinations_test[[i]])
  
  # Recherche et suppression du label 3 lorsque la salle 1 est isolée (soit en train on enlève en test, soit en test on enlève en train)
  if ((length(combinations_train[[i]]) == 1) & (combinations_train[[i]][1] == 1)) {
    folds_test[[i]] <- folds_test[[i]][group_path$path_ID[folds_test[[i]]] != 3]
  }
  if ((length(combinations_test[[i]]) == 1) & (combinations_test[[i]][1] == 1)) {
    folds_train[[i]] <- folds_train[[i]][group_path$path_ID[folds_train[[i]]] != 3]
  }
  
  # Création des données d'entrainement et de validation
  training_data[[i]] <- mini_lm[folds_train[[i]], which(best_weights$optimizer$discrete == 1)]
  testing_data[[i]] <- mini_lm[folds_test[[i]], which(best_weights$optimizer$discrete == 1)]
  
  # Enregistrement des données CSV
  fwrite(training_data[[i]], paste0(file_tag, "trainNL_", sprintf("%02d", i), ".csv"))
  fwrite(testing_data[[i]], paste0(file_tag, "testNL_", sprintf("%02d", i), ".csv"))
  
  # Transformation des données au format approprié pour xgboost
  training_xgb[[i]] <- xgb.DMatrix(data = as.matrix(training_data[[i]]), label = group_path$path_ID[folds_train[[i]]] - 1)
  testing_xgb[[i]] <- xgb.DMatrix(data = as.matrix(testing_data[[i]]), label = group_path$path_ID[folds_test[[i]]] - 1)
  
  # Dumping des datasets binaires xgboost
  xgb.DMatrix.save(training_xgb[[i]], paste0(file_tag, "trainL_", sprintf("%02d", i), ".data"))
  xgb.DMatrix.save(testing_xgb[[i]], paste0(file_tag, "testL_", sprintf("%02d", i), ".data"))
  
  # Transformation des données au format approprié pour H2O
  training_h2o[[i]] <- as.h2o(cbind(Label = temp_factors[folds_train[[i]]], training_data[[i]]))
  testing_h2o[[i]] <- as.h2o(cbind(Label = temp_factors[folds_test[[i]]], testing_data[[i]]))
  
  # Enregistrement des frames H2O (CSV + Label)
  h2o.exportFile(training_h2o[[i]], paste0(file_tag, "trainL_", sprintf("%02d", i), ".csv"), force = TRUE)
  h2o.exportFile(testing_h2o[[i]], paste0(file_tag, "testL_", sprintf("%02d", i), ".csv"), force = TRUE)
  
}