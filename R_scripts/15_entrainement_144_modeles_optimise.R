# Où sauvegarder les fichiers ?
file_tag <- "4_models/"
file_h2o <- "4_models"

# Boucle d'évaluation
for (i in 1:12) {
  
  # Entrainement du modèle de régression logistique (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gblinear", # Linéaire
                                  nrounds = 1000000, # Arrêté au meilleur résultat
                                  num_parallel_trees = 1)
  xgb.dump(model = temp_model, # Modèle à enregistrer
           fname = paste0(file_tag, "xgb_glm_", sprintf("%02d", i), ".json"), # Où enregistrer le modèle ?
           with_stats = TRUE, # Enregistrement des statistiques si modèle gbtree
           dump_format = "json") # Dump au format json, ré-utilisable
  accuracy[i, 2] <- 1 - temp_model$evaluation_log[[2]][temp_model$best_iteration] # Récupération du meilleur résultat
  
  # Entrainement du modèle d'arbre de décision (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gbtree", # Non-linéaire
                                  nrounds = 1, # Un seul arbre
                                  num_parallel_trees = 1)
  xgb.dump(model = temp_model, # Modèle à enregistrer
           fname = paste0(file_tag, "xgb_dt_", sprintf("%02d", i), ".json"), # Où enregistrer le modèle ?
           with_stats = TRUE, # Enregistrement des statistiques si modèle gbtree
           dump_format = "json") # Dump au format json, ré-utilisable
  accuracy[i, 3] <- 1 - temp_model$evaluation_log[[2]][1] # Récupération du meilleur résultat
  
  # Entrainement du modèle de Random Forest (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gbtree", # Non-linéaire
                                  nrounds = 1, # Une seule itération
                                  num_parallel_trees = 200) # De 200 arbres
  xgb.dump(model = temp_model, # Modèle à enregistrer
           fname = paste0(file_tag, "xgb_rf_", sprintf("%02d", i), ".json"), # Où enregistrer le modèle ?
           with_stats = TRUE, # Enregistrement des statistiques si modèle gbtree
           dump_format = "json") # Dump au format json, ré-utilisable
  accuracy[i, 4] <- 1 - temp_model$evaluation_log[[2]] # Récupération du meilleur résultat
  
  # Entrainement du modèle d'arbre de décision boosté avec protection contre l'overfitting (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gbtree", # Non-linéaire
                                  nrounds = 1000000, # Arrêté au meilleur résultat
                                  num_parallel_trees = 1)
  xgb.dump(model = temp_model, # Modèle à enregistrer
           fname = paste0(file_tag, "xgb_gbt_", sprintf("%02d", i), ".json"), # Où enregistrer le modèle ?
           with_stats = TRUE, # Enregistrement des statistiques si modèle gbtree
           dump_format = "json") # Dump au format json, ré-utilisable
  accuracy[i, 5] <- 1 - temp_model$evaluation_log[[2]][temp_model$best_iteration] # Récupération du meilleur résultat
  
  # Entrainement du modèle de régression logistique (h2o)
  temp_model <- h2o.glm(y = 1,
                        training_frame = training_h2o[[i]],
                        validation_frame = testing_h2o[[i]],
                        model_id = paste0("h2o_glm_", sprintf("%02d", i)), # Nom du modèle
                        max_iterations = 100, # 100 itérations d'optimisation
                        solver = "IRLSM", # Solveur par défaut
                        standardize = FALSE, # Pas de standardisation puisque [-1, 1]
                        family = "multinomial", # Classification multi-classe
                        seed = 0, # Reproduction des résultats
                        intercept = TRUE)
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 6] <- temp_model@model$validation_metrics@metrics$hit_ratio_table[1, 2]
  
  # Entrainement du modèle d'arbre de décision (h2o)
  temp_model <- h2o.randomForest(y = 1,
                                 training_frame = training_h2o[[i]],
                                 validation_frame = testing_h2o[[i]],
                                 model_id = paste0("h2o_dt_", sprintf("%02d", i)), # Nom du modèle
                                 sample_rate = 1, # Toutes les observations seront prises en compte pour le seul arbre de décision
                                 mtries = sum(best_weights$optimizer$discrete), # Toutes les features seront prises en compte pour le seul arbre de décision
                                 ntrees = 1, # Un seul arbre
                                 seed = 0) # Reproduction des résultats
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 7] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du modèle de Random Forest (h2o)
  temp_model <- h2o.randomForest(y = 1,
                                 training_frame = training_h2o[[i]],
                                 validation_frame = testing_h2o[[i]],
                                 model_id = paste0("h2o_rf_", sprintf("%02d", i)), # Nom du modèle
                                 sample_rate = 0.632, # Bootstrapping .632 pour chaque arbre de décision
                                 mtries = -1, # sqrt(36) features seront prises en compte pour chaque arbre de décision
                                 ntrees = 200, # 200 arbres
                                 seed = 0) # Reproduction des résultats
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 8] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du modèle d'arbre de décision boosté avec protection contre l'overfitting (h2o)
  temp_model <- h2o.gbm(y = 1,
                      training_frame = training_h2o[[i]],
                      validation_frame = testing_h2o[[i]],
                      model_id = paste0("h2o_gbt_", sprintf("%02d", i)), # Nom du modèle
                      distribution = "multinomial", # Classification multi-classe
                      sample_rate = 1, # Pas de processus stochastique
                      ntrees = 100, # 100 itérations de boosting au maximum
                      score_each_iteration = TRUE, # Noter la valeur de chaque itération
                      stopping_rounds = 10, # Arrêt après 10 itérations sans amélioraton de la métrique
                      stopping_metric = "misclassification", # Surveiller l'inexactitude de la classification pour l'arrêt
                      stopping_tolerance = 0.00001, # Arrêter lorsque la métrique stagne de 0.001%
                      seed = 0) # Reproduction des résultats
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 9] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du réseau de neurones à architecture 32x6 + ReLU (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_32x6_ReLU_", sprintf("%02d", i)), # Nom du modèle
                             activation = "Rectifier", # ReLU
                             hidden = 32) # Architecture 32x6
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 10] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du réseau de neurones à architecture 32x6 + Tanh (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_32x6_Tanh_", sprintf("%02d", i)), # Nom du modèle
                             activation = "Tanh", # "Sigmoide"
                             hidden = 32) # Architecture 32x6
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 11] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du réseau de neurones à architecture 16x16x6 + ReLU (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_16x16x6_ReLU_", sprintf("%02d", i)), # Nom du modèle
                             activation = "Rectifier", # ReLU
                             hidden = c(16, 16)) # Architecture 16x16x6
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 12] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du réseau de neurones à architecture 16x16x6 + Tanh (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_16x16x6_Tanh_", sprintf("%02d", i)), # Nom du modèle
                             activation = "Tanh", # "Sigmoide"
                             hidden = c(16, 16)) # Architecture 16x16x6
  h2o.download_pojo(temp_model, # Modèle à enregistrer
                    path = file_h2o, # Où enregistrer le modèle ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 13] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
}