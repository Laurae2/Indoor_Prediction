# O� sauvegarder les fichiers ?
file_tag <- "1_models/"
file_h2o <- "1_models"

xgb_dynamic_train <- function(train, test, booster, nrounds, num_parallel_trees) {
  
  # Fixation du seed du g�n�rateur de nombres al�atoires pour qu'on puisse reproduire les r�sultats sur d'autres machines de mani�re exacte
  set.seed(11111)
  
  # Entrainement du mod�le
  return(xgb.train(data = train,
                   num_class = 6, # Classification � 6 classes
                   nthread = 1, # 1 coeur utilis�
                   nrounds = nrounds, # Nombre d'it�rations de boosting
                   num_parallel_trees = num_parallel_trees, # Nombre d'arbres pour le mode Random Forest
                   subsample = ifelse(num_parallel_trees > 1, 0.632, 1), # Bootstrap des donn�es pour l'�chantillonnage en mode Random Forest
                   eta = 0.10, # Shrinkage pour le boosting
                   booster = booster, # Type d'entrainement : lin�aire ou non-lin�aire
                   objective = "multi:softprob", # Gradient/Hessian pour l'optimisation par Gradient Descent
                   eval_metric = "merror", # Inexactitude de la classification
                   maximize = FALSE, # Minimisation de l'erreur
                   early_stopping_rounds = 100, # Arr�t apr�s 100 it�rations sans am�lioration de la m�trique
                   verbose = FALSE, # Sans print des it�rations
                   watchlist = list(test = test), # Estimation sur les donn�es de test
                   callbacks = list(cb.evaluation.log()))) # Logging des donn�es d'entrainement pour pouvoir r�cup�rer les m�triques
}

h2o_nn_train <- function(train, test, model_id, activation, hidden) {
  
  return(temp_model <- h2o.deeplearning(y = 1, # Label = 1�re colonne
                                        training_frame = train,
                                        validation_frame = test,
                                        model_id = model_id, # Nom du mod�le
                                        standardize = FALSE, # Pas de standardisation des donn�es, puisque [-1, 1]
                                        activation = activation, # Activation finale du r�seau de neurones
                                        hidden = hidden, # Architecture du r�seau de neurones
                                        epochs = 100, # Nombre de passes
                                        loss = "CrossEntropy", # Optimisation Softmax
                                        distribution = "multinomial", # Classification multi-class
                                        stopping_rounds = 10, # Arr�t apr�s 10 it�rations sans am�lioration sp�cifique
                                        stopping_metric = "misclassification", # Minimisation de l'erreur
                                        stopping_tolerance = 0.00001, # Tol�rance maximale de 0.001% de stagnation de l'erreur
                                        reproducible = TRUE, # Tentative de r�sultats reproductibles
                                        seed = 0)) # Reproduction des r�sultats
  
}

# Boucle d'�valuation
for (i in 1:12) {
  
  # Entrainement du mod�le de r�gression logistique (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gblinear", # Lin�aire
                                  nrounds = 1000000, # Arr�t� au meilleur r�sultat
                                  num_parallel_trees = 1)
  xgb.dump(model = temp_model, # Mod�le � enregistrer
           fname = paste0(file_tag, "xgb_glm_", sprintf("%02d", i), ".json"), # O� enregistrer le mod�le ?
           with_stats = TRUE, # Enregistrement des statistiques si mod�le gbtree
           dump_format = "json") # Dump au format json, r�-utilisable
  accuracy[i, 2] <- 1 - temp_model$evaluation_log[[2]][temp_model$best_iteration] # R�cup�ration du meilleur r�sultat
  
  # Entrainement du mod�le d'arbre de d�cision (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gbtree", # Non-lin�aire
                                  nrounds = 1, # Un seul arbre
                                  num_parallel_trees = 1)
  xgb.dump(model = temp_model, # Mod�le � enregistrer
           fname = paste0(file_tag, "xgb_dt_", sprintf("%02d", i), ".json"), # O� enregistrer le mod�le ?
           with_stats = TRUE, # Enregistrement des statistiques si mod�le gbtree
           dump_format = "json") # Dump au format json, r�-utilisable
  accuracy[i, 3] <- 1 - temp_model$evaluation_log[[2]][1] # R�cup�ration du meilleur r�sultat
  
  # Entrainement du mod�le de Random Forest (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gbtree", # Non-lin�aire
                                  nrounds = 1, # Une seule it�ration
                                  num_parallel_trees = 200) # De 200 arbres
  xgb.dump(model = temp_model, # Mod�le � enregistrer
           fname = paste0(file_tag, "xgb_rf_", sprintf("%02d", i), ".json"), # O� enregistrer le mod�le ?
           with_stats = TRUE, # Enregistrement des statistiques si mod�le gbtree
           dump_format = "json") # Dump au format json, r�-utilisable
  accuracy[i, 4] <- 1 - temp_model$evaluation_log[[2]] # R�cup�ration du meilleur r�sultat
  
  # Entrainement du mod�le d'arbre de d�cision boost� avec protection contre l'overfitting (xgboost)
  temp_model <- xgb_dynamic_train(train = training_xgb[[i]],
                                  test = testing_xgb[[i]],
                                  booster = "gbtree", # Non-lin�aire
                                  nrounds = 1000000, # Arr�t� au meilleur r�sultat
                                  num_parallel_trees = 1)
  xgb.dump(model = temp_model, # Mod�le � enregistrer
           fname = paste0(file_tag, "xgb_gbt_", sprintf("%02d", i), ".json"), # O� enregistrer le mod�le ?
           with_stats = TRUE, # Enregistrement des statistiques si mod�le gbtree
           dump_format = "json") # Dump au format json, r�-utilisable
  accuracy[i, 5] <- 1 - temp_model$evaluation_log[[2]][temp_model$best_iteration] # R�cup�ration du meilleur r�sultat
  
  # Entrainement du mod�le de r�gression logistique (h2o)
  temp_model <- h2o.glm(y = 1,
                        training_frame = training_h2o[[i]],
                        validation_frame = testing_h2o[[i]],
                        model_id = paste0("h2o_glm_", sprintf("%02d", i)), # Nom du mod�le
                        max_iterations = 100, # 100 it�rations d'optimisation
                        solver = "IRLSM", # Solveur par d�faut
                        standardize = FALSE, # Pas de standardisation puisque [-1, 1]
                        family = "multinomial", # Classification multi-classe
                        seed = 0, # Reproduction des r�sultats
                        intercept = TRUE)
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 6] <- temp_model@model$validation_metrics@metrics$hit_ratio_table[1, 2]
  
  # Entrainement du mod�le d'arbre de d�cision (h2o)
  temp_model <- h2o.randomForest(y = 1,
                                 training_frame = training_h2o[[i]],
                                 validation_frame = testing_h2o[[i]],
                                 model_id = paste0("h2o_dt_", sprintf("%02d", i)), # Nom du mod�le
                                 sample_rate = 1, # Toutes les observations seront prises en compte pour le seul arbre de d�cision
                                 mtries = 36, # Toutes les features seront prises en compte pour le seul arbre de d�cision
                                 ntrees = 1, # Un seul arbre
                                 seed = 0) # Reproduction des r�sultats
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 7] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du mod�le de Random Forest (h2o)
  temp_model <- h2o.randomForest(y = 1,
                                 training_frame = training_h2o[[i]],
                                 validation_frame = testing_h2o[[i]],
                                 model_id = paste0("h2o_rf_", sprintf("%02d", i)), # Nom du mod�le
                                 sample_rate = 0.632, # Bootstrapping .632 pour chaque arbre de d�cision
                                 mtries = -1, # sqrt(36) features seront prises en compte pour chaque arbre de d�cision
                                 ntrees = 200, # 200 arbres
                                 seed = 0) # Reproduction des r�sultats
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 8] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du mod�le d'arbre de d�cision boost� avec protection contre l'overfitting (h2o)
  temp_model <- h2o.gbm(y = 1,
                      training_frame = training_h2o[[i]],
                      validation_frame = testing_h2o[[i]],
                      model_id = paste0("h2o_gbt_", sprintf("%02d", i)), # Nom du mod�le
                      distribution = "multinomial", # Classification multi-classe
                      sample_rate = 1, # Pas de processus stochastique
                      ntrees = 100, # 100 it�rations de boosting au maximum
                      score_each_iteration = TRUE, # Noter la valeur de chaque it�ration
                      stopping_rounds = 10, # Arr�t apr�s 10 it�rations sans am�lioraton de la m�trique
                      stopping_metric = "misclassification", # Surveiller l'inexactitude de la classification pour l'arr�t
                      stopping_tolerance = 0.00001, # Arr�ter lorsque la m�trique stagne de 0.001%
                      seed = 0) # Reproduction des r�sultats
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 9] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du r�seau de neurones � architecture 32x6 + ReLU (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_32x6_ReLU_", sprintf("%02d", i)), # Nom du mod�le
                             activation = "Rectifier", # ReLU
                             hidden = 32) # Architecture 32x6
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 10] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du r�seau de neurones � architecture 32x6 + Tanh (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_32x6_Tanh_", sprintf("%02d", i)), # Nom du mod�le
                             activation = "Tanh", # "Sigmoide"
                             hidden = 32) # Architecture 32x6
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 11] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du r�seau de neurones � architecture 16x16x6 + ReLU (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_16x16x6_ReLU_", sprintf("%02d", i)), # Nom du mod�le
                             activation = "Rectifier", # ReLU
                             hidden = c(16, 16)) # Architecture 16x16x6
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 12] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
  # Entrainement du r�seau de neurones � architecture 16x16x6 + Tanh (h2o)
  temp_model <- h2o_nn_train(train = training_h2o[[i]],
                             test = testing_h2o[[i]],
                             model_id = paste0("h2o_nn_16x16x6_Tanh_", sprintf("%02d", i)), # Nom du mod�le
                             activation = "Tanh", # "Sigmoide"
                             hidden = c(16, 16)) # Architecture 16x16x6
  h2o.download_pojo(temp_model, # Mod�le � enregistrer
                    path = file_h2o, # O� enregistrer le mod�le ?
                    get_jar = FALSE) # Pas de fichier .jar
  accuracy[i, 13] <- 1 - min(temp_model@model$scoring_history$validation_classification_error, na.rm = TRUE)
  
}