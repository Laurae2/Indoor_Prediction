CE_Features <- function(x, y, train, test) {
  
  # Pré-initialisation de certaines variables
  to_keep <- as.boolean(y)
  iters <<- iters + 1
  
  # Au moins une feature ?
  if (sum(to_keep) > 0) {
    
    error <- numeric(3)
    lloss <- numeric(3)
    
    for (i in 10:12) {
      
      temp_model <- xgb.train(data = xgb.DMatrix(data = as.matrix(train[[i]][, which(to_keep)]), label = train[[i]][["Label"]]), # Données d'entrainement
                              watchlist = list(test = xgb.DMatrix(data = as.matrix(test[[i]][, which(to_keep)]), label = test[[i]][["Label"]])), # Données de validation
                              num_class = 6, # 6 classe de classification
                              nthread = 1, # 1 thread pour la reproduction des résultats
                              nrounds = 1000, # 1000 itérations, où arrêt prématuré
                              alpha = x[1], # Régularisation L1 (Lasso)
                              lambda = x[2], # Régularisation L2 (Ridge)
                              lambda_bias = x[3], # Régularisation L2 du biais (Ridge)
                              eta = 0.10, # Shrinkage pour le boosting
                              booster = "gblinear", # Type d'entrainement : linéaire ou non-linéaire
                              objective = "multi:softprob", # Gradient/Hessian pour l'optimisation par Gradient Descent
                              eval_metric = "mlogloss", # Perte logiarthmique de la classification (cette métrique est optimisée par xgboost)
                              eval_metric = "merror", # Inexactitude de la classification
                              maximize = FALSE, # Minimisation de l'erreur
                              early_stopping_rounds = 50, # Arrêt après 50 itérations sans amélioration de la métrique
                              verbose = FALSE, # Sans print des itérations
                              callbacks = list(cb.evaluation.log())) # Logging des données d'entrainement pour pouvoir récupérer les métriques)
      error[13 - i] <- temp_model$evaluation_log$test_merror[temp_model$best_iteration] # Enregistrement du meilleur score (inexactitude)
      lloss[13 - i] <- temp_model$evaluation_log$test_mlogloss[temp_model$best_iteration] # Enregistrement du meilleur score (perte logarithmique)
      
    }
    
    # Enregistrement de l'erreur et logging dans l'environnemnt global
    error_list[iters, 2:4] <<- error # Inexactitude
    error <- mean(error) # Inexactitude moyenne
    error_list[iters, 5] <<- error # Inexactitude moyenne
    error_list[iters, 6:8] <<- lloss # Perte logarithmique
    lloss <- mean(lloss) # Perte logarithmique moyenne
    error_list[iters, 9] <<- lloss # Perte logarithmique moyenne
    score <- error * lloss # Score de perte
    error_list[iters, 10] <<- score # Score de perte
    error_list[iters, 11] <<- sum(to_keep) # Compte de features
    error_list[iters, 12:14] <<- as.numeric(x) # Hyperparamètres
    error_list[iters, 15:50] <<- as.numeric(to_keep) # Features utilisées
    error_list[iters, 51] <<- 1
    
    # Le résultat est-il meilleur ? (pour le logging en temps réel)
    if (error < best_error) {
      best_error <<- error
      star <- "(*** - "
    } else {
      star <- "(    - "
    }
    if (lloss < best_lloss) {
      best_lloss <<- lloss
      star <- paste0(star, "*** - ")
    } else {
      star <- paste0(star, "    - ")
    }
    if (score < best_score) {
      best_score <<- score
      star <- paste0(star, "***) ")
    } else {
      star <- paste0(star, "   ) ")
    }
    
    # Logging en temps réel
    cat(star, "[", format(Sys.time(), "%X"), "] Pass ", sprintf("%05d", iters), ": Error=", sprintf("%.05f", error), " - Loss=", sprintf("%.07f", lloss), " - Score=", sprintf("%.07f", score), " - feats=", sprintf("%04d", sum(to_keep)), " - alpha=", sprintf("%07.05f", x[1]), ", lambda=", sprintf("%07.05f", x[2]), ", lambda_bias=", sprintf("%07.05f", x[3]), "\n", sep = "", file = "optim/log.txt", append = TRUE)
    return(score)
    
  } else {
    
    # Logging en temps réel
    cat("(    -     -    ) [", format(Sys.time(), "%X"), "] Pass ", sprintf("%05d", iters), ": failed\n", sep = "", file = "optim/log.txt", append = TRUE)
    return(9.9999)
    
  }
  
}

# Où sauvegarder les fichiers ?
file_tag <- "4_data/"

# Création des données d'entrainement et de validation
for (i in 1:12) {
  
  # Création des données d'entrainement et de validation
  training_data[[i]][["Label"]] <- group_path$path_ID[folds_train[[i]]] - 1
  testing_data[[i]][["Label"]] <- group_path$path_ID[folds_test[[i]]] - 1
  
}

# Paramètres de l'optimiseur
cont_opt <- list(mean = c(1, 1, 1), # Débute avec en moyenne, Alpha=1, Lambda=1, Lambda_bias=1
                 sd = c(1, 1, 1), # Débute avec en écart-type, Alpha=1, Lambda=1, Lambda_bias=1
                 conMat = rbind(diag(3), -diag(3)), # Optimisation linéaire conditionnée par la matrice du simplexe
                 conVec = c(5, 5, 5, 0, 0, 0), # 0<=alpha<=5, 0<=Lambda<=5, 0<=Lambda_bias<=5
                 sdThr = 0.1) # On suppose les hyperparamètres convergés lorsque tous les écart-types sont en-dessous de 0.1
p0 <- list() # Pré-initaisliation de la liste pour les variables discrètes
for (i in 1:36) {p0 <- c(p0, list(c(0.72, 0.28)))} # On souhaite 50% des features à la fin en moyenne, à moins que certaines variables ont une importance telle qu'elles ne peuvent être omises et seront forcément sélectionnées
disc_opt <- list(probs = p0,
                 smoothProb = 1.00, # On va tenter de converger rapidement ici pour la réalistion d'un proof of concept, mais sinon avec plus de temps on pourra réaliser un shrinkage de 5% de la probabilité élite à chaque itération (smoothProb = 0.95)
                 probThr = 0.0001) # On suppose la sélection de features convergé lorsque toutes les probabilités sont en-dessous de 0.0001
n_family <- 250 # Le nombre d'estimations par itération de l'optimiseur
elite <- 0.1 # Le nombre d'élites par itération qui dictent la loi dans l'entropie croisée
iterations <- 21 # Le nombre d'itérations d'optimisation (plus un pour l'itération d'initialisation)
early_stop <- 5 # Arrêt prématuré lorsque la fonction de perte (ici l'inexactitude de la classification) ne diminue pas après X itérations

# Pré-initialisation de la variable de logging
iters <- 0 # Suivi de l'itération
best_error <- 1 # Suivi de la perte (inexactitude), initialisé à une très mauvaise valeur possible
best_lloss <- 9.9999 # Suivi de la perte (logarithmique), initialisé à une très mauvaise valeur possible
best_score <- 9.9999 # Suivi de la perte (inexactitude * logarithmique), initialisé à une très mauvaise valeur possible
error_list <- data.frame(Iteration = 1:(n_family * iterations),
                         Error_1 = numeric(n_family * iterations),
                         Error_2 = numeric(n_family * iterations),
                         Error_3 = numeric(n_family * iterations),
                         Error_Mean = numeric(n_family * iterations),
                         Loss_1 = numeric(n_family * iterations),
                         Loss_2 = numeric(n_family * iterations),
                         Loss_3 = numeric(n_family * iterations),
                         Loss_Mean = numeric(n_family * iterations),
                         Score = numeric(n_family * iterations),
                         Features_n = numeric(n_family * iterations),
                         Alpha = numeric(n_family * iterations),
                         Lambda = numeric(n_family * iterations),
                         Lambda_bias = numeric(n_family * iterations),
                         matrix(rep(0, n_family * iterations * 36), ncol = 36),
                         Logging = rep(0, n_family * iterations))
colnames(error_list)[15:50] <- c(paste0(rep(c(paste0("Coef", 1:4), paste0("Rési", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4))

set.seed(0) # Fixation du seed aléatoire pour des résultats qui puissent être reproduits

# Optimisation par entropie croisée
best_weights <- CEoptim(CE_Features,
                        f.arg = list(train = training_data, # Données d'entrainement
                                     test = testing_data), # Données de validation
                        maximize = FALSE, # Minimisation du problème
                        continuous = cont_opt,
                        discrete = disc_opt,
                        N = n_family,
                        rho = elite,
                        verbose = TRUE,
                        iterThr = iterations - 1,
                        noImproveThr = early_stop)

# Enregistrement du log détaillé
fwrite(error_list, "optim/error_raw.csv")

# Enregistrement du log détaillé et nettoyé des éléments inutiles
error_list <- error_list[error_list$Logging == 1, ]
fwrite(error_list, "optim/error_clean.csv")

# Enregistrement de la varible contenant l'optimisation
saveRDS(best_weights, "optim/optimized.rds")

# Affichage des résultats
x <- best_weights$optimizer$continuous # Récupération des hyperparamètres
y <- best_weights$optimizer$discrete # Récupération des features sélectionnées
cat("  \nL'optimiseur a trouvé :  \n  - Meilleure Inexactitude = ", best_error, "  \n  - Meilleure Perte Logarithmique = ", best_lloss, "\n  - alpha = ", x[1], "  \n  - lambda = ", x[2], "  \n - lambda_bias = ", x[3], "  \n  - features = ", sum(as.boolean(y)), " (binary = ", paste(y, collapse = ""), ")  \n  \nFeatures utilisées :  \n", sep = "")
dput(c(paste0(rep(c(paste0("Coef", 1:4), paste0("Rési", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4))[as.boolean(y)]) # Affichage des noms des features