CE_Features <- function(x, y, train, test) {
  
  # Pr�-initialisation de certaines variables
  to_keep <- as.boolean(y)
  iters <<- iters + 1
  
  # Au moins une feature ?
  if (sum(to_keep) > 0) {
    
    error <- numeric(3)
    lloss <- numeric(3)
    
    for (i in 10:12) {
      
      temp_model <- xgb.train(data = xgb.DMatrix(data = as.matrix(train[[i]][, which(to_keep)]), label = train[[i]][["Label"]]), # Donn�es d'entrainement
                              watchlist = list(test = xgb.DMatrix(data = as.matrix(test[[i]][, which(to_keep)]), label = test[[i]][["Label"]])), # Donn�es de validation
                              num_class = 6, # 6 classe de classification
                              nthread = 1, # 1 thread pour la reproduction des r�sultats
                              nrounds = 1000, # 1000 it�rations, o� arr�t pr�matur�
                              alpha = x[1], # R�gularisation L1 (Lasso)
                              lambda = x[2], # R�gularisation L2 (Ridge)
                              lambda_bias = x[3], # R�gularisation L2 du biais (Ridge)
                              eta = 0.10, # Shrinkage pour le boosting
                              booster = "gblinear", # Type d'entrainement : lin�aire ou non-lin�aire
                              objective = "multi:softprob", # Gradient/Hessian pour l'optimisation par Gradient Descent
                              eval_metric = "mlogloss", # Perte logiarthmique de la classification (cette m�trique est optimis�e par xgboost)
                              eval_metric = "merror", # Inexactitude de la classification
                              maximize = FALSE, # Minimisation de l'erreur
                              early_stopping_rounds = 50, # Arr�t apr�s 50 it�rations sans am�lioration de la m�trique
                              verbose = FALSE, # Sans print des it�rations
                              callbacks = list(cb.evaluation.log())) # Logging des donn�es d'entrainement pour pouvoir r�cup�rer les m�triques)
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
    error_list[iters, 12:14] <<- as.numeric(x) # Hyperparam�tres
    error_list[iters, 15:50] <<- as.numeric(to_keep) # Features utilis�es
    error_list[iters, 51] <<- 1
    
    # Le r�sultat est-il meilleur ? (pour le logging en temps r�el)
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
    
    # Logging en temps r�el
    cat(star, "[", format(Sys.time(), "%X"), "] Pass ", sprintf("%05d", iters), ": Error=", sprintf("%.05f", error), " - Loss=", sprintf("%.07f", lloss), " - Score=", sprintf("%.07f", score), " - feats=", sprintf("%04d", sum(to_keep)), " - alpha=", sprintf("%07.05f", x[1]), ", lambda=", sprintf("%07.05f", x[2]), ", lambda_bias=", sprintf("%07.05f", x[3]), "\n", sep = "", file = "optim/log.txt", append = TRUE)
    return(score)
    
  } else {
    
    # Logging en temps r�el
    cat("(    -     -    ) [", format(Sys.time(), "%X"), "] Pass ", sprintf("%05d", iters), ": failed\n", sep = "", file = "optim/log.txt", append = TRUE)
    return(9.9999)
    
  }
  
}

# O� sauvegarder les fichiers ?
file_tag <- "4_data/"

# Cr�ation des donn�es d'entrainement et de validation
for (i in 1:12) {
  
  # Cr�ation des donn�es d'entrainement et de validation
  training_data[[i]][["Label"]] <- group_path$path_ID[folds_train[[i]]] - 1
  testing_data[[i]][["Label"]] <- group_path$path_ID[folds_test[[i]]] - 1
  
}

# Param�tres de l'optimiseur
cont_opt <- list(mean = c(1, 1, 1), # D�bute avec en moyenne, Alpha=1, Lambda=1, Lambda_bias=1
                 sd = c(1, 1, 1), # D�bute avec en �cart-type, Alpha=1, Lambda=1, Lambda_bias=1
                 conMat = rbind(diag(3), -diag(3)), # Optimisation lin�aire conditionn�e par la matrice du simplexe
                 conVec = c(5, 5, 5, 0, 0, 0), # 0<=alpha<=5, 0<=Lambda<=5, 0<=Lambda_bias<=5
                 sdThr = 0.1) # On suppose les hyperparam�tres converg�s lorsque tous les �cart-types sont en-dessous de 0.1
p0 <- list() # Pr�-initaisliation de la liste pour les variables discr�tes
for (i in 1:36) {p0 <- c(p0, list(c(0.72, 0.28)))} # On souhaite 50% des features � la fin en moyenne, � moins que certaines variables ont une importance telle qu'elles ne peuvent �tre omises et seront forc�ment s�lectionn�es
disc_opt <- list(probs = p0,
                 smoothProb = 1.00, # On va tenter de converger rapidement ici pour la r�alistion d'un proof of concept, mais sinon avec plus de temps on pourra r�aliser un shrinkage de 5% de la probabilit� �lite � chaque it�ration (smoothProb = 0.95)
                 probThr = 0.0001) # On suppose la s�lection de features converg� lorsque toutes les probabilit�s sont en-dessous de 0.0001
n_family <- 250 # Le nombre d'estimations par it�ration de l'optimiseur
elite <- 0.1 # Le nombre d'�lites par it�ration qui dictent la loi dans l'entropie crois�e
iterations <- 21 # Le nombre d'it�rations d'optimisation (plus un pour l'it�ration d'initialisation)
early_stop <- 5 # Arr�t pr�matur� lorsque la fonction de perte (ici l'inexactitude de la classification) ne diminue pas apr�s X it�rations

# Pr�-initialisation de la variable de logging
iters <- 0 # Suivi de l'it�ration
best_error <- 1 # Suivi de la perte (inexactitude), initialis� � une tr�s mauvaise valeur possible
best_lloss <- 9.9999 # Suivi de la perte (logarithmique), initialis� � une tr�s mauvaise valeur possible
best_score <- 9.9999 # Suivi de la perte (inexactitude * logarithmique), initialis� � une tr�s mauvaise valeur possible
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
colnames(error_list)[15:50] <- c(paste0(rep(c(paste0("Coef", 1:4), paste0("R�si", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4))

set.seed(0) # Fixation du seed al�atoire pour des r�sultats qui puissent �tre reproduits

# Optimisation par entropie crois�e
best_weights <- CEoptim(CE_Features,
                        f.arg = list(train = training_data, # Donn�es d'entrainement
                                     test = testing_data), # Donn�es de validation
                        maximize = FALSE, # Minimisation du probl�me
                        continuous = cont_opt,
                        discrete = disc_opt,
                        N = n_family,
                        rho = elite,
                        verbose = TRUE,
                        iterThr = iterations - 1,
                        noImproveThr = early_stop)

# Enregistrement du log d�taill�
fwrite(error_list, "optim/error_raw.csv")

# Enregistrement du log d�taill� et nettoy� des �l�ments inutiles
error_list <- error_list[error_list$Logging == 1, ]
fwrite(error_list, "optim/error_clean.csv")

# Enregistrement de la varible contenant l'optimisation
saveRDS(best_weights, "optim/optimized.rds")

# Affichage des r�sultats
x <- best_weights$optimizer$continuous # R�cup�ration des hyperparam�tres
y <- best_weights$optimizer$discrete # R�cup�ration des features s�lectionn�es
cat("  \nL'optimiseur a trouv� :  \n  - Meilleure Inexactitude = ", best_error, "  \n  - Meilleure Perte Logarithmique = ", best_lloss, "\n  - alpha = ", x[1], "  \n  - lambda = ", x[2], "  \n - lambda_bias = ", x[3], "  \n  - features = ", sum(as.boolean(y)), " (binary = ", paste(y, collapse = ""), ")  \n  \nFeatures utilis�es :  \n", sep = "")
dput(c(paste0(rep(c(paste0("Coef", 1:4), paste0("R�si", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4))[as.boolean(y)]) # Affichage des noms des features