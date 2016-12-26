# Pré-initialisation des variables
predictedValues <- matrix(nrow = 314, ncol = 6)
evolution <- list()
temp_dt <- list()
temp_means <- data.frame(Feature = c(paste0(rep(c(paste0("Coef", 1:4), paste0("Rési", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4)),
                         Fold_1 = numeric(36),
                         Fold_2 = numeric(36),
                         Fold_3 = numeric(36),
                         Fold_Mean = numeric(36),
                         Feature_Mean = numeric(36),
                         Feature_SD = numeric(36))

# Boucle d'entrainement 2 contre 1
for (i in 10:12) {
  
  # Entrainement d'un modèle linéaire
  temp_model <- xgb.train(data = training_xgb[[i]],
                          num_class = 6, # Classification à 6 classes
                          nthread = 1, # 1 coeur utilisé
                          nrounds = 1000000, # Nombre d'itérations de boosting
                          eta = 0.10, # Shrinkage pour le boosting
                          booster = "gblinear", # Type d'entrainement : linéaire ou non-linéaire
                          objective = "multi:softprob", # Gradient/Hessian pour l'optimisation par Gradient Descent
                          eval_metric = "merror", # Inexactitude de la classification
                          maximize = FALSE, # Minimisation de l'erreur
                          early_stopping_rounds = 100, # Arrêt après 100 itérations sans amélioration de la métrique
                          verbose = FALSE, # Sans print des itérations
                          watchlist = list(test = testing_xgb[[i]]), # Estimation sur les données de test
                          callbacks = list(cb.evaluation.log())) # Logging des données d'entrainement pour pouvoir récupérer les métriques
  
  # Enregistrement du log
  evolution[[i - 9]] <- cbind(temp_model$evaluation_log, Fold = rep(13 - i, temp_model$niter))
  
  # Entrainement du meilleur modèle (obtention des meilleurs coefficients)
  temp_model <- xgb.train(data = training_xgb[[i]],
                          num_class = 6, # Classification à 6 classes
                          nthread = 1, # 1 coeur utilisé
                          nrounds = temp_model$best_iteration, # Nombre d'itérations de boosting
                          eta = 0.10, # Shrinkage pour le boosting
                          booster = "gblinear", # Type d'entrainement : linéaire ou non-linéaire
                          objective = "multi:softprob", # Gradient/Hessian pour l'optimisation par Gradient Descent
                          eval_metric = "merror", # Inexactitude de la classification
                          maximize = FALSE, # Minimisation de l'erreur
                          early_stopping_rounds = 99999, # Sans arrêt
                          verbose = FALSE, # Sans print des itérations
                          watchlist = list(test = testing_xgb[[i]]), # Estimation sur les données de test
                          callbacks = list(cb.evaluation.log())) # Logging des données d'entrainement pour pouvoir récupérer les métriques
  
  # Prédiction du modèle linéaire
  predictedValues[folds_test[[i]], ] <- t(matrix(predict(temp_model, testing_xgb[[i]], ntreelimit = 0), nrow = 6))
  
  # Calcul et formattage de l'importance des variables
  temp_importance <- data.table(Feature = temp_means[["Feature"]],
                                matrix(xgb.importance(model = temp_model)$Weight, ncol = 6))
  colnames(temp_importance) <- c("Feature", paste0("Label_", 1:6))
  temp_importance[["Sign"]] <- paste0(ifelse(temp_importance[[2]] >= 0, "+", "-"), ifelse(temp_importance[[3]] >= 0, "+", "-"), ifelse(temp_importance[[4]] >= 0, "+", "-"), ifelse(temp_importance[[5]] >= 0, "+", "-"), ifelse(temp_importance[[6]] >= 0, "+", "-"), ifelse(temp_importance[[7]] >= 0, "+", "-"))
  temp_importance[, 2:7] <- abs(temp_importance[, 2:7, with = FALSE]) 
  temp_means[[14 - i]] <- rowMeans(temp_importance[, 2:7, with = FALSE])
  temp_importance[[paste0("Fold_", 13 - i, "_Mean")]] <- temp_means[[14 - i]]
  
  # Enregistrement sous forme de tableau interactif
  temp_dt[[i - 9]] <- datatable(temp_importance,
        filter = "top", # Filtrage au-dessus de la table
        class = "cell-border stripe", # CSS
        extensions = c("ColReorder",
                       "RowReorder"), # Reordonner manuellement à la main
        options = list(order = list(list(9, "desc")), # Ordonner par défaut par les facteurs ayant le poids le plus gros
                       colReorder = TRUE, # Plugin
                       rowReorder = TRUE)) %>% # Plugin
  formatStyle(paste0("Label_", 1:6),
                  background = styleColorBar(range(temp_importance[, 2:7, with = FALSE]), 'lightblue'), # Couleur bleue pour le coefficient
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle(paste0("Fold_", 13 - i, "_Mean"),
              background = styleColorBar(range(temp_importance[[9]]), 'pink'), # Couleur rose pour la métrique de moyenne
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatRound(columns = c(paste0("Label_", 1:6), paste0("Fold_", 13 - i, "_Mean")),
              digits = 6)
  
}

# Calcul du poids moyen affecté à chaque feature
temp_means[[5]] <- rowMeans(temp_means[, 2:4]) # Poids moyen
temp_means[[6]] <- apply(mini_lm, 2, function(x) {mean(x)}) # Moyenne de la feature dans les données
temp_means[[7]] <- apply(mini_lm, 2, function(x) {sd(x)}) # Ecart-type de la feature dans les données

# Prépration du tableau interactif sur les poids moyens agrégés
temp_dt[[4]] <- datatable(temp_means,
        filter = "top", # Filtrage au-dessus de la table
        class = "cell-border stripe", # CSS
        extensions = c("ColReorder",
                       "RowReorder"), # Reordonner manuellement à la main
        options = list(order = list(list(5, "desc")), # Ordonner par défaut par les facteurs ayant le poids le plus gros en moyenne
                       colReorder = TRUE, # Plugin
                       rowReorder = TRUE)) %>% # Plugin
  formatStyle(paste0("Fold_", 1:3),
                  background = styleColorBar(range(temp_means[, 2:4]), 'lightblue'), # Couleur bleue pour le coefficient
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle("Fold_Mean",
              background = styleColorBar(range(temp_means[[5]]), 'pink'), # Couleur rose pour la métrique de moyenne
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatStyle("Feature_Mean",
              background = styleColorBar(range(temp_means[[6]]), 'lightgreen'), # Couleur verte pour la moyenne des features
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatStyle("Feature_SD",
              background = styleColorBar(range(temp_means[[7]]), 'orange'), # Couleur verte pour l'écart-type des features
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatRound(columns = c(paste0("Fold_", 1:3), "Fold_Mean", "Feature_Mean", "Feature_SD"),
              digits = 6)

# Dépivotage du log
evolution <- rbindlist(evolution)
colnames(evolution) <- c("Iteration", "Exactitude", "Fold")
evolution$Exactitude <- 1 - evolution$Exactitude
evolution$Fold <- as.factor(evolution$Fold)

# Prédiction à partir des probabilités
predictedLabel <- data.frame(Label = group_path$path_ID, Prediction = apply(predictedValues, 1, function(x) {which.max(x)}))

# Affichage de l'évolution de la performance du modèle selon le nombre d'itération, sous forme de plot interactif
ggplotly(ggplot(data = evolution, aes_string(x = "Iteration", y = "Exactitude", group = "Fold", color = "Fold")) + geom_line() + geom_point() + scale_color_brewer(palette = "Set2") + theme_bw() + labs(title = "Evolution de l'exactitude par rapport au nombre d'itérations d'entrainement"), width = 960, height = 720)

# Affichage de la matrice de confusion sous forme de plot interactif
confusion_mat <- expand.grid(Label = 1:6, Prediction = 1:6)
confusion_mat <- merge(confusion_mat, data.table(predictedLabel)[, list(Freq = sum(.N)), by = list(Label, Prediction)], by = c("Label", "Prediction"), all.x = TRUE)
confusion_mat[["Freq"]][is.na(confusion_mat[["Freq"]])] <- 0
ggplotly(ggplot() + geom_rect(data = data.frame(cent = 1:6), size = 2, fill = NA, colour = "black", aes(xmin = cent - 0.5, xmax = cent + 0.5, ymin = cent - 0.5, ymax = cent + 0.5)) + geom_tile(data = confusion_mat, aes_string(x = "Label", y = "Prediction", fill = "Freq")) + geom_text(data = confusion_mat, aes_string(x = "Label", y = "Prediction", label = "Freq")) + scale_x_discrete(name = "Trajectoire Réelle") + scale_y_discrete(name = "Trajectoire Prédite") + scale_fill_gradientn(colours = rev(brewer.pal_extended(3, "PiYG"))) + labs(title = "Matrice de Confusion de la Trajectoire", fill = "Fréquence"), width = 960, height = 720)

# Affichage des tables à la fin car le formattage possède un bug inhérent lorsqu'on a plusieurs datatables (DT) dans le même chunk
# htmltools::tagList(temp_dt[[3]], temp_dt[[2]], temp_dt[[1]], temp_dt[[4]])