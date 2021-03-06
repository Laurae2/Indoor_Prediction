# Chargement des scores enregistr�s avec nettoyage
accuracy1 <- t(fread("scores/1_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])
accuracy2 <- t(fread("scores/2_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])
accuracy3 <- t(fread("scores/3_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])
accuracy4 <- t(fread("scores/4_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])

# Agr�gation des scores
accuracy_agg <- data.table(Defaut = c(accuracy1[1:2], mean(accuracy1[9:10]), mean(accuracy1[11:12]), accuracy1[3:8]),
                           Approximation = c(accuracy2[1:2], mean(accuracy2[9:10]), mean(accuracy2[11:12]), accuracy2[3:8]),
                           Inversement = c(accuracy3[1:2], mean(accuracy3[9:10]), mean(accuracy3[11:12]), accuracy3[3:8]),
                           Selection = c(accuracy4[1:2], mean(accuracy4[9:10]), mean(accuracy4[11:12]), accuracy4[3:8]))
accuracy_agg[["Evolution"]] <- apply(accuracy_agg, 1, function(x) {max(x) - min(x)})
row.names(accuracy_agg) <- c("R�gression logistique (xgb)",
                             "R�gression logistique (h2o)",
                             "R�seau de neurones 32x6 (h2o)",
                             "R�seau de neurones 16x16x6 (h2o)",
                             "Arbre de d�cision (xgb)",
                             "Arbre de d�cision (h2o)",
                             "For�t al�atoire (xgb)",
                             "For�t al�atoire (h2o)",
                             "Arbres boost�s (xgb)",
                             "Arbres boost�s (h2o)")

# Affichage des scores dans un tableau interactif
datatable(accuracy_agg,
          filter = "top", # Filtrage au-dessus de la table
          class = "cell-border stripe", # CSS
          extensions = c("ColReorder",
                         "RowReorder"), # Reordonner manuellement � la main
          options = list(pageLength = 10, # Page affichant 10 lignes
                         colReorder = TRUE, # Plugin
                         rowReorder = TRUE)) %>% # Plugin
  formatStyle(c("Defaut", "Approximation", "Inversement", "Selection"),
                  background = styleColorBar(c(0, max(accuracy_agg[["Selection"]])), 'lightgreen'), # Couleur vert clair pour les m�triques par fold
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle(c("Evolution"),
                  background = styleColorBar(c(0, max(accuracy_agg[["Evolution"]])), 'pink'), # Couleur rose pour l'�volution de la m�trique
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatPercentage(columns = c("Defaut", "Approximation", "Inversement", "Selection", "Evolution"),
              digits = 4)