# Chargement des scores enregistrés avec nettoyage
accuracy1 <- t(fread("scores/1_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])
accuracy2 <- t(fread("scores/2_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])
accuracy3 <- t(fread("scores/3_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])
accuracy4 <- t(fread("scores/4_models.csv")[15, -1][1, c(1, 5, 2, 6, 3, 7, 4, 8, 9:12)])

# Agrégation des scores
accuracy_agg <- data.table(Defaut = c(accuracy1[1:2], mean(accuracy1[9:10]), mean(accuracy1[11:12]), accuracy1[3:8]),
                           Approximation = c(accuracy2[1:2], mean(accuracy2[9:10]), mean(accuracy2[11:12]), accuracy2[3:8]),
                           Inversement = c(accuracy3[1:2], mean(accuracy3[9:10]), mean(accuracy3[11:12]), accuracy3[3:8]),
                           Selection = c(accuracy4[1:2], mean(accuracy4[9:10]), mean(accuracy4[11:12]), accuracy4[3:8]))
accuracy_agg[["Evolution"]] <- apply(accuracy_agg, 1, function(x) {max(x) - min(x)})
row.names(accuracy_agg) <- c("Régression logistique (xgb)",
                             "Régression logistique (h2o)",
                             "Réseau de neurones 32x6 (h2o)",
                             "Réseau de neurones 16x16x6 (h2o)",
                             "Arbre de décision (xgb)",
                             "Arbre de décision (h2o)",
                             "Forêt aléatoire (xgb)",
                             "Forêt aléatoire (h2o)",
                             "Arbres boostés (xgb)",
                             "Arbres boostés (h2o)")

# Affichage des scores dans un tableau interactif
datatable(accuracy_agg,
          filter = "top", # Filtrage au-dessus de la table
          class = "cell-border stripe", # CSS
          extensions = c("ColReorder",
                         "RowReorder"), # Reordonner manuellement à la main
          options = list(pageLength = 10, # Page affichant 10 lignes
                         colReorder = TRUE, # Plugin
                         rowReorder = TRUE)) %>% # Plugin
  formatStyle(c("Defaut", "Approximation", "Inversement", "Selection"),
                  background = styleColorBar(c(0, max(accuracy_agg[["Selection"]])), 'lightgreen'), # Couleur vert clair pour les métriques par fold
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle(c("Evolution"),
                  background = styleColorBar(c(0, max(accuracy_agg[["Evolution"]])), 'pink'), # Couleur rose pour l'évolution de la métrique
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatPercentage(columns = c("Defaut", "Approximation", "Inversement", "Selection", "Evolution"),
              digits = 4)