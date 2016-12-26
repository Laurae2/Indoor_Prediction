for (i in 2:13) {
  accuracy[13, i] <- mean(accuracy[1:6, i])
  accuracy[14, i] <- mean(accuracy[7:9, i])
  accuracy[15, i] <- mean(accuracy[10:12, i])
  accuracy[16, i] <- mean(accuracy[13:15, i])
}

# Enregistrement des scores
fwrite(accuracy, "scores/1_models.csv")

# Affichage des résultats dans un tableau interactif
to_print <- data.table(t(accuracy[13:16, -1])) # Préparation des données à mettre sur table
colnames(to_print) <- c("1 contre 1", "1 contre 2", "2 contre 1", "Moyenne") # Remise des noms des colonnes
row.names(to_print) <- colnames(accuracy)[-1] # Remise des noms des lignes
datatable(to_print,
          filter = "top", # Filtrage au-dessus de la table
          class = "cell-border stripe", # CSS
          extensions = c("ColReorder",
                         "RowReorder"), # Reordonner manuellement à la main
          options = list(pageLength = 12, # Page affichant 12 lignes
                         order = list(list(4, "desc")), # Ordonner par défaut par l'exactitude moyenne
                         colReorder = TRUE, # Plugin
                         rowReorder = TRUE)) %>% # Plugin
  formatStyle(c("1 contre 1", "1 contre 2", "2 contre 1"),
                  background = styleColorBar(c(0, 1), 'lightgreen'), # Couleur vert clair pour les métriques par fold
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle("Moyenne",
              background = styleColorBar(c(0, 1), 'pink'), # Couleur rose pour la métrique de moyenne
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatPercentage(columns = c("1 contre 1", "1 contre 2", "2 contre 1"),
              digits = 8) %>%
  formatPercentage(columns = "Moyenne",
              digits = 8)

# Affichage des résultats dans un tableau statique
formattable(accuracy[, c(1, 2:5)], list(formattable::area(col = xgb_LinearModel:xgb_GradientBoosting) ~ color_bar("orange")))
formattable(accuracy[, c(1, 6:9)], list(formattable::area(col = h2o_LinearModel:h2o_GradientBoosting) ~ color_bar("cyan")))
formattable(accuracy[, c(1, 10:13)], list(formattable::area(col = h2o_NN_32x6_ReLU:h2o_NN_16x16x6_Soft) ~ color_bar("yellow")))