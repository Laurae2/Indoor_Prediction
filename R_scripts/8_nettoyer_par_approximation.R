data_pre_old <- list()
for (i in 1:314) {
  data_pre_old[[i]] <- copy(data_pre[[i]]) # Replication en mémoire au lieu de la copie du pointeur
}

# Boucle par ancre
for (i in 1:4) {
  
  # Regroupage des données selon l'ancre et la salle
  temp_salle1 <- avg_series[(Anchor == paste0("Ancre", i)) & (Room == "Salle1"), ]$Strength
  temp_salle2 <- avg_series[(Anchor == paste0("Ancre", i)) & (Room == "Salle2"), ]$Strength
  
  # Détermination du modèle linéaire par la méthode des moindres carrés
  temp_model <- fastLmPure(X = cbind(as.matrix(temp_salle1), rep(1, length(temp_salle1))), y = temp_salle2)
  
  # Nettoyage des données d'après le coefficient directeur et l'intersection
  for (j in which(group_room$dataset_ID == 1)) {
    data_pre[[j]][[i]] <- data_pre[[j]][[i]] * temp_model$coefficients[1] + temp_model$coefficients[2]
  }
  
}

# Relancement de l'agrégation des données
avg_series_clean <- data.table(matrix(rep(0, 16 * 4 * 6 * 6), nrow = 16 * 4 * 6, ncol = 4))
colnames(avg_series_clean) <- c("Strength", "Anchor", "Label", "Time")

# Pré-filling des facteurs (Anchor, Label, Time)
avg_series_clean[["Anchor"]] <- as.factor(rep(inverse.rle(list(lengths = rep(16, 4), values = 1:4)), 6))
levels(avg_series_clean[["Anchor"]]) <- paste("Ancre", 1:4, sep = "")
avg_series_clean[["Label"]] <- as.factor(inverse.rle(list(lengths = rep(16 * 4, 6), values = 1:6)))
levels(avg_series_clean[["Label"]]) <- paste("Trajectoire", 1:6, sep = "")
avg_series_clean[["Time"]] <- rep(1:16, 6 * 4)

# Transformation en liste par salle à dépivoter par la suite
avg_series_clean <- list(cbind(avg_series_clean, Room = rep(1, 384)),
                         cbind(avg_series_clean, Room = rep(2, 384)),
                         cbind(avg_series_clean, Room = rep(3, 384)))

# Pré-compte du nombre d'occurrence des labels
label_count <- list(tabulate(group_path[["path_ID"]][group_room[["dataset_ID"]] == 1]),
                    tabulate(group_path[["path_ID"]][group_room[["dataset_ID"]] == 2]),
                    tabulate(group_path[["path_ID"]][group_room[["dataset_ID"]] == 3]))

# Détermination des 16 dernières observations (2 secondes à 8 Hz), moyennisées
for (i in 1:314) {
  temp_label <- ((group_path[["path_ID"]][i] - 1) * 64) + 1 # Ligne de démarrage dans la matrice agrégée
  temp_obs <- nrow(data_pre[[i]]) - 15 # Ligne de démarrage dans la matrice à sauvegarder
  
  avg_series_clean[[group_room[["dataset_ID"]][i]]][temp_label:(temp_label + 63), 1] <- avg_series_clean[[group_room[["dataset_ID"]][i]]][temp_label:(temp_label + 63), 1] + (unlist(data_pre[[i]][temp_obs:(temp_obs + 15), ]) / label_count[[group_room[["dataset_ID"]][i]]][group_path[["path_ID"]][i]])
}

# Dépivotage de la variable définissant la salle
avg_series_clean <- rbind(avg_series_clean[[1]], avg_series_clean[[2]], avg_series_clean[[3]])
avg_series_clean[["Room"]] <- as.factor(inverse.rle(list(lengths = rep(384, 3), values = 1:3)))
levels(avg_series_clean[["Room"]]) <- paste("Salle", 1:3, sep = "")

# Affichage sous forme de plot interactif de manière automatisée
ggplotly(ggplot(data = avg_series_clean, aes_string(x = "Time", y = "Strength", group = "Label", color = "Label")) + geom_line() + geom_point() + scale_color_brewer(palette = "Set2") + theme_bw() + facet_grid(Anchor ~ Room) + labs(title = "Evolution de la force du signal de l'ancre (corrigé) par rapport au temps"), width = 960, height = 720)

# Enregistrement de la table pour usage ultérieur si nécessaire
fwrite(avg_series_clean, "agregation/agregation2.csv")