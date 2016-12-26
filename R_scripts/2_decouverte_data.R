# Pré-initialisation de la série temporelle dépivotée par le label
# 
# Archictecture de la matrice (dépivotée sans la salle) 384x4 (64 observations par label, 16 observations par ancre par label) :
# ID  Strength    Anchor      Label       Time
#  1    val_a1         1          1          1
#  2    val_a2         1          1          2
#  ...
# 16   val_a16         1          1         16
# 17   val_a17         2          1          1
#  ...
# 32   val_a32         2          1         16
# 33   val_a33         3          1          1
#  ...
# 48   val_a48         3          1         16
# 49   val_a49         4          1          1
#  ...
# 64   val_a16         4          1         16
# 65   val_a17         1          2          1
#  ...
# ID(anchor, label, time) = ((label - 1) * 64) + ((anchor - 1) * 16) + time
avg_series <- data.table(matrix(rep(0, 16 * 4 * 6 * 6), nrow = 16 * 4 * 6, ncol = 4))
colnames(avg_series) <- c("Strength", "Anchor", "Label", "Time")

# Pré-filling des facteurs (Anchor, Label, Time)
avg_series[["Anchor"]] <- as.factor(rep(inverse.rle(list(lengths = rep(16, 4), values = 1:4)), 6))
levels(avg_series[["Anchor"]]) <- paste("Ancre", 1:4, sep = "")
avg_series[["Label"]] <- as.factor(inverse.rle(list(lengths = rep(16 * 4, 6), values = 1:6)))
levels(avg_series[["Label"]]) <- paste("Trajectoire", 1:6, sep = "")
avg_series[["Time"]] <- rep(1:16, 6 * 4)

# Transformation en liste par salle à dépivoter par la suite
avg_series <- list(cbind(avg_series, Room = rep(1, 384)),
                   cbind(avg_series, Room = rep(2, 384)),
                   cbind(avg_series, Room = rep(3, 384)))

# Pré-compte du nombre d'occurrence des labels
label_count <- list(tabulate(group_path[["path_ID"]][group_room[["dataset_ID"]] == 1]),
                    tabulate(group_path[["path_ID"]][group_room[["dataset_ID"]] == 2]),
                    tabulate(group_path[["path_ID"]][group_room[["dataset_ID"]] == 3]))

# Détermination des 16 dernières observations (2 secondes à 8 Hz), moyennisées
for (i in 1:314) {
  temp_label <- ((group_path[["path_ID"]][i] - 1) * 64) + 1 # Ligne de démarrage dans la matrice agrégée
  temp_obs <- nrow(data_pre[[i]]) - 15 # Ligne de démarrage dans la matrice à sauvegarder
  
  avg_series[[group_room[["dataset_ID"]][i]]][temp_label:(temp_label + 63), 1] <- avg_series[[group_room[["dataset_ID"]][i]]][temp_label:(temp_label + 63), 1] + (unlist(data_pre[[i]][temp_obs:(temp_obs + 15), ]) / label_count[[group_room[["dataset_ID"]][i]]][group_path[["path_ID"]][i]])
}

# Dépivotage de la variable définissant la salle
avg_series <- rbind(avg_series[[1]], avg_series[[2]], avg_series[[3]])
avg_series[["Room"]] <- as.factor(inverse.rle(list(lengths = rep(384, 3), values = 1:3)))
levels(avg_series[["Room"]]) <- paste("Salle", 1:3, sep = "")

# Affichage sous forme de plot interactif de manière automatisée
ggplotly(ggplot(data = avg_series, aes_string(x = "Time", y = "Strength", group = "Label", color = "Label")) + geom_line() + geom_point() + scale_color_brewer(palette = "Set2") + theme_bw() + facet_grid(Anchor ~ Room) + labs(title = "Evolution de la force du signal de l'ancre par rapport au temps"), width = 960, height = 720)

# Enregistrement de la table pour usage ultérieur si nécessaire
fwrite(avg_series, "agregation/agregation1.csv")