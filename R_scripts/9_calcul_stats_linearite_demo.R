# Pré-initialisation des variables
temp_utest <- data.frame(Feature = c(paste0(rep(c(paste0("Coef", 1:4), paste0("Rési", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4)),
                         p_value = numeric(36),
                         median_est = numeric(36),
                         median_inf95 = numeric(36),
                         median_sup95 = numeric(36))

# Mann-Whitney exact two-tailed U-test
for (i in 1:36) {
  temp_whitney <- wilcox.test(mini_lm[[i]][group_path$path_ID == 1], mini_lm[[i]][group_path$path_ID != 1], alternative = "two.sided", paired = FALSE, exact = TRUE, conf.int = TRUE, conf.level = 0.95)
  temp_utest[i, 2:5] <- c(temp_whitney$p.value, temp_whitney$estimate, temp_whitney$conf.int)
}

# Dump des données des tests U à deux bornes de Mann-Whitney
fwrite(accuracy, "stats/u_test.csv")

# Tableau interactif du U test de Mann-Whitney
datatable(temp_utest,
        filter = "top", # Filtrage au-dessus de la table
        class = "cell-border stripe", # CSS
        extensions = c("ColReorder",
                       "RowReorder"), # Reordonner manuellement à la main
        options = list(order = list(list(2, "desc")), # Ordonner par défaut par les facteurs ayant la p.value la plus grande
                       colReorder = TRUE, # Plugin
                       rowReorder = TRUE)) %>% # Plugin
  formatStyle("p_value",
                  background = styleColorBar(c(0, 1), 'lightblue'), # Couleur bleue pour le coefficient
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
  formatStyle(c("median_est", "median_inf95", "median_sup95"),
              background = styleColorBar(range(temp_utest[, 3:5]), 'pink'), # Couleur rose pour la différence de médiane estimée
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatRound(columns = c("p_value", "median_est", "median_inf95", "median_sup95"),
              digits = 6)

# Création de la table pour la visualisation
temp_lm <- copy(mini_lm)
colnames(temp_lm) <- c(paste0(rep(c(paste0("Coef", 1:4), paste0("Rési", 1:4)), 4), paste0("_", inverse.rle(list(lengths = rep(8, 4), values = 1:4)))), paste0("PosInitiale_", 1:4))

# Tableplot des coefficients, trajectoires, et salles
plot(tableplot(dat = cbind(Trajectoire = as.factor(group_path$path_ID), Salle = as.factor(group_room$dataset_ID), temp_lm[, c(1:4, 9:12, 17:20, 25:28)]), sortCol = 2, nBins = 20, scales = "lin", plot = FALSE), title = "Trajectoire vs Coefficients")

# Tableplot des résidus, trajectoires, et salles
plot(tableplot(dat = cbind(Trajectoire = as.factor(group_path$path_ID), Salle = as.factor(group_room$dataset_ID), temp_lm[, c(5:8, 13:16, 21:24, 29:32)]), sortCol = 2, nBins = 20, scales = "lin", plot = FALSE), title = "Trajectoire vs Résidus")

# Tableplot des positions initiales, trajectoires, et salles
plot(tableplot(dat = cbind(Trajectoire = as.factor(group_path$path_ID), Salle = as.factor(group_room$dataset_ID), temp_lm[, c(33:36)]), sortCol = 2, nBins = 20, scales = "lin", plot = FALSE), title = "Trajectoire vs Position Initiale")