# Pré-initialisation des variables
data_pre <- list()

# Chargement des données
for (i in 1:314) {
  data_pre[[i]] <- fread(paste0("dataset/MovementAAL_RSS_", i, ".csv"), sep = ",", verbose = FALSE, showProgress = FALSE, col.names = c("RSS_anchor1", "RSS_anchor2", "RSS_anchor3", "RSS_anchor4"))
}

# Chargement des données annexes
labels <- fread("dataset/MovementAAL_target.csv", sep = ",", verbose = FALSE, showProgress = FALSE, col.names = c("sequence_ID", "class_label"))
labels$class_label[labels$class_label == -1] <- 0
group_room <- fread("groups/MovementAAL_DatasetGroup.csv", sep = ",", verbose = FALSE, showProgress = FALSE, col.names = c("sequence_ID", "dataset_ID"))
group_path <- fread("groups/MovementAAL_Paths.csv", sep = ",", verbose = FALSE, showProgress = FALSE, col.names = c("sequence_ID", "path_ID"))