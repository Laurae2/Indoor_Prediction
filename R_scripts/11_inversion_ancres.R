# Recopie des features dans le sens correct (ancre 1<=>3, ancre 2<=>4)
for (i in which(group_room$dataset_ID == 1)) {
  data_pre[[i]][[1]] <- data_pre_old[[i]][[3]]
  data_pre[[i]][[3]] <- data_pre_old[[i]][[1]]
  data_pre[[i]][[2]] <- data_pre_old[[i]][[4]]
  data_pre[[i]][[4]] <- data_pre_old[[i]][[2]]
}

# Enregistrement des frames corrigées et des anciennes frames, pour avoir un set solide, en respectant la nomenclature initiale MovementAAL_RSS_xxx.csv.
for (i in 1:314) {
  fwrite(data_pre[[i]], paste0("dataset_corrected/MovementAAL_RSS_", i, ".csv"))
}