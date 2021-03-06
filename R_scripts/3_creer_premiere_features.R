# Pr�-initialisation de la frame
mini_lm <- data.frame(matrix(nrow = 314, ncol = 36))

# Boucle par s�rie temporelle
for (i in 1:314) {
  
  # Boucle par ancre
  for (j in 1:4) {
    
    # Entrainement d'un mod�le lin�aire utilisant les autres ancres, avec l'interceptrice
    temp_model <- fastLmPure(X = cbind(as.matrix(data_pre[[i]][, (1:4)[-j], with = FALSE]), rep(1, nrow(data_pre[[i]]))), y = data_pre[[i]][[j]])
    
    # Enregistrement des coefficients et des r�sidus
    mini_lm[i, (j * 8 - 7):(j * 8)] <- c(temp_model$coefficients, temp_model$stderr)
    
  }
  
  # Ajout du dernier �l�ment de la s�rie temporelle (4 ancres)
  mini_lm[i, 33:36] <- data_pre[[i]][nrow(data_pre[[i]]), ]
  
}

# Enregistrement des donn�es au format CSV
fwrite(cbind(mini_lm, Group = group_room[["dataset_ID"]], Label = group_path[["path_ID"]]), "features/features1.csv")