acc_eval <- function(pred, dtrain) {
  
  # R�cup�ration du label
  y_true <- getinfo(dtrain, "label")
  
  # Cr�ation de la data.table tri�e avec comme cl� primaire la probabilit�
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  
  # Pr�paration pour le nettoyage des doublons post�rieurs
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  # Pr�-calcul de variables sp�cifiques
  lens <- length(y_true)
  nump <- sum(y_true)
  
  # D�termination des vrais n�gatifs et des vrais positifs
  DT[, tn_v := cumsum(y_true == 0)]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  
  # Nettoyage des doublons pour �viter le probl�me d'ordre par chance
  DT <- DT[cleaner, ]
  
  # D�termination de l'exactitude des donn�es
  DT[, acc := (tn_v + tp_v) / lens]
  
  # Annulation � z�ro pour toute observation dont le calcul aboutit � une erreur
  DT[, acc := ifelse(!is.finite(acc), 0, acc)]
  
  # Recherche de la meilleure exactitude
  best_row <- which.max(DT$acc)
  best_acc <- round(100 * DT$acc[best_row[1]], digits = 8)
  
  # Retour de la meilleure exactitude
  return(list(metric = "acc", value = best_acc))
  
}