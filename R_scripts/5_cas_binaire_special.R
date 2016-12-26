acc_eval <- function(pred, dtrain) {
  
  # Récupération du label
  y_true <- getinfo(dtrain, "label")
  
  # Création de la data.table triée avec comme clé primaire la probabilité
  DT <- data.table(y_true = y_true, y_prob = pred, key = "y_prob")
  
  # Préparation pour le nettoyage des doublons postérieurs
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  
  # Pré-calcul de variables spécifiques
  lens <- length(y_true)
  nump <- sum(y_true)
  
  # Détermination des vrais négatifs et des vrais positifs
  DT[, tn_v := cumsum(y_true == 0)]
  DT[, tp_v := nump - cumsum(y_true == 1)]
  
  # Nettoyage des doublons pour éviter le problème d'ordre par chance
  DT <- DT[cleaner, ]
  
  # Détermination de l'exactitude des données
  DT[, acc := (tn_v + tp_v) / lens]
  
  # Annulation à zéro pour toute observation dont le calcul aboutit à une erreur
  DT[, acc := ifelse(!is.finite(acc), 0, acc)]
  
  # Recherche de la meilleure exactitude
  best_row <- which.max(DT$acc)
  best_acc <- round(100 * DT$acc[best_row[1]], digits = 8)
  
  # Retour de la meilleure exactitude
  return(list(metric = "acc", value = best_acc))
  
}