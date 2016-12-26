# Indoor_Prediction
Indoor User Movement Prediction from RSS data Data Set

1. Fichiers importants
2. Méthode de travail
3. Infos sur les fichiers

# Fichiers importants

Fichiers :

- [Notebook for 1280x720 screens](https://rawgit.com/Laurae2/Indoor_Prediction/master/Notebook_1280x720.html) (7.889KB)
- [Notebook for 1920x1080 screens](https://rawgit.com/Laurae2/Indoor_Prediction/master/Notebook_1920x1080.html) (7.889KB)
- [Report for 1280x720 screens](https://rawgit.com/Laurae2/Indoor_Prediction/master/Notebook_Style_1280x720.html) (6.491KB) <= version recommandée à lire si votre écran est à définition 1280x720
- [Report for 1920x1080 screens](https://rawgit.com/Laurae2/Indoor_Prediction/master/Notebook_Style_1920x1080.html) (6.491KB) <= version recommandée à lire

Avantages du Notebook :

* Cacher le(s) code(s) pour voire uniquement le texte, les tableaux, les graphiques
* Avoir un Sommaire qui suit la navigation en temps réel dans un panneau annexe
* Le code est sans scrolling horizontal

Avantages du Report :

* Stylisé
* Espacement du code
* Le code est avec scrolling vertical, typé GitHub

Inconvénients du Notebook :

* Problème de parsing des sauts de ligne dans les morceaux de code

Inconvénients du Report :

* Impossible de cacher le code

# Methode de travail

Analyse d'introduction :

1. Lecture des papiers de recherche associés pour me faire des idées sur la dataset
2. Téléchargement des données, et vu que les données sont petites => Découverte des données via Notepad++ (ouverture pour regarder le contenu des fichiers)
3. Réflexion sur le langage de programmation à utiliser => R vu que le temps est limité (je code bien plus rapidement sur R que sur Python, et je trouve R meilleur dés qu'il s'agit de partager des analyses / recherches)
4. Problème de français => gestion du texte en UTF-8 dans RStudio
5. Définition de la méthode pour charger les données en mémoire car dataset "atypique" (séries temporelles à taille variable)
6. Réflexion sur les éléments en commun à toutes les séries temporelles (j'ai pris les 16 derniers points) => découverte des ancres inversées, de signaux inversés... qui font qu'il faudra un nettoyage
7. Recherche créative de création de features, au final => coefficients de régression linéaire entre les 4 ancres, résidus des ancres, le dernier point enregistré pour chaque ancre (total = 36 features)
8. Recherche d'une méthode de validation qui puisse prendre en compte le chemin 3 manquant dans la salle 1 => séparer par "salle" (par dataset), enlever le label 3 de l'entrainement + validation lorsque la salle 1 est seule lors de l'entrainement ou de la validation
9. Recherche des modèles pouvant couvrir une vaste surface des modèles existants : linéaire (logistique, neuronal), non-linéaire (arbres de décision, forêt aléatoire, boosting d'arbres), avec des implémentations différentes (extreme gradient boosting, h2o) qui puissent être déployable facilement en entreprise (Java, C, C++, Shell/Bash...)
10. Création des sets d'entrainement et de validation conformes à tous les modèles à utiliser
11. Entrainement des modèles (144 modèles)
12. Analyse des résultats => les modèles linéaires sont les meilleurs, pourquoi ? + résultats horribles pour la salle 1 (dataset 1)
13. Tentative de correction des ancres de la salle 1 (dataset 1) par approximation linéaire de la salle 2 (dataset 2) avec la méthode des moindres carrés (apparemment, le papier de recherche que je cite dans l'analyse dit que la dataset 1 et 2 sont proches en termes de caractéristiques spatiales)
14. Répétition 8-12, on remarque de bien meilleur résultats pour la salle 1
15. Démonstration statistique du problème de linéarité (et que les features créées sont favorables à un traitement linéaire et non pas non-linéaire)
16. Correction de l'inversion des ancres

Laisser deux journées le temps de trouver des idées avec le cerveau car ce n'est pas toujours en une journée qu'on a des belles idées.

1. Inversion des ancres de la salle 1 (dataset 1)
2. Répétition 8-12, on remarque un avancement pour la salle 1 mais ce n'est pas forcément le mieux qu'on puisse avoir avec nos features
3. Comme pas suffisamment de temps de trouver des idées de nouvelles features => comme on est dans un problème linéaire, on va optimiser la régression logistique par une optimisation par entropie croisée qui donne un boost de 7% d'exactitude (env. 60% à 70%)
4. Répétition 8-12, on remarque que les modèles s'améliorent énormément pour tous même si on met de côté l'optimisation des hyperparamètres, mais problème => la trajectoire 3 n'est pas prédite correctement du tout, peut être qu'elle est ambïgue à gérer et que l'optimiseur a préféré mettre de côté cette trajectoire au profit de toutes les autres
5. Conclusion => tous les modèles ont évolué par rapport au début, jusqu'à atteindre 70% d'exactitude
6. Problème toujours présent : la régression logistique reste toujours compliquée à comprendre, j'aurais préféré faire un exemple Shiny pour l'interprétation (interactivité totale avec la prédiction pour voir comment évolue les prédictions selon le changement de telle ou telle variable) mais le temps est limité

# Infos sur les fichiers

Les graphiques sont normalement tous interactifs, vous pouvez réaliser des actions directement dessus comme zoomer, faire afficher les valeurs, exporter en PNG... :

![Matrice de confusion](https://cloud.githubusercontent.com/assets/9083669/21483698/3be1e6f2-cb87-11e6-8008-dc3bf010dbf9.png)

![Optimisation exportable](https://cloud.githubusercontent.com/assets/9083669/21483713/723912c0-cb87-11e6-8189-0479aa1df42f.png)

Idem pour certains tableaux, qui peuvent être filtrer / triés, et même triés manuellement (cliquer-glisser sur les noms des lignes / colonnes)... :

![Tableau interactif](https://cloud.githubusercontent.com/assets/9083669/21483717/87f5bd5c-cb87-11e6-9a71-3cd6e7012547.png)

![Tri manuel](https://cloud.githubusercontent.com/assets/9083669/21483728/ac4587c8-cb87-11e6-8ab0-558e848c832d.png)
