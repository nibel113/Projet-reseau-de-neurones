# Réseau de neurones en actuariat

Projet de recherche dans le cadre du cours ACT-2101 de l'Université Laval sous la supervision de la professeure Marie-Pier Côté. 
On y compare différentes architectures de réseaux de neurones à propagation directe (« feed forward neural networks »). 
Deux techniques ont été principalement appliquées pour obtenir de meilleurs résultats, soit l'approche CANN, https://doi.org/10.1017/asb.2018.42,
 et les couches « embedding ». 
 
## Description de dépôt

Le projet est toujours en développement. On utilise les paquetages `keras` et `tfruns` pour l'implémentation des réseaux et la recherche
des hyperparamètres. On divise les fichiers en trois types selon leur utilité: les fichiers de traitement des données, les fichiers `*_tuning.R` 
contiennent l'architecture des réseaux et les fichiers `tfruns_*.R` contiennent le code pour faire la recherche d'hyperparamètres.

Un document complet suivra bientôt pour expliquer comment utliser ces paquetages. Pour l'instant, les fichiers pour les réseaux shallow sont 
commentés.

## Réseaux utilisés

On construit l'architecture pour des réseaux avec 1 à 4 couches cachées selon trois approches différentes.

### Réseaux standards

Les fichiers `Shallow_tuning.R` et `Deep_*hidden_tuning.R` contiennent le code pour les réseaux standards.

### Couche embedding

Les fichiers `Shallow_embed_tuning.R` et `Deep*_embed_tuning.R` contiennent le code pour les réseaux avec des couches « embedding ». 
La différence avec les réseaux standard est la façon dont on traite les variables catégorielles. Dans les réseaux standards, on les traite 
en créant une variable indicatrice pour tous les niveaux de la variable catégorielle sauf le niveau de référence, ce qu'on appelle « dummy coding ».
Dans les réseaux en question, on traite les variables catégorielles avec les couches embedding. Celle-ci permettent de réduire la dimension 
de ces variables catégorielles. Ces couches sont appliquées avant l'entrée dans le réseau. C'est leur résultat qui sera utilisé comme intrant 
dans le réseau. 

### Approche CANN

Cette approche est contenue dans les fichiers `Cann_*_tuning.R`. On y utilise les couches embedding. La différence avec les deux 
autres approches est la façon qu'on initialise le réseau. On utilise la moyenne de la fréquence observée sur l'ensemble des observations 
comme initialisation des deux autres approches. Dans ces approches, la prévision du réseau de la première phase de propagation directe est 
donc la moyenne empirique, et ce , pour toute nouvelle observation. Pour l'approche CANN, on utilise la prévision d'un GLM pour l'initialisation 
du réseau. 


## Interprétation

On procède également à l'interprétation des réseaux avec couches embedding avec le fichier `Interpretation_embedding_layers.R`. On y utilise 
le paquetage `iml` et on applique les techniques d'interprétation de graphique d'indépendance partielle univariée et bivariée ( « PDP » ), 
de graphique d'espérance conditionnelle individuelle ( « ICE » ) et de la statistique H de Friedman.
