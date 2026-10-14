# Validation des métadonnées de traits (Andes)

Routine R qui génère un rapport HTML de validation sommaire des métadonnées de traits
(coordonnées lat/lon et prof.) saisies dans Andes lors du relevé.

Le rapport repère les irrégularités et propose des valeurs de remplacement tirées des
données de navigation du navire. **Aucune correction n'est appliquée
automatiquement** : l'utilisateur doit effectuer les corrections manuellement dans Andes.

## Vérifications effectuées

Les traits CTD sont exclus ; seuls les traits de pêche sont validés.

1. **Distances de draguage** : statistiques descriptives de la distance début–fin de chaque
   trait et détection des valeurs aberrantes (*outliers*) par écart interquartile (1,5 × IQR).
   Pour chaque *outlier*, des coordonnées de remplacement sont suggérées à partir des
   *waypoints* du navire (provenant du logiciel Rosepoint sur le Leim).
2. **Coordonnées manquantes** : traits ayant au moins une latitude/longitude de début ou de
   fin manquante, avec coordonnées et *timestamps* suggérés. Les coordonnées de remplacement sont suggérées à partir des
   *waypoints* du navire (provenant du logiciel Rosepoint sur le Leim)
3. **Profondeurs manquantes** : traits sans profondeur de début ou de fin, avec profondeurs
   suggérées à partir des données de capteurs (`true_depth`) provenant du rapport ANdes.

Chaque section inclut des tableaux comparatifs et des cartes situant les positions
suggérées par rapport aux stations prévues.

## Fichiers d'entrée

Tous les fichiers doivent être déposés dans le dossier `inputs/`.

| Fichier | Source | Exemple de nom |
|---|---|---|
| Traits | rapport Andes | `IMLAAAAXXX_set_export.csv` |
| Stations | rapport Andes | `IML-AAAA-XXX stations.gpx` |
| Positions (trace) | rapport Andes | `IML-AAAA-XXX_track.gpx` |
| Événements de pêche | rapport Andes | `IMLAAAAXXX_fishing_events.csv` |
| Données de capteurs | rapport Andes | `IMLAAAAXXX_sensor_data.csv` |
| Navigation du navire | timonerie | fichier `.gpx` |

Le fichier `.gpx` de la timonerie doit contenir les couches `track_points` et `waypoints`.
L'entité `track_points` correspond à l'itinéraire complet du navire tandis que `waypoints` comprends les évènements spéciaux.
Le nom de chaque *waypoint* de début/fin de trait doit être une date+heure au format
`AAAA-MM-JJ HH:MM:SS`, **en heure de l'Est (ET)** et non en UTC.

## Utilisation

1. Déposer les fichiers d'entrée dans `inputs/`.
2. Ouvrir `R/run_validation.R` et renseigner, en haut du script :
   - `mission_id` (ex. `"IML-2026-020"`) ; il doit correspondre au `mission_number` du fichier de traits ;
   - le nom de chacun des fichiers d'entrée.
3. Ouvrir le projet `andesleim_set_metadata_validation.Rproj` puis sourcer le script :

   ```r
   source("R/run_validation.R")
   ```

Le rapport est écrit dans `reports/` sous le nom
`validation_<IMLAAAAXXX>_<AAAAMMJJ-HHh>.html`.

## Dépendances

- R avec les packages `sf`, `rmarkdown` et `knitr`
- Pandoc (inclus avec RStudio)

## Structure

```
R/
  run_validation.R        # point d'entrée : paramètres et génération du rapport
  validation_functions.R  # fonctions de validation
  validation_report.Rmd   # gabarit du rapport
inputs/                   # fichiers d'entrée
reports/                  # rapports générés
```

## Limites connues

- L'utilisateur doit faire les modifications manuellement dans andes.
- Faire attention aux fuseaux horaires lors de la transcription des dates.

Auteur : A. Théberge
