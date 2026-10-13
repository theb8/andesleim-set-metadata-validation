# sourcer ce script pour gérérer le rapport de validation
# auteur : A. Theberge

# (!) svp entrer numero de mission
mission_id <- "IML-2026-020"


# (!) svp entrer nom du fichier de traits (doit etre dans dossier "/inputs")
set_file <- "IML2026020_set_export_B.csv" 
ship_xycoord_file <- "Petoncles_2026.gpx"
andes_xycoord_file <- "IML-2026-020_track (2).gpx"
sta_file <- "IML-2026-020 stations.gpx"
fishevent_file <- "IML2026020_fishing_events.csv"
sensor_file <- "IML2026020_sensor_data.csv"


########################### #
## DO NOT EDIT PAST THIS LINE 
########################### #

# build paths
set_path <- file.path("inputs", set_file)
sta_path <- file.path("inputs", sta_file)
ship_xycoord_path <- file.path("inputs", ship_xycoord_file)
andes_xycoord_path <- file.path("inputs", andes_xycoord_file)
fishevent_path <- file.path("inputs", fishevent_file)
sensor_path <- file.path("inputs", sensor_file)

# make paths absolute: the report is knitted from the R/ folder
set_path <- normalizePath(set_path, mustWork = FALSE)
sta_path <- normalizePath(sta_path, mustWork = FALSE)
ship_xycoord_path <- normalizePath(ship_xycoord_path, mustWork = FALSE)
andes_xycoord_path <- normalizePath(andes_xycoord_path, mustWork = FALSE)
fishevent_path <- normalizePath(fishevent_path, mustWork = FALSE)
sensor_path <- normalizePath(sensor_path, mustWork = FALSE)

# check for files
if (!file.exists(set_path)) stop("File not found: ", set_path, call. = FALSE)
if (!file.exists(sta_path)) stop("File not found: ", sta_path, call. = FALSE)
if (!file.exists(ship_xycoord_path)) stop("File not found: ", ship_xycoord_path, call. = FALSE)
if (!file.exists(andes_xycoord_path)) stop("File not found: ", andes_xycoord_file, call. = FALSE)
if (!file.exists(fishevent_path)) stop("File not found: ", fishevent_path, call. = FALSE)
if (!file.exists(sensor_path)) stop("File not found: ", sensor_path, call. = FALSE)

# some more vars
mission_id_compact <- gsub(pattern = "-", replacement = "", mission_id)
SysTime_compact <- format(Sys.time(), "%Y%m%d-%Hh")

############### #
# create report
############### #
out_dir <- normalizePath("reports")

out_file <- rmarkdown::render(
  input       = file.path("R", "validation_report.Rmd"),
  params      = list(mission_id = mission_id,
                     sta_path = sta_path,
                     set_path = set_path,
                     fishevent_path = fishevent_path,
                     sensor_path = sensor_path,
                     ship_xycoord_path=ship_xycoord_path,
                     andes_xycoord_path=andes_xycoord_path),
  output_file = paste0("validation_", mission_id_compact, "_", SysTime_compact, ".html"),
  output_dir  = out_dir,
  envir       = new.env()
)

# to open the report after rendering
# browseURL(out_file)
