

repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
source("https://raw.githubusercontent.com/PredictiveEcology/pemisc/refs/heads/development/R/getOrUpdatePkg.R")
getOrUpdatePkg(c("Require", "SpaDES.project"), c("1.0.1.9004", "0.1.1.9030")) # only install/update if required

projPath = file.path("~", "PSPcleanTools")
out <- SpaDES.project::setupProject(
  Restart = TRUE,
  #useGit = TRUE,
  updateRprofile = TRUE,
  paths = list(projectPath = "~/PSPcleanTools",
               cachePath = "cache",
               inputPath = "inputs",
               outputPath = "outputs",
               modulePath = "modules"),
  modules = c("PredictiveEcology/Biomass_speciesParameters@manual"),
  packages = c('RCurl', 'XML', 'snow', 'googledrive', 'httr2', "gert", "remotes"
               , "pkalanta/PSPclean@development (HEAD)"
               , "pkgload"
  ),
  times = list(start = 2011, end = 2012),
  params = list(Biomass_speciesParameters = list(PSPdataTypes = "NB")),
)
# This should load your package
pkgload::load_all("~/GitHub/PSPClean")

do.call(simInit, out)
