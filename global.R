
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install_load <- function (package1, ...)  {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  instp <- rownames(installed.packages())
  for (package in packages) {
    if (!package %in% instp) {
      install.packages(package, repos = "http://cran.us.r-project.org", dependencies = T)
    }
  }
}

libs <- c(
  "shiny",
  "bslib",
  "bsicons",
  "htmltools",
  "plotly",
  
  "reactable",
  "excelR",
  
  "RColorBrewer",
  "paletteer",
  
  "yaml",
  "zip",
  "openxlsx2",
  "progress",
  "data.table",
  "lubridate"
)


install_load(libs)

library("shiny")
library("bslib")
library("bsicons")
library("htmltools")
library("plotly")
#table UI
library("reactable")
library("excelR")
# library(data.table)
#file IO
library("yaml")
library("zip")
#color
library("paletteer")
library("RColorBrewer")
#utiliy
library("openxlsx2")
library("progress")
library(lubridate)

# plan(multisession)



theme_color <- list(
  primary = "#8B3E04",
  secondary = "#FA842B",
  dark = "#00241B",
  success = "#007D92",
  info = "#61701f",
  warning = "#ffe230",
  danger = "#b90101",
  light1 = "#EADEBD",
  light2 = "#F5F0E0"
)

default_wd <- getwd()

tooltip_custom <- function(...) {
  tooltip(..., options = list(customClass = "custom-tooltip"))
}

source("R/wanulcas.R")

### GUI ######################

tab_df <- read.csv("config/input_tabs.csv")
subtab_df <- read.csv("config/input_subtabs.csv")
subsubtab_df <- read.csv("config/input_subsubtabs.csv")
inputvars_pos_df <- read.csv("config/input_vars.csv")
inputvars_pos_df[is.na(inputvars_pos_df)] <- ""
wanulcas_params <- read_yaml("R/default_params.yaml", handlers = yaml_handler)

inputvars_df <- data.frame(
  var = names(wanulcas_params$vars),
  label = names(wanulcas_params$vars),
  value = as.numeric(wanulcas_params$vars),
  min = NA,
  max = NA,
  step = NA
)

input_array <- wanulcas_def_arr

# assign array parameter
for (a in names(wanulcas_params$arrays)) {
  df <- input_array[[a]]
  v_df <- as.data.frame(wanulcas_params$arrays[[a]]$vars)
  df[names(v_df)] <- v_df
  input_array[[a]] <- df
}

# par_df <- data.frame(
#   var = names(defaul_params$vars),
#   type = "vars",
#   stype = ""
# )
# 
# for(a in names(defaul_params$arrays)){
#   df <- data.frame(
#     var = names(defaul_params$arrays[[a]]$vars),
#     type = "arrays",
#     stype = a
#   )
#   # print(df)
#   par_df <- rbind(par_df, df)
# }
# 
# df <- data.frame(
#   var = names(defaul_params$graphs),
#   type = "graphs",
#   stype = ""
# )
# par_df <- rbind(par_df, df)
# write.csv(par_df, "input_vars.csv",row.names = F)

### variable def ################

var_cols <- c("variable",
              "definition",
              "category",
              "unit",
              "min",
              "max",
              "default")
# variable_df <- read.csv("docs/variable_df.csv")

month_cols <- data.frame(
  var = c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  ),
  month = c(1:12)
)

### opentopography.org ########################

desc <- list(
  landcover = "**Land cover** is one of the main factors in watershed dynamics.
  A **time series** of land cover maps follows the dynamic changes in soil properties.
  Please provide land cover maps on the observed periods.
  The land cover map's boundary box will be used as the area for the DEM query,
  The DEM is later delineated to generate the watershed boundary.",
  
  soil_hydraulic = "Soil hydraulic at a potential of 0 kPa is in a state of saturation.
  At saturation, all soil pores are filled with water, and water typically drains
  from large pores by gravity. At a potential of −33 kPa, or −1/3 bar, (−10 kPa for sand),
  soil is at field capacity. Typically, at field capacity, air is in the macropores,
  and water in the micropores. Field capacity is viewed as the optimal condition
  for plant growth and microbial activity. At a potential of −1500 kPa,
  the soil is at its permanent wilting point, at which plant roots cannot
  extract the water through osmotic diffusion.(https://en.wikipedia.org/wiki/Water_potential)",
  
  soil_db = "The soil data is acquired from **Harmonized World Soil Database version 2.0** (HWSD v2.0).
  **HWSD** is a comprehensive global soil inventory that offers detailed insights into soil properties,
  including their morphology, chemistry, and physical characteristics,
  with a focus on a 1 km resolution. Please visit
  <a href='https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/' target='_blank'>**FAO SOILS PORTAL**</a>
  for more information on the database and its suggested citation",
  
  soil_list = "The soil list here is extracted from the global soil database, including the **summarized properties** required for the model.
  Check the Global Soil Database tab to view the soil properties in detail."
)

default_par <- function(df) {
  val <- as.list(df$value)
  names(val) <- df$var
  return(val)
}

af_par_df <- data.frame(
  var = c("AF_Circ", "AF_Crop", "AF_DeepSubSoil", "AF_DepthDynamic"),
  label = c(
    "Routing velocity (m sec<sup>-1</sup>)",
    "TortuoSity",
    "River flow dispersal factor",
    "Surface loss fraction"
  ),
  value = c(0, 1, 3, 0),
  min = rep(0, 4),
  max = c(100, rep(1, 3)),
  step = rep(0.1, 4),
  stringsAsFactors = FALSE
)

### AF System pars ########################

zone_df <- data.frame(
  zone = c(1:4),
  AF_Zone = c(0.5, 1, 1, 1),
  T_TreesperHa = c(400, 0, 0, NA),
  AF_TreePosit = c(1,1,1,NA),
  T_RelPosinZone = c(1,1,1,NA),
  stringsAsFactors = FALSE
)

