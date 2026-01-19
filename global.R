


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
  # "shinyalert",
  # "shinyWidgets",
  
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
  # "config",
  # "shinyjs",
  
  # "future",
  # "ipc"
  # "promises"
)


install_load(libs)

library("shiny")
library("bslib")
library("bsicons")
library("htmltools")
library("plotly")
# library(shinyalert)
# library(shinyWidgets)

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
# library(later)

# library(shinyjs)
# library(config)
#multisession
# library(future)
library(ipc) #AsyncProgress
# library(promises)
library(mirai)

# plan(multisession)

# Set the number of local daemons (e.g., 2)
mirai::daemons(6L) 

# Register a function to shut down daemons when the app stops
onStop(function() {
  mirai::daemons(0L)
})


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

compact_button_style <- "width:auto;height:36px;padding:5px 20px;" #padding:0px 20px;



# setwd("R")
source("R/wanulcas.R")
source("R/wanulcas_lib.R")
# setwd(default_wd)

# is_run_online <- config::get("is_online")
# print(is_run_online)
# print(config::get("data_path"))
is_run_online <- Sys.getenv('SHINY_PORT') != ""
print(paste("Run online:", is_run_online))

### GUI ######################

wanulcas_params_def <- read_yaml("R/default_params.yaml", handlers = yaml_handler)
input_gui_tabs_df <- read.csv("config/input_gui_tabs.csv")
input_vars_conf_df <- read.csv("config/input_vars.csv")
input_group_df <- read.csv("config/input_group.csv")
output_vars_option_df <- read.csv("R/output_vars.csv")
output_vars <- default_output_vars

input_gui_tabs_df[is.na(input_gui_tabs_df)] <- ""
input_vars_conf_df[is.na(input_vars_conf_df)] <- ""
input_vars_conf_df[input_vars_conf_df$group_id == "", "group_id"] <- 0




  
  

input_vars_conf_df$var_label <- paste0(
  ifelse(
    input_vars_conf_df$label == "",
    input_vars_conf_df$var,
    input_vars_conf_df$label
  ),
  ifelse(input_vars_conf_df$unit == "", "", paste0(
    " [", trimws(input_vars_conf_df$unit), "]"
  ))
)
input_vars_conf_df$var_desc <- paste0(input_vars_conf_df$desc,
                                      ifelse(input_vars_conf_df$label == "", "", paste0(" [", trimws(
                                        input_vars_conf_df$var
                                      ), "]")))

# preparing variable input parameters

inputvars_df <- data.frame(
  var = names(wanulcas_params_def$vars),
  label = names(wanulcas_params_def$vars),
  value = as.numeric(wanulcas_params_def$vars),
  min = NA,
  max = NA,
  step = NA
)

inputvars_df <- merge(inputvars_df,
                      input_vars_conf_df[c("var", "var_label", "var_desc", "id", "group_id")],
                      by = "var",
                      all.x = T)
inputvars_df$label <- inputvars_df$var_label
inputvars_df$info <- inputvars_df$var_desc
inputvars_df$ui_id <- paste("input_var", inputvars_df$id, inputvars_df$group_id, sep = "_")


arr_ids_df <- unique(input_vars_conf_df[input_vars_conf_df$type == "arrays" &
                                    input_vars_conf_df$id != "", c("subtype", "id", "group_id")])
arr_ids_df$arr <- paste(arr_ids_df$subtype, "df", sep = "_")
arr_ids_df$ui_id <- paste("input_array", arr_ids_df$subtype, arr_ids_df$id, arr_ids_df$group_id, sep = "_")

array_params_to_ui_inp <- function(array_params) {
  ui_inp <- apply(arr_ids_df, 1, function(x) {
    df <- array_params[[x["arr"]]]$vars
    v <- input_vars_conf_df[input_vars_conf_df$type == "arrays" &
                              input_vars_conf_df$subtype == x["subtype"] &
                              input_vars_conf_df$id == as.numeric(x["id"]) &
                              input_vars_conf_df$group_id == as.numeric(x["group_id"]), "var"]
    as.data.frame(c(array_params[[x["arr"]]]$keys, df[v]))
  })
  names(ui_inp) <- arr_ids_df$ui_id
  return(ui_inp)
}

arr_inp <- array_params_to_ui_inp(wanulcas_params_def$arrays)

arr_conf <- apply(arr_ids_df, 1, function(x) {
  keys <- names(wanulcas_def_arr[[x["arr"]]])
  lab_df <- input_vars_conf_df[input_vars_conf_df$type == "arrays" &
                                 input_vars_conf_df$subtype == x["subtype"] &
                                 input_vars_conf_df$id == as.numeric(x["id"]) &
                                 input_vars_conf_df$group_id == as.numeric(x["group_id"]), c("var_label", "var_desc")]
  desc <- ifelse(lab_df$var_desc == "",
                 "",
                 paste(" <!--", lab_df$var_desc, "-->"))
  title_desc <- paste0(lab_df$var_label, desc)
  list(keys = keys, title_desc = c(keys, title_desc))
})
names(arr_conf) <- arr_ids_df$ui_id


# preparing graph input parameters

graph_vars <- names(wanulcas_params_def$graphs)
graph_subvars <- sapply(graph_vars, function(a){
  subvars <- names(wanulcas_params_def$graphs[[a]]$xy_data)
  paste("input_graph", a, subvars, sep = "-")
})
graph_allvars <- unlist(graph_subvars, recursive = F, use.names = F)

graph_params_to_ui_inp <- function(graph_params) {
  graph_ui_inp <- unlist(lapply(graph_params, function(a) {
    gv <- names(a$xy_data)
    df_list <- lapply(gv, function(b){
      df <- data.frame(a$xy_data[[b]]$x_val, a$xy_data[[b]]$y_val)
      colnames(df) <- c(a$x_var, b)
      df
    })
  }), recursive = FALSE)
  names(graph_ui_inp) <- graph_allvars
  return(graph_ui_inp)
}

graph_inp <- graph_params_to_ui_inp(wanulcas_params_def$graphs)

# graph_inp <- unlist(lapply(wanulcas_params_def$graphs, function(a) {
#   gv <- names(a$xy_data)
#   df_list <- lapply(gv, function(b){
#     df <- data.frame(a$xy_data[[b]]$x_val, a$xy_data[[b]]$y_val)
#     colnames(df) <- c(a$x_var, b)
#     df
#   })
# }), recursive = FALSE)
# names(graph_inp) <- graph_allvars

###




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
  AF_TreePosit = c(1, 1, 1, NA),
  T_RelPosinZone = c(1, 1, 1, NA),
  stringsAsFactors = FALSE
)
