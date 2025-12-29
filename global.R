


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

setwd("R")
source("wanulcas.R")
source("wanulcas_lib.R")
setwd(default_wd)

### GUI ######################

# tab_df <- read.csv("config/input_tabs.csv")
# subtab_df <- read.csv("config/input_subtabs.csv")
# subsubtab_df <- read.csv("config/input_subsubtabs.csv")
# inputvars_pos_df <- read.csv("config/input_vars.csv")
# inputvars_pos_df[is.na(inputvars_pos_df)] <- ""

input_gui_tabs_df <- read.csv("config/input_gui_tabs.csv")
input_vars_conf_df <- read.csv("config/input_vars.csv")
input_group_df <- read.csv("config/input_group.csv")

input_gui_tabs_df[is.na(input_gui_tabs_df)] <- ""
input_vars_conf_df[is.na(input_vars_conf_df)] <- ""
input_vars_conf_df[input_vars_conf_df$group_id == "", "group_id"] <- 0
wanulcas_params <- read_yaml("R/default_params.yaml", handlers = yaml_handler)

# preparing variable input parameters

inputvars_df <- data.frame(
  var = names(wanulcas_params$vars),
  label = names(wanulcas_params$vars),
  value = as.numeric(wanulcas_params$vars),
  min = NA,
  max = NA,
  step = NA
)
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
inputvars_df <- merge(inputvars_df,
                      input_vars_conf_df[c("var", "var_label", "var_desc")],
                      by = "var",
                      all.x = T)
inputvars_df$label <- inputvars_df$var_label
inputvars_df$info <- inputvars_df$var_desc

input_array <- wanulcas_def_arr

# preparing array input parameters

for (a in names(wanulcas_params$arrays)) {
  df <- input_array[[a]]
  v_df <- as.data.frame(wanulcas_params$arrays[[a]]$vars)
  df[names(v_df)] <- v_df
  input_array[[a]] <- df
}

a_df <- unique(input_vars_conf_df[input_vars_conf_df$type == "arrays" &
                                    input_vars_conf_df$id != "", c("subtype", "id", "group_id")])
a_ids <- paste("input_array", a_df$subtype, a_df$id, a_df$group_id, sep = "_")

arr_inp <- apply(a_df, 1, function(x) {
  ndf <- paste0(x["subtype"], "_df")
  df <- input_array[[ndf]]
  def_df <- wanulcas_def_arr[[ndf]]
  v <- input_vars_conf_df[input_vars_conf_df$type == "arrays" &
                            input_vars_conf_df$subtype == x["subtype"] &
                            input_vars_conf_df$id == as.numeric(x["id"]) &
                            input_vars_conf_df$group_id == as.numeric(x["group_id"]), "var"]
  cbind(def_df, df[v])
})
names(arr_inp) <- a_ids

arr_conf <- apply(a_df, 1, function(x) {
  keys <- names(wanulcas_def_arr[[paste0(x["subtype"], "_df")]])
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
names(arr_conf) <- a_ids


# preparing graph input parameters

graph_vars <- names(wanulcas_params$graphs)
graph_subvars <- sapply(graph_vars, function(a){
  subvars <- names(wanulcas_params$graphs[[a]]$xy_data)
  paste("input_graph", a, subvars, sep = "-")
})
graph_allvars <- unlist(graph_subvars, recursive = F, use.names = F)
graphplot_ids <- paste("input_graph_plot", graph_vars, sep = "-")

graph_inp <- unlist(lapply(wanulcas_params$graphs, function(a) {
  gv <- names(a$xy_data)
  df_list <- lapply(gv, function(b){
    df <- data.frame(a$xy_data[[b]]$x_val, a$xy_data[[b]]$y_val)
    colnames(df) <- c(a$x_var, b)
    df
  })
}), recursive = FALSE)
names(graph_inp) <- graph_allvars




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
