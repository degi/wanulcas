# Rahmat
# tambah tombol stop simulasi <- gak bisa, restart aja

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2)
  options(future.globals.maxSize = 5000 * 1024^2)
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  map_color <-
    colorRampPalette(c("darkgreen", "#ffb703", "#9a130e", "#023e8a"))
  grad_color <- colorRampPalette(c("#023047", "#219ebc", "#06d6a0", "#ffb703", "#9a130e"))
  slope_color <- colorRampPalette(c("#023047", "#06d6a0", "#ffb703", "#f78c6b"))
  stream_color <-
    colorRampPalette(c(
      "#e07a5f",
      "#7D60E0",
      "#606EE0",
      "#609AE1",
      "#0077b6",
      "#023e8a",
      "#03045e"
    ))
  # soil_color <- colorRampPalette(c("#331A05", "#a39171", "#FDEFE1"))
  soil_color <- colorRampPalette(c("darkgreen", "#ffb703", "#9a130e", "#023e8a"))
  depth_color <- colorRampPalette(c("#FDEFE1", "#a39171", "#331A05"))
  water_color <- colorRampPalette(c("#D4FEF3", "#219ebc", "#034464"))
  
  options(reactable.theme = reactableTheme(
    style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em")
  ))
  
  #not working for vector of number
  f_number <- function(v, ...) {
    format(v, big.mark = ",", scientific = F, ...)
  }
  
  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }
  
  
  ### Conditional panel UI logic ###
  
  conditional_id <-
    c(
      "is_dem_map",
      "is_stream_map",
      "is_lc_df",
      "is_subcatchment",
      "is_lake_df",
      "is_dam_df"
    )
  conditional_v <-
    c(
      "dem_map_stars",
      "dem_stream_sf",
      "lc_df",
      "subcatchment_map_sf",
      "lake_df",
      "dam_df"
    )
  
  # mapply(function(id, val) {
  #   output[[id]] <- reactive({
  #     type <- suffix(val)
  #     if (is.null(v[[val]])) {
  #       return(FALSE)
  #     }
  #     if (type == "df") {
  #       if (nrow(v[[val]]) == 0)
  #         return(F)
  #     }
  #     TRUE
  #   })
  #   outputOptions(output, id, suspendWhenHidden = FALSE)
  # }, conditional_id, conditional_v)
  
  
  
  ### reactiveValues #############################################
  
  rv_arr <- do.call(reactiveValues, arr_inp)
  
  v <- reactiveValues(wanulcas_cfg = list(), )
  
  
  
  
  
  ### INPUT PARAMETES AND DATA
  
  rv_arr_edit <- reactiveValues()
  react_arr <- reactive({
    react_arr <- lapply(names(rv_arr), function(x) {
      rv_arr_edit[[x]] <- table_edit_server(x, reactive(rv_arr[[x]]))
    })
  })
  
  observe(react_arr())

  observe({
    lapply(names(rv_arr_edit), function(x) {
      df <- rv_arr_edit[[x]]()
      if (!is.null(df)) {
        rv_arr[[x]] <- df
      }
    })
  })
  
  observe({
    # tes
    # print(rv_arr[["input_array_layer_1"]])
  })
  
  
  
  # soil_type_table_edit <- table_edit_server(
  #   "soil_type_table",
  #   reactive(v$soil_type_df),
  #   col_title = soil_type_h,
  #   col_type = c("numeric", "character", "character", rep("numeric", 16)),
  #   allowRowModif = T,
  #   nrow = 10,
  #   digits = 2,
  #   nestedHeaders = list(data.frame(
  #     title = paste0(
  #       div_h_style,
  #       c(
  #         "Soil Type</div>",
  #         "Top Soil Properties</div>",
  #         "Sub Soil Properties</div>"
  #       )
  #     ),
  #     colspan = c(3, 8, 8)
  #   ))
  # )
  #
  # observe({
  #   v$soil_type_df <- soil_type_table_edit()
  # })
  
  
  
  
}
