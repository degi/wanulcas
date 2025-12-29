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
  
  

  
  
  ### reactiveValues #############################################
  
  rv_arr <- do.call(reactiveValues, arr_inp)
  rv_graph <- do.call(reactiveValues, graph_inp)

  v <- reactiveValues(wanulcas_cfg = list(), )
  
  
  
  
  
  ### INPUT PARAMETES AND DATA
  
  
  ### array input UI ######################
  
  rv_arr_edit <- reactiveValues()
  react_arr <- reactive({
    react_arr <- lapply(names(rv_arr), function(x) {
      nkeys <- length(arr_conf[[x]]$keys)
      nvar <- length(arr_conf[[x]]$title_desc) - nkeys
      rv_arr_edit[[x]] <- table_edit_server(x, reactive(rv_arr[[x]]), 
                                            col_title = arr_conf[[x]]$title_desc,
                                            col_disable = c(rep(T, nkeys), rep(F, nvar)))
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
  
  
  ### graph input UI ######################
  
  rv_graph_edit <- reactiveValues()
  react_graph <- reactive({
    react_graph <- lapply(names(rv_graph), function(x) {
      rv_graph_edit[[x]] <- table_edit_server(x, reactive(rv_graph[[x]]))
    })
  })
  
  observe(react_graph())
  
  observe({
    lapply(names(rv_graph_edit), function(x) {
      # print(x)
      df <- rv_graph_edit[[x]]()
      if (!is.null(df)) {
        rv_graph[[x]] <- df
      }
    })
  })
  
  # test
  observe({
    print(rv_arr[["input_array_layer_6_0"]])
  })
  
  
  generate_graph_plot <- function(var) {
    g_ids <- graph_subvars[[var]]
    
    if (is.null(g_ids)) return(NULL)
    
  
    fig <- plot_ly()
    for (g_id in g_ids) {
      df <- rv_graph[[g_id]]
      
      fig <- fig |> add_trace(
        x = df[[1]],
        y = df[[2]],
        type = "scatter",
        mode = "lines+markers",
        name = names(df)[2]
      )
    }
    fig <- fig |>   layout(
      legend = list(orientation = 'h'),
      # showlegend = F,
      yaxis = list(title = var),
      xaxis = list(title = wanulcas_params$graphs[[var]]$x_var)
    )
    
    fs_id <- paste0("input_graph_card-", var, "_full_screen")
    if(input[[fs_id]]) {
      fig <- fig |> layout(showlegend = T)
    } else {
      fig <- fig |> layout(showlegend = F)
    }
    return(fig)
  }
  
  lapply(graph_vars, function(x){
    gp_id <- paste("input_graph_plot", x, sep = "-")
    output[[gp_id]] <- renderPlotly(
      generate_graph_plot(x)
    )
  })
  

  
}
