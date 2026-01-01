# Rahmat
# tambah tombol stop simulasi <- gak bisa, restart aja

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2)
  options(future.globals.maxSize = 5000 * 1024^2)
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
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
  
  ### vars input UI ######################
  inputvars_ui_id <- unique(inputvars_df$ui_id)
  
  # infiltration_par <- numeric_input_server("infiltration_par_input", infiltration_par_df)
  # observe({
  #   v$infiltration_par_cfg <- infiltration_par()
  # })
  
  rv_var_edit <- reactiveValues()
  numeric_var <- reactive({
    numeric_var <- lapply(inputvars_ui_id, function(x) {
      var_ids <- inputvars_df[inputvars_df$ui_id == x, "var"]
      rv_var_edit[[x]] <- numeric_input_server(x, var_ids)
    })
  })
  
  observe(numeric_var())
  
  
  lapply(inputvars_ui_id, function(x) {
    observe({
      inp <- rv_var_edit[[x]]()
      if (length(inp) > 0) {
        #TODO: assign var here
        print(inp)
      }
    })
  })
  
  ### array input UI ######################
  
  rv_arr_edit <- reactiveValues()
  react_arr <- reactive({
    react_arr <- lapply(names(arr_inp), function(x) {
      nkeys <- length(arr_conf[[x]]$keys)
      nvar <- length(arr_conf[[x]]$title_desc) - nkeys
      rv_arr_edit[[x]] <- table_edit_server(
        x,
        reactive(rv_arr[[x]]),
        col_title = arr_conf[[x]]$title_desc,
        col_disable = c(rep(T, nkeys), rep(F, nvar))
      )
    })
  })
  
  observe(react_arr())
  
  lapply(names(arr_inp), function(x) {
    observe({
      df <- rv_arr_edit[[x]]()
      if (!is.null(df)) {
        rv_arr[[x]] <- df
      }
    })
  })
  
  
  ### graph input UI ######################
  
  rv_graph_edit <- reactiveValues()
  react_graph <- reactive({
    react_graph <- lapply(names(graph_inp), function(x) {
      rv_graph_edit[[x]] <- table_edit_server(x, reactive(rv_graph[[x]]))
    })
  })
  
  observe(react_graph())
  
  
  lapply(names(graph_inp), function(x) {
    observe({
      df <- rv_graph_edit[[x]]()
      if (!is.null(df)) {
        rv_graph[[x]] <- df
      }
    })
  })
  
  # test
  # observe({
  #   print(rv_arr[["input_array_layer_6_0"]])
  # })
  
  
  generate_graph_plot <- function(var) {
    g_ids <- graph_subvars[[var]]
    if (is.null(g_ids))
      return(NULL)
    
    desc <- input_vars_conf_df[input_vars_conf_df$var == var, "var_desc"]
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
      showlegend = T,
      title = desc,
      yaxis = list(title = var),
      xaxis = list(title = wanulcas_params$graphs[[var]]$x_var),
      hoverlabel = list(namelength = -1)
    )
    
    is_fullscreen <- input[[paste0("input_graph_card-", var, "_full_screen")]]
    if (!is_fullscreen) {
      fig <- fig |> layout(
        xaxis = list(showgrid = F),
        yaxis = list(title = ""),
        margin = list(l = 0),
        showlegend = F,
        title = ""
      )
      fig <- fig |> config(displayModeBar = FALSE)
    }
    return(fig)
  }
  
  lapply(graph_vars, function(x) {
    gp_id <- paste("input_graph_plot", x, sep = "-")
    output[[gp_id]] <- renderPlotly(generate_graph_plot(x))
  })
  
  
  ### Simulation #############
  
  
  get_input_parameters <- function() {
    
  }
  
}
