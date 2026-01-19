# Rahmat
# tambah tombol stop simulasi <- gak bisa, restart aja

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2)
  # options(future.globals.maxSize = 5000 * 1024^2)
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  options(
    reactable.theme = reactableTheme(
      style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em"),
      stripedColor = "#FBFAF4",
      highlightColor = "#F5F0E0"
    )
  )
  
  
  
  #not working for vector of number
  f_number <- function(v, ...) {
    format(v, big.mark = ",", scientific = F, ...)
  }
  
  f_percent <- function(v) {
    sprintf("%0.1f%%", v * 100)
  }
  
  
  
  
  
  ### reactiveValues #############################################
  
  rv_var <- do.call(reactiveValues, wanulcas_params_def$vars)
  rv_arr <- do.call(reactiveValues, arr_inp)
  rv_graph <- do.call(reactiveValues, graph_inp)
  
  rv <- reactiveValues(wanulcas_cfg = list(), sim_output = NULL)
  
  conditional_id <-
    c("is_sim_output")
  conditional_v <-
    c("sim_output")
  
  mapply(function(id, val) {
    output[[id]] <- reactive({
      type <- suffix(val)
      if (is.null(rv[[val]])) {
        return(FALSE)
      }
      if (type == "df") {
        if (nrow(rv[[val]]) == 0)
          return(F)
      }
      TRUE
    })
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }, conditional_id, conditional_v)
  
  ### INPUT PARAMETES AND DATA
  
  ### vars input UI ######################
  inputvars_ui_id <- unique(inputvars_df$ui_id)
  
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
      lapply(names(inp), function(x) {
        rv_var[[x]] <- inp[[x]]
      })
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
    fig <- fig |> layout(
      legend = list(orientation = 'h'),
      showlegend = T,
      title = desc,
      yaxis = list(title = var),
      xaxis = list(title = wanulcas_params_def$graphs[[var]]$x_var),
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
  
  
  ### Run Simulation #############
  
  # sim_output <- reactiveVal()
  local_task <- reactiveVal()
  
  get_input_parameters <- function() {
    params <- list()
    # vars
    params$vars <- reactiveValuesToList(rv_var)
    # arrays
    v_arr <- reactiveValuesToList(rv_arr)
    params$arrays <- sapply(names(wanulcas_params_def$arrays), function(x) {
      key_cols <- names(wanulcas_def_arr[[x]])
      arrs <- v_arr[arr_ids_df[arr_ids_df$arr == x, "ui_id"]]
      names(arrs) <- NULL
      arr_df <- do.call(cbind, arrs)
      narr <- names(arr_df)
      narr <- narr[!narr %in% key_cols]
      list(keys = as.list(wanulcas_def_arr[[x]]),
           vars = as.list(arr_df[, narr, drop = FALSE]))
    }, simplify = F)
    # graphs
    v_graph <- reactiveValuesToList(rv_graph)
    params$graphs <- wanulcas_params_def$graphs
    xy <- lapply(names(wanulcas_params_def$graphs), function(x) {
      v <- v_graph[graph_subvars[[x]]]
      vg <- lapply(v, function(x) {
        list(x_val = x[[1]], y_val = x[[2]])
      })
      names(vg) <- names(params$graphs[[x]]$xy_data)
      vg
    })
    params$graphs <- mapply(function(a, b) {
      a$xy_data <- b
      a
    }, params$graphs, xy, SIMPLIFY = F)
    return(params)
  }
  
  # task <- ExtendedTask$new(
  #   function(n, pars, outvars, progress)
  #     mirai(
  #       run_wanulcas(n, pars, outvars, progress),
  #       run_wanulcas = run_wanulcas,
  #       n = n,
  #       pars = pars,
  #       outvars = outvars,
  #       progress = progress
  #     )
  # ) |> bind_task_button("sim_run_button")
  # 
  # if (is_run_online) {
  #   observeEvent(input$sim_run_button, {
  #     if (!is_simulation_ready())
  #       return()
  #     print("Starting simulation")
  #     n_iteration <- input$n_iteration
  #     pars <- isolate(get_input_parameters())
  #     progress <- AsyncProgress$new(
  #       session,
  #       min = 1,
  #       max = n_iteration,
  #       message = "Processing the server",
  #       detail = "Please wait while preparing the server session.."
  #     )
  #     on.exit(progress$close())
  #     progress_trigger <- function(i, n) {
  #       progress$set(i, "Running simulation", paste("Day", i, "of", n))
  #     }
  #     
  #     task$invoke(n_iteration, pars, output_vars, progress)
  #     
  #     
  #   })
  # } else {
    
    local_task <- eventReactive(input$sim_run_button, ignoreNULL = T, {
      if (!is_simulation_ready())
        return()
      n_iteration <- input$n_iteration
      pars <- isolate(get_input_parameters())
      
      progress <- Progress$new(session, min = 1, max = n_iteration)
      on.exit(progress$close())
      progress_trigger <- function(i, n) {
        progress$set(i, "Running simulation", paste("Day", i, "of", n))
      }
      
      run_wanulcas(n_iteration, pars, output_vars, progress_trigger)
    })
    
  # }
  
  is_simulation_ready <- function() {
    if (length(output_vars) == 0) {
      show_alert(
        "Output variables was not selected",
        "Please select the output variable on the table below by checking the correspondent box."
      )
      return(F)
    }
    return(T)
  }
  
  ### Output ######################
  
  observe(rv$sim_output <- local_task())
  
  # observe(rv$sim_output <- task$result())
  # observe({
  #   if (!is.null(data_result))
  #     rv$sim_output <- data_result()
  # })
  
  output$sim_output_ui <- renderUI({
    result <- rv$sim_output
    if (is.null(result))
      return()
    print("Simulation done!")
    showNotification("Simulation done!")
    navset_card_underline(
      id = "output_tabs",
      height = "100%",
      nav_panel(
        "Output Variables",
        card_body(padding = 20, reactableOutput("output_vars_table"))
      ),
      !!!output_arr_panels(result)
    )
  })
  
  generate_output_plot <- function(df, key_df, arr) {
    # variable filter
    vars <- input[[paste0("input_plot_vars-", arr)]]
    if (!is.null(vars)) {
      df <- df[c("time", names(key_df), vars)]
    } else {
      vars <- setdiff(names(df), c("time", names(key_df)))
    }
    
    if (arr == "single_df") {
      fig <- plot_ly(type = "scatter", mode = "lines+markers")
      for (v in vars) {
        fig <- fig |> add_trace(
          x = df[["time"]],
          y = df[[v]],
          name = v,
          color = I(chart_color[match(v, vars)])
        )
      }
      return(fig)
    }
    # subplot filter
    sp <- input[[paste0("input_subplot-", arr)]]
    if (!is.null(sp)) {
      k_df <- as.data.frame(t(sapply(sp, function(a) {
        unlist(strsplit(a, " "))
      })))
      k <- unique(k_df[[1]])
      f_df <- df
      f_key_df <- key_df
      for (x in k) {
        f_df <- f_df[f_df[[x]] %in% k_df[k_df[[1]] == x, 2], ]
        f_key_df <- f_key_df[f_key_df[[x]] %in% k_df[k_df[[1]] == x, 2], ]
      }
      df <- f_df
      if (class(f_key_df) == "data.frame") {
        key_df <- f_key_df
      } else {
        key_df <- data.frame(f_key_df)
        colnames(key_df) <- k
      }
    }
    kn <- names(key_df)
    ncolplot <- length(unique(key_df[[1]]))
    nrowplot <- nrow(key_df) / ncolplot
    key_df$row <- 1:nrow(key_df)
    subfont = list(size = 14)
    figs <- apply(key_df, 1, function(k) {
      # get data with similar id for all selected keys
      row <- as.numeric(k[["row"]])
      coltitle <- ""
      if (row <= ncolplot) {
        coltitle <- paste0("<i>", kn[1], ":</i> <b>", k[[kn[1]]], "</b>")
      }
      k <- as.data.frame(t(as.data.frame(k)))
      k$row <- NULL
      rowtitle <- ""
      if (row %% ncolplot == 1 | ncolplot == 1) {
        rowtitle <-  paste(paste0("<i>", kn[-1], ":</i> <b>", k[-1], "</b>"), collapse = "; ")
      }
      kk <- k[rep(1, nrow(df)), ]
      kk_is <- df[kn] == kk
      df2 <- df[apply(kk_is, 1, function(x)
        all(x == T)), ]
      
      fig <- plot_ly(
        type = "scatter",
        mode = "lines",
        showlegend = ifelse(row == 1, T, F)
      )
      for (v in vars) {
        fig <- fig |> add_trace(
          x = df2[["time"]],
          y = df2[[v]],
          name = v,
          legendgroup = v,
          color = I(chart_color[match(v, vars)])
        )
      }
      fig <- fig |> layout(
        annotations = list(
          list(
            y = 1,
            yref = 'paper',
            yanchor = "bottom",
            text = coltitle,
            showarrow = FALSE,
            font = subfont
          )
        ),
        xaxis = list(title = "Time"),
        yaxis = list(title = list(text = rowtitle, font = subfont)),
        hoverlabel = list(namelength = -1)
      )
      return(fig)
    })
    
    subplot(
      figs,
      shareX = T,
      shareY = T,
      titleX = T,
      titleY = T,
      nrows = nrowplot
    )
  }
  
  observeEvent(input$show_plot, {
    i <- input$show_plot$index
    arr <- output_vars_df[i, "arr"]
    var <- output_vars_df[i, "var"]
    id = paste0("input_plot_vars-", arr)
    nav_select(id = "output_tabs", selected = arr)
    updateSelectInput(session, inputId = id, selected = var)
  })
  
  output_vars_df <- NULL
  io_file_df <- NULL
  
  observe({
    result <- rv$sim_output
    if (is.null(result))
      return()
    
    io_df <- data.frame(var = names(result))
    io_df$file <- sapply(io_df$var, prefix)
    io_file_df <<- io_df
    
    vars_df <- data.frame(var = sort(output_vars))
    vars_df$arr <- ""
    vars_df$details <- NA
    lapply(names(result), function(x) {
      df <- result[[x]]
      vars <- setdiff(names(df), c("time", names(wanulcas_def_arr[[x]])))
      vars_df[vars_df$var %in% vars, "arr"] <<- x
    })
    vars_df$cat <- sapply(vars_df$var, prefix)
    output_vars_df <<- vars_df
    
    output$output_vars_table <- renderReactable(
      reactable(
        vars_df,
        highlight = T,
        compact = T,
        striped = T,
        filterable = T,
        showPageSizeOptions = T,
        pageSizeOptions = c(10, 20, 40, 100),
        defaultPageSize = 20,
        groupBy = "cat",
        paginateSubRows = T,
        
        columns = list(
          details = colDef(
            name = "",
            sortable = FALSE,
            cell = function()
              actionButton(
                "show_plot",
                "Show Plot",
                icon = icon("chart-line", style = "margin-right:5px;"),
                style = compact_button_style
              )
          ),
          var = colDef(name = "Variable"),
          arr = colDef(name = "Array Dimension")
        ),
        onClick = JS(
          "function(rowInfo, column) {
          if (column.id !== 'details') return
          if (window.Shiny) {
            Shiny.setInputValue('show_plot', { index: rowInfo.index + 1 }, { priority: 'event' })
          }
        }"
        )
      )
    )
    
    lapply(names(result), function(x) {
      output[[paste("output_plot", x, sep = "_")]] <- renderPlotly(generate_output_plot(result[[x]], wanulcas_def_arr[[x]], x))
      output[[paste("output_data", x, sep = "_")]] <- renderReactable(reactable(
        result[[x]],
        pagination = F,
        highlight = T,
        compact = T,
        groupBy = names(wanulcas_def_arr[[x]])
      ))
    })
    
  })
  
  
  output_arr_panels <- function(result) {
    lapply(names(result), function(x) {
      df <- result[[x]]
      
      subplot <- NULL
      keys <- NULL
      if (x != "single_df") {
        keys <- apply(wanulcas_def_arr[[x]], 2, unique, simplify = F)
        subplot <- sapply(names(keys), function(a) {
          k <- paste(a, keys[[a]])
          kk <- as.list(k)
          names(kk) <- k
          kk
        }, simplify = F, USE.NAMES = T)
        
      }
      vars <- setdiff(names(df), c("time", names(keys)))
      
      nav_panel(x,
                card_body(
                  class = "bordercard",
                  padding = 10,
                  
                  navset_card_underline(
                    full_screen = T,
                    
                    title = flowLayout(
                      cellArgs = list(style = "width:auto; margin:0px;"),
                      div(
                        class = "d-flex align-items-center",
                        tags$label("Subplot filter:", style = "margin-right: 10px;"),
                        selectInput(
                          inputId = paste0("input_subplot-", x),
                          label = NULL,
                          choices = subplot,
                          multiple = TRUE
                        )
                      ),
                      div(
                        class = "d-flex align-items-center",
                        tags$label("Variables:", style = "margin-right: 10px;"),
                        selectInput(
                          inputId = paste0("input_plot_vars-", x),
                          label = NULL,
                          choices = vars,
                          multiple = TRUE
                        )
                      )
                    ),
                    nav_panel(
                      "Plot",
                      icon = icon("chart-line"),
                      card_body(padding = 5, plotlyOutput(paste(
                        "output_plot", x, sep = "_"
                      )))
                    ),
                    nav_panel(
                      "Data",
                      icon = icon("table"),
                      card_body(padding = 5, reactableOutput(paste(
                        "output_data", x, sep = "_"
                      )))
                    )
                  )
                ))
    })
  }
  
  #### Output vars selection ###################
  
  output$output_var_selector <- renderReactable({
    selected <- which(output_vars_option_df$var %in% default_output_vars,
                      arr.ind = TRUE)
    reactable(
      output_vars_option_df,
      selection = "multiple",
      onClick = "select",
      defaultSelected = selected,
      highlight = T,
      compact = T,
      striped = T,
      filterable = T,
      showPageSizeOptions = T,
      pageSizeOptions = c(10, 20, 40, 100),
      defaultPageSize = 20,
      groupBy = "cat",
      paginateSubRows = T,
      columns = list(
        cat = colDef(name = "Category"),
        var = colDef(name = "Variable"),
        arr = colDef(name = "Array Dimension")
      )
    )
  })
  
  output$output_var_selected <- renderReactable({
    i <- getReactableState("output_var_selector", "selected")
    df <- output_vars_option_df[i, c("var", "arr")]
    rownames(df) <- NULL
    reactable(
      df,
      highlight = T,
      compact = T,
      striped = T,
      showPageSizeOptions = T,
      pageSizeOptions = c(10, 20, 40, 100),
      defaultPageSize = 20,
      paginateSubRows = T,
      rownames = T,
      columns = list(
        var = colDef(name = "Variable"),
        arr = colDef(name = "Array Dimension")
      )
    )
  })
  
  observe({
    i <- getReactableState("output_var_selector", "selected")
    output_vars <<- output_vars_option_df[i, "var"]
    output$selected_vars_info <- renderUI(div(
      "Number of selected output variabels:",
      tags$strong(length(output_vars))
    ))
  })
  
  observeEvent(
    input$clear_selected_output_vars,
    updateReactable("output_var_selector", selected = NA)
  )
  
  observeEvent(input$reset_default_output_vars, {
    selected <- which(output_vars_option_df$var %in% default_output_vars,
                      arr.ind = TRUE)
    updateReactable("output_var_selector", selected = selected)
  })
  
  
  
  observeEvent(input$reset_button, {
    show_input_dialog(
      "Reset Output",
      "The current output will be removed. Continue resetting the output?",
      "confirm_reset_button",
      "Yes"
    )
  })
  
  observeEvent(input$confirm_reset_button, {
    removeModal()
    rv$sim_output <- NULL
  })
  
  ### UPLOAD ######################
  
  observeEvent(input$upload_parameter, {
    dpath <- input$upload_parameter$datapath
    set_parameters(read_params(dpath))
  })
  
  set_parameters <- function(params) {
    # vars
    lapply(inputvars_ui_id, function(x) {
      varnames <- inputvars_df[inputvars_df$ui_id == x, "var"]
      update_numeric_input_ui(x, params$vars[varnames])
    })
    
    # arrays
    arrays_df <- array_params_to_ui_inp(params$arrays)
    lapply(names(arrays_df), function(x) {
      rv_arr[[x]] <- arrays_df[[x]]
    })
    
    # graphs
    graph_df <- graph_params_to_ui_inp(params$graphs)
    lapply(names(graph_df), function(x) {
      rv_graph[[x]] <- graph_df[[x]]
    })
  }
  
  ### DOWNLOAD ######################
  
  output$download_parameter <- downloadHandler(
    filename = function() {
      paste("wanulcas_params.yaml")
    },
    content = function(fname) {
      pars <- isolate(get_input_parameters())
      write_params(pars, fname)
    }
  )
  
  output$download_output <- downloadHandler(
    filename = function() {
      paste("wanulcas_output.zip")
    },
    content = function(fname) {
      setwd(tempdir())
      fs <- save_variables(io_file_df, isolate(rv$sim_output))
      z <- zip::zip(zipfile = fname, files = fs)
      return(z)
    },
    contentType = "application/zip"
  )
  
}
