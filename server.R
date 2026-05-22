# Rahmat
# tambah tombol stop simulasi <- gak bisa, restart aja



server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2)
  
  options(
    reactable.theme = reactableTheme(
      style = list(fontFamily = "Arial, Helvetica, sans-serif", fontSize = "1em"),
      stripedColor = "#FBFAF4",
      highlightColor = "#F5F0E0"
    )
  )
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
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
  
  rv <- reactiveValues(
    wanulcas_cfg = list(),
    sim_output = NULL,
    output_timeseries_vars = default_output_timeseries_vars,
    output_final_vars = default_output_final_vars,
    output_graph_cfg = wanulcas_params_def$output$timeseries_layout
  )
  
  ### conditional variables #####################################
  
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
        col_disable = c(rep(T, nkeys), rep(F, nvar)),
        col_type = c(rep(NA, nkeys), rep("numeric", nvar))
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
      rv_graph_edit[[x]] <- table_edit_server(
        x,
        reactive(rv_graph[[x]]),
        allowRowModif = T,
        nrow = NA,
        col_type = rep("numeric", length(rv_graph[[x]]))
      )
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
    fig <- fig |> plotly::layout(
      legend = list(orientation = 'h'),
      showlegend = T,
      title = desc,
      yaxis = list(title = var),
      xaxis = list(title = wanulcas_params_def$graphs[[var]]$x_var),
      hoverlabel = list(namelength = -1)
    )
    
    fig <- fig |> plotly::layout(
      xaxis = list(showgrid = F),
      yaxis = list(title = ""),
      margin = list(l = 0),
      showlegend = F,
      title = ""
    )
    fig <- fig |> plotly::config(displayModeBar = FALSE)
    # }
    return(fig)
  }
  
  lapply(graph_vars, function(x) {
    gp_id <- paste("input_graph_plot", x, sep = "-")
    output[[gp_id]] <- renderPlotly(generate_graph_plot(x))
  })
  
  with_tooltip <- function(tooltip_col) {
    JS(
      sprintf(
        'function(cellInfo) {
    const style = "cursor: help"
    const title = cellInfo.row["%s"]
    return `<span style="${style}" title="${title}">${cellInfo.value}</span>`
   }',
        tooltip_col
      )
    )
  }
  
  #### crop species library ###############
  
  crop_ui_id <- "input_array_crop_7_0"
  tree_ui_id <- "input_array_tree_8_0"
  
  get_crop_list <- function() {
    c(user_crop(), crop_species_col)
  }
  
  crop_select_ids <- c("input_crop_1",
                       "input_crop_2",
                       "input_crop_3",
                       "input_crop_4",
                       "input_crop_5")
  
  output$input_crop_select <- renderUI({
    crop_list <- get_crop_list()
    cr_select <- rv_arr[[crop_ui_id]]$CQ_Species
    
    # Safe lapply to avoid zero-length errors
    crop_ui <- lapply(c(1:5), function(i) {
      selectInput(crop_select_ids[i],
                  paste0("Crop ", i, ":"),
                  crop_list,
                  selected = cr_select[i])
    })
    do.call(flowLayout, c(list(
      cellArgs = list(style = "width:200px; margin:0px;")
    ), crop_ui))
  })
  
  lapply(c(1:5), function(i) {
    observeEvent(input[[crop_select_ids[i]]], {
      rv_arr[[crop_ui_id]]$CQ_Species[i] <- input[[crop_select_ids[i]]]
    })
  })
  
  output$input_crop_lib <- renderReactable({
    edit_crop <- user_crop()
    edit_col <- NULL
    if (length(edit_crop) > 0) {
      edit_col <- lapply(edit_crop, function(x) {
        colDef(
          cell = text_extra("crop_edit", class = "reactable-text-input"),
          headerStyle = list(
            background = theme_color$primary,
            color = "#FFF"
          )
        )
      })
      names(edit_col) <- edit_crop
    }
    hpar <- list(color = theme_color$primary)
    reactable(
      crop_species_df[c(crop_key_col, edit_crop , crop_species_col)],
      highlight = T,
      compact = T,
      striped = T,
      pagination = F,
      groupBy = "group",
      columns = c(
        list(
          group = colDef(
            name = "Categories",
            width = 240,
            style = list(color = theme_color$primary),
            headerStyle = hpar
          ),
          var_desc = colDef(show = F),
          var_label = colDef(
            name = "Parameters",
            width = 240,
            style = list(color = theme_color$primary),
            headerStyle = hpar,
            html = TRUE,
            cell = with_tooltip("var_desc")
          ),
          sub_var = colDef(
            name = "Att",
            style = list(color = theme_color$primary, width = 30),
            headerStyle = hpar
          )
        ),
        edit_col
      )
    )
  })
  
  observeEvent(input$crop_edit, {
    i <- input$crop_edit
    crop_species_df[i$row, i$column] <<- as.numeric(i$value)
  })
  
  observeEvent(input$add_crop_button, {
    show_input_dialog(
      "Add New Crop Type",
      "Select the base crop parameter from the available library and define the crop name",
      "confirm_add_crop",
      input_var = "input_crop_name",
      input_label = "New crop name:",
      custom_input = selectInput(
        "input_crop_def",
        "Base parameters:",
        crop_species_col
      )
    )
  })
  
  user_crop <- reactiveVal()
  
  observeEvent(input$confirm_add_crop, {
    removeModal()
    cn <- input$input_crop_name
    if (cn == "")
      return()
    crop_species_df[[cn]] <<- crop_species_df[[input$input_crop_def]]
    user_crop(c(user_crop(), cn))
  })
  
  observeEvent(input$remove_crop_button, {
    if (length(user_crop()) == 0)
      return()
    show_input_dialog(
      "Remove Crop",
      "",
      "confirm_remove_crop",
      custom_input = selectInput("removed_crop", "Select the crop to removed:", user_crop())
    )
  })
  
  observeEvent(input$confirm_remove_crop, {
    removeModal()
    rc <- input$removed_crop
    if (rc == "")
      return()
    crop_species_df[[rc]] <<- NULL
    uc <- user_crop()
    uc <-  uc[uc != rc]
    user_crop(uc)
  })
  
  observe({
    req(input$crop_edit_text)
    values <- input$crop_edit_text
  })
  
  #### tree species library ###############
  
  get_tree_list <- function() {
    c(user_tree(), tree_species_col)
  }
  
  tree_select_ids <- c("input_tree_1", "input_tree_2", "input_tree_3")
  
  output$input_tree_select <- renderUI({
    tree_list <- get_tree_list()
    tr_select <- rv_arr[[tree_ui_id]]$T_Species
    # Safe lapply to avoid zero-length errors
    tree_ui <- lapply(c(1:3), function(i) {
      selectInput(tree_select_ids[i],
                  paste0("Tree ", i, ":"),
                  tree_list,
                  selected = tr_select[i])
    })
    do.call(flowLayout, c(list(
      cellArgs = list(style = "width:300px; margin:0px;")
    ), tree_ui))
  })
  
  lapply(c(1:3), function(i) {
    observeEvent(input[[tree_select_ids[i]]], {
      rv_arr[[tree_ui_id]]$T_Species[i] <- input[[tree_select_ids[i]]]
    })
  })
  
  output$input_tree_lib <- renderReactable({
    edit_tree <- user_tree()
    
    edit_col <- NULL
    if (length(edit_tree) > 0) {
      edit_col <- lapply(edit_tree, function(x) {
        colDef(
          cell = text_extra("tree_edit", class = "reactable-text-input"),
          headerStyle = list(
            background = theme_color$primary,
            color = "#FFF"
          )
        )
      })
      names(edit_col) <- edit_tree
    }
    hpar <- list(color = theme_color$primary)
    reactable(
      tree_species_df[c(tree_key_col, edit_tree , tree_species_col)],
      highlight = T,
      compact = T,
      striped = T,
      pagination = F,
      groupBy = "group",
      columns = c(
        list(
          group = colDef(
            name = "Categories",
            width = 300,
            style = list(color = theme_color$primary),
            headerStyle = hpar
          ),
          var_desc = colDef(show = F),
          var_label = colDef(
            name = "Parameters",
            width = 300,
            style = list(color = theme_color$primary),
            headerStyle = hpar,
            html = TRUE,
            cell = with_tooltip("var_desc")
          ),
          sub_var = colDef(
            name = "Att",
            style = list(color = theme_color$primary, width = 30),
            headerStyle = hpar
          )
        ),
        edit_col
      )
    )
  })
  
  observeEvent(input$tree_edit, {
    i <- input$tree_edit
    tree_species_df[i$row, i$column] <<- as.numeric(i$value)
  })
  
  observeEvent(input$add_tree_button, {
    show_input_dialog(
      "Add New tree Type",
      "Select the base tree parameter from the available library and define the tree name",
      "confirm_add_tree",
      input_var = "input_tree_name",
      input_label = "New tree name:",
      custom_input = selectInput(
        "input_tree_def",
        "Base parameters:",
        tree_species_col
      )
    )
  })
  
  user_tree <- reactiveVal()
  
  observeEvent(input$confirm_add_tree, {
    removeModal()
    cn <- input$input_tree_name
    if (cn == "")
      return()
    tree_species_df[[cn]] <<- tree_species_df[[input$input_tree_def]]
    user_tree(c(user_tree(), cn))
  })
  
  observeEvent(input$remove_tree_button, {
    if (length(user_tree()) == 0)
      return()
    show_input_dialog(
      "Remove tree",
      "",
      "confirm_remove_tree",
      custom_input = selectInput("removed_tree", "Select the tree to removed:", user_tree())
    )
  })
  
  observeEvent(input$confirm_remove_tree, {
    removeModal()
    rc <- input$removed_tree
    if (rc == "")
      return()
    tree_species_df[[rc]] <<- NULL
    uc <- user_tree()
    uc <-  uc[uc != rc]
    user_tree(uc)
  })

  ### Run Simulation #############
  validate_crop <- function(input_crop) {
    ifelse(is.null(input_crop),
           c(user_crop(), crop_species_col)[1],
           input_crop)
  }
  
  validate_tree <- function(input_tree) {
    ifelse(is.null(input_tree),
           c(user_tree(), tree_species_col)[1],
           input_tree)
  }
  
  species_var_keys <- c("var", "sub_var")
  
  apply_species_params <- function(params) {
    croplist <- rv_arr[[crop_ui_id]]$CQ_Species
    croplist_df <- crop_species_df[c(species_var_keys, croplist)]
    treelist <- rv_arr[[tree_ui_id]]$T_Species
    treelist_df <- tree_species_df[c(species_var_keys, treelist)]
    params <- params |> apply_croplist_params(croplist_df) |> apply_treelist_params(treelist_df)
    return(params)
  }
  
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
    
    params <- apply_species_params(params)
    
    # species library
    edit_crop <- isolate(user_crop())
    if(length(edit_crop) > 0) {
      df <- crop_species_df[c(species_var_keys, edit_crop)]
      params[["crop_library"]] <- df
    }
    edit_tree <- isolate(user_tree())
    if(length(edit_tree) > 0) {
      df <- tree_species_df[c(species_var_keys, edit_tree)]
      params[["tree_library"]] <- df
    }
    return(params)
  }
  
  # sim_output <- reactiveVal()
  local_task <- reactiveVal()
  
  task <- ExtendedTask$new(
    function(n, pars, outvars, progress)
      mirai(
        run_wanulcas(n, pars, outvars, progress),
        run_wanulcas = run_wanulcas,
        n = n,
        pars = pars,
        outvars = outvars,
        progress = progress
      )
  ) |> bind_task_button("sim_run_button")
  
  if (is_run_online) {
    observeEvent(input$sim_run_button, {
      if (!is_simulation_ready())
        return()
      
      n_iteration <- input$n_iteration
      pars <- isolate(get_input_parameters())
      progress <- AsyncProgress$new(
        session,
        min = 1,
        max = n_iteration,
        message = "Processing the server",
        detail = "Please wait while preparing the server session.."
      )
      on.exit(progress$close())
      progress_trigger <- function(i, n) {
        progress$set(i, "Running simulation", paste("Day", i, "of", n))
      }
      print("Starting simulation: online")
      task$invoke(
        n_iteration,
        pars,
        output_timeseries_vars = rv$output_timeseries_vars,
        output_final_vars = rv$output_final_vars,
        progress = progress_trigger
      )
    })
  } else {
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
      print("Starting simulation: local")
      run_wanulcas(
        n_iteration,
        pars,
        output_timeseries_vars = rv$output_timeseries_vars,
        output_final_vars = rv$output_final_vars,
        progress = progress_trigger
      )
    })
  }
  
  is_simulation_ready <- function() {
    if (length(rv$output_timeseries_vars) == 0) {
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
  observe(rv$sim_output <- task$result())
  
  #### Output vars selection ###################
  
  output$output_var_selector <- renderReactable({
    selected <- which(output_vars_disp_df$var %in% rv$output_timeseries_vars,
                      arr.ind = TRUE)
    reactable(
      output_vars_disp_df,
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
      paginateSubRows = T,
      columns = list(
        var = colDef(name = "Variable"),
        arr = colDef(name = "Array Dimension")
      )
    )
  })
  
  output$output_final_var_selector <- renderReactable({
    selected <- which(output_vars_disp_df$var %in% rv$output_final_vars,
                      arr.ind = TRUE)
    reactable(
      output_vars_disp_df,
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
      paginateSubRows = T,
      columns = list(
        var = colDef(name = "Variable"),
        arr = colDef(name = "Array Dimension")
      )
    )
  })
  
  
  output$output_var_selected <- renderUI({
    i <- getReactableState("output_var_selector", "selected")
    df <- output_vars_disp_df[i, "var"]
    output$selected_vars_info <- renderUI(tags$strong(length(df)))
    tags$ul(lapply(df, tags$li))
  })
  
  observe({
    i <- getReactableState("output_var_selector", "selected")
    if (!is.null(i)) {
      rv$output_timeseries_vars <- output_vars_disp_df[i, "var"]
    }
  })
  
  observeEvent(input$clear_selected_output_vars,
               rv$output_timeseries_vars <- c())
  
  observeEvent(input$reset_default_output_vars, {
    selected <- which(output_vars_disp_df$var %in% default_output_timeseries_vars,
                      arr.ind = TRUE)
    updateReactable("output_var_selector", selected = selected)
  })
  
  output$output_final_var_selected <- renderUI({
    i <- getReactableState("output_final_var_selector", "selected")
    df <- output_vars_disp_df[i, "var"]
    output$selected_final_vars_info <- renderUI(tags$strong(length(df)))
    tags$ul(lapply(df, tags$li))
  })
  
  observe({
    i <- getReactableState("output_final_var_selector", "selected")
    if (!is.null(i)) {
      rv$output_final_vars <- output_vars_disp_df[i, "var"]
    }
  })
  
  observeEvent(input$clear_selected_output_final_vars,
               rv$output_final_vars <- c())
  
  observeEvent(input$reset_default_output_final_vars, {
    selected <- which(output_vars_disp_df$var %in% default_output_final_vars,
                      arr.ind = TRUE)
    updateReactable("output_final_var_selector", selected = selected)
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

  #### dynamic output graph  #########################

  card_id_counter <- 0
  get_next_card_id <- function() {
    card_id_counter <<- card_id_counter + 1
    return(paste0("outgraph", card_id_counter))
  }
  
  page_id_counter <- 0
  get_next_page_id <- function() {
    page_id_counter <<- page_id_counter + 1
    return(paste0("page", page_id_counter))
  }

  reset_output_config <- function() {
    card_id_counter <<- 0
    page_id_counter <<- 0
    rv$output_graph_cfg <- list()
  }
  
  generate_dim_keys <- function(sp, arr, df) {
    if (is.null(arr) || is.na(arr))
      return()
    # filter dataframe with the selected dimension keys
    if (arr == "single_df") {
      key_df <- data.frame(single = 0)
    } else {
      key_df <- wanulcas_def_arr[[arr]]
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
    }
    return(key_df)
  }
  
  generate_output_graph <- function(df, key_df, vars) {
    if (is.null(key_df))
      return()
    kn <- names(key_df)
    ncolplot <- length(unique(key_df[[1]]))
    nrowplot <- nrow(key_df) / ncolplot
    key_df$row <- 1:nrow(key_df)
    subfont = list(size = 14)
    figs <- apply(key_df, 1, function(k) {
      # get data with similar id for all selected keys
      row <- as.numeric(k[["row"]])
      if (kn[1] == "single") {
        df2 <- df
        coltitle <- ""
        rowtitle <- ""
      } else {
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
      }
      
      fig <- plot_ly(
        type = "scatter",
        mode = "lines",
        showlegend = ifelse(length(vars) == 1 || row != 1, F, T)
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
      fig <- fig |> plotly::layout(
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
        xaxis = list(title = "Days"),
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
  
  ### output time series UI ##############
  
  output$sim_output_ui <- renderUI({
    out <- rv$sim_output
    if (is.null(out))
      return()
    formatted_output_data(format_output_data(out$timeseries_vars))
    output_cfg <- isolate(rv$output_graph_cfg)
    #reset the output cfg
    reset_output_config()
    
    page_panels <- NULL
    if (length(output_cfg) > 0) {
      n <- length(output_cfg)
      page_ids <- replicate(n, get_next_page_id())
      page_panels <- lapply(1:n, function(i) {
        create_output_page_panel(page_ids[i], output_cfg[[i]]$title, output_cfg[[i]]$content)
      })
      names(page_panels) <- NULL
    }
    
    btn <- actionButton(
      "add_output_page_button",
      "Add New Page",
      icon = icon("plus"),
      style = compact_button_style
    )
    card_body(do.call(navset_card_pill, c(
      list(id = "output_timeseries", nav_item(btn), nav_spacer()),
      page_panels
    )))
  })
  
  observeEvent(input$add_output_page_button, {
    show_input_dialog(
      "Add New Output Page",
      confirm_id = "confirm_add_page",
      input_var = "input_page_title",
      input_label = "Page title:"
    )
  })
  
  user_crop <- reactiveVal()
  
  observeEvent(input$confirm_add_page, {
    removeModal()
    p_id <- get_next_page_id()
    add_output_page(p_id, input$input_page_title)
  })
  
  get_output_page_ui <- function(page_id, title, content_list = NULL) {
    add_card_button_id <- paste0("add_dynamic_card_button_", page_id)
    observeEvent(input[[add_card_button_id]], add_output_card(page_id, title))
    
    remove_page_button_id <- paste0("remove_page_button_", page_id)
    observeEvent(input[[remove_page_button_id]], nav_remove("output_timeseries", page_id))
    
    nav_panel(value = page_id, title = title, div(
      do.call(layout_column_wrap, c(
        list(
          id = paste0("dynamic_card_container_", page_id),
          width = "400px",
          style = "margin:10px"
        ),
        content_list
      )),
      div(
        style = "width:100%",
        actionButton(
          style = "float:right; margin-right:50px",
          add_card_button_id,
          "Add Output Graph",
          icon = icon("plus")
        ),
        actionButton(
          style = "float:right; margin-right:20px",
          remove_page_button_id,
          "Remove This Page",
          icon = icon("trash-can")
        )
      )
    ))
  }
  
  add_output_page <- function(page_id, title) {
    set_output_config_page(page_id, title)
    nav_insert("output_timeseries",
               get_output_page_ui(page_id, title),
               select = TRUE)
  }
  
  set_output_config_page <- function(page_id, title) {
    rv$output_graph_cfg[[page_id]][["title"]] <- title
  }
  
  set_output_config_content <- function(page_id,
                                        page_title,
                                        card_id,
                                        vars = NULL,
                                        filter = NULL) {
    if (!is.null(page_title)) {
      rv$output_graph_cfg[[page_id]][["title"]] <- page_title
    }
    rv$output_graph_cfg[[page_id]][["content"]][[card_id]][["vars"]] <- vars
    rv$output_graph_cfg[[page_id]][["content"]][[card_id]][["filter"]] <- filter
  }
  
  create_output_page_panel <- function(page_id, title, content_cfg = NULL) {
    content_list <- list()
    if (!is.null(content_cfg)) {
      n <- length(content_cfg)
      card_ids <- replicate(n, get_next_card_id())
      data <- isolate(formatted_output_data())
      content_list <- lapply(1:n, function(i) {
        id <- card_ids[i]
        vars <- content_cfg[[i]]$vars
        filter <- content_cfg[[i]]$filter
        card_graph_ui(id, data, vars, filter)
      })
      names(content_list) <- NULL
      
      lapply(1:n, function(i) {
        id <- card_ids[i]
        card_graph_server(
          id,
          data,
          content_cfg = content_cfg[[i]],
          page_id = page_id,
          page_title = title,
          update_card = set_output_config_content
        )
      })
    }
    
    content_list <- c(content_list, uiOutput(paste0("graph_add_", page_id)))
    get_output_page_ui(page_id, title, content_list)
    
  }
  
  add_output_card <- function(page_id, title) {
    id <- get_next_card_id()
    data <- isolate(formatted_output_data())
    ui <- card_graph_ui(id, data)
    insertUI(
      selector = paste0("#dynamic_card_container_", page_id),
      where = "beforeEnd",
      ui = ui
    )
    card_graph_server(
      id,
      data,
      page_id = page_id,
      page_title = title,
      update_card = set_output_config_content
    )
    return(id)
  }
  
  ### output final vars UI ###############
  
  fin_id_prefix <- "output_final_var_"
  
  output$sim_output_final_ui <- renderUI({
    out <- rv$sim_output
    if (is.null(out))
      return()
    out_list <- out$final_vars
    # Safe lapply to avoid zero-length errors
    out_ui <- lapply(names(out_list), function(x) {
      card(
        full_screen = T,
        card_header(
          class = "d-flex justify-content-between",
          suffix_remove(x),
          table_download_link(
            paste0(fin_id_prefix, x),
            paste0(suffix_remove(x), "_final_vars.csv")
          )
        ),
        reactableOutput(paste0(fin_id_prefix, x))
      )
    })
    card_body(do.call(flowLayout, c(list(
      cellArgs = list(style = "width:auto; margin:0px; max-width:600px")
    ), out_ui)))
  })
  
  observe({
    out <- rv$sim_output
    if (is.null(out))
      return()
    out_list <- out$final_vars
    lapply(names(out_list), function(x) {
      output[[paste0(fin_id_prefix, x)]] <- renderReactable(reactable(
        out_list[[x]],
        defaultColDef = colDef(width = 120, cell = numeric_cell_coldef)
      ))
    })
  })

  output$download_output_cfg <- downloadHandler(
    filename = function() {
      paste("output_config.yaml")
    },
    content = function(fname) {
      write_yaml(rv$output_graph_cfg, fname)
    },
    contentType = "application/yaml"
  )
  
  ### UPLOAD ######################
  
  observeEvent(input$upload_parameter, {
    dpath <- input$upload_parameter$datapath
    set_parameters(read_params(dpath))
    show_alert(
      "Upload Successful!",
      "The paramaters file has been successfully uploaded!",
      "success"
    )
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
    
    if(!is.null(params$crop_library)) {
      df <- as.data.frame(do.call(cbind, params$crop_library))
      crop_names <- setdiff(names(df), species_var_keys)
      crop_species_df <<- cbind(crop_species_df, df[crop_names])
      user_crop(crop_names)
    }
    
    if(!is.null(params$tree_library)) {
      df <- as.data.frame(do.call(cbind, params$tree_library))
      tree_names <- setdiff(names(df), species_var_keys)
      tree_species_df <<- cbind(tree_species_df, df[tree_names])
      user_tree(tree_names)
    }
    
    crops <- rv_arr[[crop_ui_id]]$CQ_Species
    for (i in 1:5) {
      updateSelectInput(session, crop_select_ids[i], selected = crops[i])
    }
    
    trees <- rv_arr[[tree_ui_id]]$T_Species
    for (i in 1:5) {
      updateSelectInput(session, tree_select_ids[i], selected = trees[i])
    }
    
    if (!is.null(params$output$timeseries_vars))
      rv$output_timeseries_vars <- params$output$timeseries_vars
    if (!is.null(params$output$timeseries_layout))
      rv$output_graph_cfg <- params$output$timeseries_layout
  }
  
  observeEvent(input$upload_xls_parameter, {
    dpath <- input$upload_xls_parameter$datapath
    params <- get_input_parameters()
    params <- apply_xls_params(params, dpath, xls_config_df)
    set_parameters(params)
    show_alert(
      "Upload Successful!",
      "The MS-Excel paramaters file has been successfully uploaded and applied!",
      "success"
    )
  })
  
  formatted_output_data <- reactiveVal()
  
  format_output_data <- function(data) {
    if (is.null(data)) {
      return()
    }
    var_data <- lapply(names(data), function(x) {
      df <- data[[x]]
      keys <- NULL
      keys_label <- NULL
      if (x != "single_df") {
        keys <- apply(wanulcas_def_arr[[x]], 2, unique, simplify = F)
        keys_label <- sapply(names(keys), function(a) {
          k <- paste(a, keys[[a]])
          kk <- as.list(k)
          names(kk) <- k
          kk
        }, simplify = F, USE.NAMES = T)
      }
      v <- setdiff(names(df), c("time", names(keys)))
      list(
        vars = v,
        keys = keys_label,
        arr = x,
        data = df
      )
    })
    names(var_data) <- names(data)
    
    v <- unlist(sapply(var_data, function(x)
      x[["vars"]]))
    a <- unlist(sapply(var_data, function(x)
      rep(x[["arr"]], length(x[["vars"]]))))
    var_df <- data.frame(vars = v, arr = a)
    # a variable may exist on multiple array, it should be selected to the shortest array dimensions
    # TODO: should prevented on the wanulcas output loop
    var_df <- aggregate(
      var_df[2],
      var_df[-2],
      FUN = function(x) {
        d <- unique(x)
        d[which.min(nchar(d))]
      }
    )
    return(list(var_df = var_df, arr_data = var_data))
  }
  
  observeEvent(input$upload_output_data_button, {
    print(paste("Extracting the files:", input$upload_parameter$name))
    dpath <- input$upload_output_data_button$datapath
    data <- upload_output_data(dpath)
    fdata <- format_output_data(data)
    formatted_output_data(fdata)
  })
  
  show_alert_file_error <- function(file_error) {
    show_alert("File Error!",
               paste("File error! Or it is not a", file_error, "file!"),
               type = "error")
  }
  
  data_dir <- paste0(tempdir(), "/data_temp")
  
  upload_output_data <- function(dpath) {
    file_list <- NULL
    try(file_list <- utils::unzip(dpath, list = TRUE), silent = T)
    if (is.null(file_list)) {
      show_alert_file_error("compressed (zip)")
      return()
    }
    utils::unzip(dpath, exdir = data_dir, junkpaths = T)
    d <- list()
    for (f in file_list$Name) {
      arr <- paste0(prefix(f, "."), "_df")
      fpath <- paste0(data_dir, "/", f)
      df <- read.csv(fpath)
      d[[arr]] <- df
    }
    print("Output data uploaded!")
    showNotification("Output data uploaded!", type = "message")
    return(d)
  }
  
  
  ### DOWNLOAD ######################
  
  output$download_parameter <- downloadHandler(
    filename = function() {
      paste("wanulcas_params.yaml")
    },
    content = function(fname) {
      pars <- isolate(get_input_parameters())
      pars$output <- list(
        timeseries_vars = rv$output_timeseries_vars,
        timeseries_layout = rv$output_graph_cfg
      )
      write_params(pars, fname)
    }
  )
  
  output$download_output <- downloadHandler(
    filename = function() {
      paste("wanulcas_output.zip")
    },
    content = function(fname) {
      sim_output <- rv$sim_output
      if (is.null(sim_output))
        return()
      
      lv <- sim_output$log_vars
      log_arr <- paste0("log_", names(lv))
      names(lv) <- log_arr
      
      fv <- sim_output$final_vars
      fin_arr <- paste0("final_", names(fv))
      names(fv) <- fin_arr
      
      io_df <- data.frame(var = c(log_arr, fin_arr))
      io_df$file <- sapply(io_df$var, function(x)
        paste(head(unlist(
          strsplit(x, "_")
        ), -1), collapse = "_"))
      
      setwd(tempdir())
      fs <- save_variables(io_df, c(lv, fv))
      z <- zip::zip(zipfile = fname, files = fs)
      return(z)
    },
    contentType = "application/zip"
  )
  
  
  ### GUI #######################
  
  
  card_graph_ui <- function(id,
                            data,
                            def_vars = NULL,
                            def_filter = NULL) {
    var_df <- data$var_df
    arr_data <- data$arr_data
    def_v_choices <- var_df$vars
    def_arr <- NULL
    def_dim_choices <- NULL
    if (!is.null(def_vars)) {
      def_arr <- var_df[var_df$vars == def_vars[1], "arr"]
      if (length(def_arr) > 0) {
        def_v_choices <- var_df[var_df$arr == def_arr, "vars"]
        def_dim_choices <- arr_data[[def_arr]]$keys
      }
    }
    
    ns <- NS(id)
    button_id <- ns("remove_card_button")
    var_select_id <- ns("var_select")
    dim_id <- ns("dim_compare")
    var_ext_select_id <- ns("var_ext_select")
    sp_select_id <- ns("sp_select")
    graph_id <- ns("graph_id")
    card_title_id <- ns("card_title")
    table_id <- ns("table")
    
    div(
      id = ns("card"),
      navset_card_underline(
        full_screen = T,
        title = textOutput(card_title_id),
        
        sidebar = sidebar(
          open = is.null(def_vars),
          selectizeInput(
            inputId = var_select_id,
            label = "Variables:",
            choices = def_v_choices,
            selected = def_vars,
            multiple = TRUE,
            options = list(dropdownParent = 'body')
          ),
          conditionalPanel(
            condition = "input['var_select'].length > 0",
            ns = ns,
            selectizeInput(
              inputId = sp_select_id,
              label = "Filter:",
              choices = def_dim_choices,
              selected = def_filter,
              multiple = TRUE,
              options = list(dropdownParent = 'body')
            )
          )
        ),
        nav_spacer(),
        nav_panel(
          "Graph",
          icon = icon("chart-line"),
          card_body(padding = 0, plotlyOutput(graph_id, height = "300px"))
        ),
        nav_panel(
          "Data",
          icon = icon("table"),
          card_body(
            padding = 0,
            download_link(table_id),
            reactableOutput(table_id)
          )
        ),
        nav_item(actionLink(button_id, "", icon = icon("trash-can")))
      )
    )
  }
  
  card_graph_server <- function(id,
                                data,
                                content_cfg = NULL,
                                page_id,
                                page_title,
                                update_card) {
    moduleServer(id, function(input, output, session) {
      variable_df <- reactiveVal(data$var_df)
      array_data <- reactiveVal(data$arr_data)
      
      selected_df <- reactiveVal()
      key_df <- reactiveVal()
      
      page_id <- page_id
      page_title <- page_title
      button_id <- "remove_card_button"
      var_select_id <- "var_select"
      dim_id <- "dim_compare"
      sp_select_id <- "sp_select"
      graph_id <- "graph_id"
      card_title_id <- "card_title"
      table_id <- "table"
      
      observeEvent(input[[button_id]], {
        ns <- NS(id)
        removeUI(selector = paste0("#", ns("card")))
        update_card(page_id, page_title, id, NULL, NULL)
      })
      
      # variable selection
      observeEvent(input[[var_select_id]], {
        vs <- input[[var_select_id]]
        sp <- input[[sp_select_id]]
        var_df <- variable_df()
        arr_data <- array_data()
        output[[card_title_id]] <- renderText(paste(vs, collapse = ", "))
        if (is.null(vs)) {
          if (is.null(content_cfg)) {
            updateSelectizeInput(session, var_select_id, choices = var_df$vars)
            updateSelectizeInput(session,
                                 sp_select_id,
                                 choices = character(0),
                                 selected = character(0))
          } else {
            updateSelectizeInput(
              session,
              var_select_id,
              choices = var_df$vars,
              selected = content_cfg$vars
            )
            content_cfg <<- NULL
          }
          output[[dim_id]] <- NULL
        } else if (length(vs) == 1) {
          arr <- var_df[var_df$vars == vs, "arr"]
          vfilt <- var_df[var_df$arr == arr, "vars"]
          dim_choices <- arr_data[[arr]]$keys
          # sp <- input[[sp_select_id]]
          updateSelectizeInput(session,
                               var_select_id,
                               choices = vfilt,
                               selected = vs)
          updateSelectizeInput(session,
                               sp_select_id,
                               choices = dim_choices,
                               selected = sp)
        }
        update_card(page_id, page_title, id, vs, sp)
      }, ignoreNULL = FALSE)
      
      # sub plot or array dimension selection
      observeEvent(input[[sp_select_id]], {
        sp <- input[[sp_select_id]]
        vs <- input[[var_select_id]]
        var_df <- variable_df()
        arr <- var_df[var_df$vars == vs[1], "arr"]
        vfilt <- var_df[var_df$arr == arr, "vars"]
        if (is.null(sp)) {
          output[[dim_id]] <- NULL
        } else if (length(sp) >= 1) {
          updateSelectizeInput(session,
                               var_select_id,
                               choices = vfilt,
                               selected = vs)
          
        }
        update_card(page_id, page_title, id, vs, sp)
      }, ignoreNULL = FALSE)
      
      output[[graph_id]] <- renderPlotly({
        vs <- input[[var_select_id]]
        if (is.null(vs)) {
          validate(need(F, text_output_empty))
        } else {
          generate_output_graph(selected_df(), key_df(), vs)
        }
      })
      
      output[[table_id]] <- renderReactable({
        vs <- input[[var_select_id]]
        if (is.null(vs)) {
          validate(need(F, text_output_empty))
        } else {
          reactable(
            selected_df(),
            highlight = T,
            compact = T,
            showPageSizeOptions = T,
            defaultColDef = colDef(cell = numeric_cell_coldef)
          )
        }
      })
 
      observe({
        vs <- input[[var_select_id]]
        sp <- input[[sp_select_id]]
        var_df <- variable_df()
        arr_data <- array_data()
        v_df <- var_df[var_df$vars %in% vs, ]
        arr <- unique(v_df$arr)[1]
        df <- arr_data[[arr]]$data[c("time", names(arr_data[[arr]]$keys), vs)]
        keys <- generate_dim_keys(sp, arr, df)
        f_df <- df
        if (!is.null(keys) && ncol(keys) > 0) {
          for (n in names(keys)) {
            if (n == "single")
              next
            f_df <- f_df[f_df[[n]] %in% unique(keys[[n]]), ]
          }
        }
        key_df(keys)
        selected_df(f_df)
      })
      
    })
  }
  
}
