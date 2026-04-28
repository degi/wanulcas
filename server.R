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
  
  rv <- reactiveValues(
    wanulcas_cfg = list(),
    sim_output = NULL,
    output_vars = default_output_vars,
    output_graph_cfg = list()
  )
  
  # validate_output_vars <- function() {
  #   print(rv$output_vars)
  #   if(is.null(rv$output_vars)) rv$output_vars <- default_output_vars
  # }
  
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
      rv_graph_edit[[x]] <- table_edit_server(x, reactive(rv_graph[[x]]), allowRowModif = T, nrow = NA,
                                              col_type = rep("numeric", length(rv_graph[[x]])))
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
    
    is_fullscreen <- input[[paste0("input_graph_card-", var, "_full_screen")]]
    if (!is_fullscreen) {
      fig <- fig |> plotly::layout(
        xaxis = list(showgrid = F),
        yaxis = list(title = ""),
        margin = list(l = 0),
        showlegend = F,
        title = ""
      )
      fig <- fig |> plotly::config(displayModeBar = FALSE)
    }
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
    flowLayout(cellArgs = list(style = "width:200px; margin:0px;"),
               !!!lapply(c(1:5), function(i) {
                 selectInput(crop_select_ids[i],
                             paste0("Crop ", i, ":"),
                             crop_list,
                             selected = cr_select[i])
               }))
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
          cell = text_extra("crop_edit_text", class = "reactable-text-input"),
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
  
  observeEvent(input$add_crop_button, {
    show_input_dialog(
      "Add New Crop Type",
      "Please select the default crop parameter from the available library and define the crop name",
      "confirm_add_crop",
      input_var = "input_crop_name",
      input_label = "New crop name:",
      custom_input = selectInput(
        "input_crop_def",
        "Default crop paramaters:",
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
    print(values)
  })
  
  #### tree species library ###############
  
  get_tree_list <- function() {
    c(user_tree(), tree_species_col)
  }
  
  tree_select_ids <- c("input_tree_1", "input_tree_2", "input_tree_3")
  
  output$input_tree_select <- renderUI({
    tree_list <- get_tree_list()
    tr_select <- rv_arr[[tree_ui_id]]$T_Species
    flowLayout(cellArgs = list(style = "width:300px; margin:0px;"),
               !!!lapply(c(1:3), function(i) {
                 selectInput(tree_select_ids[i],
                             paste0("Tree ", i, ":"),
                             tree_list,
                             selected = tr_select[i])
               }))
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
          cell = text_extra("tree_edit_text", class = "reactable-text-input"),
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
  
  observeEvent(input$add_tree_button, {
    show_input_dialog(
      "Add New tree Type",
      "Please select the default tree parameter from the available library and define the tree name",
      "confirm_add_tree",
      input_var = "input_tree_name",
      input_label = "New tree name:",
      custom_input = selectInput(
        "input_tree_def",
        "Default tree paramaters:",
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
  
  observe({
    req(input$tree_edit_text)
    values <- input$tree_edit_text
    print(values)
  })
  
  #### oilpalm species library ###############
  
  output$input_oilpalm_select <- renderUI({
    oilpalm_list <-  c(user_crop(), oilpalm_species_col)
    flowLayout(
      cellArgs = list(style = "width:300px; margin:0px;"),
      selectInput("input_oilpalm_1", "oilpalm 1:", oilpalm_list),
      selectInput("input_oilpalm_2", "oilpalm 2:", oilpalm_list),
      selectInput("input_oilpalm_3", "oilpalm 3:", oilpalm_list)
      
      # selectInput("input_oilpalm_1", "oilpalm 1:", oilpalm_list, selected = oilpalm_list[1]),
      # selectInput("input_oilpalm_2", "oilpalm 2:", oilpalm_list, selected = oilpalm_list[1]),
      # selectInput("input_oilpalm_3", "oilpalm 3:", oilpalm_list, selected = oilpalm_list[1])
    )
  })
  
  output$input_oilpalm_lib <- renderReactable({
    edit_oilpalm <- user_oilpalm()
    
    edit_col <- NULL
    if (length(edit_oilpalm) > 0) {
      edit_col <- lapply(edit_oilpalm, function(x) {
        colDef(
          cell = text_extra("oilpalm_edit_text", class = "reactable-text-input"),
          headerStyle = list(
            background = theme_color$primary,
            color = "#FFF"
          )
        )
      })
      names(edit_col) <- edit_oilpalm
    }
    hpar <- list(color = theme_color$primary)
    reactable(
      oilpalm_species_df[c(oilpalm_key_col, edit_oilpalm , oilpalm_species_col)],
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
  
  observeEvent(input$add_oilpalm_button, {
    show_input_dialog(
      "Add New oilpalm Type",
      "Please select the default oilpalm parameter from the available library and define the oilpalm name",
      "confirm_add_oilpalm",
      input_var = "input_oilpalm_name",
      input_label = "New oilpalm name:",
      custom_input = selectInput(
        "input_oilpalm_def",
        "Default oilpalm paramaters:",
        oilpalm_species_col
      )
    )
  })
  
  user_oilpalm <- reactiveVal()
  
  observeEvent(input$confirm_add_oilpalm, {
    removeModal()
    cn <- input$input_oilpalm_name
    if (cn == "")
      return()
    oilpalm_species_df[[cn]] <<- oilpalm_species_df[[input$input_oilpalm_def]]
    user_oilpalm(c(user_oilpalm(), cn))
  })
  
  observeEvent(input$remove_oilpalm_button, {
    if (length(user_oilpalm()) == 0)
      return()
    show_input_dialog(
      "Remove oilpalm",
      "",
      "confirm_remove_oilpalm",
      custom_input = selectInput(
        "removed_oilpalm",
        "Select the oilpalm to removed:",
        user_oilpalm()
      )
    )
  })
  
  observeEvent(input$confirm_remove_oilpalm, {
    removeModal()
    rc <- input$removed_oilpalm
    if (rc == "")
      return()
    oilpalm_species_df[[rc]] <<- NULL
    uc <- user_oilpalm()
    uc <-  uc[uc != rc]
    user_oilpalm(uc)
  })
  
  observe({
    req(input$oilpalm_edit_text)
    values <- input$oilpalm_edit_text
    print(values)
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
  
  validate_oilpalm <- function(input_oilpalm) {
    ifelse(is.null(input_oilpalm),
           c(user_oilpalm(), oilpalm_species_col)[1],
           input_oilpalm)
  }
  
  apply_species_params <- function(params) {
    # croplist <- c(
    #   validate_crop(input$input_crop_1),
    #   validate_crop(input$input_crop_2),
    #   validate_crop(input$input_crop_3),
    #   validate_crop(input$input_crop_4),
    #   validate_crop(input$input_crop_5)
    # )
    # croplist_df <- crop_species_df[c("var", "sub_var", croplist)]
    # treelist <- c(
    #   validate_tree(input$input_tree_1),
    #   validate_tree(input$input_tree_2),
    #   validate_tree(input$input_tree_3)
    # )
    # treelist_df <- tree_species_df[c("var", "sub_var", treelist)]
    oilpalmlist <- c(
      validate_oilpalm(input$input_oilpalm_1),
      validate_oilpalm(input$input_oilpalm_2),
      validate_oilpalm(input$input_oilpalm_3)
    )
    oilpalmlist_df <- oilpalm_species_df[c("var", "sub_var", oilpalmlist)]
    
    croplist <- rv_arr[[crop_ui_id]]$CQ_Species
    croplist_df <- crop_species_df[c("var", "sub_var", croplist)]
    treelist <- rv_arr[[tree_ui_id]]$T_Species
    treelist_df <- tree_species_df[c("var", "sub_var", treelist)]
    
    # params$arrays$crop_df$vars$CQ_Species <- croplist
    # params$arrays$tree_df$vars$T_Species <- treelist
    params <- params |> apply_croplist_params(croplist_df) |> apply_treelist_params(treelist_df) |> apply_oilpalmlist_params(oilpalmlist_df)
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
      print("Starting simulation")
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
      task$invoke(n_iteration, pars, rv$output_vars, progress)
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
      run_wanulcas(n_iteration, pars, rv$output_vars, progress_trigger)
    })
  }
  
  is_simulation_ready <- function() {
    if (length(rv$output_vars) == 0) {
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
  
  check_dynamic_card_container <- function() {
    runjs(
      "Shiny.setInputValue('card_container_exists', document.getElementById('dynamic_card_container') !== null, {priority: 'event'});"
    )
  }
  
  observe({
    out <- rv$sim_output
    if (is.null(out))
      return()
    formatted_output_data(format_output_data(out))
    output_cfg <- isolate(rv$output_graph_cfg)
    if (length(output_cfg) > 0) {
      check_dynamic_card_container()
      # runjs(
      #   "Shiny.setInputValue('exists', document.getElementById('dynamic_card_container') !== null, {priority: 'event'});"
      # )
    }
  })
  
  # if not being checked and existed, the card can't be displayed automatically
  observeEvent(input$card_container_exists, {
    if (input$card_container_exists) {
      output_cfg <- isolate(rv$output_graph_cfg)
      if (length(output_cfg) > 0) {
        reset_output_config(output_cfg)
      }
    } else {
      shinyjs::delay(1000, check_dynamic_card_container())
      #                {
      #   runjs(
      #     "Shiny.setInputValue('exists', document.getElementById('dynamic_card_container') !== null, {priority: 'event'});"
      #   )
      # })
    }
  })
  
  output$sim_output_ui_old <- renderUI({
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
        xaxis = list(title = "Time"),
        yaxis = list(title = list(text = rowtitle, font = subfont)),
        hoverlabel = list(namelength = -1),
        
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
    
    vars_df <- data.frame(var = sort(isolate(rv$output_vars)))
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
    # selected <- which(output_vars_option_df$var %in% default_output_vars,
    #                   arr.ind = TRUE)
    selected <- which(output_vars_option_df$var %in% rv$output_vars, arr.ind = TRUE)
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
  
  # output$output_var_selected <- renderReactable({
  #   i <- getReactableState("output_var_selector", "selected")
  #   df <- output_vars_option_df[i, c("var", "arr")]
  #   rownames(df) <- NULL
  #   reactable(
  #     df,
  #     highlight = T,
  #     compact = T,
  #     striped = T,
  #     showPageSizeOptions = T,
  #     pageSizeOptions = c(10, 20, 40, 100),
  #     defaultPageSize = 20,
  #     paginateSubRows = T,
  #     rownames = T,
  #     columns = list(
  #       var = colDef(name = "Variable"),
  #       arr = colDef(name = "Array Dimension")
  #     )
  #   )
  # })
  
  output$output_var_selected <- renderUI({
    i <- getReactableState("output_var_selector", "selected")
    df <- output_vars_option_df[i, "var"]
    tags$ul(lapply(df, tags$li))
  })
  
  observe({
    i <- getReactableState("output_var_selector", "selected")
    if (is.null(i))
      return()
    rv$output_vars <- output_vars_option_df[i, "var"]
    output$selected_vars_info <- renderUI(
      #div(
      # "Selected log variabels:",
      tags$strong(
        length(rv$output_vars)
        # )
      )
    )
  })
  
  observeEvent(input$clear_selected_output_vars, rv$output_vars <- c())
  
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
  
  
  #### dynamic output graph  #########################
  
  # output_graph_cfg <- reactiveVal(list())
  
  card_id_counter <- 0
  get_next_card_id <- function() {
    card_id_counter <<- card_id_counter + 1
    return(paste0("outgraph", card_id_counter))
  }
  
  reset_output_config <- function(cfg = NULL) {
    card_id_counter <<- 0
    prev_cfg <- isolate(rv$output_graph_cfg)
    lapply(names(prev_cfg), function(id) {
      ns <- NS(id)
      # removeUI(selector = paste0("#", ns("card"), " *"), multiple = TRUE)
      removeUI(selector = paste0("#", ns("card")))
    })
    rv$output_graph_cfg <- list()
    if (!is.null(cfg)) {
      set_output_config(cfg)
    }
  }
  
  create_dynamic_card <- function(id,
                                  data,
                                  def_vars = NULL,
                                  def_filter = NULL,
                                  def_vars_ext = NULL) {
    cfg <- isolate(rv$output_graph_cfg)
    cfg[[id]] <- list()
    rv$output_graph_cfg <- cfg
    
    # print("create_dynamic_card")
    # print(id)
    # print(def_vars)
    # print(def_filter)
    
    var_df <- data$var_df
    arr_data <- data$arr_data
    
    # find other variables which has similar array dimension with the selected dimensions
    get_match_vars <- function(keys, arr) {
      dim_choices <- arr_data[[arr]]$keys
      ks <- unlist(lapply(names(dim_choices), function(k) {
        n <- sum(keys %in% dim_choices[[k]])
        if (n == 1) {
          NULL
        } else {
          k
        }
      }))
      arr_match <- unlist(sapply(arr_data, function(x)
        if (setequal(names(x$keys), ks))
          x$arr))
      
      if (length(arr_match) > 0 && arr_match == arr)
        return(NULL)
      if (is.null(ks))
        ks <- "single"
      list(vars = var_df[var_df$arr == arr_match, "vars"],
           dim = ks,
           arr = arr_match)
    }
    
    
    def_v_choices <- var_df$vars
    def_arr <- NULL
    def_dim_choices <- NULL
    def_v_ext_choices <- NULL
    if (!is.null(def_vars)) {
      def_arr <- var_df[var_df$vars == def_vars[1], "arr"]
      def_v_choices <- var_df[var_df$arr == def_arr, "vars"]
      def_dim_choices <- arr_data[[def_arr]]$keys
      v_ext <- get_match_vars(def_filter, def_arr)
      def_v_ext_choices <- v_ext$vars
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
    
    dim_label <- function(x)
      markdown(sprintf(
        "Comparable variables (array dimension: **%s**):",
        trimws(paste(x, collapse = ", "))
      ))
    
    observeEvent(input[[button_id]], {
      removeUI(selector = paste0("#", ns("card")))
      cfg <- isolate(rv$output_graph_cfg)
      cfg[[id]] <- NULL
      rv$output_graph_cfg <- cfg
    })
    
    
    
    render_dim <- function(v_ext) {
      if (is.null(v_ext) || length(v_ext$vars) == 0) {
        output[[dim_id]] <- NULL
      } else {
        output[[dim_id]] <- renderUI(dim_label(v_ext$dim))
      }
    }
    
    # variable selection
    observeEvent(input[[var_select_id]], {
      vs <- input[[var_select_id]]
      output[[card_title_id]] <- renderText(paste(vs, collapse = ", "))
      if (is.null(vs)) {
        updateSelectizeInput(session, var_select_id, choices = var_df$vars)
        updateSelectizeInput(session,
                             sp_select_id,
                             choices = character(0),
                             selected = character(0))
        updateSelectizeInput(session, var_ext_select_id, choices = character(0))
        output[[dim_id]] <- NULL
      } else if (length(vs) == 1) {
        arr <- var_df[var_df$vars == vs, "arr"]
        vfilt <- var_df[var_df$arr == arr, "vars"]
        dim_choices <- arr_data[[arr]]$keys
        
        sp <- input[[sp_select_id]]
        # if (is.null(sp) && !is.null(def_vars)) {
        #   sp <- def_filter
        # }
        # if(!is.null(def_filter)) {
        #   sp <- def_filter
        # }
        # print(dim_choices)
        v_ext <- NULL
        if (length(sp) >= 1) {
          v_ext <- get_match_vars(sp, arr)
          render_dim(v_ext)
        }
        updateSelectizeInput(session,
                             var_select_id,
                             choices = vfilt,
                             selected = vs)
        updateSelectizeInput(session,
                             sp_select_id,
                             choices = dim_choices,
                             selected = sp)
        updateSelectizeInput(session, var_ext_select_id, choices = v_ext$vars)
        def_vars <<- NULL
        def_filter <<- NULL
      }
    }, ignoreNULL = FALSE)
    
    
    # sub plot or array dimension selection
    observeEvent(input[[sp_select_id]], {
      sp <- input[[sp_select_id]]
      vs <- input[[var_select_id]]
      arr <- var_df[var_df$vars == vs[1], "arr"]
      vfilt <- var_df[var_df$arr == arr, "vars"]
      if (is.null(sp)) {
        output[[dim_id]] <- NULL
        updateSelectizeInput(session, var_ext_select_id, choices = character(0))
      } else if (length(sp) >= 1) {
        v_ext <- get_match_vars(sp, arr)
        render_dim(v_ext)
        updateSelectizeInput(session,
                             var_select_id,
                             choices = vfilt,
                             selected = vs)
        updateSelectizeInput(session,
                             var_ext_select_id,
                             choices = v_ext$vars,
                             selected = def_vars_ext)
        def_vars_ext <<- NULL
      }
    }, ignoreNULL = FALSE)
    
    observe({
      vs <- input[[var_select_id]]
      if (is.null(vs) || is.null(var_df)) {
        t <- "Please select the variables on left panel!"
        output[[graph_id]] <- renderPlotly(validate(need(F, t)))
        output[[table_id]] <- renderReactable(validate(need(F, t)))
        return()
      }
      v_df <- var_df[var_df$vars %in% vs, ]
      arr <- unique(v_df$arr)[1]
      df <- arr_data[[arr]]$data[c("time", names(arr_data[[arr]]$keys), vs)]
      sp <- input[[sp_select_id]]
      vs_ext <- input[[var_ext_select_id]]
      
      #save to config
      cfg <- isolate(rv$output_graph_cfg)
      cfg[[id]][["vars"]] <- vs
      cfg[[id]][["filter"]] <- sp
      cfg[[id]][["vars_ext"]] <- vs_ext
      rv$output_graph_cfg <- cfg
      
      # print(vs)
      # print(var_df)
      # print(arr)
      
      
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
      # is there any addition data from other array
      
      if (!is.null(vs_ext)) {
        vx_df <- var_df[var_df$vars %in% vs_ext, ]
        arr_ext <- unique(vx_df$arr)[1]
        df_ext <- arr_data[[arr_ext]]$data[vs_ext]
        df <- cbind(df, df_ext)
        vs <- c(vs, vs_ext)
      }
      
      
      
      output[[table_id]] <- renderReactable(
        reactable(
          df,
          highlight = T,
          compact = T,
          showPageSizeOptions = T,
          defaultColDef = colDef(
            cell = function(value) {
              # Only format if the value is not an integer
              if (is.numeric(value) && value %% 1 != 0) {
                if (abs(value) >= 10) {
                  format(round(value, 2), nsmall = 2)
                } else {
                  format(round(value, 4), nsmall = 4)
                }
              } else {
                value
              }
            }
          )
        )
      )
      
      output[[graph_id]] <- renderPlotly({
        # generate_output_graph_data(df, key_df, vs)
        generate_output_graph(df, key_df, vs)
      })
    })
    
    
    # graph setting ui
    div(
      id = ns("card"),
      navset_card_underline(
        full_screen = T,
        title = textOutput(card_title_id),
        
        sidebar = sidebar(
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
          ),
          div(
            style = "padding: 0; margin: 0;",
            uiOutput(dim_id),
            conditionalPanel(
              condition = "output['dim_compare'] != null",
              ns = ns,
              selectizeInput(
                inputId = var_ext_select_id,
                label = NULL,
                choices = def_v_ext_choices,
                selected = def_vars_ext,
                multiple = TRUE,
                options = list(dropdownParent = 'body')
              )
            )
          )
        ),
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
  
  
  generate_output_graph <- function(df, key_df, vars) {
    kn <- names(key_df)
    ncolplot <- length(unique(key_df[[1]]))
    nrowplot <- nrow(key_df) / ncolplot
    key_df$row <- 1:nrow(key_df)
    subfont = list(size = 14)
    figs <- apply(key_df, 1, function(k) {
      # get data with similar id for all selected keys
      if (kn[1] == "single") {
        df2 <- df
        coltitle <- ""
        rowtitle <- ""
      } else {
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
  
  generate_output_graph_data <- function(df, key_df, vars) {
    kn <- names(key_df)
    ncolplot <- length(unique(key_df[[1]]))
    nrowplot <- nrow(key_df) / ncolplot
    key_df$row <- 1:nrow(key_df)
    subfont = list(size = 14)
    d <- apply(key_df, 1, function(k) {
      # get data with similar id for all selected keys
      if (kn[1] == "single") {
        df2 <- df
        coltitle <- ""
        rowtitle <- ""
      } else {
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
        
      }
      print(rowtitle)
      print(coltitle)
      print(df2)
      
      
    })
    
    
  }
  
  output$sim_output_ui <- renderUI({
    card_body(
      fileInput(
        "upload_output_data_button",
        span(icon("upload"), "Upload output data file"),
        accept = c("application/zip", ".zip"),
        width = "300px"
      ),
      div(
        style = "width:100%",
        fileInput(
          "upload_output_cfg",
          "Upload Output Configuration",
          accept = c("application/yaml", ".yaml")
        ),
        downloadLink(style = "float:right; margin-right:50px", "download_output_cfg", "Save Output Configuration")
      ),
      layout_column_wrap(
        id = "dynamic_card_container",
        width = "400px",
        style = "margin:10px"
      ),
      div(
        style = "width:100%",
        actionButton(
          style = "float:right; margin-right:50px",
          "add_dynamic_card_button",
          "Add Output Graph",
          icon = icon("plus")
        )
      )
    )
  })
  
  add_dynamic_card <- function(def_vars = NULL,
                               def_filter = NULL,
                               def_vars_ext = NULL) {
    id <- get_next_card_id()
    insertUI(
      selector = "#dynamic_card_container",
      where = "beforeEnd",
      ui = create_dynamic_card(
        id,
        formatted_output_data(),
        def_vars,
        def_filter,
        def_vars_ext
      )
    )
    return(id)
  }
  
  
  
  observeEvent(input$add_dynamic_card_button, add_dynamic_card())
  
  
  output$download_output_cfg <- downloadHandler(
    filename = function() {
      paste("output_config.yaml")
    },
    content = function(fname) {
      write_yaml(rv$output_graph_cfg, fname)
    },
    contentType = "application/yaml"
  )
  
  set_output_config <- function(cfg) {
    cs <- lapply(cfg, function(x) {
      add_dynamic_card(x$vars, x$filter, x$vars_ext)
    })
  }
  
  observeEvent(input$upload_output_cfg, {
    dpath <- input$upload_output_cfg$datapath
    # set_output_config(read_yaml(dpath))
    reset_output_config(read_yaml(dpath))
  })
  
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
    
    crops <- rv_arr[[crop_ui_id]]$CQ_Species
    for (i in 1:5) {
      updateSelectInput(session, crop_select_ids[i], selected = crops[i])
    }
    
    trees <- rv_arr[[tree_ui_id]]$T_Species
    for (i in 1:5) {
      updateSelectInput(session, tree_select_ids[i], selected = trees[i])
    }
    
    if (!is.null(params$output)) {
      rv$output_vars <- params$output$time_vars
      rv$output_graph_cfg <- params$output$output_cfg
    }
    
    
  }
  
  # observe({
  #   crops <- rv_arr[["input_array_crop_117_0"]]$CQ_Species
  #   for (i in 1:5) {
  #     updateSelectInput(session, crop_select_ids[i], selected = crops[i])
  #   }
  # })
  #
  # observe({
  #   trees <- rv_arr[["input_array_tree_117_0"]]$T_Species
  #   for (i in 1:5) {
  #     updateSelectInput(session, tree_select_ids[i], selected = trees[i])
  #   }
  # })
  
  
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
      pars$output <- list(time_vars = rv$output_vars,
                          output_cfg = rv$output_graph_cfg)
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
