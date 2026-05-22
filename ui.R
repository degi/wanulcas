


### INPUT GUI ###############

crop_params_ui <- function() {
  card_body(
    class = "bordercard",
    height = "100%",
    div(
      "Select at most 5 types of crop you want to simulate from the crop library:"
    ),
    uiOutput("input_crop_select"),
    card(
      card_header(
        class = "d-flex justify-content-between",
        "Crop Library",
        flowLayout(
          cellArgs = list(style = "width:auto; margin:0px;"),
          actionButton(
            "add_crop_button",
            "Add New Crop Type",
            icon = icon("plus"),
            class = "compact_button"
          ),
          actionButton(
            "remove_crop_button",
            "Remove Crop",
            icon = icon("trash-can"),
            class = "compact_button"
          )
        )
      ),
      p("You may add and define new crop to the library"),
      reactableOutput("input_crop_lib")
    )
  )
}

tree_params_ui <- function() {
  card_body(
    class = "bordercard",
    height = "100%",
    div(
      "Select at most 3 types of tree you want to simulate from the tree library:"
    ),
    uiOutput("input_tree_select"),
    card(
      card_header(
        class = "d-flex justify-content-between",
        "Tree Library",
        flowLayout(
          cellArgs = list(style = "width:auto; margin:0px;"),
          actionButton(
            "add_tree_button",
            "Add New Tree Type",
            icon = icon("plus"),
            class = "compact_button"
          ),
          actionButton(
            "remove_tree_button",
            "Remove Tree",
            icon = icon("trash-can"),
            class = "compact_button"
          )
        )
      ),
      p("You may add and define new tree species to the library"),
      reactableOutput("input_tree_lib")
    )
  )
}

# oilpalm_params_ui <- function() {
#   card_body(
#     class = "bordercard",
#     height = "100%",
#     div(
#       "Select at most 3 types of oilpalm you want to simulate from the tree library:"
#     ),
#     uiOutput("input_oilpalm_select"),
#     card(
#       card_header(
#         class = "d-flex justify-content-between",
#         "Oilpalm Library",
#         flowLayout(
#           cellArgs = list(style = "width:auto; margin:0px;"),
#           actionButton(
#             "add_oilpalm_button",
#             "Add New oilpalm Type",
#             icon = icon("plus"),
#             class = "compact_button"
#           ),
#           actionButton(
#             "remove_oilpalm_button",
#             "Remove oilpalm",
#             icon = icon("trash-can"),
#             class = "compact_button"
#           )
#         )
#       ),
#       p("You may add and define new oilpalm species to the library"),
#       reactableOutput("input_oilpalm_lib")
#     )
#   )
# }

get_input_graph <- function(title, desc, v) {
  title_tt <- div(title, style = "width:180px;")
  if (desc != "") {
    title_tt <- title_tt |> bslib::tooltip(desc, options = list(customClass = "custom-tooltip"))
  }
  
  # Safe lapply to avoid zero-length errors
  graph_items <- lapply(graph_subvars[[v]], function(x) {
    table_edit_ui(x, is_upload_button = F, vspace = "0px")
  })
  
  div(
    class = "whitecard",
    navset_card_underline(
      id = paste("input_graph_card", v, sep = "-"),
      full_screen = TRUE,
      height = 300,
      
      title = title_tt,
      nav_panel("Plot", card_body(padding = 5, plotlyOutput(
        paste("input_graph_plot", v, sep = "-")
      ))),
      nav_panel("Data", do.call(
        layout_column_wrap, c(list(width = "200px", fill = FALSE), graph_items)
      ))
    )
  )
}


get_input_subcontent <- function(id, group_id) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id &
                              input_vars_conf_df$group_id == group_id, ]
  if (nrow(idf) == 0)
    return(NULL)
  idf <- idf[order(as.numeric(idf$order)), ]
  
  # variable input
  v_content <- NULL
  v <- idf[idf$type == "vars", "var"]
  if (length(v) > 0) {
    par_df <- inputvars_df[inputvars_df$var %in% v, ]
    par_df <- par_df[order(as.numeric(par_df$order)), ]
    if (nrow(par_df) > 0) {
      n_ui <- numeric_input_ui(par_df$ui_id[1], par_df, tooltip_class = "custom-tooltip")
      v_content <- n_ui
    }
  }
  
  # array input
  a_content <- NULL
  adf <- idf[idf$type == "arrays", ]
  if (nrow(adf) > 0) {
    a <- unique(adf$subtype)
    a_id <- paste("input_array", a, id, group_id, sep = "_")
    a_content <- lapply(a_id, function(x) {
      card(
        full_screen = TRUE,
        max_height = 300,
        card_body(
          padding = 10,
          table_edit_ui(x, is_upload_button = F, vspace = "4px")
        )
      )
    })
  }
  
  # graph input
  g_content <- NULL
  gdf <- idf[idf$type == "graphs", ]
  if (nrow(gdf) > 0) {
    g_content <- apply(gdf, 1, function(x) {
      g_content <- get_input_graph(x[["var_label"]], x[["var_desc"]], x[["var"]])
    })
    names(g_content) <- NULL
  }
  
  return(list(var = v_content, table = c(a_content, g_content)))
}



get_input_content <- function(id) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id, ]
  if (nrow(idf) == 0)
    return(NULL)
  # by group
  g_id <- sort(unique(idf$group_id))
  page_content <- lapply(g_id, function(x) {
    sc <- get_input_subcontent(id, x)
    
    var_args <- if (!is.null(sc$var))
      sc$var
    else
      list()
    table_args <- if (!is.null(sc$table))
      sc$table
    else
      list()
    
    content <- card_body(
      padding = 10,
      class = "bordercard",
      do.call(flowLayout, c(list(
        cellArgs = list(style = "width:auto; margin:0px;")
      ), var_args)),
      do.call(flowLayout, c(list(
        cellArgs = list(style = "width:auto; margin:0px;")
      ), table_args))
    )
    g_df <- input_group_df[input_group_df$group_id == x, ]
    if (nrow(g_df) > 0) {
      return (card(card_header(g_df$title), markdown(g_df$desc), content))
    }
    card(content)
  })
  
  if (length(page_content) == 1)
    return(page_content)
  
  card_body(
    class = "bordercard",
    height = "100%",
    do.call(flowLayout, c(list(
      cellArgs = list(style = "width:auto; margin:0px;")
    ), page_content))
  )
}

input_subtab <- function(st) {
  row.names(st) <- NULL
  apply(st, 1, function(x) {
    id <- as.numeric(x["id"])
    sst <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
    if (nrow(sst) > 0) {
      sst_ui <- input_subtab(sst)
      content <- get_input_content(id)
      if (!is.null(content)) {
        sst_ui <- c(list(nav_panel("Variables", content)), sst_ui)
      }
      # crop parameter tab
      if (id == 7) {
        return(nav_panel(
          x["title"],
          card_body(
            class = "subpanel",
            padding = 0,
            crop_params_ui()
          )
        ))
      }
      # tree parameter tab
      if (id == 8) {
        return(nav_panel(
          x["title"],
          card_body(
            class = "subpanel",
            padding = 0,
            tree_params_ui()
          )
        ))
      }
      # oilpalm parameter tab
      # if (id == 87) {
      #   return(nav_panel(
      #     x["title"],
      #     card_body(
      #       class = "subpanel",
      #       padding = 0,
      #       oilpalm_params_ui()
      #     )
      #   ))
      # }
      nav_panel(x["title"], card_body(
        class = "subpanel",
        padding = 0,
        do.call(navset_card_pill, sst_ui)
        # navset_card_pill(!!!sst_ui)
      ))
    } else {
      content <- get_input_content(id)
      desc <- card_body(padding = 10,
                        fillable = F,
                        fill = F,
                        x["desc"])
      nav_panel(x["title"], desc, content)
    }
  })
}

input_tab <- function() {
  tab_df <- input_gui_tabs_df[input_gui_tabs_df$parent_id == 0, ]
  row.names(tab_df) <- NULL
  apply(tab_df, 1, function(x) {
    id <- as.numeric(x["id"])
    st <- input_gui_tabs_df[input_gui_tabs_df$parent_id == id, ]
    if (nrow(st) > 0) {
      st_ui <- input_subtab(st)
      content <- get_input_content(id)
      if (!is.null(content)) {
        st_ui <- c(list(nav_panel("Variables", content)), st_ui)
      }
      nav_panel(x["title"], card_body(
        class = "subpanel",
        padding = 0,
        do.call(navset_card_underline, st_ui)
      ))
    } else {
      nav_panel(x["title"], x["desc"])
    }
  })
}






### UI ####################

ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      version = 5,
      primary = theme_color$primary,
      secondary = theme_color$secondary,
      dark = theme_color$dark,
      success = theme_color$success,
      info = theme_color$info,
      warning = theme_color$warning,
      danger = theme_color$danger,
      font_scale = 0.8,
      "navbar-light-color" = theme_color$light1,
      "navbar-light-active-color" = "white",
      "navbar-light-hover-color" = theme_color$secondary
    ),
    navbar_options = navbar_options(bg = theme_color$primary, theme = "light"),
    header =
      tags$head(
        tags$style(
          tags$link(rel = "shortcut icon", href = "favicon.ico"),
          HTML(
            "
            .custom-tooltip {
              --bs-tooltip-bg: #8B3E04;
              --bs-tooltip-border-radius: 8px;
              --bs-tooltip-opacity: 1;
              --bs-tooltip-max-width: 300px;
            }

            .card-header {
              background-color: #F5F0E0
            }

            .subpanel .card-header {
              background-color: white;
              border-width: 0px;
            }

            .subpanel .card {
              border-width: 0px;
            }

            .bordercard .card {
              border-width:1px;
            }

            .bordercard .card-header {
              border-width:1px;
              background-color: #F5F0E0;
            }

            .whitecard .card-header {
              border-width:0px;
              background-color: #FFFFFF;
            }

            .home {
              background-color: #FA842B;
              background-image: url('images/wanulcas_diagram.png');
              background-repeat: no-repeat;
              background-size: auto 100%;
              height:100%;
              padding:50px;
              color:#fff;
              text-shadow: 2px 2px 6px black;
              text-align: right;

            }

            .jexcel > tbody > tr > td.readonly {
                color:#cc3d00;
                font-weight: bold;
            }

            .reactable-text-input {
              max-width: 80px;
            }

            .compact_button {
              width:auto;
              height:36px;
              padding:5px 20px;
            }

            .selectize-dropdown {
                  z-index: 999999 !important;
                }
          "
          )
        ),
        # tags$script(src = "jexcel.js"),
        # tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        
        tags$script(src = "jspreadsheet.js"),
        tags$link(rel = "stylesheet", href = "jspreadsheet.css", type = "text/css"),
        # tags$link(rel = "stylesheet", href = "jspreadsheet.themes.css", type = "text/css"),
        
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", href = "table.css", type = "text/css")
        
      ),
    window_title = "WaNuLCAS 5.0",
    title =
      tags$b(
        tags$img(
          height = 22,
          src = "images/wanulcas_logo.svg",
          style = "margin-right:5px;"
        ),
        "WaNuLCAS",
        span("5.0", style = "color:#FA842B;")
      ),
    padding = 0,
    
    
    nav_panel(
      title = "",
      icon = icon("house"),
      reactable.extras::reactable_extras_dependency(),
      div(
        class = "home",
        
        p("WaNuLCAS", span("5.0", style = "color:#EADEBD;"), style = "font-size:5em;font-family:'Arial black';"),
        
        p(
          span("Wa", style = "color:#8ECAE6;font-family:'Arial black';", .noWS = c('before', "after")),
          "ter, ",
          span("Nu", style = "color:#E4A4A0;font-family:'Arial black';", .noWS = c('before', "after")),
          "trient and ",
          span("L", style = "color:#FFD15C;font-family:'Arial black';", .noWS = c('before', "after")),
          "ight ",
          span("C", style = "color:#FFD15C;font-family:'Arial black';", .noWS = c('before', "after")),
          "apture in ",
          span("A", style = "color:#ADC178;font-family:'Arial black';", .noWS = c('before', "after")),
          "groforestry ",
          span("S", style = "color:#ADC178;font-family:'Arial black';", .noWS = c('before', "after")),
          "ystem",
          style = "font-size:3em;width:50%;margin-left: auto;margin-right:0;"
        ),
        p(HTML("&copy; World Agroforestry (ICRAF) - 2026"), style = "position:fixed;right:50px;bottom:0px;")
        
      ),
      
    ),
    nav_panel(
      title = "Input Parameters",
      icon = icon("arrow-down"),
      do.call(navset_card_tab, c(list(id = "input_panel"), input_tab()))
    ),
    
    ### SIMULATION #############################
    
    nav_panel(
      title = "Simulation",
      icon = icon("gears"),
      card_body(
        padding = 0,
        div(
          style = "margin:16px 0 0 50px",
          flowLayout(
            cellArgs = list(style = "width:auto; margin:0px; height:30px;"),
            div("Simulation Time (days):", style = "padding:5px 0;font-weight:bold"),
            numericInput("n_iteration", NULL, value = 50, width = "150px"),
            input_task_button(
              "sim_run_button",
              "Run Simulation",
              icon = icon("play"),
              style = compact_button_style
            ),
            conditionalPanel(
              condition = "output.is_sim_output",
              actionButton(
                "reset_button",
                "Reset output log variables",
                icon = icon("arrows-rotate"),
                style = compact_button_style
              ),
              downloadButton("download_output", "Download output data", style = compact_button_style)
            )
          )
        ),
        card_body(
          class = "bordercard",
          padding = 0,
          height = "100%",
          fillable = F,
          conditionalPanel(condition = "output.is_sim_output", navset_card_tab(
            nav_panel(
              title = "Time series output",
              card_body(
                class = "whitecard",
                padding = 0,
                uiOutput("sim_output_ui")
              )
            ),
            nav_panel(
              title = "Final value output",
              card_body(
                class = "whitecard",
                padding = 0,
                uiOutput("sim_output_final_ui")
              )
            )
          )),
          conditionalPanel(
            condition = "!output.is_sim_output",
            card_body(
              padding = 10,
              height = "100%",
              div("Please select the output variables below"),
              layout_column_wrap(
                card(
                  card_header(
                    "Time series output variables",
                    div(
                      actionButton(
                        "clear_selected_output_vars",
                        "Clear selections",
                        icon = icon("square"),
                        class = "btn-sm"
                      ),
                      actionButton(
                        "reset_default_output_vars",
                        "Reset to default",
                        icon = icon("arrows-rotate"),
                        class = "btn-sm"
                      )
                    ),
                    class = "d-flex justify-content-between"
                  ),
                  layout_column_wrap(
                    style = css(grid_template_columns = "2fr 1fr"),
                    width = NULL,
                    reactableOutput("output_var_selector"),
                    card(
                      class = "whitecard",
                      card_header("Selected variabels:", uiOutput("selected_vars_info")),
                      uiOutput("output_var_selected")
                    )
                  ),
                ),
                card(
                  card_header(
                    "Final value output variables",
                    div(
                      actionButton(
                        "clear_selected_output_final_vars",
                        "Clear selections",
                        icon = icon("square"),
                        class = "btn-sm"
                      ),
                      actionButton(
                        "reset_default_output_final_vars",
                        "Reset to default",
                        icon = icon("arrows-rotate"),
                        class = "btn-sm"
                      )
                    ),
                    class = "d-flex justify-content-between"
                  ),
                  layout_column_wrap(
                    style = css(grid_template_columns = "2fr 1fr"),
                    width = NULL,
                    reactableOutput("output_final_var_selector"),
                    card(
                      class = "whitecard",
                      card_header("Selected variabels:", uiOutput("selected_final_vars_info")),
                      uiOutput("output_final_var_selected")
                    )
                  ),
                )
                
              )
            )
          )
        )
        
      )
    ),
    
    ### ABOUT ##########################
    
    nav_panel(
      title = "",
      icon = bs_icon("question-circle", size = "1.3em"),
      navset_card_tab(
        id = "info_panel",
        nav_panel(
          title = "About",
          icon = icon("circle-info"),
          card_body(includeMarkdown("docs/about.md"))
        ),
        nav_panel(
          title = "Background",
          icon = icon("book"),
          card_body(includeMarkdown("docs/background.md"))
        ),
        nav_panel(
          title = "Overview",
          icon = icon("book"),
          card_body(includeMarkdown("docs/overview.md"))
        ),
        nav_panel(
          title = "Tutorial",
          icon = icon("book"),
          card_body(includeMarkdown("docs/manual.md"))
        ),
        nav_panel(
          title = "Technical Notes",
          icon = icon("screwdriver-wrench"),
          card_body(includeMarkdown("docs/w_notes.md"))
        )
      )
    ),
    
    nav_spacer(),
    nav_menu(
      title = "Options",
      icon = icon("ellipsis-vertical"),
      align = "right",
      nav_item(
        style = "margin: 0 20px",
        fileInput(
          "upload_parameter",
          span(icon("upload"), "Upload input parameter file"),
          accept = c("application/yaml", ".yaml", ".yml"),
          width = "300px"
        )
      ),
      nav_item(style = "border-top: 2px dashed lightgray; margin:10px 20px"),
      nav_item(
        style = "margin: 0 20px",
        fileInput(
          "upload_xls_parameter",
          span(
            icon("file-excel"),
            "Import and apply MS-Excel parameter file from earlier version of WaNuLCAS"
          ),
          accept = c("application/vnd.ms-excel", ".xlsx", ".xls", ".xlsm"),
          width = "300px"
        )
      ),
      nav_item(style = "border-top: 2px dashed lightgray; margin:10px 20px"),
      nav_item(span(
        icon("download"),
        downloadLink("download_parameter", "Download and save parameters"),
        style = "margin:0 20px"
      ))
    )
  )
