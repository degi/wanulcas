








### INPUT GUI ###############

get_input_graph <- function(title, desc, v) {
  title_tt <- div(title, style = "width:180px;")
  if(desc != "") {
    title_tt <- title_tt |> tooltip(desc, options = list(customClass = "custom-tooltip"))
  }
  card(
    id = paste("input_graph_card", v, sep = "-"),
    full_screen = TRUE,
    height = 300,
    
    card_body(
      padding = 0,
      navset_card_underline(
        title = title_tt,
        nav_panel("Plot", card_body(padding = 5, plotlyOutput(
          paste("input_graph_plot", v, sep = "-")
        ))),
        nav_panel(
          "Data",
          layout_column_wrap(
            width = "200px",
            fill = F,
            !!!lapply(graph_subvars[[v]], function(x) {
              table_edit_ui(x, is_upload_button = F, vspace = "0px")
            })
          )
        )
      )
    )
  )
}


get_input_subcontent <- function(id, group_id) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id &
                              input_vars_conf_df$group_id == group_id, ]
  if (nrow(idf) == 0)
    return(NULL)
  
  # variable input
  v_content <- NULL
  v <- idf[idf$type == "vars", "var"]
  if (length(v) > 0) {
    par_df <- inputvars_df[inputvars_df$var %in% v, ]
    if (nrow(par_df) > 0) {
      # n_ui <- numeric_input_ui(paste("input_var", id, group_id, sep = "_"), par_df, tooltip_class = "custom-tooltip")
      n_ui <- numeric_input_ui(par_df$ui_id[1], par_df, tooltip_class = "custom-tooltip")
      v_content <- list(layout_column_wrap(
        width = "200px",
        fill = F,
        heights_equal = "row",
        !!!n_ui
      ))
    }
  }
  
  # array input
  a_content <- NULL
  adf <- idf[idf$type == "arrays", ]
  if (nrow(adf) > 0) {
    a <- unique(adf$subtype)
    a_id <- paste("input_array", a, id, group_id, sep = "_")
    a_content <- lapply(a_id, function(x) {
      table_edit_ui(x, is_upload_button = F, vspace = "0px")
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
  
  return(c(v_content, a_content, g_content))
}



get_input_content <- function(id) {
  idf <- input_vars_conf_df[input_vars_conf_df$id == id, ]
  if (nrow(idf) == 0)
    return(NULL)
  # by group
  g_id <- sort(unique(idf$group_id))
  page_content <- lapply(g_id, function(x) {
    content <- card_body(
      padding = 10,
      layout_column_wrap(
        width = "280px",
        fill = F,
        gap = 10,
        heights_equal = "row",
        !!!get_input_subcontent(id, x)
      )
    )
    g_df <- input_group_df[input_group_df$group_id == x, ]
    if (nrow(g_df) > 0) {
      return (card(card_header(g_df$title), markdown(g_df$desc), content))
    }
    card(content)
  })
  card_body(
    class = "bordercard",
    height = "100%",
    layout_column_wrap(
      width = "600px",
      fill = F,
      heights_equal = "row",
      !!!page_content
    )
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
      nav_panel(x["title"],
                card_body(
                  class = "subpanel",
                  padding = 0,
                  navset_card_pill(!!!sst_ui)
                ))
    } else {
      content <- get_input_content(id)
      desc <- card_body(
        padding = 10,
        fillable = F,
        fill = F,
        x["desc"]
        # div(paste("id:", id))
      )
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
      nav_panel(x["title"],
                card_body(
                  class = "subpanel",
                  padding = 0,
                  navset_card_underline(!!!st_ui)
                ))
    } else {
      nav_panel(x["title"], x["desc"])
    }
  })
}


### TOOLS ################

menu_button <- function(id,
                        label,
                        icon = NULL,
                        desc = NULL,
                        placement = "auto") {
  d <- actionButton(id, label, icon = icon, class = "menu_button")
  if (is.null(desc)) {
    return(d)
  } else {
    return(tooltip_blue(
      d,
      desc,
      placement = placement,
      id = paste0("tooltip_", id)
    ))
  }
}

download_link <- function(id, filename = NULL) {
  if (is.null(filename))
    filename <- paste0(id, ".csv")
  div(style = "margin-left:auto; margin-right:0;", table_download_link(id, filename = filename))
}


### MAIN GUI ####################

ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      primary = theme_color$primary,
      secondary = theme_color$secondary,
      dark = theme_color$dark,
      success = theme_color$success,
      info = theme_color$info,
      warning = theme_color$warning,
      danger = theme_color$danger,
      font_scale = 0.9
    ),
    navbar_options = navbar_options(bg = theme_color$primary),
    header =
      tags$head(
        tags$style(
          tags$link(rel = "shortcut icon", href = "favicon.ico"),
          HTML(
            "
            .menu_button {
              height: 26px;
              padding: 2px 10px;
              margin: 2px;
              border-width: 0px;
            }

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
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
        
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
      title = "Input",
      icon = icon("arrow-down"),
      navset_card_tab(
        title = div("Input Parameters", style = "color:#cc3d00;font-size:1.2em; padding:5px 0 0;font-family:'Arial black';"),
        id = "input_panel",
        !!!input_tab()
      )
    ),
    
    
    
    ### SIMULATION #############################
    
    nav_panel(title = "Simulation", icon = icon("gears")),
    
    
    
    ### ABOUT ##########################
    
    nav_panel(
      title = "",
      icon = bs_icon("question-circle", size = "1.3em"),
      navset_card_tab(
        id = "info_panel",
        nav_panel(
          title = "About",
          icon = icon("circle-info")
          # card_body(includeMarkdown("docs/about.md"))
        ),
        nav_panel(
          title = "Tutorial",
          icon = icon("book")
          # card_body(includeMarkdown("docs/manual.md"))
        ),
        nav_panel(
          title = "References",
          icon = icon("bookmark")
          # card_body(includeMarkdown("docs/references.md"))
        ),
        nav_panel(
          title = "Software Library",
          icon = icon("screwdriver-wrench")
          # card_body(includeMarkdown("docs/library.md"))
        )
      )
    )
    
    
  )
