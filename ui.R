

get_input_content <- function(id, place) {
  idf <- inputvars_pos_df[inputvars_pos_df$place == place & inputvars_pos_df$id == id, ]
  content <- NULL
  if(nrow(idf) > 0) {
    v <- idf[idf$type == "vars", "var"]
    if(length(v) > 0) {
      par_df <- inputvars_df[inputvars_df$var %in% v,]
      if(nrow(par_df) > 0){
        content <- numeric_input_ui(paste("input", place,id, sep = "_"), par_df)
      }      
    }
    
    
    adf <- idf[idf$type == "arrays", ]
    if(nrow(adf) > 0) {
      aa <- unique(adf$stype)
      for(a in aa) {
        avar <- adf[adf$stype == a, "var"]
        df <- input_array[[a]][avar]
        
      }
    }
  }
  return(content)
}


input_subsubtab <- function(sst) {
  row.names(sst) <- NULL
  apply(sst, 1, function(x) {
    content <- get_input_content(x["subsubtab_id"], "subsubtab")
    if(is.null(content)) {  
      nav_panel(x["title"], x["desc"], h3(paste("subsubtab_id:", x["subsubtab_id"])))
    } else {
      nav_panel(x["title"], x["desc"], p(paste("subsubtab_id:", x["subsubtab_id"])), content)
    }
  })
}

input_subtab <- function(st) {
  row.names(st) <- NULL
  apply(st, 1, function(x) {
    sst <- subsubtab_df[subsubtab_df$subtab_id == x["subtab_id"], ]
    if (nrow(sst) > 0) {
      nav_panel(x["title"],
                card_body(
                  class = "subpanel",
                  padding = 0,
                  navset_card_pill(!!!input_subsubtab(sst))
                ))
    } else {
      content <- get_input_content(x["subtab_id"], "subtab")
      if(is.null(content)) {  
        nav_panel(x["title"], x["desc"], h3(paste("subtab_id:", x["subtab_id"])))
      } else {
        nav_panel(x["title"], x["desc"], p(paste("subtab_id:", x["subtab_id"])), content)
      }
    }
  })
}

input_tab <- function() {
  apply(tab_df, 1, function(x) {
    st <- subtab_df[subtab_df$tab_id == x["tab_id"], ]
    if (nrow(st) > 0) {
      nav_panel(x["title"],
                card_body(
                  class = "subpanel",
                  padding = 0,
                  navset_card_underline(!!!input_subtab(st))
                ))
    } else {
      nav_panel(x["title"], x["desc"], h3(paste("tab_id:", x["tab_id"])))
    }
  })
}


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
    bg = theme_color$primary,
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
              --bs-tooltip-bg: #023047D9;
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


          "
          )
        ),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
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
        p(HTML("&copy; World Agroforestry (ICRAF) - 2025"), style = "position:fixed;right:50px;bottom:0px;")
        
      ),
      
    ),
    nav_panel(
      #### INPUT OPTIONS ####
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
