

chart_color <- c(
  paletteer_d("ggthemes::calc"),
  paletteer_d("ggsci::schwifty_rickandmorty"),
  paletteer_d("ggsci::default_uchicago"),
  paletteer_d("ggthemes::Classic_10"),
  paletteer_d("ggsci::default_jco"),
  paletteer_d("ggsci::springfield_simpsons"),
  paletteer_d("ggsci::light_uchicago"),
  paletteer_d("ggthemes::stata_s2color"),
  paletteer_d("RColorBrewer::Set1"),
  paletteer_d("RColorBrewer::Set2"),
  paletteer_d("RColorBrewer::Set3"),
  paletteer_dynamic("cartography::multi.pal", 20)
)

light_color <- c(
  paletteer_d("ggsci::legacy_tron"),
  paletteer_d("ggthemes::Superfishel_Stone"),
  paletteer_d("ggthemes::Classic_10_Light"),
  paletteer_d("ggpomological::pomological_palette"),
  paletteer_d("ggthemes::Tableau_10"),
  paletteer_d("ggthemes::Classic_10_Medium"),
  paletteer_dynamic("cartography::pastel.pal", 20)
)

get_color <- function(idx = NULL, is_light = F) {
  if (is_light) {
    cl <- light_color
  } else {
    cl <- chart_color
  }
  max_idx <- length(cl)
  if (is.null(idx))
    idx <- sample.int(max_idx, 1)
  idx <- abs(idx)
  idx <- idx %% max_idx
  idx[idx == 0] <- max_idx
  return(substring(cl[idx], 1, 7))
  # return(cl[idx])
}

y_axis_setting <- function(p, i, title = "") {
  l <- list(p = p)
  if (i == 1) {
    l[["yaxis"]] <-
      list(color = chart_color[i],
           zeroline = F,
           title = title)
  } else {
    l[[paste0("yaxis", i)]] <-
      list(
        overlaying = "y",
        side = "left",
        anchor = "free",
        position = (i - 1) * 0.1,
        color = chart_color[i],
        zeroline = F,
        showgrid = F,
        title = title
      )
  }
  return(l)
}

plotly_multiple_axis <- function(df) {
  v <- names(df)
  fig <- plot_ly()
  for (i in 2:length(v)) {
    fig <- fig |> add_trace(
      x = df[[1]],
      y = df[[i]],
      name = v[i],
      mode = "lines",
      type = "scatter",
      line = list(color = chart_color[i - 1]),
      yaxis = paste0("y", i - 1)
    )
    fig <- do.call(layout, y_axis_setting(fig, i - 1, v[i]))
  }
  fig <- fig |> layout(hovermode = 'x',
                       xaxis = list(
                         title = v[1],
                         tickfont = list(family = "Arial black")
                       ))
  return(fig)
}


# REFFERENCES
# https://shiny.posit.co/r/articles/improve/modules/
#

#' Editable table UI using excelR library
#'
#' @param id
#' @param title
#' @param is_paginated
#'
#' @return
#' @export
#' @import excelR
#'
#' @examples
table_edit_ui <- function(id,
                          title = "",
                          is_paginated = F,
                          is_upload_button = T,
                          is_label = F, vspace = NULL) {
  ns <- NS(id)
  if(is.null(vspace)) {
    vspace <- ifelse(is_paginated, "90px", "40px")
  }
  label <- ifelse(is_label, "Upload", "")
  upload_button <- ifelse(is_upload_button, tagList(
                          
                          div(
                            style = "float:right;",
                            actionButton(
                              ns("upload_btn"),
                              label,
                              icon = icon("upload"),
                              class = "menu_button"
                            )
                            |> tooltip(
                              "Upload data",
                              id = ns("upload_tt"),
                              options = list(customClass = "custom-tooltip")
                            )
                          ) |>
                            popover(
                              id = ns("upload_pop"),
                              fileInput(ns("upload_file"), "Upload data", accept = c(".csv"))
                            ))
                          , 
                          tagList())
  div(style = "height:100%;overflow:auto;",
      span(upload_button,title),
      excelR::excelOutput(ns("table_edit"), height = paste0("calc(100% - ", vspace, ")")))
}

table_edit_server <- function(id,
                              reactive_data,
                              col_title = NULL,
                              col_type = NULL,
                              col_disable = NULL,
                              col_source = NULL,
                              col_width = NULL,
                              allowRowModif = F,
                              nrow = 0,
                              pagination = NULL,
                              csvFileName = "table_data", ...) {
  moduleServer(id, function(input, output, session) {
    output$table_edit <- excelR::renderExcel({
      data <- reactive_data()
      if (is.null(data)) {
        return()
      }
      col_names <<- colnames(data)
      
      if (is.null(col_title)) {
        col_title <- gsub("_", " ", col_names)
        col_title <- tools::toTitleCase(col_title)
      }
      
      col_render <- rep(NA, ncol(data))
      if (is.null(col_width))
        col_width <- col_render
      
      col_align <- rep("left", ncol(data))
      
      if (is.null(col_type)) {
        col_type_data <<- sapply(data, class)
      } else {
        col_type_data <<- col_type
      }
      
      col_type <- rep(NA, ncol(data))
      idx <- which(tolower(col_type_data) == "date")
      for (i in idx) {
        col_type[i] <- "calendar"
      }
      idx <- which(col_type_data == "logical")
      for (i in idx) {
        col_type[i] <- "checkbox"
      }
      idx <- which(col_names == "color")
      for (i in idx) {
        col_type[i] <- "color"
        # col_title[i] <- " "
        col_render[i] <- "square"
        col_align[i] <- "center"
        col_width[i] <- 30
      }
      idx <- which(col_type_data == "numeric")
      for (i in idx) {
        col_align[i] <- "right"
      }
      idx <- which(col_type_data == "dropdown")
      for (i in idx) {
        col_type[i] <- "dropdown"
      }
      
      if (is.null(col_disable)) {
        col_disable <- rep(F, ncol(data))
      }
      
      data_column <- data.frame(
        # title = col_title,
        type = col_type,
        render = col_render,
        align = col_align,
        width = col_width,
        readOnly = col_disable
      )
      
      if (!is.null(col_source)) {
        data_column$source <- col_source
      }
      
      excelR::excelTable(
        data = data,
        columns = data_column,
        colHeaders = col_title,
        tableOverflow = T,
        tableWidth = "100%",
        tableHeight = "100%",
        allowDeleteColumn = F,
        allowRenameColumn = F,
        allowInsertColumn = F,
        allowDeleteRow = allowRowModif,
        allowInsertRow = allowRowModif,
        rowDrag = allowRowModif,
        minDimensions = c(NA, nrow),
        pagination = pagination,
        autoIncrement = F,
        dateFormat = "DD-Mon-YYYY",
        csvFileName = csvFileName,
        defaultColWidth = 100,
        includeHeadersOnDownload = T,
        ...
      )
    })
    
    col_names <- NULL
    col_type_data <- NULL
    table_data <- reactiveVal()
    # table_data_edit <- reactiveVal(data)
    
    observeEvent(input$table_edit, {
      inp <- input$table_edit
      df_input <- excel_to_R(inp)
      names(df_input) <- col_names
      df_input[df_input == ""] <- NA
      df_input <- df_input[rowSums(!is.na(df_input)) > 0, ]
      idx <- which(col_type_data == "numeric")
      for (i in idx) {
        df_input[[i]] <- as.numeric(df_input[[i]])
      }
      table_data(df_input)
    })
    
    date_formats <- c("%d-%b-%Y", "%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")
    # date_format <- "%d-%b-%Y"
    
    observeEvent(input$upload_file, {
      fpath <- input$upload_file$datapath
      df <- read.csv(fpath)
      toggle_popover("upload_pop", show = F)
      if(is.null(col_type_data)) {
        table_data(df)
        return()
      }
      if (ncol(df) > length(col_type_data))
        df <- df[c(1:length(col_type_data))]
      for (i in 1:length(col_type_data)) {
        if (i <= ncol(df)) {
          if (tolower(col_type_data[i]) == "date") {
            df[[i]] <- as.Date(df[[i]], tryFormats = date_formats)
          } else if (col_type_data[i] == "numeric") {
            df[[i]] <- as.numeric(df[[i]])
          } else if (col_type_data[i] == "color") {
            if (!is_color_code(df[[i]])) {
              df[[i]] <- "#000"
            }
          }
        } else {
          if (col_type_data[i] == "numeric") {
            df[, col_names[i]] <- NA
          } else {
            df[, col_names[i]] <- ""
          }
        }
      }
      colnames(df) <- col_names
      # table_data_edit(df)
      table_data(df)
    })
    
    observeEvent(input$upload_btn, {
      toggle_tooltip("upload_tt", show = F)
    })
    
    return(table_data)
  })
}

is_color_code <- function(x) {
  res <- try(col2rgb(x), silent = TRUE)
  return(!"try-error" %in% class(res))
}


reactable_edit_ui <- function(id, height = NULL) {
  ns <- NS(id)
  reactableOutput(ns("table_id"), height = height) |> popover(
    id = ns("pop_id"),
    title = span(icon("edit"), "Edit"),
    textOutput(ns("pop_desc_id")),
    textInput(ns("input_id"), NULL),
    actionButton(ns("confirm_id"), "Confirm"),
    options = list(trigger = "manual")
  )
  
}

reactable_edit_server <- function(id,
                                  reactive_df,
                                  editable = NULL,
                                  col_type = NULL,
                                  columns = NULL,
                                  ...) {
  moduleServer(id, function(input, output, session) {
    output$table_id <- renderReactable({
      df <- reactive_df()
      if (is.null(df) || nrow(df) == 0)
        return()
      
      click_edit <- NULL
      if (!is.null(editable)) {
        cn <- colnames(df)
        ed <- paste(paste0("'", cn[editable], "'"), collapse = ",")
        click_edit <- JS(
          sprintf(
            "function(rowInfo, column) {
              const ed = [%s];
              if(!ed.includes(column.id)) {return}
              if (window.Shiny) {
                Shiny.setInputValue('%s',{row: rowInfo.index + 1, column: column.id}, {priority: 'event'})
              }
            }",
            ed,
            session$ns("table_edit")
          )
        )
      }
      
      if (is.null(col_type)) {
        col_type_data <<- sapply(df, class)
      } else {
        col_type_data <<- col_type
      }
      # print(col_type_data)
      reactable(
        df,
        columns = columns,
        ...,
        defaultColDef = colDef(style = list(cursor = "default")),
        onClick = click_edit
      )
    })
    
    col_type_data <- NULL
    edited_data <- reactiveVal()
    edited_cell <- reactiveVal()
    
    observeEvent(input$table_edit, {
      ed <- input$table_edit
      edited_cell(ed)
      df <- reactive_df()
      val <- df[ed$row, ed$column]
      updateTextInput(session, "input_id", value = val)
      name <- columns[[ed$column]]$name
      if (is.null(name))
        name <- ed$column
      update_popover("pop_id", title = paste("Edit", name))
      output$pop_desc_id <- renderText(paste("Row:", ed$row))
      toggle_popover("pop_id", show = T)
      
    })
    
    observeEvent(input$confirm_id, {
      ed <- edited_cell()
      df <- reactive_df()
      idx <- which(colnames(df) == ed$column)
      if (col_type_data[idx] == "numeric") {
        df[ed$row, ed$column] <- as.numeric(input$input_id)
      } else {
        df[ed$row, ed$column] <- input$input_id
      }
      edited_data(df)
      toggle_popover("pop_id", F)
    })
    
    return(edited_data)
  })
}


numeric_input_ui <- function(id, df, tooltip_class = NULL, ...) {
  # print(numeric_input_ui)
  # print(id)
  # print(df)
  # print(tooltip_class)
  ns <- NS(id)
  n_ui <- apply(df, 1, function(x) {
    if(!is.null(x[["info"]]) && x[["info"]] != "") {
      numericInput(
        ns(x[["var"]]),
        markdown(x[["label"]]),
        as.numeric(x[["value"]]),
        as.numeric(x[["min"]]),
        as.numeric(x[["max"]]),
        as.numeric(x[["step"]]), 
        ...
      ) |> bslib::tooltip(x[["info"]], options = list(customClass = tooltip_class))  
    } else {
    numericInput(
      ns(x[["var"]]),
      markdown(x[["label"]]),
      as.numeric(x[["value"]]),
      as.numeric(x[["min"]]),
      as.numeric(x[["max"]]),
      as.numeric(x[["step"]]), 
      ...
    )
    }
  })
  names(n_ui) <- NULL
  return(n_ui)
}

numeric_input_server <- function(id, var_ids) {
  moduleServer(id, function(input, output, session) {
    var_list <- reactiveVal(list())
    lapply(var_ids, function(x) {
      observeEvent(input[[x]], {
        vl <- isolate(var_list())
        vl[[x]] <- input[[x]]
        var_list(vl)
      })
    })
    return(var_list)
  })
}

update_numeric_input_ui <- function(id, var_list) {
  if (is.null(var_list))
    return()
  ns <- NS(id)
  for (var in names(var_list)) {
    updateNumericInput(inputId = ns(var), value = var_list[[var]])
  }
}

table_download_link <- function(id,
                                filename = "data.csv",
                                label = "Download as CSV") {
  actionLink(
    "download_csv",
    label,
    icon("download"),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

info <- function(i, class = "x", ...) {
  tooltip(icon("info-circle", style = "margin-left:10px;"), i, 
          options = list(customClass = paste("custom-tooltip", class)),...)
}


input_dialog <- function(title = "",
                         desc = "",
                         confirm_id,
                         confirm_label = "Confirm",
                         input_var = NULL,
                         input_label = NULL,
                         input_def = NULL,
                         input_pholder = NULL,
                         input_type = NULL,
                         input_info = NULL,
                         custom_input = NULL) {
  inp <- NULL
  if (!is.null(input_var)) {
    blank <- rep("", length(input_var))
    if (is.null(input_label))
      input_label <- blank
    if (is.null(input_def))
      input_def <- blank
    if (is.null(input_pholder))
      input_pholder <- blank
    if (is.null(input_type))
      input_type <- blank
    if (is.null(input_info))
      input_info <- blank
    inp <- mapply(
      function(v, l, d, p, t, i) {
        if (i != "") {
          # label = span(HTML(l), tooltip(icon("info-circle", style = "margin-left:10px;"), i))
          label = span(HTML(l), info(i))
        } else {
          label = HTML(l)
        }
        if (t == "numeric") {
          paste(numericInput(v, label, d, width = "100%"))
        } else if (t == "boolean") {
          paste(checkboxInput(v, label, d, width = "100%"))
        } else {
          paste(textInput(v, label, d, width = "100%", p))
        }
      },
      input_var,
      input_label,
      input_def,
      input_pholder,
      input_type,
      input_info
    )
  }
  names(inp) <- NULL
  inp <- HTML(inp)
  modalDialog(
    title = title,
    HTML(paste("<p>", desc, "</p>")),
    custom_input,
    inp,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(confirm_id, confirm_label)
    )
  )
}

show_input_dialog <- function(...) {
  showModal(input_dialog(...))
}

show_alert <- function(title, desc) {
  showModal( 
    modalDialog( 
      desc, 
      title = title, 
      easy_close = TRUE
    ) 
  )
}