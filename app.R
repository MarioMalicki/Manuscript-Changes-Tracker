library(shiny)
library(DT)
library(meta)

# ============================================================
# Configuration
# ============================================================

github_base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "MarioMalicki/Manuscript-Changes-Tracker/main/"
)

summary_filename <- "Mainpage.csv"
included_studies_filename <- "Includedstudies.csv"

# Mainpage.csv:
# grouping,parent,percentage,lowerci,upperci,order,dataset,notes
#
# Main-category rows:
# - parent is blank
# - grouping is the page name
# - dataset specifies the raw CSV filename
# - notes is optional
#
# Subgroup rows:
# - parent matches a main-category grouping
# - grouping matches the raw data's grouping value
# - dataset can be blank; the parent's dataset is used
# - blank notes inherit the parent's notes
#
# Includedstudies.csv:
# studies,loweryear,upperyear,day,month,year

transformation_choices <- stats::setNames(
  c("PLOGIT", "PAS", "PFT", "PLN", "PRAW"),
  c(
    "PLOGIT - Pooled Logit",
    "PAS - Pooled Arcsine Square Root Transformed Proportions",
    "PFT - Pooled Freeman-Tukey Transformed Proportions",
    "PLN - Pooled Log-Normal",
    "PRAW - Pooled Raw Proportions"
  )
)

analysis_note <- paste(
  "Note: Click Conduct Analysis on the left to analyse all rows",
  "in the displayed table.",
  "To analyse only specific rows, select those rows first,",
  "then click Conduct Analysis.",
  "Results are not calculated until you click the button."
)

# ============================================================
# Helper functions
# ============================================================

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

parse_numeric_column <- function(x, column_name) {
  text <- clean_text(x)
  values <- suppressWarnings(as.numeric(text))

  invalid <- !is.na(text) & (
    is.na(values) | !is.finite(values)
  )

  if (any(invalid)) {
    stop(
      paste0(
        "Column '",
        column_name,
        "' contains values that are not valid numbers."
      ),
      call. = FALSE
    )
  }

  values
}

format_number <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

read_github_csv <- function(filename) {
  read.csv(
    paste0(github_base_url, filename),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

# ------------------------------------------------------------
# Read main-page results and analysis-page configuration.
# No meta-analyses are conducted here.
# ------------------------------------------------------------

read_mainpage <- function() {
  df <- read_github_csv(summary_filename)

  required_columns <- c(
    "grouping",
    "parent",
    "percentage",
    "lowerci",
    "upperci",
    "order",
    "dataset",
    "notes"
  )

  missing_columns <- setdiff(required_columns, names(df))

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Mainpage.csv is missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(df) == 0) {
    stop("Mainpage.csv contains no rows.", call. = FALSE)
  }

  for (column_name in c("grouping", "parent", "dataset", "notes")) {
    df[[column_name]] <- clean_text(df[[column_name]])
  }

  if (anyNA(df$grouping)) {
    stop(
      "Every row in Mainpage.csv must have a grouping value.",
      call. = FALSE
    )
  }

  for (column_name in c("percentage", "lowerci", "upperci", "order")) {
    df[[column_name]] <- parse_numeric_column(
      df[[column_name]],
      column_name
    )
  }

  if (anyNA(df$order)) {
    stop(
      "Every row in Mainpage.csv must have an order value.",
      call. = FALSE
    )
  }

  if (anyDuplicated(df$order)) {
    stop(
      "The order values in Mainpage.csv must be unique.",
      call. = FALSE
    )
  }

  df <- df[order(df$order), , drop = FALSE]
  rownames(df) <- NULL

  # Main-category rows define the analysis pages.
  pages <- df[is.na(df$parent), , drop = FALSE]

  if (nrow(pages) == 0) {
    stop(
      "Mainpage.csv must contain at least one main-category row.",
      call. = FALSE
    )
  }

  if (anyDuplicated(pages$grouping)) {
    stop(
      "Main-category names in Mainpage.csv must be unique.",
      call. = FALSE
    )
  }

  if (anyNA(pages$dataset)) {
    stop(
      "Each main-category row in Mainpage.csv must specify a dataset.",
      call. = FALSE
    )
  }

  missing_parents <- setdiff(
    unique(df$parent[!is.na(df$parent)]),
    pages$grouping
  )

  if (length(missing_parents) > 0) {
    stop(
      paste(
        "Mainpage.csv is missing main-category rows for:",
        paste(missing_parents, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df$analysis_page <- ifelse(
    is.na(df$parent),
    df$grouping,
    df$parent
  )

  grouping_keys <- paste(
    df$analysis_page,
    df$grouping,
    sep = "\r"
  )

  if (anyDuplicated(grouping_keys)) {
    stop(
      paste(
        "Mainpage.csv contains duplicate grouping names",
        "within an analysis page."
      ),
      call. = FALSE
    )
  }

  pages$page_id <- paste0("analysis_page_", seq_len(nrow(pages)))
  rownames(pages) <- NULL

  df$page_index <- match(
    df$analysis_page,
    pages$grouping
  )

  list(
    summary = df,
    pages = pages
  )
}

# ------------------------------------------------------------
# Read study count, publication years, and update date.
# ------------------------------------------------------------

read_included_studies <- function() {
  df <- read_github_csv(included_studies_filename)

  required_columns <- c(
    "studies", "loweryear", "upperyear",
    "day", "month", "year"
  )

  missing_columns <- setdiff(required_columns, names(df))

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Includedstudies.csv is missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(df) != 1L) {
    stop(
      "Includedstudies.csv must contain exactly one data row.",
      call. = FALSE
    )
  }

  for (column_name in required_columns) {
    df[[column_name]] <- parse_numeric_column(
      df[[column_name]],
      column_name
    )
  }

  values <- unlist(
    df[1, required_columns],
    use.names = FALSE
  )

  if (anyNA(values) || any(values != floor(values))) {
    stop(
      paste(
        "All required values in Includedstudies.csv",
        "must be non-missing whole numbers."
      ),
      call. = FALSE
    )
  }

  if (
    df$studies < 0 ||
    df$loweryear < 1 ||
    df$upperyear < df$loweryear
  ) {
    stop(
      "Includedstudies.csv contains an invalid study count or year range.",
      call. = FALSE
    )
  }

  if (
    df$year < 1 ||
    df$year > 9999 ||
    df$month < 1 ||
    df$month > 12 ||
    df$day < 1 ||
    df$day > 31
  ) {
    stop(
      "Includedstudies.csv contains an invalid update date.",
      call. = FALSE
    )
  }

  date_text <- sprintf(
    "%04d-%02d-%02d",
    as.integer(df$year),
    as.integer(df$month),
    as.integer(df$day)
  )

  update_date <- tryCatch(
    as.Date(date_text, format = "%Y-%m-%d"),
    error = function(e) as.Date(NA_character_)
  )

  if (
    is.na(update_date) ||
    format(update_date, "%Y-%m-%d") != date_text
  ) {
    stop(
      "Includedstudies.csv contains an invalid update date.",
      call. = FALSE
    )
  }

  month_names <- c(
    "January", "February", "March", "April",
    "May", "June", "July", "August",
    "September", "October", "November", "December"
  )

  list(
    studies = df$studies,
    loweryear = df$loweryear,
    upperyear = df$upperyear,
    update_date_label = paste(
      format_number(df$day),
      month_names[as.integer(df$month)],
      format_number(df$year)
    )
  )
}

# ------------------------------------------------------------
# Read raw study data.
# ------------------------------------------------------------

read_study_data <- function(filename, page_title) {
  df <- read_github_csv(filename)

  required_columns <- c(
    "author", "year", "authoryear", "cases", "total",
    "discipline", "comparison", "sampleyear"
  )

  missing_columns <- setdiff(required_columns, names(df))

  if (length(missing_columns) > 0) {
    stop(
      paste(
        filename,
        "is missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!"outcome" %in% names(df)) {
    df$outcome <- rep(NA_character_, nrow(df))
  }

  df$outcome <- clean_text(df$outcome)

  # Missing grouping values belong to the original category.
  if (!"grouping" %in% names(df)) {
    df$grouping <- rep(page_title, nrow(df))
  } else {
    df$grouping <- clean_text(df$grouping)
    df$grouping[is.na(df$grouping)] <- page_title
  }

  df
}

# ============================================================
# UI-building functions
# ============================================================

make_main_tab <- function() {
  tabPanel(
    "Manuscript Changes Tracker",
    value = "main_page",

    fluidPage(
      sidebarLayout(
        sidebarPanel(
          style = paste(
            "background-color: white;",
            "border-radius: 8px;"
          ),

          tags$div(
            style = "padding: 18px;",

            tags$span(
              style = "font-weight:bold; font-size:18pt;",
              "Manuscript Changes Tracker"
            ),

            tags$br(),
            tags$br(),

            tags$div(
              style = "text-align:justify;",

              tags$strong("Objective:"),
              paste(
                "Scholarly manuscripts change from their preprinted,",
                "submitted, or rejected version to the later published",
                "(peer-reviewed) version of record (VoR).",
                "With this project we are summarizing knowledge on",
                "how much they change."
              ),

              tags$br(),
              tags$br(),

              tags$strong("Methods:"),
              paste(
                "Living synthesis of studies with series of",
                "meta-analyses of proportions."
              ),

              uiOutput("included_studies_sentence", inline = TRUE),

              "If we missed any studies please send them our way.",

              tags$br(),
              tags$br(),

              tags$strong("Results:"),
              "Explore the table to the right.",

              tags$br(),
              tags$br(),

              tags$strong("Note:"),
              paste(
                "The project is in its pilot stage, with design",
                "and data updates planned on a regular basis."
              ),

              tags$br(),
              tags$br(),

              tags$strong("Date of last update:"),
              uiOutput("last_update_date", inline = TRUE)
            )
          )
        ),

        mainPanel(
          tags$span(
            style = "font-weight:bold; font-size:18pt;",
            paste(
              "How often do specific manuscript aspects change",
              "during peer review and publishing?"
            )
          ),

          p(
            HTML(
              paste0(
                "<br>",
                "Use the percentage links below to view the ",
                "underlying study data or conduct an analysis ",
                "with different study selections, transformations, ",
                "or effect models."
              )
            )
          ),

          DT::dataTableOutput("main_table")
        )
      )
    )
  )
}

make_analysis_tab <- function(i, page_title, page_id) {
  tabPanel(
    title = page_title,
    value = page_id,

    fluidPage(
      titlePanel(page_title),

      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = paste0("analysis_group_", i),
            label = "Analysis",
            choices = page_title,
            selected = page_title
          ),

          selectInput(
            inputId = paste0("effect_model_", i),
            label = "Effect Model",
            choices = c("Fixed", "Random"),
            selected = "Random"
          ),

          selectInput(
            inputId = paste0("transformation_", i),
            label = "Transformation",
            choices = transformation_choices,
            selected = "PFT"
          ),

          actionButton(
            inputId = paste0("conduct_", i),
            label = "Conduct Analysis"
          )
        ),

        mainPanel(
          h3(
            textOutput(paste0("selected_analysis_title_", i))
          ),

          uiOutput(paste0("analysis_notes_", i)),

          DT::dataTableOutput(paste0("data_table_", i)),

          uiOutput(paste0("forest_plot_container_", i)),

          verbatimTextOutput(paste0("meta_results_", i))
        )
      )
    )
  )
}

make_about_tab <- function() {
  tabPanel(
    "About",
    value = "about_page",

    fluidPage(
      titlePanel("About"),

      p(
        HTML(
          paste0(
            "The idea for this living review originated in 2020, ",
            "while the live website was built in 2025.<br>",

            "The data is freely available at ",
            "<a href='https://github.com/MarioMalicki/",
            "Manuscript-Changes-Tracker' target='_blank'>",
            "Github</a>.<br>",

            "The list of studies included in our review is ",
            "available in a ",
            "<a href='https://www.zotero.org/groups/4772441/",
            "living_review_of_manuscript_changes' ",
            "target='_blank'>Zotero library</a>, or a ",

            "<a href='https://docs.google.com/spreadsheets/d/",
            "1tOk9DSi80uwvAp5nSfSI63ik4ps8ejp8nczeLU0ynew/",
            "edit?gid=0#gid=0' target='_blank'>",
            "Google spreadsheet</a>.<br>",

            "Please let us know if we have missed any studies ",
            "by filling out the ",
            "<a href='https://forms.gle/oAGFe1mqtV73mqcz7' ",
            "target='_blank'>following form</a>.<br>",

            "The full details of our methodology and results ",
            "will be preprinted soon."
          )
        )
      )
    )
  )
}

make_contact_tab <- function() {
  tabPanel(
    "Contact Us",
    value = "contact_page",

    fluidPage(
      titlePanel("Contact Us"),

      p(
        paste(
          "If you have any questions about this project,",
          "please email Mario Malicki at",
          "mmalicki@stanford.edu."
        )
      )
    )
  )
}

# The navigation and analysis pages are created after
# Mainpage.csv has been loaded for the session.
ui <- uiOutput("app_navigation")

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Load configuration once per session.
  # ----------------------------------------------------------

  mainpage_data <- reactive({
    tryCatch(
      {
        list(
          data = read_mainpage(),
          error = NULL
        )
      },
      error = function(e) {
        list(
          data = NULL,
          error = conditionMessage(e)
        )
      }
    )
  })

  included_studies_data <- reactive({
    tryCatch(
      {
        list(
          data = read_included_studies(),
          error = NULL
        )
      },
      error = function(e) {
        list(
          data = NULL,
          error = conditionMessage(e)
        )
      }
    )
  })

  # ----------------------------------------------------------
  # Build navigation and pages from Mainpage.csv.
  # ----------------------------------------------------------

  output$app_navigation <- renderUI({
    loaded <- mainpage_data()

    if (!is.null(loaded$error)) {
      return(
        fluidPage(
          h3("Unable to load the app configuration"),
          p(loaded$error),
          p(
            paste(
              "Please check that Mainpage.csv is available",
              "and contains the required columns."
            )
          )
        )
      )
    }

    pages <- loaded$data$pages

    analysis_tabs <- lapply(seq_len(nrow(pages)), function(i) {
      make_analysis_tab(
        i = i,
        page_title = pages$grouping[i],
        page_id = pages$page_id[i]
      )
    })

    analysis_menu <- do.call(
      navbarMenu,
      c(list("Meta-Analyses"), analysis_tabs)
    )

    navbarPage(
      "",
      id = "navpage",
      selected = "main_page",
      make_main_tab(),
      analysis_menu,
      make_about_tab(),
      make_contact_tab()
    )
  })

  # ----------------------------------------------------------
  # Study count and update date.
  # ----------------------------------------------------------

  output$included_studies_sentence <- renderUI({
    loaded <- included_studies_data()

    if (!is.null(loaded$error)) {
      return(
        tags$span(
          title = loaded$error,
          " Current sample information is temporarily unavailable. "
        )
      )
    }

    info <- loaded$data

    HTML(
      paste0(
        " Current sample includes <strong>",
        format_number(info$studies),
        " studies</strong> published from <strong>",
        format_number(info$loweryear),
        " till ",
        format_number(info$upperyear),
        "</strong>. "
      )
    )
  })

  output$last_update_date <- renderUI({
    loaded <- included_studies_data()

    if (!is.null(loaded$error)) {
      return(
        tags$span(
          title = loaded$error,
          " Unavailable"
        )
      )
    }

    tags$span(
      paste0(" ", loaded$data$update_date_label)
    )
  })

  # ----------------------------------------------------------
  # Main summary table.
  # Uses precomputed results only.
  # ----------------------------------------------------------

  output$main_table <- DT::renderDataTable({
    loaded <- mainpage_data()
    req(is.null(loaded$error))

    df <- loaded$data$summary

    aspect_html <- vapply(
      seq_len(nrow(df)),
      function(j) {
        label <- htmltools::htmlEscape(df$grouping[j])

        if (is.na(df$parent[j])) {
          as.character(tags$strong(HTML(label)))
        } else {
          paste0(
            '<span style="display:inline-block;',
            'padding-left:24px;">',
            "&#8627; ",
            label,
            "</span>"
          )
        }
      },
      character(1)
    )

    percentage_links <- vapply(
      seq_len(nrow(df)),
      function(j) {
        estimates <- c(
          df$percentage[j],
          df$lowerci[j],
          df$upperci[j]
        )

        if (all(!is.na(estimates))) {
          link_text <- paste0(
            format_number(df$percentage[j]),
            " (",
            format_number(df$lowerci[j]),
            " to ",
            format_number(df$upperci[j]),
            ")"
          )
        } else {
          link_text <- "Not available"
        }

        click_code <- paste0(
          "Shiny.setInputValue(",
          "'selected_summary_row',",
          j,
          ",{priority:'event'}",
          "); return false;"
        )

        as.character(
          tags$a(
            href = "#",
            onclick = click_code,
            link_text
          )
        )
      },
      character(1)
    )

    display_data <- data.frame(
      Aspect = aspect_html,
      Percentage = percentage_links,
      stringsAsFactors = FALSE
    )

    table <- DT::datatable(
      display_data,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",

      colnames = c(
        "Aspect",
        "Pooled Percentage (95% CI)"
      ),

      options = list(
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        dom = "t",

        columnDefs = list(
          list(
            className = "dt-left",
            targets = "_all"
          )
        )
      )
    )

    DT::formatStyle(
      table,
      columns = c("Aspect", "Percentage"),
      border = "1px solid #ddd",
      lineHeight = "1.5"
    )
  })

  # ----------------------------------------------------------
  # Main-table navigation.
  # Does not conduct an analysis.
  # ----------------------------------------------------------

  observeEvent(input$selected_summary_row, {
    loaded <- mainpage_data()
    req(is.null(loaded$error))

    config <- loaded$data

    j <- suppressWarnings(
      as.integer(input$selected_summary_row)
    )

    if (
      length(j) != 1L ||
      is.na(j) ||
      !j %in% seq_len(nrow(config$summary))
    ) {
      return()
    }

    selected_row <- config$summary[j, , drop = FALSE]
    i <- selected_row$page_index

    page_title <- config$pages$grouping[i]

    available_groups <- unique(c(
      page_title,
      config$summary$grouping[
        config$summary$analysis_page == page_title
      ]
    ))

    updateSelectInput(
      session,
      inputId = paste0("analysis_group_", i),
      choices = available_groups,
      selected = selected_row$grouping
    )

    updateNavbarPage(
      session,
      inputId = "navpage",
      selected = config$pages$page_id[i]
    )
  })

  # ----------------------------------------------------------
  # Register each analysis page after configuration loads.
  # ----------------------------------------------------------

  observeEvent(mainpage_data(), {
    loaded <- mainpage_data()
    req(is.null(loaded$error))

    config <- loaded$data

    lapply(seq_len(nrow(config$pages)), function(i) {
      force(i)

      page_title <- config$pages$grouping[i]
      dataset_filename <- config$pages$dataset[i]
      parent_note <- config$pages$notes[i]

      page_groups <- config$summary[
        config$summary$analysis_page == page_title,
        ,
        drop = FALSE
      ]

      available_groups <- unique(c(
        page_title,
        page_groups$grouping
      ))

      group_id <- paste0("analysis_group_", i)
      model_id <- paste0("effect_model_", i)
      transformation_id <- paste0("transformation_", i)
      button_id <- paste0("conduct_", i)
      table_id <- paste0("data_table_", i)
      selected_rows_id <- paste0(table_id, "_rows_selected")

      analysis_result <- reactiveVal(NULL)

      # Populate the selector after its UI has been inserted.
      observeEvent(input[[group_id]], {
        current_group <- isolate(input[[group_id]])

        if (
          is.null(current_group) ||
          !current_group %in% available_groups
        ) {
          current_group <- page_title
        }

        updateSelectInput(
          session,
          inputId = group_id,
          choices = available_groups,
          selected = current_group
        )
      }, once = TRUE)

      selected_group <- reactive({
        value <- input[[group_id]]

        if (is.null(value) || !nzchar(value)) {
          page_title
        } else {
          value
        }
      })

      output[[paste0("selected_analysis_title_", i)]] <- renderText({
        selected_group()
      })

      # Notes may contain HTML such as <br>.
      # Subgroups inherit the parent note when their note is blank.
      output[[paste0("analysis_notes_", i)]] <- renderUI({
        group_name <- selected_group()

        matching_row <- match(
          group_name,
          page_groups$grouping
        )

        note <- NA_character_

        if (!is.na(matching_row)) {
          note <- page_groups$notes[matching_row]
        }

        if (is.na(note)) {
          note <- parent_note
        }

        tagList(
          if (!is.na(note)) {
            tags$p(HTML(note))
          },
          tags$p(analysis_note)
        )
      })

      # Study data are loaded only when needed.
      load_data <- reactive({
        tryCatch(
          {
            list(
              data = read_study_data(
                filename = dataset_filename,
                page_title = page_title
              ),
              error = NULL
            )
          },
          error = function(e) {
            list(
              data = NULL,
              error = conditionMessage(e)
            )
          }
        )
      })

      filtered_data <- reactive({
        loaded_studies <- load_data()

        validate(
          need(
            is.null(loaded_studies$error),
            paste(
              "Unable to load study data:",
              loaded_studies$error
            )
          )
        )

        df <- loaded_studies$data

        df <- df[
          df$grouping == selected_group(),
          ,
          drop = FALSE
        ]

        rownames(df) <- NULL
        df
      })

      # No study rows are selected initially.
      output[[table_id]] <- DT::renderDataTable({
        df <- filtered_data()

        display_columns <- c(
          "authoryear",
          "cases",
          "total",
          "discipline",
          "comparison",
          "sampleyear",
          "outcome"
        )

        display_data <- df[, display_columns, drop = FALSE]

        display_data$outcome[
          is.na(display_data$outcome)
        ] <- "NA"

        DT::datatable(
          display_data,
          rownames = FALSE,
          selection = list(
            mode = "multiple",
            selected = NULL
          ),
          editable = FALSE,
          options = list(
            paging = FALSE,
            scrollX = TRUE,
            language = list(
              emptyTable = paste(
                "No study rows are available",
                "for the selected analysis."
              )
            )
          )
        )
      })

      # Settings changes clear previous results.
      # They do not conduct an analysis.
      observeEvent(
        list(
          input[[group_id]],
          input[[model_id]],
          input[[transformation_id]]
        ),
        {
          analysis_result(NULL)
        },
        ignoreInit = TRUE
      )

      # Conduct analysis only after a button click.
      observeEvent(input[[button_id]], {
        analysis_result(NULL)

        loaded_studies <- load_data()

        if (!is.null(loaded_studies$error)) {
          analysis_result(list(
            meta = NULL,
            error = paste(
              "Unable to load study data:",
              loaded_studies$error
            )
          ))
          return()
        }

        df <- filtered_data()

        if (nrow(df) == 0) {
          analysis_result(list(
            meta = NULL,
            error = paste(
              "No study rows are available",
              "for the selected analysis."
            )
          ))
          return()
        }

        selected_indices <- input[[selected_rows_id]]

        # No selection means use all rows in the current group.
        if (length(selected_indices) == 0L) {
          selected_indices <- seq_len(nrow(df))
        }

        selected_indices <- selected_indices[
          !is.na(selected_indices) &
            selected_indices >= 1 &
            selected_indices <= nrow(df)
        ]

        df <- df[selected_indices, , drop = FALSE]

        if (nrow(df) < 2) {
          analysis_result(list(
            meta = NULL,
            error = paste(
              "At least two rows are required for analysis.",
              "Select at least two rows, or clear the selection",
              "to use all rows in the displayed table."
            )
          ))
          return()
        }

        effect_model <- input[[model_id]]
        transformation <- input[[transformation_id]]

        valid_models <- c("Fixed", "Random")
        valid_transformations <- c(
          "PLOGIT", "PAS", "PFT", "PLN", "PRAW"
        )

        if (
          length(effect_model) != 1L ||
          length(transformation) != 1L ||
          !effect_model %in% valid_models ||
          !transformation %in% valid_transformations
        ) {
          analysis_result(list(
            meta = NULL,
            error = "Invalid effect model or transformation."
          ))
          return()
        }

        tryCatch(
          {
            df$cases <- parse_numeric_column(df$cases, "cases")
            df$total <- parse_numeric_column(df$total, "total")

            df <- df[
              !is.na(df$cases) & !is.na(df$total),
              ,
              drop = FALSE
            ]

            if (nrow(df) < 2) {
              stop(
                paste(
                  "At least two rows with non-missing cases",
                  "and total are required for analysis."
                ),
                call. = FALSE
              )
            }

            if (
              any(df$total <= 0) ||
              any(df$cases < 0) ||
              any(df$cases > df$total)
            ) {
              stop(
                paste(
                  "Each row must have total greater than zero",
                  "and cases between zero and total."
                ),
                call. = FALSE
              )
            }

            res <- meta::metaprop(
              event = df$cases,
              n = df$total,
              studlab = df$authoryear,
              sm = transformation,
              common = (effect_model == "Fixed"),
              random = (effect_model == "Random"),
              pscale = 100
            )

            analysis_result(list(
              meta = res,
              error = NULL
            ))
          },
          error = function(e) {
            analysis_result(list(
              meta = NULL,
              error = paste(
                "Meta-analysis failed:",
                conditionMessage(e)
              )
            ))
          }
        )
      })

      output[[paste0("forest_plot_container_", i)]] <- renderUI({
        result <- analysis_result()

        if (is.null(result) || is.null(result$meta)) {
          return(NULL)
        }

        plot_height <- max(
          450,
          250 + 22 * result$meta$k
        )

        plotOutput(
          outputId = paste0("forest_plot_", i),
          height = paste0(plot_height, "px")
        )
      })

      output[[paste0("forest_plot_", i)]] <- renderPlot({
        result <- analysis_result()

        req(
          !is.null(result),
          !is.null(result$meta)
        )

        meta::forest(
          result$meta,
          xlab = "Percentage"
        )
      })

      output[[paste0("meta_results_", i)]] <- renderPrint({
        result <- analysis_result()

        if (is.null(result)) {
          return(invisible(NULL))
        }

        if (!is.null(result$error)) {
          cat(result$error)
        } else {
          print(summary(result$meta))
        }
      })
    })
  }, once = TRUE)
}

# ============================================================
# Run the app
# ============================================================

shinyApp(ui, server)