#' Cathlab page UI Function
#' @description A function to create the main page for the cathlab.
#'
#' @import shiny
#' @importFrom shinyWidgets prettyRadioButtons sliderTextInput
#' @return a tagList object representing the cathlab page
#' @noRd
#'
mod_cathlab_ui <- function(id){
  # All "outputId" needs to be wrapped in this module's namespace using NS(id, str_for_component_id)
  ns <- NS(id)

  # Create the page content
  tagList(
    # A title
    titlePanel("Cath lab"),
    # Horizontal line
    tags$hr(),
    # Header title
    tags$h3("Inpatients awaiting procedures"),
    #1 number of people awaiting procedures - pie chart
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("cathlab_awaiting_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("Ward", "Consultant"),
                           choiceValues = c("ward", "consultant"),
                           selected = "ward",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        align = "centre",
        mainPanel(plotOutput(outputId=ns("cath_lab_awaiting")))
      )
    ),
    # Header title
    tags$h3("Cath lab activity"),
    # slider - days to cumulate
    # radiobuttons - all(stacked), angio etc
    #2 number of procedures in the last 7 days - bar chart, stacked by type
    fluidRow(
      column(
        width = 12,
        align = "centre",
        prettyRadioButtons(inputId = ns("cathlab_activity_grouping_variable"),
                           label = "Select Grouping:",
                           choiceNames = c("All", "Angiogram", "Pacemaker", "CRT", "Ablation"),
                           choiceValues = c("all", "angiogram", "pacemaker", "crt", "ablation"),
                           selected = "all",
                           inline = TRUE,
                           animation = "smooth")
      ),
      column(
        width = 12,
        align = "centre",
        sliderTextInput(inputId = ns("cathlab_activity_slider"),
                        label = "Over the last (days):",
                        choices = c(1, 14, 30, 90, 180, 270, 365),
                        selected = 30,
                        grid = TRUE)
      ),
      column(
        width = 12,
        mainPanel(plotOutput(outputId=ns("cath_lab_activity"),
                             brush = ns("cath_lab_activity_plot_brush")))
      ),
      column(
        width = 12,
        mainPanel(tableOutput(outputId = ns("cath_lab_activity_table")))
      )
    )
  )
}

#     # A Stack with a card in it
#     Stack(horizontal=TRUE,
#           tokens=list(childrenGap=10),
#           makeCard("Filters", filters, size=4, style="max-height: 320px")),
#     # Some more output
#     uiOutput("analysis")
#
# filters <- shiny.fluent::Stack(
#   tokens = list(childrenGap = 10),
#   shiny.fluent::Stack(
#     horizontal = TRUE,
#     tokens = list(childrenGap = 10),
#     shiny.fluent::DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
#     shiny.fluent::DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
#   ),
#   shiny.fluent::Label("Filter by sales reps", className = "my_class"),
#   shiny.fluent::NormalPeoplePicker.shinyInput(
#     "selectedPeople",
#     class = "my_class",
#     options = shiny.fluent::fluentPeople,
#     pickerSuggestionsProps = list(
#       suggestionsHeaderText = 'Matching people',
#       mostRecentlyUsedHeaderText = 'Sales reps',
#       noResultsFoundText = 'No results found',
#       showRemoveButtons = TRUE
#     )
#   ),
#   shiny.fluent::Slider.shinyInput("slider",
#                                   value = 0, min = 0, max = 1000000, step = 100000,
#                                   label = "Minimum amount",
#                                   valueFormat = shiny.fluent::JS("function(x) { return '$' + x}"),
#                                   snapToStep = TRUE
#   ),
#   shiny.fluent::Toggle.shinyInput("closedOnly", value = TRUE, label = "Include closed deals only?")
# )









#' cathlab Server Functions
#'
#' @importFrom stringr str_to_upper str_to_title
#' @importFrom dplyr bind_rows
#' @importFrom zoo rollmean
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr arrange ungroup coalesce first
#' @importFrom tidyr complete
#' @importFrom lubridate time_length interval
#' @noRd
#'
mod_cathlab_server <- function(id){
  moduleServer(id, function(input, output, session){

    # calculate the cath lab data
    today <- as.POSIXct("2023-07-30 23:59")
    cath_lab_orders_data <- admissions_data(discharged=FALSE) |>
        left_join(orders_data(), by="nhs_number") |>
        left_join(procedures_data(), by="nhs_number") |>
        mutate(awaiting     = if_else(!is.na(.data$order_datetime) & is.na(.data$procedure_datetime), TRUE, FALSE),
               waiting_time = if_else(!is.na(.data$order_datetime) & is.na(.data$procedure_datetime),
                                      time_length(interval(with_tz(.data$order_datetime, tzone = "GMT"),
                                                           with_tz(today, tzone = "GMT")), unit="days"),
                                      NA_real_),
               time_to_proc = if_else(!is.na(.data$order_datetime) & !is.na(.data$procedure_datetime),
                                      time_length(interval(with_tz(.data$order_datetime, tzone = "GMT"),
                                                           with_tz(.data$procedure_datetime, tzone = "GMT")), unit="days"),
                                      NA_real_))

    # calculate activity over time
    .tmp_cath_lab_activity_data <- procedures_data() |>
      mutate(date = as.Date(.data$procedure_datetime)) |>
      select(-.data$procedure_datetime)

    .tmp_cath_lab_activity_all <- .tmp_cath_lab_activity_data |>
      group_by(.data$date) |>
      summarise(count = n()) |>
      mutate(procedure_type = "all")

    cath_lab_activity_data <- .tmp_cath_lab_activity_data |>
      group_by(.data$date, .data$procedure_type) |>
      summarise(count = n()) |>
      bind_rows(.tmp_cath_lab_activity_all) |>
      mutate(procedure_type = factor(.data$procedure_type,
                                     levels=c("all", unique(.data$procedure_type)[!unique(.data$procedure_type)=="all"]))) |>
      ungroup() |>
      complete(.data$date, .data$procedure_type, fill=list(count=0))


    # Pie chart showing % of patients awaiting a procedure
    output$cath_lab_awaiting <- renderPlot({

      grouping <- as.character(input$cathlab_awaiting_grouping_variable)
      n_groups <- length(unique(cath_lab_orders_data[[grouping]]))
      format_labs <- function(x, grouping) {
        if (grouping == "ward") {
          str_to_upper(x)
        } else if (grouping == "consultant") {
          str_to_title(x)
        } else {
          "error"
        }
      }

      cath_lab_orders_data |>
        group_by(.data$awaiting) |>
        ggplot(aes(x = .data[[grouping]],
                   # y = ..count..,
                   fill = .data[[grouping]])) +
        geom_bar(stat="count") +
        scale_x_discrete(labels = function(x) format_labs(x, grouping)) +
        scale_fill_manual(values = colorRampPalette(wes_palette(n=4, name = "GrandBudapest1"))(n_groups)) +
        ylab("Number of patients") +
        xlab("") +
        theme(legend.position = "none")
    })

    output$cath_lab_activity <- renderPlot({

      grouping <- as.character(input$cathlab_activity_grouping_variable)
      today <- as.Date("2023-07-30")
      win_start <- today - input$cathlab_activity_slider
      win_len <- time_length(interval(win_start, today), unit="days")
      # line_smooth_k <- ifelse(win_len < 7, 2, 7)
      num_proc_types <- length(unique(cath_lab_activity_data$procedure_type))
      colours <- colorRampPalette(wes_palette(n=5, name = "Darjeeling1"))(num_proc_types)
      names(colours) <- levels(cath_lab_activity_data$procedure_type)
      colours <- if(grouping=="all") colours else colours[grouping]

      cath_lab_activity_data |>
        group_by(.data$procedure_type) |>
        arrange(.data$date) |>
        mutate(roll_mean = rollmean(.data$count, k=7, align = "right", fill=NA),
               roll_mean = coalesce(.data$roll_mean, first(.data$roll_mean, na_rm=TRUE))) |>
        filter(date > win_start) %>%
        {if(grouping!="all") filter(., .data$procedure_type==grouping) else .} |>
        ggplot(aes(x     = .data$date,
                   y     = .data$count,
                   color = .data$procedure_type,
                   fill  = .data$procedure_type)) +
        geom_point(shape=21, size=1) +
        geom_line(aes(y=.data$roll_mean)) +
        scale_fill_manual(values = colours,
                          labels = function(x) str_to_title(x)) +
        scale_color_manual(values = colours,
                           labels = function(x) str_to_title(x)) +
        ylab("Procedures per day") +
        xlab("Date") +
        xlim(win_start-1, today+1) +
        labs(fill = "", color="") +
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12))


    })

    output$cath_lab_activity_table <- renderTable({

      grouping <- as.character(input$cathlab_activity_grouping_variable)

      # Require a click event to have happened
      req(input$cath_lab_activity_plot_brush)
      # Get the rows of the data frame based on the click info
      brushedPoints(df    = cath_lab_activity_data,
                    brush = input$cath_lab_activity_plot_brush,
                    xvar  = "date",
                    yvar  = "count") %>%
        # filter for the grouping being shown, if not "all" of the procedures
        {if(grouping!="all") filter(., .data$procedure_type==grouping) else .} |>
        # Mutate for nice formatting
        mutate(date = format(as.Date(.data$date), format="%d-%m-%Y")) |>
        # Rename table column names for nice formatting
        select("Date"      = .data$date,
               "Procedure" = .data$procedure_type,
               "Count"     = .data$count)
    })

    output$cath_lab_waiting_time <- renderPlot({
      #shinipsum::random_ggplot()
    })
    output$cath_lab_proc_to_dis_time <- renderPlot({
      #shinipsum::random_ggplot()
    })

  })
}
