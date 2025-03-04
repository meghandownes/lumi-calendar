library(shiny)
library(ggplot2)
library(lubridate)
library(shinyTime)
library(ggforce)

activity_colors <- c(
  "Teaching" = "firebrick4",
  "Class Prep" = "firebrick3",
  "Non-Teaching NMSU Activity" = "firebrick2",
  "NMSU Service Work" = "firebrick1",
  "Third Shift" = "coral1",
  "Tidying" = "coral2",
  "Maintenance" = "coral3",
  "Provisioning" = "coral4",
  "Hygiene" = "cadetblue1",
  "Physical Self-Care" = "cadetblue2",
  "Bowflex" = "cadetblue3",
  "Ride Bike" = "cadetblue4",
  "Restoration" = "turquoise1",
  "Adventure" = "turquoise2",
  "Create" = "turquoise3",
  "Build the Future" = "turquoise4"
  
)

draw_clock <- function(events = NULL) {
  p <- ggplot() +
    coord_polar() +
    scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 23, 1),
      labels = c(00, 1:23)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    annotate(
      "text",
      x = 0:23,
      y = 1.1,
      label = c(00, 1:23),
      size = 5
    )
  
  if (!is.null(events) && nrow(events) > 0) {
    if(all(c("start_x", "end_x", "Activity") %in% names(events))) {
      p <- p + geom_rect(
        data = events,
        aes(xmin = start_x, xmax = end_x, ymin = 0, ymax = 0.6, fill = Activity),
        alpha = 0.8,
        inherit.aes = FALSE
      ) +
        scale_fill_manual(values = activity_colors)
    }
  }
  return(p)
}

ui <- fluidPage(
  titlePanel("Lumi - Activity Clock Visualizer"),
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Select Date", value = Sys.Date()),
      timeInput("start_time", "Start Time", value = strptime("12:00", "%H:%M"), seconds = FALSE),
      timeInput("end_time", "End Time", seconds = FALSE),
      textInput("event", "Event Description"),
      selectInput("activity", "Activity Type", choices = names(activity_colors)),
      actionButton("add", "Add Event", class = "btn-primary"),
      hr(),
      uiOutput("delete_ui"),
      actionButton("delete", "Delete Selected", class = "btn-danger"),
      width = 3
    ),
    mainPanel(
      plotOutput("clock", click = "clock_click"),
      h4("Today's Events:"),
      tableOutput("event_table")
    )
  )
)

server <- function(input, output, session) {
  events <- reactiveValues(data = data.frame(
    date = as.Date(character()),
    start_time = character(),
    end_time = character(),
    description = character(),
    Activity = character(),
    start_x = numeric(),
    end_x = numeric()
  ))
  
  observeEvent(input$add, {
    tryCatch({
      req(input$start_time, input$end_time, input$event, input$activity)
      
      start_time <- input$start_time
      end_time <- input$end_time
      
      validate(
        need(!is.null(start_time), "Please select a valid start time"),
        need(!is.null(end_time), "Please select a valid end time"),
        need(input$event != "", "Please enter an event description"),
        need(start_time < end_time, "Start time must be before end time")
      )
      
      start_x <- (hour(start_time) %% 24) + minute(start_time)/60
      end_x <- (hour(end_time) %% 24) + minute(end_time)/60
      
      if (end_x < start_x) end_x <- end_x + 24
      
      new_event <- data.frame(
        date = input$date,
        start_time = format(start_time, "%H:%M"),
        end_time = format(end_time, "%H:%M"),
        description = input$event,
        Activity = input$activity,
        start_x = start_x,
        end_x = end_x
      )
      
      events$data <- rbind(events$data, new_event)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$delete, {
    tryCatch({
      req(input$selected_row)
      row <- as.integer(input$selected_row)
      validate(
        need(row > 0 && row <= nrow(events$data), "Invalid row selected")
      )
      events$data <- events$data[-row, ]
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$delete_ui <- renderUI({
    if(nrow(events$data) > 0) {
      choices <- setNames(1:nrow(events$data), 
                          paste(events$data$start_time, "-", events$data$description))
      selectInput("selected_row", "Select Event to Delete:", choices = choices)
    }
  })
  
  observeEvent(input$clock_click, {
    tryCatch({
      req(input$clock_click)
      x <- input$clock_click$x - 0.5
      y <- input$clock_click$y - 0.5
      angle <- (atan2(y, x) + pi/2) %% (2*pi)
      polar_x <- angle/(2*pi) * 24
      
      hours <- floor(polar_x)
      minutes <- round((polar_x - hours) * 60)
      
      time <- strptime(paste(hours, minutes), "%H %M")
      
      if (input$time_select == "Start") {
        updateTimeInput(session, "start_time", value = time)
      } else {
        updateTimeInput(session, "end_time", value = time)
      }
    }, error = function(e) {
      showNotification("Invalid click position", type = "warning")
    })
  })
  
  selected_events <- reactive({
    req(input$date)
    subset(events$data, date == input$date)
  })
  
  output$clock <- renderPlot({
    req(input$date)
    if(nrow(selected_events()) > 0) {
      draw_clock(selected_events())
    } else {
      draw_clock()
    }
  })
  
  output$event_table <- renderTable({
    events <- selected_events()
    if(nrow(events) > 0) {
      events[, c("start_time", "end_time", "description", "Activity")]
    }
  })
}

shinyApp(ui, server)
