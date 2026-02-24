# Read in CSV data about school enrollment
# This data is taken from https://www.census.gov/data/tables/time-series/demo/school-enrollment/cps-historical-time-series.html, 
# it includes historical enrollment data for schools.  
 enrollment <- read_csv("https://raw.githubusercontent.com/EmmaBrunsell/Wisconsin-School-District-Data/refs/heads/main/School_Enrollment.csv")

######################################### Clean Data ######################################### 

# Much of the data before 1995 is incomplete
# Focus on more recent enrollment trends
enrollment <- enrollment %>%
  filter(Year >= 1995)

enrollment$Total <- NULL

# Transform data for easier plotting
enrollment_long <- enrollment %>%
  # Pivot longer to show each year, level, and type as one row (entry)
  pivot_longer(
    cols = -Year,
    names_to = "School_Type",
    values_to = "Enrollment"
  ) %>%
  # Split the "School Type" column into the grade level and type (publice, private, etc)
  extract(
    col = School_Type,
    into = c("Level", "Type"),
    regex = "^(.*) (Public|Private|Full Time)$", # Regex expression to split columns
  )

# Order the levels by grade for better plotting clarity
enrollment_long$Level <- factor(enrollment_long$Level,
  levels = c("Nursery", "Kindergarten", "Elementary", "High School", "College")
)

# Change the type of each column for plotting 
enrollment_long$Year <- as.numeric(as.character(enrollment_long$Year))
enrollment_long$Level <- as.character(enrollment_long$Level)
enrollment_long$Type <- as.character(enrollment_long$Type)

#############################################################################################

################################## Shiny Interface ##########################################

# Filter the data down to specific, selected, years
filter_year <- function(df, years) {
  df[df$Year %in% years, ]
}

# Function for making the column plot
column_plot <- function(df) {
  ggplot(df, aes(x = factor(Year), y = Enrollment, fill = Level)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Enrollment by School Level", x = "Year", y = "Enrollment")
}

# Function for making the stacked area plot
percentage_plot <- function(df, selected_year = NULL) {
  # Must have two x value points
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  # Plot the area graph that gives stacked percentages of enrollment by level
  p <- ggplot(df, aes(x = Year, y = Percent, fill = Level)) +
    geom_area(position = "stack") +
    
    # Ensures that the years stay as integers (ex. 2012, 2013, 2014)
    scale_x_continuous(
      breaks = seq(min(df$Year), max(df$Year), by = 1)
    ) +
    
    # Creates continuous y axis and turns numbers into percents
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Enrollment by Level (%)",
      x = "Year",
      y = "Percentage of Total Enrollment"
    )
  
  # Adds a vertical dashed line for a year that was clicked on
  if (!is.null(selected_year)) {
    p <- p + geom_vline(
      xintercept = selected_year,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5
    )
  }
  
  # Return the area plot
  return(p)
}

# User Interface
ui <- fluidPage(
  titlePanel("School Enrollment in Chicago"),
  
  tags$h4("CPS Historical Enrollment Data from the United States Census", 
          style = "color: gray; margin-top: -10px;"),
  
  tags$p("This visualization shows school enrollment trends in CPS (Current Population Survey)
from the U.S. Census time series data 1995-2022. 
Use the filters to explore enrollment by school level (Nursery, Kindergarten, etc.) and type - indicating
if it is public, private, or Full Time (at the college level). 
Click on a year in the bar chart to highlight it in the percentage trend plot."), 
  
  # First row is the filter panel and column plot
  fluidRow(
    # Filters first column
    column(
      3,
      
      # Year slider filter
      sliderInput(
        "year_range",
        "Selected Year Range:",
        min = min(enrollment_long$Year),
        max = max(enrollment_long$Year),
        value = c(min(enrollment_long$Year), max(enrollment_long$Year)),
        step = 1, sep = ""
      ),
      
      # Drop down level filter
      selectInput(
        "level_select",
        "Select Level:",
        choices = c("All", sort(unique(enrollment_long$Level))),
        selected = "All"
      ),
      
      # Drop down type filter
      selectInput(
        "type_select",
        "Select Type:",
        choices = c("All", sort(unique(enrollment_long$Type))),
        selected = "All"
      ),
      
      # Button to reset all filters
      actionButton("reset", "Reset Selection")
    ),
    
    # Column plot in the second column
    column(
      9,
      plotOutput("plot", click = "plot_click", height = "400px")
    )
  ),
  
  # Second row is the data table and stacked area graph
  fluidRow(
    column(
      8,
      plotOutput("percentage_plot", height = "300px")
    ),
    column(
      4,
      dataTableOutput("table")
    )
  )
)


server <- function(input, output, session) {
  # Reactive value for interacting with the column plot
  clicked_year <- reactiveVal(NULL)
  
  # Reactive filtering of the data based on filters/inputs
  filtered_data <- reactive({
    df <- enrollment_long
    
    # Filter for the year range in the slider
    df <- df[df$Year >= input$year_range[1] & df$Year <= input$year_range[2], ]
    
    # Filter for the year that is clicked on the column plot
    if (!is.null(clicked_year())) {
      df <- df[df$Year == clicked_year(), ]
    }
    
    # Filter by the drop down selection of level
    if (!is.null(input$level_select) && input$level_select != "All") {
      df <- df[df$Level == input$level_select, ]
    }
    
    # Filter for the drop down selection of type
    if (!is.null(input$type_select) && input$type_select != "All") {
      df <- df[df$Type == input$type_select, ]
    }
    
    df
  })
  
  # Get the percentage of enrollment data for the stacked area graph based on inputs
  percentage_data <- reactive({
    df <- enrollment_long
    
    # Filter for the slider range of years
    df <- df[df$Year >= input$year_range[1] & df$Year <= input$year_range[2], ]
    
    # Filter for the level drop down
    if (!is.null(input$level_select) && input$level_select != "All") {
      df <- df[df$Level %in% input$level_select, ]
    }
    
    # Filter for the type drop down
    if (!is.null(input$type_select) && input$type_select != "All") {
      df <- df[df$Type %in% input$type_select, ]
    }
    
    # Get the percentage of Enrollment for each filter
    df %>%
      group_by(Year, Level) %>%
      summarize(Enrollment = sum(Enrollment), .groups = "drop") %>%
      group_by(Year) %>%
      mutate(Percent = Enrollment / sum(Enrollment)) %>%
      ungroup()
  })
  
  # Filter data when the plot is clicked
  observeEvent(input$plot_click, {
    # Get the filtered data
    df <- filtered_data()
    if (nrow(df) == 0) {
      return()
    }
    
    x_vals <- 1:length(unique(df$Year))
    
    nearest_index <- round(input$plot_click$x)
    nearest_index <- min(max(nearest_index, 1), length(x_vals))
    
    # Find the nearest year to the clicked area
    nearest_year <- sort(unique(df$Year))[nearest_index]
    
    clicked_year(nearest_year)
  })
  
  observeEvent(input$reset, {
    # Reset the clicked year
    clicked_year(NULL)
    
    # Reset the slider
    updateSliderInput(session, "year_range", value = c(min(enrollment_long$Year), max(enrollment_long$Year)))
    
    # Reset the drop downs
    updateSelectInput(session, "level_select", selected = "All")
    updateSelectInput(session, "type_select", selected = "All")
  })
  
  # Render the plots and the data table
  output$plot <- renderPlot(column_plot(filtered_data()))
  output$table <- renderDT(filtered_data())
  output$percentage_plot <- renderPlot({
    percentage_plot(percentage_data(), selected_year = clicked_year())
  })
}

shinyApp(ui, server)
