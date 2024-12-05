library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

# Calories burned function based on workout type, time, gender, age, and location
calculate_calories <- function(workout_type, time_minutes, gender, age, location) {
  # Regression coefficients from linear regression model
  intercept <- 23.14789
  age_coef <- -0.00047
  gender_male_coef <- 0.00839
  gender_nonbinary_coef <- 0.10387
  location_miami_coef <- 0.10592
  workout_duration_coef <- -0.12044
  workout_type_coeffs <- c(
    cardio = 0,
    crossfit = 0.03685,
    pilates = 0.16100,
    swimming = 0.38898,
    weightlifting = 0.29708,
    yoga = 0.23139
  )
  interaction_coeffs <- c(
    cardio = 0,
    crossfit = -0.00024,
    pilates = -0.00128,
    swimming = -0.00288,
    weightlifting = -0.00217,
    yoga = -0.00169
  )
  
  # Calculate gender coefficient
  gender_coef <- ifelse(gender == "Male", gender_male_coef, ifelse(gender == "Non-binary", gender_nonbinary_coef, 0))
  
  # Calculate location coefficient
  location_coef <- ifelse(location == "Miami", location_miami_coef, 0)
  
  # Get workout type coefficient
  workout_type_coef <- workout_type_coeffs[tolower(workout_type)]
  
  # Get interaction coefficient
  interaction_coef <- interaction_coeffs[tolower(workout_type)]
  
  # Calculate calories burned per minute
  calories_per_min <- intercept + (age_coef * age) + gender_coef + location_coef + 
    (workout_duration_coef * time_minutes) + workout_type_coef + (interaction_coef * time_minutes)
  
  # Calculate total calories burned
  total_calories <- calories_per_min * time_minutes
  
  return(round(total_calories, 1))
}

# Generate motivational message
generate_motivational_message <- function() {
  messages <- c(
    "Keep on working, you got this!",
    "Great job! Stay consistent and keep pushing!",
    "You are doing amazing, keep it up!",
    "Believe in yourself, you're making great progress!",
    "One step at a time, you're getting stronger!",
    "Keep going, every effort counts!",
    "Stay positive, stay active! You can do it!",
    "Your hard work is paying off, keep pushing!",
    "Fantastic effort, don't stop now!",
    "Keep the momentum, you're doing great!"
  )
  return(sample(messages, 1))
}

# Generate sample data for visualization
generate_sample_data <- function(n = 5000) {
  workout_types <- c("Cardio", "Crossfit", "Pilates", "Swimming", "Weightlifting", "Yoga")
  locations <- c("Austin", "Boston", "Denver", "Detroit", "Las Vegas", "Miami", "Orlando", "San Francisco", "Seattle")
  data <- data.frame(
    workout_type = sample(workout_types, n, replace = TRUE),
    time_minutes = sample(10:60, n, replace = TRUE),
    gender = sample(c("Male", "Female", "Non-binary"), n, replace = TRUE),
    age = sample(18:70, n, replace = TRUE),
    location = sample(locations, n, replace = TRUE)
  )
  
  data$calories_burned <- mapply(calculate_calories,
                                 data$workout_type,
                                 data$time_minutes,
                                 data$gender,
                                 data$age,
                                 data$location)
  return(data)
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Calories Burned Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "workout_type",
        "Workout Type:",
        choices = c("Cardio", "Crossfit", "Pilates", "Swimming", "Weightlifting", "Yoga")
      ),
      numericInput("time", "Workout Time (minutes):", value = 30, min = 1),
      selectInput(
        "gender",
        "Gender:",
        choices = c("Male", "Female", "Non-binary")
      ),
      numericInput("age", "Age:", value = 25, min = 1),
      selectInput(
        "location",
        "Location:",
        choices = c("Austin", "Boston", "Denver", "Detroit", "Las Vegas", "Miami", "Orlando", "San Francisco", "Seattle")
      )
    ),
    mainPanel(
      h4("Estimated Total Calories Burned:"),
      verbatimTextOutput("calories_output"),
      verbatimTextOutput("motivational_message"),
      h4("Calories Burned Per Minute Distribution by Workout Type"),
      plotlyOutput("calories_histogram"),
      h4("Workout Duration vs Calories Burned"),
      plotlyOutput("duration_vs_calories_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$calories_output <- renderText({
    total_calories <- calculate_calories(
      workout_type = input$workout_type,
      time_minutes = input$time,
      gender = input$gender,
      age = input$age,
      location = input$location
    )
    paste("You burned approximately", total_calories, "calories in total.")
  })
  
  output$motivational_message <- renderText({
    generate_motivational_message()
  })
  
  # Generate and display the workout duration vs calories burned plot
  output$duration_vs_calories_plot <- renderPlotly({
    final <- read_csv("datasets/final.csv")
    plot1 <- final %>%
      ggplot(aes(x = workout_duration, y = calories_per_min)) +
      geom_bin2d() +
      scale_fill_gradientn(colors = c("#FFFFFF", "#A3C9A8", "#F4A261", "#E76F51")) +
      labs(
        title = "Workout Duration vs Calories Burned", 
        x = "Workout Duration (minutes)", 
        y = "Calories Burned per Minute"
      )
    ggplotly(plot1)
  })
  
  # Generate a histogram of calories burned per minute
  output$calories_histogram <- renderPlotly({
    final <- read_csv("datasets/final.csv")
    plot2 <- final %>%
      ggplot(aes(x = calories_per_min, fill = workout_type)) +
      geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge") +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = "Calories Burned Per Minute Distribution by Workout Type",
        x = "Calories Burned Per Minute",
        y = "Count"
      )
    ggplotly(plot2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
