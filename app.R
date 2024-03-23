library(fpp3)
library(plotly)
library(shiny)
library(ggplot2)
library(shinythemes)

# Load data
file_path <- 'multiTimeline.csv'
ml_path <- 'MLdata.csv'
ai_path <- 'AIdata.csv'

neuro <- read.csv(file_path, skip = 2)
names(neuro) <- c("Month", "Interest")

neuro$Month <- yearmonth(neuro$Month)
neuro <- tsibble(neuro)

# Convert "<1" to 0's
neuro$Interest <- as.numeric(
  ifelse(neuro$Interest == "<1", 0, neuro$Interest)
)

ml <- read.csv(ml_path, skip = 2)
names(ml) <- c("Month", "Interest")

ml$Month <- yearmonth(ml$Month)
ml <- tsibble(ml)

# Convert "<1" to 0's
ml$Interest <- as.numeric(
  ifelse(ml$Interest == "<1", 0, ml$Interest)
)

ai <- read.csv(ai_path, skip = 2)
names(ai) <- c("Month", "Interest")

ai$Month <- yearmonth(ai$Month)
ai <- tsibble(ai)

# Convert "<1" to 0's
ai$Interest <- as.numeric(
  ifelse(ai$Interest == "<1", 0, ai$Interest)
)

ui <- fluidPage(theme = shinytheme("superhero"),

  # title and instructions
  titlePanel("\"Neuromorphic Computing\" Google Searches Over Time"),

  HTML("<div style='text-align: left; font-size: 16px; margin-bottom: 20px;'>
          <p>How to Use this Tool
            <ul style='text-align: left;'>
              <li>Optionally overlay the Neuromorphic Computing plot with Machine Learning and/or Artificial
       Intelligence Interest by using the checkboxes. </li>
              <li>Use the side panel buttons to choose to view either a seasonality, autocorrelation,
       or decomposition plot.
              <li>Choose a more specific option (e.g. type of decomposiiton, type of seasonality plot)
       corresponding to your selection. </li>
            </ul>
       </div>"
  ),
  
  checkboxInput("overlay_ml", "Overlay Machine Learning Interest", value = FALSE),
  checkboxInput("overlay_ai", "Overlay Artificial Intelligence Interest", value = FALSE),

  # plot of full time series and description
  plotlyOutput("linePlot"),

  HTML("<div style='text-align: left; font-size: 16px; margin-bottom: 20px;'>
          <p>This plot shows the full time series of the number of Google searches of
       \"Neuromorphic Computing\". The data were collected on a monthly basis, beginning in
       January of 2004 and ending in February of 2024. Until about 2013, the relative interest
       was most frequently zero, with a few spikes. From 2013 onward, the trend has been increasing
       steadily. The series appears to follow an irregular, mostly multiplicative trend after 2013.
       The trend does not appear to be linear. Rather, it appear to have some concavity, especially
       at the end of the timeline. The \"bumps\" in the data do not appear to follow any sort of pattern.
       Therefore the series does not have significant seasonality. When the neuromorphic computing interest
       is overlaid with similar topics, such as artificial intelligence or machine learning, we can see how
       the spikes in each series correlate. For example, all 3 series spiked in 2018 with the release of Intel's
       Loihi chip.
       </div>"
  ),

  sidebarLayout(
    sidebarPanel(
      # choose plot from seasonality, autocorrelation, and decomposition
      radioButtons(
        inputId = "selectPlot",
        label = "Choose a Plot",
        choices = c("Seasonality", "Autocorrelation", "Decomposition"),
        selected = "Seasonality"
      ),
      conditionalPanel(
        condition = "input.selectPlot == 'Seasonality'",
        selectInput(
          inputId = "seasonalityOption",
          label = "Choose a Seasonality Visualization",
          choices = c("Season", "Subseries")
        )
      ),
      conditionalPanel(
        condition = "input.selectPlot == 'Autocorrelation'",
        selectInput(
          inputId = "autocorrOption",
          label = "Select Autocorrelation Option",
          choices = c("Lag Scatter Plots", "ACF")
        )
      ),
      conditionalPanel(
        condition = "input.selectPlot == 'Decomposition'",
        selectInput(
          inputId = "decompositionOption",
          label = "Choose a Type of Decomposition",
          choices = c("Classical (Additive)", "Classical (Multiplicative)", "STL")
        )
      ),
      conditionalPanel(
        condition = "input.selectPlot == 'Autocorrelation' & input.autocorrOption == 'ACF'",
        numericInput(
          inputId = "acfYears",
          label = "Select number of previous years to show ACF for",
          min = 1,
          max = 20,
          value = 20
        )
      ),
      # interpretation for selected plot
      textOutput("interpretation")
      # additional feature inputs

    ),
    mainPanel(
      # seasonality plot
      conditionalPanel(
        condition = "input.selectPlot == 'Seasonality'",
        plotOutput("seasonalPlot")
      ),
      # autocorrelation plot
      conditionalPanel(
        condition = "input.selectPlot == 'Autocorrelation'",
        plotOutput("autocorrPlot")
      ),
      # decomposition plot
      conditionalPanel(
        condition = "input.selectPlot == 'Decomposition'",
        plotOutput("decompPlot"),
      # additional feature
      )
    )
  )
)

server <- function(input, output, session) {

  # full-time series plot
  output$linePlot <- renderPlotly({
    neuro$Month <- as.Date(neuro$Month)
    ml$Month <- as.Date(ml$Month)
    ai$Month <- as.Date(ai$Month)
    
    plot_data <- neuro
    if (input$overlay_ml) {
      plot_data <- merge(plot_data, ml, by = "Month", all = TRUE)
    }
    if (input$overlay_ai) {
      plot_data <- merge(plot_data, ai, by = "Month", all = TRUE)
    }
    plot_data <- na.omit(plot_data)
    
    p <- plot_ly(plot_data, x = ~Month)
    if (input$overlay_ml) {
      p <- p %>% add_lines(y = ~ml$Interest, color = I("orange"), name = "Machine Learning")
    }
    if (input$overlay_ai) {
      p <- p %>% add_lines(y = ~ai$Interest, color = I("red"), name = "Artificial Intelligence")
    }
    p <- p %>% add_lines(y = ~neuro$Interest, color = I("blue"), name = "Neuromorphic Computing") %>%
      layout(title = "Neuromorphic Computing Google Search Interest Over Time", xaxis = list(title = "Month"), yaxis = list(title = "Interest"),
             margin = list(l = 50, r = 50, b = 100, t = 100))
    
    m <- neuro[which(neuro$Month == yearmonth("2018 Feb")), ]
    m2 <- neuro[which(neuro$Month == yearmonth("2013 Dec")), ]
    m3 <- neuro[which(neuro$Month == yearmonth("2014 May")), ]
    m4 <- neuro[which(neuro$Month == yearmonth("2011 Dec")), ]
    m5 <- neuro[which(neuro$Month == yearmonth("2006 Aug")), ]
    m6 <- ai[which(ai$Month == yearmonth("2023 May")), ]

    a <- list(x = m$Month, y = m$Interest, text = "Intel Loihi",
      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40)
    
    a2 <- list(x = m2$Month, y = m2$Interest, text = "Markham's MBP",
              xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 20, ay = -40)
    
    a3 <- list(x = m3$Month, y = m3$Interest, text = "IBM TrueNorth",
               xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 50, ay = -70)
    
    a4 <- list(x = m4$Month, y = m4$Interest, text = "Google Brain",
               xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 50, ay = -70)
    
    a5 <- list(x = m5$Month, y = m5$Interest, text = "Hinton puslishes “A Fast Learning Algorithm for Deep Belief Nets”",
               xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 50, ay = -70)
    
    annot = list(a, a2, a3, a4, a5)
    
    if (input$overlay_ai) {
      a6 <- list(x = m6$Month, y = m6$Interest, text = "ChatGPT",
                 xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = 50, ay = -70)
      annot = list(a, a2, a3, a4, a5, a6)
    }
    
    p <- p %>% layout(annotations = annot)
    
    p
  })

  # seasonality plot
  output$seasonalPlot <- renderPlot({
    req(input$selectPlot == "Seasonality")
    if (input$seasonalityOption == "Season") {
      gg_season(neuro, Interest)
    } else if (input$seasonalityOption == "Subseries") {
      gg_subseries(neuro, Interest)
    }
  })
  # autocorrelation plot
  output$autocorrPlot <- renderPlot({
    if (input$autocorrOption == "Lag Scatter Plots") {
      neuro %>%
        gg_lag(
          Interest,
          geom = "point",
          lags = c(1, 6, 12)
        ) +
        guides(color = "none")
    } else {
      neuro %>%
        ACF(Interest, lag_max = input$acfYears * 12) %>%
        autoplot()
    }
  })
  # decomposition plot
  output$decompPlot <- renderPlot({
    req(input$selectPlot == "Decomposition")
    if (input$decompositionOption == "Classical (Additive)") {
      neuro %>%
        model(classical_decomposition(Interest)) %>%
        components() %>%
        autoplot()
    } else if (input$decompositionOption == "Classical (Multiplicative)") {
      neuro %>%
        model(classical_decomposition(Interest, type="multiplicative")) %>%
        components() %>%
        autoplot()
    } else if (input$decompositionOption == "STL") {
      neuro %>%
        model(STL(Interest ~ trend(window = 7) +
                    season(window = "periodic"), robust=TRUE)) %>%
        components() %>%
        autoplot(season_adjust)
    }
  })

  # explanation corresponding to selected plot
  output$interpretation <- renderText({
    if (input$selectPlot == "Seasonality") {
      if (input$seasonalityOption == "Season") {
        "This plot displays the data in a seasonal time series plot. Different colored lines correspond
        to different years. In this case there doesn't appear to be a significant seasonal pattern in the data.
        No month seems to repeatedly see higher or lower searches than other months. This is logical given that
        neuromorphic computing is a field of research and attracts interest year-round. We are able to see the
        3 major spikes in the data that we see in the above plot in December 2005 and August 2006 (in orange) and
        February 2024 (in pink)."
      } else if (input$seasonalityOption == "Subseries") {
        "This plot displays the time series grouped by each month. The horizontal blue lines mark the average
        Interest for each month from 2004 to 2024. Similarly to the Season plot, we do not see a significant
        seasonal pattern in the data. The average Interest is roughly uniform across months. Again, this is due
        to the nature of the search term, as neuromorphic computing is a research field attracting interest year
        round. Lastly, we again are able to see the 3 major spikes in the above plot in December 2005, August 2006,
        and February 2024."
      }
    } else if (input$selectPlot == "Decomposition") {
      if (input$decompositionOption == "Classical (Additive)") {
        "Classical Decomposition assumes the seasonal component will not change over the years, which is why the pattern
        is uniform in the seasonal plot. The trend component gives us a better view of the overall trend of the data with
        the \"bumps\" smoothed out. The random, or residual, component is what's left over after trend and seasonality are
        removed, and ideally should look as random as possible, not coinciding with patterns in the seasonal component.
        This plot displays the trend, seasonal, and random components of this series when Additive Classical
        Decomposiiton is applied. In Additive Decomposition, these components are added together to form the series.
        Additive Decomposition is best applied when the series trends linearly and the seasonal patterns are constant.
        Multiplicative Decomposition seems to be more suited to our data than Additive Decomposiiton, because the trend
        is not linear. The plot of random component verifies this assumption because it doesn't appear to be as random as
        the random component in the Multiplicative Model. The component looks nearly identical to the series itself and
        we can clearly see spikes in the series that correspond to spikes in the randomn component."
      } else if (input$decompositionOption == "Classical (Multiplicative)") {
        "Classical Decomposition assumes the seasonal component will not change over the years, which is why the pattern
        is uniform in the seasonal plot. The trend component gives us a better view of the overall trend of the data with
        the \"bumps\" smoothed out. The random, or residual, component is what's left over after trend and seasonality are
        removed, and ideally should look as random as possible, not coinciding with patterns in the seasonal component.
        This plot displays the trend, seasonal, and random components of this series when Multiplicative Classical
        Decomposiiton is applied. In Multiplicative Decomposition, these components are multiplied to form the series.
        Multiplicative Decomposition is best applied when the series trends exponentially and the seasonal patterns fluctuate
        with the series (e.g. when the series value is higher, the seasonality is more drastic). Multiplicative Decomposition
        seems to be more suited to our data than Additive Decomposiiton, because the trend is not linear. The plot of random
        component verifies this assumption because it doesn't appear to adhere to patterns in the series."
      } else if (input$decompositionOption == "STL"){
        "Unlike classical decomposition, STL decomposition allows the seasonal component to change over time, which is often
        more realistic to real-world data. It is also more robust to outliers. We can see that the spikes around 2005 and 2006
        are not noticeable in the trend plot for STL decomposition, as opposed to the trend plots for classical decomposition."
      }
    } else if (input$selectPlot == "Autocorrelation") {
      if (input$autocorrOption == "Lag Scatter Plots") {
        "These plots show the correlation for lag values of 1, 6, and 12 months. A lag of 1 is the previous month's data.
        This plot shows a positive correlation (although rather weak) because we would expect values of the previous month
        to be similar to values of the current month. We would expect a 6 month lag to have a negative correlation in seasonal
        data, but since our dataset has no significant seasonality this is not the case. We would also a 12 month lag to have
        a positive correlation, and this holds for our dataset."
      } else {
        "This plot shows the Autocorrelation Function (ACF) for 20 years of data (the entire series). We can see a sharp change
        from positive to negative correlation around the 7 year mark. The positive correlation corresponds to the years where
        interest in neuromorphic computing was increasing, because it is increasing at the current value. The negative correlation
        corresponds to years where there was little to no interest in neuromorphic computing. This plot reinforces the conclusion
        that this data has virtually no sesasonality, because the correlation is not alternating between positive and negative.
        You can adjust the number of years to display using the button above."
      }
    } else {
      "Choose a plot to see interpretation."
    }
  })
}

shinyApp(ui, server)