library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(infer)
library(cowplot)
library(readr)
library(tidyr)

ril_link <- "https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/clarkia_rils.csv"
ril_data <- readr::read_csv(ril_link) |>
  dplyr::mutate(growth_rate = case_when(growth_rate == "1.8O" ~ "1.80",
                                      .default = growth_rate),
                growth_rate = as.numeric(growth_rate),
                visited = mean_visits > 0)
gc_rils <- ril_data |>
  filter(location == "GC", !is.na(prop_hybrid), !is.na(mean_visits)) |>
  mutate(pink_flowers = as.numeric(petal_color == "pink")) |>
  select(petal_area_mm, pink_flowers, mean_visits, prop_hybrid)


# --- UI Definition ---
ui <- fluidPage(
    titlePanel("The Central Limit Theorem in Action"),
    sidebarLayout(
        sidebarPanel(
            selectInput("var", "Population Distribution:",
                        choices = c("Petal Area" = "petal_area_mm",
                                    "Prop. Hybrid" = "prop_hybrid",
                                    "Mean Visits" = "mean_visits",
                                    "Pink Flowers" = "pink_flowers")),
            selectInput("n", "Sample Size (n):",
                        choices = c("2", "5", "10", "25", "50", "100")),
            hr(),
            helpText("We take 1000 random samples, each of the specified size, from the chosen population distribution. We then calculate the mean for each sample.")
        ),
        mainPanel(
            plotOutput("distPlot", height = "600px")
        )
    )
)

# --- Server Logic ---
server <- function(input, output) {

    # Reactive expression to get the selected population data
    population_dist <- reactive({
        req(input$var)
        tibble(x = gc_rils[[input$var]]) %>% drop_na()
    })

    # Reactive expression for the sampling distribution
    sampling_dist <- reactive({
        req(population_dist(), input$n)
        n_reps <- 1000
        pop_data <- population_dist()
        
        # Using a loop for clarity, equivalent to rep_sample_n
        means <- replicate(n_reps, {
            sample_data <- sample(pop_data$x, size = as.numeric(input$n), replace = TRUE)
            mean(sample_data)
        })
        tibble(mean_x = means)
    })

    output$distPlot <- renderPlot({
        pop_data <- population_dist()
        samp_dist <- sampling_dist()
        pop_mean <- mean(pop_data$x)

        # --- Plots for Population Distribution (Top Row) ---
        pop_hist <- ggplot(pop_data, aes(x = x)) +
            geom_histogram(bins = 30, color = "white", fill = "pink") +
            labs(x = "Observed Values", y = "Count", title = "Actual Data") +
            theme_minimal(base_size = 14)+
          theme(title = element_text(color = "pink"))

        pop_qq <- ggplot(pop_data, aes(sample = x)) +
            geom_qq(color = "pink") +
            geom_qq_line(color = "pink") +
            labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Actual Data") +
            theme_minimal(base_size = 14)+
          theme(title = element_text(color = "pink"))

        # --- Plots for Sampling Distribution (Bottom Row) ---
        samp_hist <- ggplot(samp_dist, aes(x = mean_x)) +
            geom_histogram(bins = 30, color = "white", fill = "#3b82f6") +
            labs(x = "Sample Means", y = "Count",title = "Simulated Sampling Dist.") +
            theme_minimal(base_size = 14)+
          theme(title = element_text(color = "#3b82f6"))

        samp_qq <- ggplot(samp_dist, aes(sample = mean_x)) +
            geom_qq(color = "#3b82f6") +
            geom_qq_line(color = "#3b82f6") +
            labs(x = "Theoretical Quantiles", y = "Sample Quantiles",title = "Simulated Sampling Dist.") +
            theme_minimal(base_size = 14)+
          theme(title = element_text(color = "#3b82f6"))

        # --- Assemble the Grid with cowplot ---
        
        # Column and Row Labels
        col1_label <- ggdraw() + draw_label("Histogram", fontface = 'bold', size = 16)
        col2_label <- ggdraw() + draw_label("QQ Plot", fontface = 'bold', size = 16)
        row1_label <- ggdraw() + draw_label("", angle = 270, fontface = 'bold', size = 16)
        row2_label <- ggdraw() + draw_label("", angle = 270, fontface = 'bold', size = 16)

        # Main title
        main_title <- ggdraw() +
            draw_label(sprintf("Population: %s | Sample Size n = %s", input$var, input$n),
                       fontface = 'bold', size = 18, x = 0, hjust = 0) +
            theme(plot.margin = margin(0, 0, 0, 7))

        # Arrange plots
        plot_row1 <- plot_grid(pop_hist, pop_qq, ncol = 2)
        plot_row2 <- plot_grid(samp_hist, samp_qq, ncol = 2)
        
        # Add row labels
        labeled_row1 <- plot_grid(plot_row1, row1_label, ncol = 2, rel_widths = c(1, 0.05))
        labeled_row2 <- plot_grid(plot_row2, row2_label, ncol = 2, rel_widths = c(1, 0.05))

        # Add column labels
        top_row <- plot_grid(col1_label, col2_label, ncol = 2)
        
        # Combine everything
        plot_grid(main_title, top_row, labeled_row1, labeled_row2, ncol = 1,
                  rel_heights = c(0.1, 0.1, 1, 1))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
