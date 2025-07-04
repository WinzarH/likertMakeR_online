library(shiny)
library(bslib) # easier cards panels and other layout
library(bsicons) # even more icons to not use

library(shinycssloaders) # interactive and pretty

library(matrixcalc) # to test positive definite status
library(ragg) # to save output chart
library(markdown) # display text files
library(DT) # data table rendering
library(skimr) # spark histograms
# library(psych) # pairs panels visualisation
library(GGally) # pairs plots
library(ggplot2)

library(LikertMakeR)

link_github <- tags$a(shiny::icon("github"),
  "LikertMakeR development version",
  href = "https://winzarh.github.io/LikertMakeR/",
  target = "_blank"
)
link_cran <- tags$a(shiny::icon("r-project"),
  "LikertMakeR on CRAN",
  href = "https://CRAN.R-project.org/package=LikertMakeR",
  target = "_blank"
)

ui <- page_navbar(
  theme = bs_theme(version = 5, preset = "litera"),
  ## theme options:
  ## litera
  ## journal
  ## lumen



  title = "LkertMakeR online",
  underline = TRUE,
  nav_panel(
    title = "Instructions",
    layout_sidebar(
      sidebar = sidebar(
        title = "How to use LikertMakeR online",
        div(img(
          src = "LikertMakeR_4.png",
          width = "60%", align = "center"
        ))
      ),
      # card(
      #   includeMarkdown("www/how_to.md")
      # ),

      accordion(
        # open = TRUE,
        multiple = FALSE,
        accordion_panel(
          "Purpose",
          includeMarkdown("www/purpose.md")
        ),
        accordion_panel(
          "How to use LikertMakeR online",
          includeMarkdown("www/overview.md")
        ),
        accordion_panel(
          "#1: Generate correlation matrix from Cronbach's Alpha",
          includeMarkdown("www/step_1.md")
        ),
        accordion_panel(
          "#2: Specify parameters and generate data",
          includeMarkdown("www/step_2.md")
        ),
        accordion_panel(
          "#3: Validate Results",
          includeMarkdown("www/step_3.md")
        )
      )
    )
  ),
  nav_panel(
    title = "Correlation Matrix from Cronbach's Alpha",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Matrix parameters",
        h5("Input parameters for correlation matrix"),
        numericInput("alpha", "Cronbach's Alpha",
          value = 0.85, min = -0.95, max = 1.00, step = 0.05
        ),
        numericInput("items", "Number of Items (Columns/Rows)",
          value = 5, min = 2, max = 32, step = 1
        ),
        numericInput("variance", "Variance (optional)",
          value = 0.5, min = 0, max = 2, step = 0.1
        ),
        br(), # Space before the action button
        actionButton(
          "calculate", "Generate Matrix",
          icon("calculator")
        ) |>
          tooltip("Generate Correlation Matrix!")
      ),
      card(
        tableOutput("matrix") |> withSpinner(type = 5) # ,
      ),
      card(
        layout_columns(
          fill = TRUE,
          value_box(
            title = "Success?",
            value = textOutput("matrixStatus") # ,
            # showcase = bs_icon("trophy")
          ),
          value_box(
            title = "calculated Cronbach's Alpha",
            value = textOutput("cronbachAlpha") # ,
            # showcase = bs_icon("crosshair")
          ),
          value_box(
            title = "Eigenvalues",
            value = textOutput("eigenValues") # ,
            # showcase = bs_icon("bar-chart-steps")
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Generate Synthetic Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 450,
        padding = 2,
        open = list(desktop = "open", mobile = "open"),
        title = "Data parameters",
        h5("For each variable, input the desired mean,
           standard deviation, and lower & upper boundaries"),
        numericInput("n", "Number of Observations",
          value = 64, min = 4, max = 512, step = 1
        ),
        uiOutput("dynamicInputs"),
        fluidRow(
          style = "display: flex; flex-wrap: nowrap; width: min-content;",
          actionButton(
            "generate", "Generate my data",
            icon("paper-plane"),
            style = "width:200px; margin-left:40px;
            color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ),
          # br(),
          downloadButton(
            "downloadData", "Download as CSV",
            style = "width:200px; margin-left:5px;",
            helpText("Comma-separated Values file")
          )
        )
      ), ## END sidebar
      card(
        title = "generated data",
        dataTableOutput("syntheticData") |>
          withSpinner(type = 5)
      )
    )
  ),
  nav_panel(
    title = "Data validation",
    layout_sidebar(
      sidebar = sidebar(
        width = 200,
        padding = 5,
        open = list(desktop = "open", mobile = "open"),
        title = "Visualisation Controls",
        actionButton(
          "plotData", "Update Plot",
          icon("image")
        ),
        br(),
        downloadButton("downloadPlot", "Download Plot")
      ),
      card(
        # Output for the pairs plot
        full_screen = TRUE,
        class = "border-0",
        card_header(
          "Visual Summary",
          tooltip(
            bs_icon("info-circle"),
            "Correlations of generated columns, histograms of
            each variable, and scatterplots"
          )
        ),
        card_body(
          plotOutput("dataVis")
        )
      ),
      layout_columns(
        col_widths = c(5, 4, 3),
        card( # means and standard deviations
          class = "border-0",
          min_height = 100,
          card_header(
            "Summary Moments" # ,
            # tooltip(
            #   bs_icon("info-circle"),
            #   "Means and Standard Deviations of created variables."
            # )
          ),
          card_body(
            verbatimTextOutput("dataSummary")
          )
        ),
        card( # Cronbach alpha
          min_height = 100,
          class = "border-0",
          card_header(
            "Cronbach's Alpha"
          ),
          card_body(
            verbatimTextOutput("cronbachAlphaOutput")
          )
        ),
        card( # eigenvalues
          min_height = 100,
          class = "border-0",
          card_header(
            "Eigenvalues",
            tooltip(
              bs_icon("info-circle"),
              "Eigenvalues of created variables."
            )
          ),
          card_body(
            verbatimTextOutput("eigenSummary")
          )
        ) # END eigenvalues card
      ) # end layout columns
    ) # end layout sidebar
  ), # end nav_panel
  nav_panel(
    title = "About LikertMakeR",
    layout_sidebar(
      sidebar = sidebar(
        width = 200,
        padding = 5,
        open = list(desktop = "open", mobile = "open"),
        title = "About LikertMakeR online",
        img(
          src = "LikertMakeR_4.png",
          width = "75%", align = "center"
        )
      ), # end sidebar

      accordion(
        # open = TRUE,
        multiple = FALSE,
        accordion_panel(
          "About LikertMakeR Online",
          includeMarkdown("www/about.md")
        ),
        accordion_panel(
          "Things to try",
          includeMarkdown("www/playtime.md")
        ),
        accordion_panel(
          "How to cite LikertMakeR",
          includeMarkdown("www/citation.md")
        )
      )
    ) # end layout_sidebar
  ), # end "about" nav_panel
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_github),
    nav_item(link_cran)
  ) # end links nav menu
) # end ui
###
##### END User Interface _____________________________________
###

###
##### Server logic ___________________________________________
###
server <- function(input, output, session) {
  # Reactive expression for generated matrix
  resultMatrix <- eventReactive(input$calculate, {
    if (input$calculate > 0) {
      tryCatch(
        {
          matrix <- makeCorrAlpha(
            alpha = input$alpha,
            items = input$items,
            variance = input$variance
          )
          if (!is.positive.definite(matrix)) {
            stop(
              paste("Matrix is not positive-definite."),
              paste(" Try again,"),
              paste(" or reduce the value of ‘Variance’.")
            )
          }
          return(matrix) # Return the matrix if positive definite
        },
        error = function(e) {
          return(list(error = e$message)) # Return an error as a list
        }
      )
    }
  })

  output$matrix <- renderTable({
    # Render the matrix if no error is present
    if (is.matrix(resultMatrix())) {
      return(resultMatrix())
    } else if (is.list(resultMatrix()) && !is.null(resultMatrix()$error)) {
      # Handle the case where an error has occurred
      return(data.frame(Error = resultMatrix()$error))
    }
  })

  output$matrixStatus <- renderText({
    if (is.list(resultMatrix()) && !is.null(resultMatrix()$error)) {
      resultMatrix()$error
    } else {
      "Positive-Definite Matrix"
    }
  })

  # Calculate and display Cronbach's Alpha
  output$cronbachAlpha <- renderText({
    if (is.matrix(resultMatrix())) {
      c_alpha <- LikertMakeR::alpha(resultMatrix()) |> round(4)
      return(c_alpha)
    }
  })

  # Calculate and display the eigenvalues of generated correlation matrix
  output$eigenValues <- renderText({
    if (is.matrix(resultMatrix())) {
      eigen_vals <- eigen(resultMatrix())$values |> round(2)
      # toString(round(eigen_vals, 4))
      return(eigen_vals)
    }
  })


  ## Dynamic inputs for variable parameters
  output$dynamicInputs <- renderUI({
    numItems <- input$items
    inputFields <- lapply(1:numItems, function(i) {
      fluidRow(
        column(3, numericInput(paste0("mean", i),
          label = paste0("Mean v", i), value = 3
        )),
        column(3, numericInput(paste0("sd", i),
          label = paste0("SD v", i), value = 1
        )),
        column(3, numericInput(paste0("lower", i),
          label = paste0("Lower v", i), value = 1
        )),
        column(3, numericInput(paste0("upper", i),
          label = paste0("Upper v", i), value = 5
        ))
      )
    })
    do.call(tagList, inputFields)
  })

  # bounds for pairs plot
  bounds <- reactiveValues(lower = NULL, upper = NULL)

  # Synthetic data generation logic with dynamic inputs
  syntheticData <- eventReactive(input$generate, {
    req(resultMatrix()) # Check 'resultMatrix' has been calculated
    # Check if 'resultMatrix' contains an error
    if (is.list(resultMatrix()) && !is.null(resultMatrix()$error)) {
      return(data.frame(Error = resultMatrix()$error))
    }

    matrix <- resultMatrix()
    n <- input$n
    numItems <- input$items
    means <- sapply(1:numItems, function(i) input[[paste0("mean", i)]])
    sds <- sapply(1:numItems, function(i) input[[paste0("sd", i)]])
    lowerbounds <- sapply(1:numItems, function(i) input[[paste0("lower", i)]])
    upperbounds <- sapply(1:numItems, function(i) input[[paste0("upper", i)]])

    bounds$lower <- lowerbounds
    bounds$upper <- upperbounds


    # Validation passed, proceed to generate data
    tryCatch(
      {
        data <- makeItems(n, means, sds, lowerbounds, upperbounds, matrix)
        colnames(data) <- paste0("v_", seq_len(ncol(data)))
        return(data)
      },
      error = function(e) {
        return(data.frame(Error = e$message))
      }
    )
  })


  output$syntheticData <- renderDT({
    syntheticData()
  })

  ## Visualise the data and correlations
  # A reactive expression to store plot data only when 'Generate Plot' is clicked
  plotData <- eventReactive(input$plotData,
    {
      req(syntheticData()) # Ensure there are data to plot
      syntheticData() # Holds the data to be used for plotting
    },
    ignoreNULL = FALSE
  )

  # Calculate and display the eigenvalues of generated dataframe
  output$eigenSummary <- renderText({
    cor_dat <- syntheticData() |>
      cor() |>
      as.matrix()
    eigen_vals <- eigen(cor_dat)$values |> round(2)
    return(eigen_vals)
  })


  # generatePairsPlot <- function(data) {
  #   pairs.panels(data,
  #     lm = TRUE,
  #     col = "firebrick", # or use "#8A0707" for a darker blood red
  #     lwd = 7,
  #     # line.col = "red",
  #     cex.cor = 0.5,
  #     jiggle = TRUE,
  #     ellipses = FALSE,
  #     density = TRUE,
  #     smoother = TRUE
  #   )
  # }

  custom_smooth <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
      geom_point(
        alpha = 0.6,
        color = "steelblue",
        position = position_jitter(width = 0.2, height = 0.2),
        size = 1.5
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "firebrick",
        size = 1.2
      )
  }

  cor_only <- function(data, mapping, method = "pearson", digits = 2, ...) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    corr <- cor(x, y, method = method, use = "pairwise.complete.obs")
    label <- formatC(corr, format = "f", digits = digits)

    ggally_text(
      label = label,
      mapping = aes(),
      xP = 0.5, yP = 0.5,
      size = 6, ...
    ) +
      theme_void()
  }

  custom_bar_diag <- function(data, mapping, ...) {
    varname <- as_label(mapping$x)
    idx <- as.integer(gsub("v_", "", varname)) # assumes colnames are v_1, v_2, etc.

    bin_count <- bounds$upper[idx] - bounds$lower[idx] + 1

    ggplot(data, mapping) +
      geom_histogram(
        bins = bin_count,
        fill = "steelblue",
        color = "white"
      )
  }


  generatePairsPlot <- function(data) {
    ggpairs(
      data,
      lower = list(continuous = custom_smooth),
      diag = list(continuous = custom_bar_diag),
      upper = list(continuous = wrap(cor_only, digits = 2))
    ) +
      theme_minimal(base_size = 14) + # changes default sizes
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.line = element_line(color = "black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      )
  }

  # Calculate and display means and standard deviations
  output$dataSummary <- renderPrint({
    req(plotData()) # Ensure the plot data is ready
    data <- plotData()

    myMoments <- data.frame(
      mean = apply(data, 2, mean, na.rm = TRUE) |> round(3),
      sd = apply(data, 2, sd, na.rm = TRUE) |> round(3)
    ) |> t()
    # print("Summary Moments")
    print(myMoments)
  })

  # Calculate and display Cronbach's Alpha
  output$cronbachAlphaOutput <- renderText({
    req(syntheticData()) # Make sure syntheticData is available
    data <- syntheticData()
    cr_alpha <- LikertMakeR::alpha(NULL, data) |> round(4)
    return(cr_alpha)
  })

  # Render the plot in the UI when the 'Generate Plot' button is clicked
  output$dataVis <- renderPlot({
    req(plotData()) # Ensure the plot data is ready
    generatePairsPlot(plotData()) # Generate and display the plot
  })
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("LikertMakeR_corr_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(plotData()) # Ensure the plot is ready
      ragg::agg_png(file,
        width = input$items * 200,
        height = input$items * 200,
        res = 144
      )
      print(generatePairsPlot(plotData()))
      dev.off()
    }
  )

  # Add download handler for downloading the synthetic data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("likertmaker-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- syntheticData()
      write.csv(data, file, row.names = FALSE)
    }
  )
}

## Run the application
shinyApp(ui = ui, server = server)

# run_with_themer(shinyApp(ui = ui, server = server))
