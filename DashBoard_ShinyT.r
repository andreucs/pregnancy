
################################################################################
# Pregnancy Analytical Dashboard – app.R 
################################################################################
#  ▸ Load CSV/XLSX – select target variable and numeric predictors
#  ▸ Data preprocessing with interactive imputation (MICE/KNN)
#  ▸ EDA with progress, configurable and colorable UMAP/t-SNE
#  ▸ PCA + Generic GAM with significant smooths
#  ▸ PLS (Partial Least Squares) analysis with visualizations
#  ▸ Random Forest with hyperparameter tuning
#  ▸ BKMR (Bayesian Kernel Machine Regression) for mixture analysis
################################################################################
# ----------------  PACKAGES  --------------------------------------------------
# Load required packages for data analysis, visualization, and modeling
pkgs <- c("shiny","bslib","thematic","shinyWidgets","shinyalert","shinyFeedback",
          "DT","plotly","ggplot2","data.table","dplyr","tidyr","reshape2",
          "readxl","FactoMineR","mgcv","caTools","Metrics","umap","scales",
          "bkmr","factoextra","corrplot","pdp","DALEX","ingredients","pls", "rsconnect",
          "mice", "VIM", "missForest", "Rtsne", "randomForest", "caret", "e1071",
          "car", "MASS", "bestNormalize", "outliers", "robustbase")
# Check if any packages are missing and stop if they need to be installed
need <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(need)) stop("Install missing packages: ", paste(need,collapse=", "))
suppressPackageStartupMessages(lapply(pkgs, require, character.only=TRUE))
thematic::thematic_shiny()  # Apply theming to Shiny plots

# ----------------  UI  --------------------------------------------------------
ui <- fluidPage(
  # Set up the theme with Flatly Bootswatch theme and Lato font
  theme = bs_theme(bootswatch="flatly", base_font=font_google("Lato"),
                   primary="#2c3e50"),
  useShinyalert(), useShinyFeedback(),  # Enable alert and feedback components
  
  # Dark/Light mode toggle
  div(style = "position: absolute; top: 10px; right: 10px;",
      materialSwitch(
        inputId = "dark_mode",
        label = "Dark Mode",
        status = "primary",
        right = TRUE
      )
  ),
  
  # Application title with icon
  titlePanel(div(icon("chart-line"), span("Pregnancy Analytical Dashboard",
                                          style="font-weight:bold"))),
  
  # Main layout with sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # File input for data upload (CSV or Excel)
      fileInput("datafile", "CSV or XLSX", accept=c(".csv",".xlsx")),
      
      # Dynamic UI elements for selecting target and predictor variables
      uiOutput("target_ui"),
      uiOutput("vars_ui"),
      
      # Data preprocessing section
      h4("📊 Data Preprocessing", style="margin-top:15px"),
      tabsetPanel(
        tabPanel("Imputation",
                 sliderInput("na_threshold", "Missing data threshold (%)", 0, 100, 50),
                 selectInput("imputation_method", "Imputation method",
                             choices = c("MICE" = "mice", 
                                         "KNN" = "knn", 
                                         "Random Forest" = "rf",
                                         "None" = "none")),
                 conditionalPanel(
                   condition = "input.imputation_method == 'mice'",
                   numericInput("mice_m", "Number of imputations", 5, 1, 20),
                   numericInput("mice_maxit", "Max iterations", 5, 1, 50)
                 ),
                 conditionalPanel(
                   condition = "input.imputation_method == 'knn'",
                   numericInput("knn_k", "Number of neighbors", 5, 1, 20)
                 ),
                 actionButton("run_imputation", "Run Imputation", icon=icon("magic"),
                              class="btn-info")
        ),
        tabPanel("Transformations",
                 selectInput("transform_method", "Transformation method",
                             choices = c("None" = "none",
                                         "Log" = "log",
                                         "Square Root" = "sqrt",
                                         "Box-Cox" = "boxcox",
                                         "Yeo-Johnson" = "yeojohnson")),
                 checkboxInput("scale_vars", "Standardize selection", TRUE),
                 selectInput("scaling_method", "Scaling method",
                             choices = c("Standard (Z-score)" = "standard",
                                         "Robust (MAD)" = "robust",
                                         "Min-Max" = "minmax")),
                 actionButton("run_transform", "Apply Transformations", icon=icon("exchange-alt"),
                              class="btn-info")
        ),
        tabPanel("Outliers",
                 selectInput("outlier_method", "Detection method",
                             choices = c("Z-score" = "zscore",
                                         "Tukey's fences" = "tukey",
                                         "MAD" = "mad")),
                 sliderInput("outlier_threshold", "Threshold", 1.5, 5, 3, step=0.1),
                 checkboxInput("remove_outliers", "Remove outliers", FALSE),
                 actionButton("detect_outliers", "Detect Outliers", icon=icon("search"),
                              class="btn-info")
        )
      ),
      
      hr(),
      numericInput("n_components", "PCA Components (max)", 10, 2, 30),
      hr(),
      
      # Dimensionality reduction parameters
      h5("⚙️ Dimensionality Reduction Parameters"),
      tabsetPanel(
        tabPanel("UMAP",
                 sliderInput("umap_k", "n_neighbors", 5, 50, 15),
                 sliderInput("umap_md", "min_dist", 0.001, 1, 0.1, step=0.01)
        ),
        tabPanel("t-SNE",
                 sliderInput("tsne_perplexity", "Perplexity", 5, 50, 30),
                 sliderInput("tsne_theta", "Theta", 0.1, 1, 0.5, step=0.1),
                 numericInput("tsne_dims", "Dimensions", 2, 2, 3),
                 numericInput("tsne_max_iter", "Max iterations", 1000, 100, 5000)
        )
      ),
      
      uiOutput("color_by_ui"),
      checkboxInput("cluster_dr", "Automatic clustering (k‑means)", FALSE),
      conditionalPanel("input.cluster_dr==true",
                       numericInput("k_clusters", "k (clusters)", 4, 2, 10)
      ),
      hr(),
      
      # Button to run EDA analysis
      actionButton("analyze_basic", "Analyze EDA", icon=icon("search"),
                   class="btn-primary"),
      hr(),
      
      # Button to run PCA + GAM analysis
      h5("⚙️ GAM Parameters"),
      selectInput("gam_family", "Distribution family", 
                  choices = c("Gaussian" = "gaussian",
                              "Poisson" = "poisson",
                              "Binomial" = "binomial",
                              "Gamma" = "Gamma")),
      checkboxInput("gam_cv", "Cross-validation", TRUE),
      conditionalPanel("input.gam_cv==true",
                       numericInput("gam_cv_folds", "CV folds", 5, 3, 10)),
      actionButton("run_pcagam", "Run PCA + GAM", icon=icon("play"),
                   class="btn-success"),
      hr(),
      
      # PLS configuration parameters
      h5("⚙️ PLS Parameters"),
      numericInput("pls_ncomp", "Number of Components", 3, 1, 10),
      checkboxInput("pls_cv", "Cross-validation", TRUE),
      conditionalPanel("input.pls_cv==true",
                       numericInput("pls_cv_folds", "CV folds", 5, 3, 10)),
      actionButton("run_pls", "Run PLS", icon=icon("chart-bar"),
                   class="btn-info"),
      hr(),
      
      # Random Forest configuration parameters
      h5("⚙️ Random Forest Parameters"),
      numericInput("rf_ntree", "Number of trees", 500, 100, 2000),
      selectInput("rf_tune_method", "Tuning method",
                  choices = c("None" = "none",
                              "Grid Search" = "grid",
                              "Random Search" = "random")),
      conditionalPanel("input.rf_tune_method != 'none'",
                       numericInput("rf_cv_folds", "CV folds", 5, 3, 10)),
      actionButton("run_rf", "Run Random Forest", icon=icon("tree"),
                   class="btn-success"),
      hr(),
      
      # BKMR configuration parameters
      h5("⚙️ BKMR Parameters"),
      numericInput("bkmr_iter", "Iterations", 1000, 100, 10000),
      numericInput("bkmr_burn", "Burn-in", 500, 50, 5000),
      numericInput("bkmr_thin", "Thin", 2, 1, 10),
      actionButton("run_bkmr", "Run BKMR", icon=icon("dna"),
                   class="btn-warning")
    ),
    
    mainPanel(
      # Tab layout for different analyses
      tabsetPanel(id="tabs", type="tabs",
                  # Data preprocessing tab
                  tabPanel("🔍 Preprocessing", 
                           h4("Data Overview"),
                           DTOutput("data_preview"),
                           h4("Missing Data Visualization"),
                           plotOutput("missing_plot", height=400),
                           h4("Outlier Detection"),
                           plotOutput("outlier_plot", height=400),
                           h4("Transformation Results"),
                           plotOutput("transform_plot", height=400)),
                  
                  # Summary statistics and density plots
                  tabPanel("📊 Summary", DTOutput("summary"), br(), plotlyOutput("dens")),
                  
                  # Correlation matrix visualization
                  tabPanel("📈 Correlation", plotlyOutput("corr", height=500)),
                  
                  # Boxplots for each variable
                  tabPanel("📦 Boxplots", plotlyOutput("box", height=500)),
                  
                  # PCA analysis tab with multiple visualizations
                  tabPanel("🧮 PCA", uiOutput("pca_ui")),
                  
                  # Dimensionality reduction visualizations
                  tabPanel("🧭 Dimensionality Reduction", 
                           tabsetPanel(
                             tabPanel("UMAP", plotlyOutput("umap", height=500)),
                             tabPanel("t-SNE", plotlyOutput("tsne", height=500))
                           )),
                  
                  # PCA + GAM analysis with model summary and diagnostics
                  tabPanel("📎 PCA + GAM",
                           verbatimTextOutput("gam_summary"), br(),
                           plotlyOutput("pcagam_scatter", height=500), br(),
                           DTOutput("gam_metrics"), br(),
                           plotOutput("gam_smooths", height = "600px"), br(),
                           plotOutput("gam_ice_plots", height = "600px"), br(),
                           plotOutput("gam_qq", height = "400px"), br(),
                           plotOutput("gam_residuals", height = "400px"), br(),
                           plotOutput("gam_hist_residuals", height = "400px")
                  ),
                  
                  # PLS analysis tab with model results and visualizations
                  tabPanel("📊 PLS",
                           h4("Partial Least Squares Analysis"),
                           verbatimTextOutput("pls_summary"),
                           plotOutput("pls_scores", height = "400px"),
                           plotOutput("pls_loadings", height = "400px"),
                           plotOutput("pls_variance", height = "400px"),
                           plotOutput("pls_biplot", height = "400px"),
                           plotOutput("pls_vip", height = "400px"),
                           plotOutput("pls_predictions", height = "400px")
                  ),
                  
                  # Random Forest analysis tab
                  tabPanel("🌲 Random Forest",
                           h4("Random Forest Analysis"),
                           verbatimTextOutput("rf_summary"),
                           plotOutput("rf_importance", height = "400px"),
                           plotOutput("rf_predictions", height = "400px"),
                           conditionalPanel("input.rf_tune_method != 'none'",
                                           h4("Hyperparameter Tuning Results"),
                                           plotOutput("rf_tuning", height = "400px"),
                                           verbatimTextOutput("rf_best_params"))
                  ),
                  
                  # BKMR analysis tab with model results and visualizations
                  tabPanel("🧬 BKMR",
                           h4("Bayesian Kernel Machine Regression"),
                           verbatimTextOutput("bkmr_summary"),
                           plotOutput("bkmr_pips", height = "400px"),
                           plotOutput("bkmr_univar", height = "400px"),
                           plotOutput("bkmr_bivar", height = "400px")
                  )
      )
    )
  )
)
# ----------------  SERVER  ----------------------------------------------------
server <- function(input, output, session){
  
  # ---------- Theme switching -------------------------------------------------
  observeEvent(input$dark_mode, {
    if(input$dark_mode) {
      session$setCurrentTheme(
        bs_theme(bootswatch = "darkly", base_font = font_google("Lato"),
                 primary = "#375a7f")
      )
    } else {
      session$setCurrentTheme(
        bs_theme(bootswatch = "flatly", base_font = font_google("Lato"),
                 primary = "#2c3e50")
      )
    }
  })
  
  # ---------- loading ---------------------------------------------------------
  # Reactive function to load data from CSV or Excel file
  data_raw <- reactive({
    req(input$datafile)  # Require file input before proceeding
    ext <- tolower(tools::file_ext(input$datafile$name))
    if (ext=="csv") fread(input$datafile$datapath)  # Fast read for CSV
    else             readxl::read_excel(input$datafile$datapath)  # Read Excel
  })
  
  # ---------- Data preprocessing ----------------------------------------------
  # Store processed data
  processed_data <- reactiveVal(NULL)
  
  # Preview of the data
  output$data_preview <- renderDT({
    req(data_raw())
    datatable(head(data_raw(), 100), options = list(scrollX = TRUE))
  })
  
  # Missing data visualization
  output$missing_plot <- renderPlot({
    req(data_raw())
    VIM::aggr(data_raw(), col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
              labels=names(data_raw()), cex.axis=.7, gap=3, 
              ylab=c("Histogram of missing data","Pattern"))
  })
  
  # Run imputation
  observeEvent(input$run_imputation, {
    withProgress(message = "Imputing missing values", value = 0, {
      req(data_raw())
      df <- as.data.frame(data_raw())
      
      # Calculate missing percentage for each column
      na_percent <- colMeans(is.na(df)) * 100
      
      # Filter columns based on threshold
      cols_to_impute <- names(na_percent[na_percent > 0 & na_percent < input$na_threshold])
      
      if(length(cols_to_impute) == 0) {
        shinyalert("No columns to impute", "No columns meet the imputation criteria")
        processed_data(df)
        return()
      }
      
      # Perform imputation based on selected method
      imputed_df <- df
      
      if(input$imputation_method == "mice") {
        incProgress(0.3, "Running MICE imputation")
        # Only impute numeric columns
        numeric_cols <- names(select_if(df[, cols_to_impute, drop=FALSE], is.numeric))
        if(length(numeric_cols) > 0) {
          mice_data <- mice(df[, numeric_cols, drop=FALSE], m=input$mice_m, maxit=input$mice_maxit, printFlag=FALSE)
          imputed_df[, numeric_cols] <- complete(mice_data)
        }
      } else if(input$imputation_method == "knn") {
        incProgress(0.3, "Running KNN imputation")
        # Only impute numeric columns
        numeric_cols <- names(select_if(df[, cols_to_impute, drop=FALSE], is.numeric))
        if(length(numeric_cols) > 0) {
          imputed_values <- VIM::kNN(df[, numeric_cols, drop=FALSE], k=input$knn_k)
          imputed_df[, numeric_cols] <- imputed_values[, numeric_cols]
        }
      } else if(input$imputation_method == "rf") {
        incProgress(0.3, "Running Random Forest imputation")
        # Only impute numeric columns
        numeric_cols <- names(select_if(df[, cols_to_impute, drop=FALSE], is.numeric))
        if(length(numeric_cols) > 0) {
          imputed_values <- missForest(df[, numeric_cols, drop=FALSE])
          imputed_df[, numeric_cols] <- imputed_values$ximp
        }
      }
      
      processed_data(imputed_df)
      incProgress(1)
      shinyalert("Imputation complete", "Missing values have been imputed")
    })
  })
  
  # Outlier detection
  observeEvent(input$detect_outliers, {
    withProgress(message = "Detecting outliers", value = 0, {
      req(processed_data() || data_raw())
      df <- processed_data() %||% as.data.frame(data_raw())
      
      # Get numeric columns
      numeric_cols <- names(select_if(df, is.numeric))
      
      if(length(numeric_cols) == 0) {
        shinyalert("No numeric columns", "No numeric columns found for outlier detection")
        return()
      }
      
      # Create a list to store outlier indices for each column
      outliers_list <- list()
      
      # Detect outliers based on selected method
      for(col in numeric_cols) {
        x <- df[[col]]
        if(all(is.na(x))) next
        
        if(input$outlier_method == "zscore") {
          z_scores <- scale(x)
          outliers_list[[col]] <- which(abs(z_scores) > input$outlier_threshold)
        } else if(input$outlier_method == "tukey") {
          q1 <- quantile(x, 0.25, na.rm = TRUE)
          q3 <- quantile(x, 0.75, na.rm = TRUE)
          iqr <- q3 - q1
          lower_bound <- q1 - input$outlier_threshold * iqr
          upper_bound <- q3 + input$outlier_threshold * iqr
          outliers_list[[col]] <- which(x < lower_bound | x > upper_bound)
        } else if(input$outlier_method == "mad") {
          med <- median(x, na.rm = TRUE)
          mad_val <- mad(x, na.rm = TRUE)
          outliers_list[[col]] <- which(abs(x - med) / mad_val > input$outlier_threshold)
        }
      }
      
      # Visualize outliers
      output$outlier_plot <- renderPlot({
        par(mfrow = c(min(3, length(numeric_cols)), 2), mar = c(4, 4, 2, 1))
        
        for(col in numeric_cols[1:min(6, length(numeric_cols))]) {
          x <- df[[col]]
          if(all(is.na(x))) next
          
          # Boxplot
          boxplot(x, main = paste("Boxplot of", col), col = "lightblue")
          
          # Histogram with density
          hist(x, main = paste("Histogram of", col), probability = TRUE, 
               col = "lightblue", border = "white")
          lines(density(x, na.rm = TRUE), col = "red", lwd = 2)
          
          # Mark outliers if any
          if(length(outliers_list[[col]]) > 0) {
            points(rep(1, length(outliers_list[[col]])), x[outliers_list[[col]]], 
                   col = "red", pch = 19)
          }
        }
      })
      
      # Remove outliers if requested
      if(input$remove_outliers) {
        # Create a copy of the data
        clean_df <- df
        
        # Replace outliers with NA
        for(col in names(outliers_list)) {
          if(length(outliers_list[[col]]) > 0) {
            clean_df[outliers_list[[col]], col] <- NA
          }
        }
        
        processed_data(clean_df)
        shinyalert("Outliers removed", "Outliers have been replaced with NA values")
      }
      
      incProgress(1)
    })
  })
  
  # Apply transformations
  observeEvent(input$run_transform, {
    withProgress(message = "Applying transformations", value = 0, {
      req(processed_data() || data_raw())
      df <- processed_data() %||% as.data.frame(data_raw())
      
      # Get numeric columns
      numeric_cols <- names(select_if(df, is.numeric))
      
      if(length(numeric_cols) == 0) {
        shinyalert("No numeric columns", "No numeric columns found for transformation")
        return()
      }
      
      # Create a copy of the data
      transformed_df <- df
      
      # Store transformation results for visualization
      transform_results <- list()
      
      # Apply transformations
      for(col in numeric_cols) {
        x <- df[[col]]
        if(all(is.na(x))) next
        
        # Store original data
        transform_results[[col]] <- list(original = x)
        
        # Apply selected transformation
        if(input$transform_method == "log") {
          # Handle negative values
          if(min(x, na.rm = TRUE) <= 0) {
            offset <- abs(min(x, na.rm = TRUE)) + 1
            transformed_df[[col]] <- log(x + offset)
            transform_results[[col]]$transformed <- log(x + offset)
            transform_results[[col]]$name <- paste("log(", col, " + ", round(offset, 2), ")", sep="")
          } else {
            transformed_df[[col]] <- log(x)
            transform_results[[col]]$transformed <- log(x)
            transform_results[[col]]$name <- paste("log(", col, ")", sep="")
          }
        } else if(input$transform_method == "sqrt") {
          # Handle negative values
          if(min(x, na.rm = TRUE) < 0) {
            offset <- abs(min(x, na.rm = TRUE)) + 1
            transformed_df[[col]] <- sqrt(x + offset)
            transform_results[[col]]$transformed <- sqrt(x + offset)
            transform_results[[col]]$name <- paste("sqrt(", col, " + ", round(offset, 2), ")", sep="")
          } else {
            transformed_df[[col]] <- sqrt(x)
            transform_results[[col]]$transformed <- sqrt(x)
            transform_results[[col]]$name <- paste("sqrt(", col, ")", sep="")
          }
        } else if(input$transform_method == "boxcox") {
          # Box-Cox requires positive values
          if(min(x, na.rm = TRUE) <= 0) {
            offset <- abs(min(x, na.rm = TRUE)) + 1
            x_pos <- x + offset
            bc <- MASS::boxcox(x_pos ~ 1, plotit = FALSE)
            lambda <- bc$x[which.max(bc$y)]
            transformed_df[[col]] <- (x_pos^lambda - 1)/lambda
            transform_results[[col]]$transformed <- (x_pos^lambda - 1)/lambda
            transform_results[[col]]$name <- paste("boxcox(", col, ", lambda=", round(lambda, 2), ")", sep="")
          } else {
            bc <- MASS::boxcox(x ~ 1, plotit = FALSE)
            lambda <- bc$x[which.max(bc$y)]
            transformed_df[[col]] <- (x^lambda - 1)/lambda
            transform_results[[col]]$transformed <- (x^lambda - 1)/lambda
            transform_results[[col]]$name <- paste("boxcox(", col, ", lambda=", round(lambda, 2), ")", sep="")
          }
        } else if(input$transform_method == "yeojohnson") {
          # Yeo-Johnson works with any values
          yj <- bestNormalize::yeojohnson(x)
          transformed_df[[col]] <- yj$x.t
          transform_results[[col]]$transformed <- yj$x.t
          transform_results[[col]]$name <- paste("yeojohnson(", col, ", lambda=", round(yj$lambda, 2), ")", sep="")
        }
      }
      
      # Visualize transformations
      output$transform_plot <- renderPlot({
        par(mfrow = c(min(3, length(transform_results)), 2), mar = c(4, 4, 2, 1))
        
        for(col in names(transform_results)[1:min(6, length(transform_results))]) {
          if(input$transform_method != "none") {
            # Original data histogram
            hist(transform_results[[col]]$original, main = paste("Original:", col),
                 probability = TRUE, col = "lightblue", border = "white")
            lines(density(transform_results[[col]]$original, na.rm = TRUE), col = "red", lwd = 2)
            
            # Transformed data histogram
            hist(transform_results[[col]]$transformed, main = paste("Transformed:", transform_results[[col]]$name),
                 probability = TRUE, col = "lightgreen", border = "white")
            lines(density(transform_results[[col]]$transformed, na.rm = TRUE), col = "red", lwd = 2)
          }
        }
      })
      
      processed_data(transformed_df)
      incProgress(1)
      shinyalert("Transformation complete", "Data has been transformed")
    })
  })
  
  # Get the current working dataset (processed or raw)
  current_data <- reactive({
    if(!is.null(processed_data())) {
      return(processed_data())
    } else {
      return(as.data.frame(data_raw()))
    }
  })
  
  # ---------- selectors -----------------------------------------------------
  # Dynamic UI for selecting target variable
  output$target_ui <- renderUI({
    req(current_data()); selectInput("target","Target variable", names(current_data()))
  })
  
  # Dynamic UI for selecting numeric predictor variables
  output$vars_ui <- renderUI({
    req(current_data(), input$target)
    nums <- names(select_if(current_data(), is.numeric)); nums <- setdiff(nums, input$target)
    checkboxGroupInput("vars","Numeric variables (predictors)", nums, nums)
  })
  
  # UI for selecting variable to color plots by
  output$color_by_ui <- renderUI({
    req(current_data())
    vars <- names(current_data())
    selectInput("color_by","Color by…", choices = c("None", vars), selected = "None")
  })
  
  # ---------- helpers -------------------------------------------------------
  # Function to scale data if requested by user
  safe_scale <- function(x) {
    if (!input$scale_vars) return(as.matrix(x))
    
    if (input$scaling_method == "standard") {
      return(scale(x))
    } else if (input$scaling_method == "robust") {
      return(apply(x, 2, function(col) {
        (col - median(col, na.rm = TRUE)) / mad(col, na.rm = TRUE)
      }))
    } else if (input$scaling_method == "minmax") {
      return(apply(x, 2, function(col) {
        (col - min(col, na.rm = TRUE)) / (max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
      }))
    }
  }
  
  # ----------------  EDA -----------------------------------------------------
  # Event handler for EDA analysis button
  observeEvent(input$analyze_basic,{
    withProgress(message="EDA", value=0,{  # Progress indicator
      incProgress(0.05,"preprocessing")
      df <- current_data()
      req(length(input$vars)>0)  # Require at least one predictor
      X  <- safe_scale(df[ , input$vars, drop=FALSE])
      
      # Generate summary statistics table
      incProgress(0.10,"summary table")
      output$summary <- renderDT({
        info <- data.frame(
          Variable = input$vars,
          Mean = sapply(df[,input$vars], mean, na.rm=TRUE),
          StdDev  = sapply(df[,input$vars], sd,   na.rm=TRUE),
          `%NA` = percent(colSums(is.na(df[,input$vars]))/nrow(df))
        )
        datatable(info, options=list(dom='t'))  # Compact table display
      })
      
      # Create density plots for each variable
      incProgress(0.25,"densities")
      output$dens <- renderPlotly({
        ggplotly(
          ggplot(pivot_longer(as.data.frame(X), everything()),
                 aes(value, fill=name))+
            geom_density(alpha=.4)+
            facet_wrap(~name, scales="free_y")
        )
      })
      
      # Create correlation heatmap
      incProgress(0.35,"correlation")
      output$corr <- renderPlotly({
        M <- cor(X, use="pairwise"); dfc <- as.data.frame(as.table(M))
        ggplotly(ggplot(dfc,aes(Var1,Var2,fill=Freq))+geom_tile()+
                   scale_fill_gradient2())
      })
      
      # Create boxplots for each variable
      incProgress(0.45,"boxplots")
      output$box <- renderPlotly({
        ggplotly(
          ggplot(pivot_longer(as.data.frame(X), everything()),
                 aes(name,value))+geom_boxplot()+
            theme(axis.text.x=element_text(angle=45,hjust=1))
        )
      })
      
      # Perform PCA and create visualizations
      incProgress(0.55,"PCA")
      pc <- prcomp(X, scale.=FALSE)  # PCA without additional scaling
      output$pca_ui <- renderUI({
        tagList(
          fluidRow(
            column(6, plotlyOutput("pca_scatter", height=400)),  # PCA scatter plot
            column(6, plotlyOutput("pca_loadings", height=400))  # PCA loadings
          ),
          fluidRow(
            column(6, plotlyOutput("pca_scree", height=300)),    # Scree plot
            column(6, plotlyOutput("pca_biplot", height=300))    # Biplot
          ),
          fluidRow(
            column(12, plotlyOutput("pca_contrib", height=300))  # Variable contributions
          )
        )
      })
      
      # PCA scatter plot with optional coloring
      output$pca_scatter <- renderPlotly({
        dfp <- as.data.frame(pc$x[,1:2]); names(dfp)<-c("PC1","PC2")
        
        # Add color if selected
        if (input$color_by != "None") {
          dfp$color <- df[[input$color_by]]
          p <- ggplot(dfp, aes(PC1, PC2, color=color)) + 
            geom_point(alpha=0.7) +
            labs(title="PCA - Scatter Plot", color=input$color_by)
        } else {
          p <- ggplot(dfp, aes(PC1, PC2)) + 
            geom_point(alpha=0.7) +
            labs(title="PCA - Scatter Plot")
        }
        
        ggplotly(p + theme_minimal())
      })
      
      # PCA loadings plot showing variable contributions to PC1 and PC2
      output$pca_loadings <- renderPlotly({
        # Loadings plot
        loadings <- as.data.frame(pc$rotation[,1:2])
        loadings$variable <- rownames(loadings)
        
        p <- ggplot(loadings, aes(PC1, PC2, label=variable)) +
          geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2), 
                       arrow=arrow(length=unit(0.2,"cm")), color="steelblue") +
          geom_text(hjust=0, vjust=0, size=3) +
          coord_fixed() +
          theme_minimal() +
          labs(title="PCA - Loading Plot")
        
        ggplotly(p)
      })
      
      # Scree plot showing variance explained by each PC
      output$pca_scree <- renderPlotly({
        var_exp <- pc$sdev^2 / sum(pc$sdev^2)
        dfv <- data.frame(PC = paste0("PC",seq_along(var_exp)),
                          Var = var_exp)
        # Ensure PCs are ordered from PC1 to the last PC
        dfv$PC <- factor(dfv$PC, levels = paste0("PC", 1:length(var_exp)))
        
        ggplotly(
          ggplot(dfv,aes(PC,Var))+geom_col(fill="#2c3e50")+
            geom_line(aes(PC,Var), group=1, colour="#e67e22")+
            geom_point(aes(PC,Var), colour="#e67e22")+
            scale_y_continuous(labels=percent)+
            labs(y="Explained variance", title="Scree plot")
        )
      })
      
      # Simplified biplot combining scores and loadings
      output$pca_biplot <- renderPlotly({
        # Simplified biplot
        scores <- as.data.frame(pc$x[,1:2])
        loadings <- as.data.frame(pc$rotation[,1:2])
        
        # Scale loadings for visualization
        mult <- min(
          max(abs(scores$PC1))/max(abs(loadings$PC1)),
          max(abs(scores$PC2))/max(abs(loadings$PC2))
        ) * 0.7
        
        loadings_scaled <- loadings * mult
        loadings_scaled$variable <- rownames(loadings)
        
        p <- ggplot() +
          geom_point(data=scores, aes(PC1, PC2), alpha=0.5) +
          geom_segment(data=loadings_scaled, 
                       aes(x=0, y=0, xend=PC1, yend=PC2),
                       arrow=arrow(length=unit(0.2,"cm")), color="red") +
          geom_text(data=loadings_scaled, aes(PC1, PC2, label=variable),
                    hjust=0, vjust=0, size=3, color="red") +
          theme_minimal() +
          labs(title="PCA - Simplified biplot")
        
        ggplotly(p)
      })
      
      # Variable contribution to principal components
      output$pca_contrib <- renderPlotly({
        # Variable contribution to components
        contrib <- as.data.frame(pc$rotation[,1:min(5, ncol(pc$rotation))]^2)
        contrib$variable <- rownames(contrib)
        contrib_long <- pivot_longer(contrib, -variable, 
                                    names_to="PC", values_to="contribution")
        
        p <- ggplot(contrib_long, aes(variable, contribution, fill=PC)) +
          geom_col(position="dodge") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle=45, hjust=1)) +
          labs(title="Variable contribution to components",
               y="Contribution (loading^2)")
        
        ggplotly(p)
      })
      
      # UMAP visualization with optional coloring
      incProgress(0.70,"UMAP")
      output$umap <- renderPlotly({
        withProgress("calculating UMAP…", value=0,{
          # Configure UMAP parameters
          cfg <- umap.defaults
          cfg$n_neighbors <- input$umap_k
          cfg$min_dist    <- input$umap_md
          incProgress(.4,"layout")
          um <- umap(X, config = cfg)$layout  # Run UMAP algorithm
          dfu <- data.frame(U1=um[,1],U2=um[,2])
          
          # Apply coloring based on user selection or clustering
          if (input$color_by != "None"){
            dfu$color <- df[[input$color_by]]
          } else if (isTRUE(input$cluster_umap)){
            k <- input$k_clusters
            set.seed(123); dfu$color <- factor(kmeans(um, k)$cluster)
          } else dfu$color <- factor(1)
          
          incProgress(.9,"plot")
          ggplotly(
            ggplot(dfu,aes(U1,U2,color=color))+
              geom_point(alpha=.8)+theme_minimal()+
              labs(color=input$color_by)
          )
        })
      })
      incProgress(1)
    })
  })
  
  # ----------------  Generic PCA + GAM -------------------------------------
  # Event handler for PCA + GAM analysis button
  observeEvent(input$run_pcagam,{
    withProgress(message="PCA + GAM", value=0,{
      incProgress(.05,"data")
      df <- as.data.frame(data_raw())
      req(length(input$vars)>0)
      preds <- input$vars
      X <- safe_scale(df[, preds, drop=FALSE])
      y <- df[[input$target]]
      if (!is.numeric(y)){ shinyalert("Error","Target variable must be numeric"); return() }
      
      # Split data into training (70%) and test (30%) sets
      set.seed(123); sp <- sample.split(y, 0.7)
      X_tr <- X[ sp, , drop=FALSE]; y_tr <- y[ sp]
      X_te <- X[!sp, , drop=FALSE]; y_te <- y[!sp]
      
      # Save original data for ICE plots
      df_tr <- df[sp, ]
      df_te <- df[!sp, ]
      
      # Perform PCA on training data
      incProgress(.25,"PCA")
      ncp <- min(input$n_components, ncol(X_tr))
      pc <- prcomp(X_tr, center=FALSE, scale.=FALSE)
      PCs_tr <- pc$x[,1:ncp, drop=FALSE]
      PCs_te <- predict(pc, X_te)[,1:ncp, drop=FALSE]  # Project test data onto PCs
      colnames(PCs_tr) <- colnames(PCs_te) <- paste0("PC",1:ncp)
      dft <- data.frame(y = y_tr, PCs_tr)
      
      # Fit GAM model using PCs as predictors
      incProgress(.55,"GAM")
      # Ensure column names are correct
      pc_names <- colnames(PCs_tr)
      form <- as.formula(
        paste("y ~", paste0("s(", pc_names, ", bs='cr')",
                            collapse=" + "))
      )
      
      # Verify all columns exist in the dataframe
      if (!all(pc_names %in% colnames(dft))) {
        shinyalert("Error", "Column names don't match in training data")
        return()
      }
      
      # Fit GAM with cubic regression splines and automatic smoothing parameter selection
      gam_mod <- mgcv::gam(form, data=dft, select=TRUE)
      
      # Make predictions on test data
      incProgress(.75,"prediction")
      # Ensure column names match
      dft_test <- as.data.frame(PCs_te)
      colnames(dft_test) <- pc_names
      
      preds <- predict(gam_mod, dft_test)
      r2 <- summary(gam_mod)$r.sq
      rmse <- Metrics::rmse(y_te, preds)
      mape <- Metrics::mape(y_te, preds)
      
      # Create a combined model for DALEX
      combined <- list(
        pca = pc,
        gam = gam_mod,
        ncp = ncp
      )
      
      # Prediction function for the combined model
      predict.pca_lasso_gam <- function(model, newdata) {
        # Transform original data to PCs
        pcs <- predict(model$pca, newdata)[, 1:model$ncp, drop=FALSE]
        colnames(pcs) <- paste0("PC", 1:model$ncp)
        # Predict with GAM
        predict(model$gam, as.data.frame(pcs))
      }
      
      # Display model summary and performance metrics
      incProgress(.90,"outputs")
      output$gam_summary <- renderPrint(summary(gam_mod))
      output$pcagam_scatter <- renderPlotly({
        ggplotly(
          ggplot(data.frame(Real=y_te,Pred=preds), aes(Real,Pred))+
            geom_point(alpha=.7)+geom_abline(slope=1,intercept=0,lty=2,col="red")+
            theme_minimal()
        )
      })
      output$gam_metrics <- renderDT({
        datatable(data.frame(Metric=c("R²","RMSE","MAPE"),
                             Value  =round(c(r2,rmse,mape),4)),
                  options=list(dom='t'))
      })
      
      # Plot significant smooth terms from the GAM model
      incProgress(.95,"smooths")
      output$gam_smooths <- renderPlot({
        sig <- which(summary(gam_mod)$s.table[,"p-value"] < 0.05)
        if (!length(sig)){ plot.new(); text(0.5,0.5,"No smooths p<0.05"); return() }
        n <- length(sig); ncol <- 3; nrow <- ceiling(n/ncol)
        par(mfrow=c(nrow,ncol), mar=c(2.5,2.5,2,1))
        lapply(sig, function(i)
          plot(gam_mod, select=i, shade=TRUE,
               main=row.names(summary(gam_mod)$s.table)[i]))
      })
      
      # Generate ICE plots to show individual-level effects
      output$gam_ice_plots <- renderPlot({
        # 1) extract indices of significant smooths
        s_tab <- summary(gam_mod)$s.table
        sig   <- which(s_tab[,"p-value"] < 0.05)
        if (!length(sig)) {
          plot.new(); text(0.5,0.5,"No significant variables for ICE plots")
          return()
        }
        # 2) extract terms "s(PCk)" and unwrap to "PCk"
        terms <- rownames(s_tab)[sig]
        ice_vars <- gsub("^s\\((.*)\\)$", "\\1", terms)
        # ensure we only keep those that actually exist
        ice_vars <- intersect(ice_vars, colnames(PCs_tr))
        if (!length(ice_vars)) {
          plot.new(); text(0.5,0.5,"No matching PCs in ICE vars")
          return()
        }
        # 3) layout: up to 4 plots in 2x2
        n <- length(ice_vars)
        ncol <- 2; nrow <- ceiling(n/ncol)
        op <- par(mfrow=c(nrow,ncol), mar=c(4,4,2,1))
        on.exit(par(op), add=TRUE)
        
        # 4) for each PC, generate ICE plots with individual-level variability
        for (pc in ice_vars) {
          # range of PC in training
          r <- range(PCs_tr[, pc])
          seq_pc <- seq(r[1], r[2], length.out=50)
          
          # Create a matrix to store all individual predictions
          n_samples <- min(50, nrow(PCs_tr)) # Limit to 50 samples for clarity
          sample_indices <- sample(1:nrow(PCs_tr), n_samples)
          ice_predictions <- matrix(NA, nrow=length(seq_pc), ncol=n_samples)
          
          # For each sample, create predictions across the range of the PC
          for (i in 1:n_samples) {
            idx <- sample_indices[i]
            # Create newdata where all PCs are from this sample except the one we're varying
            newdata <- as.data.frame(matrix(
              rep(NA, length(seq_pc) * ncol(PCs_tr)),
              nrow=length(seq_pc), ncol=ncol(PCs_tr))
            )
            colnames(newdata) <- colnames(PCs_tr)
            
            for (j in colnames(PCs_tr)) {
              if (j == pc) newdata[[j]] <- seq_pc
              else         newdata[[j]] <- PCs_tr[idx, j]
            }
            
            # Get predictions
            ice_predictions[, i] <- predict(gam_mod, newdata=newdata)
          }
          
          # Center each curve at the minimum value of the PC
          min_idx <- which.min(seq_pc)
          for (i in 1:n_samples) {
            ice_predictions[, i] <- ice_predictions[, i] - ice_predictions[min_idx, i]
          }
          
          # Calculate the average effect (PDP)
          pdp_values <- rowMeans(ice_predictions)
          
          # Plot
          plot(seq_pc, pdp_values, type="l", lwd=3, col="red",
               xlab=pc, ylab="Centered Effect",
               main=paste("ICE Plot:", pc),
               ylim=range(ice_predictions))
          
          # Add individual ICE curves
          for (i in 1:n_samples) {
            lines(seq_pc, ice_predictions[, i], col=rgb(0,0,1,0.2))
          }
          
          # Add the PDP line on top
          lines(seq_pc, pdp_values, lwd=3, col="red")
          
          # Add a horizontal line at y=0 (baseline)
          abline(h=0, lty=2, col="darkgray")
          
          # Add legend
          legend("topleft", 
                 legend=c("Average effect (PDP)", "Individual effects (ICE)"),
                 col=c("red", rgb(0,0,1,0.5)), 
                 lwd=c(3, 1),
                 bg="white")
        }
      })
      
      # Generate diagnostic plots for the GAM model
      # QQ plot of residuals to check normality
      output$gam_qq <- renderPlot({
        par(mfrow=c(1,1), mar=c(4,4,2,1))
        residuos <- residuals(gam_mod)
        qqnorm(residuos, main="QQ Plot of Residuals")
        qqline(residuos, col="red")
      })
      
      # Residuals vs Fitted values to check homoscedasticity
      output$gam_residuals <- renderPlot({
        par(mfrow=c(1,1), mar=c(4,4,2,1))
        fitted_values <- fitted(gam_mod)
        residuos <- residuals(gam_mod)
        plot(fitted_values, residuos, 
             xlab="Fitted Values", ylab="Residuals",
             main="Residuals vs Fitted Values",
             pch=19, col=rgb(0,0,1,0.5))
        abline(h=0, col="red", lty=2)
        lines(lowess(fitted_values, residuos), col="green", lwd=2)
      })
      
      # Histogram of residuals to check distribution
      output$gam_hist_residuals <- renderPlot({
        par(mfrow=c(1,1), mar=c(4,4,2,1))
        residuos <- residuals(gam_mod)
        hist(residuos, 
             breaks=30, 
             main="Histogram of Residuals",
             xlab="Residuals", 
             col="lightblue",
             border="white",
             probability=TRUE)
        lines(density(residuos), col="red", lwd=2)
        curve(dnorm(x, mean=mean(residuos), sd=sd(residuos)), 
              add=TRUE, col="blue", lwd=2, lty=2)
        legend("topright", 
               legend=c("Density", "Normal"), 
               col=c("red", "blue"), 
               lwd=2, 
               lty=c(1,2))
      })
      
      incProgress(1)
    })
  })
  
  # ----------------  PLS Analysis  ------------------------------------------
  # Event handler for PLS analysis button
  observeEvent(input$run_pls, {
    withProgress(message="PLS Analysis", value=0, {
      incProgress(.05, "preparing data")
      df <- as.data.frame(data_raw())
      req(length(input$vars) > 0)
      X <- as.matrix(df[, input$vars, drop=FALSE])
      y <- df[[input$target]]
      
      if (!is.numeric(y)) {
        shinyalert("Error", "Target variable must be numeric")
        return()
      }
      
      # Remove rows with NA values
      complete_cases <- complete.cases(X, y)
      X <- X[complete_cases, ]
      y <- y[complete_cases]
      
      # Normalize data for PLS
      X_scaled <- scale(X)
      y_scaled <- scale(y)
      
      incProgress(.15, "fitting PLS model")
      
      # Fit Partial Least Squares model
      pls_model <- plsr(y_scaled ~ X_scaled, ncomp = input$pls_ncomp, validation = "CV")
      
      incProgress(.60, "processing results")
      
      # Display model summary
      output$pls_summary <- renderPrint({
        summary(pls_model)
      })
      
      # Plot PLS results - MSEP validation plot
      output$pls_plot <- renderPlot({
        validationplot(pls_model, val.type = "MSEP")
      })
      
      # Plot PLS loadings
      output$pls_loadings <- renderPlot({
        par(mar = c(8, 4, 4, 2))
        loadings <- loadings(pls_model)
        barplot(loadings[,1], 
                names.arg = rownames(loadings),
                main = "PLS Component 1 Loadings",
                las = 2,
                cex.names = 0.8,
                col = "steelblue")
      })
      
      # Plot PLS scores
      output$pls_scores <- renderPlot({
        scores <- scores(pls_model)
        plot(scores[,1], scores[,2],
             xlab = "Component 1",
             ylab = "Component 2",
             main = "PLS Scores Plot",
             pch = 19,
             col = rgb(0, 0, 1, 0.5))
        abline(h = 0, v = 0, lty = 2, col = "gray")
      })
      
      # Plot predicted vs measured values
      output$pls_pred_vs_obs <- renderPlot({
        pred <- predict(pls_model, ncomp = min(input$pls_ncomp, dim(pls_model$scores)[2]))
        plot(y_scaled, pred,
             xlab = "Measured (Scaled)",
             ylab = "Predicted (Scaled)",
             main = "Predicted vs Measured Values",
             pch = 19,
             col = rgb(0, 0, 1, 0.5))
        abline(0, 1, col = "red", lwd = 2)
        
        # Add R² to the plot
        pred_r2 <- cor(y_scaled, pred)^2
        legend("topleft", 
               legend = sprintf("R² = %.3f", pred_r2),
               bty = "n")
      })
      
      # Plot variable importance
      output$pls_var_importance <- renderPlot({
        par(mar = c(8, 4, 4, 2))
        vip_scores <- vip(pls_model)[,1:min(3, input$pls_ncomp)]
        if(is.vector(vip_scores)) {
          vip_scores <- as.matrix(vip_scores)
          colnames(vip_scores) <- paste("Comp", 1)
        }
        barplot(vip_scores[,1], 
                names.arg = rownames(vip_scores),
                main = "Variable Importance in Projection (VIP)",
                las = 2,
                cex.names = 0.8,
                col = "darkgreen")
        abline(h = 1, lty = 2, col = "red")
      })
      
      incProgress(1)
    })
  })
  
  # ----------------  BKMR Analysis  ------------------------------------------
  # Event handler for BKMR analysis button
  observeEvent(input$run_nkmr, {
    withProgress(message="BKMR Analysis", value=0, {
      incProgress(.05, "preparing data")
      df <- as.data.frame(data_raw())
      req(length(input$vars) > 0)
      X <- as.matrix(df[, input$vars, drop=FALSE])
      y <- df[[input$target]]
      
      if (!is.numeric(y)) {
        shinyalert("Error", "Target variable must be numeric")
        return()
      }
      
      # Remove rows with NA values
      complete_cases <- complete.cases(X, y)
      X <- X[complete_cases, ]
      y <- y[complete_cases]
      
      # Normalize data for BKMR
      X_scaled <- scale(X)
      y_scaled <- scale(y)
      
      incProgress(.15, "fitting BKMR model")
      
      # Fit Bayesian Kernel Machine Regression model
      set.seed(123)
      tryCatch({
        bkmr_model <- kmbayes(
          y = y_scaled,
          Z = X_scaled,
          iter = input$bkmr_iter,
          verbose = FALSE,
          varsel = TRUE
        )
        
        incProgress(.60, "processing results")
        
        # Extract posterior inclusion probabilities
        ExtractPIPs <- ExtractPIPs(bkmr_model)
        
        # Display model summary
        output$bkmr_summary <- renderPrint({
          summary(bkmr_model)
        })
        
        # PIPs (Posterior Inclusion Probabilities) plot
        output$bkmr_pips <- renderPlot({
          par(mar = c(5, 10, 4, 2))
          barplot(ExtractPIPs$pip, 
                  names.arg = ExtractPIPs$varnames,
                  horiz = TRUE, 
                  las = 1,
                  main = "Posterior Inclusion Probabilities (PIPs)",
                  xlab = "Probability")
        })
        
        # Univariate effects
        output$bkmr_univar <- renderPlot({
          withProgress(message = "Generating univariate effects", value = 0, {
            incProgress(0.5)
            
            # Select variables with highest PIPs
            top_vars <- order(ExtractPIPs$pip, decreasing = TRUE)[1:min(4, length(ExtractPIPs$pip))]
            
            par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
            for (i in top_vars) {
              PredictorResponseUnivar(
                fit = bkmr_model,
                z.name = colnames(X)[i],
                method = "approx"
              )
            }
          })
        })
        
        # Bivariate effects
        output$bkmr_bivar <- renderPlot({
          withProgress(message = "Generating bivariate effects", value = 0, {
            incProgress(0.5)
            
            # Select the two variables with highest PIPs
            top_vars <- order(ExtractPIPs$pip, decreasing = TRUE)[1:min(2, length(ExtractPIPs$pip))]
            
            if (length(top_vars) >= 2) {
              PredictorResponseBivar(
                fit = bkmr_model,
                z.names = colnames(X)[top_vars[1:2]],
                method = "approx"
              )
            } else {
              plot.new()
              text(0.5, 0.5, "Insufficient variables for bivariate analysis")
            }
          })
        })
        
      }, error = function(e) {
        shinyalert("Error in BKMR", paste("Error:", e$message))
      })
      
      incProgress(1)
    })
  })
}

# ----------------  RUN  -------------------------------------------------------
shinyApp(ui, server)
