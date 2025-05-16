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
