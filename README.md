# Menarche Mixture Analysis

This repository contains the development of an investigation project to compare alternative methods to Bayesian Kernel Machine Regression (BKMR) for correlating environmental exposure mixtures with age at menarche.

## Folder Structure

* **Dashboard/**: Shiny application (`app.R`) to interactively visualize and compare results from each method.
* **Data_cleaning/**:

  * `01_preprocess_data.R` / notebooks: initial data cleaning, imputation, and outlier filtering.

* **Methodology/**:

  * PCA + GAM: dimensionality reduction with PCA followed by nonlinear modeling with GAM.
  * Univariate and multivariate GAMs to capture nonlinear relationships between components and menarche age.
* **PLS\_K-PLS/**:

  * Implementation of Partial Least Squares (PLS) and Kernel PLS (K-PLS).
  * Performance metrics (R², RMSE) and VIP scores (Variable Importance in Projection).
* **data/**:

  * Additional reference data files

## Requirements & Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/andreucs/pregnancy
   ```

2. Install R dependencies:

   ```r
   install.packages(c(
     "shiny", "tidyverse", "mgcv", "pls", "kernlab",
     "rpart", "randomForest", "bkmr"
   ))
   ```

3. (Optional) Install Python dependencies:

   ```bash
   pip install pandas numpy scikit-learn statsmodels
   ```

> **Note:** All scripts generate output tables and figures in their respective `output/` directories.

## Expected Results

* Performance comparison of methods (R², RMSE, AIC)
* Identification of the most influential exposures according to each model
* Plots of nonlinear effects and partial dependence

## Authors & Affiliations

Jaime Ballester\*,¹, Andreu Bonet\*,¹, Juan Alfonso García\*,¹, Anna Gil\*,¹, Kiril Ivaylov\*,¹, and Marcos Ranchal\*,¹

\*Core contributors
¹ Polytechnic University of Valencia

**Corresponding authors:**

* Jaime Ballester: [jbalsol@etsinf.upv.es](mailto:jbalsol@etsinf.upv.es)
* Andreu Bonet: [abonpav@etsinf.upv.es](mailto:abonpav@etsinf.upv.es)
* Juan Alfonso García: [jagarrea@etsinf.upv.es](mailto:jagarrea@etsinf.upv.es)
* Anna Gil: [agilmol@etsinf.upv.es](mailto:agilmol@etsinf.upv.es)
* Kiril Ivaylov: [kivatze@etsinf.upv.es](mailto:kivatze@etsinf.upv.es)
* Marcos Ranchal: [mrangar@etsinf.upv.es](mailto:mrangar@etsinf.upv.es)

---

For more details on methodology and parameter settings, refer to comments within each script and supporting documents in `Methodology/`.
