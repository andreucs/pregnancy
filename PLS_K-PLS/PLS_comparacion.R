# --------------------------------------------
# COMPARACIÓN ENTRE OPLS Y KERNEL PLS
# --------------------------------------------
library(pls)
library(kernlab)
library(caret)
library(Metrics)
library(tidyverse)
library(readxl)
library(ropls)   # Para OPLS
library(ggplot2)

# --------------------------------------------
# CARGA Y PREPARACIÓN DE LOS DATOS
# --------------------------------------------
setwd(".")
df <- read_excel('C:/Users/kiril/Documentos/CienciaDatos3/PROYIII/pregnancy/datos_limpios_2_log_transformado.xlsx')

# Limpieza de variables innecesarias
df <- df %>%
  select(-c('pfhxs', 'pfoa', 'pfos', 'pfna', 'v_44DDT', 'v_44DDE', 'HCB', 'bHCH' , 'PCB'))

# Recodificación de variables categóricas
df$femb <- ifelse(df$femb == "Sí", 1, 0)
df$preterm <- ifelse(df$preterm == "sí", 1, 0)
df$alcohol <- ifelse(df$alcohol == "Si", 1, 0)

# Variables dummy para tipo de zona
dummies <- model.matrix(~ tipozona - 1, data = df)
colnames(dummies) <- c("z_Rural", "z_Semiurbana", "z_Urbana")
df <- cbind(df, dummies)

# Clase social como ordinal
df$Clase_social <- as.numeric(factor(df$CSMIX3, levels = c("CS I+II", "CS III", "CS IV+V"), ordered = TRUE))

# Quitar columnas no numéricas
df <- df %>% select(-c('sexo', 'idnum', 'CSMIX3', 'tipozona'))

# Variable respuesta y predictores
y <- df$edad_menarquia
X <- df %>% select(-edad_menarquia)

# Escalado
X <- scale(X)

# --------------------------------------------
# MODELO OPLS (ropls)
# --------------------------------------------
opls_model <- opls(x = X, y = y, predI = 2, crossvalI = 10, scaleC = "none")
opls_preds <- fitted(opls_model)
opls_rmse <- rmse(y, opls_preds)
opls_r2 <- R2(opls_preds, y)

# --------------------------------------------
# KERNEL PLS MANUAL
# --------------------------------------------
kernel_types <- list(
  vanilladot = vanilladot(),
  rbfdot = rbfdot(sigma = 0.1),
  polydot = polydot(degree = 2, scale = 1, offset = 1),
  tanhdot = tanhdot(scale = 1, offset = 1),
  laplacedot = laplacedot(sigma = 0.1),
  besseldot = besseldot(sigma = 1, order = 1, degree = 1),
  anovadot = anovadot(degree = 2, sigma = 0.5),
  splinedot = splinedot()
)

kplsr <- function(train_x, train_y, kernel, folds = 5) {
  kfolds <- createFolds(train_y, k = folds, list = TRUE)
  rmse_list <- c(); mae_list <- c(); r2_list <- c()
  
  for (i in 1:folds) {
    idx_test <- kfolds[[i]]
    x_train_cv <- train_x[-idx_test, , drop = FALSE]
    y_train_cv <- train_y[-idx_test]
    x_test_cv <- train_x[idx_test, , drop = FALSE]
    y_test_cv <- train_y[idx_test]
    tryCatch({
    
    # Matrices de kernel
      K <- kernelMatrix(kernel, x_train_cv)
      Ktest <- kernelMatrix(kernel, x_test_cv, x_train_cv)
      
      # Estimación tipo ridge
      lambda <- 1e-3  # o prueba 1e-2 si sigue habiendo problemas
      KtK_reg <- t(K) %*% K + diag(lambda, nrow(K))
      alpha <- solve(KtK_reg, t(K) %*% y_train_cv)
      preds <- Ktest %*% alpha
      }, error = function(e) {
        warning(paste("Error con kernel:", deparse(substitute(kernel)), "-", e$message))
        preds <- rep(mean(y_train_cv), length(y_test_cv))  # fallback: predicción constante
      })
      rmse_list <- c(rmse_list, rmse(y_test_cv, preds))
      mae_list <- c(mae_list, mae(y_test_cv, preds))
      r2_list <- c(r2_list, R2(preds, y_test_cv))
  }
  
  return(data.frame(
    Kernel = deparse(substitute(kernel)),
    RMSE = mean(rmse_list),
    MAE = mean(mae_list),
    R2 = mean(r2_list)
  ))
}

kernel_results <- map_dfr(kernel_types, ~ kplsr(X, y, .x, folds = 5), .id = "Kernel")

# Añadir OPLS a resultados
kernel_results <- rbind(kernel_results, data.frame(
  Kernel = "OPLS",
  RMSE = opls_rmse,
  MAE = mae(y, opls_preds),
  R2 = opls_r2
))

# --------------------------------------------
# VISUALIZACIÓN DE RESULTADOS
# --------------------------------------------
# Comparativa de rendimiento
ggplot(kernel_results, aes(x = reorder(Kernel, RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Comparación de RMSE por tipo de kernel", x = "Kernel", y = "RMSE")

ggplot(kernel_results, aes(x = reorder(Kernel, MAE), y = MAE)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Comparación de MAE por tipo de kernel", x = "Kernel", y = "MAE")

ggplot(kernel_results, aes(x = reorder(Kernel, R2), y = R2)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Comparación de R² por tipo de kernel", x = "Kernel", y = "R²")

# Visualización de predicciones OPLS vs reales
ggplot(data.frame(Real = y, Predicho = opls_preds), aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "OPLS: Predicho vs Real", x = "Valor real", y = "Predicción") +
  theme_minimal()
