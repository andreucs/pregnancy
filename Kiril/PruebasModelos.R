#librerias
library(readxl)
library(rsample)
library(tidymodels)
library(rpart.plot)
library(vip)
library(dplyr)
library(fastDummies)
library(stringr)

setwd("C:/Users/kiril/Documentos/CienciaDatos3/PROYIII/pregnancy/LimpiezaDatos")

# Leemos datos
df <- read_excel("datos_limpios_2_log_transformado.xlsx")
df_limpia <- df %>% select(-sexo, -idnum)
df_limpia <- dummy_cols(df_limpia, select_columns = c("femb", "alcohol", "preterm", "CSMIX3", "tipozona"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
df_limpia$edad_menarquia <-  as.factor(df_limpia$edad_menarquia)

log_cols <- names(df_limpia)[str_starts(names(df_limpia), "log_")]
orig_cols <- str_remove(log_cols, "^log_")

df_normal <- df_limpia %>% select(-all_of(log_cols))
df_log <- df_limpia %>% select(-all_of(orig_cols))

# Preparamos entrenamiento y set
set.seed(123)
data_split <- initial_split(df_normal, prop = 0.75)
data_split <- initial_split(df_log, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Creamos arbol
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Fit the model to the training data
tree_fit <- tree_spec %>%
  fit(edad_menarquia ~ ., data = train_data)

# Evaluamos
predictions <- tree_fit %>%
  predict(test_data) %>%
  pull(.pred_class)

# Calculate accuracy y nose que mas
metrics_cls <- metric_set(accuracy, kap, sens, spec)

model_performance_cls <- test_data %>%
  mutate(predictions = predictions) %>%
  metrics_cls(truth = edad_menarquia, estimate = predictions)


# Plot the decision tree
rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")


rules <- rpart.rules(tree_fit$fit)
print(rules)

# Create a variable importance plot
var_importance <- vip::vip(tree_fit, num_features = 18)
print(var_importance)




# PROBAMOS CON MAS MODELOS




