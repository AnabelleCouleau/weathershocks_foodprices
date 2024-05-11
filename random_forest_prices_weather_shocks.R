#remove list of objects
rm(list = ls())

# Load libraries ------
libs <- c(
  "here", "haven", "tidyverse",
  "randomForest", "caret",
  "corrr", "ggcorrplot", 
  "FactoMineR", "factoextra",
  "patchwork"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# Load libraries
invisible(lapply(libs, library, character.only = T))

# Load datasets ------
res_permanentes <- read_dta(here::here('metadata_permanentes.dta'))
res_transitorios <- read_dta(here::here('metadata_transitorios.dta'))
caracteristicas_old <- read_dta(here::here('caracterisiticas_product_2015_2022.dta'))
caracteristicas <- read_dta(here::here('caracteristicas_product_2015_2022_v4.dta'))

# exploring `caracteristicas` dataset

## check missing data
colSums(is.na(caracteristicas))
# drop na from caracteristicas
unique(caracteristicas$producto[is.na(caracteristicas$irrigacion)])
caracteristicas <- caracteristicas[complete.cases(caracteristicas),]

## normalize the data
col_number = dim(caracteristicas)[2]
numerical_data <- caracteristicas[,2:col_number]
head(numerical_data)
data_normalized <- scale(numerical_data)
head(data_normalized)

## compute the correlation matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

caracteristicas <- caracteristicas %>%
  select(-c("pobl_rur", "pobl_urb", "pib_agricola", "gorinoquia", "gpacifica", "gamazonia",
            "gcaribe", "gandina", "distance_km", "nbicabecera"))

## normalize the data
col_number = dim(caracteristicas)[2]
numerical_data <- caracteristicas[,2:col_number]
head(numerical_data)
data_normalized <- scale(numerical_data)
head(data_normalized)


## conducting PCA analysis
data.pca <- princomp(corr_matrix)
summary(data.pca)

## loading for the first 3 comp.
data.pca$loadings[, 1:3]

## vizualization of the importance of the princ. comp.
fviz_eig(data.pca, addlabels = TRUE)

## Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

## graph of the variables for the comp. 1 and 2
fviz_cos2(data.pca, choice = "var", axes = 1:2)

# Based on the PCA analysis I will exclude the following variables: pobl_rur,
# pib_agricola, gorinoquia, gpacifica, gamazonia, gcaribe, gandina. 

caracteristicas <- caracteristicas %>%
  select(-c("agua_fuentenatural", "agua_carrotanque", "agua_rio", 
            "agua_lago", "agua_rio", "agua_cienaga", "agua_embalse", "agua_pozo",
            "agua_acueducto", "irrigacion", "agua_lluvia", "agua_riego", "agua_notiene", "agua_tiene"))


## compute the adjusted correlation matrix (with excluded variables from previous analysis)
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

data_normalized_water = data_normalized[,c("agua_fuentenatural", "agua_carrotanque", "agua_rio",
                                           "agua_lago", "agua_rio", "agua_cienaga", "agua_embalse",
                                           "agua_pozo","agua_acueducto", "irrigacion", "agua_lluvia",
                                           "agua_sololluvia", "agua_solonatural", "agua_riego", "agua_tiene",
                                           "agua_notiene", "agua_otrafuentenatural", "agua_nonatural")]

corr_matrix_water <- cor(data_normalized_water)
ggcorrplot(corr_matrix_water)

# Select variable by category 
cat1_caracteristicas <- caracteristicas %>%
  select(c("producto","altura","maquinaria","construccion","red_electrica",
           "asistencia_tecnica","credito","area_upa","duration_h","num_mpio",
           "agua_sololluvia", "agua_otrafuentenatural", "agua_nonatural"))
cat1_caracteristicas <- caracteristicas %>%
  select(c("producto","altura","red_electrica",
           "asistencia_tecnica","credito","area_upa","duration_h","num_mpio",
            "agua_otrafuentenatural", "agua_nonatural")) %>% 
  mutate(area_upa_sq = area_upa^2)

# Data wrangling -----

res_all <- bind_rows(res_permanentes, res_transitorios) 

res_all = res_all |>
  mutate(dum_pt = case_when(ciclo == "PERMANENTE" ~ 1,
                            (ciclo == "TRANSITORIO" | ciclo == "ANUAL") ~ 0)) |>
  mutate(y_a = ifelse((b_preci_a_p + b_preci_a_t > 0) & 
                      ((b_preci_a_p > 0 & pval_preci_a_p <= 0.10) |
                         (b_preci_a_t > 0 & pval_preci_a_t <= 0.10)), 1, 0)) |>
  mutate(y_b = ifelse((b_preci_b_p + b_preci_b_t > 0) & 
                      ((b_preci_b_p > 0 & pval_preci_b_p <= 0.10) |
                         (b_preci_b_t > 0 & pval_preci_b_t <= 0.10)), 1, 0)) |>
  mutate(dum_pt = as.factor(dum_pt)) |>
  select(c("cod_producto", "producto", "dum_pt", "y_a", "y_b")) 


# pivot to long format the variables of interest
pivot_and_mutate <- function(data, prefix) {
  data %>%
    select(c("cod_producto", "producto", "dum_pt", starts_with(prefix))) %>%
    pivot_longer(cols = starts_with(prefix),
                 names_to = "y_variable", values_to = "value") %>%
    mutate(value = as.factor(value),
           y_variable = as.factor(y_variable))
}

res_all_y_a <- pivot_and_mutate(res_all, "y_a")
res_all_y_b <- pivot_and_mutate(res_all, "y_b")


# merge 'res_all' with 'caracteristicas'

database_ML_y_a <- merge(res_all_y_a, cat1_caracteristicas, by = "producto")
database_ML_y_b <- merge(res_all_y_b, cat1_caracteristicas, by = "producto")

# Model Random Forest -----


classifier_RF = function(database){
  set.seed(123)
  train_index <- sample(1:nrow(database), 0.7*nrow(database))
  train <- database[train_index,]
  test <- database[-train_index,]
  
  rf_model <- randomForest(y = train$value,
                           x = train[-2],
                           data = train,
                           ntree = 500, importance = T)
  
  # predict the test set
  rf_pred = predict(rf_model, newdata = test)
  confusion_mtx = confusionMatrix(rf_pred, test$value)
  confusion_mtx
  
  # Plotting model
  plot(rf_model)
  
  # Importance plot
  importance(rf_model)
  
  
  rf_model
  return(rf_model)
}


## Panel coefficients (y_a) ------
data_RF1 = database_ML_y_a |> select(!c("cod_producto", "producto", "y_variable"))

RF1 = classifier_RF(data_RF1)
OOB_RF1 = RF1$err.rate[nrow(RF1$err.rate),1]
plot(RF1)
varImpPlot(RF1)

## Panel coefficients (y_b) ------
data_RF2 = database_ML_y_b |> select(!c("cod_producto", "producto", "y_variable"))

RF2 = classifier_RF(data_RF2)
OOB_RF2 = RF2$err.rate[nrow(RF2$err.rate),1]
plot(RF2)
varImpPlot(RF2)


# Notes: 
#The Out-of-bag OOB error estimate rate should be small 
#(100 - OOB error rate = Accuracy)


# Revisited plot of the importance of the variables

# Model 1 precip alta
importance = importance(RF1)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "MeanDecreaseAccuracy"],2))
rankImportance=varImportance%>%mutate(Rank=paste("#",dense_rank(desc(Importance))))
p_m1 = ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat="identity") + 
  geom_text(aes(x = Variables, y = 0.25, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = "white") +
  labs(x = "Variables", title = "RF High Precipitation Linear Combination Coefs") +
  coord_flip() + 
  theme_classic()
p_m1

# Model 2 precip baja
importance = importance(RF2)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "MeanDecreaseAccuracy"],2))
rankImportance=varImportance%>%mutate(Rank=paste("#",dense_rank(desc(Importance))))
p_m2 = ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat="identity") + 
  geom_text(aes(x = Variables, y = 0.25, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = "white") +
  labs(x = "Variables", title = "RF Low Precipitation Linear Combination Coefs") +
  coord_flip() + 
  theme_classic()
p_m2

OOB_RF1
OOB_RF2

print(100*(1-OOB_RF1))
print(100*(1-OOB_RF2))


library(patchwork)
p_all = p_m1 + p_m2
p_all

