# Carregar Pacotes --------------------------------------------------------
pacman::p_load(tidyverse,
               here,
               stringr,
               stringdist,
               gtsummary,
               dplyr,
               huxtable,
               ggplot2,
               lubridate,
               mice,
               janitor)

# Carregar as Bases de Dados ----------------------------------------------
df <- rio::import(here("Bases de dados", "dados_2022_2025.xlsx"), which = 1, col_types = "text") %>% 
  janitor::clean_names() %>% 
  select(-c(idade_dias, idade_meses, idade_anos)) %>% 
  filter(resultado_sars_cov_2 != "indeterminado") %>% 
  mutate(
    hemograma = ifelse(str_to_lower(hemograma) == "n_f", "negativo", hemograma),
    bioquimica = ifelse(str_to_lower(bioquimica) == "n_f", "negativo", bioquimica)
  )

# Limpeza de inconsistencias ----------------------------------------------

# Formatacao dos tipos de variaveis
df <- df %>%
  mutate(
    # Numéricas
    idade = as.numeric(idade),
    sintomas_dias = as.numeric(sintomas_dias),
    sintomas_freq_resp = as.numeric(sintomas_freq_resp),
    
    # Data
    data_da_colheita = as.Date(as.numeric(data_da_colheita), origin = "1899-12-30"),
    
    # Categóricas → inteiros (com codificação explícita)
    sexo = as.integer(factor(str_to_lower(sexo), levels = c("masculino", "feminino"))),
    vac_sarscov2 = as.integer(factor(vac_sarscov2)),
    hospitalizacao = as.integer(factor(hospitalizacao)),
    febre = as.integer(factor(febre)),
    
    # Codificação correta: NEGATIVO = 0, POSITIVO = 1
    resultado_influenza = as.integer(factor(str_to_upper(resultado_influenza),
                                            levels = c("NEGATIVO", "POSITIVO"))) - 1,
    
    resultado_sars_cov_2 = as.integer(factor(str_to_upper(resultado_sars_cov_2),
                                             levels = c("NEGATIVO", "POSITIVO"))) - 1,
    
    rsv = as.integer(factor(rsv)),
    hemograma = as.integer(factor(hemograma)),
    bioquimica = as.integer(factor(bioquimica))
  )

# Tratar Sintomas --------------------------------------------------
# Função para normalizar os nomes das categorias automaticamente
normalizar_categorias <- function(valores) {
  # Remover caracteres especiais, transformar em minúsculas e eliminar espaços extras
  valores <- tolower(valores)
  valores <- gsub("[^a-z0-9 ]", "", valores)
  valores <- gsub(" +", " ", valores)
  
  # Criar um dicionário de termos únicos usando distância de strings
  termos_unicos <- unique(valores)
  grupos <- list()
  
  # Definir um limiar para considerar duas strings como iguais (~85% de semelhança)
  limiar <- 0.15  # 0 = idêntico, 1 = totalmente diferente
  
  for (termo in termos_unicos) {
    encontrado <- FALSE
    for (grupo in names(grupos)) {
      if (stringdist::stringdist(termo, grupo, method = "jaccard") < limiar) {
        grupos[[grupo]] <- c(grupos[[grupo]], termo)
        encontrado <- TRUE
        break
      }
    }
    if (!encontrado) {
      grupos[[termo]] <- c(termo)
    }
  }
  
  # Criar um mapeamento dos valores originais para os termos padronizados
  mapeamento <- setNames(rep(names(grupos), lengths(grupos)), unlist(grupos))
  
  # Aplicar o mapeamento aos valores originais
  valores_padronizados <- unname(mapeamento[valores])
  
  return(valores_padronizados)
}

# Função para criar colunas binárias com nomes normalizados
criar_colunas_binarias <- function(base, coluna) {
  # Aplicar normalização
  base[[coluna]] <- normalizar_categorias(base[[coluna]])
  
  # Obter valores únicos corrigidos
  valores_unicos <- unique(unlist(strsplit(paste(base[[coluna]], collapse = " "), " ")))
  
  # Criar colunas binárias
  for (valor in valores_unicos) {
    base[[valor]] <- ifelse(grepl(valor, base[[coluna]]), 1, 0)
  }
  
  return(base)
}

# Aplicar a função para cada coluna relevante
df <- criar_colunas_binarias(df, "sintomas")
df <- criar_colunas_binarias(df, "comorbidades")

df <- df %>% select(-c(
  data_da_colheita, hospitalizacao_motivo,
  sintomas, comorbidades))

df <- df %>%
  mutate(
    # Consolidar sintomas com nomes inconsistentes
    nauseas = pmax(nauseas, nuseas, na.rm = FALSE),
    vomitos = pmax(vomitos, vmitos, na.rm = FALSE),
    dificuldaderespiratoria = pmax(dificuldaderespiratoria, dificuldaderespiratria, na.rm = FALSE),
    otorreiaotalgia = pmax(otorreiaotalgia, otorreiaotalgia1, na.rm = FALSE),
    aumentodasamigdalas = pmax(aumentodasamigdalas, aumentodasamgdalas, na.rm = FALSE),
    dortoraxica = pmax(dortoraxica, dortorxica, na.rm = FALSE),
    congestonasal = pmax(congestonasal, congestaonasal, na.rm = FALSE),
    
    # Agrupar sintomas clinicamente equivalentes
    mialgia_articular = pmax(mialgia, dornasarticulaes, dormuscular, na.rm = TRUE),
    congestao_nasal   = pmax(congestonasal, corriza, na.rm = TRUE),
    dispneia          = pmax(faltadear, dificuldaderespiratoria, na.rm = TRUE),
    perda_sentidos    = pmax(perdadeolfacto, perdadepaladar, na.rm = TRUE)
  ) %>%
  select(
    # Remover colunas inconsistentes
    -nuseas, -vmitos, -otorreiaotalgia1,
    -dificuldaderespiratria, -aumentodasamgdalas,
    -dortorxica, -congestaonasal,
    
    # Remover colunas agrupadas
    -mialgia, -dornasarticulaes, -dormuscular,
    -corriza, -faltadear, -dificuldaderespiratoria,
    -perdadeolfacto, -perdadepaladar,
    # Nao necessarias
    -tratamentocomantibiticosnasltimas, -option1,
    -doencarespiratoriacronica, -doenaneuromuscular,
    -doencahepaticacronica,-doenarenalcrnica,
    
    # Elevada % de outliers
    -hemograma, -bioquimica, -rsv, hemorragias
  ) %>%
  mutate(sintomas_dias = as.numeric(sintomas_dias)) %>% 
  mutate(across(
    .cols = where(~ is.numeric(.x) && n_distinct(.x, na.rm = TRUE) == 2),
    .fns = ~ as.factor(.x)
  ))

# Limpeza de outliers -----------------------------------------------------
remove_outliers_todos_consistentes <- function(data, grupo, variaveis_numericas) {
  data_original <- data
  filtros <- rep(TRUE, nrow(data))  # vetor lógico para manter as linhas válidas
  
  for (var in variaveis_numericas) {
    # Calcular limites por grupo
    stats <- data %>%
      group_by(.data[[grupo]]) %>%
      summarise(
        Q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[var]], 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        IQR = Q3 - Q1,
        lim_inf = Q1 - 1.5 * IQR,
        lim_sup = Q3 + 1.5 * IQR
      )
    
    # Juntar limites ao dataset original
    data_com_limites <- left_join(data_original, stats, by = grupo)
    
    # Aplicar filtro usando colunas padrão (sem .data)
    valores <- data_com_limites[[var]]
    lim_inf <- data_com_limites$lim_inf
    lim_sup <- data_com_limites$lim_sup
    
    filtro_var <- is.na(valores) | (valores >= lim_inf & valores <= lim_sup)
    
    # Atualizar filtro geral
    filtros <- filtros & filtro_var
  }
  
  # Retornar base limpa
  return(data_original[filtros, ])
}

vars_numericas <- c("idade", "sintomas_dias", "sintomas_freq_resp")

df_sem_outliers_flu <- remove_outliers_todos_consistentes(
  data = df,
  grupo = "resultado_influenza",
  variaveis_numericas = vars_numericas
)


df_sem_outliers_sars <- remove_outliers_todos_consistentes(
  data = df,
  grupo = "resultado_sars_cov_2",
  variaveis_numericas = vars_numericas
)

# MICE --------------------------------------------------------------------
# Definir limiar de missing (ex: 30%)
limiar_na <- 0.35

# Calcular percentagem de missing por variável
porc_missing_flu <- colMeans(is.na(df_sem_outliers_flu))
porc_missing_sars <- colMeans(is.na(df_sem_outliers_sars))

# Selecionar colunas com < 30% de missing
colunas_para_imputar_flu <- names(porc_missing_flu[porc_missing_flu > 0 & porc_missing_flu < limiar_na])
colunas_para_imputar_sars <- names(porc_missing_sars[porc_missing_sars > 0 & porc_missing_sars < limiar_na])
# Preparar dataset apenas com essas colunas
df_sub_flu <- df_sem_outliers_flu[, colunas_para_imputar_flu]
df_sub_sars <- df_sem_outliers_sars[, colunas_para_imputar_sars]
# Obter métodos automáticos sugeridos
ini_flu <- mice(df_sub_flu, maxit = 0)
ini_sars <- mice(df_sub_sars, maxit = 0)
metodos_flu <- ini_flu$method
metodos_sars <- ini_sars$method

# Aplicar imputação
imp_flu <- mice(df_sub_flu, m = 5, method = metodos_flu, seed = 123)
imp_sars <- mice(df_sub_sars, m = 5, method = metodos_sars, seed = 123)

# Extrair o dataset imputado
df_sub_imputado_flu <- complete(imp_flu)
df_sub_imputado_sars <- complete(imp_sars)
# Substituir colunas imputadas no dataframe original
df_sem_outliers_flu[colunas_para_imputar_flu] <- df_sub_imputado_flu
df_sem_outliers_sars[colunas_para_imputar_sars] <- df_sub_imputado_sars

# Influenza ---------------------------------------------------------------
# Criar tabela resumida com gtsummary ------------------------------------------
desc_geral <- df_sem_outliers_flu %>%
  select(-resultado_sars_cov_2) %>% 
  tbl_summary(
    by = resultado_influenza,
    statistic = list(
      all_continuous() ~ "{median} ({min} to {max})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,  
      all_categorical() ~ c(0, 0, 1)  
    ),
    missing_text = "Missing") %>% 
  add_overall() %>% 
    add_ci() %>% 
  add_p()

# Print the table
desc_geral

desc_geral_hux <- as_hux_table(desc_geral)
quick_xlsx(desc_geral_hux, file = "tabela_descricao_flu.xlsx")

# SARS-CoV-2 --------------------------------------------------------------
# Criar tabela resumida com gtsummary ------------------------------------------
desc_geral <- df_sem_outliers_sars %>%
  select(-resultado_influenza) %>% 
  tbl_summary(
    by = resultado_sars_cov_2,
    statistic = list(
      all_continuous() ~ "{median} ({min} to {max})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,  
      all_categorical() ~ c(0, 0, 1)  
    ),
    missing_text = "Missing") %>% 
  add_overall() %>% 
  add_ci() %>% 
  add_p()

# Print the table
desc_geral

desc_geral_hux <- as_hux_table(desc_geral)
quick_xlsx(desc_geral_hux, file = "tabela_descricao_sars.xlsx")

# -------------------------------------------------------------------------
# Normalizar variaveis continuas ------------------------------------------
df_sem_outliers_flu[vars_numericas] <- scale(df_sem_outliers_flu[vars_numericas])
df_sem_outliers_sars[vars_numericas] <- scale(df_sem_outliers_sars[vars_numericas])
# -------------------------------------------------------------------------
# Exportar a base de dados 
rio::export(df_sem_outliers_flu, 'flu_clean.xlsx')
rio::export(df_sem_outliers_sars, 'sars-clean.xlsx')

# Selecao de variaveis ----------------------------------------------------

# Construcao de Modelos --------------------------------------------------
# Carregando pacotes necessários
library(caret)
library(pROC)
library(tidyverse)
library(smotefamily)
library(PRROC)
library(randomForest)
library(xgboost)

# Função para ajustar e avaliar modelos
run_model <- function(X, y, seed, balance_method = "none") {
  cat(sprintf("\nIniciando modelo para seed %d, balanceamento: %s...\n", seed, balance_method))
  set.seed(seed)
  
  # Garantindo que todas as colunas de X sejam numéricas
  X <- as.data.frame(lapply(X, function(col) {
    if (is.factor(col)) {
      as.numeric(as.character(col))  # Converte fator para caractere, depois para numérico
    } else {
      col  # Mantém colunas já numéricas
    }
  }))
  
  # Feature engineering: categorizando idade
  X$idade_cat <- cut(X$idade, breaks = c(-Inf, 2, 5, 14, 55, 65, Inf), 
                     labels = c("Menor_2", "2_5", "5_14", "15_55", "55_65", ">65"))
  X$idade <- NULL  # Removendo idade contínua
  X$idade_cat <- as.numeric(X$idade_cat)  # Convertendo para numérico
  
  # Feature engineering: interações e respiratory_score
  X$febre_dispneia <- X$febre * X$dispneia
  X$hospitalizacao_idade_cat <- X$hospitalizacao * X$idade_cat
  X$respiratory_score <- X$febre + X$dispneia + X$rinorria + X$sintomas_freq_resp
  
  # Verificando tipos de dados após conversão
  cat("\nTipos de dados das features após conversão:\n")
  print(sapply(X, class))
  
  # Escalonamento das features
  preProc <- preProcess(X, method = c("center", "scale"))
  X_scaled <- predict(preProc, X)
  
  # Convertendo y para fator com níveis válidos
  y <- as.factor(y)
  levels(y) <- c("Negative", "Positive")
  
  # Divisão treino-teste (60% treino, ~7,475 samples)
  trainIndex <- createDataPartition(y, p = 0.6, list = FALSE)
  X_train <- X_scaled[trainIndex, ]
  y_train <- y[trainIndex]
  X_test <- X_scaled[-trainIndex, ]
  y_test <- y[-trainIndex]
  
  # Balanceamento apenas no treino
  if (balance_method == "SMOTE") {
    tryCatch({
      smote_result <- smotefamily::SMOTE(X_train, as.numeric(y_train) - 1, K = 5, dup_size = 1)
      data_balanced <- smote_result$data
      colnames(data_balanced)[ncol(data_balanced)] <- "resultado_influenza"
      data_balanced$resultado_influenza <- as.factor(data_balanced$resultado_influenza)
      levels(data_balanced$resultado_influenza) <- c("Negative", "Positive")
    }, error = function(e) {
      stop("Erro ao aplicar SMOTE: ", e$message)
    })
    X_train_bal <- data_balanced %>% select(-resultado_influenza)
    y_train_bal <- data_balanced$resultado_influenza
  } else if (balance_method == "ADASYN") {
    tryCatch({
      adasyn_result <- smotefamily::ADAS(X_train, as.numeric(y_train) - 1, K = 5)
      data_balanced <- adasyn_result$data
      colnames(data_balanced)[ncol(data_balanced)] <- "resultado_influenza"
      data_balanced$resultado_influenza <- as.factor(data_balanced$resultado_influenza)
      levels(data_balanced$resultado_influenza) <- c("Negative", "Positive")
    }, error = function(e) {
      stop("Erro ao aplicar ADASYN: ", e$message)
    })
    X_train_bal <- data_balanced %>% select(-resultado_influenza)
    y_train_bal <- data_balanced$resultado_influenza
  } else {
    X_train_bal <- X_train
    y_train_bal <- y_train
  }
  
  # Verificando balanceamento
  cat(sprintf("\nDistribuição de resultado_influenza após %s (treino):\n", balance_method))
  print(table(y_train_bal))
  
  # Combinando dados para treinamento
  data <- data.frame(X_train_bal, resultado_influenza = y_train_bal)
  
  # Configuração do cross-validation
  train_control <- trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    savePredictions = "final"
  )
  
  # Random Forest
  cat("\nTreinando Random Forest...\n")
  rf_model <- train(
    resultado_influenza ~ .,
    data = data,
    method = "rf",
    trControl = train_control,
    tuneGrid = data.frame(mtry = c(2, 4, 6)),
    metric = "ROC",
    ntree = 100
  )
  
  # XGBoost
  cat("\nTreinando XGBoost...\n")
  xgb_model <- train(
    resultado_influenza ~ .,
    data = data,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = expand.grid(
      nrounds = 100,
      max_depth = c(3, 6),
      eta = 0.1,
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.7
    ),
    metric = "ROC",
    verbosity = 0
  )
  
  # Ajuste de limiar para maximizar F1-score
  threshold_rf <- thresholder(rf_model, threshold = seq(0.1, 0.9, by = 0.1), 
                              final = TRUE, statistics = "all")
  f1_rf <- 2 * threshold_rf$Precision * threshold_rf$Sensitivity / 
    (threshold_rf$Precision + threshold_rf$Sensitivity)
  f1_rf[is.nan(f1_rf)] <- 0  # Substituir NaN por 0
  best_threshold_rf <- threshold_rf[which.max(f1_rf), "prob_threshold"]
  
  threshold_xgb <- thresholder(xgb_model, threshold = seq(0.1, 0.9, by = 0.1), 
                               final = TRUE, statistics = "all")
  f1_xgb <- 2 * threshold_xgb$Precision * threshold_xgb$Sensitivity / 
    (threshold_xgb$Precision + threshold_xgb$Sensitivity)
  f1_xgb[is.nan(f1_xgb)] <- 0  # Substituir NaN por 0
  best_threshold_xgb <- threshold_xgb[which.max(f1_xgb), "prob_threshold"]
  
  # Previsões no conjunto de teste
  rf_probs <- predict(rf_model, X_test, type = "prob")[, "Positive"]
  rf_pred <- factor(ifelse(rf_probs > best_threshold_rf, "Positive", "Negative"), 
                    levels = c("Negative", "Positive"))
  
  xgb_probs <- predict(xgb_model, X_test, type = "prob")[, "Positive"]
  xgb_pred <- factor(ifelse(xgb_probs > best_threshold_xgb, "Positive", "Negative"), 
                     levels = c("Negative", "Positive"))
  
  # Calculando métricas
  cm_rf <- confusionMatrix(rf_pred, y_test)
  cm_xgb <- confusionMatrix(xgb_pred, y_test)
  
  pr_rf <- PRROC::pr.curve(scores.class0 = rf_probs[y_test == "Positive"], 
                           scores.class1 = rf_probs[y_test == "Negative"], curve = TRUE)
  pr_xgb <- PRROC::pr.curve(scores.class0 = xgb_probs[y_test == "Positive"], 
                            scores.class1 = xgb_probs[y_test == "Negative"], curve = TRUE)
  
  roc_rf <- pROC::roc(y_test, rf_probs, levels = c("Negative", "Positive"), direction = "<")
  roc_xgb <- pROC::roc(y_test, xgb_probs, levels = c("Negative", "Positive"), direction = "<")
  
  # Intervalos de confiança para AUC
  ci_rf <- pROC::ci.auc(roc_rf)
  ci_xgb <- pROC::ci.auc(roc_xgb)
  
  # Resultados
  cat(sprintf("\nResultados para seed %d, balanceamento: %s\n", seed, balance_method))
  cat("\nRandom Forest:\n")
  cat(sprintf("Melhor score ROC (CV): %.4f\n", max(rf_model$results$ROC)))
  cat(sprintf("AUC-PR (teste): %.4f\n", pr_rf$auc.integral))
  cat(sprintf("AUC 95%% CI: [%.4f, %.4f]\n", ci_rf[1], ci_rf[3]))
  cat(sprintf("Melhor limiar (max F1): %.2f\n", best_threshold_rf))
  cat(sprintf("F1-Score (teste): %.4f\n", cm_rf$byClass["F1"]))
  cat("\nRelatório de classificação (Random Forest, teste):\n")
  print(cm_rf)
  
  cat("\nXGBoost:\n")
  cat(sprintf("Melhor score ROC (CV): %.4f\n", max(xgb_model$results$ROC)))
  cat(sprintf("AUC-PR (teste): %.4f\n", pr_xgb$auc.integral))
  cat(sprintf("AUC 95%% CI: [%.4f, %.4f]\n", ci_xgb[1], ci_xgb[3]))
  cat(sprintf("Melhor limiar (max F1): %.2f\n", best_threshold_xgb))
  cat(sprintf("F1-Score (teste): %.4f\n", cm_xgb$byClass["F1"]))
  cat("\nRelatório de classificação (XGBoost, teste):\n")
  print(cm_xgb)
  
  # Importância das variáveis
  cat("\nImportância das variáveis (Random Forest):\n")
  print(varImp(rf_model))
  cat("\nImportância das variáveis (XGBoost):\n")
  print(varImp(xgb_model))
  
  cat(sprintf("\nModelo para seed %d, balanceamento %s concluído.\n", seed, balance_method))
  return(list(rf_model = rf_model, xgb_model = xgb_model, 
              rf_threshold = best_threshold_rf, xgb_threshold = best_threshold_xgb,
              rf_cm = cm_rf, xgb_cm = cm_xgb))
}

# Função principal
main <- function(df) {
  # Features selecionadas
  features <- c(
    'calafrios', 'conjutivite', 'diabetes', 'dispneia', 'dorabdominal',
    'dortoraxica', 'febre', 'fraqueza', 'hospitalizacao', 'idade',
    'malestargeral', 'nenhumacclinica', 'otorreiaotalgia', 'perda_sentidos',
    'prurido', 'rinorria', 'sexo', 'sintomas_dias', 'sintomas_freq_resp',
    'tbnoltimoano', 'vac_sarscov2'
  )
  
  # Separando features e target
  X <- df[, features]
  y <- df$resultado_influenza
  
  # Verificando tipos de dados antes da conversão
  cat("Tipos de dados originais das features selecionadas:\n")
  print(sapply(X, class))
  
  # Verificando balanceamento das classes
  cat("\nDistribuição de resultado_influenza:\n")
  print(table(y))
  
  # Convertendo y para fator com níveis válidos
  y <- as.factor(y)
  levels(y) <- c("Negative", "Positive")
  
  # Usando um único seed
  seed <- 42
  
  # Modelos com diferentes técnicas de balanceamento
  models_smote <- run_model(X, y, seed, balance_method = "SMOTE")
  models_adasyn <- run_model(X, y, seed, balance_method = "ADASYN")
  
  cat("\nTodos os modelos concluídos.\n")
  return(list(smote = models_smote, adasyn = models_adasyn))
}

# Executando
models <- main(df_sem_outliers_flu)

