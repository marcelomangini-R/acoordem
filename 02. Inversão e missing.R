####################################################################
# Autor: Marcelo Mangini Dias
# Data: 04/09/2025
# Última Atualização: 01/11/2025
# Objetivo: Inversão de escores de itens 
#            cujo maior valor é o menos desejável
####################################################################

####################################################################
# Bibliotecas
####################################################################
library(readxl)
library(dplyr)
library(flextable)
library(ggplot2)
library(reshape2)
library(scales)
library(openxlsx)

####################################################################
# Banco de dados
####################################################################
#dados <- read_excel("BancoAFE.xlsx")
dados <- read_excel("AFC2.xlsx")
dados_original <- dados

####################################################################
# Garantir que as variáveis do instrumento estejam numéricas
# (evita "Error in max_val + min_val : argumento não-numérico para
# operador binário" quando alguma coluna vem como character do Excel
# por causa de #NULL!, vírgula decimal, texto solto etc.)
####################################################################
variaveis_numericas_instrumento <- c(
  "idade", "sexo", "idademeses", "mpreferida", "Tipoescola",
  "MD_PT1", "MD_PT2", "MD_PF", "MD_P", "MD_MC1", "MD_MC2", "MD_C",
  "MT_S", "MT_C", "MT_F", "MR_Q", "MR_C",
  "CB_A", "CB_R", "CP_A1", "CP_A2", "CP_P",
  "CL_1", "CL_2", "CE_P1", "CE_P2", "CE_T"
)

garantir_numericas <- function(dados, variaveis) {
  for (v in intersect(variaveis, names(dados))) {
    if (!is.numeric(dados[[v]])) {
      valor_original <- dados[[v]]
      valor_convertido <- suppressWarnings(as.numeric(valor_original))
      
      problematicos <- which(is.na(valor_convertido) &
                               !is.na(valor_original) &
                               trimws(as.character(valor_original)) != "")
      
      if (length(problematicos) > 0) {
        cat("\n---", v, "--- valor(es) original(is) que NÃO converteram (linha: valor):\n")
        print(data.frame(linha = problematicos, valor_original = valor_original[problematicos]))
      } else {
        cat("Variável", v, ": convertida de", class(valor_original)[1], "para numeric sem perda.\n")
      }
      
      dados[[v]] <- valor_convertido
    }
  }
  return(dados)
}

dados <- garantir_numericas(dados, variaveis_numericas_instrumento)
dados_original <- dados

####################################################################
# Variáveis a serem invertidas
####################################################################
variaveis_inversao <- c("MD_PT1", "MD_PT2", "MD_PF", "MD_MC1", "MD_MC2", 
                        "MD_C", "MD_P", "MT_S", "MT_C", "MR_Q", "MR_C",
                        "CL_1", "CL_2")

####################################################################
# Função de inversão
####################################################################
inverter_escores <- function(dados, variaveis) {
  dados_invertidos <- dados
  for (var in variaveis) {
    if (var %in% names(dados)) {
      min_val <- min(dados[[var]], na.rm = TRUE)
      max_val <- max(dados[[var]], na.rm = TRUE)
      dados_invertidos[[var]] <- (max_val + min_val) - dados[[var]]
      cat("Variável", var, "invertida. Min:", min_val, "Max:", max_val, "\n")
    } else {
      cat("Variável", var, "não encontrada no dataset\n")
    }
  }
  
  return(dados_invertidos)
}

dados_invertidos <- inverter_escores(dados, variaveis_inversao)

####################################################################
# Resultados da Inversão
####################################################################

variaveis_existentes <- variaveis_inversao[variaveis_inversao %in% names(dados)]
variaveis_amostra <-variaveis_existentes

tabela_comparacao <- data.frame(
  Variavel = variaveis_amostra,
  Min_Original = sapply(variaveis_amostra, function(x) min(dados_original[[x]], na.rm = TRUE)),
  Max_Original = sapply(variaveis_amostra, function(x) max(dados_original[[x]], na.rm = TRUE)),
  Mean_Original = sapply(variaveis_amostra, function(x) round(mean(dados_original[[x]], na.rm = TRUE), 2)),
  Min_Invertido = sapply(variaveis_amostra, function(x) min(dados_invertidos[[x]], na.rm = TRUE)),
  Max_Invertido = sapply(variaveis_amostra, function(x) max(dados_invertidos[[x]], na.rm = TRUE)),
  Mean_Invertido = sapply(variaveis_amostra, function(x) round(mean(dados_invertidos[[x]], na.rm = TRUE), 2))
)

ft_inversao <- flextable(tabela_comparacao) %>%
  set_header_labels(
    Variavel = "Variável",
    Min_Original = "Min Original",
    Max_Original = "Max Original", 
    Mean_Original = "Média Original",
    Min_Invertido = "Min Invertido",
    Max_Invertido = "Max Invertido",
    Mean_Invertido = "Média Invertida"
  ) %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  width(width = 1.2) %>%
  add_header_row(
    values = c("", "Valores Originais", "Valores Invertidos"),
    colwidths = c(1, 3, 3)
  ) %>%
  bg(bg = "#E8F4F8", part = "header") %>%
  color(color = "#2C5282", part = "header") %>%
  bold(part = "header")

print(ft_inversao)

dados_invertidos_final <- dados_invertidos
cat("\nInversão de escores concluída para", length(variaveis_existentes), "variáveis.\n")
cat("Variáveis processadas:", paste(variaveis_existentes, collapse = ", "), "\n")

####################################################################
# Variáveis para tratamento de Missing por penalização (menor pontuação)
####################################################################

variaveis_missing <- c("MD_PT1", "MD_PT2", "MD_PF", "MD_MC1", "MD_MC2", 
                       "MD_C", "MD_P", "MT_S", "MT_C", "MR_Q", "MR_C",
                       "CL_1", "CL_2", "MT_F", "CB_A", 
                       "CB_R", "CP_A1", "CP_A2", "CP_P", "CE_P1", "CE_P2", "CE_T")

####################################################################
# Análise prévia
####################################################################
missing_antes <- sapply(variaveis_missing, function(x) {
  if (x %in% names(dados_invertidos_final)) {
    sum(is.na(dados_invertidos_final[[x]]))
  } else {
    NA
  }
})
variaveis_existentes_missing <- variaveis_missing[variaveis_missing %in% names(dados_invertidos_final)]
missing_antes <- missing_antes[!is.na(missing_antes)]
total_observacoes <- nrow(dados_invertidos_final)
missing_percentual_antes <- round((missing_antes / total_observacoes) * 100, 2)

####################################################################
# Função para imputação por penalização
####################################################################
imputacao_penalizacao <- function(dados, variaveis) {
  dados_imputados <- dados
  
  for (var in variaveis) {
    if (var %in% names(dados)) {
      pior_escore <- min(dados[[var]], na.rm = TRUE)
      n_nas <- sum(is.na(dados[[var]]))
      
      if (n_nas > 0) {
        dados_imputados[[var]][is.na(dados[[var]])] <- pior_escore
        cat("Variável", var, ":", n_nas, "NAs imputados com valor", pior_escore, "\n")
      } else {
        cat("Variável", var, ": Nenhum NA encontrado\n")
      }
    }
  }
  
  return(dados_imputados)
}

dados_final <- imputacao_penalizacao(dados_invertidos_final, variaveis_existentes_missing)

####################################################################
# Análise posterior
####################################################################

missing_depois <- sapply(variaveis_existentes_missing, function(x) sum(is.na(dados_final[[x]])))

tabela_missing <- data.frame(
  Variavel = names(missing_antes),
  Missing_Antes = missing_antes,
  Percentual_Antes = missing_percentual_antes,
  Missing_Depois = missing_depois,
  Dados_Imputados = missing_antes - missing_depois
) %>%
  arrange(desc(Missing_Antes))

ft_missing <- flextable(tabela_missing) %>%
  set_header_labels(
    Variavel = "Variável",
    Missing_Antes = "Missing Antes",
    Percentual_Antes = "% Missing",
    Missing_Depois = "Missing Depois", 
    Dados_Imputados = "Dados Imputados"
  ) %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  width(width = 1.4) %>%
  bg(bg = "#F7FAFC", part = "body") %>%
  bg(bg = "#E53E3E", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  color(~ Missing_Antes > 0, ~ Dados_Imputados, color = "#D53F8C") %>%
  bold(~ Missing_Antes > 0, ~ Dados_Imputados)

print(ft_missing)

cat("\n=== RESUMO FINAL ===\n")
cat("Total de variáveis analisadas:", length(variaveis_existentes_missing), "\n")
cat("Variáveis com missing data:", sum(missing_antes > 0), "\n")
cat("Total de dados imputados:", sum(missing_antes), "\n")
cat("Taxa geral de missing data:", round(sum(missing_antes) / (total_observacoes * length(variaveis_existentes_missing)) * 100, 2), "%\n")

####################################################################
# Gravar arquivo com inversões e tratamento de missing data
####################################################################
write.xlsx(dados_final, "AFC.xlsx")
