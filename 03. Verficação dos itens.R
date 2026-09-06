####################################################################
# Autor: Marcelo Mangini Dias
# Data: 13/09/2025
# Última Atualização: 01/11/2025
# Objetivo: Checagem de Teto/Piso e Variância dos Dados
####################################################################

####################################################################
# Bibliotecas
####################################################################
library(dplyr)
library(flextable)

####################################################################
# Banco de dados e variáveis de interesse
####################################################################
#dados_final <- read_excel("AFE.xlsx")
dados_final <- read_excel("AFC.xlsx")

todas_variaveis <- c("MD_PT1", "MD_PT2", "MD_PF", "MD_MC1", "MD_MC2", 
                     "MD_C", "MD_P", "MT_S", "MT_C", "MR_Q", "MR_C",
                     "CL_1", "CL_2", "MT_F", "CB_A", 
                     "CB_R", "CP_A1", "CP_A2", "CP_P", "CE_P1", "CE_P2", "CE_T")

variaveis_existentes <- todas_variaveis[todas_variaveis %in% names(dados_final)]

####################################################################
# Detecção de piso e teto
####################################################################
detectar_teto_piso <- function(dados, variaveis, limiar = 0.80) {
  resultados <- data.frame()
  
  for (var in variaveis) {
    if (var %in% names(dados)) {
      valores <- dados[[var]]
      valores_validos <- valores[!is.na(valores)]
      
      if (length(valores_validos) > 0) {
        # Calcular estatísticas básicas
        min_val <- min(valores_validos)
        max_val <- max(valores_validos)
        media <- mean(valores_validos)
        variancia <- var(valores_validos)
        desvio_padrao <- sd(valores_validos)
        
        freq_minimo <- sum(valores_validos == min_val) / length(valores_validos)
        freq_maximo <- sum(valores_validos == max_val) / length(valores_validos)
        
        efeito_piso <- freq_minimo >= limiar
        efeito_teto <- freq_maximo >= limiar
        
        variancia_baixa <- variancia < 0.01
        
        status_teto <- ifelse(efeito_teto, "SIM", "NÃO")
        status_piso <- ifelse(efeito_piso, "SIM", "NÃO")
        status_variancia <- ifelse(variancia_baixa, "BAIXA", "NORMAL")
        
        resultados <- rbind(resultados, data.frame(
          Variavel = var,
          Min = min_val,
          Max = max_val,
          Media = round(media, 2),
          Variancia = round(variancia, 4),
          DP = round(desvio_padrao, 3),
          Freq_Minimo = round(freq_minimo * 100, 1),
          Freq_Maximo = round(freq_maximo * 100, 1),
          Efeito_Piso = status_piso,
          Efeito_Teto = status_teto,
          Status_Variancia = status_variancia,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(resultados)
}

resultados_checagem <- detectar_teto_piso(dados_final, variaveis_existentes)

####################################################################
# Resultados
####################################################################
ft_checagem <- flextable(resultados_checagem) %>%
  set_header_labels(
    Variavel = "Variável",
    Min = "Mínimo",
    Max = "Máximo",
    Media = "Média",
    Variancia = "Variância",
    DP = "Desvio Padrão",
    Freq_Minimo = "% no Mínimo",
    Freq_Maximo = "% no Máximo",
    Efeito_Piso = "Efeito Piso",
    Efeito_Teto = "Efeito Teto",
    Status_Variancia = "Status Variância"
  ) %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  width(width = 0.9) %>%
  add_header_row(
    values = c("", "Estatísticas Descritivas", "Frequências Extremas (%)", "Problemas Detectados"),
    colwidths = c(1, 5, 2, 3)
  ) %>%
  bg(bg = "#4A90E2", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  # Colorir problemas detectados
  color(~ Efeito_Piso == "SIM", ~ Efeito_Piso, color = "#E53E3E") %>%
  color(~ Efeito_Teto == "SIM", ~ Efeito_Teto, color = "#E53E3E") %>%
  color(~ Status_Variancia == "BAIXA", ~ Status_Variancia, color = "#E53E3E") %>%
  bold(~ Efeito_Piso == "SIM", ~ Efeito_Piso) %>%
  bold(~ Efeito_Teto == "SIM", ~ Efeito_Teto) %>%
  bold(~ Status_Variancia == "BAIXA", ~ Status_Variancia) %>%
  # Destacar variâncias muito baixas
  bg(~ Variancia < 0.01, ~ Variancia, bg = "#FFF2F2")

print(ft_checagem)

problemas_piso <- sum(resultados_checagem$Efeito_Piso == "SIM")
problemas_teto <- sum(resultados_checagem$Efeito_Teto == "SIM")
problemas_variancia <- sum(resultados_checagem$Status_Variancia == "BAIXA")

cat("Total de variáveis analisadas:", nrow(resultados_checagem), "\n")
cat("Variáveis com efeito piso:", problemas_piso, "\n")
cat("Variáveis com efeito teto:", problemas_teto, "\n")
cat("Variáveis com variância baixa:", problemas_variancia, "\n")

if(problemas_piso > 0) {
  vars_piso <- resultados_checagem$Variavel[resultados_checagem$Efeito_Piso == "SIM"]
  cat("Variáveis com efeito piso:", paste(vars_piso, collapse = ", "), "\n")
}

if(problemas_teto > 0) {
  vars_teto <- resultados_checagem$Variavel[resultados_checagem$Efeito_Teto == "SIM"]
  cat("Variáveis com efeito teto:", paste(vars_teto, collapse = ", "), "\n")
}

if(problemas_variancia > 0) {
  vars_variancia <- resultados_checagem$Variavel[resultados_checagem$Status_Variancia == "BAIXA"]
  cat("Variáveis com variância baixa:", paste(vars_variancia, collapse = ", "), "\n")
}


