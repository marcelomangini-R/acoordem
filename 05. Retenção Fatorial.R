####################################################################
# Autor: Marcelo Mangini Dias
# Data: 21/09/2025
# Última Atualização: 23/10/2025
# Objetivo: Retenção fatorial
####################################################################

####################################################################
# Bibliotecas
####################################################################

library(psych)
library(EGAnet)
library(EFAtools)
library(dplyr)
library(ggplot2)
library(flextable)
library(paran)
library(openxlsx)
library(readxl)

####################################################################
# Banco de dados, parâmetros e variáveis
####################################################################

dados <- read_excel("AFE.xlsx")

set.seed(123)  
n_iteracoes <- 1000  
cor_method <- "pearson" 

####################################################################
# Análise Paralela
####################################################################
tryCatch({
  paralela_pc <- paran(dados, iterations = n_iteracoes, graph = TRUE, quietly = FALSE)
  n_paralela_pc <- ifelse(!is.null(paralela_pc$Retained), paralela_pc$Retained, NA)
  cat("Análise Paralela (PC):", n_paralela_pc, "componentes\n\n")
}, error = function(e) {
  cat("Erro na análise paralela PCA:", e$message, "\n")
  n_paralela_pc <- NA
})

n_paralela_pc <- paralela_pc$Retained


####################################################################
# BIC / AIC
####################################################################

if (!require(lavaan)) install.packages("lavaan"); library(lavaan)


bic_values <- numeric(6)
aic_values <- numeric(6)

for (n_f in 1:6) {
  tryCatch({
    model_spec <- paste(paste0("f =~ ", paste(colnames(dados), collapse = " + ")))
    
    # Ajusta modelo com lavaan
    fit <- cfa(model_spec, data = dados, estimator = "ML", 
               std.lv = TRUE, optim.method = "nlminb")
    
    if (lavInspect(fit, "converged")) {
      bic_values[n_f] <- fitMeasures(fit, "bic")
      aic_values[n_f] <- fitMeasures(fit, "aic")
      cat(sprintf("  %d fatores - BIC: %.1f, AIC: %.1f\n", 
                  n_f, bic_values[n_f], aic_values[n_f]))
    }
  }, error = function(e) {
    cat("  Erro com", n_f, "fatores no lavaan:", e$message, "\n")
  })
}

n_bic <- which.min(bic_values)
n_aic <- which.min(aic_values)
cat("\nBIC (lavaan) mínimo com:", n_bic, "fatores\n")
cat("AIC (lavaan) mínimo com:", n_aic, "fatores\n")


####################################################################
# HULL
####################################################################

comunalidades_df <- data.frame(n_fatores = 1:6, comm_mean = NA)

for (n_f in 1:6) {
  tryCatch({
    modelo_paf <- fa(dados, nfactors = n_f, fm = "pa", rotate = "none")
    comunalidades_df$comm_mean[n_f] <- mean(modelo_paf$communalities)
    cat(sprintf("  %d fatores - Comunalidade média: %.3f\n", 
                n_f, comunalidades_df$comm_mean[n_f]))
  }, error = function(e) {
    cat("  Erro com", n_f, "fatores no PAF:", e$message, "\n")
  })
}

if (sum(!is.na(comunalidades_df$comm_mean)) >= 3) {
  valid_data <- comunalidades_df[!is.na(comunalidades_df$comm_mean), ]
  
  valid_data <- valid_data %>%
    mutate(
      ganho = comm_mean - lag(comm_mean),
      ganho_relativo = ganho / n_fatores
    )

    n_hull <- valid_data$n_fatores[which.max(valid_data$ganho_relativo) + 1]
  cat("Método Hull sugere:", n_hull, "fatores\n")
} else {
  n_hull <- NA
  cat("Dados insuficientes para Hull alternativo\n")
}

####################################################################
# EGA
####################################################################

ega_tmfg <- EGA(
  data = dados,
  model = "GLASSO",  
  algorithm = "walktrap",
  corr = cor_method,
  plot.EGA = TRUE,
  seed = 123,
  verbose = FALSE
)
n_ega <- ega_tmfg$n.dim


####################################################################
# Resumo dos resultados
####################################################################
tabela_resultados <- data.frame(
  metodo = c(
    "Análise Paralela (PC)",
    "BIC (lavaan)",
    "AIC (lavaan)",
    "Hull",
    "EGA"
  ),
  n_fatores = c(
    n_paralela_pc,
    n_bic,
    n_aic,
    n_hull,
    n_ega
  )
)

ft <- flextable(tabela_resultados) %>%
  set_header_labels(
    metodo = "Método de Retenção Fatorial",
    n_fatores = "Número de Fatores Sugerido"
  ) %>%
  theme_booktabs() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 11, part = "all")

ft