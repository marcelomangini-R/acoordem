# Código 2: Tratamento de Missing Data com Imputação por Penalização
# ====================================================================

# Carregar bibliotecas adicionais
library(ggplot2)
library(reshape2)
library(scales)
library(openxlsx)
# Usar os dados já invertidos do código anterior
# Se executando separadamente, descomente a linha abaixo:
#dados_invertidos_final <- read_excel("Banco45AFE.xlsx")

# Definir todas as variáveis para tratamento de missing data
variaveis_missing <- c("MD_PT1", "MD_PT2", "MD_PF", "MD_MC1", "MD_MC2", 
                       "MD_C", "MD_P", "MT_S", "MT_C", "MR_Q", "MR_C",
                       "CL_1", "CL_2", "MT_F", "CB_A", 
                       "CB_R", "CP_A1", "CP_A2", "CP_P", "CE_P1", "CE_P2", "CE_T")

# Análise inicial de missing data
missing_antes <- sapply(variaveis_missing, function(x) {
  if (x %in% names(dados_invertidos_final)) {
    sum(is.na(dados_invertidos_final[[x]]))
  } else {
    NA
  }
})

# Remover variáveis que não existem no dataset
variaveis_existentes_missing <- variaveis_missing[variaveis_missing %in% names(dados_invertidos_final)]
missing_antes <- missing_antes[!is.na(missing_antes)]

# Calcular percentual de missing data
total_observacoes <- nrow(dados_invertidos_final)
missing_percentual_antes <- round((missing_antes / total_observacoes) * 100, 2)

# Função para imputação por penalização
imputacao_penalizacao <- function(dados, variaveis) {
  dados_imputados <- dados
  
  for (var in variaveis) {
    if (var %in% names(dados)) {
      # Calcular o pior escore (mínimo) da amostra para a variável
      pior_escore <- min(dados[[var]], na.rm = TRUE)
      
      # Contar quantos NAs existem
      n_nas <- sum(is.na(dados[[var]]))
      
      if (n_nas > 0) {
        # Substituir NAs pelo pior escore
        dados_imputados[[var]][is.na(dados[[var]])] <- pior_escore
        cat("Variável", var, ":", n_nas, "NAs imputados com valor", pior_escore, "\n")
      } else {
        cat("Variável", var, ": Nenhum NA encontrado\n")
      }
    }
  }
  
  return(dados_imputados)
}

# Aplicar imputação por penalização
dados_final <- imputacao_penalizacao(dados_invertidos_final, variaveis_existentes_missing)

# Verificar missing data após imputação
missing_depois <- sapply(variaveis_existentes_missing, function(x) sum(is.na(dados_final[[x]])))

# Criar tabela de resumo do missing data
tabela_missing <- data.frame(
  Variavel = names(missing_antes),
  Missing_Antes = missing_antes,
  Percentual_Antes = missing_percentual_antes,
  Missing_Depois = missing_depois,
  Dados_Imputados = missing_antes - missing_depois
) %>%
  arrange(desc(Missing_Antes))

# Criar flextable para missing data
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

# Exibir tabela no Viewer
print(ft_missing)

# Criar gráfico de barras para missing data (apenas variáveis com missing data)
variaveis_com_missing <- tabela_missing[tabela_missing$Missing_Antes > 0, ]

if(nrow(variaveis_com_missing) > 0) {
  # Preparar dados para o gráfico
  dados_grafico <- variaveis_com_missing %>%
    select(Variavel, Missing_Antes, Dados_Imputados) %>%
    melt(id.vars = "Variavel", variable.name = "Tipo", value.name = "Quantidade")
  
  # Renomear os níveis para melhor visualização
  levels(dados_grafico$Tipo) <- c("Missing Original", "Dados Imputados")
  
  # Criar gráfico de barras
  grafico_missing <- ggplot(dados_grafico, aes(x = reorder(Variavel, Quantidade), 
                                               y = Quantidade, fill = Tipo)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    coord_flip() +
    scale_fill_manual(values = c("Missing Original" = "#E53E3E", 
                                 "Dados Imputados" = "#38A169")) +
    labs(title = "Missing Data: Antes e Após Imputação por Penalização",
         subtitle = paste("Total de", total_observacoes, "observações"),
         x = "Variáveis",
         y = "Quantidade de Observações",
         fill = "Status") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray60"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    geom_text(aes(label = Quantidade), 
              position = position_dodge(width = 0.9), 
              hjust = -0.1, size = 3)
  
  # Exibir gráfico no painel Plots
  print(grafico_missing)
  
} else {
  cat("Nenhuma variável apresentou missing data.\n")
  
  # Criar gráfico alternativo mostrando que não há missing data
  grafico_sem_missing <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_text(aes(label = "Nenhum Missing Data\nDetectado"), 
              size = 8, color = "#38A169", fontface = "bold") +
    xlim(0, 2) + ylim(0, 2) +
    theme_void() +
    labs(title = "Status do Missing Data",
         subtitle = paste("Análise de", length(variaveis_existentes_missing), "variáveis")) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60")
    )
  
  print(grafico_sem_missing)
}

# Resumo final
cat("\n=== RESUMO FINAL ===\n")
cat("Total de variáveis analisadas:", length(variaveis_existentes_missing), "\n")
cat("Variáveis com missing data:", sum(missing_antes > 0), "\n")
cat("Total de dados imputados:", sum(missing_antes), "\n")
cat("Taxa geral de missing data:", round(sum(missing_antes) / (total_observacoes * length(variaveis_existentes_missing)) * 100, 2), "%\n")

# Salvar dados finais (opcional)
 write.xlsx(dados_final, "AFE68.xlsx")
