####################################################################
# Autor: Marcelo Mangini Dias
# Data: 22/10/2025
# Objetivo: Dividir base de dados em dois arquivos para AFs distintas
####################################################################

####################################################################
# Bibliotecas
##################################################################### Carregar pacotes necessários
library(readxl)
library(writexl)
library(dplyr)

####################################################################
# Preparação dos dados
#################################################################### 
dados <- read_excel("Dados Gerais.xlsx")

set.seed(123) # seed para reprodutibilidade
dados$id_original <- seq_len(nrow(dados)) # índice original

####################################################################
# Estratificação e divisão aleatória
#################################################################### 
banco_afe <- dados %>%
  group_by(Idade) %>%
  slice_sample(prop = 80/160, replace = FALSE) %>%
  ungroup()

ids_afe <- banco_afe$id_original
banco_afc <- dados %>%
  filter(!(id_original %in% ids_afe))

####################################################################
# Resultados
#################################################################### 
cat("Banco AFE:", nrow(banco_afe), "registros\n")
cat("Banco AFC:", nrow(banco_afc), "registros\n")
cat("\n--- Distribuição por Idade no Banco Original ---\n")
print(table(dados$Idade))
cat("\n--- Distribuição por Idade no Banco AFE ---\n")
print(table(banco_afe$Idade))
cat("\n--- Distribuição por Idade no Banco AFC ---\n")
print(table(banco_afc$Idade))

####################################################################
# Novos Bancos de Dados
#################################################################### 
write_xlsx(banco_afe, "BancoAFE.xlsx")
write_xlsx(banco_afc, "BancoAFC.xlsx")

cat("\n✓ Arquivo 'BancoAFE.xlsx' criado com sucesso!\n")
cat("✓ Arquivo 'BancoAFC.xlsx' criado com sucesso!\n")
