
################################################################################
# Regressão de Dado em Painel
# Cliente: Carlos Benasuly
# Demanda: Doutorado
################################################################################


################################################################################
# LIMPAR MEMÓRIA
################################################################################
rm(list=ls())
################################################################################

################################################################################
# DEFINIR DIRETÓRIO DE TRABALHAO
################################################################################
setwd("C:/Users/usuario/Documents/Modelo_Regressao/Dado_Painel")
################################################################################


################################################################################
# PACKAGES
################################################################################
library(readxl)
library(dplyr)
library(tidyr)
library(stringi)
library(plm)
library(foreign)
library(lmtest)
library(stargazer)
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(tseries)
################################################################################


################################################################################
# DATASET
################################################################################
Base_dados <- read_excel("Base_Volume_Credito_Rural.xlsx")

# Verificar a Estrutura
str(Base_Volume_Credito_Rural)

# Garantir: 'Municipio' e 'Ano' Estejam como Fatores
Base_dados <- Base_dados %>%
  mutate(Municipio = as.factor(Municipio),
         Ano = as.factor(Ano))
################################################################################


################################################################################
# Transformar em objeto de painel
################################################################################
painel <- pdata.frame(Base_dados, index = c("Municipio", "Ano"))

# Fórmulas dos Modelos
formula_rural <- Credito_Rural ~ Escore_Fator1 + Escore_Fator2 + Escore_Fator3 +
  Pessoa_Fisica + Pessoa_Juridica + Producao_Familiar +
  Financiamento_Mini + Financiamento_Pequeno + Financiamento_Medio + Financiamento_Grande


formula_nao_rural <- Credito_Nao_Rural ~ Escore_Fator1 + Escore_Fator2 + Escore_Fator3 +
  Pessoa_Fisica + Pessoa_Juridica + Producao_Familiar +
  Financiamento_Mini + Financiamento_Pequeno + Financiamento_Medio + Financiamento_Grande

################################################################################



################################################################################
# Leitura de Base de Dados (DATASET)
Base_dados <- read_excel("Base_Volume_Credito_Rural.xlsx")

# Transformar em Objeto de Painel
painel <- pdata.frame(Base_dados, index = c("Municipio", "Ano"))


# Modelo de Dados Empilhados (Pooled OLS Model)
# Crédito Rural
modelo_polled_rural <- plm(Credito_Rural1~Escore_Fator1+Escore_Fator2+Escore_Fator3+
                             Pessoa_Fisica + Pessoa_Juridica + Producao_Familiar+
                             Financiamento_Mini,
                       data = painel, 
                       model = "pooling")

summary(modelo_polled_rural)

################################################################################


################################################################################
# Modelo de Efeitos Fixos (Fixed Effects Estimation)
# Crédito Rural
modelo_FE_rural <- plm(Credito_Rural1~Escore_Fator1+Escore_Fator2+Escore_Fator3+
                         Pessoa_Fisica + Pessoa_Juridica + Producao_Familiar+
                         Financiamento_Pequeno, 
                       data = painel, 
                       model = "within")

# Coeficientes
summary(modelo_FE_rural)

# Estimate individual effects
fixef(modelo_FE_rural)
###############################################################################


shapiro.test(modelo_FE_rural$residuals)
shapiro.test(correcao$residuals)

# Ferramenta de Diagnóstico: Heterocesdaticidade
# Teste de Breusch-Pagan
bptest(modelo_FE_rural, studentize = FALSE)

# Interpretação:
# p-valor < 0.05 → há evidência de heterocedasticidade
# p-valor > 0.05 → resíduos com variância constante (homocedásticos)

# White's Standard Errors
# Coeficientes com erros padrão robustos para heterocedasticidade

# Erro Robusto para Matriz Variância/Covariância
summary(modelo_FE_rural, vcov. = function(x) vcovHC(x, type = "HC1", maxlag = 4))

# Erro Robusto de Driscoll e Kraay(1998)
summary(modelo_FE_rural, vcov. = function(x) vcovSCC(x, type = "HC1", maxlag = 4))

# Erro Robusto de Croissant e Millo (2008)
summary(modelo_FE_rural, vcov. = function(x) pvcovHC(x, method = "arellano", type = "HC1"))


#---------------------------------- Analise -----------------------------------#
# Os Resultados indicam que fatores representados pelo Escore_Fator3 exercem influência positiva 
# substancial sobre o volume de crédito rural nos municípios analisados.
# O Escore_Fator2 exerce efeito negativo altamente significativo. 
# O Escore_Fator1 não apresentou impacto estatisticamente siginificante.”
# O Modelo explica 99,1% (R² = 0.991) da variância dentro dos municípios ao longo do tempo.


#--------------------------------- Fator 2 ------------------------------------#
# CADA aumento de 1 unidade no Escore_Fator2 está associado a uma redução de 
# R$ 357,14 no Volume de Crédito Rural, mantendo os demais fatores constantes.

# Quanto maior a intensidade das atividades agropecuárias (especialmente em áreas 
# com baixo desmatamento recente), menor o volume de crédito rural.

# 1 - Autossuficiência: Regiões com alta produção agropecuária já estão bem estabelecidas e não precisam tanto de crédito novo. 
# 2 - Política pública: Pode haver menos incentivo de crédito em regiões que já têm forte presença agropecuária, especialmente se as políticas visarem fomentar áreas menos produtivas.
# 3 - Sazonalidade ou restrições ambientais: Pode refletir o comportamento das instituições financeiras em reduzir crédito em regiões muito desmatadas ou saturadas.


#--------------------------------- Fator 3 ------------------------------------#
# CADA aumento de 1 unidade no Escore_Fator3 está associado a um aumento de 
# R$ 1.879,07 no Volume de Crédito Rural.

# Quanto maior o PIB e a arrecadação via ICMS do município, maior o volume de crédito rural.

# Conclusão Simples
# Crédito rural é fortemente influenciado pela força econômica local, mais do que 
# pela simples presença de população ou atividades agropecuárias.

#------------------------------------------------------------------------------#



#Crédito Não Rural
modelo_fe_n_rural <- plm(Credito_Nao_Rural~Escore_Fator1+Escore_Fator2+Escore_Fator3,
                         data = painel, 
                         model = "within")

################################################################################


################################################################################
# Modelo Efeito Aleatorio (Random Effects Estimation)

# Credito Rural
modelo_RE_rural <- plm(Credito_Rural1 ~ Escore_Fator1 + Escore_Fator2 + Escore_Fator3 +
                         Pessoa_Fisica + Pessoa_Juridica + Financiamento_Mini,
                       data = painel, 
                       model = "random")

summary(modelo_RE_rural)


bptest(modelo_RE_rural, studentize = TRUE)

summary(modelo_RE_rural, vcov = function(x) vcovHC(x, method = "arellano"))

################################################################################


# Boxplot para Credito_Rural por Ano
ggplot(Base_dados, aes(x = as.factor(Ano), y = Credito_Rural)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Distribuição do Crédito Rural por Ano",
    x = "Ano",
    y = "Volume de Crédito Rural"
  ) +
  theme_gray()

ggplot(Base_dados, aes(x = as.factor(Ano), y = Credito_Nao_Rural)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Distribuição do Crédito Não Rural por Ano",
    x = "Ano",
    y = "Volume de Crédito Não Rural"
  ) +
  theme_gray()



# Converte os dados para formato longo
dados_long <- Base_dados %>%
  pivot_longer(
    cols = c(Credito_Rural, Credito_Nao_Rural),
    names_to = "Tipo_Credito",
    values_to = "Valor"
  )

# Cria o boxplot com facet
ggplot(dados_long, aes(x = as.factor(Ano), y = Valor, fill = Tipo_Credito)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  facet_wrap(~Tipo_Credito, scales = "free_y") +
  labs(
    title = "Distribuição dos Créditos Rural e Não Rural por Ano",
    x = "Ano",
    y = "Valor (R$)",
    fill = "Tipo de Crédito"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



Base_dados %>%
  summarise(
    media_rural = mean(Credito_Rural, na.rm = TRUE),
    mediana_rural = median(Credito_Rural, na.rm = TRUE),
    sd_rural = sd(Credito_Rural, na.rm = TRUE),
    q1_rural = quantile(Credito_Rural, 0.25, na.rm = TRUE),
    q3_rural = quantile(Credito_Rural, 0.75, na.rm = TRUE)
  )


Base_dados %>%
  group_by(Ano) %>%
  summarise(
    media_rural = mean(Credito_Rural, na.rm = TRUE),
    total_rural = sum(Credito_Rural, na.rm = TRUE),
    media_nao_rural = mean(Credito_Nao_Rural, na.rm = TRUE),
    total_nao_rural = sum(Credito_Nao_Rural, na.rm = TRUE)
  )

Base_dados %>%
  group_by(Municipio, UF) %>%
  summarise(
    total_rural = sum(Credito_Rural, na.rm = TRUE),
    total_nao_rural = sum(Credito_Nao_Rural, na.rm = TRUE) 
  ) %>%
  arrange(desc(total_rural)) %>%
  slice_head(n = 15)

ggplot(Base_dados, 
       aes(x = Credito_Rural)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histograma do Crédito Rural", x = "Valor", y = "Frequência")




ranking_rural <- Base_dados %>%
  group_by(Municipio) %>%
  summarise(Total_Credito_Rural = sum(Credito_Rural, na.rm = TRUE)) %>%
  arrange(desc(Total_Credito_Rural)) %>%
  slice_head(n = 20)

# Visualiza o ranking
print(ranking_rural)


ggplot(ranking_rural, aes(x = reorder(Municipio, Total_Credito_Rural), y = Total_Credito_Rural)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 Municípios com Maior Crédito Rural",
    x = "Município",
    y = "Total Crédito Rural (R$)"
  ) +
  theme_minimal()




#--------------------------------------------------------------------------#
coeftest(modelo_RE_rural)


# Extrair os componentes do erro
componentes <- ercomp(modelo_RE_rural)

# Visualizar tudo (opcional)
print(componentes)

# Acessar o vetor de theta
theta_valores <- componentes$theta

# Mostrar estatísticas de resumo
summary(theta_valores)

# Ou mostrar o valor médio
cat("Valor médio de theta:", round(mean(theta_valores), 4), "\n")







#Crédito Não Rural
modelo_re_n_rural <- plm(Credito_Nao_Rural~Escore_Fator1+Escore_Fator2+Escore_Fator3,
                         data = painel, 
                         model = "random")


################################################################################
# TESTE DIAGNOSTICOS

# F Test de Chow
plm::pFtest(modelo_FE_rural, modelo_polled_rural)

# # Teste LM de Breusch-Pagan
plm::plmtest(modelo_polled_rural, effect = "individual", type = "bp")

# Teste de Hausman para Escolher (FE x RE)
plm::phtest(modelo_RE_rural, modelo_FE_rural)


# Teste de Correlação Cross-setion (Breusch-Pagan LM)
plm::pcdtest(modelo_FE_rural, test = c("lm"))

# Teste de Correlação (Pesaran CD test for cross-sectional dependence in panels)
plm::pcdtest(modelo_FE_rural, test = c("cd"))


# Teste Correlação Serial (Breusch-Godfrey/Wooldridge)
plm::pbgtest(modelo_FE_rural)

plm::pwartest(modelo_FE_rural)


# Teste de Durbin-Watson 
plm::pdwtest(modelo_FE_rural)


# Teste de Raiz Unitaria
adf.test(modelo_FE_rural, k=2)


# Teste de Breusch-Pagan P/ Heterocedasticidade
# Teste para Homocedasticidade (variância constante) dos resíduos de Breusch-Pagan (1979):
lmtest::bptest(modelo_FE_rural, studentize = FALSE)
################################################################################











# Comparando modelos em uma única tabela

tabela_polled <- stargazer::stargazer(modelo_polled_rural,
                     type = "text",  # use "html" para HTML ou "latex" para LaTeX
                     title = "Resultados da Regressão em Painel ",
                     align = TRUE,
                     style = "all",
                     column.labels = c("Modelo em Pooled"),
                     dep.var.labels.include = FALSE,
                     covariate.labels = c("Escore Fator 1", "Escore Fator 2", "Escore Fator 3"),
                     keep.stat = c("aic", "bic", "rsq", 
                                   "adj.rsq", "n"),
                     omit.stat = c("f", "ser"),
                     digits = 3)

write(tabela_polled, "resultado1.html") 



stargazer::stargazer(modelo_polled_rural, modelo_FE_rural, modelo_RE_rural,
          type = "text",  # use "html" para HTML ou "latex" para LaTeX
          title = "Resultados da Regressão em Painel",
          align = TRUE,
          style = "all",
          column.labels = c("Pooled", "Eefeito Fixol"),
          dep.var.labels.include = FALSE,
          covariate.labels = c("Escore Fator 1", "Escore Fator 2", "Escore Fator 3"),
          keep.stat = c("aic", "bic", "rsq", 
                        "adj.rsq", "n"),
          omit.stat = c("f", "ser"),
          digits = 3)

################################################################################



# Previsão manual com base nos coeficientes
painel$Credito_Previsto <- 45.97 * painel$Escore_Fator1 +
  (-357.14) * painel$Escore_Fator2 +
  1879.07 * painel$Escore_Fator3




impacto_municipio <- painel %>%
  group_by(Municipio) %>%
  summarise(Credito_Previsto_Medio = mean(Credito_Previsto, na.rm = TRUE)) %>%
  arrange(desc(Credito_Previsto_Medio))



# Top 10 municípios com maior impacto previsto


top_mun <- impacto_municipio %>% slice_max(Credito_Previsto_Medio, n = 10)
 
ggplot(top_mun, aes(x = reorder(Municipio, Credito_Previsto_Medio), y = Credito_Previsto_Medio)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 municípios com maior previsão de Crédito Rural (modelo FE)",
       x = "Município", y = "Crédito Rural Previsto (R$)") +
  theme_minimal()



library(geobr)
library(sf)

# Baixar shapefile dos municípios do Pará
mapa_pa <- read_municipality(code_state = "PA", year = 2020)

# Remover acentos e deixar em caixa baixa
library(stringi)

impacto_municipio <- impacto_municipio %>%
  mutate(name_muni = stri_trans_general(tolower(Municipio), "Latin-ASCII"))

mapa_pa <- mapa_pa %>%
  mutate(name_muni = stri_trans_general(tolower(name_muni), "Latin-ASCII"))

# Juntar os dados
mapa_impacto <- left_join(mapa_pa, impacto_municipio, by = "name_muni")


ggplot(mapa_impacto) +
  geom_sf(aes(fill = Credito_Previsto_Medio), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", direction = -1, na.value = "gray90") +
  labs(title = "Impacto médio previsto do Crédito Rural por município (modelo FE)",
       fill = "Crédito Previsto (R$)") +
  theme_minimal() +
  theme(legend.position = "right")




#-------------------------------------------------------------#

ggplot(data = Base_dados, aes(x = Ano, y = Credito_Rural1)) +
  geom_line() +
  labs(x = "Year",  y = "Volume de Crédito") +
  theme(legend.position = "none")








