
################################################################################
# Regressão de Dado em Painel
# Cliente: Carlos Benasuly
# Demanda: Doutorado
################################################################################


################################################################################
# LIMPAR MEMÓRIA
rm(list=ls())
################################################################################

################################################################################
# DEFINIR DIRETÓRIO DE TRABALHAO
setwd("C:/Users/usuario/Documents/Modelo_Regressao/Dado_Painel")
################################################################################


################################################################################
# PACKAGES
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
library(corrplot)
library(sf)
library(geobr)
################################################################################


################################################################################
# DATASET
Base_dados <- read_excel("Base_Volume_Credito_Rural.xlsx")

# Verificar a Estrutura
str(Base_Volume_Credito_Rural)

# Garantir: 'Municipio' e 'Ano' Estejam como Fatores
Base_dados <- Base_dados %>%
  mutate(Municipio = as.factor(Municipio),
         Ano = as.factor(Ano))
################################################################################


################################################################################
# Transformar em Objeto de Painel

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
################################################################################


################################################################################
# MATRIZ DE CORRELAÇÃO

# Selecionar apenas variáveis numéricas para correlação
dados_numeric <- Base_dados %>%
  select(Credito_Rural, 
         Credito_Nao_Rural, 
         Escore_Fator1, 
         Escore_Fator2, 
         Escore_Fator3,
         Pessoa_Fisica, 
         Pessoa_Juridica, Producao_Familiar,
         Financiamento_Mini, 
         Financiamento_Pequeno, 
         Financiamento_Medio, 
         Financiamento_Grande)

# Remover linhas com NA
dados_numeric <- na.omit(dados_numeric)

# Calcular a matriz de correlação
cor_matriz <- cor(dados_numeric)

# Visualizar a correlação em gráfico
corrplot(cor_matriz, 
         method = "color", 
         type = "lower",
         tl.col = "black", 
         tl.srt = 90,
         is.corr = TRUE,
         sig.level = 0.05,
         insig = 'p-value',
         addCoef.col = "black", 
         diag =  TRUE,
         number.cex = 0.8)

################################################################################


################################################################################
# Gráfico de Disperssão 
ggplot(Base_dados, aes(x = Pessoa_Juridica, y = Credito_Rural)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Volume de Crédito Rural vs Pessoa Juridica", 
       x = "Pessoa Jurídica", 
       y = "Volume de Crédito Rural")

# Gráfico de Disperssão 
ggplot(Base_dados, aes(x = Pessoa_Fisica, y = Credito_Rural)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Volume de Crédito Rural vs Pessoa Física", 
       x = "Pessoa Física", 
       y = "Volume de Crédito Rural")

# Gráfico de Disperssão 
ggplot(Base_dados, aes(x = Producao_Familiar, y = Credito_Rural)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Volume de Crédito Rural vs Produção Familiar", 
       x = "Produção Familiar",
       y = "Volume de Crédito Rural")
################################################################################


################################################################################
# Média de Crédito Rural

Base_dados %>%
  group_by(Ano) %>%
  summarise(Media_Credito_Rural = mean(Credito_Rural, na.rm = TRUE)) %>%
  ggplot(aes(x = Ano, y = Media_Credito_Rural)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  theme_minimal() +
  labs(title = "Tendência do Crédito Rural ao longo do tempo", x = "Ano", y = "Média do Crédito Rural")



# Calcular a média e o percentual (Top 10 municípios Rural)
top_municipios <- Base_dados %>%
  group_by(Municipio) %>%
  summarise(Media_Credito_Rural = mean(Credito_Rural1, na.rm = TRUE)) %>%
  arrange(desc(Media_Credito_Rural)) %>%
  slice_head(n = 10) %>%
  mutate(Percentual = Media_Credito_Rural / sum(Media_Credito_Rural) * 100)

# Criar gráfico com percentual dentro das barras
ggplot(top_municipios, aes(x = reorder(Municipio, Media_Credito_Rural), 
                           y = Media_Credito_Rural)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentual, 1), "%")),
            hjust = 1.1, color = "white", size = 4) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Municípios com Maior Média de Crédito Rural",
    #subtitle = "Com percentual de participação entre os Top 10",
    x = "Município",
    y = "Volume de Crédito Rural"
  )


# Calcular a média e o percentual (Top 10 municípios Não Rural)
top_municipios <- Base_dados %>%
  group_by(Municipio) %>%
  summarise(Media_Credito_Rural = mean(Credito_Nao_Rural, na.rm = TRUE)) %>%
  arrange(desc(Media_Credito_Rural)) %>%
  slice_head(n = 10) %>%
  mutate(Percentual = Media_Credito_Rural / sum(Media_Credito_Rural) * 100)

# Criar gráfico com percentual dentro das barras
ggplot(top_municipios, aes(x = reorder(Municipio, Media_Credito_Rural), 
                           y = Media_Credito_Rural)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(Percentual, 1), "%")),
            hjust = 1.1, color = "white", size = 4) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Municípios com Maior Média de Crédito Não Rural",
    #subtitle = "Com percentual de participação entre os Top 10",
    x = "Município",
    y = "Volume de Crédito Não Rural"
  )



# Calcular média de crédito rural por município e ano
top_municipios_por_ano <- Base_dados %>%
  group_by(Ano, Municipio) %>%
  summarise(Media_Credito_Rural = mean(Credito_Rural, na.rm = TRUE), .groups = "drop") %>%
  arrange(Ano, desc(Media_Credito_Rural)) %>%
  group_by(Ano) %>%
  slice_max(Media_Credito_Rural, n = 10) %>%
  ungroup()

# Criar gráfico com facet por ano
ggplot(top_municipios_por_ano, aes(x = reorder(Municipio, Media_Credito_Rural), 
                                   y = Media_Credito_Rural)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  facet_wrap(~Ano, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Top 10 Municípios com Maior Média de Crédito Rural por Ano",
    x = "Município",
    y = "Média de Crédito Rural"
  )


# Calcular média de crédito rural por UF e ano
top_ufs_por_ano <- Base_dados%>%
  group_by(Ano, UF) %>%
  summarise(Media_Credito_Rural = mean(Credito_Rural, na.rm = TRUE), .groups = "drop") %>%
  arrange(Ano, desc(Media_Credito_Rural)) %>%
  group_by(Ano) %>%
  slice_max(Media_Credito_Rural, n = 10) %>%
  ungroup()

# Gráfico com facet por ano
ggplot(top_ufs_por_ano, aes(x = reorder(UF, Media_Credito_Rural), 
                            y = Media_Credito_Rural)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  facet_wrap(~Ano, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Top 10 UFs com Maior Média de Crédito Rural por Ano",
    x = "UF",
    y = "Média de Crédito Rural"
  )
################################################################################


################################################################################
# Mapa Tematico

# Baixa o shapefile dos estados brasileiros
ufs_sf <- read_state(year = 2020, showProgress = FALSE)

# Agrupar os dados por UF e calcular média
media_uf <- Base_dados %>%
  group_by(UF) %>%
  summarise(Media_Credito_Rural = mean(Credito_Rural, na.rm = TRUE))

# Verifique se a coluna 'abbrev_state' no shapefile é compatível com 'UF'
mapa_dados <- ufs_sf %>%
  left_join(media_uf, by = c("abbrev_state" = "UF"))


ggplot(mapa_dados) +
  geom_sf(aes(fill = Media_Credito_Rural), color = "white") +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Média Crédito Rural") +
  theme_minimal() +
  labs(
    title = "Distribuição Espacial da Média de Crédito Rural por UF",
    subtitle = "Valores médios agregados por Unidade da Federação"
  )
################################################################################



################################################################################
# Modelo de Dados Empilhados (Pooled OLS Model)
# Crédito Rural
modelo_polled_rural <- plm(Credito_Rural1 ~ Escore_Fator1+Escore_Fator2+Escore_Fator3+
                             Pessoa_Fisica + Pessoa_Juridica + Producao_Familiar+
                             Financiamento_Mini,
                       data = painel, 
                       model = "pooling")
# Coeficientes
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



library(modelsummary)
modelsummary(modelo_FE_rural, 
             output = "markdown", 
             stars = TRUE)
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
summary(modelo_FE_rural, vcov. = function(modelo_FE_rural) vcovHC(modelo_FE_rural, type = "HC1", maxlag = 4))

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


summary(modelo_RE_rural, vcov = function(x) vcovHC(x, method = "arellano"))

################################################################################






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
plm::phtest(modelo_FE_rural, modelo_RE_rural)


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
          column.labels = c("Pooled", "Efeito Fixo", "Efeito Aleatório"),
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


top_mun <- impacto_municipio %>% slice_max(Credito_Previsto_Medio, n = 15)
 
ggplot(top_mun, aes(x = reorder(Municipio, Credito_Previsto_Medio), y = Credito_Previsto_Medio)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 15 Maior Previsão de Crédito Rural (Efeito Fixo)",
       x = "Municípios", y = "Volume de Crédito Rural Previsto (R$)") +
  theme_minimal()




ggplot(Base_dados, aes(x = Escore_Fator3, y = Credito_Rural1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(
    title = "Crédito Rural vs. Escore Fator 3 (PIB, ICMS)",
    x = "Escore Fator 3",
    y = "Crédito Rural"
  )



#-------------------------------------------------------------------#

library(ggplot2)

# Fator 1: Estrutura urbana
ggplot(Base_dados, aes(x = Escore_Fator1, y = Credito_Rural1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Crédito Rural vs. Fator 1 (Urbano/Social)",
       x = "Escore Fator 1",
       y = "Crédito Rural")

# Fator 2: Agropecuária/Desmatamento
ggplot(Base_dados, aes(x = Escore_Fator2, y = Credito_Rural1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "forestgreen") +
  theme_minimal() +
  labs(title = "Crédito Rural vs. Fator 2 (Agropecuária/Desmatamento)",
       x = "Escore Fator 2",
       y = "Crédito Rural")

# Fator 3: Dinamismo Econômico
ggplot(Base_dados, aes(x = Escore_Fator3, y = Credito_Rural1)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Crédito Rural vs. Fator 3 (PIB + ICMS)",
       x = "Escore Fator 3",
       y = "Crédito Rural")





# Gráfico de coeficientes com intervalos de confiança dos fatores

library(broom)

# Extrair os coeficientes do modelo
coeficientes <- tidy(modelo_FE_rural, conf.int = TRUE)

# Filtrar apenas as variáveis explicativas (caso tenha efeitos fixos ocultos)
coef_plot <- coeficientes %>%
  filter(term != "(Intercept)")

# Plotar o forest plot
ggplot(coef_plot, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Coeficientes do Modelo com Intervalos de Confiança",
    x = "Variáveis",
    y = "Estimativas"
  )





# Extrair os coeficientes com IC do modelo
coeficientes <- tidy(modelo_FE_rural, conf.int = TRUE)

# Substituir nomes técnicos por nomes legíveis
nomes_amigaveis <- c(
  "Escore_Fator1" = "Fator 1: Urbano/Social",
  "Escore_Fator2" = "Fator 2: Agro/Desmatamento",
  "Escore_Fator3" = "Fator 3: PIB + ICMS",
  "Pessoa_Fisica" = "Pessoa Física",
  "Pessoa_Juridica" = "Pessoa Jurídica",
  "Producao_Familiar" = "Produção Familiar",
  "Financiamento_Pequeno" = "Financiamento Pequeno"
)

# Substituir os nomes na coluna term
coef_plot <- coeficientes %>%
  filter(term %in% names(nomes_amigaveis)) %>%
  mutate(term_legivel = nomes_amigaveis[term])

# Plot
ggplot(coef_plot, aes(x = reorder(term_legivel, estimate), y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Efeitos Estimados no Crédito Rural (Modelo de Efeitos Fixos)",
    x = "Variáveis Explicativas",
    y = "Coeficiente Estimado (com IC 95%)"
  )







