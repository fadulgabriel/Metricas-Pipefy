pacman::p_load(tidyverse, readxl)

#### IMPORTANDO O BANCO ######
setwd("/Users/imac/Desktop/ESTAT")
dados <- read_excel("metricas_31-03-2025.xlsx")


#### TRATANDO O BANCO ######

#gerando o funil de vendas
ordem_funil <- c("Novo Lead", "Primeiro Contato", "Em Contato", "Diagnóstica", "Validação", 
                 "Proposta Elaboração", "Proposta Marcada", "Negociação e Follow-Up", "Ganho", 
                 "Perdido", "Retomada")

banco_ajustado <- dados %>%
  mutate(
    `Fase Final` = case_when(
      !is.na(`Etapa da Perda`) ~ `Etapa da Perda`,
      `Fase atual` == "Retomada" & !is.na(`Última Fase`) ~ `Última Fase`,
      TRUE ~ `Fase atual`))

#nome dos vendedores
banco_ajustado <- banco_ajustado %>%
  mutate(Responsáveis = case_when(
    Responsáveis == "beatriz-ximenes" ~ "Bia Ximenes",
    Responsáveis == "Bruno Boaventura" ~ "Bruno",
    Responsáveis == "Gustavo Lopes" ~ "Gustavo",
    Responsáveis == "Isabela Yule" ~ "Bela",
    Responsáveis == "Marina Braulio" ~ "Nina",
    Responsáveis == "joao-bertoli" ~ "Rola",
    Responsáveis == "Pedro Oliveros Santarem" ~ "Santarem",
    TRUE ~ Responsáveis
  ))

#tipos de tráfego (pago ou orgânico)
banco_ajustado <- banco_ajustado %>%
  mutate(`Page URL` = case_when(
    `Page URL` == "" ~ "",
    is.na(`Page URL`) ~ "Sem informação",
    str_detect(`Page URL`, regex("empresarial", ignore_case = TRUE)) & str_detect(`Page URL`, "gclid") ~ "Campanha Empresarial",
    str_detect(`Page URL`, regex("academicos", ignore_case = TRUE)) & str_detect(`Page URL`, "gclid") ~ "Campanha Acadêmica",
    str_detect(`Page URL`, ".+") & !str_detect(`Page URL`, regex("empresarial|academicos", ignore_case = TRUE)) ~ "Orgânico",
    TRUE ~ `Page URL`
  ))

#### GRÁFICOS E INDICADORES ######

#calculando a taxa de conversão
funil <- banco_ajustado %>% 
  count(`Fase Final`) %>% 
  mutate(`Fase Final` = factor(`Fase Final`, levels = ordem_funil)) %>%
  arrange(`Fase Final`)

total_leads <- rev(cumsum(rev(funil$n)))
taxa_conversao <- round(total_leads / sum(funil$n),3) * 100
funil <- funil%>%
  mutate(`Taxa de Conversão` = taxa_conversao)

grafico_funil <- ggplot(funil, aes(x = `Fase Final`, y = `Taxa de Conversão`)) +
  geom_col(fill = "grey") +
  geom_text(aes(label = paste0(`Taxa de Conversão`, "%")), 
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  coord_flip() + 
  labs(title = "Funil de Conversão",
       x = "Etapa",
       y = "Taxa de Conversão (%)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
