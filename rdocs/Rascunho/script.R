library(pacman)
pacman::p_load(tidyverse, gt, readxl, scales, forcats)

# lendo os arquivos xlsx e suas respectivas abas 

atenas <- read_xlsx("C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/Banco/Olimpiadas 2000 - 2016.xlsx", sheet = "Athina")
pequim <- read_xlsx("C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/Banco/Olimpiadas 2000 - 2016.xlsx", sheet = "Beijing")
london <- read_xlsx("C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/Banco/Olimpiadas 2000 - 2016.xlsx", sheet = "London")
sydney <- read_xlsx("C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/Banco/Olimpiadas 2000 - 2016.xlsx", sheet = "Sydney")
rio <- read_xlsx("C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/Banco/Olimpiadas 2000 - 2016.xlsx", sheet = "Rio de Janeiro")

# atribuindo o ano de cada olimpiada

atenas <- atenas %>% 
  mutate(Ano = 2004)

pequim <- pequim %>% 
  mutate(Ano = 2008) %>% 
  rename(`Height (cm)` = Height,
         `Weight (lbs)` = Weight,
         Gender = Sex)

sydney <- sydney %>% 
  mutate(Ano = 2000) %>% 
  rename(Names = N4m3,
         Gender = `S3x`,
         Age = `4g3`,
         `Height (cm)` = H31ght,
         `Weight (lbs)` = W31ght,
         Team = T34m,
         Sport = Sp0rt,
         Event = `3v3nt`,
         Medal = M3d4l)

london <- london %>% 
  mutate(Ano = 2012) %>% 
  rename(Names = Name,
         `Height (cm)` = Height,
         `Weight (lbs)` = Weight,
         Gender = Sex)

rio <- rio %>% 
  mutate(Ano =  2016) %>% 
  rename(Names = Name,
         Gender = Sex,
         `Height (cm)` = Height_cm,
         `Weight (lbs)` =Weight_lbs,
         Team = Country,
         Age = Age_year)

# unindo os bancos de dados

dados_brutos <- bind_rows(atenas, pequim, sydney, london, rio)


# limpando os nao medalhistas

dados_filtrados <- dados_brutos %>% 
  filter(Medal != "NA")



########################### ENTREGA 01 ###########################

### filtrando apenas para o top 5

unicos <- dados_filtrados %>% 
  distinct(Names, Event, Ano, .keep_all = T) 

dados_fem <- dados_filtrados %>% 
  filter(Gender == "F") %>% 
  unique(, by = c("Names", "Event", "Ano"))



top_5_geral <- dados_filtrados %>% 
  filter(Gender == "F") %>%
  unique() %>% 
  select(Team, Medal, Ano) %>%  
  group_by(Team) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total)) %>% 
  mutate(proporcao = total / sum(total)) 
  slice_head(n = 5) 

### para fazer o comparativo com os demais
  
demais <- top_5_geral %>% 
  slice_tail(n = -5) %>%
  summarize( Team = "Demais",
    total = sum(total),
         proporcao = sum(proporcao))

dados_prop <- bind_rows(top_5_geral %>% 
                          slice_head(n = 5), demais)

dados_prop[1, "Team"] <- "Estados Unidos"
dados_prop[2, "Team"] <- "Rússia"
dados_prop[3, "Team"]  <- "China"
dados_prop[4, "Team"] <- "Austrália"
dados_prop[5, "Team"] <- "Alemanha"

dados_prop <- dados_prop %>% 
  mutate(freq = total,
         relative_freq = round(proporcao * 100, 2),
         freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
         label = str_c(total, " (", freq, ")") %>% str_squish()) %>% 
  filter(Team != "Demais")

# modelo estat

# Definindo paleta de cores da Estat
cores_estat <- c(
  "#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
  "#CC9966", "#999966", "#006606", "#008091", "#041835",
  "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}

# grafico prop

ggplot(dados_prop) +
  aes(x = fct_reorder(Team, total, .desc = TRUE), y = total) +  
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    aes(label = label),  
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "Países", y = "Frequência") +
  theme_estat()
ggsave("colunas-prop-medalha.png", width = 158, height = 93, units = "mm")

# Grafico de setor

ggplot(dados_prop %>% 
         arrange(desc(Team)) %>% 
         mutate(posicao = cumsum(relative_freq) - 0.5 * relative_freq)) +
  aes(x = factor(""), y = relative_freq , fill = factor(Team)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(relative_freq, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Países')
ggsave("setor_paises.png", width = 158, height = 93, units = "mm")

########################### ENTREGA 02 ###########################
########## Média, desvio padrão, boxplot e histogramas por esporte
####### SEMPRE VERIFICAR SE EXISTEM NA`s NAS ANALISES DO IMC(MESMO QUANDO FOR SO DOS HOMENS E SÓ DAS MULHERES)

"1 pound (avoirdupois)= 0.45359237 kilogram"

imc_bruto <- dados_filtrados %>% 
  filter(Sport %in% c("Athletics","Gymnastics", "Football", "Judo", "Badminton")) %>% 
  select(`Height (cm)`, `Weight (lbs)`, Sport, Gender) %>%
  mutate(Sport = recode(Sport,
                        "Athletics" = "Atletismo",
                        "Football" = "Futebol",
                        "Judo" = "Judô",
                        "Badminton" = "Badminton",  
                        "Gymnastics" = "Ginástica")) 

sum(is.na(imc_bruto$`Weight (lbs)`))
sum(is.na(imc_bruto$`Height (cm)`))

imc_limpo <- imc_bruto %>% 
  mutate(`Peso(kg)` = `Weight (lbs)`*0.45359237,
         `Altura(m^2)` = (`Height (cm)`/100)^2,
         imc = `Peso(kg)`/`Altura(m^2)`) %>% 
  select(Sport, imc) %>% 
  filter(imc != "NA")
  
sum(is.na(imc_limpo$imc))


##### Funcao quadro resumo

print_quadro_resumo <- function(data, var_name, title="Medidas resumo do IMC por esportes", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
  
}

imc_limpo %>% 
  group_by(Sport) %>% 
  print_quadro_resumo(var_name = imc)

#### Boxplots

imc_limpo  %>% 
  ggplot(aes(x = reorder(Sport, imc, FUN = median), y = imc)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5)+
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) + 
  labs(x = "Esportes de interesse", y = "IMC dos atletas (kilograma/metro^2)") +
  theme_estat()
ggsave("box_bi.png", width = 158, height = 93, units = "mm")  


###### Coeficiente de variação (sd/mean) * 100 (%)

coef_var <- imc_limpo %>% 
  group_by(Sport) %>% 
  summarise(desv = sd(imc),
         media = mean(imc),
         cv = (desv/media))

coef_var <- coef_var %>% 
  mutate(cv = scales::percent(cv))
  
###### imc feminino

imc_fem <- imc_bruto %>%
  filter(Gender == "F") %>% 
  mutate(`Peso(kg)` = `Weight (lbs)`*0.45359237,
         `Altura(m^2)` = (`Height (cm)`/100)^2,
         imc = `Peso(kg)`/`Altura(m^2)`) %>% 
  select(Sport, imc) %>% 
  filter(imc != "NA")

sum(is.na(imc_fem$imc))

###### quadro medida resumo feminina

imc_fem %>% 
  group_by(Sport) %>% 
  print_quadro_resumo(var_name = imc)

##### boxplots feminino

imc_fem %>%
  ggplot(aes(x = reorder(Sport, imc, FUN = median), y = imc)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5)+
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) + 
  labs(x = "Esportes de interesse", y = "IMC dos atletas (kilograma/metro^2)") +
  theme_estat()
ggsave("box_bi_fem.png", width = 158, height = 93, units = "mm")

##### coefvar fem

coef_var_fem <- imc_fem %>% 
  group_by(Sport)  %>% 
  summarise(desv = sd(imc),
            media = mean(imc),
            cv = (desv/media))

coef_var_fem <- coef_var_fem %>% 
  mutate(cv = scales::percent(cv))

######## imc masc

imc_masc <- imc_bruto %>%
  filter(Gender == "M") %>% 
  mutate(`Peso(kg)` = `Weight (lbs)`*0.45359237,
         `Altura(m^2)` = (`Height (cm)`/100)^2,
         imc = `Peso(kg)`/`Altura(m^2)`) %>% 
  select(Sport, imc) %>% 
  filter(imc != "NA")

########## quadro resumo

imc_masc %>% 
  group_by(Sport)%>% 
  print_quadro_resumo(var_name = imc)

########## boxplots masc

imc_masc %>%
  ggplot(aes(x = reorder(Sport, imc, FUN = median), y = imc)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5)+
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) + 
  labs(x = "Esportes de interesse", y = "IMC dos atletas (kilograma/metro^2)") +
  theme_estat()
ggsave("box_bi_masc.png", width = 158, height = 93, units = "mm")

######  coefvar masc

coef_var_masc <- imc_masc %>% 
  group_by(Sport) %>% 
  summarise(desv = sd(imc),
            media = mean(imc),
            cv = (desv/media))

coef_var_masc <- coef_var_masc %>% 
  mutate(cv = scales::percent(cv))

########################### ENTREGA 03 ###########################

#### qui quadrado

medalhistas <- dados_filtrados %>% 
  select(Names, Medal) %>% 
  group_by(Names) %>% 
  summarise(total_med = n()) %>% 
  top_n(3, total_med)

top_med <- dados_filtrados %>% 
  filter(Names %in% medalhistas$Names) %>% 
  group_by(Names, Medal) %>% 
  summarise(quantidade = n()) %>% 
  ungroup()

########## Fazer grafico para visualizar 


############ chi quadrado

tabela_chi <- xtabs(quantidade ~ Names + Medal, data = top_med)

chisq.test(tabela_chi)

#data:  tabela_chi
#X-squared = 12.776, df = 4, p-value = 0.01243  

# fisher 

fisher.test(tabela_chi)





