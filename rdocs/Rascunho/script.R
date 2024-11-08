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



dados_fem <- dados_filtrados %>% 
  filter(Gender == "F") %>% 
  distinct(Names, .keep_all = T) %>% 
  select(Names, Team, Medal) %>% 
  group_by(Team) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  slice_head(n = 5)

top_5_geral <- dados_filtrados %>% 
  filter(Gender == "F") %>%
  distinct(Names, .keep_all = T) %>% 
  select(Team, Medal, Ano) %>%  
  group_by(Team) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total)) %>% 
  mutate(proporcao = total / sum(total))

sum(top_5_geral$total)
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
         label = str_c(total, " (", freq, ")") %>% str_squish()) 

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

ggplot(dados_prop%>% 
         filter(Team != "Demais")) +
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
ggsave("colunas-prop-medalha.png", width = 158, height = 93, units = "mm", path = "C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/resultados/images")

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
ggsave("setor_paises.png", width = 158, height = 93, units = "mm", path = "C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/resultados/images")

########################### ENTREGA 02 ###########################
########## Média, desvio padrão, boxplot e histogramas por esporte
####### SEMPRE VERIFICAR SE EXISTEM NA`s NAS ANALISES DO IMC(MESMO QUANDO FOR SO DOS HOMENS E SÓ DAS MULHERES)

"1 pound (avoirdupois)= 0.45359237 kilogram"

dados_imc <- dados_filtrados %>% 
  filter(Sport %in% c("Gymnastics","Football", "Judo", "Athletics", "Badminton")) %>% 
  mutate(Sport = recode(Sport,
                        "Athletics" = "Atletismo",
                        "Judo" = "Judô",
                        "Badminton" = "Badminton",  
                        "Gymnastics" = "Ginástica",
                        "Football" = "Futebol")) %>% 
  mutate(`Peso(kg)` = `Weight (lbs)`*0.45359237,
         `Altura(m^2)` = (`Height (cm)`/100)^2,
         IMC = `Peso(kg)`/`Altura(m^2)`) %>%
  select(Names, Ano, Sport, IMC) %>% 
  filter(!is.na(IMC)) %>% 
  distinct(Names, Ano, .keep_all = T)
  
##### Funcao quadro resumo

print_quadro_resumo <- function(data, var_name, title="", label="")
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

dados_imc %>%
  mutate(Sport = factor(Sport, levels = c("Ginástica", "Atletismo", "Badminton", "Futebol", "Judô")))
  group_by(Sport) %>% 
  print_quadro_resumo(var_name = IMC)

#### Boxplots

dados_imc  %>% 
  ggplot(aes(x = reorder(Sport, IMC, FUN = median), y = IMC)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5)+
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 2, fill = "white"
  ) + 
  labs(x = "Esportes de interesse", y = "IMC dos atletas (kg/m²)") +
  theme_estat() +
  ylim(15,57)
ggsave("box_bi.png", width = 158, height = 93, units = "mm", path = "C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/resultados/images")  


###### Coeficiente de variação (sd/mean) * 100 (%)

coef_var <- dados_imc %>% 
  group_by(Sport) %>% 
  summarise(desv = round(sd(IMC), 2),
         media = round(mean(IMC), 2),
         cv = (desv/media))

coef_var <- coef_var %>% 
  mutate(cv = scales::percent(cv))
  

########################### ENTREGA 03 ###########################

#### preparando os dados para fazer o gráfico

medalhistas <- dados_filtrados %>%  # os top 3 medalistas
  select(Names, Medal) %>% 
  group_by(Names) %>% 
  summarise(total_med = n()) %>% 
  top_n(3, total_med)


top_med <- dados_filtrados %>%  # determinando quais as medalhas conquistadas por cada um do TOP 3
  filter(Names %in% medalhistas$Names) %>% 
  group_by(Names, Medal) %>% 
  summarise(quantidade = n(), .groups = "drop") %>% 
  group_by(Names) %>% 
  mutate(
    freq_relativa = round(quantidade / sum(quantidade) * 100, 2)
  )

porcentagens <- str_c(top_med$freq_relativa, "%") %>% str_replace("
 \\.", ",")
legendas <- str_squish(str_c(top_med$quantidade, " (", porcentagens, ")")
)

########## Fazer grafico para visualizar 

top_med %>% 
  mutate(Medal = recode(Medal,
                        "Gold" = "Ouro",
                        "Silver" = "Prata",
                        "Bronze" = "Bronze")) %>%  
  rename(Medalha = Medal) %>%
  mutate(Medalha = fct_relevel(Medalha, "Ouro", "Prata", "Bronze")) %>%
  ggplot()+
  aes(
    x = fct_reorder(Names, quantidade), y = quantidade,
    fill = Medalha, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Atletas", y = "Frequência") + 
  theme_estat()
ggsave("colunas-bi-freq.png", width = 158, height = 93, units = "mm", path = "C:/Users/Bruno/OneDrive/Documentos/GitHub/Projeto_Ghost/resultados/images")

####### tabela de valores


########################### ENTREGA 04 ###########################

####### grafico dispersao

dados_filtrados %>% 
  mutate(`Weight (lbs)` = `Weight (lbs)` * 0.45359237) %>% 
  ggplot() + 
    aes(x = `Height (cm)`, y = `Weight (lbs)`) +
    geom_point(colour = "#A11D21", size = 1, alpha = 0.2) +
    labs(
      x = "Altura dos atletas (cm)",
      y = "Peso dos atletas (kg)"
    ) +
    theme_estat()
ggsave("disp_uni_pesoaltura.png", width = 158, height = 93, units = "mm")


#### boxplots altura

ggplot(dados_filtrados %>%
         filter(`Height (cm)` != "NA")) %>% +
  aes(x = factor(""), y = `Height (cm)`) +
  geom_boxplot(fill = "#A11D21", width = 0.3, outlier.size = 0.5, outlier.shape = 21, outlier.fill = "black") +
  guides(fill = F) +
  stat_summary(fun = "mean", geom="point", shape=23, size=3, fill="white") +
  labs(x = "", y = "Altura (cm)") +
  theme_estat() +
  ylim(137,219)
ggsave("box_altura.png", width = 158, height = 93, units = "mm")

##### boxplot peso

ggplot(dados_filtrados %>%  # teste
         filter(!is.na(`Weight (lbs)`)) %>% 
         mutate(`Weight (lbs)` = `Weight (lbs)` * 0.45359237) ) +
  aes(x = factor(""), y = `Weight (lbs)`) +
  geom_boxplot(fill = "#A11D21", width = 0.3, outlier.size = 0.5, outlier.shape = 21, outlier.fill = "black") + 
  stat_summary(fun = "mean", geom="point", shape=23, size=3, fill="white") + 
  labs(x = "", y = "Peso (kg)") +
  theme_estat() +
  ylim(25, 175)  # Ajuste dos limites do eixo y para mais foco nos outliers
ggsave("box_peso.png", width = 158, height = 93, units = "mm")


###### coeficiente de variação altura

coef_var_altura <- dados_filtrados %>% 
  filter(`Height (cm)` != "NA") %>% 
    summarise(desv = round(sd(`Height (cm)`), 2),
            media = mean(`Height (cm)`),
            cv = (desv/media))

coef_var_altura <- coef_var_altura %>% 
  mutate(cv = scales::percent(cv))

###### coeficiente de variação peso

coef_var_peso <- dados_filtrados %>% 
  filter(`Weight (lbs)` != "NA") %>% 
  mutate(`Weight (lbs)` = `Weight (lbs)` * 0.45359237) %>%
  summarise(desv = round(sd(`Weight (lbs)`), 2),
            media = round(mean(`Weight (lbs)`), 2),
            cv = (desv/media))

coef_var_peso <- coef_var_peso %>% 
  mutate(cv = scales::percent(cv))

######## quadro medida resumo

dados_filtrados %>% 
  filter(`Weight (lbs)` != "NA") %>% 
  mutate(`Weight (lbs)` = `Weight (lbs)` * 0.45359237) %>% 
  print_quadro_resumo(var_name = "Weight (lbs)")

dados_filtrados %>% 
  filter(`Height (cm)` != "NA") %>%
  print_quadro_resumo(var_name = "Height (cm)")

###### coeficiente de correlação de pearson

correlacao <- dados_filtrados %>% 
  filter(`Weight (lbs)` != "NA") %>% 
  mutate(`Weight (lbs)` = `Weight (lbs)` * 0.45359237) %>% 
  filter(`Height (cm)` != "NA") 

cor.test(correlacao$`Height (cm)`, correlacao$`Weight (lbs)`, method = "pearson")















