if (!require("pacman")) install.packages("pacman")

suppressMessages(suppressWarnings(
  suppressPackageStartupMessages(
    pacman::p_load(
      tidyverse, data.table, readxl, readr, ggcorrplot, cowplot,
      RColorBrewer, scales, nortest, xlsx,
      skimr, xtable, geobr, sf, ggrepel, abjutils, grDevices
    )
  )
))



#windowsFonts(Arial=windowsFont("sans"))

options(scipen=999)
options(OutDec = ",")

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

# Definindo função que retorna frequências relativas de um vetor
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com frequências
# relativas e absolutas de uma variável categórica
vector_frequencies <- function(vector) {
  frequency <- vector %>%
    table() %>%
    as_tibble() %>%
    mutate(
      rel = n %>%
        percent() %>%
        paste("%", sep = "")
    )
  colnames(frequency) <- c("groups", "absolute", "relative")
  return(frequency)
}

quadro <- function(dados,t=T){
  rn=F
  dados <- dados %>%
    select_if(is.numeric)
  tabela <- skim(dados)
  tabela <- as.data.frame(tabela)
  tabela <- tabela %>% 
    select(!c(1,3:4,12)) %>%
    mutate(
      numeric.mean = round(numeric.mean,2),
      numeric.sd = round(numeric.sd,2),
      numeric.p0 = round(numeric.p0,2),
      numeric.p25 = round(numeric.p25,2),
      numeric.p50 = round(numeric.p50,2),
      numeric.p75 = round(numeric.p75,2),
      numeric.p100 = round(numeric.p100,2),
      di = numeric.p75-numeric.p25,
      di = round(di,2)
    )
  colnames(tabela) <- c("Categoria",
                        "Média","Desvio padrão",
                        "Mínimo","q25","Mediana","q75","Máximo","D I")
  if (t == T){
    tabela <- as.data.frame(t(tabela))
    colnames(tabela) <- tabela[1,]
    tabela <- tabela[-1,]
    rn=T
  }
  tabela <- tabela %>%
    mutate_if(is.character, function(x) {
      as.numeric(x)
    })
  
  tabela <- tabela %>%
    mutate_if(is.numeric, function(x) {
      format(x, decimal.mark = ",")
    })

  print(xtable(tabela, type = "latex"), include.rownames=rn)
}

# Remover eixos do gráfico coroplético
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


## Nova função de quadro resumo
print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
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
