# Imports
library(lubridate) # Date
library(dplyr) # Tables
library(forecast)

# Checa pasta de trabalho
print(getwd())

# Read Dataframe ----
df <- read.csv(
  'data/tab_serie_gmt_1999-2021.csv',
  header = TRUE,
  sep = ';',
  dec=',',
)

# Ajusta os dtypes
df$data_hora_gmt <- as.Date(df$data_hora_gmt, format='%Y-%m-%d %H:%M:%S')
df$data_hora_sp <- as.Date(df$data_hora_sp, format='%Y-%m-%d %H:%M:%S')

# Extraí parte da data
df$ano <- lubridate::year(df$data_hora_sp)
df$mes <- lubridate::month(df$data_hora_sp)
df$dia <- lubridate::day(df$data_hora_sp)

# Vê os dtypes
sapply(df, class)
sapply(df, typeof)
str(df)

# Print head
print.data.frame(head(df, 3))

# Seleciona Subset por Satélite ----
# Lista de Satélites
list_sats <- unique(df$satelite)
list_sats[1]

# Extraí Satélite
df <- subset(df, satelite == 'AQUA_M-T')
df

# Agrupa Dataframe ----
df_sat <- df %>%
  dplyr::group_by(ano, mes) %>%
  dplyr::summarise(n_focos = n())

# Cria coluna com Data
df_sat$date <- zoo::as.yearmon(paste(df_sat$ano, df_sat$mes), '%Y %m')

# Deleta Mês e Ano

df_sat$ano <- NULL
df_sat$mes <- NULL
df_sat

# Vê os dtypes
sapply(df_sat, class)
sapply(df_sat, typeof)

# Reorder columns
df_sat <- df_sat[,c(2, 1)]
colnames(df_sat)

# Print head
print.data.frame(head(df_sat, 3))
print.data.frame(tail(df_sat, 3))



# -------------------------------------------
# Séries Temporais ----

# Definição da série
serie <- ts(sat, start = c(2002,7), end = c(2021, 9), frequency = 12)
serie

# Gráficos
forecast::autoplot(serie)
plot.ts(serie, main = 'Nº de Focos de Calor')

# Explorando
acf(serie) # autocorrelação
pacf(serie) # autocorrelação parcial
ggtsdisplay(serie) # avaliação em visualização única
diff(serie) # diferença entre os meses
seasonplot(
  serie,
  col=rainbow(12),
  year.labels=TRUE,
  type='o',
  pch=16
)


## Etapa 1: N Diff -----
n_dif <- ndiffs(serie) # diferença entre os meses
n_dif
#serie_ndiff <- diff(serie, n_dif)
serie_ndiff <- serie
ggtsdisplay(serie_ndiff) # avaliação em visualização única

## Etapa 2: Box-Cox -----
lbd <- BoxCox.lambda(serie_ndiff)
lbd
serie_ndiff_bc <- BoxCox(serie_ndiff, lambda = lbd)
hist(serie_ndiff_bc) # antes
autoplot(serie_ndiff_bc)
ggtsdisplay(serie_ndiff_bc)






