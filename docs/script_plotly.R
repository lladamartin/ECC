setwd("C:/Users/user/Documents/GitHub/ECC")

library(tidyverse)
library(gt)
library(gtExtras)
library(plotly)

df = read.csv("./reporte/dataset_report.csv")


#agregar label al grafico
df <- df %>%
  mutate(mylabel = paste("Date: ", date, "\n",
                         "Monthly inflation rate: ", round(gcpi,2), "\n",
                         "SBS index: ", round(ss3,2),sep=""))


df = df %>% mutate(
    ss3_scaled = scale(ss3)[, 1],
    gcpi_scaled = scale(gcpi)[, 1]  
  )

df = as_tibble(df)


####
# otra

df2 = df %>% select(date,mylabel, ss3_scaled, gcpi_scaled)
df2 = df2 %>% gather(var,value,3:length(df2))

#labels = c("gcpi_scaled" = "Inflation Rate", "ss3_scaled" = "SBS Index")
df2$var2 = ""
df2$var2[df2$var=="gcpi_scaled"] = "Inflation Rate"
df2$var2[df2$var=="ss3_scaled"] = "SBS Index"

# Crear el gráfico con ggplot y usar "text" en lugar de "label"
p <- ggplot(df2) +
  geom_line(aes(x = as.Date(date), y = value, color = var2)) +
  geom_point(aes(x = as.Date(date), y = value), alpha = 0) + # Añadir puntos invisibles para que plotly pueda leer los 'hover'
scale_x_date(date_labels = "%Y-%m-%d",date_breaks  ="6 month")+ 
  labs(x = "",y="", title = "",color = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
axis.line = element_line(colour = "black"),
panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  scale_color_brewer(palette = "Dark2")  # Asignar nuevos nombres a las categorías




# Convertir a ggplotly y pasar directamente los valores del tooltip
p_plotly <- ggplotly(p) %>%
  layout(
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
  annotations = list(
      x = 0.05,  # Centramos la nota al pie
      y = -0.17,  # Un poco más cerca del gráfico
      text = "Note: the variables were standardized.",
      xref = 'paper',
      yref = 'paper',
      showarrow = FALSE,
      xanchor = 'center',  # Centramos horizontalmente la nota al pie
      yanchor = 'top',
      font = list(size = 10)
    )
  ) %>%
  style(text = df2$mylabel, hoverinfo = "text")

# Mostrar el gráfico interactivo
p_plotly


###

https://lladamartin.github.io/ECC/script_report.html



