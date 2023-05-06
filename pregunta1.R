# ---- ENUNCIADO ----
# Un equipo de investigadores del área de interacción humano-información está estudiando
# si el área temática y el nivel de dificultad del problema de información influyen en el 
# tiempo (en segundos) que toma un usuario en formular una consulta de búsqueda para resolver 
# dicho problema. Para ello, han reclutado a un grupo de participantes voluntarios, asignados aleatoriamente 
# a distintos grupos. Cada participante debe resolver tres problemas de información con diferentes niveles de 
# dificultad: baja, media y alta. A su vez, cada grupo debe resolver problemas relacionados a una temática diferente.
# Los datos recolectados contemplan las siguientes variables:

if(!require("ez")) install.packages("ez")
if(!require("readxl")) install.packages("readxl")
if(!require("ggpubr")) install.packages("ggpubr")
if(!require("tidyverse")) install.packages("tidyverse")

EP07_Datos <- read_excel("EP07 Datos.xlsx")

cat("1- Se cumple condición de la escala ya que es de tiempo\n")
cat("2- Se cumple segunda condición de independencia y aleatoriedad\n")
cat("3- El gráfico QQ nos demuestra que si se cumple el suspuesto de normalidad.\n")
cat("4- \n")


g1 <- ggqqplot(EP07_Datos %>% filter(dificultad == "Alta"),
              x = "tiempo",
              color = "dificultad", 
              palette = "#0073C2FF",
              ggtheme = theme_pubclean())

g2 <- ggqqplot(EP07_Datos %>% filter(dificultad == "Media"),
              x = "tiempo",
              color = "dificultad", 
              palette = "#FC4E07",
              ggtheme = theme_pubclean())

g3 <- ggqqplot(EP07_Datos %>% filter(dificultad == "Baja"),
              x = "tiempo",
              color = "dificultad", 
              palette = "green",
              ggtheme = theme_pubclean())

g <- ggarrange(g1, g2 ,g3, ncol = 3, nrow = 1)
print(g)

cat("Procedimiento ANOVA con azANOVA()")
prueba <- ezANOVA(
  data = EP07_Datos,
  dv = tiempo,
  wid = id,
  between = area,
  return_aov = TRUE
)

print(prueba)

g_tam_efecto = ezPlot(
  data = EP07_Datos,
  dv = tiempo,
  wid = id,
  between = area,
  x = area
)

print(g_tam_efecto)



