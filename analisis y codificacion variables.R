
install.packages("readxl")  # Si no lo tienes
library(readxl)

# Carga el archivo (ajusta la ruta si lo tienes en otra carpeta)
df <- read_excel("Mental_Health_Lifestyle_Dataset.xlsx")


# Ver las primeras filas
head(df)

# Estructura de los datos
str(df)

# Ver nombres de columnas
colnames(df)

# Revisar valores faltantes
colSums(is.na(df))

summary(df$"Mental Health Condition")
summary(df$Sleep_hours)
summary(df$Exercise_frequency)

##VARIABLE DE SALUD MENTAL
#Cuantas respuestas hay de cada
table(df$"Mental Health Condition")

# Definir los niveles deseados
niveles_salud_mental <- c("Anxiety", "Bipolar", "Depression", "None", "PTSD")

# Convertir a factor con esos niveles
niveles_salud_mental <- c("Anxiety", "Bipolar", "Depression", "None", "PTSD")

df$MentalHealth_num <- as.numeric(factor(df$`Mental Health Condition`, levels = niveles_salud_mental))

levels(df$`Mental Health Condition`)
# Ver resumen de frecuencia
table(df$`Mental Health Condition`)

table(df$"Age")

boxplot(Age ~ `Mental Health Condition`, data = df,
        main = "Edad según condición de salud mental",
        xlab = "Condición de salud mental",
        ylab = "Edad",
        col = "lightblue")

library(dplyr)
library(ggplot2)

##VARIABLE DE EJERCICIO
table(df$"Exercise Level")

#convertir variable en factor
df$ExerciseLevel_num <- as.numeric(factor(df$`Exercise Level`, 
                                          levels = c("Low", "Moderate", "High"),
                                          labels = c(1, 2, 3)))

ggplot(df, aes(x = factor(ExerciseLevel_num), fill = `Mental Health Condition`)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c("Low", "Moderate", "High")) +
  ylab("Cantidad") +
  xlab("Nivel de Ejercicio") +
  labs(fill = "Condición de Salud Mental") +
  theme_minimal() +
  ggtitle("Distribución de condiciones de salud mental por nivel de ejercicio")

#tabla contingencia
tbl <- table(df$ExerciseLevel_num, df$`Mental Health Condition`)
chisq.test(tbl)
##no son significativas


##NIVELES DE ESTRÉS
table(df$"Stress Level")
df$StressLevel_num <- as.numeric(factor(df$`Stress Level`, 
                                        levels = c("Low", "Moderate", "High")))
##HORAS DE SUEÑO
table(df$"Sleep Hours")
summary(df$`Sleep Hours`)

##horas de sueño x niveles de estrés
df %>%
  group_by(`Stress Level`) %>%
  summarise(
    mean_sleep = mean(`Sleep Hours`, na.rm = TRUE),
    sd_sleep = sd(`Sleep Hours`, na.rm = TRUE),
    median_sleep = median(`Sleep Hours`, na.rm = TRUE),
    n = n()
  )

ggplot(df, aes(x = `Stress Level`, y = `Sleep Hours`, fill = `Stress Level`)) +
  geom_boxplot(alpha = 0.7) +
  xlab("Nivel de Estrés") +
  ylab("Horas de Sueño") +
  theme_minimal() +
  ggtitle("Distribución de Horas de Sueño según Nivel de Estrés") +
  theme(legend.position = "none")

anova_result <- aov(`Sleep Hours` ~ `Stress Level`, data = df)
summary(anova_result)
#no es significativo

##HORAS DE TRABAJO
table(df$"Work Hours per Week")

ggplot(df, aes(x = `Stress Level`, y = `Work Hours per Week`, fill = `Stress Level`)) +
  geom_boxplot(alpha = 0.7) +
  xlab("Nivel de Estrés") +
  ylab("Horas de Trabajo por Semana") +
  theme_minimal() +
  ggtitle("Distribución de Horas de Trabajo por Semana según Nivel de Estrés") +
  theme(legend.position = "none")

anova_work <- aov(`Work Hours per Week` ~ `Stress Level`, data = df)
summary(anova_work)