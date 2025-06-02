##TRABAJO FINAL DE DATA SCIENCE##
##Relación de hábitos saludables y felicidad##
##Dataset: Mental Health and Lifestyle Habits (2019-2024) - Kaggle
##SANDRA Y MARTA##
# Fri May 30 00:41:31 2025 ------------------------------

# Abrir archivo
Mental_Health_Lifestyle_Dataset <- read_csv("C:/Users/sandr/Documents/DATA SCIENCE/TRABAJO-FINAL-DATA-SCIENCE")

# Reasignar nombre 
library(readxl)
datos <- read_excel("C:/Users/sandr/Documents/DATA SCIENCE/TRABAJO-FINAL-DATA-SCIENCE/Mental_Health_Lifestyle_Dataset.xlsx")

library(janitor)

# Limpiar nombres de columna: todo en minúscula y con guiones bajos
datos <- clean_names(datos)

# Verificar nuevos nombres
colnames(datos)

# Convertir variables categóricas a factores
library(dplyr)

datos <- datos %>%
  mutate(
    gender = as.factor(gender),
    exercise_level = as.factor(exercise_level),
    diet_type = as.factor(diet_type),
    stress_level = as.factor(stress_level),
    mental_health_condition = as.factor(mental_health_condition),
    country = as.factor(country)
  )
# Ver las primeras filas
head(datos)

# Ver estructura general
str(datos)

# Revisión de datos
summary(datos)

# Ver nombres de columnas
colnames(datos)

# Ver valores faltantes
colSums(is.na(datos))

# Valores únicos por variable
sapply(datos, function(x) length(unique(x)))

# Eliminar valores faltantes
library(tidyr)
datos <- datos %>% drop_na()

# Crear nuevas variables binarias para cada hábito saludable
datos <- datos %>%
  mutate(
    ejercicio_saludable = exercise_level %in% c("Moderate", "High"),
    sueno_saludable = between(sleep_hours, 7, 9),
    dieta_saludable = diet_type %in% c("Balanced", "Vegan", "Vegetarian"),
    screen_saludable = screen_time_per_day_hours < 6,
    trabajo_saludable = work_hours_per_week < 40,
    social_saludable = social_interaction_score > 5,
    
    # Índice total de hábitos saludables (de 0 a 4)
    indice_salud = ejercicio_saludable + sueno_saludable + dieta_saludable + screen_saludable + trabajo_saludable + social_saludable
  )
### Visualización de datos
library(ggplot2)
# Distribución de Felicidad
ggplot(datos, aes(x = happiness_score)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribución de la felicidad", x = "Happiness Score", y = "Frecuencia") +
  theme_minimal()

## Visualización variables de hábitos saludables
# Ejercicio vs felicidad
ggplot(datos, aes(x = exercise_level, y = happiness_score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Felicidad según nivel de ejercicio",
       x = "Nivel de ejercicio",
       y = "Happiness Score") +
  theme_minimal()
anova_ejercicio <- aov(happiness_score ~ exercise_level, data = datos)
summary(anova_ejercicio)

# Horas de sueño vs felicidad
library(ggplot2)
datos %>%
  mutate(rango_sueno = cut(sleep_hours, breaks = c(0, 4, 6, 8, 10), labels = c("<4h", "4–6h", "6–8h", "8–10h"))) %>%
  ggplot(aes(x = rango_sueno, y = happiness_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Felicidad según rango de horas de sueño", x = "Horas de sueño", y = "Happiness Score") +
  theme_minimal()
cor(datos$sleep_hours, datos$happiness_score)  

# Tiempo de pantalla vs felicidad
ggplot(datos, aes(x = screen_time_per_day_hours, y = happiness_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "Relación entre tiempo de pantalla y felicidad",
       x = "Horas frente a pantalla",
       y = "Happiness Score") +
  theme_minimal()
cor(datos$screen_time_per_day_hours, datos$happiness_score)

# Dieta vs felicidad
ggplot(datos, aes(x = `Diet Type`, y = `Happiness Score`)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Felicidad según tipo de dieta",
       x = "Tipo de dieta",
       y = "Happiness Score") +
  theme_minimal()
anova_dieta <- aov(`Happiness Score` ~ `Diet Type`, data = datos)
summary(anova_dieta)
TukeyHSD(anova_dieta)

# Horas de trabajo y felicidad
ggplot(datos, aes(x = work_hours_per_week, y = happiness_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "Relación entre horas de trabajo y felicidad", x = "Horas de trabajo por semana", y = "Happiness Score") +
  theme_minimal()
cor(datos$work_hours_per_week, datos$happiness_score)

# Interacción social y felicidad
ggplot(datos, aes(x = social_interaction_score, y = happiness_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "darkred") +
  labs(title = "Relación entre interacción social y felicidad", x = "Horas de interacción social por semana", y = "Happiness Score") +
  theme_minimal()
cor(datos$social_interaction_score, datos$happiness_score)

# Índice de hábitos saludables vs felicidad
ggplot(datos, aes(x = factor(indice_salud), y = happiness_score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Felicidad según número de hábitos saludables", x = "Número de hábitos saludables", y = "Happiness Score") +
  theme_minimal()
cor(datos$indice_salud, datos$happiness_score)


## Visualización de variables demográficas
# Sexo vs felicidad
ggplot(datos, aes(x = gender, y = happiness_score)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Felicidad por género", x = "Género", y = "Happiness Score") +
  theme_minimal()
anova_genero <- aov(happiness_score ~ gender, data = datos)
summary(anova_genero)

# Felicidad y país
ggplot(datos, aes(x = Country, y = happiness_score)) +
  geom_boxplot() +
  labs(title = "Felicidad por país", x = "País", y = "Happiness Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
anova_pais <- aov(happiness_score ~ Country, data = datos)
summary(anova_pais)

# Edad y felicidad
datos <- datos %>%
  mutate(grupo_edad = cut(age, breaks = c(0, 18, 30, 45, 60, 100),
                          labels = c("<18", "18-30", "30-45", "45-60", "60+")))

ggplot(datos, aes(x = grupo_edad, y = happiness_score)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Felicidad por grupo de edad", x = "Grupo de Edad", y = "Happiness Score") +
  theme_minimal()
anova_edad <- aov(happiness_score ~ grupo_edad, data = datos)
summary(anova_edad)


## Visualización de variables psicológicas
# Estrés y felicidad
ggplot(datos, aes(x = `Stress Level`, y = `Happiness Score`)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Felicidad según nivel de estrés", x = "Nivel de estrés", y = "Happiness Score") +
  theme_minimal()

anova_estres <- aov(happiness_score ~ stress_level, data = datos)
summary(anova_estres)

# Condición mental y felicidad
ggplot(datos, aes(x = mental_health_condition, y = happiness_score)) +
  geom_boxplot(fill = "lightcyan") +
  labs(title = "Felicidad según condición mental", x = "Condición de salud mental", y = "Happiness Score") +
  theme_minimal()

anova_salud_mental <- aov(happiness_score ~ mental_health_condition, data = datos)
summary(anova_salud_mental)

### Análisis de regresión: predictores de la felicidad

modelo <- lm(happiness_score ~ sleep_hours + exercise_level + diet_type + 
               screen_time_per_day_hours + work_hours_per_week +
               social_interaction_score + gender + stress_level + 
               mental_health_condition + age + country, data = datos)

summary(modelo)

lm(happiness_score ~ exercise_level * diet_type, data = datos)
summary(lm(formula = happiness_score ~ exercise_level * diet_type, data = datos))

library(car)
vif(modelo) 
plot(modelo)  

# Primero, creamos una nueva variable felicidad transformada
# Sumamos 1 por si hay ceros en happiness_score
datos$happiness_score_log <- log(datos$happiness_score + 1)

# Ajustamos el nuevo modelo con la variable transformada
modelo_log <- lm(happiness_score_log ~ sleep_hours + exercise_level + diet_type + 
                   screen_time_per_day_hours + work_hours_per_week +
                   social_interaction_score + gender + stress_level + 
                   mental_health_condition + age + country, data = datos)

# Resumen del modelo
summary(modelo_log)

# Diagnóstico gráfico
qqnorm(rstandard(modelo_log))
qqline(rstandard(modelo_log), col = "red")

# 4 plots básicos de diagnóstico:
par(mfrow = c(2, 2)) 
plot(modelo_log)

----

  # Interacción entre ejercicio y dieta
  modelo_interaccion <- lm(`Happiness Score` ~ `Exercise Level` * `Diet Type`, data = datos)
summary(modelo_interaccion)

# Multicolinealidad
library(car)
vif(modelo) 

# Diagnóstico gráfico
plot(modelo)  

# Transformación logarítmica de felicidad
datos$happiness_score_log <- log(datos$`Happiness Score` + 1)

# Modelo con variable transformada
modelo_log <- lm(happiness_score_log ~ `Sleep Hours` + `Exercise Level` + `Diet Type` + 
                   `Screen Time per Day (Hours)` + `Work Hours per Week` +
                   `Social Interaction Score` + `Gender` + `Stress Level` + 
                   `Mental Health Condition` + `Age` + `Country`, data = datos)

# Resumen del modelo transformado
summary(modelo_log)

# Gráfico QQ
qqnorm(rstandard(modelo_log))
qqline(rstandard(modelo_log), col = "red")

# 4 gráficos de diagnóstico
par(mfrow = c(2, 2)) 
plot(modelo_log)
