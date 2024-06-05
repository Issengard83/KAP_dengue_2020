### Data analysis for the manuscript: Seropositivity to Dengue virus DENV in 
### three neighborhoods in the periphery of a city with a recent history of 
### outbreaks in Argentina: what can we learn from unreported cases?
### Last update:
# Wed Jun  5 10:04:28 2024 ------------------------------


# Load packages -----------------------------------------------------------
pacman::p_load(
  ### Data analysis
  glmmTMB,
  performance,
  DHARMa,

  ### Exploratory analysis
  gtsummary,
  skimr,
  
  ### Table format
  flextable,
  
  ### Graphic tools
  gghighlight, 
  scico, 
  
  ### Data management
  epikit,
  rio,
  janitor,
  tidyverse
)


# Carga datos MSAL 2020 ---------------------------------------------------
data_msal_2020 <- import("raw/informacion-publica-dengue-zika-nacional-hasta-20201231_1.xlsx", na = c("(en blanco)", "*sin dato*")) |>  
  
  ## Descarta columnas
  select(-ends_with("_id")) |>  
  
  ### Selecciona casos de dengue
  filter(evento_nombre=="Dengue" & provincia_nombre == "Santa Fe") |>  
  
  ### Agrupa por provincia, año y semana epidemiológica
  summarise(n_casos = sum(cantidad_casos, na.rm = T), 
            .by = c(anio, semanas_epidemiologicas))


# Carga datos MSAL 2024 ---------------------------------------------------
data_msal_2024 <- import("raw/informacion-publica-dengue-zika-nacional-se-1-a-19-de-2024-2024-05-20.csv", na = c("(en blanco)", "*sin dato*")) |>  
  
  ## Descarta columnas
  select(-ends_with("_id")) |>  
  
  ### Selecciona casos de dengue
  filter(evento=="Dengue" & provincia_residencia == "Santa Fe") |>  
  
  ### Agrupa por provincia, año y semana epidemiológica
  summarise(n_casos = sum(cantidad, na.rm = T), 
            .by = c(anio_min, sepi_min))


# Load dataset ------------------------------------------------------------
data_dengue <- import("clean/kap_dengue_clean.xlsx") |>  
  
  ### Elimina registros del mismo domicilio
  filter(!id_encuesta %in% c("CS214", "CS175", "CH032"))


# Análisis exploratorio ---------------------------------------------------
### Casos dengue Santa Fe semanas 1-52 año 2020
sum(data_msal_2020$n_casos)

## incidencia Santa Fe 2020
attack_rate(cases = sum(data_msal_2020$n_casos), 
            population = 572265, 
            multiplier = 10^4, 
            mergeCI = T, digits = 1)


### Casos dengue Santa Fe semanas 1-19 2024
sum(data_msal_2024$n_casos)

## incidencia Santa Fe
attack_rate(cases = sum(data_msal_2024$n_casos), 
            population = 572265, 
            multiplier = 10^4, 
            mergeCI = T, digits = 1)

### Explora NAs
skim(data_dengue)

### Muestras por barrio
tabyl(data_dengue$barrio)

### Distancia a baldíos
tabyl(data_dengue$viv_baldios_dist)

### Distancia a basurales
tabyl(data_dengue$viv_basura_acum_dist)

### Distancia a zanjas y cunetas
tabyl(data_dengue$viv_cunetas_zanjas_dist)


# Limpia dataset ----------------------------------------------------------
data_dengue_clean <- data_dengue |>  
  
  ### Elimina participantes con dengue previo
  filter(tuvo_dengue == "No") |>  
  
  ### Variables caracter a factor
  mutate(across(where(is.character), as.factor)) |>  
  
  ### Ordena niveles variables
  ## Ocupación
  mutate(ocupacion_cat = fct_relevel(
    ocupacion_cat, "desocupado/a o subocupado/a", "ocupado/a", after = Inf)
  ) |>  
  
  ## Fuente de agua
  mutate(viv_fuente_agua = fct_relevel(viv_fuente_agua, "otra/s", after = Inf)
  ) |>  
  
  ### Traduce niveles
  mutate(
    # genero
    genero = if_else(genero == "Femenino", "female", "male"),
    
    # edad
    edad_cat = paste(edad_cat, "years"),
    
    # nivel educativo
    educacion_cat = fct_recode(
      educacion_cat,
      "illiterate/inc. primary school" = "analfabeto/primaria incompleta",
      "primary school/inc. high school" = "primaria completa/secundaria incompleta",
      "high school and/or university" = "secundaria completa y/o superior"),
    
    # ocupacion
    ocupacion_cat = fct_recode(
      ocupacion_cat,
      "homemaker/student" = "jefe/a de hogar o estudiante",
      "retired/pensioner" = "jubilado/a o pensionado/a",
      "unemployed/underemployed" = "desocupado/a o subocupado/a",
      "employed" = "ocupado/a"),
    
    # tipo de calle
    viv_tipo_calle_cat = fct_recode(
      viv_tipo_calle_cat,
      "paved or semi-paved" = "pavimento o mejorado",
      "dirt" = "tierra",
      "sand" = "arena") |>   
      fct_relevel("sand", after = Inf),
    
    # fuente de agua potable
    viv_fuente_agua = fct_collapse(
      viv_fuente_agua,
      "tank truck/other" = c("Camión aguatero","otra/s"),
      other_level = "water pipes")
  ) |>  
  
  mutate(across(ends_with("_dist"), 
                .fns = ~fct_relevel(.x, "> 50m", after = Inf))) |>  
  
  ### Descarta columnas innecesarias
  select(-fecha, -starts_with("sint_d"), -starts_with("trasm_"), -tuvo_dengue)


# clean working environment
rm(list = setdiff(ls(), "data_dengue_clean"))


# Tabla 1: descriptivos por barrio ----------------------------------------
table1 <- data_dengue_clean |>  
  
  ## Tabla
  tbl_summary(by = barrio,
              include = c(genero:ocupacion_cat, sint_6m_alguno, barrio,
                          viv_tipo_calle_cat, viv_cunetas_zanjas, viv_basura_acum,
                          viv_baldios, viv_prox_rio, viv_suelo_cubierto,
                          viv_techo_impermeable, viv_acum_agua, viv_acum_agua_30d,
                          viv_fuente_agua, prac_cacharros, prac_resid_camion,
                          prac_usa_agua_lluvia, prac_resid_fondo),
              missing = "no",
              percent = "col",
              digits = list(all_categorical() ~ c(0,1)),
              value = c(starts_with("viv_") & !(matches("viv_tipo_calle")|
                                                  matches("viv_fuente_agua")),
                        starts_with("prac_"), sint_6m_alguno) ~ "Si",
              label = c(
                genero = "Gender",
                edad = "Age (years)",
                edad_cat = "Age group",
                educacion_cat = "Education",
                ocupacion_cat = "Occupation",
                viv_tipo_calle_cat = "Street type",
                viv_cunetas_zanjas = "Prox. to roadside channels and ditches",
                viv_basura_acum = "Prox. to dump-yards",
                viv_baldios = "Prox. to vacant lots",
                viv_prox_rio = "Prox. to water bodies",
                viv_suelo_cubierto = "Floor impermeability",
                viv_techo_impermeable = "Roof impermeability",
                viv_acum_agua = "Acc. of rainwater or flooding",
                viv_acum_agua_30d = "Acc. of rainwater or flooding (30d)",
                viv_fuente_agua = "Source of drinking water",
                prac_cacharros = "Acc. of water containers",
                prac_resid_camion = "Regular garbage collection",
                prac_usa_agua_lluvia = "Storage of rainwater",
                prac_resid_fondo = "Garbage acc. in the backyard"
              )) |>  
  
  ## Añade significancia
  add_overall() |> 
  add_p()  |>  
  bold_p() |>  
  bold_labels() |>  
  
  ## Table layout
  modify_header(
    stat_0 ~ "**Total**",
    stat_1 ~ "**CH**",
    stat_2 ~ "**CS**",
    stat_3 ~ "**VP**")


## Exporta tabla
table1 |> 
  as_flex_table() |> 
  font(fontname = "calibri", part = "all") |> 
  fontsize(size = 12, part = "all") |> 
  align(align = "left", part = "all") |> 
  line_spacing(space = 1.5, part = "all") |> 
  save_as_docx(path = "table1.docx")


# Seropositividad (muestra completa) --------------------------------------
### Prevalencia total
tabyl(data_dengue_clean, ELISA_dengue) |>  
  adorn_pct_formatting()

### Prevalencia por barrio
tbl_summary(data = data_dengue_clean,
            by = ELISA_dengue,
            include = barrio) |>  
  add_p()

# calcula IC
attack_rate(cases = c(28, 14, 8, 6),
            population = c(184, 52, 71, 33), 
            mergeCI = T, digits = 1
)

### Prevalencia por sexo
tbl_summary(data = data_dengue_clean,
            by = ELISA_dengue,
            include = genero) |>  
  add_p()

### Genera tabla 
table2 <- data_dengue_clean |>  
  
  ### Selecciona variables
  select(genero:ocupacion_cat, sint_6m_alguno, 
         barrio, viv_tipo_calle_cat:viv_acum_agua_30d, viv_fuente_agua,
         prac_cacharros, prac_usa_agua_lluvia, prac_resid_camion,
         prac_resid_fondo, contains("dengue"), sint_6m_alguno,
         -starts_with("sint_den"), -starts_with("trasm_"), -ends_with("_dist")
  ) |>  
  
  ### Crea tabla
  tbl_summary(by = ELISA_dengue, 
              missing = "no",
              percent = "row",
              value = c(starts_with("viv_") & !(matches("viv_tipo_calle")|
                                                  matches("viv_fuente_agua")),
                        starts_with("prac_"), 
                        contains("dengue"), sint_6m_alguno) ~ "Si",
              digits = list(all_categorical() ~ c(0,1)),
              label = c(
                genero = "Gender",
                edad = "Age (years)",
                edad_cat = "Age group",
                educacion_cat = "Education",
                ocupacion_cat = "Occupation",
                viv_tipo_calle_cat = "Street type",
                viv_cunetas_zanjas = "Prox. to roadside channels and ditches",
                viv_basura_acum = "Prox. to dump-yards",
                viv_baldios = "Prox. to vacant lots",
                viv_prox_rio = "Prox. to water bodies",
                viv_suelo_cubierto = "Floor impermeability",
                viv_techo_impermeable = "Roof impermeability",
                viv_acum_agua = "Acc. of rainwater or flooding",
                viv_acum_agua_30d = "Acc. of rainwater or flooding (30d)",
                viv_fuente_agua = "Source of drinking water",
                prac_cacharros = "Acc. of water containers",
                prac_resid_camion = "Regular garbage collection",
                prac_usa_agua_lluvia = "Storage of rainwater",
                prac_resid_fondo = "Garbage acc. in the backyard",
                sint_6m_alguno = "Symptoms of febrile illness (6 months)",
                con_dengue = "Awareness of dengue",
                con_alguien_dengue = "Knew someone who had dengue",
                con_alguien_dengue_barrio = "Someone in the neighborhood had dengue",
                con_alguien_dengue_hogar = "Anyone in their household had dengue",
                con_sint_dengue = "Aware of any symptom",
                con_trasm_dengue = "Aware of any way of transmission"
              )) |>  
  
  ## Añade significancia
  add_p() |>  
  bold_p(t = .1) |>  
  bold_labels() |>  
  
  ## Table layout
  modify_header(stat_1 ~ "**{level} (n = {n})**",
                stat_2 ~ "**{level} (n = {n})**")


## Exporta tabla
table2 |>  as_flex_table() |>
  font(fontname = "calibri", part = "all") |>
  fontsize(size = 11, part = "all") |>
  align(align = "left", part = "all") |>
  line_spacing(space = 1.5, part = "all") |>
  save_as_docx(path = "table2.docx")


# Modelo regresión (s/ dengue previo) -------------------------------------
## Selecciona variables con P-valor < 0.1
predictors <- table2$table_body |> 
  filter(p.value <= .1) |> 
  select(variable) |> 
  pull()

## Limpia base
data_dengue_fit <- data_dengue_clean |> 
  select(all_of(predictors), barrio, ELISA_dengue) |> 
  
  # Elimina datos ausentes
  drop_na() |> 
  
  # VR a binomial
  mutate(ELISA_dengue_bin = if_else(ELISA_dengue == "POS", 1, 0))


### GLMM
# interacción tipo calle y baldíos
fit_glmm1 <- glmmTMB(ELISA_dengue_bin ~ viv_tipo_calle_cat * viv_baldios + 
                       prac_cacharros + (1|barrio),
                     family = binomial,
                     data = data_dengue_fit)

summary(fit_glmm1)

# interacción baldíos y cacharros
fit_glmm2 <- glmmTMB(ELISA_dengue_bin ~ viv_tipo_calle_cat + viv_baldios * 
                       prac_cacharros + (1|barrio),
                     family = binomial,
                     data = data_dengue_fit)

summary(fit_glmm2)

# interacción tipo calle y cacharros
fit_glmm3 <- glmmTMB(ELISA_dengue_bin ~ viv_tipo_calle_cat * prac_cacharros +
                       viv_baldios + (1|barrio),
                     family = binomial,
                     data = data_dengue_fit)

summary(fit_glmm3)


### Regresión logística
# interacción tipo calle y baldíos
fit_glm1 <- glmmTMB(ELISA_dengue_bin ~ barrio + viv_tipo_calle_cat * viv_baldios + prac_cacharros,
                    family = binomial,
                    data = data_dengue_fit)

summary(fit_glm1)

# interacción baldíos y cacharros
fit_glm2 <- glmmTMB(ELISA_dengue_bin ~ barrio + viv_tipo_calle_cat + viv_baldios * prac_cacharros,
                    family = binomial,
                    data = data_dengue_fit)

summary(fit_glm2)

# interacción tipo calle y cacharros
fit_glm3 <- glmmTMB(ELISA_dengue_bin ~ barrio +viv_tipo_calle_cat * prac_cacharros + viv_baldios,
                    family = binomial,
                    data = data_dengue_fit)

summary(fit_glm3)

### Compara modelos
compare_performance(fit_glmm1, fit_glmm2, fit_glmm3, fit_glm1, fit_glm2, fit_glm3,
                    metrics = "common", rank = F) |>  
  as_flextable()

compare_performance(fit_glmm1, fit_glmm2, fit_glmm3, fit_glm1, fit_glm2, fit_glm3,
                    metrics = "common", rank = T) 


## Selección de modelos x AIC
drop1(fit_glm1)

fit_glm1a <- update(fit_glm1, ~.-viv_tipo_calle_cat:viv_baldios)

drop1(fit_glm1a)

fit_glm1b <- update(fit_glm1a, ~.-viv_tipo_calle_cat)

drop1(fit_glm1b)

fit_glm1c <- update(fit_glm1b, ~.-prac_cacharros)

# compara modelos
compare_performance(fit_glm1a, fit_glm1b, fit_glm1c, 
                    metrics = "AIC", rank = T)

# coeficientes
tbl_regression(fit_glm1c, exponentiate = T)

# R2
r2(fit_glm1c)

# residuales
testResiduals(fit_glm3)

