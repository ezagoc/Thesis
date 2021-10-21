# Generando la base de datos de Google Trends
# gTrendsR 
# Autor: Eduardo Zago

# 1.0) Libraries

if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl",repos="http://cran.r-project.org")}
try(suppressPackageStartupMessages(library(readxl,quietly = TRUE,warn.conflicts = FALSE)),silent = TRUE)
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr",repos="http://cran.r-project.org")}
try(suppressPackageStartupMessages(library(tidyr,quietly = TRUE,warn.conflicts = FALSE)),silent = TRUE)
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr",repos="http://cran.r-project.org")}
try(suppressPackageStartupMessages(library(dplyr,quietly = TRUE,warn.conflicts = FALSE)),silent = TRUE)
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr",repos="http://cran.r-project.org")}
try(suppressPackageStartupMessages(library(stringr,quietly = TRUE,warn.conflicts = FALSE)),silent = TRUE)
if("abind" %in% rownames(installed.packages()) == FALSE) {install.packages("abind",repos="http://cran.r-project.org")}
try(suppressPackageStartupMessages(library(abind,quietly = TRUE,warn.conflicts = FALSE)),silent = TRUE)
library(gtrendsR)
library(ggplot2)


# Path out

path_out <- "C:/Users/lalo-/OneDrive/Documentos/Tesis/Bases_de_Datos/Limpias"



# State Codes

claves_mex <- countries %>% filter(country_code == "MX")
claves_mex <- claves_mex[2:33,1:3]
claves_mex <- as.data.frame(claves_mex[['sub_code']])

# Function to scrape data directly from Google Trends

get_trendsR <- function(clave, query, fecha){
     
     trendsR <- gtrends(query, clave, fecha)
     
     df_trendsR <- trendsR[['interest_over_time']]
     df_trendsR <- df_trendsR %>% select(date, geo, hits) %>% 
          rename(est = geo, trend = hits) %>% 
          mutate(sem = as.numeric(strftime(date, format = "%V"))) %>%
          mutate(clavEst = as.numeric(as.factor(est)))
     
     return(df_trendsR)
}

claves_mex_gen <- c("MX-DIF", "MX-JAL", "MX-MEX", "MX-VER", "MX-PUE")

gen_2020 <- claves_mex_gen %>% 
     lapply(function(x){get_trendsR(x, "violencia de genero", 
                                    "2020-02-01 2020-04-30")})
gen_2020 <- abind(gen_2020, along = 1)
row.names(gen_2020) <- NULL

gen_2020 <- data.frame(unlist(gen_2020[,1]), unlist(gen_2020[,2]), 
                       as.numeric(unlist(gen_2020[,3])),
                       as.numeric(unlist(gen_2020[,4])), 
                       as.numeric(unlist(gen_2020[,5])))
colnames(gen_2020) <- c("date", "est", "trend", "sem", "clavEst")

#2019
gen_2019 <- claves_mex_gen %>% 
     lapply(function(x){get_trendsR(x, "violencia de genero", 
                                    "2019-02-01 2019-04-30")})
gen_2019 <- abind(gen_2019, along = 1)
row.names(gen_2019) <- NULL

gen_2019 <- data.frame(unlist(gen_2019[,1]), unlist(gen_2019[,2]), 
                       as.numeric(unlist(gen_2019[,3])),
                       as.numeric(unlist(gen_2019[,4])), 
                       as.numeric(unlist(gen_2019[,5])))
colnames(gen_2019) <- c("date", "est", "trend", "sem", "clavEst")

# 2019 - 2020
gen_tot <- claves_mex_gen %>% 
     lapply(function(x){get_trendsR(x, "violencia de genero", 
                                    "2019-02-01 2020-04-30")})
gen_tot <- abind(gen_tot, along = 1)
row.names(gen_tot) <- NULL

gen_tot <- data.frame(unlist(gen_tot[,1]), unlist(gen_tot[,2]),
                      as.numeric(unlist(gen_tot[,3])))
colnames(gen_tot) <- c("date", "est", "trend")

# Promedios semanales de todo el periodo:

gen_tot <- gen_tot %>% mutate(treat = if_else(format(as.Date(date), "%Y") == 2020, 1, 0),
                              sem = as.numeric(strftime(date, format = "%V")))

# Unimos y limpiamos:

gen <- rbind(gen_2020, gen_2019)

gen <- gen %>% 
     mutate(treat = if_else(format(as.Date(date), "%Y") == 2020, 1, 0))

# Sacamos promedios semanales:

gen_sem <- gen %>% group_by(est, sem, treat) %>% 
     summarise(m_trends_sem = mean(trend)) %>% ungroup()

# Reescalamos: 

gen_reesc <- gen %>% left_join(gen_tot, by = c("est", "sem", "treat")) %>% 
     left_join(gen_sem, by = c("est", "sem", "treat")) %>% 
     select(date.x, est, trend.x, trend.y, m_trends_sem, treat, sem) %>%
     rename(date = date.x, trend = trend.x, m_trends = trend.y)


gen_reesc <- gen_reesc %>% mutate(w_sem = m_trends/m_trends_sem) %>% 
     mutate(trend_aux = trend*w_sem) %>%
     mutate(trend_reesc = trend_aux/max(trend_aux, na.rm = T)*100) %>%
     mutate(trend_reesc = if_else(trend_reesc == "NaN", 0, trend_reesc))

summary(gen_reesc)

gen_reesc <- gen_reesc %>% select(date, est, treat, sem, trend_reesc) %>%
     mutate(clavEst = as.numeric(as.factor(est))) %>% filter(sem != 18)

gen_reesc_Mex <- gen_reesc %>% group_by(date, treat) %>% 
     summarise(trend = mean(trend_reesc)) %>% ungroup()


# ----------------------------------------------------------------------------
# Resultados desde aquí, importamos la base ya creada:



# Sacamos los promedios de "México" por dia

gen_reesc_2020 <- gen_reesc_Mex %>% filter(treat == 1) %>% select(date, trend) %>%
     filter(date != "2020-02-09") %>% mutate(dias = row_number(date)) %>%
     mutate(dias_dif = dias - 29) %>% 
     select(dias_dif, trend)
gen_reesc_2019 <- gen_reesc_Mex %>% filter(treat == 0) %>% select(date, trend) %>%
     mutate(dias = row_number(date)) %>% mutate(dias_dif = dias - 29) %>% 
     select(dias_dif, trend)


write.csv(gen_reesc, file = paste0(path_out,"/trendsgen_limpia2.csv"), row.names = FALSE)

# - --------------------------------------------------------------------------

# Para no volver a correr el código simplemente jalamos la base echa:


path_in <- "C:/Users/lalo-/OneDrive/Documentos/Tesis/Bases_de_Datos/Limpias/trendsgen_limpia2.csv"

# Importamos los Excels que necesitamos:

gen_reesc_import <- read.csv(path_in)

gen_reesc_Mex <- gen_reesc_import %>% group_by(date, treat) %>% 
        summarise(trend = mean(trend_reesc)) %>% ungroup()


# Sacamos los promedios de "México" por dia

gen_reesc_2020 <- gen_reesc_Mex %>% filter(treat == 1) %>% select(date, trend) %>%
        filter(date != "2020-02-09") %>% mutate(dias = row_number(date)) %>%
        mutate(dias_dif = dias - 29) %>% 
        select(dias_dif, trend)
gen_reesc_2019 <- gen_reesc_Mex %>% filter(treat == 0) %>% select(date, trend) %>%
        mutate(dias = row_number(date)) %>% mutate(dias_dif = dias - 29) %>% 
        select(dias_dif, trend)


setwd("C:/Users/lalo-/OneDrive/Documentos/Tesis/Out")

ggplot(gen_reesc_2019, aes(dias_dif, trend)) + geom_line(aes(color="Interés 2019"), size = .8)+
     geom_line(data = gen_reesc_2020, aes(color = "Interés 2020"), size = .8) +
     labs(color="", x = "Días diferidos", y = "") + 
     theme_minimal() + scale_color_manual(values=c("black", "darkred")) +
     geom_vline(xintercept=0, color="black", linetype="dotted") + 
     geom_label(mapping = aes(x = 0, y = 100), label = "1ro de marzo", colour='black') +
     ggtitle("Interés de búsqueda en Google re-escalado", subtitle = "Query: Violencia de Género") 
ggsave("Trends_ViolGen2.jpeg",  width = 8, height = 4.95)

# Generamos las de tratamiento:

gen_reesc_import <- gen_reesc_import %>% mutate(treat1 = if_else(date > "2020-03-08", 1, 0), 
                                  treat2 = if_else(date > "2020-03-30", 1, 0),
                                  diasem = as.numeric(as.factor(weekdays(as.Date(date))))) %>%
     rename(treat = dummy_year)

write.csv(gen_reesc_import, file = paste0(path_in), row.names = FALSE)


# -----------------------------------------------------------------------------------

# Now we need the daily GTrend data for queries: "Coronavirus" and "COVID-19"
# This to present evidence that Google user interest for these words, and in a sense, for the 
# ongoing pandemic didn´t start until march

clave_mex <- c("MX")

# Corona

corona <- clave_mex %>% 
        lapply(function(x){get_trendsR(x, "Coronavirus", 
                                       "2020-01-01 2020-04-30")})
corona <- abind(corona, along = 1)
row.names(corona) <- NULL

corona <- data.frame(unlist(corona[,1]), unlist(corona[,2]), 
                       as.numeric(unlist(corona[,3])),
                       as.numeric(unlist(corona[,4])), 
                       as.numeric(unlist(corona[,5])))
colnames(corona) <- c("date", "est", "trend", "sem", "clavEst")

# There´s some NAs, this because Google Trends assigns "<1" to those values.
# I assume those values are = 0

corona <- corona %>% mutate(trend = if_else(is.na(trend) == T, 1, trend),
                            date = as.Date(date))

# Covid-19

covid <- clave_mex %>% 
        lapply(function(x){get_trendsR(x, "Covid-19", 
                                       "2020-01-01 2020-04-30")})
covid <- abind(covid, along = 1)
row.names(covid) <- NULL

covid <- data.frame(unlist(covid[,1]), unlist(covid[,2]), 
                     as.numeric(unlist(covid[,3])),
                     as.numeric(unlist(covid[,4])), 
                     as.numeric(unlist(covid[,5])))
colnames(covid) <- c("date", "est", "trend", "sem", "clavEst")

covid <- covid %>% mutate(trend = if_else(is.na(trend) == T, 1, trend), 
                          date = as.Date(date))


# Graph



ggplot(covid, aes(date, trend)) + geom_line(aes(color="Interés 'COVID-19'"), size = .8)+
        geom_line(data = corona, aes(color = "Interés 'Coronavirus'"), size = .8) +
        labs(color="", x = "Mes", y = "") + 
        theme_minimal() + scale_color_manual(values=c("darkblue", "darkred")) +
        geom_vline(xintercept=0, color="black", linetype="dotted") +
        ggtitle("Figura 5: Interés de búsqueda en Google", subtitle = "Queries: Coronavirus y COVID-19") 
ggsave("InteresCOVID.jpeg",  width = 8, height = 4.95)
