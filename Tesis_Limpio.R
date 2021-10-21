# Cleaning the data:
# Author: Eduardo Zago

# 0.0) Libraries

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

# 0.1 Defining the path out

path_out <- "C:/Users/lalo-/OneDrive/Documentos/Tesis/Bases_de_Datos/Limpias"

# 0.2 Importing the data:

# Calls to 911 in Mexico City

path_in_911 <- "C:/Users/lalo-/OneDrive/Documentos/Tesis/Bases_de_Datos/911_CDMX.xlsx"
datos_911 <- read_excel(path_in_911, col_names = FALSE)

# Google Regional Mobility Reports

path_in_mov <- "C:/Users/lalo-/OneDrive/Documentos/Tesis/Bases_de_Datos/2020_MX_Region_Mobility_Report.csv"
datos_mov <- read.csv(path_in_mov)

# 1.0 Cleaning the Calls data

datos_911 <- datos_911 %>% select(...1, ...2,...3,...4) %>% rename(date = ...1, A = ...2, otros = ...3,
                                                              total = ...4) %>%
     filter(is.na(A) == FALSE) %>% filter(date != "FECHA") %>% 
     mutate(date = as.Date(as.numeric(date), origin =  "1899-12-30"), A = as.numeric(A), 
            otros = as.numeric(otros), total = as.numeric(total)) %>% drop_na()

datos_911 <- datos_911 %>% mutate(year = as.numeric(format(date, format="%Y")), 
                                  mes = as.numeric(format(date, format="%m")),
                                  diasem = as.numeric(as.factor(weekdays(date))),
                                  dia = as.numeric(format(date, format="%d")),
                                  sem = as.numeric(strftime(date, format = "%V")),
                                  treat = if_else(year == 2020, 1,0))

datos_911 <- datos_911 %>% filter(sem < 27)


write.csv(datos_911, file = paste0(path_out,"/911_limpia1.csv"), row.names = T)

# 2.0 Cleaning the Regional Mobility Data:

datos_mov <- datos_mov %>% select(sub_region_1, date, residential_percent_change_from_baseline) %>%
     rename(est = sub_region_1, residential = residential_percent_change_from_baseline)

write.csv(datos_mov, file = paste0(path_out,"/mov_limpia.csv"), row.names = FALSE)

datos_mov <- datos_mov %>% mutate(date = as.Date(date))

datos_mov <- datos_mov %>% mutate(year = as.numeric(format(date, format="%Y")), 
                                  mes = as.numeric(format(date, format="%m")),
                                  diasem = as.numeric(as.factor(weekdays(date))),
                                  dia = as.numeric(format(date, format="%d")),
                                  sem = as.numeric(strftime(date, format = "%V")))

write.csv(datos_mov, file = paste0(path_out,"/mov_limpia.csv"), row.names = FALSE)


# 3.0 Data preparation: Graphing

sem_911 <- datos_911 %>% group_by(year, sem) %>% 
        summarise_at(c("A", "otros", "total"), mean, na.rm = TRUE)

sem_911_2019 <- sem_911 %>% filter(year == 2019)
sem_911_2020 <- sem_911 %>% filter(year == 2020)

sem_911_merge <- sem_911_2020 %>% left_join(sem_911_2019, by = c("sem"))

sem_graph <- sem_911_merge %>% select(sem, A.x, A.y, otros.x, otros.y, total.x, total.y) %>% 
        rename(A_2020 = A.x, A_2019 = A.y, otros_2020 = otros.x, otros_2019 = otros.y,
               total_2020 = total.x, total_2019 = total.y)

sem_graph <- sem_graph %>% mutate(sem_dif = sem - 12) %>% select(sem_dif, total_2019, total_2020)

sem_graph_2019 <- sem_graph[,1:2]
sem_graph_2020 <- data.frame(sem_graph[,1], sem_graph[,3])

sem_graph_2020 <- sem_graph_2020 %>% rename(total = total_2020)
sem_graph_2019 <- sem_graph_2019 %>% rename(total = total_2019)

# Movilidad

datos_mov_cdmx <- datos_mov %>% filter(est == "Mexico City") %>% group_by(sem) %>%
        summarise(residential = mean(residential))%>% ungroup()

datos_aux <- sem_911 %>% ungroup() %>% filter(year == 2020) %>% select(sem) %>%
        filter(sem < 7) %>% mutate(residential = 0)

datos_mov_cdmx <- rbind(datos_aux, datos_mov_cdmx)

datos_mov_cdmx <- datos_mov_cdmx %>% mutate(sem_dif = sem - 12) %>% 
        select(sem_dif, residential) %>% rename(total = residential)

datos_mov_aux <- datos_mov_cdmx %>% mutate(total = 30)

# 5.0 Graph: Weekly comparison of 911 data for 2019 and 2020, alngside weekly data of mobility
# for Mexico City

setwd("C:/Users/lalo-/OneDrive/Documentos/Tesis/Out")

p1 <- ggplot(sem_graph_2019, aes(sem_dif, total)) + geom_line(aes(color="Llamadas 2019"), size = .6)+
        geom_line(data = sem_graph_2020, aes(color = "Llamadas 2020"), size = .6) + 
        scale_color_manual(values=c("darkred", "black")) +
        labs(color="", x = "", y = "") + 
        theme_minimal() + ylim(200, 470) +
        geom_vline(xintercept=0, color="black", linetype="dotted") + 
        geom_label(mapping = aes(x = 0, y = 450, 
                                 label = "Aislamiento"), colour='black') + 
        ggtitle("Promedio de llamadas semanales al 911 y movilidad (CDMX)")

p2 <- ggplot(datos_mov_cdmx, aes(sem_dif, total)) + 
        geom_line(aes(color="Movilidad (Residencial)"), size = .6) +
        geom_line(data = datos_mov_aux, aes(color = ""), size = .6) + 
        scale_color_manual(values=c("white", "darkblue")) +
        labs(color="", x = "Semanas diferidas (Aislamiento)", y = "") + 
        theme_minimal() + 
        geom_vline(xintercept=0, color="black", linetype="dotted")


plibrary(grid)
gb1 <- ggplot_build(p1)
gb2 <- ggplot_build(p2)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)

ggsave("911_stacked.jpeg",  width = 8, height = 4.95)
