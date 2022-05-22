#Работа с базой данных "Подвиг народа"
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(scales)
library(wesanderson)
wes_palette("Darjeeling2")

df <- read_delim("/Users/annalevina/Downloads/split_chelovek_nagrazhdenie/chelovek_nagrazhdenie.csv", delim = "~")
podvig_nagrazhdenie_5000 <- head(df, 5000)

#Выбираю строки файла, в которых указано место призыва
process <- df %>%
  filter(!is.na(mesto_priziva))
unique(process$naimenovanie_nagradi)
process_5000 <- head(process, 5000)

# Добавляю пробел для места призыва вроде РВКМосква
process <- process %>%
  mutate(add_space = grepl("РВК[А-Я]", mesto_priziva),
         mesto_priziva_changed = ifelse(add_space, gsub("(.*)(РВК)([А-Я].*)", "\\1\\2, \\3", mesto_priziva), mesto_priziva)) %>%
  select(-add_space)

# Добавляю запятую для места призыва вида _._.1941 Москва
process <- process %>%
  mutate(add_comma = grepl("\\..*\\.19[0-9]{2} ", mesto_priziva_changed),
         mesto_priziva_changed = ifelse(add_comma, gsub("(\\..*\\.19[0-9]{2})( .*)", "\\1,\\2", mesto_priziva_changed), mesto_priziva_changed)) %>%
  select(-add_comma)

process <- process %>%
  mutate(mesto_priziva_changed_1 = map_chr(mesto_priziva_changed, ~ strsplit(., ", ")[[1]][1]),
         mesto_priziva_changed_2 = map_chr(mesto_priziva_changed, ~ strsplit(., ", ")[[1]][2]),
         mesto_priziva_changed_3 = map_chr(mesto_priziva_changed, ~ strsplit(., ", ")[[1]][3]))

process <- process %>%
  mutate(is_number = grepl("\\..*\\.", mesto_priziva_changed_1), # Проверяем есть ли дата в месте призыва
         region = ifelse(is_number, mesto_priziva_changed_3, mesto_priziva_changed_2)) # Вытаскиваем регион

process <- process %>% 
  filter(!is.na(region) & !grepl("г\\.", region))

process <- process %>% 
  filter(!grepl("р\\-н", region))

#Выбираю нужный сегмент поля "Место призыва"
process <- process %>%
  mutate(region = ifelse(grepl("РВК", region), ifelse(is_number, mesto_priziva_changed_2, mesto_priziva_changed_1), region))

process %>%
  pull(region) %>%
  unique()

regions_corrected <- read_delim("/Users/annalevina/Desktop/Data/остальное/regions_obd.csv", delim = ";")
podvig <- process %>% left_join(regions_corrected %>% mutate(is_present = 1), by = "region")
# Убираем регионы, для которых у нас нет мэппинга (10 000 строк)
podvig <- podvig %>% filter(!is.na(is_present))
#Убраем неразборчивые (пустые) скорректированные названия регионов
podvig <- podvig %>% filter(!is.na(region_corrected))
podvig %>% pull(naimenovanie_nagradi) %>% as.factor() %>% summary() %>% sort(decreasing = T)
podvig <- podvig %>%
  rename(`Регион 1937-1940` = region_corrected) %>%
  select(id, `Регион 1937-1940`, naimenovanie_nagradi)
podvig %>%
  write_delim("/Users/annalevina/Desktop/Data/Podvig/podvig_final.csv", delim = ";")


#Количесвто полученных наград
podvig%>%
  filter(naimenovanie_nagradi %in% names(sort(summary(as.factor(process$naimenovanie_nagradi)), decreasing = T)[1:10]))%>%
  count(naimenovanie_nagradi)%>%
  ggplot(aes(x = fct_reorder(naimenovanie_nagradi, n),n))+
  geom_col(fill="#d69c4e")+
  scale_y_continuous(labels = label_number()) +
  theme_minimal()+
  coord_flip()+
  labs(y = "Количество полученных наград", x = "Название награды") +
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"))
