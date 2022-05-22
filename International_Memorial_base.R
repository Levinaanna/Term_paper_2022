#Работа с базой данных о репрессиях, -- Международный Мемориал

library(tidyverse)

memorial <- read_delim("/Users/annalevina/Desktop/Data/Международный мемориал/memorial_lists.tsv", delim = '\t')
glimpse(memorial)

# Выделяем год из колонки с датой процесса
memorial <- memorial %>% 
  filter(process_date != "None") %>%
  mutate(process_date_year = map_dbl(process_date, ~ strsplit(., "\\.")[[1]][3] %>% as.numeric()))

# Оставляем только 37-38 год процессов
memorial <- memorial %>%
  filter(process_date_year == 1937 | process_date_year == 1938)

# Добавляем колонку с современным названием регионов на основе колонки с книгами памяти
map_book_reg <- read_delim("/Users/annalevina/Desktop/Data/Международный мемориал/Books_table.csv", delim = ';')
memorial <-  left_join(memorial, map_book_reg, by = c("memory_book" = "initial_memory_book"))
memorial <- memorial %>% filter(!is.na(modern_names))

# Отфильтруем регионы РСФСР
memorial <- memorial %>% filter(RSFSR == 1)

# Составим рейтинг репрессивности регионов 
memorial_repressions_new_regions <- memorial %>% 
  group_by(modern_names) %>%
  summarise(num_repress = n()) %>%
  arrange(desc(num_repress))
memorial_repressions_new_regions %>%
  write_delim("/Users/annalevina/Desktop/Data/Международный мемориал/memorial_repression_new_regions.csv", delim = ';')

# Добавим к данным по репрессиям в современных регионах территориальную разбивку 37-40 годов и численность населения в этих регионах
memorial_repressions_new_regions <- read_delim("/Users/annalevina/Desktop/Data/Международный мемориал/memorial_repression_new_regions.csv", delim = ';')
regions_research <- read_delim("/Users/annalevina/Desktop/Data/остальное/regions_research.csv", delim = ';')

memorial_repressions_old_regions <- left_join(memorial_repressions_new_regions, 
                              regions_research %>% select("Современный регион", " РСФСР", "Регион 1937-1940"),
                              by = c("modern_names" = "Современный регион"))
memorial_repressions_old_regions %>% filter(` РСФСР` != 1) %>% nrow()

# Проверим нет ли современных областей для которых не указана область 37-40 годов
memorial_repressions_old_regions %>% filter(is.na(`Регион 1937-1940`)) %>% nrow()

# Считаем репрессивность регионов 37-40 годов
memorial_repressions_old_regions <- memorial_repressions_old_regions %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(num_repress = sum(num_repress)) %>%
  arrange(desc(num_repress))
memorial_repressions_old_regions %>%
  write_delim("/Users/annalevina/Desktop/Data/Международный мемориал/memorial_repression_old_regions.csv", delim = ';')

# Подгружаем файл с установленным соответствием регионов 37-40 годов и современных регионов
regions_research <- read_delim("/Users/annalevina/Desktop/Data/остальное/regions_research.csv", delim = ';')
regions_research <- regions_research %>% 
  filter(` РСФСР` == 1) %>% #Выбираем регионоы РСФСР
  select(`Регион 1937-1940`, `Население по переписи 1939 г. `, `арестовано в 1937 и 1938 по Мозохину`) %>%
  unique()
#Проверка отсутствия повторяющихся регионов
regions_research %>% 
  pull(`Регион 1937-1940`)%>%
  unique()%>% 
  length() == nrow(regions_research)

# Формируем итоговый файл, собирая данные
repressions_pop <- memorial_repressions_old_regions %>%
  rename(`Количество репрессированных в 1937 и 1938 по Мемориалу` = num_repress) %>%
  left_join(regions_research, by = 'Регион 1937-1940') %>%
  mutate(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения` = `Количество репрессированных в 1937 и 1938 по Мемориалу`/`Население по переписи 1939 г. `,
         `арестовано в 1937 и 1938 по Мозохину на душу населения` = `арестовано в 1937 и 1938 по Мозохину`/`Население по переписи 1939 г. `)
repressions_pop %>%
  write_delim("/Users/annalevina/Desktop/Data/Международный мемориал/репрессии.csv", delim = ';')
