# Работа с базой ОБД Мемориал

library(tidyverse)

Chelovek_dopolnitelnoe_donesenie <- read_delim('/Users/annalevina/Downloads/OBD/chelovek_dopolnitelnoe_donesenie.csv', delim = '~')
Chelovek_dopolnitelnoe_donesenie_reduced <- head(Chelovek_dopolnitelnoe_donesenie, 5000)

summary(as.factor(Chelovek_dopolnitelnoe_donesenie$prichina_vibitiya))

Chelovek_donesenie <- read_delim('/Users/annalevina/Downloads/OBD/chelovek_donesenie.csv', delim = ';')
Chelovek_donesenie_reduced <- head(Chelovek_donesenie, 5000)

summary(as.factor(Chelovek_donesenie$prichina_vibitiya))

#Соединяем файлы человек-донесение и человек-дополнительное донесение
colnames(Chelovek_dopolnitelnoe_donesenie)
colnames(Chelovek_donesenie)

doneseniya_obd <- rbind(Chelovek_donesenie, Chelovek_dopolnitelnoe_donesenie)
doneseniya_obd <- doneseniya_obd %>%
  filter(data_i_mesto_priziva != "NA")

unique(doneseniya_obd$prichina_vibitiya)

doneseniya_obd_reasons <- doneseniya_obd %>%
  mutate(vibitiye_dezertirstvo = ifelse(prichina_vibitiya %in% c("дезертировал", "дезертир", "перешел на сторону врага", "остался на захваченной территории",
                                                                 "попал в плен (сотрудничал с врагом)", "измена Родине"), 1, 0),
         vibitiye_only_dezertirstvo = ifelse(prichina_vibitiya %in% c("дезертировал", "дезертир"), 1, 0),
         vibitiye_nakazanie = ifelse(prichina_vibitiya %in% c("ВМН", "отправлен в штрафную часть", "осужден (реабилитирован)", "осужден (заочно)",
                                                              "арестован", "осужден"), 1, 0),
         vibitiye_plen = ifelse(prichina_vibitiya %in% c("попал в плен", "погиб в плену", "попал в плен (сотрудничал с врагом)",
                                                         "Попал в плен (освобожден)", "попал в плен (освобожден)", "Попал в плен (освобожден)",
                                                         "попала в плен (освобождена)", "попал в плен (бежал)"), 1, 0))

doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  filter(vibitiye_dezertirstvo == 1 | vibitiye_only_dezertirstvo == 1 | vibitiye_nakazanie == 1 | vibitiye_plen == 1)

# Добавляем пробел для места призыва вроде "РВКМосква"
doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  mutate(add_space = grepl("РВК[А-Я]", data_i_mesto_priziva),
         data_i_mesto_priziva_changed = ifelse(add_space, gsub("(.*)(РВК)([А-Я].*)", "\\1\\2, \\3", data_i_mesto_priziva), data_i_mesto_priziva)) %>%
  select(-add_space)

# Добавляем запятую для места призыва вида _._.1941 Москва
doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  mutate(add_comma = grepl("\\..*\\.19[0-9]{2} ", data_i_mesto_priziva_changed),
         data_i_mesto_priziva_changed = ifelse(add_comma, gsub("(\\..*\\.19[0-9]{2})( .*)", "\\1,\\2", data_i_mesto_priziva_changed), data_i_mesto_priziva_changed)) %>%
  select(-add_comma)

doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  mutate(data_i_mesto_priziva_1 = map_chr(data_i_mesto_priziva_changed, ~ strsplit(., ", ")[[1]][1]),
         data_i_mesto_priziva_2 = map_chr(data_i_mesto_priziva_changed, ~ strsplit(., ", ")[[1]][2]),
         data_i_mesto_priziva_3 = map_chr(data_i_mesto_priziva_changed, ~ strsplit(., ", ")[[1]][3]))

doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  mutate(is_number = grepl("\\..*\\.", data_i_mesto_priziva_1), # Проверяем есть ли дата в месте призыва
         region = ifelse(is_number, data_i_mesto_priziva_3, data_i_mesto_priziva_2)) # Вытаскиваем регион

doneseniya_obd_reasons <- doneseniya_obd_reasons %>% 
  filter(!is.na(region) & !grepl("г\\.", region))

doneseniya_obd_reasons <- doneseniya_obd_reasons %>% 
  filter(!grepl("р\\-н", region))

#Выбираем нужный сегмент поля "Место призыва"
doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  mutate(region = ifelse(grepl("РВК", region), ifelse(is_number, data_i_mesto_priziva_2, data_i_mesto_priziva_1), region))

#Добавляем файл с названиями регионов, скорректированными вручную
regions_corrected <- read_delim("/Users/annalevina/Desktop/Data/остальное/regions_obd.csv", delim = ";")
doneseniya_obd_reasons <- left_join(doneseniya_obd_reasons, 
                                    regions_corrected %>% mutate(is_present = 1),
                                    by = "region")
doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  filter(region != "")

#Проверяем, все ли уникальные названия регионов скорректированны
doneseniya_obd_reasons %>% filter(is.na(is_present)) %>% nrow() == 0
doneseniya_obd_reasons <- doneseniya_obd_reasons %>% select(-is_present)

#Убраем неразборчивые (пустые) скорректированные названия регионов
doneseniya_obd_reasons <- doneseniya_obd_reasons %>%
  filter(!is.na(region_corrected))

#Формируем итоговый файл
reasons_final <- doneseniya_obd_reasons %>%
  rename(`Регион 1937-1940` = region_corrected) %>%
  select(id, `Регион 1937-1940`, vibitiye_dezertirstvo, vibitiye_only_dezertirstvo, vibitiye_nakazanie,
         vibitiye_plen, rank)

reasons_final %>%
  write_delim("/Users/annalevina/Desktop/Data/ОБД/reasons_final.csv", delim = ';')
