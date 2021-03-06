#Статистическая работа на данных о репрессиях и материалах базы "Подвиг народа"
library(tidyverse)

# Подгружаем данные по репрессиям
repressions_pop <- read_delim("/Users/annalevina/Desktop/Data/Международный мемориал/репрессии.csv", delim = ';')
# Подгружаем данные по наградам
podvig_final <- read_delim("/Users/annalevina/Desktop/Data/Podvig/podvig_final.csv", delim = ';')
# Подгружаем данные по численности населения
regions_research <- read_delim("/Users/annalevina/Desktop/Data/остальное/regions_research.csv", delim = ';')
regions_research <- regions_research %>% filter(` РСФСР` == 1) %>%
  select(`Регион 1937-1940`, `Население по переписи 1939 г. `) %>%
  unique()
regions_research %>% pull(`Регион 1937-1940`) %>% unique() %>% length() == nrow(regions_research)

# посчитаем количество людей с медалями за отвагу и боевые заслуги
podvig_otvaga_zaslugi <- podvig_final %>%
  filter(naimenovanie_nagradi %in% c("Медаль «За отвагу»", "Медаль «За боевые заслуги»")) %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(`Количество людей` = n()) %>% # Считаем количество людей
  left_join(regions_research, by = c("Регион 1937-1940")) %>% # Добавляем информацию о численности населения
  mutate(`Количество медалей за отвагу и боевые заслуги на душу населения` = `Количество людей`/`Население по переписи 1939 г. `) %>% # Считаем количество на душу населения
  arrange(desc(`Количество людей`))

# посчитаем количество людей со всеми медалями
podvig_all_medals <- podvig_final %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(`Количество людей` = n()) %>% # Считаем количество людей
  left_join(regions_research, by = c("Регион 1937-1940")) %>% # Добавляем информацию о численности населения
  mutate(`Количество медалей на душу населения` = `Количество людей`/`Население по переписи 1939 г. `) %>% # Считаем количество на душу населения
  arrange(desc(`Количество людей`))

# Объединим все данные на душу населения
data_test <- repressions_pop %>% 
  select(`Регион 1937-1940`, `Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         `арестовано в 1937 и 1938 по Мозохину на душу населения`) %>%
  left_join(podvig_otvaga_zaslugi %>% select(`Регион 1937-1940`, `Количество медалей за отвагу и боевые заслуги на душу населения`),
            by = c("Регион 1937-1940")) %>%
  left_join(podvig_all_medals %>% select(`Регион 1937-1940`, `Количество медалей на душу населения`),
            by = c("Регион 1937-1940"))

# Проверяем корреляцию между количеством репрессированных и количеством медалей за отвагу и боевые заслуги на душу населения
# Мемориал
data_test %>%
  filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
  ggplot(aes(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`*100,
             `Количество медалей за отвагу и боевые заслуги на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm",  color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "\nЧисло репрессированных в 1937 и 1938 по Мемориалу\nв % от численности населения",
       y = "Число медалей за отвагу и боевые заслуги\nв % от численности населения\n")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))

# Корреляция
cor.test(data_test %>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`),
         data_test %>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей за отвагу и боевые заслуги на душу населения`))
cor.test(data_test %>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`),
         data_test %>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей за отвагу и боевые заслуги на душу населения`),
         method = "spearman")
# Мозохин
data_test%>%
  filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
  ggplot(aes(`арестовано в 1937 и 1938 по Мозохину на душу населения`*100,
             `Количество медалей за отвагу и боевые заслуги на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "\nЧисло арестованных в 1937 и 1938 по Мозохину \nв % от численности населения",
       y = "Число медалей за отвагу и боевые заслуги \nв % от численности населения\n")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))

# Корреляция
cor.test(data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`арестовано в 1937 и 1938 по Мозохину на душу населения`),
         data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей за отвагу и боевые заслуги на душу населения`))
cor.test(data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`арестовано в 1937 и 1938 по Мозохину на душу населения`),
         data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей за отвагу и боевые заслуги на душу населения`), method = "spearman")

# Проверяем корреляцию между количеством репрессированных и количеством медалей на душу населения
# Мемориал
data_test %>%
  filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
  ggplot(aes(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`*100,
             `Количество медалей на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "\nЧисло репрессированных в 1937 и 1938 по Мемориалу \nв % от численности населения",
       y = "Число медалей в % от численности населения\n")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))

# Корреляция
cor.test(data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`),
         data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей на душу населения`))
cor.test(data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`),
         data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей на душу населения`), method = "spearman")

# Мозохин
data_test%>%
  filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
  ggplot(aes(`арестовано в 1937 и 1938 по Мозохину на душу населения`*100,
             `Количество медалей на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "\nЧисло арестованных в 1937 и 1938 по Мозохину \nв % от численности населения",
       y = "Число медалей в % от численности населения\n")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12))

# Корреляция
cor.test(data_test%>%
           filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`арестовано в 1937 и 1938 по Мозохину на душу населения`),
         data_test%>%
         filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
         pull(`Количество медалей на душу населения`))
cor.test(data_test%>%
         filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`арестовано в 1937 и 1938 по Мозохину на душу населения`),
         data_test%>%
         filter(!(`Регион 1937-1940` %in% c("Воронежская область", "Тамбовская область (всего)", "Калининская область", "Куйбышевская область", "Ленинградская область", "Саратовская область"))) %>%
           pull(`Количество медалей на душу населения`), method = "spearman")
