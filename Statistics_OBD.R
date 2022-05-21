#Статистическая работа на данных о репрессиях и материалах военной базы ОБД Мемориал
library(tidyverse)

# Подгружаем данные по репрессиям
repressions_pop <- read_delim("/Users/annalevina/Desktop/Data/Международный мемориал/репрессии.csv", delim = ';')
# Подгружаем данные по выбытиям
reasons_final <- read_delim("/Users/annalevina/Desktop/Data/ОБД/reasons_final.csv", delim = ';')
# Подгружаем данные по численности населения
regions_research <- read_delim("/Users/annalevina/Desktop/Data/остальное/regions_research.csv", delim = ';')
regions_research <- regions_research %>% filter(` РСФСР` == 1) %>%
  select(`Регион 1937-1940`, `Население по переписи 1939 г. `) %>%
  unique()
regions_research %>% pull(`Регион 1937-1940`) %>% unique() %>% length() == nrow(regions_research)

# vibitiye_dezertirstvo
case_vibitiye_dezertirstvo <- reasons_final %>%
  filter(vibitiye_dezertirstvo == 1) %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(`Количество людей` = n()) %>% # Считаем количество людей с vibitiye_dezertirstvo = 1
  left_join(regions_research, by = c("Регион 1937-1940")) %>% # Добавляем информацию о численности населения
  mutate(`Количество всех случаев дезертирства на душу населения` = `Количество людей`/`Население по переписи 1939 г. `) %>% # Считаем количество на душу населения
  arrange(desc(`Количество людей`))

# vibitiye_only_dezertirstvo
case_vibitiye_only_dezertirstvo <- reasons_final %>%
  filter(vibitiye_only_dezertirstvo == 1) %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(`Количество людей` = n()) %>% # Считаем количество людей с vibitiye_only_dezertirstvo = 1
  left_join(regions_research, by = c("Регион 1937-1940")) %>% # Добавляем информацию о численности населения
  mutate(`Количество случаев явного дезертирства на душу населения` = `Количество людей`/`Население по переписи 1939 г. `) %>% # Считаем количество на душу населения
  arrange(desc(`Количество людей`))

# vibitiye_nakazanie
case_vibitiye_nakazanie <- reasons_final %>%
  filter(vibitiye_nakazanie == 1) %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(`Количество людей` = n()) %>% # Считаем количество людей с vibitiye_nakazanie = 1
  left_join(regions_research, by = c("Регион 1937-1940")) %>% # Добавляем информацию о численности населения
  mutate(`Количество наказаний на душу населения` = `Количество людей`/`Население по переписи 1939 г. `) %>% # Считаем количество на душу населения
  arrange(desc(`Количество людей`))

# vibitiye_plen
case_vibitiye_plen <- reasons_final %>%
  filter(vibitiye_plen == 1) %>%
  group_by(`Регион 1937-1940`) %>%
  summarise(`Количество людей` = n()) %>% # Считаем количество людей с vibitiye_plen = 1
  left_join(regions_research, by = c("Регион 1937-1940")) %>% # Добавляем информацию о численности населения
  mutate(`Количество плененных на душу населения` = `Количество людей`/`Население по переписи 1939 г. `) %>% # Считаем количество на душу населения
  arrange(desc(`Количество людей`))

# Объединим все данные на душу населения
data_test <- repressions_pop %>% 
  select(`Регион 1937-1940`, `Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         `арестовано в 1937 и 1938 по Мозохину на душу населения`) %>%
  left_join(case_vibitiye_dezertirstvo %>% select(`Регион 1937-1940`, `Количество всех случаев дезертирства на душу населения`),
            by = c("Регион 1937-1940")) %>%
  left_join(case_vibitiye_only_dezertirstvo %>% select(`Регион 1937-1940`, `Количество случаев явного дезертирства на душу населения`),
            by = c("Регион 1937-1940")) %>%
  left_join(case_vibitiye_nakazanie %>% select(`Регион 1937-1940`, `Количество наказаний на душу населения`),
            by = c("Регион 1937-1940")) %>%
  left_join(case_vibitiye_plen %>% select(`Регион 1937-1940`, `Количество плененных на душу населения`),
            by = c("Регион 1937-1940"))

# Проверяем корреляцию между количеством репрессированных и всеми случаями дезертирвства
# Мемориал
data_test%>%
  ggplot(aes(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`*100,
           `Количество всех случаев дезертирства на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm",  color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число репрессированных в 1937 и 1938 по Мемориалу в % от численности населения",
       y = "Число всех случаев дезертирства в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество всех случаев дезертирства на душу населения`)
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество всех случаев дезертирства на душу населения`,
         method = "spearman")
# Мозохин
data_test%>%
  ggplot(aes(`арестовано в 1937 и 1938 по Мозохину на душу населения`*100,
             `Количество всех случаев дезертирства на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число арестованных в 1937 и 1938 по Мозохину в % от численности населения",
       y = "Число всех случаев дезертирства в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество всех случаев дезертирства на душу населения`)
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество всех случаев дезертирства на душу населения`,
         method = "spearman")

# Проверяем корреляцию между количеством репрессированных и случаями явного дезертирвства
# Мемориал
data_test%>%
  ggplot(aes(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`*100,
             `Количество случаев явного дезертирства на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число репрессированных в 1937 и 1938 по Мемориалу в % от численности населения",
       y = "Число случаев явного дезертирства в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество случаев явного дезертирства на душу населения`)
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество случаев явного дезертирства на душу населения`,
         method = "spearman")

# Мозохин
data_test%>%
  ggplot(aes(`арестовано в 1937 и 1938 по Мозохину на душу населения`*100,
             `Количество случаев явного дезертирства на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число арестованных в 1937 и 1938 по Мозохину в % от численности населения",
       y = "Число случаев явного дезертирства в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество случаев явного дезертирства на душу населения`)
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество случаев явного дезертирства на душу населения`,
         method = "spearman")

# Проверяем корреляцию между количеством репрессированных и количеством наказаний
# Мемориал
data_test%>%
  ggplot(aes(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`*100,
             `Количество наказаний на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число репрессированных в 1937 и 1938 в % от численности населения",
       y = "Число наказаний в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция (да)
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество наказаний на душу населения`)
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество наказаний на душу населения`,
         method = "spearman")

# Мозохин
data_test%>%
  ggplot(aes(`арестовано в 1937 и 1938 по Мозохину на душу населения`*100,
             `Количество наказаний на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число арестованных в 1937 и 1938 по Мозохину в % от численности населения",
       y = "Число наказаний в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество наказаний на душу населения`)
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество наказаний на душу населения`,
         method = "spearman")

# Проверяем корреляцию между количеством репрессированных и количеством плененных
# Мемориал
data_test%>%
  ggplot(aes(`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`*100,
             `Количество плененных на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число репрессированных в 1937 и 1938 по Мемориалу в % от численности населения",
       y = "Число плененных в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция (да)
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество плененных на душу населения`)
cor.test(data_test$`Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения`,
         data_test$`Количество плененных на душу населения`,
         method = "spearman")

# Мозохин
data_test%>%
  ggplot(aes(`арестовано в 1937 и 1938 по Мозохину на душу населения`*100,
             `Количество плененных на душу населения`*100))+
  geom_point(color = "#d69c4e", size = 2)+
  geom_smooth(method = "lm", color = "#056c9a", fill = "#eccbae")+
  theme_minimal()+
  labs(x = "Число арестованных в 1937 и 1938 по Мозохину в % от численности населения",
       y = "Число плененных в % от численности населения")+
  theme(axis.text.y = element_text(colour = "black"), axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(size = 10), axis.title.x = element_text(size = 10))

# Корреляция (не значима)
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`,
         data_test$`Количество плененных на душу населения`)
# Корреляция Спирмена (не сильно значима)
cor.test(data_test$`арестовано в 1937 и 1938 по Мозохину на душу населения`, 
         data_test$`Количество плененных на душу населения`,
         method = "spearman")
