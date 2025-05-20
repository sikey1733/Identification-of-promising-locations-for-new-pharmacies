### Инфраструктурный анализ

# Подключение библиотек
library(osmdata)
library(sf)
library(leaflet)
library(purrr)
library(dplyr)
library(geosphere)
library(ggplot2)
library(tidyr)
library(readr)
library(colorspace)
library(readxl)
library(jsonlite)
library(tm)
library(magrittr)
library(textclean)
library(udpipe)
library(syuzhet)
library(textdata)
library(tidytext)
library(sentimentr)
library(wordcloud)

# Получение bbox для Чебоксар
bbox <- getbb("Чебоксары")

# Списки ключей и значений
keys <- c("amenity", "landuse", "highway", "amenity", "amenity", "shop",
          "amenity", "healthcare", "shop", "shop", "pharmacy", "highway",
          "amenity", "amenity", "amenity", "amenity", "amenity", "amenity",
          "tourism", "leisure", "shop", "railway", "leisure", "building",
          "amenity", "construction", "highway", "place", "place")

values <- c("pharmacy", "residential", "residential", "hospital", "clinic",
            "supermarket", "marketplace", "orthotics", "orthotics", "medical_supply",
            "medical_supply", "bus_stop", "parking", "school", "kindergarten",
            "university", "college", "library", "museum", "stadium", "mall",
            "station", "park", "construction", "construction", "residential",
            "primary", "neighbourhood", "quarter")

# Пустой список
results <- list()

# Цикл запроса
for (i in 1:length(keys)) {
  key <- keys[i]
  value <- values[i]
  cat("Запрашиваю:", key, "=", value, "\n")
  
  tryCatch({
    data <- bbox %>% 
      opq() %>% 
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf()

    results[[paste0(key, "_", value)]] <- data
    
    if (!is.null(data$osm_points)) {
      cat("Найдено точек:", nrow(data$osm_points), "\n")
    }
    if (!is.null(data$osm_polygons)) {
      cat("Найдено полигонов:", nrow(data$osm_polygons), "\n")
    }
  }, error = function(e) {
    cat("Ошибка при запросе", key, "=", value, ":", e$message, "\n")
  })
}


# Создание таблицы с необходимыми переменными
final_table <- map_dfr(names(results), function(x) {
  points_df <- NULL
  polygons_df <- NULL
  lines_df <- NULL
  multipolygons_df <- NULL
  
  if (!is.null(results[[x]]$osm_points)) {
    points_df <- results[[x]]$osm_points %>% 
      as_tibble() %>%
      mutate(
        feature_type = x,
        geometry_type = "point",
        lon = st_coordinates(geometry)[, 1],
        lat = st_coordinates(geometry)[, 2]
      )
  }
  if (!is.null(results[[x]]$osm_polygons)) {
    polygons_df <- results[[x]]$osm_polygons %>% 
      as_tibble() %>%
      mutate(
        feature_type = x,
        geometry_type = "polygon",
        centroid = st_centroid(geometry),
        lon = st_coordinates(centroid)[, 1],
        lat = st_coordinates(centroid)[, 2]
      ) %>%
      select(-centroid)
  }
  if (!is.null(results[[x]]$osm_lines)) {
    lines_df <- results[[x]]$osm_lines %>% 
      as_tibble() %>% 
      mutate(
        feature_type = x,
        geometry_type = "line",
        first_point = st_cast(geometry, "POINT")[1],
        lon = st_coordinates(first_point)[, 1],
        lat = st_coordinates(first_point)[, 2]
      ) %>% 
      select(-first_point)
  }
  if (!is.null(results[[x]]$osm_multipolygons)) {
    multipolygons_df <- results[[x]]$osm_multipolygons %>% 
      as_tibble() %>%
      mutate(
        valid = st_is_valid(geometry)
      ) %>%
      filter(valid) %>%  
      mutate(
        feature_type = x,
        geometry_type = "multipolygon",
        centroid = st_centroid(geometry),
        lon = st_coordinates(centroid)[, 1],
        lat = st_coordinates(centroid)[, 2]
      ) %>% 
      select(-centroid, -valid)
  }
  bind_rows(points_df, polygons_df, lines_df, multipolygons_df) 
}) %>% select(name, feature_type, lon, lat, geometry,
              any_of(c("opening_hours",
                       "contact:phone",
                       "contact:facebook",
                       "contact:instagram",
                       "contact:vk",
                       "contact:website")))

# Просмотр результата
glimpse(final_table)

# Разделение на типы
final_table <- final_table %>%
  mutate(
    category = case_when(
      feature_type %in% c("amenity_clinic", "amenity_hospital", 
                          "amenity_pharmacy", "shop_medical_supply") ~ "medical",
      feature_type %in% c("amenity_college", "amenity_kindergarten", 
                          "amenity_school", "amenity_university",
                          "amenity_library") ~ "education",
      feature_type %in% c("amenity_parking", "highway_bus_stop", 
                          "highway_primary", "highway_residential",
                          "railway_station") ~ "transport",
      feature_type %in% c("amenity_marketplace", "shop_mall", 
                          "shop_supermarket") ~ "commercial",
      feature_type %in% c("leisure_park", "leisure_stadium", 
                          "tourism_museum") ~ "social",
      feature_type %in% c("landuse_residential", "construction_residential", 
                          "place_neighbourhood", "place_quarter") ~ "residential",
      TRUE ~ "Buildings under construction"
    ) %>% factor()
  ) 

# Визуализация количественного распределения обьектов инфраструктуры
final_table %>%
  group_by(feature_type) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(feature_type, -count), y = count, fill = feature_type)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +  
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  labs(title = "Количество объектов инфраструктуры",
       x = "Тип объекта",
       y = "Количество",
       fill = "Тип объекта") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")


# Интерактивная карта обьектов инфраструктуры
category_pal <- colorFactor(
  palette = "Set1", 
  domain = final_table$category
)

leaflet(final_table) %>% 
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = ~category_pal(category),
    fillOpacity = 0.8,
    stroke = FALSE,
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      ifelse(!is.na(name), paste0("<br>Название: ", name, "</b><br>"), ""),
      ifelse(!is.na(feature_type), paste0("Тип: ", feature_type, "</b><br>"), ""),
      ifelse(!is.na(category), paste0("Категория: ", category, "</b><br>"), ""),
      ifelse(!is.na(opening_hours), paste0("<br>Время работы: ", opening_hours), "")
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = category_pal,
    values = ~category,
    title = "Категории инфраструктуры",
    opacity = 1
  )

# Сохранение
write_csv(final_table, "data/infrastructure_cheb.csv")







### Демографический анализ (возрастные группы)

# Загрузка данных
rosstat_cheb_people <- read_xls("data/info.people.cheb.rosstat.xls")

# Просмотр 
str(rosstat_cheb_people)

# Преобразование
rosstat_cheb_people_clean <- rosstat_cheb_people %>%
  slice(7:11) %>%
  rename(
    Urban_districts = 1,
    entire_population = 2,
    Male_Female_0_15_years = 3,
    Male_16_62_Female_16_57_years = 4,
    Male_63_and_older_Female_58_and_older = 5,
    perc_Male_Female_0_15 = 6,
    perc_Male_16_62_Female_16_57 = 7,
    perc_Male_63_Female_58_and_older = 8
  ) %>% 
  mutate(
    across(-1, ~ as.numeric(.x, na.rm = TRUE)),  
    Urban_districts = gsub("\n", " ", Urban_districts)
  ) %>%
  filter(!is.na(Urban_districts))

# Визуальная статистика
rosstat_cheb_people_clean %>% 
  select(1, 2, 3, 4, 5) %>% 
  rename(
    "Городской_округ" = 1,
    "Все_население" = 2,
    "0_15_лет" = 3,
    "16_62_муж_16_57_жен" = 4,
    "63+_муж_58+_жен" = 5) %>%
  pivot_longer(
    cols = -Городской_округ,  
    names_to = "Категория",
    values_to = "Численность") %>% 
  ggplot(aes(x = reorder(Городской_округ, -Численность), y = Численность, fill = Категория)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = Численность), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::label_number(
                     scale_cut = scales::cut_short_scale(),  
                     accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Численность населения по возрастным группам",
       x = "Городские округа",
       y = "Численность населения",
       fill = "Возрастные группы") +
  theme_classic() +
  facet_wrap(~ Категория, ncol = 1, scales = "free_y") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")







### Конкурентный анализ

# Визуализация количественного распределения аптек 
final_table %>% 
  filter(feature_type == "amenity_pharmacy") %>%
  count(name) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x = reorder(name, -n), y = n, fill = name)) +  
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +  
  labs(title = "Количество аптек по названиям",
       x = "Название аптеки",
       y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 

# Разделение данных по категориям
filtered_data <- final_table %>% 
  split(.$feature_type)

# Функция для вычисления минимального расстояния
get_min_dist <- function(point_lon, point_lat, target_df) {
  if (nrow(target_df) == 0) return(NA)  
  min(round(distHaversine(
    c(point_lon, point_lat),
    target_df %>% select(lon, lat) %>% as.matrix()
  ) / 1000, 3)
  )
}

# Применение функции (вычисление расcтояния до каждой аптеки)
final_dist_pharmacy <- filtered_data$amenity_pharmacy %>%
  mutate(
    dist_clinic = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_clinic)),
    dist_kindergarten = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_kindergarten)),
    dist_parking = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_parking)),
    dist_university = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_university)),
    dist_bus_stop = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$highway_bus_stop)),
    dist_landuse_residentia = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$landuse_residentia)),
    dist_place_neighbourhood = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$place_neighbourhood)),
    dist_shop_mall = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$shop_mall)),
    dist_museum = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$tourism_museum)),
    dist_college = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_college)),
    dist_library = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_library)),
    dist_building_construction = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$building_construction)),
    dist_highway_primary = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$highway_primary)),
    dist_park = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$leisure_park)),
    dist_place_quarter = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$place_quarter)),
    dist_shop_medical_supply = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$shop_medical_supply)),
    dist_hospital = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_hospital)),
    dist_marketplace = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_marketplace)),
    dist_school = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$amenity_school)),
    dist_highway_residential = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$highway_residential)),
    dist_stadium = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$leisure_stadium)),
    dist_station = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$railway_station)),
    dist_supermarket = map2_dbl(lon, lat, ~ get_min_dist(.x, .y, filtered_data$shop_supermarket))
  )

# Проверка на пропущенные значения
colSums(is.na(final_dist_pharmacy))

# Визуализация медианного расстояния до каждой аптеки
final_dist_pharmacy %>%
  select(starts_with("dist")) %>% 
  summarise(across(everything(), median, na.rm = TRUE)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "object_type",
    values_to = "median_distance") %>%
  mutate(object_type = gsub("dist_", "", object_type), 
         object_type = factor(object_type, levels = object_type[order(-median_distance)])) %>%
  ggplot(aes(x = object_type, y = median_distance, fill = object_type)) +
  geom_col() +
  geom_text(aes(label = paste(round(median_distance, 2), "км")), vjust = -0.5, size = 3.5) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  labs(title = "Медианные расстояния от аптек до объектов инфраструктуры",
       x = "Тип объекта",
       y = "Медиана расстояния") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Формирование данных по микрорайонам и создание буфера (500 м)
neighbourhoods <- final_table %>%
  filter(feature_type %in% c("place_neighbourhood", "place_quarter")) %>%
  select(neighbourhood_name = name, name, lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(32637) %>% 
  st_buffer(500) %>% 
  st_transform(4326) %>%
  distinct(neighbourhood_name, .keep_all = TRUE)

# Соединение данных по микрорайонам с аптеками по пространственной близости
pharmacies_by_area <- final_table %>%
  filter(feature_type == "amenity_pharmacy") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(neighbourhoods, join = st_within) %>%  
  group_by(neighbourhood_name, name) %>%  
  summarise(
    pharmacy_count = n(),
    mean_lon = mean(st_coordinates(geometry)[,1]),
    mean_lat = mean(st_coordinates(geometry)[,2]),
    .groups = "drop"
  ) %>% 
  mutate(
    neighbourhood_name = ifelse(is.na(neighbourhood_name),
                                "Не определенный район",
                                neighbourhood_name)
  ) %>% 
  arrange(desc(pharmacy_count)) 
  
# Просмотр результата и проверка на NA
head(pharmacies_by_area, 10)
colSums(is.na(pharmacies_by_area))

# Построение карты
leaflet() %>%
  addTiles() %>%
  addPolygons(data = neighbourhoods, popup = ~neighbourhood_name) %>%
  addCircleMarkers(data = filter(final_table, feature_type == "amenity_pharmacy"),
                   radius = 3, color = "red")



# Расчет общего количества аптек в городе, работает ли круглосуточно, есть ли страницы в социальных сетях, телефон
stat_pharmacy <- final_table %>%
  filter(feature_type == "amenity_pharmacy") %>% 
  mutate(
    Аптечная_сеть = coalesce(name, "Неизвестная аптека"),  
    Круглосуточно = case_when(opening_hours == "24/7" ~ "Да", TRUE ~ "Нет"),
    VK = ifelse(!is.na(`contact:vk`), "Да", "Нет"),
    Instagram = ifelse(!is.na(`contact:instagram`), "Да", "Нет"),
    Website = ifelse(!is.na(`contact:website`), "Да", "Нет"),
    Facebook = ifelse(!is.na(`contact:facebook`), "Да", "Нет"),
    Телефон = ifelse(!is.na(`contact:phone`), "Да", "Нет")
  ) %>%
  group_by(Аптечная_сеть) %>%
  summarise(
    Количество_точек = n(),
    Круглосуточно = ifelse(any(Круглосуточно == "Да"), "Да", "Нет"),
    VK = ifelse(any(VK == "Да"), "Да", "Нет"),
    Instagram = ifelse(any(Instagram == "Да"), "Да", "Нет"),
    Website = ifelse(any(Website == "Да"), "Да", "Нет"),
    Facebook = ifelse(any(Facebook == "Да"), "Да", "Нет"),
    Телефон = ifelse(any(Телефон == "Да"), "Да", "Нет"),
    .groups = "drop"
  ) %>%
  arrange(desc(Количество_точек)) %>%
  mutate(Количество_точек = as.integer(Количество_точек)) 

# Просмотр 
head(stat_pharmacy, 11)





# Анализ конкурента (Вита Экспресс)

# Интерактивная карта конкурента (Вита Экспресс)
pharmacies_by_area %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = neighbourhoods, popup = ~neighbourhood_name) %>%
  addCircleMarkers(data = filter(final_table, name == "Вита Экспресс"),
                   radius = 3,
                   color = "red",
                   clusterOptions = markerClusterOptions())


# График расстояний от каждого обьекта инфроструктуры (Вита экспресс)
final_dist_pharmacy %>%
  filter(name == "Вита Экспресс") %>% 
  select(starts_with("dist")) %>% 
  summarise(across(everything(), median, na.rm = TRUE)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "object_type",
    values_to = "median_distance") %>%
  mutate(object_type = gsub("dist_", "", object_type), 
         object_type = factor(object_type, levels = object_type[order(-median_distance)])) %>%
  ggplot(aes(x = object_type, y = median_distance, fill = object_type)) +
  geom_col() +
  geom_text(aes(label = paste(round(median_distance, 2), "км")), vjust = -0.5, size = 3.5) +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  labs(title = "Медианные расстояния от аптеки до объектов инфраструктуры",
       x = "Тип объекта",
       y = "Медиана расстояния") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))






### Изучение отзывов аптеки


#### Предварительная обработка

# Загрузка
reviews_vita <- read_json("data/all_reviews.json", simplifyVector = TRUE)

# Cоздание корпуса слов
corpus_reviews <- Corpus(VectorSource(reviews_vita$text)) %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(function(x) gsub("[[:punct:]]", " ", x))) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("russian")) %>% 
  tm_map(removeWords, c("аптека", "читать", "ещё", "еще", "фармация", "вита")) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(function(x) {
    x %>% 
      gsub("\\b(дий|анный|онлайть)\\b", "", .) %>% 
      gsub("скрыть ответ магазин", "", .)           
  }))

# Лемматизация 
model <- udpipe_load_model(udpipe_download_model("russian")$file_model)
lemmatized_text <- udpipe_annotate(model, x = sapply(corpus_reviews, as.character)) %>% 
  as.data.frame() %>% 
  group_by(doc_id) %>% 
  summarise(text = paste(lemma, collapse = " ")) %>% 
  pull(text)

# Обновление корпуса слов
corpus_lemmatized <- Corpus(VectorSource(lemmatized_text))





#### Анализ тональности

# Конвертация в датафрейм
reviews_data <- data.frame(
  doc_id = names(corpus_lemmatized),
  text = sapply(corpus_lemmatized, as.character),
  stringsAsFactors = FALSE
)

# Словарь тональности 
custom_polarity_ru <- sentimentr::as_key(data.frame(
  words = c(
    "удобный", "вежливый", "прекрасный", "выдающийся", "дружелюбный",
    "отличный", "профессиональный", "внимательный", "быстрый", "качественный",
    "чистый", "комфортный", "приятный", "любезный", "гостеприимный", "доступные цены",
    "ужасный", "грубый", "медленный", "грязный", "неудобный",
    "плохой", "отвратительный", "непрофессиональный", "невнимательный", "хамский",
    "разочарование", "кошмарный", "неприятный", "некачественный", "некомпетентный",
    "дорого", "дефицит", "спасибо", "лучший", "доброжелательный", "прекрасно", "душевный", 
    "жаль", "потеря", "хороший", "обманывать", "неправду", "выгодный", "возможность", "приветливый",
    "плохо", "своевременно", "нравиться", "неплохой", "быстро", "доброжелательна", "радовать",
    "акция"
  ),
  polarity = c(
    0.8, 0.7, 0.9, 1.0, 0.7,
    0.9, 0.8, 0.7, 0.6, 0.8,
    0.7, 0.7, 0.6, 0.7, 0.7, 0.7,
    -1.0, -0.9, -0.6, -0.8, -0.7,
    -0.8, -1.0, -0.9, -0.7, -1.0,
    -0.8, -1.0, -0.7, -0.8, -0.9,
    -0.8, -0.6, 0.5, 0.5, 0.5, 0.5,
    0.5, 0.2, 0.2, 0.5, -0.5, -0.1,
    0.5, 0.5, 0.6, -0.2, 0.5, 0.6, 0.5,
    0.5, 0.6, 0.6, 0.6 
  ),
  stringsAsFactors = FALSE
))

reviews_data$sentiment <- sentiment_by(
  get_sentences(reviews_data$text),
  polarity_dt = custom_polarity_ru)$ave_sentiment

# Визуализация
ggplot(data = reviews_data, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.05,              
                 fill = "steelblue",        
                 color = "white",            
                 alpha = 0.8 )

reviews_data %>%
  filter(sentiment == 0)
  



#### Анализ ключевых слов

# Частотный анализ
word_counts <- reviews_data %>% 
  unnest_tokens(word, text) %>% 
  anti_join(data.frame(word = stopwords("russian")), by = "word") %>% 
  count(word, sort = TRUE)

# Облако слов
wordcloud(words = word_counts$word, freq = word_counts$n, max.words = 100)

# Визуализация: топ-20 слов
top_words <- word_counts %>% 
  slice_max(n, n = 20)

ggplot(top_words, aes(x = n, y = reorder(word, n))) +
  geom_col(fill = "steelblue") +
  labs(title = "20 самых частых слов в отзывах",
       x = "Количество упоминаний",
       y = "Слово") +
  theme_minimal()






# Поиск потенциальных мест 

# Координаты всех объектов
amenity_pharmacy_coords <- final_table %>% 
  filter(feature_type == "amenity_pharmacy") %>% 
  select(lon, lat) %>% 
  as.matrix()

parking_coords <- final_table %>% 
  filter(feature_type == "amenity_parking") %>% 
  select(lon, lat) %>% 
  as.matrix()

bus_stop_coords <- final_table %>% 
  filter(feature_type == "highway_bus_stop") %>% 
  select(lon, lat) %>% 
  as.matrix()

# Основной расчет 
result_residential <- final_table %>%
  select(name, feature_type, lon, lat) %>% 
  filter(
    feature_type == "landuse_residential",
    !name %in% c("Карачуры", "Малые Карачуры", "Заовражная", "Сятракасы", "Светлая улица",
                 "Большое Князь-Теняково", "Чиршкасы", "Мошкаси", "Яндово", "Чемурша",
                 "Улица Дубравная", "Мясокомбинат", "Ягудары", "Заовражный водовод",
                 'Станция "Чебоксары-2"', "Молодёжная", "Кладбище", "Алатырское шоссе",
                 'Коллективный сад "Заовражный"', "Сады", "Аэропорт", "ЖБК-9"),
    !is.na(lon), !is.na(lat), !is.na(name)
  ) %>%
  rowwise() %>%
  mutate(
    dist_to_pharmacy = if (nrow(amenity_pharmacy_coords) > 0) {
      round(min(distHaversine(c(lon, lat), amenity_pharmacy_coords))/1000, 2)
    } else NA_real_,
    
    dist_to_parking = if (nrow(parking_coords) > 0) {
      round(min(distHaversine(c(lon, lat), parking_coords))/1000, 2)
    } else NA_real_,
    
    dist_to_bus = if (nrow(bus_stop_coords) > 0) {
      round(min(distHaversine(c(lon, lat), bus_stop_coords))/1000, 2)
    } else NA_real_
  ) %>%
  ungroup() %>%
    filter(dist_to_pharmacy >= 0.6,
           dist_to_bus <= 0.5,
           dist_to_parking <= 0.5)

# Визуализация
leaflet(result_residential) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat,
    color = "blue",
    radius = 6,
    popup = ~paste(
      "<b>Жилой район:</b>", name, "<br>",
      "<b>До аптеки:</b>", ifelse(is.na(dist_to_pharmacy), "нет данных", paste(dist_to_pharmacy, "км")), "<br>",
      "<b>До парковки:</b>", ifelse(is.na(dist_to_parking), "нет данных", paste(dist_to_parking, "км")), "<br>",
      "<b>До остановки:</b>", ifelse(is.na(dist_to_bus), "нет данных", paste(dist_to_bus, "км"))
    )
  )

# Сохранение
write_csv(result_residential, "data/potencial_point_pharmacy.csv")


# Арендная ставка 

                    



# Юридические ограничения