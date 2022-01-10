<h1 align="center"> Desafio Seazone | Gabriel Probst Thaumaturgo | 10/01/2022 </h1>

## Introdução - Instalação e abertura do pacote tidyverse, para execução das funções necessárias.

install.packages ("tidyverse")

library ("tidyverse")

## Leitura dos arquivos csv com criação de objetos respectivos.

desafio_priceav <- read_csv ("desafio_priceav.csv")

desafio_details <- read_csv ("desafio_details_fix.csv")

## 1 - Ordenação os bairros em ordem crescente do número de listings.

### 1.1 - Agrupamento do banco de dados 'details' de acordo com os bairros categorizados na variável 'suburb'.
desafio_details_gb_suburb <- group_by (desafio_details, suburb)

### 1.2 - Tabelamento do número de listings por bairro, em ordem crescente.
summarise_suburb_num <- desafio_details_gb_suburb %>% summarise (num_obs=n()) %>% arrange (num_obs)

## 2 - Ordenação os bairros em ordem crescente do faturamento médio dos listings.

### 2.1 - Agrupamento do banco de dados 'priceav' de acordo com os listings categorizados na variável 'airbnb_listing_id'.
desafio_priceav_gb_id <- group_by (desafio_priceav, airbnb_listing_id)

### 2.2 - Tabelamento do faturamento médio por listing, excluindo-se os missing values.
summarise_id <- desafio_priceav_gb_id %>% summarise (fat_med = mean (price_string, na.rm = TRUE))

### 2.3 - Junção (left join) dos dados da variável calculada anteriormente ao banco de dados 'details'. 
desafio_merge <- left_join (desafio_details, summarise_id, by = "airbnb_listing_id")

### 2.4 - Agrupamento do objeto 'desafio_merge' de acordo com os bairros categorizados na variável 'suburb'.
desafio_merge_gb_id <- group_by (desafio_merge, suburb)

### 2.5 - Tabelamento da soma de faturamentos médios diários por bairro em ordem crescente, excluindo-se missing values.
summarise_suburb_fat <- desafio_merge_gb_id %>% summarise (sum_fat_med = sum (fat_med, na.rm = TRUE)) %>% arrange (sum_fat_med)

### 2.5.1 - Tabelamento da média de faturamentos médios diários por bairro em ordem crescente, excluindo-se missing values.
summarise_suburb_fat_med <- desafio_merge_gb_id %>% summarise (med_fat_med = mean (fat_med, na.rm = TRUE)) %>% arrange (med_fat_med)

## 3 - Avaliação das correlações entre variáveis

### 3.1 - Geração de um objeto para referência das análises, trazendo os faturamentos e características de cada listing.
summarise_id_sum <- desafio_priceav_gb_id %>% summarise (fat_tot = sum (price_string, na.rm = TRUE))

desafio_merge_sum <- left_join (desafio_details, summarise_id_sum, by = "airbnb_listing_id")

### 3.2 - Correlação entre faturamentos dos listings e número de quartos
cor.test (desafio_merge_sum$number_of_bedrooms, desafio_merge_sum$fat_tot)

desafio_merge_sum %>% \
  ggplot() + \
  geom_point(aes(x = number_of_bedrooms, y = fat_tot), \
             color = "dodgerblue4", \
             size = 2) + \
  geom_smooth(aes(x = number_of_bedrooms, y = fat_tot), \
              color = "darkgoldenrod3", \
              method = "lm", \
              formula = y ~ x, \
              se = FALSE, \
              size = 1.2) + \
  labs(x = "Número de Quartos", \
       y = "Faturamento Total") + \
  theme_bw()

### 3.3 - Correlação entre faturamentos dos listings e número de banheiros
cor.test (desafio_merge_sum$number_of_bathrooms, desafio_merge_sum$fat_tot)

desafio_merge_sum %>% \
  ggplot() + \
  geom_point(aes(x = number_of_bathrooms, y = fat_tot), \
             color = "dodgerblue4", \
             size = 2) + \
  geom_smooth(aes(x = number_of_bathrooms, y = fat_tot), \
              color = "darkgoldenrod3", \
              method = "lm", \
              formula = y ~ x, \
              se = FALSE, \
              size = 1.2) + \
  labs(x = "Número de Banheiros", \
       y = "Faturamento Total") + \
  theme_bw()

### 3.4 - Correlação entre faturamentos dos listings e número de avaliações
cor.test (desafio_merge_sum$number_of_reviews, desafio_merge_sum$fat_tot)

desafio_merge_sum %>% \
  ggplot() + \
  geom_point(aes(x = number_of_reviews, y = fat_tot), \
             color = "dodgerblue4", \
             size = 2) + \
  geom_smooth(aes(x = number_of_reviews, y = fat_tot), \
              color = "darkgoldenrod3", \
              method = "lm", \
              formula = y ~ x, \
              se = FALSE, \
              size = 1.2) + \
  labs(x = "Número de Avaliações", \
       y = "Faturamento Total") + \
  theme_bw()

### 3.5 - Correlação entre faturamentos dos listings e avaliações (estrelas)
cor.test (desafio_merge_sum$star_rating, desafio_merge_sum$fat_tot)

desafio_merge_sum %>% \
  ggplot() + \
  geom_point(aes(x = star_rating, y = fat_tot), \
             color = "dodgerblue4", \
             size = 2) + \
  geom_smooth(aes(x = star_rating, y = fat_tot), \
              color = "darkgoldenrod3", \
              method = "lm", \
              formula = y ~ x, \
              se = FALSE, \
              size = 1.2) + \
  labs(x = "Avaliações (Estrelas)", \
       y = "Faturamento Total") + \
  theme_bw()

## 4 - Cálculo da antecedência média das reservas

### 4.1 - Exclusão de valores classificados como 'blank' no banco de dados 'desafio_priceav', bem como exclusão de 3 linhas de dados em que a reserva se refere ao ano 2000, possivelmente decorrente de um erro na geração dos dados.
desafio_priceav_booked <- desafio_priceav %>% filter (booked_on != "blank" & booked_on > 2020)

### 4.2 - Geração de uma nova coluna referente à diferença entre o dia da reserva e a data programada de locação.
desafio_priceav_booked_dif <- desafio_priceav_booked %>% mutate (dif = difftime (date, booked_on, units = "days")) %>% mutate (dif = replace (dif, dif>-1, dif+0.125))

### 4.3 - Cálculo da média das diferenças calculadas anterioremente, indicando a antecedência média da reserva.
med_antecedencia <- summarise (desafio_priceav_booked_dif, med_anteced=mean(dif))

## 4.a - Comparativo com finais de semana

### 4.a.1 - Geração de uma nova coluna com o dia da semana para a data reservada para cada locação.
desafio_priceav_booked_dif_dias <- desafio_priceav_booked_dif %>% mutate (dia_da_semana = format.Date (as.Date(date), "%a"))

### 4.a.2 - Exclusão das observações referentes a reservas realizadas para dias da semana (seg a sex).
desafio_priceav_booked_dif_dias_fds <- desafio_priceav_booked_dif_dias %>% filter (dia_da_semana == "sáb" | dia_da_semana == "dom")

### 4.a.3 - Média dos valores de antecedência calculados anterioremnte, referentes apenas aos finais de semana.
med_antecedencia_fds <- summarise (desafio_priceav_booked_dif_dias_fds, med_anteced_fds=mean(dif))
