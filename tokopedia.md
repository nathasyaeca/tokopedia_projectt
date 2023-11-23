Tokopedia
================
Nathasya Pramudita
2023-11-20

``` r
library(tidyverse) # Cleaning, manipulating, analyzing, and vizualizing data packages
library(extrafont) # for changing the font in viz
```

# Tokopedia Study Case

## Summary of the dataset

- There’s 11 variables and 5400 observant, with 33 unique data in
  `category` variables (meaning this data only focus on 33 product
  only),
- They didn’t have range of time on when the data is obtain.
- Location in this data is focus on Java island (with exception in
  Denpasar, Palembang, and Medan).
- Overall rating that customer gives are range from `4.1` to `5.0` (they
  never give the supplier \<4).
- The `emotion` ordinal data type is consist of `Anger`, `Fear`,
  `Happy`, `Love`, and `Sadness`.

*disclaimer: the data that I used is obtain from (mendeley dataset open
source) \[<https://data.mendeley.com/datasets/574v66hf2v/1>\]. This
dataset is collection of Indonesian product review data annotated with
emotion and sentiment labels. The data were collected from one of the
giant e-commerce in Indonesia named Tokopedia. The dataset contains
product reviews from 29 product categories on Tokopedia that use the
Indonesian language. Each product review is annotated with a single
emotion, i.e., love, happiness, anger, fear, or sadness. The group of
annotators does the annotation process to provide emotion labels by
following the emotions annotation criteria created by an expert in
clinical psychology. Other attributes related to the product review are
also extracted, such as Location, Price, Overall Rating, Number Sold,
Total Review, and Customer Rating*

``` r
tokopedia <- read_csv("~/Datasets/Tokopedia dataset/PRDECT-ID Dataset.csv") %>% 
  rename_all(tolower) %>% 
  mutate(`overall rating` = as.factor(`overall rating`),
         `customer rating` = as.factor(`customer rating`),
         sentiment = as.factor(sentiment),
         emotion = as.factor(emotion))
tokopedia %>% head(5)
```

    ## # A tibble: 5 × 11
    ##   category         `product name` location  price `overall rating` `number sold`
    ##   <chr>            <chr>          <chr>     <dbl> <fct>                    <dbl>
    ## 1 Computers and L… Wireless Keyb… Jakarta…  53500 4.9                       5449
    ## 2 Computers and L… PAKET LISENSI… Kota Ta…  72000 4.9                       2359
    ## 3 Computers and L… SSD Midasforc… Jakarta… 213000 5                        12300
    ## 4 Computers and L… ADAPTOR CHARG… Jakarta…  55000 4.7                       2030
    ## 5 Computers and L… ADAPTOR CHARG… Jakarta…  55000 4.7                       2030
    ## # ℹ 5 more variables: `total review` <dbl>, `customer rating` <fct>,
    ## #   `customer review` <chr>, sentiment <fct>, emotion <fct>

``` r
# created diff df for analysis process
# customer rating
customer_rating <- tokopedia %>%
  group_by(category, `customer rating`) %>% 
  summarize(`total price` = sum(price),
         `total review` = sum(`total review`)) %>%
  select(category, `customer rating`, `total price`, `total review`) %>% 
  arrange(category, `customer rating`)
# emotion rating
emotion_rating <- tokopedia %>% 
  group_by(category, emotion) %>%
  summarize(`total price` = sum(price),
         `total review` = sum(`total review`),
         `total sold` = sum(`number sold`)) %>% 
  select(category, emotion, `total price`, `total review`, `total sold`) %>% 
  arrange(category, emotion, `total price`)
# total sold of product based on categories
most_purchase <- tokopedia %>% 
  group_by(category) %>% 
  summarize(`total price` = sum(price),
            `total sold` = sum(`number sold`)) %>% 
  select(category, `total price`, `total sold`) %>% 
  arrange(desc(`total price`))
```

``` r
# create Top-7 best purchase product in Tokopedia
most_purchase %>%
  mutate(price_in_million = round(`total price`/1000000),
         category = fct_reorder(category, price_in_million)) %>% 
  head(7) %>%
  ggplot(aes(category, price_in_million, fill = category))+
  geom_col(alpha = .7, width = 0.5, show.legend = FALSE) +
  coord_flip()+
  labs(title = "Most Purchasing Product on Tokopedia",
       x = "",
       y = "Price in Million Rupiah")+
  scale_fill_brewer(palette = "Set2")+
  theme_light() +
  theme(text = element_text(family = "Segoe UI Light"))
```

![](tokopedia_files/figure-gfm/create%20Top-7%20best%20purchase%20product%20in%20Tokopedia-1.png)<!-- -->

``` r
# created graph based on emotion rating
emotion_rating %>% 
  mutate(price_in_million = `total price`/1000000,
         emotion = fct_reorder(emotion, price_in_million)) %>% 
  filter(category %in% c("Phones and Tablets", "Tour and Travel",
                         "Precious Metal", "Electronics")) %>% 
  ggplot(aes(emotion, price_in_million, fill = emotion))+
    geom_col(alpha = .7, width = .5, show.legend = F) +
    coord_flip() +
    facet_wrap(~category) +
    labs(title = "Top-4 Most Purchase Product Based on Emotion Ranking",
         x = "",
         y = "Price in Million Rupiah") +
    scale_fill_manual(breaks = c("Sadness", "Love", "Happy", "Fear", "Anger"),
                     values = c("#F7D060", "#FD8A8A", "#BAFFB4", "#F39472", "#FF4848"))+
  theme_classic() +
  theme(text = element_text(family = "Segoe UI Light"))
```

![](tokopedia_files/figure-gfm/created%20graph%20based%20on%20emotion%20rating-1.png)<!-- -->

``` r
emotion_rating %>% 
  filter(emotion %in% c("Fear", "Sadness", "Anger"),
         category %in% c("Animal Care", "Automotive", "Beauty", "Body Care")) %>% 
  group_by(category, emotion) %>% 
  summarize(`total review` = sum(`total review`)) %>% 
  mutate(emotion = fct_reorder(emotion, `total review`),
         `total review` = `total review`/1000) %>% 
  arrange(`total review`) %>% 
  ggplot(aes(emotion, `total review`, fill = emotion)) +
    geom_col(alpha = .7, width = .5, show.legend = F) +
    coord_flip() +
    facet_wrap(~category) +
    labs(title = "Top-4 Product With the Higher Negative Emotion Review",
         x = "",
         y = "Total Review in Thousand") +
    scale_fill_manual(breaks = c("Sadness", "Fear", "Anger"),
                      values = c("#f7d060", "#F39472", "#ff4848")) +
    theme_classic() +
    theme(text = element_text(family = "Segoe UI Light"))
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

![](tokopedia_files/figure-gfm/product%20with%20the%20higher%20negative%20emotion%20rating-1.png)<!-- -->

``` r
# created graph based on customer rating star
customer_rating %>% 
  mutate(price_in_million = `total price`/1000000) %>% 
  filter(category %in% c("Phones and Tablets", "Tour and Travel",
                         "Precious Metal", "Electronics")) %>% 
  ggplot(aes(`customer rating`, price_in_million, fill = `customer rating`))+
  geom_col(alpha = .7, linewidth = .5, show.legend = F) +
  facet_wrap(~category) +
  labs(title = "Top-4 Most Purchase Product Based on Customer Rating",
       x = "",
       y = "Price in Million Rupiah") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme_classic() +
  theme(text = element_text(family = "Segoe UI Light"))
```

![](tokopedia_files/figure-gfm/created%20graph%20based%20on%20customer%20rating%20star-1.png)<!-- -->

``` r
tokopedia %>% 
  group_by(emotion, sentiment) %>% 
  summarize(sum = sum(`number sold`)) %>% 
  mutate(emotion = fct_reorder(emotion, sum),
         sum = round(sum/1000)) %>% 
  ggplot(aes(emotion, sum, fill = sentiment)) +
  geom_col(linewidth = 1.5) +
  labs(x = "",
       y = "Total Product Sold",
       title = "Total Sold Product Based on Emotion and Sentiment Rating"
       ) +
  geom_text(aes(label = sum), vjust = 1) +
  theme_classic() + 
  theme(text = element_text(family = "Segoe UI Light"),
        legend.position = "bottom") +
  scale_fill_manual (values = c("#af2655", "#b4bdbf"))
```

![](tokopedia_files/figure-gfm/corelation%20between%20negative%20and%20positive%20emotion-1.png)<!-- -->

``` r
# overall rating of all product in Tokopedia ds
tokopedia %>% 
  mutate(`overall rating` = as.numeric(`overall rating`)) %>% 
  ggplot(aes(`overall rating`)) +
  geom_histogram(binwidth = 4, fill = "#ead123") +
  labs(title = "Customers rating of all product in Tokopedia") +
  theme_classic() +
  theme(text = element_text(family = "Segoe UI Light"))
```

![](tokopedia_files/figure-gfm/overall%20rating%20of%20all%20product-1.png)<!-- -->

------------------------------------------------------------------------

## Finding

From the analysis above, what we find is:

- Product that worth the most are `Phone and tablet`, `Tour and travel`,
  `Precious metal`, `Electronic` and `Gaming`.
- The best product that get positive emotion are `Phone and tablet`,
  while `Precious metal` and `Tour and travel` are side by side, while
  `Electronic` is in the last position. We need to analyze more deeply
  why this phenomena occur.
- While product that get the higher negative rating is `Body Care` and
  `Animal Care` (negative emotion consist of `Sadness`, `Anger`, and
  `Fear`).
- `Precious metal` and `Tour and Travel` usually get 5 stars rating.
- `Sadness` are the most frequent emotion that customers choose after
  receive the product.
- And frequent rating that customers pick for overall product are
  (higher percentage) 7.5 to 10
