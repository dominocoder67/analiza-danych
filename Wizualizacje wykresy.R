wykres_przebieg_cena_diesel <- dane_imp %>%
  filter(fuel %in% c("Diesel", "Gasoline")) %>%
ggplot(aes(x = mileage, y = price)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Zależność pomiędzy przbeiegiem a ceną",
       x = "Przebieg (w tysiącach, log10)",
       y = "Cena (w USD)") +
  theme_minimal()
wykres_przebieg_cena_diesel

wykres_przebieg_cena_EV <- dane_imp %>%
  filter(fuel %in% c("Hybrid", "Electric")) %>%
  ggplot(aes(x = mileage, y = price)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Zależność pomiędzy przbeiegiem a ceną",
       x = "Przebieg (w tysiącach, log10)",
       y = "Cena (w USD)") +
  theme_minimal()
wykres_przebieg_cena_EV

wykres_rok_cena_diesel <- dane_imp %>%
  filter(fuel %in% c("Diesel", "Gasoline")) %>%
  ggplot(aes(x = year, y = price, color = mark)) +
  geom_point() +
  geom_smooth(method = "loess", color = "light green", se = FALSE) +
  labs(title = "Zależność pomiędzy rokiem produkcji a ceną",
       x = "Rok produkcji",
       y = "Cena (w USD)") +
  theme_minimal()
ggplotly(wykres_rok_cena_diesel)

wykres_rok_cena_EV <- dane_imp %>%
  filter(fuel %in% c("Hybrid", "Electric")) %>%
  ggplot(aes(x = year, y = price, color = mark)) +
  geom_point() +
  geom_smooth(method = "loess", color = "light green", se = FALSE) +
  labs(title = "Zależność pomiędzy rokiem produkcji a ceną",
       x = "Rok produkcji",
       y = "Cena (w USD)") +
  theme_minimal()
ggplotly(wykres_rok_cena)


LMP <- lm(price ~ year + mileage + vol_engine, data = dane_imp)
summary(LMP)
modelsummary(LMP)

tabela_najdrozsze_samochody <- dane_imp %>%
  filter(year >= 2010) %>%
  group_by(mark) %>%
  summarise(srednia_cena = mean(price),
            mediana_cena = median(price),
            najdroższy = max(price),
            najtańszy = min(price),
            liczba_samochodow = n()) %>%
  arrange(desc(srednia_cena))

tabela_najdrozsze_samochody

wykres_srednia_cena_przez_lata <- dane_imp %>% 
  # filter(mark %in% c("mercedes", "bmw", "audi", "volvo", "alfa-romeo")) %>%
  group_by(mark, year) %>%
  summarise(srednia_cena = mean(price)) %>%
  ggplot(aes(x = year, y = srednia_cena, color = mark)) +
  geom_line()
wykres_srednia_cena_przez_lata
ggplotly(wykres_srednia_cena_przez_lata)

wykres_boxplot <- dane_imp %>% 
filter(mark %in% c("mercedes-benz", "bmw", "audi", "volvo", "alfa-romeo")) %>%
ggplot(aes(x = mark, y = price, fill = mark)) +
  geom_boxplot() +
  labs(title = "Rozkład cen wybranych marek samochodów",
       x = "Marka samochodu",
       y = "Cena (w USD)") +
  theme_minimal()
wykres_boxplot
  
  
wykres_rejestracje <- dane_imp %>%
  filter(year >= 1990) %>%
  group_by(mark, year) %>%
  summarise(liczba_aut_rok = n()) %>%
  ggplot(aes(x = year, y = liczba_aut_rok, color = mark)) +
  geom_line()
ggplotly(wykres_rejestracje)

wykres_srednia_cena_fuel <- dane_imp %>% 
  # filter(mark %in% c("mercedes", "bmw", "audi", "volvo", "alfa-romeo")) %>%
  group_by(fuel, year) %>%
  summarise(srednia_cena = mean(price)) %>%
  ggplot(aes(x = year, y = srednia_cena, color = fuel)) +
  geom_line()
wykres_srednia_cena_fuel
ggplotly(wykres_srednia_cena_fuel)

wykres_tabela_najdrozsze_samochody <- tabela_najdrozsze_samochody %>%
  filter(mark %in% c("mercedes", "bmw", "audi", "volvo", "alfa-romeo")) %>%
ggplot(tabela_najdrozsze_samochody, aes(x = mark)) +
                                               geom_boxplot()
wykres_tabela_najdrozsze_samochody

tabela_najdrozsze_samochody <- dane_imp %>%
  filter(year >= 2010) %>%
  group_by(mark) %>%
  summarise("Średnia cena" = mean(price),
            "Mediana ceny" = median(price),
            najdroższy = max(price),
            najtańszy = min(price),
            liczba_samochodow = n()) %>%
  arrange(desc("Średnia cena"))

tabela_najdrozsze_samochody


```{r macierz_korelacji, echo=FALSE, warning=FALSE, message=FALSE}
# wybór zmiennych liczbowych
dane_num <- dane_imp %>%
  select(price, year, mileage, vol_engine)

# obliczenie macierzy korelacji
korelacje <- cor(dane_num, use = "complete.obs")

# wizualizacja pełnej (kwadratowej) macierzy korelacji
corrplot::corrplot(
  korelacje,
  method = "number",
  type = "full",
  diag = TRUE
)

wykres_srednia_cena_fuel <- dane_imp %>% 
  group_by(fuel, year) %>%
  summarise(srednia_cena = mean(price)) %>%
  ggplot(aes(x = year, y = srednia_cena, color = fuel)) +
  geom_line() +
  labs(
    title = "Średnia cena samochodów w zależności od rodzaju paliwa",
    x = "Rok",
    y = "Średnia cena (PLN)",
    color = "Rodzaj paliwa"
  ) +
  theme_minimal()
ggplotly(wykres_srednia_cena_fuel)

ggbetweenstats(
 data = dane_imp,
 x = fuel,
 y = price
)

test_mileage_price <- ggscatterstats(
  data = dane_imp,
  x = mileage,
  y = price,
  title = "Zależność między przebiegiem a ceną samochodu",
  xlab = "Przebieg (w tysiącach km)",
  ylab = "Cena (PLN)"
)

test_mileage_price

test_year_price <- ggscatterstats(
  data = dane_imp,
  x = year,
  y = price,
  title = "Zależność między rokiem produkcji a ceną samochodu",
  xlab = "Rok produkcji",
  ylab = "Cena (PLN)"
)
test_year_price

test_mark_price <- ggbetweenstats(
  data = dane_imp %>% filter(mark %in% c("audi", "bmw", "mercedes-benz", "volvo", "alfa-romeo")),
  x = mark,
  y = price,
  title = "Porównanie cen samochodów wybranych marek",
  xlab = "Marka samochodu",
  ylab = "Cena (PLN)"
)
test_mark_price

test_anova_fuel_mileage_year_price <- aov(price ~ fuel + mileage + year, data = dane_imp)
ggcoefstats(test_anova_fuel_mileage_year_price,
             title = "Wpływ rodzaju paliwa, przebiegu i roku produkcji na cenę samochodu",
             xlab = "Czynniki",
             ylab = "Współczynniki")

rejestracje <- dane_imp %>%
  filter(year >= 1990) %>%
  group_by(mark, year) %>%
  summarise(liczba_aut_rok = n(), .groups = "drop") %>%
  mutate(okres = ifelse(year == 2021, "2021", "Pozostałe lata"))

ggbetweenstats(
  data = rejestracje,
  x = okres,
  y = liczba_aut_rok,
  type = "nonparametric",
  title = "Czy 2021 różni się od pozostałych lat?",
  xlab = "",
  ylab = "Liczba zarejestrowanych samochodów"
)

