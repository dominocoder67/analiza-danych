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
plotly(wykres_rok_cena)

wykres_rok_cena_EV <- dane_imp %>%
  filter(fuel %in% c("Hybrid", "Electric")) %>%
  ggplot(aes(x = year, y = price, color = mark)) +
  geom_point() +
  geom_smooth(method = "loess", color = "light green", se = FALSE) +
  labs(title = "Zależność pomiędzy rokiem produkcji a ceną",
       x = "Rok produkcji",
       y = "Cena (w USD)") +
  theme_minimal()
plotly(wykres_rok_cena)


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
plotly(wykres_srednia_cena_przez_lata)

wykres_rejestracje <- dane_imp %>%
  filter(year >= 1990) %>%
  group_by(mark, year) %>%
  summarise(liczba_aut_rok = n()) %>%
  ggplot(aes(x = year, y = liczba_aut_rok, color = mark)) +
  geom_line()
plotly(wykres_rejestracje)
