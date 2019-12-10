LIBOR3Y <- read.csv("datasets/csv/DSWP3.csv") %>%
  as_tibble() %>%
  filter(DSWP3 != ".") %>%
  mutate(DATE = as_date(DATE),
         RATE3Y = as.numeric(as.character(DSWP3)) / 100) %>%
  select(DATE, RATE3Y)

ICE3Y <- read.csv("datasets/csv/ICERATES1100USD3Y.csv") %>%
  as_tibble() %>%
  filter(ICERATES1100USD3Y != ".") %>%
  mutate(DATE = as_date(DATE),
         RATE3Y = as.numeric(as.character(ICERATES1100USD3Y)) / 100) %>%
  select(DATE, RATE3Y)


LIBOR5Y <- read.csv("datasets/csv/DSWP5.csv") %>%
  as_tibble() %>%
  filter(DSWP5 != ".") %>%
  mutate(DATE = as_date(DATE),
         RATE5Y = as.numeric(as.character(DSWP5)) / 100) %>%
  select(DATE, RATE5Y)

ICE5Y <- read.csv("datasets/csv/ICERATES1100USD5Y.csv") %>%
  as_tibble() %>%
  filter(ICERATES1100USD5Y != ".") %>%
  mutate(DATE = as_date(DATE),
         RATE5Y = as.numeric(as.character(ICERATES1100USD5Y)) / 100) %>%
  select(DATE, RATE5Y)

RATES3Y <- LIBOR3Y %>% rbind(ICE3Y) %>%
  arrange(DATE) %>% distinct(DATE, .keep_all = TRUE)

RATES5Y <- LIBOR5Y %>% rbind(ICE5Y) %>%
  arrange(DATE) %>% distinct(DATE, .keep_all = TRUE)

saveRDS(RATES3Y, "datasets/rates3Y.rds")
saveRDS(RATES5Y, "datasets/rates5Y.rds")


# Note there are 7212 days from 1 Jan 2000 to 30 Sep 2019
#
# (ymd("2000-01-01") %--% ymd("2019-09-30")) %/% days(1)
RATES <- tibble(n = seq(0, 7212)) %>%

  # Create a column with all dates
  mutate(DATE = ymd("2000-01-01") + days(n)) %>%
  select(-n) %>%

  # Add all daily 3- then 5-year rates and fill missing down
  left_join(RATES3Y) %>%
  fill(RATE3Y, .direction = "down") %>%

  left_join(RATES5Y) %>%
  fill(RATE5Y, .direction = "down")

saveRDS(RATES, "datasets/rates.rds")
