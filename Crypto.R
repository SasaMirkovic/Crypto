library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(xlsx)
library(rtweet)
library(tidyverse)
library(httpuv)
library(threejs)
library(readxl)
library(ROAuth)
library(RCurl)
library(syuzhet)
library(rvest)
library(XML)
library(imputeTS)
library(zoo)
library(ggpubr)
library(TTR)
library(plotly)
library(formattable)


install.packages('TTR', dependencies = TRUE, INSTALL_opts = '--no-lock')

install.packages("ggpubr", dependencies = TRUE, INSTALL_opts = '--no-lock')

install.packages("plotly", dependencies = TRUE, INSTALL_opts = '--no-lock')

install.packages("formattable", dependencies = TRUE, INSTALL_opts = '--no-lock')

options(scipen = 999)


### 2. UCITAVANJE TABELA I AUTENTIKACIJA TW TOKENA ###

Long_term <- read.xlsx("Long Term.xlsx",
                       sheetName = "Sheet1")

Task2 <- read.xlsx("Task 2.xlsx",
                   sheetName = "Sheet1")


View(Long_term)

View(Task2)



twitter_token <- create_token(
  app = "xxxx", 
  consumer_key = "xxxx",
  consumer_secret = "xxxx",
  access_token = "xxxx",
  access_secret = "xxxx"
)

api_key <- "xxxx"
api_secret <- "xxxx"
access_token <- "xxxx"
access_token_secret <- "xxxx"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

saveRDS(twitter_token, "C:/Users/User/Desktop/Dokumenta/Istrazivanja/Crypto/twitter_token.rds")                          

file.edit(".Renviron")                          

source("funkcije.R")                          


Tvitovi_long_term <- ekstrakcija_er(Long_term$Nalog, 
                                    pocetak = "2021-06-05 00:00:00 CET",
                                    kraj = "2021-07-05 00:00:00 CET")


write.csv(Tvitovi_long_term, "Tvitovi long term.csv", row.names = F)



setdiff(Long_term$Nalog, unique(Tvitovi_long_term$screenName))


Meta_Long_term <- naloziMeta(Long_term$Nalog)

write.csv(Meta_Long_term, "Meta podaci long term.csv", row.names = F)


Metrika_tvitovi_long_term <- retfav(Tvitovi_long_term)

write.csv(Metrika_tvitovi_long_term, "Metrika tvitovi long term.csv", row.names = F)


Klasicne_metrike_long_term <- cbind(Meta_Long_term[-4,], Metrika_tvitovi_long_term)

######################################################

#VeChain

twts_vet <- search_tweets("#VeChain", n = 18000)

sc_name <- table(twts_vet$screen_name)

head(sc_name)

sc_name_sort <- sort(sc_name, decreasing = TRUE)

head(sc_name_sort)

view(sc_name_sort)

df_sc_name_vechain <- as.data.frame(sc_name_sort)

#######################################################

#Cardano

twts_cardano <- search_tweets("#Cardano", n = 18000)

sc_name_cardano <- table(twts_cardano$screen_name)

sc_name_cardano <- sort(sc_name_cardano, decreasing = TRUE)

view(sc_name_cardano)

df_sc_name_cardano <- as.data.frame(sc_name_cardano)

##########################################################

# Sentiment VeChain

twts_vet_sentiment <- search_tweets("#VeChain", n = 18000, lang = "en", include_rts = FALSE)

sa_values <- get_nrc_sentiment(twts_vet_sentiment$text)

score <- colSums(sa_values[,])

score_df <- as.data.frame(score)

sa_score <- cbind(sentiment = row.names(score_df), score_df, row.names = NULL)

sa_score <- cbind(sa_score, status)

sa_score_group <- sa_score %>%
  group_by(status)

ggplot(data = sa_score, aes(x = sentiment, y = score, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = sa_score, aes(x = sentiment, y = score, fill = status)) +
  geom_bar(stat = 'identity') +
  theme_bw (base_size = 30)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = sa_score, aes(x = status, y = score, fill = status)) +
  geom_bar(stat = 'identity', show.legend = F) +
  theme_bw(base_size = 30)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################################

# Sentiment Filecoin

twts_fil_sentiment <- search_tweets("#Filecoin", n = 18000, lang = "en", include_rts = FALSE)

sa_values_fil <- get_nrc_sentiment(twts_fil_sentiment$text)

score_fil <- colSums(sa_values_fil[,])

score_fil_df <- as.data.frame(score_fil)

sa_score_fil <- cbind(sentiment = row.names(score_fil_df), score_fil_df, row.names = NULL)

ggplot(data = sa_score_fil, aes(x = sentiment, y = score_fil, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

status <- c("negativna", "pozitivna", "negativna", "negativna", "pozitivna", "negativna", "neutralna", "pozitivna", "negativna", "pozitivna")


sa_score_fil <- cbind(sa_score_fil, status)


ggplot(data = sa_score_fil, aes(x = sentiment, y = score_fil, fill = status)) +
  geom_bar(stat = 'identity') +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sa_score_fil_group <- sa_score_fil %>%
  group_by(status)

ggplot(data = sa_score_fil_group, aes(x = status, y = score_fil, fill = status)) +
  geom_bar(stat = 'identity', show.legend = F) +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


######################################################

# BITCOIN   17961 hash tvitova za jedan dan!

twts_bitcoin_sentiment <- search_tweets("#Bitcoin", n = 18000, lang = 'en', include_rts = F)


sa_values_bitcoin <- get_nrc_sentiment(twts_bitcoin_sentiment$text)

score_bitcoin <- colSums(sa_values_bitcoin[,])

score_bitcoin_df <- as.data.frame(score_bitcoin)

sa_score_bitcoin <- cbind(sentiment = row.names(score_bitcoin_df), score_bitcoin_df, row.names = NULL)

sa_score_bitcoin <- cbind(sa_score_bitcoin, status)


ggplot(data = sa_score_bitcoin, aes(x = sentiment, y = score_bitcoin, fill = status)) +
  geom_bar(stat = 'identity') +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sa_score_bitcoin_group <- sa_score_bitcoin %>%
  group_by(status)

ggplot(data = sa_score_bitcoin_group, aes(x = status, y = score_bitcoin, fill = status)) +
  geom_bar(stat = 'identity', show.legend = F) +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ALGORAND 

twts_algo_sentiment <- search_tweets("#Algorand", n = 18000, lang = 'en', include_rts = F)


sa_values_ALGO <- get_nrc_sentiment(twts_algo_sentiment$text)

score_ALGO <- colSums(sa_values_ALGO[,])

score_ALGO_df <- as.data.frame(score_ALGO)

sa_score_ALGO <- cbind(sentiment = row.names(score_ALGO_df), score_ALGO_df, row.names = NULL)

sa_score_ALGO <- cbind(sa_score_ALGO, status)


ggplot(data = sa_score_ALGO, aes(x = sentiment, y = score_ALGO, fill = status)) +
  geom_bar(stat = 'identity') +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sa_score_ALGO_group <- sa_score_ALGO %>%
  group_by(status)

ggplot(data = sa_score_ALGO_group, aes(x = status, y = score_ALGO, fill = status)) +
  geom_bar(stat = 'identity', show.legend = F) +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# CHAINLINK

twts_link_sentiment <- search_tweets("#Chainlink", n = 18000, lang = 'en', include_rts = F)


sa_values_LINK <- get_nrc_sentiment(twts_link_sentiment$text)

score_LINK <- colSums(sa_values_LINK[,])

score_LINK_df <- as.data.frame(score_LINK)

sa_score_LINK <- cbind(sentiment = row.names(score_LINK_df), score_LINK_df, row.names = NULL)

sa_score_LINK <- cbind(sa_score_LINK, status)


ggplot(data = sa_score_LINK, aes(x = sentiment, y = score_LINK, fill = status)) +
  geom_bar(stat = 'identity') +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sa_score_LINK_group <- sa_score_LINK %>%
  group_by(status)

ggplot(data = sa_score_LINK_group, aes(x = status, y = score_LINK, fill = status)) +
  geom_bar(stat = 'identity', show.legend = F) +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# TEZOS
twts_xtz_sentiment <- search_tweets("#Tezos", n = 18000, lang = 'en', include_rts = F)


sa_values_XTZ <- get_nrc_sentiment(twts_xtz_sentiment$text)

score_XTZ <- colSums(sa_values_XTZ[,])

score_XTZ_df <- as.data.frame(score_XTZ)

sa_score_XTZ <- cbind(sentiment = row.names(score_XTZ_df), score_XTZ_df, row.names = NULL)

sa_score_XTZ <- cbind(sa_score_XTZ, status)


ggplot(data = sa_score_XTZ, aes(x = sentiment, y = score_XTZ, fill = status)) +
  geom_bar(stat = 'identity') +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sa_score_XTZ_group <- sa_score_XTZ %>%
  group_by(status)

ggplot(data = sa_score_XTZ_group, aes(x = status, y = score_XTZ, fill = status)) +
  geom_bar(stat = 'identity', show.legend = F) +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################################

Metapodaci <- naloziMeta(Task2$Nalog)

Metapodaci <- Metapodaci[-3,]

Ukupno_tvitova_hashtag <- c(17961, 1026, 2876, 4310)

Metapodaci <- cbind(Metapodaci, Ukupno_tvitova_hashtag)

Broj_dana_hash <- c(1, 9, 9, 9)

Metapodaci <- cbind(Metapodaci, Broj_dana_hash)

write.xlsx(Metapodaci, "Metapodaci.xlsx", row.names = F)



# Sredjivanje tabela i ciscenje podataka

#Podaci su preuzeti sa CoinMarketCap-a

#BITCOIN

Tabela_Bitcoin <- read.csv("Tabela Bitcoin.csv")

str(Tabela_Bitcoin)

write.xlsx(Tabela_Bitcoin, "Tabela Bitcoin.xlsx", row.names = F)

Tabela_Bitcoin <- read.xlsx("Excel/Tabela Bitcoin.xlsx", sheetName = "Sheet1")

Tabela_Bitcoin$Date <- as.Date(Tabela_Bitcoin$Date, format = '%b %d %Y')

Tabela_Bitcoin$Date <- as.Date(na.approx(Tabela_Bitcoin$Date))


#CHAINLINK

Tabela_Chainlink <- read.csv("Tabela Chainlik.csv")

write.xlsx(Tabela_Chainlink, "Tabela Chainlink.xlsx", row.names = F)

Tabela_Chainlink <- read.xlsx("Excel/Tabela Chainlink.xlsx", sheetName = "Sheet1")

Tabela_Chainlink$Date <- as.Date(Tabela_Chainlink$Date, format = '%b %d %Y')

Tabela_Chainlink$Date <- as.Date(na.approx(Tabela_Chainlink$Date))

str(Tabela_Chainlink)



#ALGORAND

Tabela_Algorand <- read.csv("Tabela Algorand.csv")

write.xlsx(Tabela_Algorand, "Tabela Algorand.xlsx", row.names = F)

Tabela_Algorand <- read.xlsx("Excel/Tabela Algorand.xlsx", sheetName = "Sheet1")

Tabela_Algorand$Date <- as.Date(Tabela_Algorand$Date, format = '%b %d %Y')

Tabela_Algorand$Date <- as.Date(na.approx(Tabela_Algorand$Date))

str(Tabela_Algorand)



#TEZOS

Tabela_Tezos <- read.csv("Tabela Tezos.csv")

write.xlsx(Tabela_Tezos, "Tabela Tezos.xlsx", row.names = F)

Tabela_Tezos <- read.xlsx("Excel/Tabela Tezos.xlsx", sheetName = "Sheet1")


# Kreiranje novih atributa

# Raskorak

Tabela_Bitcoin$Raskorak <- Tabela_Bitcoin$Close - Tabela_Bitcoin$Open

Tabela_Algorand$Raskorak <- Tabela_Algorand$Close- Tabela_Algorand$Open

Tabela_Chainlink$Raskorak <- Tabela_Chainlink$Close- Tabela_Chainlink$Open

Tabela_Tezos$Raskorak <- Tabela_Tezos$Close - Tabela_Tezos$Open


# Mesec

Tabela_Bitcoin$Mesec <- format(as.POSIXct(Tabela_Bitcoin$Date, format = '%Y-%m-%d'), format = "%Y-%m")

Tabela_Algorand$Mesec <- format(as.POSIXct(Tabela_Algorand$Date, format = '%Y-%m-%d'), format = "%Y-%m")

Tabela_Chainlink$Mesec <- format(as.POSIXct(Tabela_Chainlink$Date, format = '%Y-%m-%d'), format = "%Y-%m")

Tabela_Tezos$Mesec <- format(as.POSIXct(Tabela_Tezos$Date, format = '%Y-%m-%d'), format = "%Y-%m")


#Procentualna promena po danu (cena i volume)


Tabela_Bitcoin <- Tabela_Bitcoin %>%
  mutate(Proc_promena = (Close/lead(Close) - 1) * 100)

Tabela_Algorand <- Tabela_Algorand %>%
  mutate(Proc_promena = (Close/lead(Close) - 1) * 100)

Tabela_Chainlink <- Tabela_Chainlink %>%
  mutate(Proc_promena = (Close/lead(Close) - 1) * 100)

Tabela_Tezos <- Tabela_Tezos %>%
  mutate(Proc_promena = (Close/lead(Close) - 1) * 100)



#Volume

Tabela_Bitcoin <- Tabela_Bitcoin %>%
  mutate(Proc_promena_volume = (Volume/lead(Volume) - 1) * 100)

Tabela_Algorand <- Tabela_Algorand %>%
  mutate(Proc_promena_volume = (Volume/lead(Volume) - 1) * 100)

Tabela_Chainlink <- Tabela_Chainlink %>%
  mutate(Proc_promena_volume = (Volume/lead(Volume) - 1) * 100)

Tabela_Tezos<- Tabela_Tezos %>%
  mutate(Proc_promena_volume = (Volume/lead(Volume) - 1) * 100)


#Filtriranje po pozitivnosti procentualne promene vrednosti po danu

Pozitivna_proc_promena_po_danu_Bitcoin <- Tabela_Bitcoin %>%
  filter(Proc_promena > 0)

Negativna_proc_promena_po_danu_Bitcoin <- Tabela_Bitcoin %>%
  filter(Proc_promena < 0)



Pozitivna_proc_promena_po_danu_Algo <- Tabela_Algorand %>%
  filter(Proc_promena > 0)

Negativna_proc_promena_po_danu_Algo <- Tabela_Algorand %>%
  filter(Proc_promena < 0)



Pozitivna_proc_promena_po_danu_Link <- Tabela_Chainlink %>%
  filter(Proc_promena > 0)

Negativna_proc_promena_po_danu_Link <- Tabela_Chainlink %>%
  filter(Proc_promena < 0)



Pozitivna_proc_promena_po_danu_Xtz <- Tabela_Tezos %>%
  filter(Proc_promena > 0)

Negativna_proc_promena_po_danu_Xtz <- Tabela_Tezos %>%
  filter(Proc_promena < 0)


#Procentualna razlika izmedju pocetne i krajnje cene po danu

Tabela_Bitcoin <- Tabela_Bitcoin %>%
  mutate(Proc_razlika_za_dan = ((Close - Open) / Open) * 100)

Tabela_Algorand <- Tabela_Algorand %>%
  mutate(Proc_razlika_za_dan = ((Close - Open) / Open) * 100)

Tabela_Chainlink <- Tabela_Chainlink %>%
  mutate(Proc_razlika_za_dan = ((Close - Open) / Open) * 100)

Tabela_Tezos <- Tabela_Tezos %>%
  mutate(Proc_razlika_za_dan = ((Close - Open) / Open) * 100)


#Procentualna promena izmedju najvise i najnize dnevne vrednosti

Tabela_Bitcoin <- Tabela_Bitcoin %>%
  mutate(Proc_razlika_max_i_min = ((High - Low) / Low ) * 100)

Tabela_Algorand <- Tabela_Algorand %>%
  mutate(Proc_razlika_max_i_min = ((High - Low) / Low ) * 100)

Tabela_Chainlink <- Tabela_Chainlink %>%
  mutate(Proc_razlika_max_i_min = ((High - Low) / Low ) * 100)

Tabela_Tezos <- Tabela_Tezos %>%
  mutate(Proc_razlika_max_i_min = ((High - Low) / Low ) * 100)


# Prvi - zadnji i  max - min


#BITCOIN

Bitkoin_maksimum_po_mesecu <- Tabela_Bitcoin %>%
  group_by(Mesec) %>%
  slice_max(Close, n = 1)


Bitkoin_maksimum_po_mesecu <- Bitkoin_maksimum_po_mesecu[-27,]

Bitcoin_minimum_po_mesecu <- Tabela_Bitcoin %>%
  group_by(Mesec) %>%
  slice_min(Close, n = 1)


Bitcoin_zadnja_vrednost_po_mesecu <- Tabela_Bitcoin %>%
  group_by(Mesec) %>%
  slice_head(n = 1)

Bitcoin_prva_vrednost_po_mesecu <- Tabela_Bitcoin %>%
  group_by(Mesec) %>%
  slice_tail(n = 1)


    
Bitcoin_prvi_zadnji_mesec <- cbind(Bitcoin_zadnja_vrednost_po_mesecu$Mesec, Bitcoin_prva_vrednost_po_mesecu$Close, Bitcoin_zadnja_vrednost_po_mesecu$Close)

Bitcoin_prvi_zadnji_mesec <- as.data.frame(Bitcoin_prvi_zadnji_mesec)


Bitcoin_prvi_zadnji_mesec <- Bitcoin_prvi_zadnji_mesec %>%
  rename(Mesec = V1) %>%
  rename(Prvi_dan_Close = V2) %>%
  rename(Poslednji_dan_Close = V3)

str(Bitcoin_prvi_zadnji_mesec)

Bitcoin_prvi_zadnji_mesec$Mesec <- as.Date(paste(Bitcoin_prvi_zadnji_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Bitcoin_prvi_zadnji_mesec$Prvi_dan_Close <- as.numeric(Bitcoin_prvi_zadnji_mesec$Prvi_dan_Close)

Bitcoin_prvi_zadnji_mesec$Poslednji_dan_Close <- as.numeric(Bitcoin_prvi_zadnji_mesec$Poslednji_dan_Close)

Bitcoin_prvi_zadnji_mesec$Razlika <- Bitcoin_prvi_zadnji_mesec$Poslednji_dan_Close - Bitcoin_prvi_zadnji_mesec$Prvi_dan_Close

Bitcoin_prvi_zadnji_mesec <- Bitcoin_prvi_zadnji_mesec %>%
                             mutate(Proc_promena = ((Poslednji_dan_Close - Prvi_dan_Close) / Prvi_dan_Close ) * 100)



Bitcoin_max_min_mesec <- cbind(Bitkoin_maksimum_po_mesecu$Mesec, Bitkoin_maksimum_po_mesecu$Close, Bitcoin_minimum_po_mesecu$Close)

Bitcoin_max_min_mesec <- as.data.frame(Bitcoin_max_min_mesec)

Bitcoin_max_min_mesec <- Bitcoin_max_min_mesec %>%
  rename(Mesec = V1) %>%
  rename(Max_Close = V2) %>%
  rename(Min_Close = V3)


Bitcoin_max_min_mesec$Mesec <- as.Date(paste(Bitcoin_max_min_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Bitcoin_max_min_mesec$Max_Close <- as.numeric(Bitcoin_max_min_mesec$Max_Close)

Bitcoin_max_min_mesec$Min_Close <- as.numeric(Bitcoin_max_min_mesec$Min_Close)




#Dolar cost average profit po koinu

Bitcoin_prva_vrednost_po_mesecu_skraceno <- Bitcoin_prva_vrednost_po_mesecu[-1:-53,]
Bitcoin_prva_vrednost_po_mesecu_skraceno$Bitcoina_za_100_dolara <- 100/Bitcoin_prva_vrednost_po_mesecu_skraceno$Close
Ukupno_bitcoina_dolar_cost_average <- sum(Bitcoin_prva_vrednost_po_mesecu_skraceno$Bitcoina_za_100_dolara)
Razlika_Bitcoin <- Ukupno_bitcoina_dolar_cost_average*39406.94 - 2600


Algo_prva_vrednost_po_mesecu$Algo_za_100_dolara <- 100/Algo_prva_vrednost_po_mesecu$Close
Ukupno_algo_dolar_cost_average <- sum(Algo_prva_vrednost_po_mesecu$Algo_za_100_dolara)
Razlika_Algo <- Ukupno_algo_dolar_cost_average*0.8542 - 2600


Link_prva_vrednost_po_mesecu_skraceno <- Link_prva_vrednost_po_mesecu[-1:-21,]
Link_prva_vrednost_po_mesecu_skraceno$Link_za_100_dolara <- 100/Link_prva_vrednost_po_mesecu_skraceno$Close
Ukupno_link_dolar_cost_average <- sum(Link_prva_vrednost_po_mesecu_skraceno$Link_za_100_dolara)
Razlika_Link <- Ukupno_link_dolar_cost_average*19.50 - 2600


Xtz_prva_vrednost_po_mesecu_skraceno <- Xtz_prva_vrednost_po_mesecu[-1:-20,]
Xtz_prva_vrednost_po_mesecu_skraceno$Xtz_za_100_dolara <- 100/Xtz_prva_vrednost_po_mesecu_skraceno$Close
Ukupno_xtz_dolar_cost_average <- sum(Xtz_prva_vrednost_po_mesecu_skraceno$Xtz_za_100_dolara)
Razlika_Xtz <- Ukupno_xtz_dolar_cost_average*2.86 - 2600


Profit <- cbind(Razlika_Bitcoin, Razlika_Algo, Razlika_Link, Razlika_Xtz)

Profit <- as.matrix(Profit)

Profit <- t(Profit)

Profit <- as.data.frame(Profit)

Profit$Coin <- c("Bitcoin", "Algo", "Link", "Xtz")

Profit <- Profit %>%
  rename(Profit = V1)

Profit$Last_close_price <- c(39406.94, 0.8542, 19.50, 2.86)

rownames(Profit) <- NULL

Profit <- Profit[, c("Coin", "Last_close_price", "Profit")]

Profit_plot <- ggplot(Profit, aes(x = reorder(Coin, -`Profit`), y = Profit, fill = Coin)) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Profit", x = "Crypto") +
  theme_bw(base_size = 30) 


Profit_tabela_plot <- ggtexttable(Profit, rows = NULL, theme = ttheme("mOrange"))

ggarrange(Profit_plot, Profit_tabela_plot, ncol = 2, nrow = 1, widths = c(5,1), heights = c(5,5))


#ALGO

Algo_maksimum_po_mesecu <- Tabela_Algorand %>%
  group_by(Mesec) %>%
  slice_max(Close, n = 1)


Algo_minimum_po_mesecu <- Tabela_Algorand %>%
  group_by(Mesec) %>%
  slice_min(Close, n = 1)


Algo_zadnja_vrednost_po_mesecu <- Tabela_Algorand %>%
  group_by(Mesec) %>%
  slice_head(n = 1)

Algo_prva_vrednost_po_mesecu <- Tabela_Algorand %>%
  group_by(Mesec) %>%
  slice_tail(n = 1)



Algo_prvi_zadnji_mesec <- cbind(Algo_zadnja_vrednost_po_mesecu$Mesec, Algo_prva_vrednost_po_mesecu$Close, Algo_zadnja_vrednost_po_mesecu$Close)

Algo_prvi_zadnji_mesec <- as.data.frame(Algo_prvi_zadnji_mesec)


Algo_prvi_zadnji_mesec <- Algo_prvi_zadnji_mesec %>%
  rename(Mesec = V1) %>%
  rename(Prvi_dan_Close = V2) %>%
  rename(Poslednji_dan_Close = V3)

str(Algo_prvi_zadnji_mesec)

Algo_prvi_zadnji_mesec$Mesec <- as.Date(paste(Algo_prvi_zadnji_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Algo_prvi_zadnji_mesec$Prvi_dan_Close <- as.numeric(Algo_prvi_zadnji_mesec$Prvi_dan_Close)

Algo_prvi_zadnji_mesec$Poslednji_dan_Close <- as.numeric(Algo_prvi_zadnji_mesec$Poslednji_dan_Close)

Algo_prvi_zadnji_mesec$Razlika <- Algo_prvi_zadnji_mesec$Poslednji_dan_Close - Algo_prvi_zadnji_mesec$Prvi_dan_Close

Algo_prvi_zadnji_mesec <- Algo_prvi_zadnji_mesec %>%
  mutate(Proc_promena = ((Poslednji_dan_Close - Prvi_dan_Close) / Prvi_dan_Close ) * 100)



Algo_max_min_mesec <- cbind(Algo_maksimum_po_mesecu$Mesec, Algo_maksimum_po_mesecu$Close, Algo_minimum_po_mesecu$Close)

Algo_max_min_mesec <- as.data.frame(Algo_max_min_mesec)

Algo_max_min_mesec <- Algo_max_min_mesec %>%
  rename(Mesec = V1) %>%
  rename(Max_Close = V2) %>%
  rename(Min_Close = V3)


Algo_max_min_mesec$Mesec <- as.Date(paste(Algo_max_min_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Algo_max_min_mesec$Max_Close <- as.numeric(Algo_max_min_mesec$Max_Close)

Algo_max_min_mesec$Min_Close <- as.numeric(Algo_max_min_mesec$Min_Close)




#CHAINLINK

Link_maksimum_po_mesecu <- Tabela_Chainlink %>%
  group_by(Mesec) %>%
  slice_max(Close, n = 1)


Link_minimum_po_mesecu <- Tabela_Chainlink %>%
  group_by(Mesec) %>%
  slice_min(Close, n = 1)


Link_zadnja_vrednost_po_mesecu <- Tabela_Chainlink %>%
  group_by(Mesec) %>%
  slice_head(n = 1)

Link_prva_vrednost_po_mesecu <- Tabela_Chainlink %>%
  group_by(Mesec) %>%
  slice_tail(n = 1)



Link_prvi_zadnji_mesec <- cbind(Link_zadnja_vrednost_po_mesecu$Mesec, Link_prva_vrednost_po_mesecu$Close, Link_zadnja_vrednost_po_mesecu$Close)

Link_prvi_zadnji_mesec <- as.data.frame(Link_prvi_zadnji_mesec)


Link_prvi_zadnji_mesec <- Link_prvi_zadnji_mesec %>%
  rename(Mesec = V1) %>%
  rename(Prvi_dan_Close = V2) %>%
  rename(Poslednji_dan_Close = V3)

str(Link_prvi_zadnji_mesec)

Link_prvi_zadnji_mesec$Mesec <- as.Date(paste(Link_prvi_zadnji_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Link_prvi_zadnji_mesec$Prvi_dan_Close <- as.numeric(Link_prvi_zadnji_mesec$Prvi_dan_Close)

Link_prvi_zadnji_mesec$Poslednji_dan_Close <- as.numeric(Link_prvi_zadnji_mesec$Poslednji_dan_Close)

Link_prvi_zadnji_mesec$Razlika <- Link_prvi_zadnji_mesec$Poslednji_dan_Close - Link_prvi_zadnji_mesec$Prvi_dan_Close

Link_prvi_zadnji_mesec <- Link_prvi_zadnji_mesec %>%
  mutate(Proc_promena = ((Poslednji_dan_Close - Prvi_dan_Close) / Prvi_dan_Close ) * 100)



Link_max_min_mesec <- cbind(Link_maksimum_po_mesecu$Mesec, Link_maksimum_po_mesecu$Close, Link_minimum_po_mesecu$Close)

Link_max_min_mesec <- as.data.frame(Link_max_min_mesec)

Link_max_min_mesec <- Link_max_min_mesec %>%
  rename(Mesec = V1) %>%
  rename(Max_Close = V2) %>%
  rename(Min_Close = V3)


Link_max_min_mesec$Mesec <- as.Date(paste(Link_max_min_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Link_max_min_mesec$Max_Close <- as.numeric(Link_max_min_mesec$Max_Close)

Link_max_min_mesec$Min_Close <- as.numeric(Link_max_min_mesec$Min_Close)




# TEZOS

Xtz_maksimum_po_mesecu <- Tabela_Tezos %>%
  group_by(Mesec) %>%
  slice_max(Close, n = 1)

Xtz_maksimum_po_mesecu <- Xtz_maksimum_po_mesecu[-9,]



Xtz_minimum_po_mesecu <- Tabela_Tezos %>%
  group_by(Mesec) %>%
  slice_min(Close, n = 1)


Xtz_zadnja_vrednost_po_mesecu <- Tabela_Tezos %>%
  group_by(Mesec) %>%
  slice_head(n = 1)

Xtz_prva_vrednost_po_mesecu <- Tabela_Tezos %>%
  group_by(Mesec) %>%
  slice_tail(n = 1)



Xtz_prvi_zadnji_mesec <- cbind(Xtz_zadnja_vrednost_po_mesecu$Mesec, Xtz_prva_vrednost_po_mesecu$Close, Xtz_zadnja_vrednost_po_mesecu$Close)

Xtz_prvi_zadnji_mesec <- as.data.frame(Xtz_prvi_zadnji_mesec)


Xtz_prvi_zadnji_mesec <- Xtz_prvi_zadnji_mesec %>%
  rename(Mesec = V1) %>%
  rename(Prvi_dan_Close = V2) %>%
  rename(Poslednji_dan_Close = V3)

str(Xtz_prvi_zadnji_mesec)

Xtz_prvi_zadnji_mesec$Mesec <- as.Date(paste(Xtz_prvi_zadnji_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Xtz_prvi_zadnji_mesec$Prvi_dan_Close <- as.numeric(Xtz_prvi_zadnji_mesec$Prvi_dan_Close)

Xtz_prvi_zadnji_mesec$Poslednji_dan_Close <- as.numeric(Xtz_prvi_zadnji_mesec$Poslednji_dan_Close)

Xtz_prvi_zadnji_mesec$Razlika <- Xtz_prvi_zadnji_mesec$Poslednji_dan_Close - Xtz_prvi_zadnji_mesec$Prvi_dan_Close

Xtz_prvi_zadnji_mesec <- Xtz_prvi_zadnji_mesec %>%
  mutate(Proc_promena = ((Poslednji_dan_Close - Prvi_dan_Close) / Prvi_dan_Close ) * 100)



Xtz_max_min_mesec <- cbind(Xtz_minimum_po_mesecu$Mesec, Xtz_maksimum_po_mesecu$Close, Xtz_minimum_po_mesecu$Close)

Xtz_max_min_mesec <- as.data.frame(Xtz_max_min_mesec)

Xtz_max_min_mesec <- Xtz_max_min_mesec %>%
  rename(Mesec = V1) %>%
  rename(Max_Close = V2) %>%
  rename(Min_Close = V3)


Xtz_max_min_mesec$Mesec <- as.Date(paste(Xtz_max_min_mesec$Mesec,1,sep="-"),"%Y-%m-%d")

Xtz_max_min_mesec$Max_Close <- as.numeric(Xtz_max_min_mesec$Max_Close)

Xtz_max_min_mesec$Min_Close <- as.numeric(Xtz_max_min_mesec$Min_Close)

Xtz_max_min_mesec <- Xtz_max_min_mesec %>%
  mutate(Proc_razlika = ((Max_Close - Min_Close) / Min_Close ) * 100)



##########################################################

#VIZUALIZACIJE

#Pocenta i krajnja cena po danu

ggplot(Tabela_Bitcoin, aes(x = Date)) +
  geom_line(aes(y = Close), color = "red") +
  geom_line(aes(y = Open), color = "blue") +
  labs(y = "Vrednost u $", x = "Dan", title = "Pocetna i krajnja vrednost BITCOIN-a po danu") +
  theme_bw(base_size = 30) 


ggplot(Tabela_Algorand, aes(x = Date)) +
  geom_line(aes(y = Close), color = "red") +
  geom_line(aes(y = Open), color = "blue") +
  labs(y = "Vrednost u $", x = "Dan", title = "Pocetna i krajnja vrednost ALGO-a po danu") +
  theme_bw(base_size = 30)


ggplot(Tabela_Chainlink, aes(x = Date)) +
  geom_line(aes(y = Close), color = "red") +
  geom_line(aes(y = Open), color = "blue") +
  labs(y = "Vrednost u $", x = "Dan", title = "Pocetna i krajnja vrednost LINK-a po danu") +
  theme_bw(base_size = 30)



ggplot(Tabela_Tezos, aes(x = Date)) +
  geom_line(aes(y = Close), color = "red") +
  geom_line(aes(y = Open), color = "blue") + 
  labs(y = "Vrednost u $", x = "Dan", title = "Pocetna i krajnja vrednost XTZ-a po danu") +
  theme_bw(base_size = 30)


#Raskorak

ggplot(Tabela_Bitcoin, aes(x = Date, y = Raskorak)) +
  geom_line() +
  labs(y = "Raskorak izmedju pocetne i krajnje cene u $", x = "Dan", title = "Volatilnost Bitcoin-a po danu") +
  theme_bw(base_size = 30)

ggplot(Tabela_Algorand, aes(x = Date, y = Raskorak)) +
  geom_line() +
  labs(y = "Raskorak izmedju pocetne i krajnje cene u $", x = "Dan", title = "Volatilnost ALGO-a po danu") +
  theme_bw(base_size = 30)

ggplot(Tabela_Chainlink, aes(x = Date, y = Raskorak)) +
  geom_line() +
  labs(y = "Raskorak izmedju pocetne i krajnje cene u $", x = "Dan", title = "Volatilnost LINKA-a po danu") +
  theme_bw(base_size = 30)

ggplot(Tabela_Tezos, aes(x = Date, y = Raskorak)) +
  geom_line() +
  labs(y = "Raskorak izmedju pocetne i krajnje cene u $", x = "Dan", title = "Volatilnost XTZ-a po danu") +
  theme_bw(base_size = 30)



#BITCOIN 

#Razlika po danu procentualna


ggplot(Tabela_Bitcoin, aes(x = Date, y = Proc_razlika_za_dan)) +
  geom_line() +
  labs(y = "Procentualna promena", title = "Procentualna promena izmedju Open i Close cene po danu BITCOIN") +
  theme_bw(base_size = 30)


#Prvi-zadnji u mesecu

ggplot(Bitcoin_prvi_zadnji_mesec, aes(x = Mesec)) +
  geom_line(aes(y = Prvi_dan_Close), color = "red") +
  geom_line(aes(y = Poslednji_dan_Close), color = "blue") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju krajnje cene za prvi i poslednji dan u mesecu BITCOIN") +
  theme_bw(base_size = 30)

ggplot(Bitcoin_prvi_zadnji_mesec, aes(x = Mesec, y = Razlika)) +
  geom_bar(stat = 'identity') +
  labs(y = "Razlika", x = "Mesec", title= "Razlika izmedju krajnje cene za prvi i poslednji dan u mesecu BITCOIN") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(Bitcoin_prvi_zadnji_mesec, aes(x = Mesec, y = Proc_promena)) +
  geom_bar(stat = 'identity') +
  labs(y = "Procentualna promena", x = "Mesec", title= "Procentualna promena krajnje cene za prvi i poslednji dan u mesecu BITCOIN") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Max-min po danu

ggplot(Tabela_Bitcoin, aes(x = Date)) +
  geom_line(aes(y = High), color = "blue") +
  geom_line(aes(y = Low), color = "red") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju maksimalne i minimalne vrednosti po danu BITCOIN") +
  theme_bw(base_size = 30)

ggplot(Tabela_Bitcoin, aes(x = Date, y = Proc_razlika_max_i_min)) +
  geom_line() +
  labs(y = "Procentualna razlika", x = "Dan", title = "Procentualna razlika maksimalna i minimalna vrednost po danu Bitcoin") +
  theme_bw(base_size = 30)


#MA i EMA

Tabela_Bitcoin <- Tabela_Bitcoin[-2380,]

Tabela_Bitcoin$MA <- SMA(Tabela_Bitcoin$Close, n = 50)

Tabela_Bitcoin$EMA <- SMA(Tabela_Bitcoin$Close, n = 200)

Death_Cross_Bitcoin <- Tabela_Bitcoin %>%
                      filter(MA > EMA, lead(MA) < lead(EMA))

Datum_Bitcoin_Death <- Death_Cross_Bitcoin$Date


Skok_Bitcoin <- Tabela_Bitcoin %>%
                filter (MA < EMA, lead(MA) > lead(EMA))



Tabela_Bitcoin <- Tabela_Bitcoin %>% arrange(Date)


#Procentualna promena po danu vrednosti i volume-a + medijana i ar. sredina


Median_Bitcoin <- median(Pozitivna_proc_promena_po_danu_Bitcoin$Proc_promena)

Mean_Bitcoin <- mean(Pozitivna_proc_promena_po_danu_Bitcoin$Proc_promena)


Neg_median_Bitcoin <- median(Negativna_proc_promena_po_danu_Bitcoin$Proc_promena)

Neg_mean_Bitcoin <- mean(Negativna_proc_promena_po_danu_Bitcoin$Proc_promena)


Sd_Bitcoin <- sd(Pozitivna_proc_promena_po_danu_Bitcoin$Proc_promena)

Neg_sd_Bitcoin <- sd(Negativna_proc_promena_po_danu_Bitcoin$Proc_promena)


Medijana_sredina_Bitcoin <- cbind(Median_Bitcoin, Mean_Bitcoin, Neg_median_Bitcoin, Neg_mean_Bitcoin)

Medijana_sredina_Bitcoin <- as.data.frame(Medijana_sredina_Bitcoin)

Medijana_sredina_Bitcoin <- cbind(Medijana_sredina_Bitcoin, Sd_Bitcoin, Neg_sd_Bitcoin)


Tabela_plot_Bitcoin <- ggtexttable(Medijana_sredina_Bitcoin, rows = NULL, theme = ttheme("mOrange"))

Text_Bitcoin <- paste("Daily percentage change in Bitcoin's price.",
                   " Mean (red) and median (blue) are given for both upward and downward trends.", sep = "")

Text_plot_Bitcoin <- ggparagraph(text = Text_Bitcoin, face = "italic", size = 11, color = "black")



Graph_Bitcoin <- ggplot(Tabela_Bitcoin, aes(x = Date)) +
  geom_bar(aes(y = Proc_promena), stat = 'identity') +
  geom_hline(yintercept = Median_Bitcoin, color = "blue") +
  geom_hline(yintercept = Mean_Bitcoin, color = "red") +
  geom_hline(yintercept = Neg_median_Bitcoin, color = "blue") +
  geom_hline(yintercept = Neg_mean_Bitcoin, color = "red") +
  ylim(-10, 10) +
  scale_x_continuous(breaks = round(seq(min(Tabela_Bitcoin$Date), max(Tabela_Bitcoin$Date), by = 366),1)) +
  labs(y = "(%)", x = "Year", title = "Percentage change in price by day Bitcoin") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



ggarrange(Graph_Bitcoin, Tabela_plot_Bitcoin, Text_plot_Bitcoin, 
          ncol = 1, nrow = 3,
          heights = c(5, 1, 0.6))


#Boxplot

ggplot(Pozitivna_proc_promena_po_danu_Bitcoin, aes(y = Proc_promena)) +
  geom_boxplot()


ggplot(Negativna_proc_promena_po_danu_Bitcoin, aes(y = Proc_promena)) +
  geom_boxplot()



# MA i EMA

Bitcoin_MA_EMA <- ggplot(Tabela_Bitcoin, aes(x = Date)) +
  geom_histogram(aes(y = Close), stat = 'identity') +
  geom_line(aes(y = MA), color = "orange") +
  geom_line(aes(y = EMA), color = "green") +
  scale_y_log10() +
  geom_vline(xintercept = Datum_Bitcoin_Death, color = "red") +
  geom_vline(xintercept = Datum_Bitcoin_Jump, color = "blue") +
  labs(y = "Price", x = "Day") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


kolone <- c(1,5)

Tabela_Death_Bitcoin <- Death_Cross_Bitcoin[, kolone]

Tabela_Jump_Bitcoin <- Skok_Bitcoin[, kolone]


Tabela_plot_Death_Bitcoin <- ggtexttable(Tabela_Death_Bitcoin, rows = NULL, theme = ttheme("lRed"))
Tabela_plot_Jump_Bitcoin <- ggtexttable(Tabela_Jump_Bitcoin, rows = NULL, theme = ttheme("lBlue"))

Tabele_Death_Jump_Bitcoin <- ggarrange(Tabela_plot_Death_Bitcoin, Tabela_plot_Jump_Bitcoin)

Tekst_Tabele_Death_Jump <- paste("Death cross (red) and golden cros (blue) dates and closing prices")
Tekst_Tabele_Death_Jump_plot <- ggparagraph(text = Tekst_Tabele_Death_Jump, size = 14, color = "purple")

Tabele_Death_Jump_Bitcoin_Tekst <- ggarrange(Tabele_Death_Jump_Bitcoin, Tekst_Tabele_Death_Jump_plot, ncol = 1, nrow = 2,
                                             heights = c(5, 1.5))

Text_Death_Jump_Bitcoin <- paste("Death crosses (red) and golden crosses (blue) for Bitcoin",
                      ". The intersections of the 50 day MA (orange) and the 200 day MA (green) are used to predict future price changes. When the 50 day MA crosses the 200 day MA from bellow (golden cross) it is believed that the price will explode. On the other hand, when the 50 day MA crosses the 200 day MA from above, it is believed that the price will plummet.", sep = "")

Text_plot_Death_Jump_Bitcoin <- ggparagraph(text = Text_Death_Jump_Bitcoin, face ="bold", size = 14, color = "black")


ggarrange(Bitcoin_MA_EMA, Tabele_Death_Jump_Bitcoin_Tekst, Text_plot_Death_Jump_Bitcoin, 
          ncol = 1, nrow = 3,
          heights = c(2, 1, 0.6))


# ALGORAND

# Razlika po danu procentualna

ggplot(Tabela_Algorand, aes(x = Date, y = Proc_razlika_za_dan)) +
  geom_line() +
  labs(y = "Procentualna promena", title = "Procentualna promena izmedju Open i Close cene po danu ALGO") +
  theme_bw(base_size = 30)

#Prvi-zadnji u mesecu

ggplot(Algo_prvi_zadnji_mesec, aes(x = Mesec)) +
  geom_line(aes(y = Prvi_dan_Close), color = "red") +
  geom_line(aes(y = Poslednji_dan_Close), color = "blue") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju krajnje cene za prvi i poslednji dan u mesecu Algo") +
  theme_bw(base_size = 30)

ggplot(Algo_prvi_zadnji_mesec, aes(x = Mesec, y = Razlika)) +
  geom_bar(stat = 'identity') +
  labs(y = "Razlika", x = "Mesec", title= "Razlika izmedju krajnje cene za prvi i poslednji dan u mesecu Algo") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(Algo_prvi_zadnji_mesec, aes(x = Mesec, y = Proc_promena)) +
  geom_bar(stat = 'identity') +
  labs(y = "Procentualna promena", x = "Mesec", title= "Procentualna promena krajnje cene za prvi i poslednji dan u mesecu Algo") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Max-min po danu

ggplot(Tabela_Algorand, aes(x = Date)) +
  geom_line(aes(y = High), color = "blue") +
  geom_line(aes(y = Low), color = "red") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju maksimalne i minimalne vrednosti po danu Algo") +
  theme_bw(base_size = 30)


ggplot(Tabela_Algorand, aes(x = Date, y = Proc_razlika_max_i_min)) +
  geom_line() +
  labs(y = "Procentualna razlika", x = "Dan", title = "Procentualna razlika maksimalna i minimalna vrednost po danu Algo") +
  theme_bw(base_size = 30)



#Procentualna promena po danu vrednosti i volume-a + medijana i ar. sredina


Median_Algo <- median(Pozitivna_proc_promena_po_danu_Algo$Proc_promena)

Mean_Algo <- mean(Pozitivna_proc_promena_po_danu_Algo$Proc_promena)


Neg_median_Algo <- median(Negativna_proc_promena_po_danu_Algo$Proc_promena)

Neg_mean_Algo <- mean(Negativna_proc_promena_po_danu_Algo$Proc_promena)


Sd_Algo <- sd(Pozitivna_proc_promena_po_danu_Algo$Proc_promena)

Neg_sd_Algo <- sd(Negativna_proc_promena_po_danu_Algo$Proc_promena)


Medijana_sredina_Algo <- cbind(Median_Algo, Mean_Algo, Neg_median_Algo, Neg_mean_Algo)

Medijana_sredina_Algo <- as.data.frame(Medijana_sredina_Algo)

Medijana_sredina_Algo <- cbind(Medijana_sredina_Algo, Sd_Algo, Neg_sd_Algo)


Tabela_plot_Algo <- ggtexttable(Medijana_sredina_Algo, rows = NULL, theme = ttheme("mOrange"))

Text_Algo <- paste("Daily percentage change in ALGO's price.",
                      " Mean (red) and median (blue) are given for both upward and downward trends.", sep = "")

Text_plot_Algo <- ggparagraph(text = Text_Algo, face = "italic", size = 11, color = "black")



Graph_Algo <- ggplot(Tabela_Algorand, aes(x = Date)) +
  geom_bar(aes(y = Proc_promena), stat = 'identity') +
  geom_hline(yintercept = Median_Algo, color = "blue") +
  geom_hline(yintercept = Mean_Algo, color = "red") +
  geom_hline(yintercept = Neg_median_Algo, color = "blue") +
  geom_hline(yintercept = Neg_mean_Algo, color = "red") +
  scale_x_continuous(breaks = round(seq(min(Tabela_Algorand$Date), max(Tabela_Algorand$Date), by = 366),1)) +
  labs(y = "(%)", x = "Year", title = "Percentage change in price by day ALGO") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



ggarrange(Graph_Algo, Tabela_plot_Algo, Text_plot_Algo, 
          ncol = 1, nrow = 3,
          heights = c(5, 1, 0.6))



# CHAINLINK

# Razlika po danu procentualna

ggplot(Tabela_Chainlink, aes(x = Date, y = Proc_razlika_za_dan)) +
  geom_line() +
  labs(y = "Procentualna promena", title = "Procentualna promena izmedju Open i Close cene po danu LINK") +
  theme_bw(base_size = 30)

#Prvi-zadnji u mesecu

ggplot(Link_prvi_zadnji_mesec, aes(x = Mesec)) +
  geom_line(aes(y = Prvi_dan_Close), color = "red") +
  geom_line(aes(y = Poslednji_dan_Close), color = "blue") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju krajnje cene za prvi i poslednji dan u mesecu Link") +
  theme_bw(base_size = 30)

ggplot(Link_prvi_zadnji_mesec, aes(x = Mesec, y = Razlika)) +
  geom_bar(stat = 'identity') +
  labs(y = "Razlika", x = "Mesec", title= "Razlika izmedju krajnje cene za prvi i poslednji dan u mesecu Link") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(Link_prvi_zadnji_mesec, aes(x = Mesec, y = Proc_promena)) +
  geom_bar(stat = 'identity') +
  labs(y = "Procentualna promena", x = "Mesec", title= "Procentualna promena krajnje cene za prvi i poslednji dan u mesecu Link") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Max-min po mesecu

ggplot(Tabela_Chainlink, aes(x = Date)) +
  geom_line(aes(y = High), color = "blue") +
  geom_line(aes(y = Low), color = "red") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju maksimalne i minimalne vrednosti po danu Link") +
  theme_bw(base_size = 30)


ggplot(Tabela_Chainlink, aes(x = Date, y = Proc_razlika_max_i_min)) +
  geom_line() +
  labs(y = "Procentualna razlika", x = "Dan", title = "Procentualna razlika maksimalna i minimalna vrednost po danu Link") +
  theme_bw(base_size = 30)


#Medijana i ar. sredina vrednost po danu

Median_Link <- median(Pozitivna_proc_promena_po_danu_Link$Proc_promena)

Mean_Link <- mean(Pozitivna_proc_promena_po_danu_Link$Proc_promena)


Neg_median_Link <- median(Negativna_proc_promena_po_danu_Link$Proc_promena)

Neg_mean_Link <- mean(Negativna_proc_promena_po_danu_Link$Proc_promena)



Sd_Link <- sd(Pozitivna_proc_promena_po_danu_Link$Proc_promena)

Neg_sd_Link <- sd(Negativna_proc_promena_po_danu_Link$Proc_promena)


Medijana_sredina_link <- cbind(Median_Link, Mean_Link, Neg_median_Link, Neg_mean_Link)

Medijana_sredina_link <- as.data.frame(Medijana_sredina_link)

Medijana_sredina_link <- cbind(Medijana_sredina_link, Sd_Link, Neg_sd_Link)


Tabela_plot_link <- ggtexttable(Medijana_sredina_link, rows = NULL, theme = ttheme("mOrange"))

Text_link <- paste("Daily percentage change in LINK's price.",
                   " Mean (red) and median (blue) are given for both upward and downward trends.", sep = "")

Text_plot_link <- ggparagraph(text = Text_link, face = "italic", size = 11, color = "black")


Proc_plot_link <- ggplot(Tabela_Chainlink, aes(x = Date)) +
  geom_bar(aes(y = Proc_promena), stat = 'identity') +
  geom_hline(yintercept = Median_Link, color = "blue") +
  geom_hline(yintercept = Mean_Link, color = "red") +
  geom_hline(yintercept = Neg_median_Link, color = "blue") +
  geom_hline(yintercept = Neg_mean_Link, color = "red") +
  scale_x_continuous(breaks = round(seq(min(Tabela_Chainlink$Date), max(Tabela_Chainlink$Date), by = 366),1)) +
  ylim(-30, 65) +
  labs(y = "(%)", x = "Year", title = "Percentage change in price by day LINK") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


ggarrange(Proc_plot_link, Tabela_plot_link, Text_plot_link, 
          ncol = 1, nrow = 3,
          heights = c(5, 1, 0.6))


#TEZOS

# Razlika po danu procentualna 

ggplot(Tabela_Tezos, aes(x = Date, y = Proc_razlika_za_dan)) +
  geom_line() +
  labs(y = "Procentualna promena", title = "Procentualna promena izmedju Open i Close cene po danu XTZ") +
  theme_bw(base_size = 30)

#Prvi-zadnji u mesecu

ggplot(Xtz_prvi_zadnji_mesec, aes(x = Mesec)) +
  geom_line(aes(y = Prvi_dan_Close), color = "red") +
  geom_line(aes(y = Poslednji_dan_Close), color = "blue") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju krajnje cene za prvi i poslednji dan u mesecu Xtz") +
  theme_bw(base_size = 30)

ggplot(Xtz_prvi_zadnji_mesec, aes(x = Mesec, y = Razlika)) +
  geom_bar(stat = 'identity') +
  labs(y = "Razlika", x = "Mesec", title= "Razlika izmedju krajnje cene za prvi i poslednji dan u mesecu Xtz") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(Xtz_prvi_zadnji_mesec, aes(x = Mesec, y = Proc_promena)) +
  geom_bar(stat = 'identity') +
  labs(y = "Procentualna promena", x = "Mesec", title= "Procentualna promena krajnje cene za prvi i poslednji dan u mesecu Xtz") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#Max-min po mesecu

ggplot(Tabela_Tezos, aes(x = Date)) +
  geom_line(aes(y = High), color = "blue") +
  geom_line(aes(y = Low), color = "red") +
  labs(y = "Vrednost u $", x = "Mesec", title = "Raskorak izmedju maksimalne i minimalne vrednosti po danu Xtz") +
  theme_bw(base_size = 30)


ggplot(Tabela_Tezos, aes(x = Date, y = Proc_razlika_max_i_min)) +
  geom_line() +
  labs(y = "Procentualna razlika", x = "Dan", title = "Procentualna razlika maksimalna i minimalna vrednost po danu Xtz") +
  theme_bw(base_size = 30)


#Procentualna promena po danu vrednosti i volume-a + medijana i ar. sredina


Median_Xtz <- median(Pozitivna_proc_promena_po_danu_Xtz$Proc_promena)

Mean_Xtz <- mean(Pozitivna_proc_promena_po_danu_Xtz$Proc_promena)


Neg_median_Xtz <- median(Negativna_proc_promena_po_danu_Xtz$Proc_promena)

Neg_mean_Xtz <- mean(Negativna_proc_promena_po_danu_Xtz$Proc_promena)


Sd_Xtz <- sd(Pozitivna_proc_promena_po_danu_Xtz$Proc_promena)

Neg_sd_Xtz <- sd(Negativna_proc_promena_po_danu_Xtz$Proc_promena)


Medijana_sredina_Xtz <- cbind(Median_Xtz, Mean_Xtz, Neg_median_Xtz, Neg_mean_Xtz)

Medijana_sredina_Xtz <- as.data.frame(Medijana_sredina_Xtz)

Medijana_sredina_Xtz <- cbind(Medijana_sredina_Xtz, Sd_Xtz, Neg_sd_Xtz)


Tabela_plot_Xtz <- ggtexttable(Medijana_sredina_Xtz, rows = NULL, theme = ttheme("mOrange"))

Text_Xtz <- paste("Daily percentage change in XTZ's price.",
                   " Mean (red) and median (blue) are given for both upward and downward trends.", sep = "")

Text_plot_Xtz <- ggparagraph(text = Text_Xtz, face = "italic", size = 11, color = "black")



Graph_Xtz <- ggplot(Tabela_Tezos, aes(x = Date)) +
  geom_bar(aes(y = Proc_promena), stat = 'identity') +
  geom_hline(yintercept = Median_Xtz, color = "blue") +
  geom_hline(yintercept = Mean_Xtz, color = "red") +
  geom_hline(yintercept = Neg_median_Xtz, color = "blue") +
  geom_hline(yintercept = Neg_mean_Xtz, color = "red") +
  scale_x_continuous(breaks = round(seq(min(Tabela_Tezos$Date), max(Tabela_Tezos$Date), by = 366),1)) +
  labs(y = "(%)", x = "Year", title = "Percentage change in price by day XTZ") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


ggarrange(Graph_Xtz, Tabela_plot_Xtz, Text_plot_Xtz, 
          ncol = 1, nrow = 3,
          heights = c(5, 1, 0.6))

#########################################################################################################################

## KRIPTOMAT ZADATAK ##


Kriptomat_podaci <- read.xlsx('Excel/User_Overview_by_Countries.xlsx', sheetIndex = 1)

str(Kriptomat_podaci)


Kriptomat_podaci <- Kriptomat_podaci %>%
  mutate(Percentage_difference_registered_verified = ((Number.of.verifications - Number.of.registrations) / Number.of.registrations) *100)

Kriptomat_podaci <- Kriptomat_podaci %>%
  mutate(Percentage_difference_verified_fiat = ((Number.of.fiat.depositors - Number.of.verifications) / Number.of.verifications)*100)

Kriptomat_podaci <- Kriptomat_podaci %>%
  mutate(Percentage_difference_verified_traders = ((Number.of.traders - Number.of.verifications) / Number.of.verifications)*100)

Kriptomat_podaci <- Kriptomat_podaci %>%
  mutate(Percentage_difference_fiat_traders = ((Number.of.traders - Number.of.fiat.depositors) / Number.of.fiat.depositors)*100)


Percentage_registered_verified_table <- Kriptomat_podaci[, c(1:3, 10)]  

Percentage_verified_fiat_table <- Kriptomat_podaci[, c(1, 3:4, 7)]

Percentage_verified_traders_table <- Kriptomat_podaci[, c(1, 3, 5 ,8)]


### Odnos registrovanih i verifikovanih ###

Percentage_registered_verified_plot <- ggplot(Kriptomat_podaci, aes(x = reorder(Country.name, Percentage_difference_registered_verified), y = Percentage_difference_registered_verified, fill = Country.name)) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "(%)", x = '')
  

Percentage_registered_verified_boxplot <- ggplot(Kriptomat_podaci, aes(y = Percentage_difference_registered_verified)) +
  geom_boxplot(col="black", fill = "yellow", outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  theme_bw(base_size = 27) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  labs(y = "(%)", x = "Aggregated data")



Plotovi_registered_verified <- ggarrange(Percentage_registered_verified_plot, Percentage_registered_verified_boxplot, ncol = 2, nrow = 1)


Percentage_registered_verified_table_plot <- ggtexttable(Percentage_registered_verified_table, rows = NULL, theme = ttheme("mOrange"))  

  
Percentage_registered_verified_text <- paste("Even if we exclude Canada as a non-European country and an outlier, we can see that the difference between countries is great (although we can make a distinction between 3 blocks (Austria, Switzerland-Slovakia, Slovenia-BiH).",
                                             " Aggregated data points to the fact that many potential customers are lost after the first step.", sep = "")

Percentage_registered_verified_text_plot <- ggparagraph(text = Percentage_registered_verified_text, face = "italic", size = 20, color = "black")  
  
  
ggarrange(Plotovi_registered_verified, Percentage_registered_verified_table_plot, Percentage_registered_verified_text_plot,
          ncol = 1, nrow = 3, heights = c(3, 1, 0.5))



### Odnos verifikovanih i onih koji su depozitovali fiat ###


Percentage_verified_fiat_plot <- ggplot(Kriptomat_podaci, aes(x = reorder(Country.name, Percentage_difference_verified_fiat), y = Percentage_difference_verified_fiat, fill = Country.name)) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "(%)", x = '')



Percentage_verified_fiat_boxplot <- ggplot(Kriptomat_podaci, aes(y = Percentage_difference_verified_fiat)) +
  geom_boxplot(col="black", fill = "yellow", outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  theme_bw(base_size = 27) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  labs(y = "(%)", x = "Aggregated data")


Plotovi_verified_fiat <- ggarrange(Percentage_verified_fiat_plot, Percentage_verified_fiat_boxplot, ncol = 2, nrow = 1)

Percentage_verified_fiat_table_plot <- ggtexttable(Percentage_verified_fiat_table, rows = NULL, theme = ttheme("mOrange"))

Percentage_verified_fiat_text <- paste("We can clearly see that, although there is a massive difference between some countries, the overall conversion rate is unacceptably inefficient.",
                                       " With a median of over 60%, immediate action is required at this step.",
                                       " This is especially true for Austria and Switzerland as the most promising countries (judging by the number of registrations and verifications).", sep = "")

Percentage_verified_fiat_text_plot <- ggparagraph(text = Percentage_verified_fiat_text,  face = "italic", size = 20, color = "black")

ggarrange(Plotovi_verified_fiat, Percentage_verified_fiat_table_plot, Percentage_verified_fiat_text_plot, ncol = 1, nrow = 3, heights = c(3, 1, 0.5))



### Odnos verifikovanih i onih koji su trgovali ###

Percentage_verified_traders_plot <- ggplot(Kriptomat_podaci, aes(x = reorder(Country.name, Percentage_difference_verified_traders), y = Percentage_difference_verified_traders, fill = Country.name)) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "(%)", x = "")


Percentage_verified_traders_boxplot <- ggplot(Kriptomat_podaci, aes(y = Percentage_difference_verified_traders)) +
  geom_boxplot(col="black", fill = "yellow", outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  theme_bw(base_size = 27) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  labs(y = "(%)", x = "Aggregated data")


Plotovi_verified_traders <- ggarrange(Percentage_verified_traders_plot, Percentage_verified_traders_boxplot, nrow = 1, ncol = 2)

Percentage_verified_traders_table_plot <- ggtexttable(Percentage_verified_traders_table, rows = NULL, theme = ttheme('mOrange'))


Percentage_verified_traders_text <- paste("The results are simmilar to those concerning ratio between verified and profiles which made fiat deposits.",
                                          " Although the number of traders is a bit bigger than the number of fiat depositors for some countries, this difference does not improve the conversion rate in any significant manner, and the sum of all traders remains a bit smaller than the sum of all fiat depositors.", sep = "")

Percentage_verified_traders_text_plot <- ggparagraph(Percentage_verified_traders_text, size = 20, face = "italic", color = "black")

ggarrange(Plotovi_verified_traders, Percentage_verified_traders_table_plot, Percentage_verified_traders_text_plot, ncol = 1,nrow = 3, heights = c(3, 1, 0.5))



### Funnel Charts ###

Funnel_nerazvrstano <- plot_ly()

Funnel_nerazvrstano <- Funnel_nerazvrstano %>%
  add_trace(
    type = "funnel",
    y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
    x = c(51356, 39152.7, 16450.6, 16395.6),
    textposition = "inside",
    textinfo = "value+percent initial") 
Funnel_nerazvrstano <- Funnel_nerazvrstano %>%
  layout(yaxis = list(categoryarray = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders")))

Funnel_nerazvrstano



Funnel_razvrstano <- plot_ly(
            type = 'funnel',
            name = 'Austria',
            y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
            x = c (23504.4, 19344.0, 9663.6, 9445.2),
            textinfo = "value+percent initial")

 Funnel_razvrstano <- Funnel_razvrstano %>%
   add_trace(type = 'funnel',
             name = 'Slovenia',
             orientation = "h",
             y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
             x = c (7696.8, 5218.2, 2977.2, 3012.3),
             textposition = "inside",
             textinfo = "value+percent initial")

 Funnel_razvrstano <- Funnel_razvrstano %>%
                      add_trace(type = 'funnel',
                      name = 'Switzerland',
                      orientation = "h",
                      y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
                      x = c (13874.9, 10826.4, 2724.8, 2757.3),
                      textposition = "inside",
                      textinfo = "value+percent initial")
 Funnel_razvrstano <- Funnel_razvrstano %>%
   add_trace(type = 'funnel',
             name = 'Slovakia',
             orientation = "h",
             y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
             x = c (1922.4, 1513.6, 816.0, 813.6),
             textposition = "inside",
             textinfo = "value+percent initial")
 Funnel_razvrstano <- Funnel_razvrstano %>%
   add_trace(type = 'funnel',
             name = 'Bosnia and Herzegovina',
             orientation = "h",
             y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
             x = c (2865.5, 1974.5, 231.0, 321.2),
             textposition = "inside",
             textinfo = "value+percent initial")
 Funnel_razvrstano <- Funnel_razvrstano %>%
   add_trace(type = 'funnel',
             name = 'Canada',
             orientation = "h",
             y = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders"),
             x = c (1492.0, 276.0, 38.0, 46.0),
             textposition = "inside",
             textinfo = "value+percent initial")
 Funnel_razvrstano <- Funnel_razvrstano %>%
   layout(yaxis = list(categoryarray = c("Number of registrations", "Number of verifications", "Number of fiat depositors", "Number of traders")))
 
Funnel_razvrstano



### GLOBAL CRYPTO ADOPTION DATA ###

GCA_data <- read.csv('Excel/country-factsheet-table.csv')

GCA_data$Numberof.crypto.owners <- gsub(",","", GCA_data$Numberof.crypto.owners)

GCA_data$Numberof.crypto.owners <- as.numeric(GCA_data$Numberof.crypto.owners)

GCA_data$Numberof.crypto.owners <- comma(GCA_data$Numberof.crypto.owners, digits = 0)

GCA_data$Percentageof.the.population <- gsub("%", "", GCA_data$Percentageof.the.population)

GCA_data$Percentageof.the.population <- as.numeric(GCA_data$Percentageof.the.population)



GCA_data <- GCA_data %>% arrange(desc(Numberof.crypto.owners))

GCA_data_top <- head(GCA_data, n = 10)

GCA_apsolutne <- ggplot(GCA_data_top, aes(x = reorder(Country.Â.â.ľ, -Numberof.crypto.owners), y = Numberof.crypto.owners, fill = Country.Â.â.ľ)) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Number of crypto owners", x = "Country")



GCA_data_procenti <- GCA_data %>% arrange(desc(Percentageof.the.population))

GCA_data_procenti_top <- head(GCA_data_procenti, n = 10)


GCA_procenti <- ggplot(GCA_data_procenti_top, aes(x = reorder(Country.Â.â.ľ, -Percentageof.the.population), y = Percentageof.the.population, fill = Country.Â.â.ľ)) +
  geom_bar(stat = 'identity', show.legend = F) +
  labs(y = "Percentage of the population", x = "Country")


GCA_tabela_apsolutne <- ggtexttable(GCA_data_top[,-4], rows = NULL, theme = ttheme("mOrange"))
  
  
GCA_tabela_procenti <- ggtexttable(GCA_data_procenti_top[,-4], rows = NULL, theme = ttheme("mOrange"))
  

ggarrange(GCA_apsolutne, GCA_procenti, GCA_tabela_apsolutne, GCA_tabela_procenti, ncol = 2, nrow = 2)


view(GCA_data)

GCA_data_filtrirano <- GCA_data %>%
  filter (Country.Â.â.ľ %in% c("Russia", "China", "Brazil", "Indonesia", "Turkey"))


write.xlsx(GCA_data_filtrirano, "GCA data filtrirano.xlsx", row.names = F)


ggplot(GCA_data_filtrirano, aes(x = Country.Â.â.ľ, y = Numberof.crypto.owners, fill = Country.Â.â.ľ)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="Waffle Chart", subtitle="Number of crypto owners by country",
       caption="Source: tripleA") + 
  theme(panel.border = element_rect(size = 2),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "right")


Crypto_ready_index <- read.csv("Excel/data.csv")

write.xlsx(Crypto_ready_index, "CRI.xlsx", row.names = F)

Crypto_ready_index <- read.xlsx("Excel/CRI.xlsx", sheetIndex = 1)

Crypto_ready_index <- Crypto_ready_index %>%
  rename(Annual_Crypto_Google_Searches_per_100K_People = X,
         Crypto_Searches_Annual_Increase = X.1,
         Number_of_Crypto_ATMs = X.2,
         People_per_Crypto_ATM = X.3,
         Area_per_Crypto_ATM = X.4,
         Tax_and_Legislation = X.5,
         Total_Crypto_Ready_Score = X.6)

CRI_filtrirano <- Crypto_ready_index %>%
  filter(Country.Region %in% c("Russia", "China", "Brazil", "Indonesia", "Turkey"))
