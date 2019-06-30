################################################################################################
#
# Análise Exploratória de Dados
# Por: Ricardo Reis
#
# CASO - Berka Financial Dataset
#
################################################################################################

#Carregando Bibliotecas
library(dplyr)
library(stringr) 
library(lubridate)

#Carregando Bases
account   <- read.csv("~/R-Projetos/Czech/dados/account.asc", sep=";")
card      <- read.csv("~/R-Projetos/Czech/dados/card.asc", sep=";")
client    <- read.csv("~/R-Projetos/Czech/dados/client.asc", sep=";")
disp      <- read.csv("~/R-Projetos/Czech/dados/disp.asc", sep=";")
district  <- read.csv("~/R-Projetos/Czech/dados/district.asc", sep=";")
loan      <- read.csv("~/R-Projetos/Czech/dados/loan.asc", sep=";")
order     <- read.csv("~/R-Projetos/Czech/dados/order.asc", sep=";")
trans     <- read.csv("~/R-Projetos/Czech/dados/trans.asc", sep=";")

glimpse(order)

###################################### TIDYING ############################################

#Account
levels(account$frequency) <- c("monthly","after transaction","weekly")
account$date <- as.Date(as.character(account$date), "%y%m%d")

#Card
card$issued <- as.Date(as.character(card$issued), "%y%m%d")

#Client
client$sex <- ifelse(substr(client$birth_number,3,4) >= 50, "F","M" )
client$yy <- substr(client$birth_number,1,2)
client$mm <- ifelse(substr(client$birth_number,3,4) >= 50, 
                    as.integer(substr(client$birth_number,3,4)) - 50,
                    substr(client$birth_number,3,4))
client$mm <- ifelse(nchar(client$mm) < 2, paste0("0",client$mm),client$mm)
client$dd <- substr(client$birth_number,5,6)
client$birth_date <- ymd(paste0("19",client$yy, client$mm, client$dd))

#Loan
loan$date <- as.Date(as.character(loan$date), "%y%m%d")

#Order
levels(order$k_symbol) <- c(" ", "leasing", "insurrance", "household", "loan")

#Transaction
trans$date <- as.Date(as.character(trans$date), "%y%m%d")
levels(trans$type) <- c("credit","withdrawal in cash","withdrawal")
levels(trans$operation) <- c("","remittance bank","collection bank", "credit cash", "cash withdraw", "credit card withdraw")
levels(trans$k_symbol) <- c("", " ", "old-age pension", "insurance", "negative balance", "household", "statement payment", "interest", "loan payment")

#District
district <- rename(district, district_id = A1)
district <- rename(district, district_name = A2)
district <- rename(district, district_region = A3)
district <- rename(district, district_inhabitants = A4)
district <- rename(district, n_cities = A9)
district <- rename(district, ratio_urban_inhabitants = A10)
district <- rename(district, average_salary = A11)
district <- rename(district, n_enterpreneurs_per_1000_inhabitants = A14)

###################################### JOINING ############################################

#Join de Disposition com Client
disp_aux <- filter(disp, type == "OWNER")
cli_dis <-  inner_join(client, disp_aux, by=c("client_id" = "client_id"))

#Join de cli_dis com Account
cli_dis_acc <- inner_join(cli_dis, account, by=c("account_id" = "account_id"))
