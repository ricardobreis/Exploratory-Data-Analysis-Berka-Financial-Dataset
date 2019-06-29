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

#Carregando Bases
account   <- read.csv("~/R-Projetos/Czech/dados/account.asc", sep=";")
card      <- read.csv("~/R-Projetos/Czech/dados/card.asc", sep=";")
client    <- read.csv("~/R-Projetos/Czech/dados/client.asc", sep=";")
disp      <- read.csv("~/R-Projetos/Czech/dados/disp.asc", sep=";")
district  <- read.csv("~/R-Projetos/Czech/dados/district.asc", sep=";")
loan      <- read.csv("~/R-Projetos/Czech/dados/loan.asc", sep=";")
order     <- read.csv("~/R-Projetos/Czech/dados/order.asc", sep=";")
trans     <- read.csv("~/R-Projetos/Czech/dados/trans.asc", sep=";")

glimpse(trans)

###################################### TIDYING ############################################

#Client
client$sex <- ifelse(substr(client$birth_number,3,4) >= 50, "F","M" )
client$yy <- substr(client$birth_number,1,2)
client$mm <- ifelse(substr(client$birth_number,3,4) >= 50, 
                    as.integer(substr(client$birth_number,3,4)) - 50,
                    substr(client$birth_number,3,4))
client$mm <- ifelse(nchar(client$mm) < 2, paste0("0",client$mm),client$mm)
client$dd <- substr(client$birth_number,5,6)
#client$yymmdd <- as.Date(paste0(client$yy, client$mm, client$dd), "%y%m%d" )

#Account
levels(account$frequency) <- c("monthly","after transaction","weekly")
account$date <- as.Date(as.character(account$date), "%y%m%d")

#Transaction
trans$date <- as.Date(as.character(trans$date), "%y%m%d")
levels(trans$type) <- c("credit","withdrawal in cash","withdrawal")
levels(trans$operation) <- c("","remittance bank","collection bank", "credit cash", "cash withdraw", "credit card withdraw")
levels(trans$k_symbol) <- c("", " ", "old-age pension", "insurance", "negative balance", "household", "statement payment", "interest", "loan payment")
