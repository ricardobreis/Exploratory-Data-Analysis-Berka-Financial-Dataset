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
library(lubridate)
library(ggplot2)
library(plotly)

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
account <- rename(account, start_date = date)

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
levels(loan$status)

#Order
levels(order$k_symbol) <- c("Desconhecido", "leasing", "insurrance", "household", "loan")

#Transaction
trans$date <- as.Date(as.character(trans$date), "%y%m%d")
levels(trans$type) <- c("credit","withdrawal in cash","withdrawal")
levels(trans$operation) <- c("Desconhecido","remittance bank","collection bank", "credit cash", "cash withdraw", "credit card withdraw")
levels(trans$k_symbol) <- c("Desconhecido", "Desconhecido", "old-age pension", "insurance", "negative balance", "household", "statement payment", "interest", "loan payment")

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

#Join de cli_dis_acc com Loan
cli_loa <- inner_join(loan, cli_dis_acc, by=c("account_id" = "account_id"))

#Join de cli_dis_acc com Order
cli_ord <- inner_join(order, cli_dis_acc, by=c("account_id" = "account_id"))

#Join de cli_dis_acc com Trans
cli_trans <- inner_join(trans, cli_dis_acc, by=c("account_id" = "account_id"))

#Join de cli_dis_acc com Card
cli_card <- inner_join(card, cli_dis_acc, by=c("disp_id" = "disp_id"))

#Join de cli_dis_acc com district
cli_dist <- left_join(cli_dis_acc, district, by=c("district_id.x" = "district_id"))

###################################### GRÁFICOS ############################################

#LOAN
#Loan por status
table(loan$status)

ggplot(loan, aes(x=status)) + 
  geom_bar()

#DISPOSITION
#Disposition por type
table(disp$type)

ggplot(disp, aes(x=type)) + 
  geom_bar()

#ORDER
#Order por K_symbol
table(order$k_symbol)

ggplot(order, aes(x=k_symbol)) + 
  geom_bar()

#Order por bank_to
ggplot(order, aes(x=bank_to)) + 
  geom_bar()

#TRANSACTION
#Transaction por type
ggplot(trans, aes(x=type)) + 
  geom_bar()

#Transaction por operation
ggplot(trans, aes(x=operation)) + 
  geom_bar()

#Transaction por k_symbol
ggplot(trans, aes(x=k_symbol)) + 
  geom_bar()

#CARD
#Card por type
ggplot(card, aes(x=type)) + 
  geom_bar()

#DISTRICT
#District por region
ggplot(district, aes(x=district_region)) + 
  geom_bar()

#Loan por sexo
ggplot(cli_loa, aes(x=sex, y=amount)) + 
  geom_boxplot(alpha=0.3)

#Loan por ano
p <- plot_ly(loan, x = ~unique(year(loan$date)), y = ~table(year(loan$date)), type = 'scatter', mode = 'lines')


#Criação de accounts por ano
p <- plot_ly(account, x = ~unique(year(start_date)), y = ~table(year(account$start_date)), type = 'scatter', mode = 'lines')

#Transactions por ano
p <- plot_ly(trans, x = ~unique(year(trans$date)), y = ~table(year(trans$date)), type = 'scatter', mode = 'lines')

#Cards por ano
p <- plot_ly(card, x = ~unique(year(card$issued)), y = ~table(year(card$issued)), type = 'scatter', mode = 'lines')

#Cards por sexo e type
p <- plot_ly(cli_card, x = ~type.x, y= ~card_id, color = ~sex, type = "box") %>%
  layout(boxmode = "group")

#Order por sexo e k_symbol
p <- plot_ly(cli_ord, x = ~k_symbol, y = ~amount, color = ~sex, type = "box") %>%
  layout(boxmode = "group")

#Transaction por sexo e k_symbol
p <- plot_ly(cli_trans, x = ~k_symbol, y = ~amount, color = ~sex, type = "box") %>%
  layout(boxmode = "group")

#Transaction por sexo e operation
p <- plot_ly(cli_trans, x = ~operation, y = ~amount, color = ~sex, type = "box") %>%
  layout(boxmode = "group")

#Transaction por sexo e type
p <- plot_ly(cli_trans, x = ~type.x, y = ~amount, color = ~sex, type = "box") %>%
  layout(boxmode = "group")

#Clientes por sexo e região
aux <- cli_dist %>% group_by(district_region) %>% summarise(f = length(sex[sex == "F"]), m = length(sex[sex == "M"]))
p <- plot_ly(aux, x = ~m, y = ~f, text = ~district_region, type = 'scatter', mode = 'markers',
             marker = list(size = ~abs(m-f), opacity = 0.5)) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

#Clientes por sexo e cidade
aux <- cli_dist %>% group_by(district_name) %>% summarise(f = length(sex[sex == "F"]), m = length(sex[sex == "M"]))
p <- plot_ly(aux, x = ~m, y = ~f, text = ~district_name, type = 'scatter', mode = 'markers',
             marker = list(size = ~abs(m-f), opacity = 0.5)) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
