#1a
library(magrittr)
7%>%.^2%>%sqrt(.)

#1b
x = rnorm(n = 1000, mean = 5, sd = 3)
(x - mean(x) / sd(x))
max(x)

#1c
library(magrittr)
x = rnorm(n = 1000, mean = 5, sd = 3)
x%>%density(.)%>%plot(.)
max(x)

#2a
library(magrittr)
dd <- read.table("bcn_pisos.txt", header=TRUE)
dd[duplicated(dd), ]
sapply(split(dd$Valor, dd$Dist), mean) 

#2b
library(magrittr)
library(dplyr)
dd <- read.table("bcn_pisos.txt", header=TRUE)
dd[duplicated(dd), ]
num_cols%>%unlist(lapply(dd, is.numeric)) 
(num_cols - mean(num_cols) / sd(num_cols))

#2c
library(dplyr)
dd <- read.table("bcn_pisos.txt", header=TRUE)
dd[duplicated(dd), ]
dd %>%group_by(Dist)%>%mutate("Greater than is mean" = case_when(Valor > mean(Valor) ~ 'Y', TRUE ~ 'N'))

#3a
library(magrittr)
library(dplyr) 
dd <- read.table("bcn_pisos.txt", header=TRUE)
dd%>%unique()%>%group_by(Dist)%>%summarize("1Dorm"=sum(Dorm==1),"2Dorm"=sum(Dorm==2),"3Dorm"=sum(Dorm==3),
            "4Dorm"=sum(Dorm==4),"5Dorm"=sum(Dorm==5), Valor=mean(Valor), 
            "AscS"=sum(Ascens=="SI"),"AscN"=sum(Ascens=="NO"),
            Atics=sum(Planta=="Atic"),Bajos=sum(Planta=="Bajos"),Plantas=sum(Planta=="Planta"),
            Nous=sum(Edat<=10), SemiNous=sum(Edat<=20 & Edat>=11),Vells=sum(Edat<=50 & Edat>=21),MoltVells=sum(Edat>=51),
            Superf=mean(Superf))%>%rename(DistrictName=Dist)

#3b
view(dd)
str(dd)
summary(dd)
head(dd, n = 20)