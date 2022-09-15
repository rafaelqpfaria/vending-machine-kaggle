library (tidyverse)
library (funModeling)

vend_mach <- read.csv("vending_machine_sales.csv")

vend_mach_utl<-
  vend_mach %>% 
  select(-TransDate,-Transaction,-RCoil,
         -MCoil, -Status, -Device.ID,
         -MPrice, -MQty, -TransTotal) %>% 
  mutate_at(c("Location","Machine","Type","Category"), as.factor)


vend_mach_utl %>% 
  df_status()

totalSales<-
  aggregate(LineTotal ~ Machine, vend_mach_utl,sum)## soma geral de qual maquina deu mais lucro

total_sales_prod <-
  aggregate(LineTotal ~ Machine + Product, vend_mach_utl, sum) ## somou a coluna de vendas pelas mesmas machines e produtos

total_sales_date <- 
  aggregate(LineTotal ~ Machine + Prcd.Date, vend_mach_utl, sum) ## somou a coluna de vendas da mesma maquina no mesmo dia

total_product <- 
  aggregate(LineTotal ~ Product, vend_mach_utl, sum) %>% 
  arrange(desc(LineTotal)) %>% 
  filter( LineTotal > 200) 

total_product %>% 
  mutate(Product = fct_reorder(Product, LineTotal )) %>%
  ggplot(aes(x = Product , y = LineTotal , fill = Product))+
  geom_bar( stat="identity")

ordered_sales<-
  total_sales_prod %>% 
  group_by(Machine,Product) %>% 
  summarise(LineTotal) ### organizou as vendas por maquina e produto


ordered_sales<-
  rename(ordered_sales, CumulativeSales = LineTotal) ## renomeou a coluna LineTotal

ordered_sales <-
  ordered_sales %>%   
  arrange(desc(CumulativeSales)) ### organizou de forma decrescente

ordered_sales %>% 
  filter(Machine == "GuttenPlans x1367") %>% 
  head(5)
ordered_sales %>% 
  filter(Machine == "EB Public Library x1380")
ordered_sales %>% 
  filter(Machine == "BSQ Mall x1364 - Zales")
ordered_sales %>% 
  filter(Machine == "Earle Asphalt x1371")
ordered_sales %>% 
  filter(Machine == "BSQ Mall x1366 - ATT")


total_sales_date %>% 
  group_by(Machine) %>% 
  mutate(partSale = cumsum(LineTotal)) %>%  ## calculou a venda cumulativa dia a dia de cada maquina
  ggplot(aes(Prcd.Date , partSale))+
  geom_point(aes(colour=Machine))

test_sales_date <- 
  total_sales_date %>% 
  group_by(Machine) %>% 
  mutate(Prcd.Date = as.Date(Prcd.Date , "%m/%d/%y") ) %>% 
  arrange(Prcd.Date) %>% 
  mutate(cumulativeSale = cumsum(LineTotal))
  

