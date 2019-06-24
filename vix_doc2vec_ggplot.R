p_load(lubridate)
df2 = read_csv('data/df2.csv',col_types = list(ymd=col_character())) %>% 
  select(-X1) %>% 
  mutate(date = ymd(ymd))
theme_set(theme_minimal(10))
select(df2,date,dv0:dv99) %>% 
  gather(key,value,-date) %>% 
  ggplot(aes(date,value,color=key))+
  geom_line()+
  geom_point()+
  theme(legend.position = 'none')
vix = read_csv('data/vix.csv')
head(vix)
lct<-Sys.getlocale('LC_TIME')
Sys.setlocale('LC_TIME','C')
vix = vix %>% 
  mutate(date = as.Date(Date,format='%b %d, %Y'))
vix
Sys.setlocale('LC_TIME',lct)
p1=ggplot(vix,aes(date,Price))+
  geom_line()+
  geom_point()
vix1 = vix %>% 
  left_join(select(df2,date,dv0:dv99),by='date')
vix1
p2=vix1 %>% 
  select(date,dv0:dv99) %>% 
  gather(key,value,-date) %>% 
  ggplot(aes(date,value,color=key))+
  geom_line()+
  geom_point()+
  theme(legend.position = 'none')
# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
# library(patchwork)
p1/p2
