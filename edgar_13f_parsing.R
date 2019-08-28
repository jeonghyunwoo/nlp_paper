library(tidyverse)
library(edgar)
a <- getFilings('1067983','13F-HR',2019,downl.permit = 'n')
y
filter(year.master,str_detect(form.type,'13F'),str_detect(tolower(company.name),'berkshire hathaway inc'))
library(XML)
fn = list.files('Edgar filings_full text/Form 13F-HR/1067983',
                full.names=T)
b <- getFilingsHTML(1067983,'13F-HR',2019)
info<-getFilingInfo(1067983,2019)
names(info)
library(rvest)
a = read_html('data/13f.xml')
a %>% html_nodes('infotable') %>% 
  html_children()

# nameofissuer
# titleofclass
# cusip
# value
# sshprnamt
# sshprnamttype
# investmentdiscretion
# othermanager
# votingauthority
fn = list.files('Edgar filings_full text/Form 13F-HR/1067983',full.names = T)
fn1 = str_replace_all(fn,'.txt','.xml')
walk2(fn,fn1,~file.rename(.x,.y))
info = read_html(fn1[3]) %>% 
  html_nodes('infotable')
# issuer = html_nodes(info,'nameofissuer') %>% html_text()
nodes = c('nameofissuer','titleofclass','cusip','value',
          'sshprnamt','sshprnamttype','investmentdiscretion',
          'othermanager','votingauthority sole')
infod = map_dfc(nodes,~html_nodes(info,.x) %>% html_text()) %>% 
  set_names(nodes) %>% 
  mutate_at(vars('value','sshprnamt'),as.numeric)

# count(infod,titleofclass)
# count(infod,nameofissuer)
filter(infod,titleofclass=='COM') %>% 
  group_by(nameofissuer) %>% 
  summarise(value = sum(value)/100000) %>% # $1억
  ggplot(aes(reorder(nameofissuer,desc(value)),value))+
  geom_col(fill='steelblue')+
  scale_y_continuous(labels = function(x) x/10000)+
  theme(axis.text.x = element_text(angle=45,hjust=1))
