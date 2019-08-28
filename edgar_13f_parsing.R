# http://rankandfiled.com : edgar 시각화 사이트 
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
a <- getFilings('1067983','13F-HR',2019,downl.permit = 'n')
fn = list.files('Edgar filings_full text/Form 13F-HR/1067983',full.names = T)
fn1 = str_replace_all(fn,'.txt','.xml')
walk2(fn,fn1,~file.rename(.x,.y))
library(rvest)
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
# 
library(pacman)
p_load(tidyverse,edgar,rvest)
senti = getSentiment('1067983',form.type='10-K',filing.year = 2017:2019)
#
getMasterIndex(2019) # 2019에 생성된 edgar 전체 파일내역이 조회된다 (회사,fileform 등)
# 
load('Master Indexes/2019master.rda') # 생성된 결과는 여기에 저장되고 load시 year.master가 생성된다 
# year.master에서 찾고싶은 회사, 파일(13F-HR) 찾으면 된다 
a = year.master
dim(a) # 673871 6
names(a) # cik, company.name, form.type, date.filed, edgar.link, quarter
mutate(a,cname = tolower(company.name)) %>% 
  filter(str_detect(cname,'soros'),form.type=='13F-HR')
