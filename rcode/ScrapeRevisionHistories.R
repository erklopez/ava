#GOAL: Scrape the ECFR site for the names and revision histories for the AVAs.

#Libraries
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
 
#Load Data

#   Website
url<-"https://www.ecfr.gov/api/renderer/v1/content/enhanced/2021-12-31/title-27?chapter=I&part=9&subchapter=A&subpart=C"
#   Scrape website

ecfr<-read_html(url)


# New code  ---------------------------------------------------------------

# Names
sections<- xml_find_all(ecfr, "//*[@class = 'section']") #Extracting the sections that contain all the info of a single viticulture area
names<-sections%>%html_nodes("h8")%>%html_text() # Extracting names from the sectionsby extracting headings (h8) and then extracting the text inside the attributes
names<-names[-c(1,260,261)]%>%as.data.frame()#Subtracting the General Section which will not be needed

# Separating section and names.
# section<-substr(names,1,6)
# section<-trimws(section, which="both", whitespace = "[\\h\\v]")
# ava.name<-substr(names, 7, 100)
# ava.name<-gsub("\\.", "", ava.name)
# ava.name<-trimws(ava.name, which="both", whitespace = "[\\h\\v]")

# Revision Strings
revision.string<-xml_find_all(sections, "//*[@class = 'citation']|//*[@class = 'section-authority']")%>%html_text() #grabs the text from dividers with classes
# citation or section-authority because 9.127 has an error on the html that makes it have no classs instead of the class citation.
revision.string<-as.data.frame(revision.string) # Makes the revision strings into a df


ava_notclean_df<-cbind.data.frame(names, revision.string) # Binds the names and revision.strings into a single df
colnames(ava_notclean_df)<- c("names","revision.string")

# Cleaning ava_notclean_df

names_sections<- names %>% mutate( section = str_replace(names$., "^(.+ .+) (.+ .+)$", "\\1"),
                                   name = str_replace(names$., "^(.+ .+) (.+ .+)$", "\\2"), )

# names_sections <- names %>%
#   separate(col = names$., into=c("sec","section","name1","name2", "name3", "name4", "name5", "name6", "name7"),sep= " ") %>%
#   unite(col = "section",sec, section, sep=" ") %>%
#   unite(col = "name", name1,name2, name3, name4, name5, name6, name7, sep=" ") 
# 
# for (i in names){
#   if (str_count(i, " ") == 2){
#     section = str_replace(names$.[i], "^(.+) (.+)$", "\\1")
#     name = str_replace(names$.[i], "^(.+) (.+)$", "\\2") 
#   }
#   
# }


# Old Code ----------------------------------------------------------------


#Get all of the H2 tags
h2<-html_nodes(ecfr, "h2")
#ava.name<-h2[grep("§", h2)]
ava.name<-h2[grep("style", h2)]
#ava.list<-substr(ava.name, regexpr("§",ava.name), nchar(ava.name)-1)
ava.list<-as_list(ava.name)
ava.df<-data.frame(matrix(unlist(ava.list)), stringsAsFactors = FALSE)
avas<-ava.df[-1,]

section<-substr(avas,1,6)
section<-trimws(section, which="both", whitespace = "[\\h\\v]")
ava.name<-substr(avas, 7, 100)
ava.name<-gsub("\\.", "", ava.name)
ava.name<-trimws(ava.name, which="both", whitespace = "[\\h\\v]")

#This doesn't work because one of the revision history strings has the wrong class assigned to it
# revision.history<-ecfr %>% 
#   html_nodes('[class="cita"]')
# revision.list<-as_list(revision.history)
# revision.df<-data.frame(matrix(unlist(revision.list)))

#Get the nodes with ' FR ' in them because all the revision histories have this
#   Not all are correctly formed so you can't get them based on the first part of the string
try.fr<-xml_find_all(ecfr, "//text()[contains(., ' FR ')]")
fr.list<-as_list(try.fr)
fr.df<-data.frame(matrix(unlist(fr.list)), stringsAsFactors = FALSE)
fr<-fr.df[,1]

#split up the revision string into it's component citations
fr.standardized<-gsub(", as amended by", ";", fr)
fr.pieces<-strsplit(fr.standardized, ";")
#fr.matrix<-do.call(rbind, fr.pieces)
#rbindfill
fr.df<-rbind.fill(lapply(fr.pieces, function(y){as.data.frame(t(y), stringsAsFacfors=FALSE)}))


all.data<-as.data.frame(cbind(section, ava.name, fr, fr.df), stringsAsFactors = FALSE)
names(all.data)[3]<-"revision.string"

#write.csv(ava.df, "d:\\avaname.csv")
write.csv(all.data, "d:\\revisionhistories.csv")
