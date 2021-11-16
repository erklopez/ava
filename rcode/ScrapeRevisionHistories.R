#GOAL: Scrape the ECFR site for the names and revision histories for the AVAs.

#Libraries
library(rvest)
library(xml2)
library(plyr)
 
#Load Data

#   Website
url<-"https://www.ecfr.gov/api/renderer/v1/content/enhanced/2021-11-08/title-27?chapter=I&part=9&subchapter=A&subpart=C"

#   Scrape website

ecfr<-read_html(url)


# New code  ---------------------------------------------------------------

# h8<- html_nodes(ecfr, "h8") #extracting all h8 nodes which contains the information of the viticulture areas
# ^ not useful, the nodes only contain the title and not the data underneat it

sections<- xml_find_all(ecfr, "//*[@class = 'section']") #Extracting the sections that contain all the info of a single viticulture area

names<-sections%>%html_nodes("h8")%>%html_text() # Extracting names from the sectionsby extracting headings (h8) and then extracting the text inside the attributes

names<-names[-1]%>%as.data.frame()#Subtracting the General Section which will not be needed

revision.string<-xml_find_all(sections, "//*[@class = 'citation']")%>%html_text()

revision.string<-as.data.frame(revision.string)




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
