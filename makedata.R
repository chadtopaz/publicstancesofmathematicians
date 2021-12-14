# Load regular libraries
library(tidyverse)
library(rvest)
library(readtext)
library(stringi)
library(pdftools)
library(NLP)
library(openNLP)
library(cleanNLP)
library(genTS)
library(pbmcapply)
library(refinr)
library(googledrive)
library(googlesheets4)

# spaCy stuff
# Make sure at least 4 GB RAM available for RStudio Cloud
# Otherwise installatin won't work
library(spacyr)
# spacy_install()


# DATA LAST SCRAPED ON DEC 12, 2021

# ###################
# # Get AMR members #
# ###################

page <- read_html("https://amathr.org/founding-members/")

origname <- page %>%
  html_nodes(".elementor-element-8c45a14 p") %>%
  as.character() %>%
  str_remove("<p><span style=\\\"color\\: blue;\\\">") %>%
  str_remove("</span>") %>%
  str_remove("<p>") %>%
  str_remove("</p>") %>%
  str_split("<br>") %>%
  sapply(function(x) x[1]) %>%
  str_squish()

affiliation <-  page %>%
  html_nodes(".elementor-element-8c45a14 p") %>%
  html_text() %>%
  str_remove(origname) %>%
  str_squish()

name <- origname %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\-"," ") %>%
  str_replace_all("\\.","") %>%
  tolower()

amrdata <- data.frame(origname=origname, name=name, affiliation=affiliation, what="AMR Member")

##############################
# Get AMS letter signatories #
##############################

amsletters <- readtext("amsletters.txt") %>%
  as.character() %>%
  str_replace_all("(?<=[:alpha:])\\.(?=[:alpha:])","") %>%
  str_squish()

# Set up data structures
amsdata <- data.frame(name=NULL,instituion=NULL,letternumber=NULL)
amslettertext <- NULL

# Letter 1
letternumber <- 1
beginning <- "I am writing regarding the article"
end <- "for her letter."
name <- "Blake Winter"
affiliation <- "Medaille College"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 2
letternumber <- 2
beginning <- "I am writing in support"
end <- "Well done!"
name <- "George E. Andrews"
affiliation <- "American Mathematical Society"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 3
letternumber <- 3
beginning <- "I applaud your running"
end <- "fresh and useful in the field."
name <- "Mark Saul"
affiliation <- "American Institute of Mathematics"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 4
letternumber <- 4
beginning <- "I applaud Professor Abby"
end <- c("education or social justice.")
name <- c("Hung-Hsi Wu")
affiliation <- "University of California Berkeley"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 5
letternumber <- 5
beginning <- "I am appalled"
end <- "editorial practices at the Notices."
name <- "Alejandro Chavez-Dominguez"
affiliation <- "University of Oklahoma"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 6 = Letter A
letternumber <- 6
beginning <- "We are a group of concerned"
end <- "community and identity."
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
name <- readtext("letterasignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("^.*?(?=,)") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
affiliation <- readtext("letterasignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("(?<=,).*$") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 7 = Letter B
letternumber <- 7
beginning <- "We write with grave concerns"
end <- "no place in our community."
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
name <- readtext("letterbsignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("^.*?(?=,)") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
affiliation <- readtext("letterbsignatories.txt") %>%
  as.character() %>%
  str_split("\\n") %>%
  unlist() %>%
  str_extract("(?<=,).*$") %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 8
letternumber <- 8
beginning1 <- "There is a false equivalence"
end1 <- "equity in their hiring practices."
beginning2 <- "Thompson has opted to politicize"
end2 <- "that of my employer."
name <- "Xander Faber"
affiliation <- "IDA/Center for Computing Sciences"
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsletters,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsletters,search2)
letter <- paste(letter1,letter2)
letter <- letter %>%
  str_replace("Tenney1","Tenney") %>%
  str_replace("bar\\.2","bar\\.") %>%
  str_replace("practices\\.3","practices\\.") %>%
  str_replace("AMS\\.4","AMS\\.") %>%
  str_replace("differently\\.”5","differently\\.”")
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 9
letternumber <- 9
beginning <- "Abigail Thompson’s article which appears in"
end <- "at our peril."
name <- "Terrence Blackman"
affiliation <- "Medgar Evars College, CUNY"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 10
letternumber <- 10
beginning <- "Thank you for publishing the article"
end <- "civilized discussion on this topic."
name <- c("Iosef Polterovich","Leonid Polterovich")
affiliation <- c("Universite de Montreal","Tel Aviv University")
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 11
letternumber <- 11
beginning <- "I would like to express my gratitude for your courageous"
end <- "support and admiration."
name <- "Mark Levi"
affiliation <- "Pennsylvania State University"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 12
letternumber <- 12
beginning <- "The heated debate"
end <- "stereotyped language!"
name <- "Valentin Ovsienko"
affiliation <- "CNRS"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 13
letternumber <- 13
beginning <- "In my opinion, diversity is an important"
end <- "on a regular basis."
name <- "Gil Kalai"
affiliation <- "Hebrew University of Jerusalem"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 14 = Letter C
letternumber <- 14
beginning <- "In an essay in the"
end <- "on this very important matter."
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
name <- readtext("lettercsignatories.txt") %>%
  as.character() %>%
  str_replace_all("\\n"," ") %>%
  str_split(",") %>%
  unlist() %>%
  stri_trans_general("Latin-ASCII") %>%
  str_squish()
affiliation <- rep(NA,length(name))
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 15
letternumber <- 15
beginning <- "I applaud Abigail Thompson for her thought-provoking and brave essay"
end <- "benefit mentioned above."
name <- "Abhishek Saha"
affiliation <- "Queen Mary University of London"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 16
letternumber <- 16
beginning <- "I am writing to express my strong support"
end <- "academics at large."
name <- "Victor Vianu"
affiliation <- "University of California San Diego"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 17
letternumber <- 17
beginning <- "I was saddened to see the reaction"
end <- "express diverse opinions."
name <- "Shachar Lovett"
affiliation <- "University of California San Diego"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 18
letternumber <- 18
beginning <- "I was delighted to see"
end <- "continue on your mission."
name <- "Yannis Papakonstantinou"
affiliation <- "University of California San Diego"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 19
letternumber <- 19
beginning <- "Universities that want to value diversity are requiring diversity statements"
end <- "Would Jean Bourgain\\?"
name <- "Svetlana Jitomirskaya"
affiliation <- "University of California Irvine"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 20
letternumber <- 20
beginning <- "I read with interest the letters section"
end <- "when those on the outside object."
name <- "Louigi Addario-Berry"
affiliation <- "McGill University"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 21
letternumber <- 21
beginning <- "The recent essay by Prof"
end <- "point to those flaws."
name <- "Juan Gutierrez"
affiliation <- "University of Texas San Antonio"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 22
letternumber <- 22
beginning1 <- "I am writing to commend Dr"
end1 <- "earlier article by Robert"
beginning2 <- "Shibley"
end2 <- "over a short period of 5 years."
name <- "Eleftherios Gkioulekas"
affiliation <- "University of Texas Rio Grande Valley"
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsletters,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsletters,search2)
letter <- paste(letter1,letter2)
letter <- letter %>%
  str_replace("editorial1","editorial") %>%
  str_replace("rubric2","rubric") %>%
  str_replace("Gilley\\.3","Gilley\\.") %>%
  str_replace("Shibley,4","Shibley,") %>%
  str_replace("book5","book") %>%
  str_replace("speech6","speech") %>%
  str_replace("affiliations7","affiliations")
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 23
letternumber <- 23
beginning1 <- "The UC system could streamline the vetting process on Diversity and Inclusion"
end1 <- "fewer generals."
beginning2 <- "Kudos to Dr. Thompson for having the courage"
end2 <- "budding Berias."
name <- "Hal Schenck"
affiliation <- "Auburn University"
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsletters,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsletters,search2)
letter <- paste(letter1,letter2)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Letter 24
letternumber <- 24
beginning <- "I joined the Mathematics Department at UC Davis in 1966"
end <- "shares her attitude."
name <- "Washek F. Pfeffer"
affiliation <- "University of California Davis"
search <- paste0(beginning,"(.|\\n)+",end)
letter <- str_extract(amsletters,search)
amslettertext <- c(amslettertext,letter)
amsdata <- rbind(amsdata,data.frame(name=name,affiliation=affiliation,letternumber=rep(letternumber,length(name))))

# Fix names
amsdata$origname <- amsdata$name
amsdata$name <- amsdata$origname %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\\-"," ") %>%
  str_replace_all("\\.","") %>%
  tolower()

# Fix one column and its data
names(amsdata)[names(amsdata)=="letternumber"] <- "what"
amsdata$newwhat <- as.character(amsdata$what)
amsdata <- amsdata %>%
  mutate(newwhat = replace(newwhat, newwhat == "6", "A")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "7", "B")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "14", "C")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "1", "D")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "2", "E")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "3", "F")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "4", "G")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "5", "H")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "8", "I")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "9", "J")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "10", "K")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "11", "L")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "12", "M")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "13", "N")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "15", "O")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "16", "P")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "17", "Q")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "18", "R")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "19", "S")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "20", "T")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "21", "U")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "22", "V")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "23", "W")) %>%
  mutate(newwhat = replace(newwhat, newwhat == "24", "X"))
amsdata$what <- amsdata$newwhat
amsdata$what <- paste("AMS Letter",amsdata$what)
amsdata <- amsdata %>% select(-newwhat)

# Get Thompson letter
amsessaytext <- pdf_text("https://www.ams.org/journals/notices/201911/rnoti-p1778.pdf") %>%
  as.character() %>%
  stri_trans_general("Latin-ASCII") %>%
  str_replace_all("\n"," ") %>%
  paste(collapse = " ") %>%
  str_squish()
beginning1 <- "This essay contains my opinions as an individual"
end1 <- "oaths of the 1950s"
beginning2 <- "were wrong\\. Whatever"
end2 <- "and not today\\."
search1 <- paste0(beginning1,"(.|\\n)+",end1)
letter1 <- str_extract(amsessaytext,search1)
search2 <- paste0(beginning2,"(.|\\n)+",end2)
letter2 <- str_extract(amsessaytext,search2)
amsessaytext <- paste(letter1,letter2)
abbyline <- data.frame(name="abigail thompson",origname="Abigail Thompson",affiliation="University of California Davis",what="AMS Essay")
amsdata <- rbind(amsdata,abbyline)
amsdata <- amsdata %>%
  mutate(affiliation = replace(affiliation, affiliation == "", NA)) %>%
  mutate(what = replace(what, what == "", NA))
 
# #####################################
# # Get California Letter Signatories #
# #####################################
# 
page <- read_html("https://www.independent.org/news/article.asp?id=13658")
# 
# origname <- page %>%
#   html_nodes("#signatories_style p") %>%
#   html_text() %>%
#   str_extract("^[^,]*(?=,)")
# 
# affiliation <-  page %>% 
#   html_nodes("#signatories_style p") %>% 
#   html_text() %>%
#   str_remove(paste0(origname,",")) %>%
#   str_squish()
# 
# name <- origname %>%
#   stri_trans_general("Latin-ASCII") %>%
#   str_replace_all("\\-"," ") %>%
#   str_replace_all("\\.","") %>%
#   tolower()
# 
# ca1data <- data.frame(origname=origname, name=name, affiliation=affiliation, what="California Letter 1")
# 
ca1lettertext <- page %>%
  html_nodes("li , p") %>%
  html_text() %>%
  paste(collapse="\n")
beginning <- "California is on the verge"
end <- "this framework rejects."
search <- paste0(beginning,"(.|\\n)+",end)
ca1lettertext <- str_extract(ca1lettertext,search) %>%
  iconv(to='ASCII//TRANSLIT', sub='') %>%
  str_replace_all("\n"," ") %>%
  str_squish()
# 
# ############################################
# # Get Second California Letter Signatories #
# ############################################
# 
page <- read_html("https://sites.google.com/view/k12mathmatters/home")
# 
# origname <- page %>%
#   html_nodes("strong") %>%
#   html_text()
# 
# affiliation <- page %>%
#   html_nodes("p") %>%
#   html_text() %>%
#   str_subset(".+")
# lastdrop <- which(affiliation=="(Affiliations are provided only for the purpose of identification)")
# affiliation <- affiliation %>%
#   tail(-lastdrop) %>%
#   head(-1) %>%
#   str_remove(paste0(origname,",")) %>%
#   str_squish()
# 
# name <- origname %>%
#   stri_trans_general("Latin-ASCII") %>%
#   str_replace_all("\\-"," ") %>%
#   str_replace_all("\\.","") %>%
#   tolower() %>%
#   str_squish()
# 
# ca2data <- data.frame(origname=origname, name=name, affiliation=affiliation, what="California Letter 2")
# 
ca2lettertext <- page %>%
  html_nodes("p") %>%
  html_text() %>%
  paste0(collapse=" ") %>%
  str_extract("We write to express.*needed for social mobility.") %>%
  iconv(to='ASCII//TRANSLIT', sub='') %>%
  str_squish()
# 
# ###########################
# # Combine Data And Export #
# ###########################
# 
# data <- rbind(amrdata,amsdata,ca1data,ca2data)
# write.csv(data,file="data.csv",row.names=FALSE)
# 
# ##############
# # Clean Data #
# ##############
# 
# # Normalize names
# # Done in openrefine
# 
# ###########################
# # Named Entity Extraction #
# ###########################
# 
# spacy_initialize()
# 
# affiliation2 <- data$affiliation %>%
#   str_replace_all("(^|[:blank:])([Uu][Cc][:blank:])","University of California ") %>%
#   str_replace_all("University of California at","University of California") %>%
#   str_replace_all("(^|[:blank:])([Uu][Tt][:blank:])","University of Texas ") %>%
#   str_replace_all("\\(|\\)","") %>%
#   str_squish()
# 
# prioritywords <- "univ|university|college"
# goodwords <- "institute|school|district|academy|inc|corp|(^|[:space:])nasa([:space:]|$)|ventures|suny|center|laboratory|"
# badwords <- "physics|electrical|engineering|mathematics|(computer science)|computer|medicine|(high school teacher)|economics|business|partner|scientist|(former elementary schl teacher)|(assistant principal)|(principal engineer)|(high school math teacher)|astronomy|mechanical|aerospace|(mathematical sciences)|civil"
# 
# getentity <- function(x){
#   x <- str_squish(gsub(badwords,"",x,ignore.case=TRUE))
#   tmp <- suppressMessages(tryCatch(
#     x %>%
#       spacy_extract_entity() %>%
#       select(text,ent_type),
#     error = function(e) NULL))
#   if (!any(tmp$ent_type == "ORG")) {
#     out <- NA
#   } else {
#     out <- suppressMessages(tryCatch(
#       tmp %>%
#         filter(ent_type %in% c("ORG","GPE")) %>%
#         filter(grepl(prioritywords,text,ignore.case=TRUE) | ent_type == "GPE") %>%
#         .[order(factor(.$ent_type, levels = c("ORG","GPE"))),] %>%
#         .$text %>%
#         paste(collapse = " "),
#       error = function(e) NULL))[1]
#   }
#   if (!is_empty(out)){
#     return(out)
#   }
#   if (!any(tmp$ent_type == "ORG")) {
#     out <- NA
#   } else {
#     out <- suppressMessages(tryCatch(
#       tmp %>%
#         filter(ent_type %in% c("ORG","GPE")) %>%
#         filter(grepl(goodwords,text,ignore.case=TRUE) | ent_type == "GPE") %>%
#         .[order(factor(.$ent_type, levels = c("ORG","GPE"))),] %>%
#         .$text %>%
#         paste(collapse = " "),
#       error = function(e) NULL))[1]
#   }
#   if (!is_empty(out)){
#     return(out)
#   }
#   out <- suppressMessages(tryCatch(
#     x %>%
#       spacy_extract_nounphrases() %>%
#       .$text %>%
#       str_subset(regex(prioritywords, ignore_case = TRUE)),
#     error = function(e) NULL))
#   if (!is_empty(out)){
#     return(out)
#   }
#   out <- suppressMessages(tryCatch(
#     x %>%
#       spacy_extract_nounphrases() %>%
#       .$text %>%
#       str_subset(regex(goodwords, ignore_case = TRUE)),
#     error = function(e) NULL))
#   if (!is_empty(out)){
#     return(out)
#   }
#   return(NA)
# }
# 
# affiliation3 <- pbmclapply(affiliation2,getentity)
# prob <- which(unlist(lapply(affiliation3, function(x) identical(x,c("College","Saint John's University")))))
# affiliation3[prob] <- "College of Saint Benedict and Saint John's University"
# 
# idx <- which(sapply(affiliation3,length)>1)
# affiliation4 <- affiliation3
# affiliation4[idx] <- NA
# affiliation4 <- unlist(affiliation4)
# 
# affiliation5 <- affiliation4 %>%
#   str_replace_all("University of California, ","University of California ") %>%
#   str_replace_all("^Williams$","Williams College") %>%
#   str_replace_all(" at Davis"," Davis")
# 
# probs <- c("Meta","College")
# affiliation6 <- affiliation5
# affiliation6[which(affiliation6 %in% probs)] <- NA

affiliation7 <- affiliation6 %>%
  iconv(to='ASCII//TRANSLIT', sub='')
affiliation7 <- str_squish(str_replace_all(affiliation7,",",""))
affiliation7 <- str_squish(str_replace_all(affiliation7,"-"," "))
affiliation7 <- gsub("^The ","",affiliation7)
idx <- which(affiliation6 == "University of Nebraska-Lincoln")
affiliation7[idx] <- "University of Nebraska Lincoln"
idx <- which(affiliation6 == "University of Wisconsin")
affiliation7[idx] <- "University of Wisconsin Madison"
idx <- which(grepl("concordia",data$affiliation,ignore.case = TRUE) & grepl("irvine",data$affiliation,ignore.case = TRUE))
affiliation7[idx] <- "Concordia University Irvine"
idx <- which(affiliation7 == "Engineer")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "UCSB")
affiliation7[idx] <- "University of California Santa Barbara"
idx <- which(affiliation7 == "USC")
affiliation7[idx] <- "University of Southern California"
idx <- which(affiliation7 == "California Institute of Technology National Academy of Sciences")
affiliation7[idx] <- "California Institute of Technology"
idx <- which(affiliation7 == "Astro")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "Biological")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "Cal Poly San Luis Obispo")
affiliation7[idx] <- "California State Polytechnic Uuniversity San Luis Obispo"
idx <- which(affiliation7 == "Cal Poly, San Luis Obispo")
affiliation7[idx] <- "California State Polytechnic Uuniversity San Luis Obispo"
idx <- which(affiliation7 == "California Polytechnic State University")
affiliation7[idx] <- "California State Polytechnic Uuniversity San Luis Obispo"
idx <- which(grepl("(St\\.|St|Saint) Mary",affiliation7,ignore.case=TRUE) & grepl("Maryland",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Saint Mary's College of Maryland"
idx <- which(grepl("(St\\.|St|Saint) Mary",affiliation7,ignore.case=TRUE) & grepl("California",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Saint Mary's College of California"
idx <- which(grepl("^Saint Mary's College of California$",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Saint Mary's College of California"
idx <- which(grepl("Professor Biology Emerita, Saint",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Saint Mary's College of California"
idx <- which(data$affiliation == "Saint Mary's College (IN)")
affiliation7[idx] <- "Saint Mary's College of Indiana"
idx <- which(grepl("rohatgi",data$name,ignore.case=TRUE) & grepl("Saint Mary",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Saint Mary's College of Indiana"
idx <- which(grepl("Technion",affiliation7))
affiliation7[idx] <- "Technion Israel Institute of Technology"
idx <- which(affiliation7 == "Stanford")
affiliation7[idx] <- "Stanford University"
idx <- which(affiliation7 == "UCSD")
affiliation7[idx] <- "University of California San Diego"
idx <- which(grepl("State University of New York at New Paltz",data$affiliation))
affiliation7[idx] <- "SUNY New Paltz"
idx <- which(grepl("State University of New York, Stony Brook",data$affiliation))
affiliation7[idx] <- "Stony Brook University"
idx <- which(affiliation7 == "The Johns Hopkins University")
affiliation7[idx] <- "Johns Hopkins University"
idx <- which(affiliation7 == "The Ohio State University")
affiliation7[idx] <- "Ohio State University"
idx <- which(grepl("University of Massachusetts Lowell",data$affiliation))
affiliation7[idx] <- "University of Massachusetts Lowell"
idx <- which(affiliation7 == "University of Massachusetts")
affiliation7[idx] <- "University of Massachusetts Amherst"
idx <- which(affiliation7 == "University of Massachussetts Boston")
affiliation7[idx] <- "University of Massachusetts Boston"
idx <- which(affiliation7 == "American Mathematical")
affiliation7[idx] <- NA
idx <- which(grepl("Amazon",affiliation7))
affiliation7[idx] <- "Amazon"
idx <- which(affiliation7 == "USF")
affiliation7[idx] <- "University of South Florida"
idx <- which(affiliation7 == "California")
affiliation7[idx] <- NA
idx <- which(grepl("Barnard College",affiliation7))
affiliation7[idx] <- "Barnard College"
idx <- which(data$affiliation == "University of Tennessee")
affiliation7[idx] <- "University of Tennessee Knoxville"
idx <- which(data$affiliation == "University of Tennessee, Knoxville")
affiliation7[idx] <- "University of Tennessee Knoxville"
idx <- which(data$affiliation == "University of Tennessee at Chattanooga")
affiliation7[idx] <- "University of Tennessee Chattanooga"
idx <- which(data$affiliation == "University of Tennessee at Martin")
affiliation7[idx] <- "University of Tennessee Martin"
idx <- which(grepl("Pinnacle 21",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Pinnacle 21"
idx <- which(affiliation7 == "University of Maryland")
affiliation7[idx] <- "University of Maryland College Park"
idx <- which(grepl("California Institute of Technology",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "California Institute of Technology"
idx <- which(affiliation7 == "Caltech")
affiliation7[idx] <- "California Institute of Technology"
idx <- which(affiliation7 == "DeAnza College")
affiliation7[idx] <- "De Anza Community College"
idx <- which(grepl("University of California Berkeley",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "University of California Berkeley"
idx <- which(affiliation7 == "Fundamental")
affiliation7[idx] <- NA
idx <- which(grepl("Google",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Google"
idx <- which(grepl("Gilead",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Gilead Sciences"
idx <- which(grepl("Microsoft",data$affiliation,ignore.case=TRUE) & grepl("UCSB",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of California Santa Barbara"
idx <- which(grepl("Microsoft",data$affiliation,ignore.case=TRUE) & grepl("UCSD",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of California San Diego"
idx <- which(grepl("Microsoft",data$affiliation,ignore.case=TRUE) & grepl("Massachusetts Institute of Technology",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Massachusetts Institute of Technology"
idx <- which(grepl("Microsoft",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Microsoft"
idx <- which(grepl("Indiana University",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Indiana University"
idx <- which(grepl("Intel ",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Intel"
idx <- which(grepl("^llc$",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- NA
idx <- which(grepl("Buffalo",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "SUNY Buffalo"
idx <- which(affiliation7 == "Univ")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "U.C. Berkeley")
affiliation7[idx] <- "University of California Berkeley"
idx <- which(grepl("Potsdam",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "SUNY Potsdam"
idx <- which(affiliation7 == "Software Developer")
affiliation7[idx] <- NA
idx <- which(grepl("Stanford",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Stanford University"
idx <- which(affiliation7 == "Moscow")
affiliation7[idx] <- "Steklov Mathematical Institute"
idx <- which(affiliation7 == "Professor of Statistics University of California")
idx <- which(data$affiliation == "Professor of Statistics, University of California, Davis")
affiliation7[idx] <- "University of California Davis"
idx <- which(data$affiliation == "Professor of Statistics, University of California, Irvine")
affiliation7[idx] <- "University of California Irvine"
idx <- which(affiliation7 == "Environmental University of California" & grepl("Davis",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of California Davis"
idx <- which(grepl("CUNY",affiliation7))
affiliation7[idx] <- "City University of New York"
idx <- which(grepl("City University of New York",affiliation7))
affiliation7[idx] <- "City University of New York"
idx <- which(grepl("City College of New York",affiliation7))
affiliation7[idx] <- "City University of New York"
idx <- which(affiliation7 == "MD")
affiliation7[idx] <- NA
idx <- which(grepl("IBM",affiliation7))
affiliation7[idx] <- "IBM"
idx <- which(grepl("RAND",affiliation7))
affiliation7[idx] <- "RAND Corporation"
idx <- which(grepl("University of Southern California",affiliation7))
affiliation7[idx] <- "University of Southern California"
idx <- which(affiliation7 == "UCLA")
affiliation7[idx] <- "University of California Los Angeles"
idx <- which(affiliation7 == "Cal Poly Pomona")
affiliation7[idx] <- "California State Polytechnic University Pomona"
idx <- which(affiliation7 == "Queens College")
affiliation7[idx] <- "City University of New York"
idx <- which(affiliation7 == "Westminster College UT")
affiliation7[idx] <- "Westminster College of Utah"
idx <- which(grepl("Hebrew University",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Hebrew University of Jerusalem"
idx <- which(affiliation7 == "University")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "MIT")
affiliation7[idx] <- "Massachusetts Institute of Technology"
idx <- which(affiliation7 == "The Univertsity of Texas Austin")
affiliation7[idx] <- "University of Texas Austin"
idx <- which(affiliation7 == "Georgia Tech")
affiliation7[idx] <- "Georgia Institute of Technology"
idx <- which(affiliation7 == "University of California")
affiliation7[idx] <- NA
idx <- which(grepl("Pomona",data$affiliation,ignore.case=TRUE) & affiliation7 == "California State Polytechnic University")
affiliation7[idx] <- "California State Polytechnic University Pomona"
idx <- which(grepl("Northridge",data$affiliation,ignore.case=TRUE) & affiliation7 == "California State University")
affiliation7[idx] <- "California State University Northridge"
idx <- which(grepl("East Bay",data$affiliation,ignore.case=TRUE) & affiliation7 == "California State University")
affiliation7[idx] <- "California State University East Bay"
idx <- which(affiliation7 == "City College San Francisco")
affiliation7[idx] <- "City College of San Francisco"
idx <- which(grepl("Penn State",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Pennsylvania State University"
idx <- which(affiliation7 == "Software Engineer")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "St Louis University")
affiliation7[idx] <- "Saint Louis University"
idx <- which(affiliation7 == "Washington University St Louis")
affiliation7[idx] <- "Washington University Saint Louis"
idx <- which(affiliation7 == "University of Washington Seattle")
affiliation7[idx] <- "University of Washington"
idx <- which(data$affiliation == "AI Researcher; PhD in Electrical Engineering, University of Washington")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "University of Illinois" & grepl("Urbana|Champaign",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of Illinois Urbana Champaign"
idx <- which(affiliation7 == "University of Illinois")
affiliation7[idx] <- "University of Illinois Urbana Champaign"
idx <- which(affiliation7 == "University of Illinois at Chicago")
affiliation7[idx] <- "University of Illinois Chicago"
idx <- which(affiliation7 == "Principal Data")
idx <- which(data$affiliation == "Principal Data Scientist, Salesforce")
affiliation7[idx] <- "Salesforce"
idx <- which(data$affiliation == "Principal Data Scientist")
affiliation7[idx] <- NA
idx <- which(affiliation7 == "State University of New York")
idx <- which(grepl("Albany School of Public Health",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "SUNY Albany"
idx <- which(grepl("Stony Brook; Fellow of APS, Fellow of IEEE",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Stony Brook University"
idx <- which(affiliation7 == "Stony Brook University")
affiliation7[idx] <- "SUNY Stony Brook"
idx <- which(affiliation7 == "University of Colorado Boulder")
affiliation7[idx] <- "University of Colorado"
idx <- which(affiliation7 == "University of Nebraska")
affiliation7[idx] <- "University of Nebraska Lincoln"
idx <- which(affiliation7 == "University of North Carolina")
affiliation7[idx] <- "University of North Carolina Chapel Hill"
idx <- which(affiliation7 == "University of Texas")
idx <- which(affiliation7 == "University of Texas" & data$affiliation =="University of Texas")
affiliation7[idx] <- "University of Texas Austin"
idx <- which(affiliation7 == "University of Texas" & grepl("Rio Grande",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of Texas Rio Grande Valley"
idx <- which(data$affiliation == "UT Austin")
affiliation7[idx] <- "University of Texas Austin"
idx <- which(grepl("university",affiliation7,ignore.case=TRUE) & grepl("texas",affiliation7,ignore.case=TRUE) & grepl("dallas",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "University of Texas Dallas"
idx <- which(affiliation7 == "Washington University St. Louis")
affiliation7[idx] <- "Washington University Saint Louis"
idx <- which(grepl("Weizmann Institute",affiliation7))
affiliation7[idx] <- "Weizmann Institute of Science"
idx <- which(affiliation7 == "St. Norbert College")
affiliation7[idx] <- "Saint Norbert College"
idx <- which(grepl("university",affiliation7,ignore.case=TRUE) & grepl("minnesota",affiliation7,ignore.case=TRUE) & grepl("school",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "University of Minnesota"
idx <- which(grepl("leidos",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "Leidos Corporation"
idx <- which(grepl("nvidia",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "NVIDIA Corporation"
idx <- which(grepl("Joseph",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Saint Joseph's University"
idx <- which(grepl("University of Hawai'i at Manoa",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of Hawaii Manoa"
idx <- which(grepl("Simons Center for Geometry",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "SUNY Stony Brook"
idx <- which(data$affiliation == "University of Chicago and Steklov Institute")
affiliation7[idx] <- "University of Chicago"
idx <- which(data$affiliation == "CIMAT")
affiliation7[idx] <- "Centro de Investigacion en Matematicas"
idx <- which(grepl("CNRS",data$affiliation,ignore.case="TRUE"))
affiliation7[idx] <- "CNRS"
idx <- which(data$affiliation == "American Institute of Mathematics")
affiliation7[idx] <- "American Institute of Mathematics"
idx <- which(data$affiliation == "NCWIT")
affiliation7[idx] <- "National Center for Women and Information Technology"
idx <- which(data$affiliation == "Sissa, Trieste, Italy")
affiliation7[idx] <- "SISSA International School for Advanced Studies"
idx <- which(data$affiliation == "Tri-Institutional MD-PhD Program")
affiliation7[idx] <- NA
idx <- which(data$affiliation == "The Hewitt School (NY)")
affiliation7[idx] <- "Hewitt School of New York"
idx <- which(data$affiliation == "University of Tubingen, Germany")
affiliation7[idx] <- "University of Tubingen"
idx <- which(data$affiliation == "Georg-August Universitat Gottingen")
affiliation7[idx] <- "University of Gottingen"
idx <- which(data$affiliation == "GVSU")
affiliation7[idx] <- "Grand Valley State University"
idx <- which(data$affiliation == "MIT Corporation")
affiliation7[idx] <- "Massachusetts Institute of Technology"
idx <- which(data$affiliation == "UC San Diego & San Diego State University")
affiliation7[idx] <- "University of California San Diego"
idx <- which(grepl("unaffiliated",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- NA
idx <- which(data$affiliation == "Millersville University (PA)")
affiliation7[idx] <- "Millersville University"
idx <- which(data$affiliation == "AHDI")
affiliation7[idx] <- "American Health Data Institute"
idx <- which(grepl("IUPUI",data$affiliation,ignore.case="TRUE"))
affiliation7[idx] <- "Indiana University Purdue University Indianapolis"
idx <- which(data$affiliation == "NC State University")
affiliation7[idx] <- "North Carolina State University"
idx <- which(data$affiliation == "Association for Women in Mathematics")
affiliation7[idx] <- "Association for Women in Mathematics"
idx <- which(data$affiliation == "Stonybrook University")
affiliation7[idx] <- "SUNY Stony Brook"
idx <- which(data$affiliation == "Moscow Institute of Physics and Technology")
affiliation7[idx] <- "Moscow Institute of Physics and Technology"
idx <- which(data$affiliation == "Universidad Complutense de Madrid")
affiliation7[idx] <- "Universidad Complutense de Madrid"
idx <- which(data$affiliation == "ETH, Zurich")
affiliation7[idx] <- "ETH Zurich"
idx <- which(data$affiliation == "Technische Universität Berlin")
affiliation7[idx] <- "TU Berlin"
idx <- which(data$affiliation == "Instituto Nacional de Matemática Pura e Aplicada")
affiliation7[idx] <- "Instituto Nacional de Matematica Pura e Aplicada"
idx <- which(grepl("NYU|Courant",affiliation7,ignore.case=TRUE))
affiliation7[idx] <- "New York University"
idx <- which(data$affiliation == "Washington and Lee University")
affiliation7[idx] <- "Washington and Lee University"
idx <- which(grepl("^Advanced Institute for Materials Research",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Advanced Institute for Materials Research"
idx <- which(grepl("^U Copenhagen",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "University of Copenhagen"
idx <- which(grepl("Sequoia",data$affiliation,ignore.case=TRUE))
affiliation7[idx] <- "Sequoia Capital"
idx <- which(affiliation7 == "NA" | affiliation7 == "na" | affiliation7 == "Na")
affiliation7[idx] <- NA

# tab <- sort(table(affiliation7),decreasing=TRUE)
# tab <- tab[tab > 1]
# dictionary <- sort(names(tab))

affiliation8 <- affiliation7
ignorestrings <- c("(^|[:blank])[Aa]([:blank]|$)","(^|[:blank])[Aa][Nn][Dd]([:blank]|$)","(^|[:blank])&([:blank]|$)","(^|[:blank])[Tt][Hh][Ee]([:blank]|$)","(^|[:blank])[Oo][Ff]([:blank]|$)")
affiliation8 <- key_collision_merge(affiliation8, ignore_strings = ignorestrings, bus_suffix = TRUE)

tmp <- data.frame(affiliation=data$affiliation,affiliation7=affiliation7,affiliation8=affiliation8)

# Fix stuff

idx <- which(grepl("University of California, Davis",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of California Davis"
idx <- which(grepl("University of California, Irvine",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of California Irvine"
idx <- which(grepl("École Polytechnique Fédérale de Lausanne \\(EPFL\\)",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Ecole Polytechnique Federale de Lausanne"
idx <- which(grepl("Math for America",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Math for America"
idx <- which(grepl("American Mathematical Society",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "American Mathematical Society"
idx <- which(grepl("Cal Poly, San Luis Obispo",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "California State Polytechnic University San Luis Obispo"
idx <- which(tmp$affiliation == "ICTP" & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "International Center for Theoretical Physics"
idx <- which(grepl("Google",tmp$affiliation,ignore.case=TRUE) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Google"
idx <- which(tmp$affiliation == "College of Charleston" & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "College of Charleston"
idx <- which(tmp$affiliation == "College of Central Florida" & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "College of Central Florida"
idx <- which(grepl("William (&|and) Mary",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "College of William and Mary"
idx <- which(tmp$affiliation == "University of Rochester" & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of Rochester"
idx <- which(tmp$affiliation == "IMJ-PRG, Universite Paris Diderot" & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "IMJ-PRG, Universite Paris Diderot"
idx <- which(tmp$affiliation == "U. Of Southern California" & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of Southern California"
idx <- which(grepl("Technion",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Technion Israel Institute of Technology"
idx <- which(grepl("CUNY",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "City University of New York"
idx <- which(grepl("WPI",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Worcester Polytechnic Institute"
idx <- which(grepl("TU Berlin",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "TU Berlin"
idx <- which(grepl("Scuola Normale Superiore, Italy",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Scuola Normale Superiore"
idx <- which(grepl("Hebrew Univ. of Jerusalem",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Hebrew University of Jerusalem"
idx <- which(grepl("Univ. of Cal., Berkeley",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of California Berkeley"
idx <- which(grepl("Lawrence Berkeley",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Lawrence Berkeley National Laboratry"
idx <- which(grepl("California State East Bay",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "California State University East Bay"
idx <- which(grepl("Navy",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "United States Navy"
idx <- which(grepl("Saint Mary",tmp$affiliation) & grepl("of California",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Saint Mary's College of California"
idx <- which(grepl("Santa Rosa",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "City of Santa Rosa"
idx <- which(grepl("University of California, Merced",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of California Merced"
idx <- which(grepl("Kavli Institute for Astrophysics and Space Research",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Massachusetts Institute of Technology"
idx <- which(grepl("San Antonio ISD",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "San Antonio Independent School District"
idx <- which(grepl("Honor",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Honor Senior Care"
idx <- which(grepl("LAUSD",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Los Angeles Unified School District"
idx <- which(grepl("UMA",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "UMA Decentralized Finance"
idx <- which(grepl("Iowa State",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Iowa State University"
idx <- which(grepl("Harvard",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Harvard University"
idx <- which(grepl("inside-higher-ed",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Inside Higher Ed"
idx <- which(grepl("Univ. of California, Davis",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "University of California Davis"
idx <- which(grepl("Cedars-Sinai",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "Cedars-Sinai Hospital"
idx <- which(grepl("LIGO",tmp$affiliation) & is.na(tmp$affiliation8))
tmp$affiliation8[idx] <- "California Institute of Technology"
idx <- which(grepl("RSA MIT",tmp$affiliation8))
tmp$affiliation8[idx] <- "Massachusetts Institute of Technology"
idx <- which(grepl("Fermi National Accelerator Theory Quantum Theory Department",tmp$affiliation8))
tmp$affiliation8[idx] <- "Fermi National Accelerator Laboratory"


companies <- c("Maplesoft","C.H. Robinson","Cisco","Seer","Roche Diagnostics",
               "Options for Youth","Design Therapeutics","Lochaber Cornwall",
               "INTEGRITY Security Services","Versant Ventures",
               "Denali Advisors","Linfield Christian, Temecula, California",
               "Home Instead Innovation Academy","Waymo","Pinterest","Riffyn",
               "Broadcom","Polished Pixels","Dolby Labs","Workday","Envista",
               "Quark Venture","DBO Partners","Keysight Technologies",
               "Hensel Phelps Construction","Liss Educational Consulting",
               "Aurora Innovation","MRM Consulting",
               "Omniscient Neurotechnology","Franklin Square Properties",
               "Autodesk","Payne Financial Consulting","Engineering10x",
               "Pendo.io","Threshold Ventures","College of Marin",
               "College of the Desert","El Dorado Union High School District",
               "Lawrence Livermore National Laboratory",
               "California Institute of Technology","Chastasy",
               "Los Angeles Unified School District","University of Southern California",
               "Famous Software","Palisades Charter High School",
               "CGTech","SAP","Adobe","Sutter Hill Ventures",
               "Panasonic","SUNY Maritime","Virginia Tech","LinkedIn",
               "Vertica","Robust Intelligence","AstraZeneca","Cold Spring Harbor Laboratory","Accenture","smileML",
               "Edgehog Trading","SUNY Oneonta","Patrick Ahearn Architecture",
               "Quora","Meta","Noteable","r2c","Massachusetts Institute of Technology",
               "Regalytics","Plume","Palantir Technologies","Iterative Scopes",
               "Onto Innovation","AECOM","Virginia Tech","University of Washington",
               "Forma Therapeutics","Robinhood","Red Sash","Permanente Medical Group",
               "Math Academy","Euler Circle","HighSage Ventures","Vesalius Therapeutics","Sanofi",
               "Akita Software","Tinder","SUNY Downstate","Bishop Verot Catholic High School","Chatstasy","Northrop Grumman")
for (company in companies){
  idx <- which(grepl(company,tmp$affiliation,ignore.case=TRUE) & is.na(tmp$affiliation8))
  tmp[idx,]$affiliation8 <- rep(company,length(idx))
}

tmp %>%
  filter(!is.na(affiliation) & is.na(affiliation8)) %>%
  view()

affiliation9 <- key_collision_merge(tmp$affiliation8, ignore_strings = ignorestrings, bus_suffix = TRUE)

data$origaffiliation <- data$affiliation
data$affiliation <- affiliation9


####################################

finaldata <- read.csv("finaldata.csv")

# Fix weird hidden chatacter issue
weirdspace <- str_sub(unique(finaldata$name[5]),7,7)
for (i in 1:ncol(finaldata)) {
  tmp <- as.character(finaldata[,i])
  tmp2 <- str_replace_all(tmp,weirdspace," ")
  finaldata[,i] <- tmp2
}
finaldata[finaldata=="na"] <- NA

# Check data
allnames <- unique(finaldata$name)
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  tmp <- ux[tab == max(tab)]
  ifelse(length(tmp)==1,tmp,NA)
}
out <- rep(NA,length(allnames))
for (i in 1:length(allnames)){
  thisname <- allnames[i]
  tmp <- finaldata %>% filter(name==thisname)
  if (nrow(tmp) == 1){
    out[i] <- TRUE
  }
  if (nrow(tmp) > 1) {
    if (all(is.na(tmp$affiliation))) {
      out[i] <- TRUE
    }
    tmp2 <- tmp[complete.cases(tmp),]
    if (length(unique(tmp2$affiliation)) == 1){
      good <- unique(tmp2$affiliation)
      finaldata <- finaldata %>%
        mutate(affiliation = replace(affiliation, name == thisname, good))
      out[i] <- TRUE
    }  
    if (length(unique(tmp2$affiliation)) > 1){
      good <- modes(tmp2$affiliation)
      finaldata <- finaldata %>%
        mutate(affiliation = replace(affiliation, name == thisname, good))
      out[i] <- ifelse(is.na(good),FALSE,TRUE)
    }  
  }
}

finaldata %>%
  select(affiliation) %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  view()

write.csv(finaldata,"finaldata.csv")

# Make nice table of information for stances
what <- unique(finaldata$what)
stances <- rep(NA,length(what))
stances[1] <- "https://amathr.org"
stances[2:25] <- amslettertext
stances[26] <- amsessaytext
stances[27] <- ca1lettertext
stances[28] <- ca2lettertext
stancecontent <- data.frame(what=what,content=stances)
stancecontent <- stancecontent[c(1,26,7,8,15,2:6,9:14,16:25,27:28),]
write.csv(stancecontent,"stancecontent.csv")