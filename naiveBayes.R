library(tm,,quietly = TRUE)
library(wordcloud,,quietly = TRUE)
library(e1071,,quietly = TRUE)
library(dplyr,,quietly = TRUE)
library(caret,,quietly = TRUE)
library(doMC,,quietly = TRUE)
library(xml2,,quietly = TRUE)
library(rvest,,quietly = TRUE)


##wget -m URL --wait=2
registerDoMC(cores=detectCores())
##find nondependent variaables 
##look at subject, look at bill number, look at author party, author district number ,legislative party leader, party govenor, governor party agenda, seperate appropriation, once in another part of the process, check probability 

subjects_list=read.csv("~/Desktop/onefourty/bill_analysis_r/subjects.csv")

##http://www.lrl.state.tx.us/legeLeaders/members/partyListSession.cfm?leg=84
party_url="http://www.lrl.state.tx.us/legeLeaders/members/partyListSession.cfm?leg=84"
dems=party_url %>%
  read_html() %>%html_nodes(xpath="//table[@summary='Democrats']/tr[2]/td/a") %>% html_text()
repubs =party_url %>%
  read_html() %>%html_nodes(xpath="//table[@summary='Republicans']/tr[2]/td/a") %>% html_text()
##<U+00F1> = n 
##<U+00E1> = a
##<U+00ED> = i
##<U+00E9> = e


repub_id=party_url %>%
  read_html() %>%html_nodes(xpath="//table[@summary='Republicans']/tr[2]/td/a") %>% html_attr("href")
dems_id = party_url %>%
  read_html() %>%html_nodes(xpath="//table[@summary='Democrats']/tr[2]/td/a") %>% html_attr("href")

dems=cbind(dems_id,dems,"D")
repubs=cbind(repub_id,repubs,"R")

all_legislators=rbind(dems,repubs)
all_legislators=as.data.frame(all_legislators)
names(all_legislators)=c("id","name","party")
all_legislators$id=gsub("memberDisplay.cfm\\?memberID=","",all_legislators$id)
all_legislators$id=as.numeric(all_legislators$id)
all_legislators$name=as.character(all_legislators$name)

##district numbers
for(i in 1:nrow(all_legislators)){
	member_districts = paste0("http://www.lrl.state.tx.us/legeLeaders/members/memberDisplay.cfm?memberID=",all_legislators$id[i]) %>%
	read_html() %>%html_nodes(xpath='//table[2]/tr[2]/td/table') %>% html_table(header=T)
	member_districts[[1]]$Legislatures=gsub(" .*","",member_districts[[1]]$Legislatures)
	member_districts[[1]]$Legislatures=gsub("[A-z]+","",member_districts[[1]]$Legislatures)
	district_number=member_districts[[1]][member_districts[[1]]$Legislatures=="84",2]
	all_legislators[i,4]= district_number
}
names(all_legislators)[4]="district_number"



##if one name, get party affliation, if multiple check either bipartisian


transformations = function(corpus){
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeWords, stopwords("english"))
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeNumbers)
    corpus = tm_map(corpus, removeWords, "a5")
    corpus = tm_map(corpus, removeWords, "a7")
    corpus = tm_map(corpus, removeWords, "")
    corpus = tm_map(corpus, stripWhitespace)
    corpus = tm_map(corpus, stemDocument,language="english")
    return(corpus)
}

invisible(Sys.setlocale(locale = "C"))
setwd("Desktop/84 bills/ftp.legis.state.tx.us/bills/84R/billtext/pdf/house_bills/")
files = list.files(pattern = "pdf$",recursive = TRUE)
enrolled=files[grep("F",files)]
##f is enrolled 
## i is introduced
introduced=files[grep("I",files)]

other_dtm=function(introduced,legislative_session,subjects,all_legislators){
	int_xml = gsub("\\/SB0000","/SB ",introduced)
	int_xml = gsub("\\/SB000","/SB ",int_xml)
	int_xml = gsub("\\/SB00","/SB ",int_xml)
	int_xml = gsub("\\/SB0","/SB ",int_xml)
	int_xml = gsub("[A-z]\\.pdf",".xml",int_xml)
	final_list=list()
	for(i in 1:length(int_xml)){
		data <- read_xml(paste0("/Users/christophvel/Desktop/84 bills/ftp.legis.state.tx.us/bills/",legislative_session,"/billhistory/senate_bills/",int_xml[i]))
		subjects <- data %>% xml_find_all("//subject") %>%  xml_text()
		coauthors <- data %>% xml_find_all("//coauthors") %>%  xml_text()
		authors <- data %>% xml_find_all("//authors") %>%  xml_text()
		authors = strsplit(authors," | ",fixed=T)
		coauthors = strsplit(coauthors," | ",fixed=T)
		authors=as.data.frame(authors)
		coauthors=as.data.frame(coauthors)
		names(authors) = "names"
		names(coauthors) = "names"
		bill_info=list(int_xml[i],authors,coauthors,subjects)
		final_list[[int_xml[i]]]=bill_info
	}
	df_total = data.frame()

	for(i in 1:length(final_list)){
		xtest=match(subjects_list[,1],final_list[[i]][[4]])
		xtest[is.na(xtest)]=0
		xtest[xtest>0]=1
		subjects=as.data.frame(t(xtest))
		df_total=rbind(df_total,subjects)

	}
	for(i in 1:length(final_list)){
		final_list[[i]][[2]][,1]=as.character(final_list[[i]][[2]][,1])
		final_list[[i]][[2]][1,1] = gsub("\\\"Mando\\\"","Armando",final_list[[i]][[2]][1,1])
		final_list[[i]][[2]][1,1] = gsub("Allen","Allen, Alma",final_list[[i]][[2]][1,1])
		final_list[[i]][[2]][1,1] = gsub("\\\"Doc\\\"","'Doc'",final_list[[i]][[2]][1,1])
	}
	tester=lapply(final_list,function(x){as.character(x[[2]][1,])})
	tester=as.data.frame(tester)
	tester=t(tester)
	x=strsplit(tester,", ")


	all_legislators$name=gsub("Lucio, III","Lucio III",all_legislators$name)
	all_legislators = rbind(all_legislators,c("NA","Bernal","D","NA"))
	all_legislators = rbind(all_legislators,c("NA","Cyrier","R","NA"))
	all_legislators = rbind(all_legislators,c("NA","Schubert","R","NA"))
	df_party_total = data.frame()
	for(i in 1:length(x)){
		party=all_legislators[grep(paste0(as.character(x[[i]][1]),"$"),all_legislators$name),3]
		if(length(party)>1){
			party=all_legislators[grep(as.character(x[[i]][2]),all_legislators$name),3]
			if(length(party)>1){
				party=all_legislators[grepl(as.character(x[[i]][1]),all_legislators$name) & grepl(as.character(x[[i]][2]),all_legislators$name),3] 
			}
		}
		if(length(party)==0){
			party=all_legislators[grep(paste0(as.character(x[[i]][1])),all_legislators$name),3]
			if(length(party)==0){
				party="NA"
			}
		}
		party=as.character(party)
		df_party_total[i,1]=party
	}
	df_total=as.data.frame(df_total)
	names(df_total)=as.character(subjects_list[,1])
	names(df_party_total)="party_affliation"
	return(cbind(df_total,df_party_total))
}

all_subjects_party=other_dtm(introduced,"84R",subjects_list,all_legislators)


enrolledfile=gsub("SB[0-9]+\\_SB[0-9]+\\/","",enrolled)
enrollednumber=gsub("F.pdf","", enrolledfile)

introducedfiles=gsub("SB[0-9]+\\_SB[0-9]+\\/","",introduced)
introducednumber=gsub("[A-z].pdf","", introducedfiles)


intro=as.data.frame(cbind(introduced,introducednumber))

intro[(intro$introducednumber %in% enrollednumber),3]="E"
intro[!(intro$introducednumber %in% enrollednumber),3]="N"
names(intro)[3]="enrolled"
intro$enrolled <- as.factor(intro$enrolled)
intro$introduced = as.character(intro$introduced)

intro=cbind(intro,all_subjects_party)

Rpdf = readPDF(control = list(text = "-layout"))
bills = Corpus(URISource(intro$introduced),
                  readerControl = list(reader = Rpdf))

bills = transformations(bills)
bills = bills[!is.na(bills)]
dtm = DocumentTermMatrix(bills)
fivefreq <- findFreqTerms(dtm, 100)
fivefreq=fivefreq[!fivefreq=="bill"]
fivefreq=fivefreq[!fivefreq=="enact"]
fivefreq=fivefreq[!fivefreq=="<a7>"]
fivefreq=fivefreq[!fivefreq=="aaa"]
fivefreq=fivefreq[!fivefreq=="aaan"]
fivefreq=fivefreq[!fivefreq=="texa"]
fivefreq=fivefreq[!fivefreq=="state"]
fivefreq=fivefreq[!fivefreq=="relat"]
fivefreq=fivefreq[!fivefreq=="entitl"]
fivefreq=fivefreq[!fivefreq=="act"]
fivefreq=fivefreq[!fivefreq=="subtot"]
fivefreq=fivefreq[!fivefreq=="hbanoa"]
fivefreq=fivefreq[!fivefreq=="albb"]
fivefreq=fivefreq[!fivefreq=="proc"]
fivefreq=fivefreq[!fivefreq=="crim"]
##take out enact, bill
dtm <- DocumentTermMatrix(bills, control=list(dictionary = fivefreq))


m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

set.seed(1)
smp_size <- floor(0.85 * nrow(intro))
train_ind <- sample(seq_len(nrow(intro)), size = smp_size)


intro.train <- intro[train_ind,]
intro.test <- intro[-train_ind,]

dtm.e = dtm [intro$enrolled=="E",]
dtm.e = dtm[-1,]

dtm.train <- dtm[train_ind,]
dtm.test <- dtm[-train_ind,]

corpus.clean.train <- bills[train_ind]
corpus.clean.test <- bills[-train_ind]


subject.train=intro.train[,4:(ncol(intro.train)-1)]
goodones=colSums(subject.train)>0
subject.train=subject.train[,goodones]
subject.test=intro.test[,4:ncol(intro.test)]
subject.test=subject.test[,goodones]


trainNB.subject <- apply(subject.train, 2, convert_count)
testNB.subject <- apply(subject.test, 2, convert_count)

trainNB <- apply(dtm.train, 2, convert_count)
testNB <- apply(dtm.test, 2, convert_count)


trainNB=cbind(trainNB,trainNB.subject,intro.train$party_affliation)
testNB=cbind(testNB,testNB.subject,intro.test$party_affliation)


system.time( classifier <- naiveBayes(trainNB, intro.train$enrolled, laplace = 1) )
system.time( pred <- predict(classifier, newdata=testNB) )

table("Predictions"= pred,  "Actual" = intro.test$enrolled )
conf.mat <- confusionMatrix(pred, intro.test$enrolled)
conf.mat$overall['Accuracy']



setwd("~/Desktop/84 bills/ftp.legis.state.tx.us/bills/85R/billtext/pdf/senate_bills/")

files_85 = list.files(pattern = "pdf$",recursive = TRUE)
introduced_85=files_85[grep("I",files_85)]
tester_corpus=Corpus(URISource(introduced_85),
                  readerControl = list(reader = Rpdf))

tester_corpus = transformations(tester_corpus)
tester_dtm <- DocumentTermMatrix(tester_corpus, control=list(dictionary = fivefreq))


m <- as.matrix(tester_dtm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
other_dtm=function(introduced,legislative_session,subjects,all_legislators){
	int_xml = gsub("\\/SB0000","/SB ",introduced_85)
	int_xml = gsub("\\/SB000","/SB ",int_xml)
	int_xml = gsub("\\/SB00","/SB ",int_xml)
	int_xml = gsub("\\/SB0","/SB ",int_xml)
	int_xml = gsub("[A-z]\\.pdf",".xml",int_xml)
	final_list=list()
	for(i in 1:length(int_xml)){
		data <- read_xml(paste0("/Users/christophvel/Desktop/84 bills/ftp.legis.state.tx.us/bills/","85R","/billhistory/senate_bills/",int_xml[i]))
		subjects <- data %>% xml_find_all("//subject") %>%  xml_text()
		coauthors <- data %>% xml_find_all("//coauthors") %>%  xml_text()
		authors <- data %>% xml_find_all("//authors") %>%  xml_text()
		authors = strsplit(authors," | ",fixed=T)
		coauthors = strsplit(coauthors," | ",fixed=T)
		authors=as.data.frame(authors)
		coauthors=as.data.frame(coauthors)
		names(authors) = "names"
		names(coauthors) = "names"
		bill_info=list(int_xml[i],authors,coauthors,subjects)
		final_list[[int_xml[i]]]=bill_info
	}
	df_total = data.frame()

	for(i in 1:length(final_list)){
		xtest=match(subjects_list[,1],final_list[[i]][[4]])
		xtest[is.na(xtest)]=0
		xtest[xtest>0]=1
		subjects=as.data.frame(t(xtest))
		df_total=rbind(df_total,subjects)

	}
	for(i in 1:length(final_list)){
		final_list[[i]][[2]][,1]=as.character(final_list[[i]][[2]][,1])
		final_list[[i]][[2]][1,1] = gsub("\\\"Mando\\\"","Armando",final_list[[i]][[2]][1,1])
		final_list[[i]][[2]][1,1] = gsub("Allen","Allen, Alma",final_list[[i]][[2]][1,1])
		final_list[[i]][[2]][1,1] = gsub("\\\"Doc\\\"","'Doc'",final_list[[i]][[2]][1,1])
	}
	tester=lapply(final_list,function(x){as.character(x[[2]][1,])})
	tester=as.data.frame(tester)
	tester=t(tester)
	x=strsplit(tester,", ")


	all_legislators$name=gsub("Lucio, III","Lucio III",all_legislators$name)
	df_party_total = data.frame()
	for(i in 1:length(x)){
		party=all_legislators[grep(paste0(as.character(x[[i]][1]),"$"),all_legislators$name),3]
		if(length(party)>1){
			party=all_legislators[grep(as.character(x[[i]][2]),all_legislators$name),3]
			if(length(party)>1){
				party=all_legislators[grepl(as.character(x[[i]][1]),all_legislators$name) & grepl(as.character(x[[i]][2]),all_legislators$name),3] 
			}
		}
		if(length(party)==0){
			party=all_legislators[grep(paste0(as.character(x[[i]][1])),all_legislators$name),3]
			if(length(party)==0){
				party="NA"
			}
		}
		party=as.character(party)
		df_party_total[i,1]=party
	}
	df_total=as.data.frame(df_total)
	names(df_total)=as.character(subjects_list[,1])
	names(df_party_total)="party_affliation"
	return(cbind(df_total,df_party_total))
}
tester_subjects=other_dtm(introduced_85,"85R",subjects,all_legislators)



subject.tester=tester_subjects[,1:(ncol(tester_subjects)-1)]
subject.tester=subject.tester[,goodones]
testerNB.subject <- apply(subject.tester, 2, convert_count)
testerNB <- apply(tester_dtm, 2, convert_count)
testerNB=cbind(testerNB,testerNB.subject,tester_subjects$party_affliation)
system.time( pred_tester <- predict(classifier, newdata=testerNB) )
system.time( pred_tester_raw <- predict(classifier, newdata=testerNB,type="raw") )
int_xml = gsub("\\/SB0000","/SB ",introduced_85)
int_xml = gsub("\\/SB000","/SB ",int_xml)
int_xml = gsub("\\/SB00","/SB ",int_xml)
int_xml = gsub("\\/SB0","/SB ",int_xml)
int_xml = gsub("[A-z]\\.pdf",".xml",int_xml)
introducedfiles_85=gsub("SB[0-9]+\\_SB[0-9]+\\/","",introduced_85)
introducednumber_85=gsub("[A-z].pdf","", introducedfiles_85)
introducednumber_85 = gsub("SB0000","SB ",introducednumber_85)
introducednumber_85 = gsub("SB000","SB ",introducednumber_85)
introducednumber_85 = gsub("SB00","SB ",introducednumber_85)
introducednumber_85 = gsub("SB0","SB ",introducednumber_85)



final_list=list()
for(i in 1:length(int_xml)){
	data <- read_xml(paste0("/Users/christophvel/Desktop/84 bills/ftp.legis.state.tx.us/bills/","85R","/billhistory/senate_bills/",int_xml[i]))
	subjects <- data %>% xml_find_all("//subject") %>%  xml_text()
	coauthors <- data %>% xml_find_all("//coauthors") %>%  xml_text()
	authors <- data %>% xml_find_all("//authors") %>%  xml_text()
	caption <- data %>% xml_find_all("//caption") %>%  xml_text()
	last_action <- data %>% xml_find_all("//lastaction") %>%  xml_text()
	authors = strsplit(authors," | ",fixed=T)
	coauthors = strsplit(coauthors," | ",fixed=T)
	authors=as.data.frame(authors)
	coauthors=as.data.frame(coauthors)
	names(authors) = "names"
	names(coauthors) = "names"
	bill_info=list(int_xml[i],authors,coauthors,subjects,caption,last_action,tester_subjects$party_affliation[i],pred_tester[i],pred_tester_raw[i,],introduced_85[i],introducednumber_85[i])
	final_list[[int_xml[i]]]=bill_info
}




##final_list, subject.tester, wordcloud of dtm 
##add to list 
##unicode 

final_list_s=final_list
subject.tester_s=subject.tester
tester_dtm_s=tester_dtm
load("ftp.legis.state.tx.us/bills/85R/billtext/pdf/house_bills/final_list_subjects_dtm.RData")


index=which(sapply(final_list, FUN=function(X) "HB 220" %in% X))
final_list[[index]]
index_dtm=tester_dtm[index,]

colSums(subject.tester[,subject.tester[index,]==1])

m <- as.matrix(index_dtm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
##like subjects

##get counts for each subject for each party, and each person 
##graph this for each one 


## do tfxidf
dtm_tfxidf <- weightTfIdf(tester_dtm)

## do document clustering

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
m<-cbind(m,subject.tester)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)


### cluster into 10 clusters
cl <- kmeans(m_norm, 10)
cl



dtm_tfxidf <- weightTfIdf(tester_dtm)
dtm_tfxidf <- as.matrix(dtm_tfxidf)
dtm_tfxidf<-cbind(dtm_tfxidf,subject.tester)
rownames(dtm_tfxidf) <- 1:nrow(dtm_tfxidf)
dtm_tfxidf<-as.matrix(dtm_tfxidf)
yes=matrix()
i=1
for(j in 1:nrow(dtm_tfxidf)){
	xx2=crossprod(dtm_tfxidf[i,],dtm_tfxidf[j,])/sqrt(crossprod(dtm_tfxidf[i,]) * crossprod(dtm_tfxidf[j,]))
	yes=rbind(yes,xx2)
	}
whichpart <- function(x, n=30) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}

indices=whichpart(yes,n=7)
indices=indices-1
similar_doc_indices=indices[!indices==i]
##take out indice


