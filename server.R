library(tm,quietly = TRUE)
library(wordcloud,quietly = TRUE)
library(e1071,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(caret,quietly = TRUE)
library(doMC,quietly = TRUE)
library(xml2,quietly = TRUE)
library(rvest,quietly = TRUE)
library(shiny,quietly = TRUE)
library(shinythemes,quietly = TRUE)
library(shinysky,quietly = TRUE)
library(ggthemes,quietly = TRUE)
library(plotly,quietly = TRUE)
library(DT,quietly = TRUE)

##objects from naiveBayes.R
##=========================
load("final_list_subjects_dtm.RData")
load("senate.RData")

##cosine function
##=========================
whichpart <- function(x, n=30) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}

##house objects
##=========================
list_names=do.call(rbind.data.frame, lapply(final_list,function(x){x[[11]]}))
list_names[,1]=as.character(list_names[,1])
probs=do.call(cbind.data.frame, lapply(final_list,function(x){x[[9]]}))
party=do.call(rbind.data.frame, lapply(final_list,function(x){x[[7]]}))

dtm_tfxidf <- weightTfIdf(tester_dtm)
dtm_tfxidf <- as.matrix(dtm_tfxidf)
dtm_tfxidf<-cbind(dtm_tfxidf,subject.tester)
rownames(dtm_tfxidf) <- 1:nrow(dtm_tfxidf)
dtm_tfxidf<-as.matrix(dtm_tfxidf)

##senate objects
##=========================

list_names_s=do.call(rbind.data.frame, lapply(final_list_s,function(x){x[[11]]}))
list_names_s[,1]=as.character(list_names_s[,1])
probs_s=do.call(cbind.data.frame, lapply(final_list_s,function(x){x[[9]]}))
party_s=do.call(rbind.data.frame, lapply(final_list_s,function(x){x[[7]]}))

dtm_tfxidf_s <- weightTfIdf(tester_dtm_s)
dtm_tfxidf_s <- as.matrix(dtm_tfxidf_s)
dtm_tfxidf_s<-cbind(dtm_tfxidf_s,subject.tester_s)
rownames(dtm_tfxidf_s) <- 1:nrow(dtm_tfxidf_s)
dtm_tfxidf_s<-as.matrix(dtm_tfxidf_s)

##server
##=========================
server <- function(input, output,session) {

	observe({
 		index_bill_name = input$bill
 		if(length(grep("^SB [0-9]+$|^HB [0-9]+$",index_bill_name))==1){
	 		withProgress(message = 'Getting Bill Info!', value = 0,{
	 		if((index_bill_name)==""){

	 			}else{
	 			output$n=reactive({
	 			return("TRUE")
	 			})

	 			if(length(grep("HB",index_bill_name))==1){
		 			x_dtm=dtm_tfxidf
		 			y=party
		 			z=probs
		 			w=list_names
		 			v=final_list
		 			zz=tester_dtm
		 			zzz=subject.tester
	 			}else{
		 			x_dtm=dtm_tfxidf_s
		 			y=party_s
		 			z=probs_s
		 			w=list_names_s
		 			v=final_list_s
		 			zz=tester_dtm
		 			zzz=subject.tester_s

	 			}
 		##if SB HB then function, then run function dtm_tfxidf party probs list_names
			output$header=renderUI({
				HTML(paste(h3("Bill PDF & Wordcloud")))
				})
			output$header1=renderUI({
				HTML(paste(h3("Chamber Bills Probability by Party Affiliation")))
				})
			output$header2=renderUI({
				HTML(paste(h3("Similar Bills")))
				})

			index=which(sapply(v, FUN=function(X) index_bill_name %in% X))
			good=v[[index]]
			index_dtm=zz[index,]
			output$text2=renderUI({
				HTML(
					paste(
						h2(good[[11]]),
						hr(),
						h5(good[[5]],
							br(),
							paste0("Last Action: ", good[[6]]),
							br(),
							paste0("Authors: ",paste(as.character(good[[2]][,1]),collapse=" | ")),
							br(),
							paste0("CoAuthors: ", paste(as.character(good[[3]][,1]),collapse= " | ")),
							br(),
							paste0("Main Author Party: ", good[[7]]),
							br()
						),"<font color=\"009900\">",h5(paste0("Probable Action: ",good[[8]])),"</font"
					)
					)
					
				})
			incProgress(.25, detail = paste("Almost Done!"))
			if(length(zzz[,zzz[index,]==1])==4291 | length(zzz[,zzz[index,]==1])==2260){
				xx=sum(zzz[,zzz[index,]==1])
				xx=as.data.frame(xx)
				xv=(zzz[index,]==1)
				xxxx=which(xv, arr.ind = T)
				names(xx)=colnames(xv)[xxxx[,2]]
				xx=t(xx)
				xx=as.data.frame(xx)
				xx$names=row.names(xx)
				names(xx)[1]="xx"
				numberofsubjects=ggplot(xx, aes(x =factor(names), y = xx,fill="red")) + geom_bar(stat = "identity")+theme_minimal()+xlab("Subjects")+ylab("Number of Bills")+ theme(legend.position="none")

				}else{
					xx=colSums(zzz[,zzz[index,]==1])
					xx=as.data.frame(xx)
					xx$names=row.names(xx)
					numberofsubjects=ggplot(xx, aes(x =factor(names), y = xx,fill="red",text=paste0("Count:",xx))) + geom_bar(stat = "identity")+theme_minimal()+xlab("Subjects")+ylab("Number of Bills")+ theme(legend.position="none",axis.text.x = element_text(size=6,angle=5,hjust=.5,vjust=.5))

				}
			output$subjectplot=renderPlotly({
				ggplotly(numberofsubjects,tooltip="text") %>% config(displayModeBar = F)
				})
			output$plot2=renderPlotly({
				overallplot=ggplot(,aes(text=paste0(w[,1],"<br>Probability of Passing: ",t(z)[,1],"<br>Probability of NOT Passing: ", t(z)[,2] ),x=t(z)[,1],y=t(z)[,2])) +xlim(0,1)+ylim(0,1)+geom_point(colour="#d3d3d3",alpha=1/10)+xlab("Probability of Passing House")+ylab("Probability of Not Passing House") +
						theme_minimal() + theme(legend.position="none")+geom_point(colour="#0080ff",aes(text=paste0(good[[11]],"<br>Probability of Passing: ",good[[9]][1],"<br>Probability of NOT Passing: ", good[[9]][2]),x=good[[9]][1],y=good[[9]][2],size=10))
					ggplotly(overallplot,tooltip="text") %>% config(displayModeBar = F)
				})
			incProgress(.50, detail = paste("Almost Done!"))
			output$plot3=renderPlotly({
				overallplot2=ggplot(,aes(text=paste0(w[,1],"<br>Probability of Passing: ",t(z)[,1],"<br>Probability of NOT Passing: ", t(z)[,2] ),colour=y[,1],x=t(z)[,1],y=t(z)[,2])) +xlim(0,1)+ylim(0,1)+geom_point(alpha=1/5)+xlab("Probability of Passing House")+ylab("Probability of Not Passing House") +
						theme_minimal() +scale_color_fivethirtyeight(y[,1],name="Party")
					ggplotly(overallplot2,tooltip="text") %>% config(displayModeBar = F)
				})
			m <- as.matrix(index_dtm)
			vv <- sort(colSums(m),decreasing=TRUE)
			d <- data.frame(word = names(vv),freq=vv)
			output$plot <- renderPlot({wordcloud(words = d$word, freq = d$freq, min.freq = 1,
			          max.words=200, random.order=FALSE, rot.per=0.35, 
			          colors=brewer.pal(8, "Dark2"))})
			 output$pdfviewer <- renderText({
							return(paste('<iframe style="height:600px; width:100%" src="', good[[10]], '"></iframe>', sep = ""))
					})
			yes=matrix()
			for(j in 1:nrow(x_dtm)){
				xx2=crossprod(x_dtm[index,],x_dtm[j,])/sqrt(crossprod(x_dtm[index,]) * crossprod(x_dtm[j,]))
				yes=rbind(yes,xx2)
			}
			incProgress(.75, detail = paste("Almost Done!"))
			indices=whichpart(yes,n=12)
			similarity_score=yes[indices]
			indices=indices-1
			similar_doc_indices=indices[!indices==index]
			similarity_score=similarity_score[!indices==index]
			sb=data.frame()
			for(i in 1:length(similar_doc_indices)){
				k=similar_doc_indices[i]
				housename=v[[k]][[11]]
				prob=as.character(v[[k]][[8]])
				party_name=v[[k]][[7]]
				caption_name=v[[k]][[5]]
				author_names=v[[k]][[2]]
				similar_bills=cbind(housename,paste(as.character(author_names[,1]),collapse=" | "),caption_name,party_name,prob,similarity_score[i])
				sb=rbind(sb,similar_bills)
			}
			names(sb)=c("Bill Number", "Author", "Caption", "Party", "Probable Action","Similarity Score")
			sb$"Similarity Score"=as.character(sb$"Similarity Score")
			sb$"Similarity Score"=as.numeric(sb$"Similarity Score")
			sb$"Similarity Score"=round(sb$"Similarity Score",2)
			sb=sb[order(sb$"Similarity Score",decreasing=T),]
			output$mytable = renderDataTable({
				sb
						},options = list(dom = 'ft',initComplete = JS(
    						"function(settings, json) {",
    				"$(this.api().table().header()).css({'background-color': '#4582ec', 'color': '#fff'});",
    						"}")))
			outputOptions(output, 'n', suspendWhenHidden=FALSE)
			}
	 	})

	 	}else{
	 		if((index_bill_name)==""){

	 			}else{
	 				output$n=reactive({
	 					return("FALSE")
	 				})
	 				output$text2=renderUI({
	 					HTML(paste("<font color=\"B20000\">",h4(paste0(index_bill_name," is not a correct bill number. Please enter a correct bill number!")),"</font>"))
	 				})
	 				outputOptions(output, 'n', suspendWhenHidden=FALSE)

	 			}

	 	}
	})
}
