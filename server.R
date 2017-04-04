library(tm,,quietly = TRUE)
library(wordcloud,,quietly = TRUE)
library(e1071,,quietly = TRUE)
library(dplyr,,quietly = TRUE)
library(caret,,quietly = TRUE)
library(doMC,,quietly = TRUE)
library(xml2,,quietly = TRUE)
library(rvest,,quietly = TRUE)
library(shiny)
library(shinythemes)
library(shinysky)
library(ggthemes)
library(plotly)
load("final_list_subjects_dtm.RData")

list_names=do.call(rbind.data.frame, lapply(final_list,function(x){x[[11]]}))
list_names[,1]=as.character(list_names[,1])
probs=do.call(cbind.data.frame, lapply(final_list,function(x){x[[9]]}))
party=do.call(rbind.data.frame, lapply(final_list,function(x){x[[7]]}))

dtm_tfxidf <- weightTfIdf(tester_dtm)
dtm_tfxidf <- as.matrix(dtm_tfxidf)
dtm_tfxidf<-cbind(dtm_tfxidf,subject.tester)
rownames(dtm_tfxidf) <- 1:nrow(dtm_tfxidf)
dtm_tfxidf<-as.matrix(dtm_tfxidf)

whichpart <- function(x, n=30) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}



server <- function(input, output) {
 observe({
 		index_bill_name = input$bill
 		if((index_bill_name)==""){

 			}else{
 		index=which(sapply(final_list, FUN=function(X) index_bill_name %in% X))
		good=final_list[[index]]
		index_dtm=tester_dtm[index,]
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
		if(length(subject.tester [,subject.tester[index,]==1])==4291){
			xx=sum(subject.tester[,subject.tester[index,]==1])
			xx=as.data.frame(xx)
			xv=(subject.tester[index,]==1)
			xxxx=which(xv, arr.ind = T)
			names(xx)=colnames(xv)[xxxx[,2]]
			xx=t(xx)
			xx=as.data.frame(xx)
			xx$names=row.names(xx)
			names(xx)[1]="xx"
			numberofsubjects=ggplot(xx, aes(x =factor(names), y = xx,fill="red")) + geom_bar(stat = "identity")+theme_minimal()+xlab("Subjects")+ylab("Number of Bills")+ theme(legend.position="none")

			}else{
				xx=colSums(subject.tester[,subject.tester[index,]==1])
				xx=as.data.frame(xx)
				xx$names=row.names(xx)
				numberofsubjects=ggplot(xx, aes(x =factor(names), y = xx,fill="red",text=paste0("Count:",xx))) + geom_bar(stat = "identity")+theme_minimal()+xlab("Subjects")+ylab("Number of Bills")+ theme(legend.position="none",axis.text.x = element_text(size=6,angle=5,hjust=.5,vjust=.5))

			}
		output$subjectplot=renderPlotly({
			ggplotly(numberofsubjects,tooltip="text") %>% config(displayModeBar = F)
			})
		output$plot2=renderPlotly({
			overallplot=ggplot(,aes(text=paste0(list_names[,1],"<br>Probability of Passing: ",t(probs)[,1],"<br>Probability of NOT Passing: ", t(probs)[,2] ),x=t(probs)[,1],y=t(probs)[,2])) +xlim(0,1)+ylim(0,1)+geom_point(colour="#d3d3d3",alpha=1/10)+xlab("Probability of Passing House")+ylab("Probability of Not Passing House") +
  				theme_minimal() + theme(legend.position="none")+geom_point(colour="#0080ff",aes(text=paste0(good[[11]],"<br>Probability of Passing: ",good[[9]][1],"<br>Probability of NOT Passing: ", good[[9]][2]),x=good[[9]][1],y=good[[9]][2],size=10))
  			ggplotly(overallplot,tooltip="text") %>% config(displayModeBar = F)
			})
		output$plot3=renderPlotly({
			overallplot2=ggplot(,aes(text=paste0(list_names[,1],"<br>Probability of Passing: ",t(probs)[,1],"<br>Probability of NOT Passing: ", t(probs)[,2] ),colour=party[,1],x=t(probs)[,1],y=t(probs)[,2])) +xlim(0,1)+ylim(0,1)+geom_point(alpha=1/5)+xlab("Probability of Passing House")+ylab("Probability of Not Passing House") +
  				theme_minimal() +scale_color_fivethirtyeight(party[,1],name="Party")
  			ggplotly(overallplot2,tooltip="text") %>% config(displayModeBar = F)
			})
		m <- as.matrix(index_dtm)
		v <- sort(colSums(m),decreasing=TRUE)
		d <- data.frame(word = names(v),freq=v)
		output$plot <- renderPlot({wordcloud(words = d$word, freq = d$freq, min.freq = 1,
		          max.words=200, random.order=FALSE, rot.per=0.35, 
		          colors=brewer.pal(8, "Dark2"))})
		 output$pdfviewer <- renderText({
      				return(paste('<iframe style="height:600px; width:100%" src="', good[[10]], '"></iframe>', sep = ""))
  			})
		yes=matrix()
		for(j in 1:nrow(dtm_tfxidf)){
			xx2=crossprod(dtm_tfxidf[index,],dtm_tfxidf[j,])/sqrt(crossprod(dtm_tfxidf[index,]) * crossprod(dtm_tfxidf[j,]))
			yes=rbind(yes,xx2)
		}
		indices=whichpart(yes,n=12)
		indices=indices-1
		similar_doc_indices=indices[!indices==index]
		sb=data.frame()
		for(i in 1:length(similar_doc_indices)){
			k=similar_doc_indices[i]
			housename=final_list[[k]][[11]]
			prob=as.character(final_list[[k]][[8]])
			party_name=final_list[[k]][[7]]
			caption_name=final_list[[k]][[5]]
			author_names=final_list[[k]][[2]]
			similar_bills=cbind(housename,paste(as.character(author_names[,1]),collapse=" | "),caption_name,party_name,prob)
			sb=rbind(sb,similar_bills)
		}
		names(sb)=c("Bill Number", "Author", "Caption", "Party", "Probable Action")
		output$mytable = renderDataTable({
			sb
		})
	}
 	})
	}
