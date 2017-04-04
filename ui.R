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
ui <- fluidPage(theme = shinytheme("readable"),
	    navbarPage(title=HTML("OneFourty"), id="nav",windowTitle="OneFourty",
	    	tabPanel("Bill Analysis",
	    		div(class="outer",
        				tags$head(
        					tags$link(
        						rel = "icon", type = "image/svg", href = "https://upload.wikimedia.org/wikipedia/commons/3/35/Texas_flag_map.svg"))),

	    		h4(img(src="https://upload.wikimedia.org/wikipedia/commons/3/35/Texas_flag_map.svg",height="50px"),"85th Lege House Bill Analysis"),
	    		textInput("bill","Enter Bill Number AS HB 1"),
	    		submitButton(text="Search Bill"),
	    		htmlOutput("text2"),
	    		 fluidRow(
	    		 	column(6,plotlyOutput("plot2")),
	    		 	column(6,plotlyOutput("subjectplot"))
	    		 	),

	    		 hr(),
 	    		fluidRow(
 	    			column(6,htmlOutput('pdfviewer')),
 	    			column(6, plotOutput("plot",height="600px",width="500px"))
 	    			)
			)
	    )
)
