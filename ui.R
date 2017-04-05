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

load("final_list_subjects_dtm.RData")
load("senate.RData")

list_names=do.call(rbind.data.frame, lapply(final_list,function(x){x[[11]]}))
list_names[,1]=as.character(list_names[,1])
probs=do.call(cbind.data.frame, lapply(final_list,function(x){x[[9]]}))
ui <- fluidPage(theme = shinytheme("readable"),
	    navbarPage(title=div(img(src="onefourty.png", height="20", width="99")
	,""), id="nav",windowTitle="OneFourty",
	    	tabPanel("Legislative Bill Analysis",
	    		div(class="outer",
        				tags$head(
        					tags$link(
        						rel = "icon", type = "image/png", href = "favicon.png"))
        				),

	    		h4(img(src="https://upload.wikimedia.org/wikipedia/commons/3/35/Texas_flag_map.svg",height="50px"),"85th Lege Bill Analysis"),
	    		helpText("Make sure to capitalize 'HB' OR 'SB' in search box"),
	    		textInput("bill","For example: Enter Bill Number AS HB 1"),
	    		submitButton("Search Bill"),
	    		htmlOutput("text2"),
	    		conditionalPanel(condition="output.n=='TRUE'",
		    		fluidRow(
		    		 	column(6,plotlyOutput("plot2")),
		    		 	column(6,plotlyOutput("subjectplot"))
		    		 	)),
		    	conditionalPanel(condition="output.n=='TRUE'",
		    		hr()
		    		),
		    	conditionalPanel(condition="output.n=='TRUE'",
		    		htmlOutput("header")
		    		),
		    	conditionalPanel(condition="output.n=='TRUE'",	
		    		fluidRow(
	 	    			column(6,htmlOutput('pdfviewer')),
	 	    			column(6, plotOutput("plot",height="600px",width="500px"))
 	    			)),
		    	conditionalPanel(condition="output.n=='TRUE'",
	 	    		hr()
	 	    		),
		    	conditionalPanel(condition="output.n=='TRUE'",
	 	    		htmlOutput("header1")
	 	    		),
		    	conditionalPanel(condition="output.n=='TRUE'",
	 	    		plotlyOutput("plot3")
	 	    	),
	 	    	conditionalPanel(condition="output.n=='TRUE'",
	 	    		hr()
	 	    		),
	 	    	conditionalPanel(condition="output.n=='TRUE'",
	 	    		htmlOutput("header2")
	 	    	),
	 	    	conditionalPanel(condition="output.n=='TRUE'",
	 	    		dataTableOutput('mytable')
	 	    	),


 	    		hr(),
 	    		h6("Probability model created by using data from previous 84th legislative session. This data is only a representation of the model and should not be taken literally."),
 	    		h6("Questions/Recommendations? Email me @" ,a(href="mailto:info@christophvel.com","info@christophvel.com"))
			)
	    )
)
