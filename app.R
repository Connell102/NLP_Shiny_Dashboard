################## Shiny NLP Dashboard #####################################################################
###########################################################################################################
###########################################################################################################

num_tweets_to_download <- 200
string_to_search <- ""

################################################################
# libraries#####################################################
################################################################
# libraries needed are imported
# defines shiny infrastructure
library(shiny)
# allows a reactable output
library(reactable)
# essential function in R, used for formatting and cleaning of data
library(dplyr)
# used to extract twitter data through API
library(twitteR)
library(purrr)
library(tidytext)
library(textdata)
library(tidyr)
# sentiment analysis package
library(syuzhet)
# visualization package used in plot output areas defined in script
library(ggplot2)
library(tidytext)
library(NLP)
# used for data cleaning and extraction of specific characters from dataset
library(tm)
# used to extract data from reddit
library(RedditExtractoR)
library(reshape2)
# sentiment analysis package
library(sentimentr)
# used to apply additional formating and layout options to existing Shiny Script
library(shinydashboard)
# allows output of word cloud visualizations
library(wordcloud)
library(topicmodels)
# used for topic modelling, particularly LDA,  within script
library(textmineR)

# below code defines UI portion of app, which controls appearance of app interface
ui <- dashboardPage(
    #defines dashboard header
    dashboardHeader(title = "nlp dashboard"),
    #defines sidebar menu, which are then linked to tabitems below, allowing user to navigate between pages
    dashboardSidebar(    sidebarMenu(
        #homepage defined
        menuItem("home", tabName = "home", icon = icon("home")),
        #sentiment analysis page defined
        menuItem("sentiment analysis", tabName = "sentimentanalysis", icon = icon("sentiment analysis")),
        #topic modelling page defined
        menuItem("Topic Modelling", tabName = "topicmodelling", icon = icon("topicmodelling"))
    )),
    #dashboard body defined
    dashboardBody(
        #3 tabitems are defined within dashboard body, all linked to corresponding name in menuitems in above dashboard sidebar
        tabItems(
            # homepage defined within first tabitem
            # title added to tab
            tabItem(tabName = "home",
                    # home content defined within box object
                    # header row defined
                    fluidRow(box(tags$h1("Welcome!"),
                                 # subheading defined within tags$h4
                                 tags$h4("This dashboard is for those new to datascience to use in exploring the application of natural language processing techniques on social media data, extracted based on user input. Please find instructions in video below on using sentiment analysis and topic modelling tabs"),
                                 # html function below used to embed youtube video inside home page
                                 # width and height are defined and fullscreen mode is set to allowable
                                 # youtube video provides an introduction on how to use the site and brief description of sentiment analysis and topic modelling techniques
                                 htmltools::HTML('<iframe width="1200" height="600" src="https://www.youtube.com/embed/3dPw6UxIs-U" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'), width = 10, height = 800))
                    
            ),
            #sentiment analysis page defined within second tabitem
            tabItem(tabName = "sentimentanalysis",
                    fluidRow(
                        # dashboard controls defined within box area
                        box(
                            #box title
                            title = "sentiment Controls",
                            #numeric input which will be lniked to quantity of tweets and comments to output within TwitteR and RedditExtractoR functions
                            numericInput("num_rows_to_request",
                                         "Number of rows outputted:",
                                         min = 100,
                                         max = 10000,
                                         value = 200),
                            #radio button allows user to select data source
                            radioButtons("SelectData", "Select Pull Source:",
                                         c("Reddit" = "Reddit",
                                           "Twitter" = "Twitter",
                                           "All" = "All")),
                            #radio button allows user to select package used for sentiment analysis
                            radioButtons("SelectSentiment", "Select Sentiment Model:",
                                         c("syuzhet" = "syuzhet",
                                           "sentimentR" = "sentimentR",
                                           "sentimentR - emotion" = "sentimentR - emotion")),
                            #below defined conditional panels, allowing user to select lexicon dictionary and visualizationsto be used within sentiment analysis model
                            #this will only appear when syuzhet is selected from selectsentiment
                            conditionalPanel(condition = "input.SelectSentiment == 'syuzhet'",
                                             selectInput('sentimentdictionary', 'choose sentiment dictionary:', c('syuzhet', 'afinn', 'nrc', 'bing' ))),
                            conditionalPanel(condition = "input.SelectSentiment == 'syuzhet'",
                                             selectInput('syuzhetplot', 'syuzhet plots 1:', c('barchart', 'histogram', 'wordcloud'))),
                            conditionalPanel(condition = "input.SelectSentiment == 'syuzhet'",
                                             selectInput('syuzhetplot1', 'syuzhet plots 2:', c('barchart', 'histogram', 'wordcloud'))),
                            #below defined conditional panels, allowing user to select  visualisations to be used within sentiment analysis model
                            #this will only appear when sentimentR is selected from selectsentiment
                            conditionalPanel(condition = "input.SelectSentiment == 'sentimentR'",
                                             selectInput('sentimentRplot', 'sentimentR plots 1:', c('barchart', 'histogram'))),
                            conditionalPanel(condition = "input.SelectSentiment == 'sentimentR'",
                                             selectInput('sentimentRplot1', 'sentimentR plots 2:', c('barchart', 'histogram'))), 
                            #text input for search string to be inputted. input from textinput will be supplied to TwitteR and sentimentR functions which will pull data from Twitter and Reddit
                            textInput("string_to_search",
                                      "Input search string: ",
                                      value = ""),
                            #action button will activate server functions.
                            actionButton("get_data", "Request data", class = "btn_primary" ),width = 2
                        ),
                        #plot outputs for visualizations, width is defined
                        box(
                            plotOutput("sentiment_visual"),width = 5),
                        
                        box(plotOutput("sentiment_visual2"),width = 5)
                        #a download button is added to  bottom of document
                    ),downloadButton("download", "download .csv"),
            ),
            
            # topic modelling tabitem
            tabItem(tabName = "topicmodelling",
                    fluidRow(
                        #box area containing dashboard controls
                        box(
                            title = "topic model Controls",
                            #numeric input defining number of rows requested
                            numericInput("num_rows_to_request2",
                                         "Number of rows outputted:",
                                         min = 100,
                                         max = 10000,
                                         value = 200),
                            #radio button selects data sources
                            radioButtons("SelectData2", "Select Pull Source:",
                                         c("Reddit" = "Reddit",
                                           "Twitter" = "Twitter",
                                           "All" = "All")),
                            #numeric input defining the number of topics (K size). This will be inputted into lda model
                            numericInput("ksize2","choose topic quantity for coherence calc: ", min = 1, value = ""),
                            #numeric input defining the number of top terms outputted within table output area. This value will be input into topterms function
                            numericInput("topterms2", "choose displayed terms: ", min = 1, max = 50, value = ""),
                            #text string input area
                            textInput("string_to_search2",
                                      "Input search string: ",
                                      value = ""),
                            #action button to trigger server functions
                            actionButton("get_data2", "Request data", class = "btn_primary" ),width = 2
                        ),
                        #plotoutput area. This will link with linechart to plot coherence and prevelance for outputted LDA topics
                        box(
                            plotOutput("topic_visual"),width = 10),
                        #table output area which will be linked to output containing top terms, prevelance and coherence per topic
                        box(
                            tableOutput("topic_visual2"),width = 10),
                        #download button which will allow download of csv file containing table outputted in table output area
                    ),downloadButton("download2", "download .csv"),
            )
        )
        #defines color of dashboard background
    ),skin = "green"
)

server <- function(input, output) {
    # OAuth authentication credentials. credentials are written to text file and readlines function is used to read line in text file specific to credential
    consumer_key <- readLines("tokens.txt")[1]
    consumer_secret <- readLines("tokens.txt")[2]
    access_token <- readLines("tokens.txt")[3]
    access_secret <- readLines("tokens.txt")[4]
    # allows use of local file to cache OAuth access credentials
    options(httr_oauth_cache = TRUE) 
    #function below accesses API and supplies credentials
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
    #Have created seperate reactive areas for specific areas of server process.
    #reactive data_extract is used to extract data from twitter and/or reddit.
    #functions within reactive area are event reactive on activation of get_data button.
    #This allows functions to be delayed until get_data is activated.
    data_extract <- eventReactive(input$get_data, {
        #required inputs are called in order to be accessable to data_extract reactive
        req(input$SelectData)
        req(input$string_to_search)
        #RedditExtractoR and TwitteR package functions which pull data from Reddit and Twitter require non blank inputs
        #in order to avoid an error message. If statement below ensures the functions contained within this reactive will
        #only be performed if the text inout area is not blank
        if(input$string_to_search !=""){
            #if statement below defines actions taking place if all data sources are selected from selectdata radiobutton
            if(input$SelectData == "All"){
                #searchtwitter function accepts string as input,accesses twitter and return text output of tweets containing supplied string
                tweets <- searchTwitter(input$string_to_search, n = input$num_rows_to_request,lang = "en")
                #TwitteR function below strips retweets from searctwitter output
                tweets <- strip_retweets(tweets,strip_manual = TRUE, strip_mt = TRUE)
                #converts searchtwitter output to a dataframe with additional variables
                tweets <- twListToDF(tweets)
                #selects distinct text and screenname from tweets dataframe
                #filters out any tweets where isRetweet columns value is false
                tweets <- tweets %>%
                    distinct(text, screenName, .keep_all = TRUE) %>%
                    filter(isRetweet == "FALSE")
                #only necessary data needs to be extracted. here we define the column number containing text variable (tweet text)
                tweetstext <- tweets[,1]
                #number of rows of tweets is defined and assigned to a varaible
                x <- nrow(tweets)
                #this is then changed to a numeric data type
                x <- as.numeric(x)
                # text input is then assigned to k variable
                #this is then sent to gsub in order to remmove #, as # is not accepted as input to reddit_urls function
                #which is used to retrieve threads from Reddit
                k <- input$string_to_search
                k <- gsub("#","", k)
                #reddit_urls function retrieves reddit threads based on string input (K)
                #this is outputted as a dataframe with original thread post and other variables such as num_comments providing metadata about thread
                #lower limits for comment and page threshold are also defined and assigned to variable
                url <- reddit_urls(k, cn_threshold = 1, page_threshold = 1)
                # dplyr filter is used in combinations with max function to filter num_comments column based on largest quantity of comments.
                # thread with largest quantity of comments is then taken as maxurl variable
                #from this we extract the url column as we will use this as input to reddit_content function
                # which will return a dataframe containing thread comments along with other coulmns supplying metadata about comment
                #such as user name
                maxurl <- url %>% 
                    filter(num_comments == max(num_comments))
                # specifies row and column to extract data from maxurl variable
                comments <- maxurl[1,5]
                #retrieves dataframe containing comments per thread
                comments <- reddit_content(comments)
                # column containing thread comments output to commentstext variable
                commentstext <- comments[,13]
                #RedditExtractoR does not supply user with option to specify number of rows output by reddit_content function
                # We need to specify number of rows outputted by reddit_content
                # this is accomplished by assigning the nrow values of tweets variable and also the nrow value of our comments variable
                # specifies number of rows within comments variable
                i <- nrow(comments)
                # converts this to numeric data type
                i <- as.numeric(i)
                # if statement below specifies that if number of rows of reddit output is > 
                # the number of rows of twitter output, then the number of rows outputted by reddit 
                # is capped at the number of rows within tweet output
                # if number of rows output from tweetstext is > than number of rows output from Reddit
                # then the number of rows output from tweetstext is capped at the number of rows from Reddit
                if(i >= x){
                    commentstext <- commentstext[1:x]
                }
                else if(i < x) {
                    tweetstext <- tweetstext[1:i]
                }
                
                #cleaning text###########################################
                #########################################################
                # In order to provide clean input into models, we first need t complete cleaning of text
                tweetstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", tweetstext)
                # we remove https text using gsub
                tweetstext <- gsub("https", "", tweetstext)
                # In order to clean text efficiently, we will use tm package
                # text needs to be in corpus format in order for tm functions to be applied
                # first we assign tweetstext as vectorsource to tweetcorpus variable
                tweetcorpus <- VectorSource(tweetstext)
                # tweetcorpus is then assigned to a corpus
                tweetcorpus <- Corpus(tweetcorpus)
                # we can now clean corpus
                # first step is to remove numbers from text
                tweetcorpus <- tm_map(tweetcorpus, removeNumbers)
                # we then convert all characters to lower case
                tweetcorpus <- tm_map(tweetcorpus,tolower)
                # we then remove stopwords as these add no value to the sentiment score
                tweetcorpus <- tm_map(tweetcorpus,removeWords,stopwords("english"))
                # we also strip whitespaces
                tweetcorpus <- tm_map(tweetcorpus,stripWhitespace)
                # finally, once cleaning has been complete, we can assign back to a dataframe
                # we assign tweetcorpus variable to a dataframe object, using sapply to convert
                # corpus content to data frame using as.character
                tweetcorpus <- data.frame(cleantext = sapply(tweetcorpus, as.character))
                # in order to distinguish between twitter and reddit output, as these will be marked in barchart output,
                # we need to label data source of cleaned text
                # this is also why we dont join Reddit and Twitter dataframe until after cleaning aperations have taken place
                tweetcorpus <- tweetcorpus %>%
                    mutate(site = "Twitter")
                # we assign a sequential column to dataframe, to be used as a unique identifier
                tweetcorpus$elementid <- seq.int(nrow(tweetcorpus))
                
                # the same cleaning operations are performed for reddit output
                # gsub is used to remove unnessicary characters
                # text is then assigned to corpus and various cleaning tasks are performed using the tm package
                # text data is then assigned back to a dataframe and columns a data source and unique identifier column are added
                commentstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", commentstext)
                commentstext <- gsub("https", "", commentstext)
                commentcorpus <- VectorSource(commentstext)
                commentcorpus <- Corpus(commentcorpus)
                commentcorpus <- tm_map(commentcorpus, removeNumbers)
                commentcorpus <- tm_map(commentcorpus,tolower)
                commentcorpus <- tm_map(commentcorpus,removeWords,stopwords("english"))
                commentcorpus <- tm_map(commentcorpus,stripWhitespace)
                commentcorpus <- data.frame(cleantext = sapply(commentcorpus, as.character))
                commentcorpus <- commentcorpus %>%
                    mutate(site = "Reddit")
                commentcorpus$elementid <- seq.int(nrow(commentcorpus))
                
                # twitter and reddit text data can now be joined together using rbind
                # Data is now suitably formatted and cleaned and can be assigned to a sentiment analysis model
                text_combine <- rbind(tweetcorpus, commentcorpus)
                text_combine
            }
            # else if statement below describes the functions to be carried out if only twitter is selected as a data source
            # data is again extracted from twitter using TwitteR package, assigned to corpus, cleaned and labelled
            else if(input$SelectData == "Twitter"){
                tweets <- searchTwitter(input$string_to_search, n = input$num_rows_to_request,lang = "en")
                tweets <- strip_retweets(tweets,strip_manual = TRUE, strip_mt = TRUE)
                tweets <- twListToDF(tweets)
                tweets <- tweets %>%
                    distinct(text, screenName, .keep_all = TRUE) %>%
                    filter(isRetweet == "FALSE")
                tweetstext <- tweets[,1]
                
                tweetstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", tweetstext)
                tweetstext <- gsub("https", "", tweetstext)
                tweetcorpus <- VectorSource(tweetstext)
                tweetcorpus <- Corpus(tweetcorpus)
                tweetcorpus <- tm_map(tweetcorpus, removeNumbers)
                tweetcorpus <- tm_map(tweetcorpus,tolower)
                tweetcorpus <- tm_map(tweetcorpus,removeWords,stopwords("english"))
                tweetcorpus <- tm_map(tweetcorpus,stripWhitespace)
                tweetcorpus <- data.frame(cleantext = sapply(tweetcorpus, as.character))
                tweetcorpus <- tweetcorpus %>%
                    mutate(site = "Twitter")
                tweetcorpus$elementid <- seq.int(nrow(tweetcorpus))
                text_combine <- tweetcorpus
                text_combine
                
            }
            # else if statement below describes the functions to be carried out if only reddit is selected as a data source
            # data is again extracted from reddit using RedditExtractoR package, assigned to corpus, cleaned and labelled
            else{
                k <- input$string_to_search
                k <- gsub("#","", k)
                url <- reddit_urls(k, cn_threshold = 1, page_threshold = 1)
                maxurl <- url %>% 
                    filter(num_comments == max(num_comments))
                comments <- maxurl[1,5]
                comments <- reddit_content(comments)
                commentstext <- comments[,13]
                
                commentstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", commentstext)
                commentcorpus <- VectorSource(commentstext)
                commentcorpus <- Corpus(commentcorpus)
                commentcorpus <- tm_map(commentcorpus, removeNumbers)
                commentcorpus <- tm_map(commentcorpus,tolower)
                commentcorpus <- tm_map(commentcorpus,removeWords,stopwords("english"))
                commentcorpus <- tm_map(commentcorpus,stripWhitespace)
                commentcorpus <- data.frame(cleantext = sapply(commentcorpus, as.character))
                commentcorpus <- commentcorpus %>%
                    mutate(site = "Reddit")
                commentcorpus$elementid <- 1:nrow(commentcorpus)
                
                text_combine <- commentcorpus
                text_combine
            }
            
        }})
    
    # below reactive sentimenttable provides an input to file download
    # this is event reactive on get_data button meaning that get_data button must first be clicked and server functions engaged before data will pull
    # this is done to ensure that the same data output to visualizations is used for dataset downloaded as csv file
    # sentimenttable accepts data_extract reactive as input, creates a type of sentiment analysis model (specified within if statement, based on user input into selectsentiment input)
    # and outputs a table containing cleaned text from tweet/reddit comment, data source column (reddit/twitter), unique identifier per row and a sentiment polarity score
    # this will then be written to csv file 
    
    #sentimenttable assigned to event reactive, based on input$get_data button being clicked
    sentimenttable <- eventReactive(input$get_data, {
        # data extract reactive is assigned to variable x
        x <- data_extract()
        # element id column is created as unique identifier
        x$elementid <- 1:nrow(x)
        # if statement below describes download actions to carry out if sentimentR package is selected from selectsentiment
        if(input$SelectSentiment == "sentimentR") {
            # sentiment_by function is used to create sentiment score per text string
            # sentiment_by accepts two variables in this case.
            # first accepts x$cleantext variable as variable to perform sentiment analysis upon
            # then accepts x$elementid as the field to calculate sentiment analysis by
            # this is because sentimentR package accepts sentiment scores by sentence rather than string
            # in order to output a sentiment score by each text string rather than by sentence automatically,
            # we specify x$elementid as our field to calculate sentiment by in our sentiment_by function
            y <- sentiment_by(x$cleantext, x$elementid)
            # we then column bind newly output sentiment score to our original variable containing cleaned text and labelled rows
            cbind(x,y)
        }
        # if statement below defines download actions to carry out when syuzhet is selected from selectsentiment input
        else if(input$SelectSentiment == "syuzhet") {
            # get sentiment function is used to extract sentiment from our x$cleantext variable
            # we also specify the sentiment lexicon to be used
            # this is done by assigning method (sentiment lexicon to use)
            # as input$sentimentdictionary, allowing user to select either Syuzhet, Bing, Afinn or NRC sentiment lexicons from sentiment control panel
            y <- get_sentiment(x$cleantext, method = input$sentimentdictionary)
            # we then column bind newly output sentiment score to our original variable containing cleaned text and labelled rows
            cbind(x,y)
        }
        # if statement defines download functions to carry out when sentimentR - emotion is selected from  selectsentiment input
        else if(input$SelectSentiment == "sentimentR - emotion") {
            # first we must extract sentences from each text string
            # this allows us to determine emotions present in each sentence
            # this formatting is necesssary to produce a workable input to emotion function
            xi <- sentimentr::get_sentences(x$cleantext)
            # we will now use sentimentR function emotion to extract emotions per sentence
            # we assign this to variable i
            i <- emotion(xi)
            # output from emotion function produces a score per each sentiment type e.g. anger, trust etc
            # function also outputs a column element_id
            # which defines the text string the sentence originated from
            # however we do not have the original text present within the function output
            # we need to link this with x variable containing cleantext
            # we do so by using inner_join function to bind tables by duplicate variables
            # in this case the column names are slightly different
            # we need to specify both column names from two tables in order to create table output
            # that will be inputted as content to download output
            x <- inner_join(x, i, by = c("elementid"= "element_id"))
            
        }
        
    })
    
    ## Topic Modelling #################################
    ####################################################
    
    # data extracion and cleaning#######################
    
    # same process for extraction of data and cleaning of said data that was used for serntiment analysis output is followed here
    # data is extracted from    Twitter and Reddit using previously mentioned functions, with strings and quantity of rows generate based on user input into topic model control panel
    data_extract_2 <- eventReactive(input$get_data2, {
        # below inputs required by the reactive area are defined
        req(input$SelectData2)
        req(input$ksize2)
        req(input$string_to_search2)
        # as with sentiment analysis data extraction and cleansing, if statement below allows server functions to be performed only if the text input string <> ""
        if(input$string_to_search2 != ""){
            # below if statement describes actions carried out if data source selected is "All"
            if(input$SelectData2 == "All"){
                tweets <- searchTwitter(input$string_to_search2, n = input$num_rows_to_request2,lang = "en")
                tweets <- strip_retweets(tweets,strip_manual = TRUE, strip_mt = TRUE)
                tweets <- twListToDF(tweets)
                tweets <- tweets %>%
                    distinct(text, screenName, .keep_all = TRUE) %>%
                    filter(isRetweet == "FALSE")
                tweetstext <- tweets[,1]
                x <- nrow(tweets)
                x <- as.numeric(x)
                k <- input$string_to_search2
                k <- gsub("#","", k)
                url <- reddit_urls(k, cn_threshold = 1, page_threshold = 1)
                maxurl <- url %>% 
                    filter(num_comments == max(num_comments))
                comments <- maxurl[1,5]
                comments <- reddit_content(comments)
                commentstext <- comments[,13]
                i <- nrow(comments)
                i <- as.numeric(i)
                if(i >= x){
                    commentstext <- commentstext[1:x]
                }
                else if(i < x) {
                    tweetstext <- tweetstext[1:i]
                }
                
                
                # data cleaning for twitter data
                tweetstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", tweetstext)
                tweetstext <- gsub("https", "", tweetstext)
                tweetcorpus <- VectorSource(tweetstext)
                tweetcorpus <- Corpus(tweetcorpus)
                tweetcorpus <- tm_map(tweetcorpus, removeNumbers)
                tweetcorpus <- tm_map(tweetcorpus,tolower)
                tweetcorpus <- tm_map(tweetcorpus,removeWords,stopwords("english"))
                tweetcorpus <- tm_map(tweetcorpus,stripWhitespace)
                tweetcorpus <- data.frame(cleantext = sapply(tweetcorpus, as.character))
                tweetcorpus <- tweetcorpus %>%
                    mutate(site = "Twitter")
                tweetcorpus$elementid <- seq.int(nrow(tweetcorpus))
                
                # data cleaning for reddit data
                commentstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", commentstext)
                commentstext <- gsub("https", "", commentstext)
                commentcorpus <- VectorSource(commentstext)
                commentcorpus <- Corpus(commentcorpus)
                commentcorpus <- tm_map(commentcorpus, removeNumbers)
                commentcorpus <- tm_map(commentcorpus,tolower)
                commentcorpus <- tm_map(commentcorpus,removeWords,stopwords("english"))
                commentcorpus <- tm_map(commentcorpus,stripWhitespace)
                textcorpus <- commentcorpus
            }
            # else if below describes actions taking place if only twitter is selected from data source radio button (selectdata2)
            else if(input$SelectData2 == "Twitter"){
                tweets <- searchTwitter(input$string_to_search2, n = input$num_rows_to_request2,lang = "en")
                tweets <- strip_retweets(tweets,strip_manual = TRUE, strip_mt = TRUE)
                tweets <- twListToDF(tweets)
                tweets <- tweets %>%
                    distinct(text, screenName, .keep_all = TRUE) %>%
                    filter(isRetweet == "FALSE")
                tweetstext <- tweets[,1]
                
                tweetstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", tweetstext)
                tweetstext <- gsub("https", "", tweetstext)
                tweetcorpus <- VectorSource(tweetstext)
                tweetcorpus <- Corpus(tweetcorpus)
                tweetcorpus <- tm_map(tweetcorpus, removeNumbers)
                tweetcorpus <- tm_map(tweetcorpus,tolower)
                tweetcorpus <- tm_map(tweetcorpus,removeWords,stopwords("english"))
                tweetcorpus <- tm_map(tweetcorpus,stripWhitespace)
                textcorpus <- tweetcorpus
                
            }
            # below else describes server functions carried out if reddit only is selected from data source input (selectdata2)
            else{
                k <- input$string_to_search2
                k <- gsub("#","", k)
                url <- reddit_urls(k, cn_threshold = 1, page_threshold = 1)
                maxurl <- url %>% 
                    filter(num_comments == max(num_comments))
                comments <- maxurl[1,5]
                comments <- reddit_content(comments)
                commentstext <- comments[,13]
                
                commentstext <- gsub("[^[:alnum:]\\ \\.\\s]", "", commentstext)
                commentstext <- gsub("https", "", commentstext)
                commentcorpus <- VectorSource(commentstext)
                commentcorpus <- Corpus(commentcorpus)
                commentcorpus <- tm_map(commentcorpus, removeNumbers)
                commentcorpus <- tm_map(commentcorpus,tolower)
                commentcorpus <- tm_map(commentcorpus,removeWords,stopwords("english"))
                commentcorpus <- tm_map(commentcorpus,stripWhitespace)
                textcorpus <- commentcorpus
            }
            # LDA model ############################################################
            ########################################################################
            
            # below script defines actions to carry out to compile cleaned data to and LDA model using the TextMineR package
            # corpus is assigned to a dataframe using method used in sentiment analysis data extraction and cleansing reactive (data_extract)
            corpusframe <- data.frame(cleantext = sapply(textcorpus, as.character))
            # unique identified column is created by specifying the row number per instance
            corpusframe$id <- 1:nrow(corpusframe)
            # in order to use data in LDA model, data needs to be assigned to a document term matrix using createdtm function
            # we specify the number of words that can be used in conjunction together using ngram_window
            # this means only two terms can be joined together as terms within the LDA model
            # we also need to specify doc_names in order to compile data to type of documenttermmatrix needed
            # we supply our unique identifier row (elementid) as doc_names in order to accomplish this
            dgc_dtm <- CreateDtm(corpusframe$cleantext,
                                 doc_names = corpusframe$id,
                                 ngram_window = c(1,2))
            # a user input input$ksize is specified in UI portion of script, we will take this as input to out LD model
            # as k size which allows us to specify the number of topics generated by the LDA model
            # we assign this to a variable, which is then assigned to our LDA MODEL
            i = input$ksize2
            # we use fitLDA function to create LDA model
            # specifying our k value (topics outputted) via i variable, obtained from user input intop topic modek control panel
            # we assign our document termmatrix (dgc_dtm) as our data source and specify 500 iterations to complete over data
            # we also specify an optional field calc_coherence in order to output an additional coherence variable
            # which will be used for additional calculations and displayed visually in order to allow the user to determine th quality of the model outputted
            ldafit <- FitLdaModel(dgc_dtm, k = i, iterations = 500, calc_coherence = TRUE)
            # to determine quality of model, we need both a coherence and prevelance score
            # coherence is outputted with our LDA model
            # however we need to calculate prevelance manually
            # in doing this, we divide col sumns for theta (also output from out LDA MODEL) by the total value for theta and * 100
            ldafit$prevalence <- colSums(ldafit$theta)/sum(ldafit$theta) * 100
            # in order to define an easily interpretable output, we need to obtain topterms per topic
            # we use gettopterms function to achieve this
            # we specify phi as phi output from LDA model and M as the quantity of top terms we want to output
            # here we specify M value as the user input into input$topterms2, allowing user to specify quantity of top terms outputted
            ldafit$topterms <- GetTopTerms(phi = ldafit$phi, M = input$topterms2)
            # to output prevelance, coherence and top terms from each topic in a readable table,
            # we needed to assign all values to a dataframe, formatting decimal places of coherence and prevelance with round function
            #  we then needed to assign top terms matrix to dataframe
            # to do this, we create a function which will collapse terms in matrix to a single string. we apply this to topterms using the apply function
            ldafit$summary <- data.frame(topic = rownames(ldafit$phi),
                                         coherence = round(ldafit$coherence,3),
                                         prevalence = round(ldafit$prevalence,3),
                                         top_terms = apply(ldafit$topterms,2,function(x){paste(x,collapse = ", ")}))
            # we change rownames of outputted table to null and assign to variable ldasummary
            ldasummary <- ldafit$summary %>%
                `rownames<-`(NULL)
            # finally we output the ldasummary table
            ldasummary
        }})
    
    # below reactive defines the output for topic model visualizations
    # this is conditional upon get_data2 button being clicked by user
    topicmodelgraphoutput <- eventReactive(input$get_data2, {
        # we first read in table variable from data_extract_2 and assign to variable ldasummary
        ldasummary <- data_extract_2()
        # table output below will create a dual lniechart output for both coherence and prevelance
        # with a score defined per topic
        # we use pivotlonger to assign coherence and prevelance to correct format
        ldasummary %>% pivot_longer(cols = c(coherence,prevalence)) %>%
            # this is then paired with ggplot2 visualization package to produce linechart output
            ggplot(aes(x = factor(topic,levels = unique(topic)), y = value, group = 1)) +
            geom_point() + geom_line() +
            facet_wrap(~name,scales = "free_y",nrow = 2) +
            theme_minimal() +
            labs(title = "topics by coherence and prevalence score",
                 x = "Topics", y = "Value")
    })
    
    # Sentiment visualizations outputs #########################################
    ############################################################################
    
    # below reactive defines sentiment analysis visualizations outputs
    # cleaned and formatted data is read from data_extract reactive and processed through sentiment analysis
    # models selected by user. data is then sent to visualization based on user specification.
    sentiment_output <-  eventReactive(input$get_data,{
        # required inputs are called
        req(input$SelectSentiment)
        req(input$sentimentdictionary)
        # below if statement defines functions to be performed if selectsentiment == SentimentR - emotion
        # sentimentR - emotion will output a bar chart giving score per emotion type e.g. anger, trust etc
        if(input$SelectSentiment == "sentimentR - emotion"){
            # text column is extracted from data_extract reactive and assigned to x
            x <- data_extract()[,1]
            # get_sentences function is used to extract individual sentences per string
            x <- sentimentr::get_sentences(x)
            # emotion function is used to get emotion sentiment score per sentence
            x <- emotion(x)
            # in order to output a table that can be read correctly by ggplot,
            # we need to use group_by function in conjunction with summarize to get a numeric score per emotion type
            x <- x %>%
                group_by(emotion_type) %>%
                summarise(sumsentiment = sum(emotion))
            # this is then read into a ggplot geom_bar plot
            # data input and x and y variables are defined for plot
            xgraph <- ggplot(data=x, aes(x=emotion_type, y=sumsentiment)) +
                # fill si defined by emotion_type
                geom_bar(aes(fill = emotion_type),stat="identity") +
                # theme is defined and text_angle for x axis is given as 90 degrees in order to improve readability
                theme(axis.text.x = element_text(angle = 90))
            # finally graph is outputted to be read by output area
            xgraph
        }
        # below if statement defines functions to be performed if selectsentiment == SentimentR
        else if(input$SelectSentiment == "sentimentR") {
            # data_extract reactive is assigned to x variable
            x <- data_extract()
            
            # if statement below defines functions to carry out if user selects histogram within sentimentRplot
            if(input$sentimentRplot == "histogram") {
                # sentiment_by is used to extract sentiment per text string
                xi <- sentiment_by(x$cleantext, x$element_id)
                # this is then added as additional columns to x variables using cbind
                x <- cbind(x,xi)
                # finally data is sent to ggplot to plot as histogram
                # data, x and y variables for output plot are defined
                # in order to aid the appearance of visualization, we splice the colour output of fill area by 100
                # which will give a large quantity of colour hues
                p <- ggplot(x, aes(ave_sentiment, fill = cut(ave_sentiment, 100))) +
                    # we then define as a histogram using geom_histogram, remove legends and set out binwidth
                    geom_histogram(show.legend = FALSE, binwidth = .01) +
                    # minimal theme is used
                    theme_minimal() +
                    # we define labels for x an y
                    labs(x = "sentiment polarity", y = "n") +
                    # plot title is created
                    ggtitle("sentiment score")
                # scale fill is then used in order to define the distribution of colour within bins of histogram
                p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
            }
            #below if statement defines functions to be performed when user selects barchart from sentimentRplot
            else if(input$sentimentRplot == "barchart") {
                # in order to implement a percentage label per positive and negative sentiment onto our barchart
                # we first need to create a function that will take a decimal value and output as a percentage value
                # in order to do this we create function below
                # function formats data, taking x (decimal value) as input, * 100 and thenb concatenating with % character
                # in order to produce a percentage value
                # we will use this later in reactive to assign percentage to decimal values 
                # before assigning to barchart labels within ggplot
                percent <- function(x, digits = 0, format = "f", ...) {      
                    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
                }
                # we again use sentiment_by to output sentiment score per text string
                xi <- sentiment_by(x$cleantext, x$element_id)
                # we join this output to x variable containing our text string, unique identifier and data source columns
                x <- cbind(x,xi)
                # we then need to assign sentiment scores to 4 variables
                # these are
                # 1. positive Twitter
                # 2. negative twitter
                # 3. positive reddit
                # 4. negative reddit
                
                # we use dplyr to filter based on site column = twitter
                # we then take a count of number of rows outputted by this filter
                # we then add two columns to identify as sentiment_type = positive and site = twitter
                # this process is repeated for the other 3 variable
                # negative twitter, positive reddit and negative reddit, created below
                pos_tw_score <- x %>%
                    filter(ave_sentiment > 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Twitter")
                
                neg_tw_score <- x %>%
                    filter(ave_sentiment < 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Twitter")
                
                pos_rd_score <- x %>%
                    filter(ave_sentiment > 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Reddit")
                
                neg_rd_score <- x %>%
                    filter(ave_sentiment < 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Reddit")
                
                # these are then bound to a single variable using rbind
                sentiment_score_bind <- rbind(pos_tw_score, neg_tw_score, pos_rd_score, neg_rd_score)
                
                # we then need to create a decimal value, which will allow us to interpret
                # a percentage score for each of four created variables
                # we use dplyr mutate function to create a new row which is the output of the count of each of four variables dviided by the total count
                sentiment_percentage_bind <- sentiment_score_bind %>%
                    mutate(total_percentage = n/sum(n))
                
                # finally, we assign this to a ggplot item to plot the data as a barchart
                # we define data, x and y variables
                xgraph <- ggplot(data=sentiment_percentage_bind, aes(x=site, y=total_percentage)) +
                    # we define fill as sentiment_type e.g. positive/negative
                    geom_bar(aes(fill = sentiment_type),stat="identity") +
                    # we then define text_angle for x axis in order to improve readability of visualization for users
                    theme(axis.text.x = element_text(angle = 90)) +
                    # we then define labels per each section of bar chart
                    # here we use the percent function defined at the start of this reactive to take 
                    # the decimal input of total_percentage column and output in percentage format
                    geom_text(aes(label = percent(total_percentage)))
                # we then output the plot which can then be read by output area
                xgraph
            }
            
        }
        # below else statement defines the functions to be performed if syuzhet is selected from selectsentiment input area
        else {
            # data_extract reactive is read from data_extract area
            # and assigned to text_score_c_bind variable
            text_score_c_bind <- data_extract() 
            # we use get_sentiment function in syuzhet to extract sentiment from cleantext variable
            # we take user selected input string from sentimentdictionary input as our lexicon value (method)
            text_senti <- get_sentiment(text_score_c_bind$cleantext, method = input$sentimentdictionary)
            # we then add this sentiment output (text_senti) to variable read from data_extract
            # containing cleaned text, data source and unique identifier variables
            text_score_c_bind <- cbind(text_score_c_bind, text_senti)
            # below if statement defines functions performed if barchart is selected as output from syuzhet model by user
            if(input$syuzhetplot == "barchart") {
                # as sentimentR barchart output above, we create percent function, read data to 4 variables
                # then join and assign to ggplot output
                # we then output plot to be read by plot output area
                percent <- function(x, digits = 0, format = "f", ...) {
                    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
                }
                
                pos_tw_score <- text_score_c_bind %>%
                    filter(text_senti > 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Twitter")
                
                neg_tw_score <- text_score_c_bind %>%
                    filter(text_senti < 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Twitter")
                
                pos_rd_score <- text_score_c_bind %>%
                    filter(text_senti > 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Reddit")
                
                neg_rd_score <- text_score_c_bind %>%
                    filter(text_senti < 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Reddit")
                
                sentiment_score_bind <- rbind(pos_tw_score, neg_tw_score, pos_rd_score, neg_rd_score)
                
                sentiment_percentage_bind <- sentiment_score_bind %>%
                    mutate(total_percentage = n/sum(n))
                
                xgraph <- ggplot(data=sentiment_percentage_bind, aes(x=site, y=total_percentage)) +
                    geom_bar(aes(fill = sentiment_type),stat="identity") +
                    theme(axis.text.x = element_text(angle = 90))+
                    geom_text(aes(label = percent(total_percentage)))
                xgraph
            }
            # as sentimentR histogram output above, we take variable outputted
            # in this case text_score_c_bind and assign to histogram in ggplot, following same steps as previous histogram creation
            # in sentimentR histogram created above
            else if(input$syuzhetplot == "histogram") {
                p <- ggplot(text_score_c_bind, aes(text_senti, fill = cut(text_senti, 100))) +
                    geom_histogram(show.legend = FALSE, binwidth = .1) +
                    theme_minimal() +
                    labs(x = "sentiment polarity", y = "n") +
                    ggtitle("sentiment score")
                p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
            }
            
            # unique to syuzhet, we also create a wordcloud
            # below if statement defines functions to be carried out if user 
            # selects wordcloud from syuzhetplot conditional input
            else if(input$syuzhetplot == "wordcloud") {
                # data extracted from data_extract reactive
                x <- data_extract()
                # we use dplyr select function to assign 
                # just cleantext variable to x variable
                x <- x %>%
                    select(cleantext)
                # in order to process data into wordcloud, 
                # we need to separate data by word and then obtain sentiment per word
                # tom do this, we use unnest_tokens to assign each individual word to a seperate instance
                # taking the cleantext variable as input and creating new output "text"
                x <- x %>%
                    unnest_tokens(word, cleantext)
                # we then use an inner join to join these words with sentiment scores per word
                xsentiment <- inner_join(x, get_sentiments("bing")) %>%
                    # we then take a count of values outputted
                    count(word, sentiment, sort = TRUE) %>%
                    # we then use acast to reformat data for wordcloud input
                    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                    # next we assign this to comparison cloud with negative words filled as red
                    # and positive words filled as blue
                    comparison.cloud(colors = c("red", "blue")
                                     # we define the maximum quantity of words that can be inputted to word cloud
                                     ,max.words = 50)
                # we then output this plot to be read by plot output
                xsentiment
            }
            
        }
        
    })
    
    # below reactive defines plot output items that will be included in second plot area of sentiment analysis tabitem
    # script used is similar to sentiment_output
    sentiment_output2 <-  eventReactive(input$get_data,{
        req(input$SelectSentiment)
        req(input$sentimentdictionary)
        # defines actions to be carried out if sentimentR is selected from selectsentiment
        if(input$SelectSentiment == "sentimentR") {
            x <- data_extract()
            xi <- sentiment_by(x$cleantext, x$element_id)
            x <- cbind(x,xi)
            # defines actions carried out if histogram is selected
            if(input$sentimentRplot1 == "histogram") {
                p <- ggplot(x, aes(ave_sentiment, fill = cut(ave_sentiment, 100))) +
                    geom_histogram(show.legend = FALSE, binwidth = .01) +
                    theme_minimal() +
                    labs(x = "sentiment polarity", y = "n") +
                    ggtitle("sentiment score")
                p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
            }
            # if statement defines actions to be carried out if barchart is selected from sentimentRplot1
            # script uses same techniques defined inside counterpart if statement in sentiment_output reactive
            else if(input$sentimentRplot1 == "barchart") {
                percent <- function(x, digits = 0, format = "f", ...) {
                    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
                }
                pos_tw_score <- x %>%
                    filter(ave_sentiment > 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Twitter")
                
                neg_tw_score <- x %>%
                    filter(ave_sentiment < 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Twitter")
                
                pos_rd_score <- x %>%
                    filter(ave_sentiment > 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Reddit")
                
                neg_rd_score <- x %>%
                    filter(ave_sentiment < 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Reddit")
                
                sentiment_score_bind <- rbind(pos_tw_score, neg_tw_score, pos_rd_score, neg_rd_score)
                
                sentiment_percentage_bind <- sentiment_score_bind %>%
                    mutate(total_percentage = n/sum(n))
                
                xgraph <- ggplot(data=sentiment_percentage_bind, aes(x=site, y=total_percentage)) +
                    geom_bar(aes(fill = sentiment_type),stat="identity") +
                    theme(axis.text.x = element_text(angle = 90))+
                    geom_text(aes(label = percent(total_percentage)), vjust = 0.2)
                xgraph
            }
        }
        # else if statement describes actions to be carried out if syuzhet is selected from selectsentiment
        else if(input$SelectSentiment == "syuzhet") {
            # if statement defines actions to be carried out if barchart is selected from sentimentRplot1
            # script uses same techniques defined inside counterpart if statement in sentiment_output reactive
            if(input$syuzhetplot1 == "barchart") {
                percent <- function(x, digits = 0, format = "f", ...) {
                    paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
                }
                text_score_c_bind <- data_extract()
                text_senti <- get_sentiment(text_score_c_bind$cleantext, method = input$sentimentdictionary)
                text_score_c_bind <- cbind(text_score_c_bind, text_senti)
                pos_tw_score <- text_score_c_bind %>%
                    filter(text_senti > 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Twitter")
                
                neg_tw_score <- text_score_c_bind %>%
                    filter(text_senti < 0, site == "Twitter") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Twitter")
                
                pos_rd_score <- text_score_c_bind %>%
                    filter(text_senti > 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "positive", site = "Reddit")
                
                neg_rd_score <- text_score_c_bind %>%
                    filter(text_senti < 0, site == "Reddit") %>%
                    count() %>%
                    mutate(sentiment_type = "negative", site = "Reddit")
                
                sentiment_score_bind <- rbind(pos_tw_score, neg_tw_score, pos_rd_score, neg_rd_score)
                
                sentiment_percentage_bind <- sentiment_score_bind %>%
                    mutate(total_percentage = n/sum(n))
                
                xgraph <- ggplot(data=sentiment_percentage_bind, aes(x=site, y=total_percentage)) +
                    geom_bar(aes(fill = sentiment_type),stat="identity") +
                    theme(axis.text.x = element_text(angle = 90))+
                    geom_text(aes(label = percent(total_percentage)), vjust = -0.2)
                xgraph
            }
            # if statement defines actions to be carried out if histogram is selected from sentimentRplot1
            # script uses same techniques defined inside counterpart if statement in sentiment_output reactive
            else if(input$syuzhetplot1 == "histogram") {
                text_score_c_bind <- data_extract()
                text_senti <- get_sentiment(text_score_c_bind$cleantext, method = input$sentimentdictionary)
                text_score_c_bind <- cbind(text_score_c_bind, text_senti)
                p <- ggplot(text_score_c_bind, aes(text_senti, fill = cut(text_senti, 100))) +
                    geom_histogram(show.legend = FALSE, binwidth = .1) +
                    theme_minimal() +
                    labs(x = "sentiment polarity", y = "n") +
                    ggtitle("sentiment score")
                p + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
            }
            
            # if statement defines actions to be carried out if wordcloud is selected from sentimentRplot1
            # script uses same techniques defined inside counterpart if statement in sentiment_output reactive
            else if(input$syuzhetplot1 == "wordcloud") {
                x <- data_extract()
                x <- x %>%
                    select(cleantext)
                x <- x %>%
                    unnest_tokens(word, cleantext)
                xsentiment <- inner_join(x, get_sentiments("bing")) %>%
                    count(word, sentiment, sort = TRUE) %>%
                    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                    comparison.cloud(colors = c("red", "blue")
                                     ,max.words = 50)
                xsentiment
            }
            
        }
        
    })
    
    output$out1 <- renderPrint(if (input$SelectSentiment == "syuzhet") 
    {input$syuzhetplot}
    else if(input$SelectSentiment == "sentimentR") {
        input$sentimentRplot
    })
    
    # output areas####################################################################
    ##################################################################################
    
    # below defines server script for output area sentiment_output reactive
    # depending on user selection, either a barchart containing values
    # for percentage of negative/positive and data source
    # a histogram containing total distribution of sentiment polarity between
    # reddit and twitter or a comparison cloud containing top positive and negative words
    # from data source will display
    # output$ calls output area sentiment_visual defined in UI portion of the script
    # sentiment_output() calls the final output from sentiment_output, in this
    # case a ggplot output and displays within sentiment_visual defined in UI portion of the script
    
    output$sentiment_visual <- renderPlot({
        sentiment_output()
        
    })
    
    # below defines server script for output area sentiment_output2 reactive
    # depending on user selection, either a barchart, histogram or comparison cloud will display
    # output$ calls output area sentiment_visual2 defined in UI portion of the script
    # sentiment_output() calls the final output from sentiment_output, in this
    # case a ggplot output and displays within sentiment_visual defined in UI portion of the script
    output$sentiment_visual2 <- renderPlot({
        sentiment_output2()
        
    })
    
    # below output defines plot output for topic modelling line chart
    # this will output a faceted line chart (two line charts will be outputted, grouped by coherence/prevelance score)
    # output$ calls topic_visual output area, defined in UI portion of script
    # final output from topicmodelgraphoutput is called inside the reactive area
    output$topic_visual <- renderPlot({
        topicmodelgraphoutput()
        
    })
    
    # below output defines table output for topic modelling tabitem
    # rendertable is used in order to allow an output of type table
    # output$ is used to call topic_visual2 from UI portion of script
    # final output from data_extract_2 is called inside the reactive area
    output$topic_visual2 <- renderTable({
        data_extract_2()
        
    })
    
    # download outputs##########################################################
    ############################################################################
    
    # Sentiment Analysis download
    # below reactive defines server function for passing dataset created to a csv file
    # values for download are passed to downloadhandler function
    # below we create two functions in order to name the output file
    # and in order to populate file with data from sentimenttable reactive
    output$download <- downloadHandler(
        # we pass below function to filename
        # function generates file name based on user inputted string
        filename = function() {
            paste0(input$string_to_search, ".csv")
        },
        # we pass below function to content
        # function writes the final output of sentimenttable reactive to a csv file
        content = function(file) {
            write.csv(sentimenttable(), file)
        }
    )
    # topic model output
    # below download area defines a csv file to be outputted from topic model output
    # file name is generated based on user inputted string
    # content is generated by writing data_extract_2 to csv file
    output$download2 <- downloadHandler(
        filename = function() {
            paste0(input$string_to_search2, ".csv")
        },
        content = function(file) {
            write.csv(data_extract_2(), file)
        }
    )
    
}
# Run the application 
shinyApp(ui = ui, server = server)

############################################################################################################################
######################## online resources used in creating code ##############################################################################
############################################################################################################################
# 1. methods for creating a dashboard accessing twitter - https://www.infoworld.com/article/3516150/create-a-shiny-app-to-search-twitter-with-rtweet-and-r.html
# 2. using conditional panels in dashboard layout- https://shiny.rstudio.com/reference/shiny/1.6.0/conditionalPanel.html
# 3. using download buttons - https://shiny.rstudio.com/reference/shiny/1.0.4/downloadButton.html
# 4. writing dataset to downloadable csv file - https://mastering-shiny.org/action-transfer.html
# 5. troubleshooting access key issues when hosting dashboard on shinyapps.io http://amsantac.co/blog/en/2016/05/28/twitter-r.html
# 6. using eventreactive to only carry out server functions when get_data button is clicked - https://riptutorial.com/shiny/example/32341/eventreactive
# 7. using TwitteR package - https://www.rdocumentation.org/packages/twitteR/versions/1.1.9
# 8. using RedditExtractoR package - https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf
# 9. using tm package to clean data - https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# 10. creating LDA model, coherence and prevelance scores and formatting data to be output as table and faceted linechart - https://rpubs.com/jojoecp/643113
# 11. creating coloured and stylized histogram output - https://www.r-bloggers.com/2017/05/pretty-histograms-with-ggplot2/
# 12. formatting data for and creating comparison cloud- https://www.tidytextmining.com/sentiment.html


