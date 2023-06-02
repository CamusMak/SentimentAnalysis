# pacman::p_load(stringr,
#                tidyverse,
#                tidytext,
#                textdata,
#                shiny,
#                shinydashboard,
#                sentimentr,
#                plotly,
#                irlba,
#                DT,
#                RColorBrewer)

library(stringr)
library(tidyverse)
library(tidytext)
library(textdata)
library(shiny)
library(shinydashboard)
library(sentimentr)
library(plotly)
library(irlba)
library(DT)
library(RColorBrewer)


df <- read_csv('AIdata.csv')


full_df <- read_csv('merged_last.csv') %>% select(-output)

bert <- read_csv('Bert.csv') 

bert <- bert %>% rename(Tag='...1',
                        Contains_fear='Contains fear') %>%
  mutate(Tag=iconv(Tag, "latin1", "ASCII", sub=""))%>%
  mutate(Tag=gsub("[[:punct:][:digit:]]+", "", Tag)) %>%
  filter(Tag!='')%>%distinct()


tokens <- df %>% 
  unnest_tokens(word,output)  %>%
  mutate(word = iconv(word, "latin1", "ASCII", sub=""))%>%
  filter(word != '')


p1 <- tokens %>%
  anti_join(stop_words,by='word') %>%
  count(word,index,sort=TRUE) %>%
  inner_join(get_sentiments('afinn'),by='word') %>%
  group_by(word)%>%
  summarise(contribution=sum(n*value))



tf_idf <- tokens %>%
  anti_join(stop_words,by='word')%>% 
  count(index,word)%>%
  filter(!str_detect(word,"\\d+"))%>%
  bind_tf_idf(word,index,n)


tf_idf_mean <- tf_idf %>%
  group_by(index) %>%
  summarise(tf_mean=mean(tf),idf_mean=mean(idf),tf_idf_mean=mean(tf_idf))



df_sentiment <- sentiment(df$output)%>%
  group_by(element_id)%>%
  summarise(sentiment_mean=mean(sentiment)) %>%
  rename(index=element_id)



df_all_sentiments <- merge(tf_idf,tf_idf_mean,by='index')
df_all_sentiments <- merge(df_all_sentiments,df_sentiment,by='index')



# pca

# preparing data for PCA

scaled_word_matrix <- tf_idf%>%select(index,word,n) %>% 
  cast_sparse(index,word,n) %>%
  scale()





#  app


linebreaks <- function(n){
  HTML(strrep(br(),n))
}


ui <- dashboardPage(
  
  dashboardHeader(title="Sentiment Analisys"),
  
  dashboardSidebar(),
  
  dashboardBody(
    
    
    
    
    # sentiment analysis
    
    fluidRow(
      box(title=h3("Sentiment analysis with 'bing' and 'affin' libraries",
                   style='font-size:40px; color:black;'),
          hr(style='border-top: 2px solid; border-color:black;'),
          
          width = 12,
          box(width = 2,background = 'blue',
              
              selectizeInput(
                inputId='sent_library',
                label='Library',
                selected='affin',
                choices=c("bing", "afinn")),
              
              numericInput(
                inputId='n_max',
                label='Top N sentiments',
                value=14,
                min=2,
                max=40),
              
              selectizeInput(
                inputId='min_or_max',
                label='Min or Max',
                selected='max',
                choices=c('min','max'))),
          
          column(8,offset = 1,
                 plotlyOutput('words_scores')))),
    
    # principal component analysis
    
    fluidRow(
      box(width = 12,
          title=h3('Sentiment data table',
                   style='font-size:40px; color:black;'),
          hr(style='border-top: 2px solid; border-color:black;'),
          
          dataTableOutput('sentiment_data_table')
      )
    )
    
    

    fluidRow(
      box(width = 12,title=h3('Principal Component Analaysis',
                              style='font-size:40px; color:black;'),
          br(),
          hr(style='font-size: 2px solid; border-color:black;'),


          box(width = 12,
              box(width = 2,background = 'blue',
                  numericInput(
                    inputId='n_components',
                    label="N conponents",
                    value=10,
                    min=2,
                    max=dim(scaled_word_matrix)[2])),
              column(5,
                     plotlyOutput('variance_viz')),

              column(4,offset = 1,
                     dataTableOutput('pca_variance'))),


          br(),
          box(width = 12,
              column(8,
                     dataTableOutput('pca_df_wide')),
              column(4,
                     dataTableOutput('pca_df_long'))),

          br(),

          box(width = 12,
              title=h3("PCA result",style='font-size:40px; color:black'),
              box(width = 2,background = 'blue',
                  numericInput(
                    inputId='top_n_for_all',
                    label='Top N',
                    value=50,
                    min=2,
                    max=1000)),
              box(width = 10,
                  plotOutput('all_pca_result'))),

          box(width = 12,
              title=h3('Relative importance of words in Principal Components',
                       style='font-size:35px; color:black;'),
              box(width = 2,background = 'blue',

                  uiOutput('principal_component_ui'),

                  numericInput(
                    inputId='n_tags',
                    label='Number of tags',
                    value=20,
                    min=1,
                    max=dim(scaled_word_matrix)[1])),

              box(width = 10,

                  plotlyOutput('principal_components_bar')),

              box(width = 12,
                  title=h3('Comarison of two Principal Components',
                           style='font-size:35px; color:black;'),
                  box(width = 2, background = 'blue',

                      uiOutput('pc_1'),
                      uiOutput('pc_2'),

                      numericInput(
                        inputId='slice_n',
                        label='Top N',
                        value=8,
                        min=2,
                        max=1000)),

                  box(width=8,
                      plotlyOutput('pc_vs_pc')))
          )),


      box(width = 12,
          title=h3('Sentiment analysis based on BERT, RoBert',
                   style='font-size:35px; color:black;'),
          box(width = 6,
              plotlyOutput('bert_histogram_excitement')),
          box(width = 6,
              plotlyOutput('bert_histogram_fear'))

      ),


      box(width = 12,
          title=h3('Final Data',
                   style='font-size:35px; color:black;'),
          hr(style='font-size: 2px solid; border-color:black;'),

          box(width = 12,
              title=h4('Sentiment for per article',
                   style='font-size:30px; color:black;'),
            dataTableOutput('final_data_index_wise')),

          box(width = 12,
              title=h4('Sentiment for per word',
                     style='font-size:30px; color:black;'),
            dataTableOutput('final_data_word_wise')),

          box(width = 12,
              title=h4('Article & Fear/Excitement',style='font-size:30px; color:black;'),

              box(width = 12,
                  box(width = 2,background = 'blue',
                    radioButtons(
                      inputId='sentiment',
                      label='Sentiment',
                      selected='Fear',
                      choices=c('Fear','Excitement'))),
                  box(width = 10,
                      plotlyOutput('per_index_hist'))),
              #
              box(width = 12,
                  box(width = 2,background = 'blue',
                      radioButtons(
                        inputId='sentiment_2',
                        label='Sentiment',
                        selected='Positive',
                        choices=c('Negative','Positive','Neutral'))),
                  box(width = 10,
                      plotlyOutput('per_index_hist_2'))),

              box(width = 12,
                  dataTableOutput('fear_ext'))
              )


      ),


      box(width = 12,
          title=h3('Linear regression',
                style='font-size:35px; color:black;'),
          hr(style='font-size: 2px solid; border-color:black;'))
      # row ends

    )

    
    # ------
    
    
  )
  
  
  
  
)



server <- function(input,output, session){
  
  
  
  lib_sentiment_data <- reactive({

    tokens <- df%>% unnest_tokens(word,output)

    sentiment_df <- data.frame()

    if (input$sent_library=='afinn'){

      sentiment_df <- tokens %>%
        anti_join(stop_words,by='word')%>%
        count(word,index,sort=TRUE) %>%
        inner_join(get_sentiments('afinn'),by='word') %>%
        group_by(word) %>%
        summarise(contribution=sum(n*value))

    }
    else if(input$sent_library=='bing'){
      sentiment_df <- tokens %>%
        anti_join(stop_words,by='word')%>%
        count(word,index,sort=TRUE) %>%
        inner_join(get_sentiments('bing'),by='word') %>%
        mutate(sentiment=ifelse(sentiment=='negative',-1,1)) %>%
        group_by(word) %>%
        summarise(contribution=sum(n*sentiment))

    }



    if (input$min_or_max=='min'){
      return(sentiment_df%>% slice_min(abs(contribution),n=input$n_max) %>%
               mutate(word = reorder(word, contribution)))
    }

    return(sentiment_df%>% slice_max(abs(desc(contribution)),n=input$n_max) %>%
             mutate(word = reorder(word, contribution)))





  })

  output$words_scores <- renderPlotly({

    lib_sentiment_data()  %>%
      plot_ly(type='bar',
              x=~contribution,
              y=~word) %>%
      layout(title=list(text=paste("Sentiment analysis with ",input$sent_library,' library')),

             xaxis=list(title=list(text='Contribution',
                                   font=list(size=20)),
                        linecolor='black',
                        linewidth=0.5,
                        mirror=T),

             yaxis=list(title=list(text='Word',
                                   font=list(size=20),
                                   standoff=40L),
                        linecolor='black',
                        linewidth=0.5,
                        mirror=T)

      )


  })


  # sentiment data table

  sentiment_data_table <- reactive({

    return(datatable(df_all_sentiments %>%
                       rename("Index of article"=index,
                              "Word"=word,
                              "Count of 'word' in an article"=n,
                              "TF"=tf,
                              "IDF"=idf,
                              "TF-IDF"=tf_idf,
                              "Mean of TF"=tf_mean,
                              "Mean of IDF"=idf_mean,
                              "Mean of TF-IDF"=tf_idf_mean,
                              "Mean of sentiment"=sentiment_mean),

                     options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                  pageLength =10,
                                  dom='Bfrtip',
                                  scrollX=T,
                                  scrollY=T),
                     rownames=F,
                     filter='top'))

  })


  output$sentiment_data_table <- renderDataTable(sentiment_data_table())


  # PCA
  #
  pca_result <- reactive({

    return(prcomp_irlba(scaled_word_matrix,n=input$n_components))


  })


  wide_pca_df <- reactive({
    return(bind_cols(Tag=colnames(scaled_word_matrix),
                     pca_result()$rotation))


  })



  output$pca_df_wide <- renderDataTable(caption=h4('PCA result: wide data',style='font-size:20px; color:black'),
                                        wide_pca_df(),
                                        options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                     pageLength =10,
                                                     dom='Bfrtip',
                                                     scrollX=T,
                                                     scrollY=T),
                                        rownames=F,
                                        filter='top')


  long_pca_df <- reactive({
    return(wide_pca_df()%>%
             gather(PC,Contribution,
                    PC1:as.name(colnames(wide_pca_df())[length(colnames(wide_pca_df()))])))
  })
  #
  output$pca_df_long <- renderDataTable(caption=h4('PCA result: long data',style='font-size:20px; color:black'),
                                        long_pca_df(),
                                        options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                     pageLength =10,
                                                     dom='Bfrtip',
                                                     scrollX=T,
                                                     scrollY=T),
                                        rownames=F,
                                        filter='top')

  # PCA variance

  pca_variance <- reactive({
    sum_res <- summary(pca_result())$importance

    pca_var <- data.frame(index=seq(1:input$n_components),
                          PC=names(sum_res[1,]),
                          Variance=sum_res[1,],
                          Proportion_variance=sum_res[2,],
                          Cumulative_proportion=sum_res[3,]) %>%
      arrange(index)

    return(pca_var)

  })

  # variance visualization

  output$variance_viz <- renderPlotly({

    pca_variance() %>% plot_ly(x=~index,
                               y=~Cumulative_proportion,
                               type='scatter',
                               mode='lines',
                               marker=list(size=10,
                                           color='orange')) %>%
      layout(
        title=list(text='Change of cumulative variance proportion'),
        xaxis=list(title="Number of Principal Components"),
        yaxis=list(title="Cumulative proportion")
      )
  })

  # pca variance table

  output$pca_variance <- renderDataTable(pca_variance()%>%
                                           select("Principal Component"=PC,
                                                  "Standard deviation" = Variance,
                                                  "Proportion of Variance"=Proportion_variance,
                                                  "Cumulative Proportion"=Cumulative_proportion),
                                         rownames=F,
                                         options=list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))


  # ui principal_component

  output$principal_component_ui <- renderUI({
    selectInput(
      inputId='principal_component',
      label='Principal Component',
      selected='PC1',
      choices= pca_variance()%>%select(PC)%>%distinct()%>%pull(),
      multiple=FALSE)
  })


  # pca_all_result
  output$all_pca_result <- renderPlot({

    long_pca_df() %>% slice_max(Contribution,by=PC,n=input$top_n_for_all)%>%
      ggplot(aes(Tag,Contribution,color=Tag)) +
      geom_col(show.legend = F) +
      theme_classic()+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      facet_wrap(~ PC,ncol=2)
  })


  # bar plot principal component

  output$principal_components_bar <- renderPlotly({

    map_dfr(c(-1, 1) * input$n_tags,
            ~ long_pca_df() %>%
              filter(PC %in% input$principal_component) %>%
              top_n(.x, Contribution)) %>%
      mutate(Tag = reorder(Tag, Contribution)) %>%

      plot_ly(x=~Tag,
              y=~Contribution,
              color=~Tag,
              type='bar',
              colors = colorRampPalette(brewer.pal(8,'Set2'))(8)) %>%
      layout(title=list(text=input$principal_component),
             xaxis=list(title=list(text='Word',standoff=15L)),
             yaxis=list(title=list(title='Relative importance in PC'),
                        line=list(color='black',size=0.5)))
  })



  # pc vs pc

  output$pc_1 <- renderUI({
    selectizeInput(
      inputId='pc_1',
      label='PC 1',
      selected='PC1',
      choices= pca_variance()%>%select(PC)%>%distinct()%>%pull())

  })


  output$pc_2 <-renderUI({
    selectizeInput(
      inputId='pc_2',
      label='PC 2',
      selected='PC2',
      choices= pca_variance()%>%select(PC)%>%distinct()%>%pull())
  })

  output$pc_vs_pc <- renderPlotly({
   ggplotly(wide_pca_df() %>% slice_head(n=input$slice_n)%>% ggplot(aes(get(input$pc_1),get(input$pc_2),label=Tag))+
               geom_text(check_overlap = TRUE)+
               labs(title=paste(input$pc_1," & ",input$pc_2),
                    x=input$pc_1,
                    y=input$pc_2) +
               theme_classic()
    )
  })


  output$bert_histogram_excitement <- renderPlotly({
    bert %>% plot_ly(
      type='histogram',
      x=~excitement) %>%
      layout(title=list(text='Excitement class probabilities'),
             xaxis=list(title='Probabilities of Excitement class'),
             yaxis=list(title='Frequency'))
  })

  output$bert_histogram_fear<- renderPlotly({
    bert %>% plot_ly(
      type='histogram',
      x=~Contains_fear) %>%
      layout(title=list(text='Fear class probabilities'),
             xaxis=list(title='Probabilities of Excitement class'),
             yaxis=list(title='Frequency'))
  })

  # final part

  final_data_ <- reactive({
    fn_data <- merge(df_all_sentiments%>%mutate(Tag=word)%>%select(-word),wide_pca_df(),by='Tag')
    fn_data <- merge(fn_data,bert,by='Tag')

    return(fn_data)
  })


  output$final_data_index_wise <- renderDataTable(final_data_(),
                                       options=list(
                                         scrollX=TRUE,
                                         scrollY=TRUE))

  output$final_data_word_wise <- renderDataTable(merge(final_data_()%>%
                                                   select(-c(index,n,tf_mean,idf_mean,
                                                             tf_idf_mean,
                                                             sentiment_mean))%>%
                                                   distinct(),p1%>%rename(Tag=word,
                                                                          afinn=contribution)),
                                                  options=list(
                                                    scrollX=TRUE,
                                                    scrollY=TRUE))



  # fear ext.

  fear_ext_ <- reactive({

    ddf <- merge(final_data_()%>%select(index,Tag),bert,by='Tag')

    ddf <- ddf %>% group_by(index) %>%
                   summarise(Fear=mean(Contains_fear),
                             Excitement=mean(excitement)) %>%
                   ungroup()

    ddf_affin <- merge(final_data_()%>%select(index,Tag),p1%>%rename(afinn=contribution,Tag=word),by='Tag')%>%
                 group_by(index) %>%
                 summarise(afinn=mean(afinn))

    ddf <- merge(ddf,final_data_()%>%select(index,tf_mean,idf_mean,tf_idf_mean,sentiment_mean),by='index')

    ddf <- merge(ddf,ddf_affin,by='index') %>% distinct()

    ddf <- merge(ddf,full_df,by='index') %>% distinct()

    return(ddf)


  })

  output$fear_ext <- renderDataTable(fear_ext_(),
                                     options=list(
                                       scrollY=T,
                                       scrollX=T
                                     ))

  # article wise histogram

  output$per_index_hist <- renderPlotly({
    fear_ext_() %>% plot_ly(x=~get(input$sentiment),
                            type='histogram') %>%
                    layout(xaxis=list(title=input$sentiment))
  })

  output$per_index_hist_2 <- renderPlotly({
    fear_ext_() %>% plot_ly(x=~get(input$sentiment_2),
                            type='histogram') %>%
      layout(xaxis=list(title=input$sentiment_2))
  })

}


shinyApp(ui,server)




