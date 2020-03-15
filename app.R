library(tidyverse)
library(shiny)
library(shinyMobile)
library(extrafont)

load('./app_data.RData')

# ui ----
ui <- f7Page(
  title = 'The Office Trivia',
  init = f7Init(theme = 'light', filled = T, color = 'black'),
  HTML('<head><style> 
       br+ .external {color: #FFF; font-weight: bold; font-family: Arial Black;} 
       a {font-weight: bold; font-family: Courier New;}
       </style></head>'),
  
  f7SingleLayout(
    
    # navbar
    navbar = f7Navbar(
      title = HTML('<font style="font-family:Courier New; font-size: 18px;"><strong>the office (trivia)</strong></font>'),
      hairline = T, 
      right_panel = T,
      shadow = T
    ),
    br(),
    
    # question topic
    div(uiOutput('topic_out'), style = 'margin-left: 0.5cm; margin-right: 0.5cm;'),
    br(),
    # question
    div(uiOutput('question_out'), style = 'margin-left: 0.5cm; margin-right: 0.5cm;'),
    
    # buttons
    div(uiOutput('button'), style = 'margin-left: 0.5cm; margin-right: 50%;'),
    br(),
    
    # play again
    br(),
    div(uiOutput('play_again'), style = 'margin-left: 0.5cm; margin-right: 50%; font-family: Courier New;'),
    br(),
    br(),
    br(),
    
    # toolbar
    toolbar = f7Toolbar(
      div(uiOutput('score_out'), style = 'margin-left: 0.5cm; margin-right: 0.5cm;'),
      position = 'bottom',
      hairline = T,
      shadow = T
    ),
    
    
    f7Panel(title = 'About the app',
            theme = 'dark',
            side = 'right',
            HTML(
              paste0('TEST your knowledge of <i>The Office</i> with this 10-question quiz.',
                     '<br><br>',
                     '<center><img src="office_logo.PNG" width="80"></center>',
                     '<br>',
                     'DATA powering the app comes from the <i>schrute</i> package and imdb.com.',
                     '<br><br>',
                     '<center>',
                     '<div style="display: inline-block; verical-align: top;">',
                     '<a class="link external" href="https://github.com/bradlindblad/schrute" target="_blank">',
                     '<img src="https://raw.githubusercontent.com/bradlindblad/schrute/master/man/figures/logo.png" width="80">',
                     '</a>',
                     '&nbsp;&nbsp;&nbsp;',
                     '<a class="link external" href="https://www.imdb.com/" target="_blank">',
                     '<img src="imdb_logo.png" width="80">',
                     '</a>',
                     '</div>',
                     '<br><br><br><br><br>',
                     'Created by | <a class="link external" href="https://datascott.com" target="_blank">Scott</a>',
                     '</center>'
                     )
              )
            )
    )
  )


# server ----
server <- function(input, output){
  
  # score ----
  score <- reactiveVal('0 | 0')
  output$score_out <- renderUI({HTML(paste0('correct: <strong>', score(), '</strong>'))})
  
  # topic
  topic <- eventReactive(score(), {
    
      n <- runif(1)
      
      if(n < 0.2) {
        "Who Did It?"
      } else if(n < 0.4) { 
        "According to IMDB"
      } else if(n < 0.65) { 
        "Talkative"
      } else {
        "Who Said It?"
      }
    
  })
  
  output$topic_out <- renderUI({
    # only show for first 10 questions
    if(as.numeric(strsplit(score(), ' \\| ')[[1]][2]) < 10) {
      HTML(paste0('<font style="font-family: Courier New; font-size: 18px;"><strong>', topic(), '</strong></font>'))
    } else {
      HTML(paste0('<font style="font-family: Courier New; font-size: 18px;"><strong>You got ', strsplit(score(), ' \\| ')[[1]][1], ' correct</strong></font>'))
    }
    }) 
  
  # Q&A ----
  question <- reactiveVal(NULL)
  output$question_out <- renderUI({
    # only show question for first 10 questions
    if(as.numeric(strsplit(score(), ' \\| ')[[1]][2]) < 10) {
      HTML(question())
    # show various gifs depending on outcome
    } else {
      # scores 0-3
      if(between(as.numeric(strsplit(score(), ' \\| ')[[1]][1]), 0, 3)) {
        x <- gifs %>% filter(score  == 3)
      } else if(between(as.numeric(strsplit(score(), ' \\| ')[[1]][1]), 4, 7)) {
        x <- gifs %>% filter(score == 7)
      } else {
        x <- gifs %>% filter(score == 10)
      }
      
      HTML(sample(x$links, 1))
      
      }
    })
  
  
  correct_answer <- reactiveVal(NULL)
  
  rvA <- reactiveVal(NULL)
  rvB <- reactiveVal(NULL)
  rvC <- reactiveVal(NULL)
  rvD <- reactiveVal(NULL)
  
  
  # answer buttons ----
  output$button <- renderUI({ 
    # only show for first 10 questions
    if(as.numeric(strsplit(score(), ' \\| ')[[1]][2]) < 10) {
      f7Radio('user_input', '', 
              choices = c(rvA(), rvB(), rvC(), rvD()), 
              selected = NULL)
      }
    })
  
  
  # check answer and update score ----
  new_score <- eventReactive(input$user_input, {
    
    current_right <- strsplit(score(), ' \\| ')[[1]][1]
    current_question <- strsplit(score(), ' \\| ')[[1]][2]
    
    # update for correct answer
    if(!is.null(input$user_input) & input$user_input == correct_answer()) {
        current_right <- as.numeric(current_right) + 1
      }
      
      paste0(current_right, ' | ', as.numeric(current_question) + 1)

  })
  
  observeEvent(new_score(), {
    score(new_score())
  })

  
  # question (dependent on topic)
  observeEvent(score(), {
    
    # who did it ----
    if(topic() == 'Who Did It?') {

      # randomly pick a row
      row <- did_it[sample(1:nrow(did_it), 1), ]
      action <- row$text_w_direction

      # determine appropriate joining word to create a coherent sentence
      if(str_detect(word(action, 1), 'ing')) {join <- 'was '} else {join <- ''}
      if(substr(word(action, 1), nchar(word(action, 1)), nchar(word(action, 1))) == 's' |
         substr(word(action, 1), nchar(word(action, 1)) - 1, nchar(word(action, 1))) == 's,') {join <- ''}
      else if(word(action, 1) %in% unique(said_it$character)){join <- 'has '} else {join <- 'was '}

      # create question: In 'episode name' from season X, who....
      q <- paste0('<font style="font-family: Courier New; font-size: 18px;">In the episode <i>',
                  row$episode_name, '</i> from season ', row$season, ', who ', join, action, '?')


      # answers
      answer <- row$character
      others <- said_it %>%
        filter(episode_name == row$episode_name & character != row$character) %>%
        distinct(character) %>%
        pull(character) %>%
        sample(3)

      answer_pool <- sample(c(answer, others))

    # According to IMDB   ----
    } else if(topic() == 'According to IMDB') {

      # randomly pick a row
      row <- imdb_ratings[sample(1:nrow(imdb_ratings), 1), ]

      # find three other rows that differ in rating from the selected
      row2 <- imdb_ratings %>%
        filter(rating != row$rating) %>%
        sample_n(size = 3, replace = F)

      # combine, randomize order, and create labels to render
      episodes <- bind_rows(row, row2) %>%
        bind_cols(order = data.frame(order = sample(1:4, 4))) %>% 
        arrange(order) %>%
        mutate(label_prefix = paste0('<font style = "font-size:18px"><strong><i>', episode_name, '</i></strong></font>',
                                     '<font style = "font-size:16px"> (Season ', season,', Episode ', episode, ')</font>'),
               label = paste0(label_prefix, '<br>', 
                              '<font style = "font-size:14px; font-family:Courier New;">', description, '</font>')
               )

      # determine max (or min)
      Max <- max(episodes$rating)
      if(sum(episodes$rating >= Max) == 1) {type = 'MAX'} else {type = 'MIN'}

      # create question
      q <- paste0('<font style="font-family: Courier New; font-size: 18px;">',
                  'Which of the following episodes has the ',
                  '<b>',
                  ifelse(type == 'MAX', 'highest ', 'lowest '),
                  '</b>',
                  'rating?</font>',
                  '<br><br>',
                  episodes$label[1], '<br><br>',
                  episodes$label[2], '<br><br>',
                  episodes$label[3], '<br><br>',
                  episodes$label[4]
                  )

      # answers
      if(type == 'MAX') {
        answer <- episodes %>% filter(rating == max(rating)) %>% pull(episode_name)
      } else {
        answer <- episodes %>% filter(rating == min(rating)) %>% pull(episode_name)
      }

      answer_pool <- episodes$episode_name
    

    # talkative ----
    } else if(topic() == 'Talkative') {

      # randomly pick a row
      row <- talkative[sample(1:nrow(talkative), 1), ]

      # find three other rows that differ in rating from the selected
      row2 <- talkative %>%
        filter(n != row$n & episode_name == row$episode_name) %>%
        sample_n(size = 3, replace = F)

      # combine
      out <- bind_rows(row, row2)

      # determine max (or min)
      Max <- max(out$n)
      if(sum(out$n >= Max) == 1) {type = 'MAX'} else {type = 'MIN'}

      # create question
      q <- paste0('<font style="font-family: Courier New; font-size: 18px;">In the episode <i>',
                  row$episode_name, '</i> from season ', row$season, ', who had the ',
                  ifelse(type == 'MAX', 'most ', 'least '),
                  'lines?'
                  )

      # answers
      if(type == 'MAX') {
        answer <- out %>% filter(n == max(n)) %>% pull(character)
      } else {
        answer <- out %>% filter(n == min(n)) %>% pull(character)
      }

      answer_pool <- sample(out$character)
      


    # who said it ----
    } else {
      
      # randomly pick a row
      row <- said_it[sample(1:nrow(said_it), 1), ]
      
      # create question
      q <- paste0('<font style="font-family: Courier New; font-size: 18px;">',
                  '"', row$text, '"<br>',
                  '&nbsp;&nbsp;&nbsp;<i>- Season ', row$season, ', ', row$episode_name, '</i>'
                  )
      
      # answers
      answer <- row$character
      others <- said_it %>%
        filter(episode_name == row$episode_name & character != row$character) %>%
        distinct(character) %>%
        pull(character) %>%
        sample(3)
      
      answer_pool <- sample(c(answer, others))

    }

      # assign to reactiveVals
      question(q)
      correct_answer(answer)
      rvA(answer_pool[1])
      rvB(answer_pool[2])
      rvC(answer_pool[3])
      rvD(answer_pool[4])
    
  },ignoreNULL = T)
  
  # play again button ----
  output$play_again <- renderUI({
    if(as.numeric(strsplit(score(), ' \\| ')[[1]][2]) == 10) {
      f7Button('playagain', 'Play again', color = 'black', fill = T, rounded = T, shadow = T, size = 'small') 
    }
  })
  
  observeEvent(input$playagain, {
    score('0 | 0')
  })
  
}


# run app ----
shinyApp(ui = ui, server = server)