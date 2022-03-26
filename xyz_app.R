library(shiny)
library(shinyWidgets)


# Helper functions

reactiveToVector <- function(user_vector){
  user_vector <- as.numeric(unlist(reactiveValuesToList(user_vector)))

  user_vector
}

difference <- function(x) {
  x[1] - sum(x[-1])
}

rand_riddle <- function() {
  n <- sample(-10:10, 3, replace = TRUE)
  
  sample(c(sum(n), difference(n), min(n) + max(n)))
}

is_correct_answer <- function(truth, answer) {
  answer_vec <- c(sum(answer), difference(answer), min(answer)+max(answer))
  
  all(sort(unique(truth)) == sort(unique(answer_vec)))
}

  



ui <- fluidPage(

  tags$style(
    HTML(
      "
  .container-fluid {
      text-align: center;
      height: calc(80vh - 60px);
      max-width: 500px;
      display: grid;
      grid-template-rows: 2fr auto;
      justify-content: center;
      justify-items: center;
      padding: 10px
  }
  
  #myheader {
  font-weight: bold;
  font-size: 40px;
  color: #9b9b9b;
  padding: 50px;
  }
  
  #riddle {
  display: grid;
  grid-template-columns: repeat(3, 80px);
  gap: 40px;
  padding: 5px;
  justify-content: center;
  }
  
  .riddle_display {
    width: 80px;
    height: 80px;
    display: grid;
    place-content: center;
    border-radius: 10px;
    background-color: #F9E298;
    color: #5C5C5C;
    font-family: 'Open Sans',sans-serif;
    font-size: 17px;
    font-weight: bold;
  }
  
  .guess {
  display: grid;
  grid-template-columns: repeat(3, 80px);
  gap: 40px;
  padding: 5px;
  justify-content: center;
  }
  
  .form-group {
  margin-bottom: 0;
  }
  
  .form-group.shiny-input-container {
    width: 80px;
    height: 80px;
    display: grid;
    place-content: center;
    border-radius: 10px;
    color: #5C5C5C;
    background-color: #9FE2BF;
    font-family: 'Open Sans','Helvetica Neue',Helvetica,Arial,sans-serif;
    font-size: 17px;
  }
  
  .form-control.shiny-bound-input {
    display: inline-block;
    width: 40px;
    height: 30px;
    background-color: #D2F7DD;
    color: #5C5C5C;
    box-shadow: none;
    vertical-align: middle;
    border-color: #D2F7DD;
    font-family: 'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
    padding: 5px;
  }
  
  #validation {
  padding: 15px;
  }
  
  .correct {
    background-color: #9FE2BF; 
    border-style: solid;
    border-radius: 10px;
    border-color: #9FE2BF;
    color: #5C5C5C;
    font-family: 'Open Sans',sans-serif;
    font-size: 17px;
    font-weight: bold;
    padding: 12px 16px;
      }
      
  .false {
    background-color: #F28D8D; 
    border-style: solid;
    border-radius: 10px;
    border-color: #F28D8D;
    color: #5C5C5C;
    font-family: 'Open Sans',sans-serif;
    font-size: 17px;
    font-weight: bold;
    padding: 12px 16px;
  }
  
  .btn-default {
    color: #5C5C5C;
    width: fit-content;
    background-color: #F1F0F0;
    border-color: #5C5C5C;
  }
  
  .btn {
    padding: 8px 14px;
    font-size: 17px;
    border-radius: 10px;
    margin-bottom: 10px;
    font-family: 'Open Sans',sans-serif;
  }
  
  .modal-dialog {
    margin: 100px auto;
    width: 450px;
  }
  
  .modal-content {
    font-family: 'Open Sans', sans-serif;
    color: #5C5C5C;
  }
  
  .modal-header {
    display: grid;
    justify-content: center;
    border-bottom: none;
    padding: 5px;
  }
  
  .modal-footer {
    text-align:center;
    border-top: none;
  }
  "
    )
  ),
  
  uiOutput("myheader"),
  uiOutput("reload_temp"),
  uiOutput("riddle"),
  
  div(formatNumericInput("guess_x", "X", NULL, format = "integer"),
      formatNumericInput("guess_y", "Y", NULL, format = "integer"),
      formatNumericInput("guess_z", "Z", NULL,  format = "integer"),
      class = "guess"),
  
  
  uiOutput("validation"),
  
  actionButton("validate", "Enter", icon=icon("dice")),
  actionButton("help", div("How to play", icon("question")))

)


server <- function(input, output) {
  
  # initial how-to-play message
  showModal(
    modalDialog(title = h3("How to play"),
                HTML("You are given the <i>sum</i>, the <i>difference</i> and the <i>minimum plus the maximum</i> 
                     of three unknown integer numbers (x, y, and z) in random order. The difference is defined
                     as x-y-z and each of the unknown integers may only take on values between -10 and 10.<br>
                     <br>
                     Can you guess a valid combination of (x, y, z) matching the given metrics?"), 
                footer = modalButton("OK"), size = "s", easyClose = TRUE
    )
  )
  
  # how-to-play message on click
  observeEvent(input$help, {
    showModal(
      modalDialog(title = h3("How to play"),
                  HTML("You are given the <i>sum</i>, the <i>difference</i> and the <i>minimum plus the maximum</i> 
                     of three unknown integer numbers (x, y, and z) in random order. The difference is defined
                     as x-y-z and each of the unknown integers may only take on values between -10 and 10.<br>
                     <br>
                     Can you guess a valid combination of (x, y, z) matching the given metrics?"), 
                  footer = modalButton("OK"), size = "s", easyClose = TRUE
      )
    )
  })
  
  output$myheader <- renderUI("Guess XYZ")
  
  game <- reactiveValues(current_riddle = rand_riddle())
  finished <- reactiveVal(FALSE)
  
  output$riddle <- renderUI(purrr::map(game$current_riddle, ~div(., class = "riddle_display")))
  
  reset_game <- function() {
    game$current_riddle <- rand_riddle()
    finished(FALSE)
  }
  
  
  is_valid_entry <- reactive({
    (input$guess_x %in% -10:10 & input$guess_y %in% -10:10 & input$guess_z %in% -10:10)
  })
  
  user_guess <- reactiveValues()

  lapply(paste0("guess_", letters[24:26]), function(x) {
    observeEvent(input$validate, {
      user_guess[[x]] <- input[[x]]
    })  
  })
  
  
  
  observeEvent(input$validate, {
    if (is_valid_entry()) {
      if (is_correct_answer(game$current_riddle, reactiveToVector(user_guess))) {
        finished(TRUE)
      }
      output$validation <- renderUI(
        if (is_correct_answer(game$current_riddle, reactiveToVector(user_guess))) {
          div("Well done", icon("check"), class = "correct")
        } else {
          div("Not correct", icon("times"), class = "false")
        })
    }
  })
  
  # Show 'play again' button when correct answer is given
  output$reload_temp <- renderUI({
    if (!finished())
      return()
    
    actionButton("reload", "Play again", icon=icon("redo"))
  })
  
  
  observeEvent(input$reload, {
    reset_game()
  })
  
}

shinyApp(ui, server)
