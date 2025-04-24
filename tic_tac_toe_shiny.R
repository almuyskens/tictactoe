
library(shiny)
library(shinyjs)
library(plotly)

# Load Q-table
q_table <- readRDS("tic_tac_toe_qtable.rds")

# Winner check
check_winner <- function(board) {
  wins <- list(
    c(1,2,3), c(4,5,6), c(7,8,9),
    c(1,4,7), c(2,5,8), c(3,6,9),
    c(1,5,9), c(3,5,7)
  )
  for (combo in wins) {
    if (board[combo[1]] != " " &&
        board[combo[1]] == board[combo[2]] &&
        board[combo[2]] == board[combo[3]]) {
      return(board[combo[1]])
    }
  }
  if (all(board != " ")) return("Draw")
  return(NULL)
}

board_to_state <- function(board) paste(board, collapse = "")
resample <- function(x, ...) x[sample.int(length(x), ...)]

# AI move using Q-table
get_best_move <- function(board) {
  state <- board_to_state(board)
  legal_moves <- which(board == " ")
  
  if (!state %in% names(q_table)) return(resample(legal_moves, 1))
  
  q_vals <- q_table[[state]]
  legal_q <- q_vals[as.character(legal_moves)]
  
  if (length(legal_q) == 0 || all(is.na(legal_q))) return(resample(legal_moves, 1))
  
  max_q <- max(legal_q, na.rm = TRUE)
  best_moves <- as.integer(names(legal_q)[which(legal_q == max_q)])
  resample(best_moves, 1)
}

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Tic Tac Toe with Q-Learning AI"),
  
  fluidRow(
    column(5,
           textOutput("status"),
           uiOutput("boardUI"),
           br(),
           actionButton("reset", "Reset Game")
    ),
    column(4,
           div(
             id = "scoreboard",
             style = "border: 2px solid #007BFF;
                      border-radius: 10px;
                      padding: 15px;
                      background-color: #f0f8ff;
                      box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
                      text-align: left;
                      width: 100%;
                      margin-top: 20px;",
             h4("🎯 Game Scoreboard"),
             tags$hr(),
             htmlOutput("game_counter"),
             br(),
             plotlyOutput("win_chart", height = "250px")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  board <- reactiveVal(rep(" ", 9))
  turn <- reactiveVal("X")
  winner <- reactiveVal(NULL)
  
  counters <- reactiveValues(
    x_wins = 0,
    o_wins = 0,
    draws = 0
  )
  
  observeEvent(input$reset, {
    board(rep(" ", 9))
    turn("X")
    winner(NULL)
  })
  
  output$game_counter <- renderUI({
    HTML(paste0(
      "<b>X wins:</b> ", counters$x_wins, "<br>",
      "<b>O wins:</b> ", counters$o_wins, "<br>",
      "<b>Draws:</b> ", counters$draws
    ))
  })
  
  output$win_chart <- renderPlotly({
    total <- counters$x_wins + counters$o_wins + counters$draws
    if (total == 0) {
      plot_ly(type = "pie", labels = c("X", "O", "Draw"), values = c(1, 1, 1),
              textinfo = "label+percent", marker = list(colors = c("dodgerblue", "tomato", "gray")),
              showlegend = TRUE) %>%
        layout(title = "Win Percentage")
    } else {
      plot_ly(
        type = "pie",
        labels = c("X Wins", "O Wins", "Draws"),
        values = c(counters$x_wins, counters$o_wins, counters$draws),
        textinfo = "label+percent",
        marker = list(colors = c("dodgerblue", "tomato", "gray")),
        showlegend = TRUE
      ) %>%
        layout(title = "Win Percentage")
    }
  })
  
  animate_scoreboard <- function(result) {
    if (result == "X") {
      runjs("document.getElementById('scoreboard').style.backgroundColor = '#d4edda';")
    }
    if (result == "O") {
      runjs("document.getElementById('scoreboard').style.backgroundColor = '#f8d7da';")
    }
    if (result == "Draw") {
      runjs("document.getElementById('scoreboard').style.backgroundColor = '#e2e3e5';")
    }
    runjs("setTimeout(function() {
             document.getElementById('scoreboard').style.backgroundColor = '#f0f8ff';
           }, 500);")
  }
  
  observe({
    if (turn() == "O" && is.null(winner())) {
      isolate({
        new_board <- board()
        move <- get_best_move(new_board)
        new_board[move] <- "O"
        board(new_board)
        
        result <- check_winner(new_board)
        if (!is.null(result)) {
          winner(result)
          if (result == "X") counters$x_wins <- counters$x_wins + 1
          if (result == "O") counters$o_wins <- counters$o_wins + 1
          if (result == "Draw") counters$draws <- counters$draws + 1
          animate_scoreboard(result)
        } else {
          turn("X")
        }
      })
    }
  })
  
  output$boardUI <- renderUI({
    b <- board()
    buttons <- lapply(1:9, function(i) {
      actionButton(
        inputId = paste0("cell", i),
        label = b[i],
        style = "height:100px;width:100px;font-size:40px;",
        disabled = b[i] != " " || !is.null(winner()) || turn() != "X"
      )
    })
    div(style = "display:grid;grid-template-columns:repeat(3, 100px);gap:5px;", buttons)
  })
  
  lapply(1:9, function(i) {
    observeEvent(input[[paste0("cell", i)]], {
      if (turn() == "X" && board()[i] == " " && is.null(winner())) {
        new_board <- board()
        new_board[i] <- "X"
        board(new_board)
        
        result <- check_winner(new_board)
        if (!is.null(result)) {
          winner(result)
          if (result == "X") counters$x_wins <- counters$x_wins + 1
          if (result == "O") counters$o_wins <- counters$o_wins + 1
          if (result == "Draw") counters$draws <- counters$draws + 1
          animate_scoreboard(result)
        } else {
          turn("O")
        }
      }
    })
  })
  
  output$status <- renderText({
    if (!is.null(winner())) {
      if (winner() == "Draw") "It's a draw!" else paste("Winner:", winner())
    } else {
      paste("Turn:", turn())
    }
  })
}

shinyApp(ui = ui, server = server)

