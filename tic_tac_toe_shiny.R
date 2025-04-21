library(shiny)

# Load Q-table
q_table <- readRDS("tic_tac_toe_qtable.rds")

# Winner check function
check_winner <- function(board) {
  wins <- list(
    c(1,2,3), c(4,5,6), c(7,8,9),
    c(1,4,7), c(2,5,8), c(3,6,9),
    c(1,5,9), c(3,5,7)
  )
  for (combo in wins) {
    if (board[combo[1]] != "" &&
        board[combo[1]] == board[combo[2]] &&
        board[combo[2]] == board[combo[3]]) {
      return(board[combo[1]])
    }
  }
  if (all(board != "")) return("Draw")
  return(NULL)
}

# Convert board to state string
board_to_state <- function(board) paste(board, collapse = "")

resample <- function(x, ...) x[sample.int(length(x), ...)]

# AI move using Q-table
get_best_move <- function(board) {
  state <- board_to_state(board)
  legal_moves <- which(board == "")
  
  if (!state %in% names(q_table)) {
    return(resample(legal_moves, 1))
  }
  
  q_vals <- q_table[[state]]
  legal_q <- q_vals[as.character(legal_moves)]
  
  # Fallback if none are valid
  if (length(legal_q) == 0 || all(is.na(legal_q))) {
    return(resample(legal_moves, 1))
  }
  
  max_q <- max(legal_q, na.rm = TRUE)
  best_moves <- as.integer(names(legal_q)[which(legal_q == max_q)])
  resample(best_moves, 1)
}

# UI
ui <- fluidPage(
  titlePanel("Tic Tac Toe with Q-Learning AI"),
  h3(textOutput("status")),
  uiOutput("boardUI"),
  actionButton("reset", "Reset Game")
)

# Server
server <- function(input, output, session) {
  board <- reactiveVal(rep("", 9))
  turn <- reactiveVal("X")  # Human = X, AI = O
  winner <- reactiveVal(NULL)
  
  observeEvent(input$reset, {
    board(rep("", 9))
    turn("X")
    winner(NULL)
  })
  
  # AI move
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
        } else {
          turn("X")
        }
      })
    }
  })
  
  # UI for board
  output$boardUI <- renderUI({
    b <- board()
    buttons <- lapply(1:9, function(i) {
      actionButton(
        inputId = paste0("cell", i),
        label = b[i],
        style = "height:60px;width:60px;font-size:24px;",
        disabled = b[i] != "" || !is.null(winner()) || turn() != "X"
      )
    })
    div(style = "display:grid;grid-template-columns:repeat(3, 60px);gap:5px;", buttons)
  })
  
  # Human move
  lapply(1:9, function(i) {
    observeEvent(input[[paste0("cell", i)]], {
      if (turn() == "X" && board()[i] == "" && is.null(winner())) {
        new_board <- board()
        new_board[i] <- "X"
        board(new_board)
        
        result <- check_winner(new_board)
        if (!is.null(result)) {
          winner(result)
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
