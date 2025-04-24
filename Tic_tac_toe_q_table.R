# train_q_table_fixed.R
rm(list=ls())
initialize_board <- function() rep(" ", 9)

is_terminal <- function(board) !is.null(get_winner(board)) || all(board != " ")

available_moves <- function(board) which(board == " ")

flip <- function(state) {
  state <- gsub("X", "Z", state)
  state <- gsub("O", "X", state)
  state <- gsub("Z", "O", state)
  return(state)
}

board_to_state <- function(board, flipped = FALSE) {
  state <- paste(board, collapse = "")
  if (flipped) {
    return(flip(state))
  }
  return(state)
}

get_winner <- function(board) {
  lines <- list(
    c(1,2,3), c(4,5,6), c(7,8,9),
    c(1,4,7), c(2,5,8), c(3,6,9),
    c(1,5,9), c(3,5,7)
  )
  for (line in lines) {
    if (board[line[1]] != " " &&
        board[line[1]] == board[line[2]] &&
        board[line[1]] == board[line[3]]) {
      return(board[line[1]])
    }
  }
  return(NULL)
}

resample <- function(x, ...) x[sample.int(length(x), ...)]

epsilon_greedy <- function(q_table, state, legal_actions, epsilon) {
  legal_actions <- as.character(legal_actions)
  state_q <- q_table[[state]]  
  if (runif(1) < epsilon) {
    return(as.integer(resample(legal_actions, 1)))
  } else {
    legal_q <- state_q[legal_actions]
    max_q <- max(legal_q, na.rm = TRUE)
    best_actions <- names(legal_q)[which(legal_q == max_q)]
    return(as.integer(resample(best_actions, 1)))
  }
}

train_q_learning <- function(
  episodes = 100000,
  alpha = 0.1,
  gamma = 0.9,
  epsilon_start = 1.0,
  epsilon_end = 0.01,
  epsilon_decay = 0.0001
) {
  q_table <- list()
  for (ep in 1:episodes) {
    board <- initialize_board()
    player <- "X"
    epsilon <- epsilon_end + (epsilon_start - epsilon_end) * exp(-epsilon_decay * ep)
    
    while (!is_terminal(board)) {
      flipped <- player == "X"
      state <- board_to_state(board, flipped = flipped)
      legal_moves <- available_moves(board)

      if (!state %in% names(q_table)) {
        q_table[[state]] <- setNames(rep(0, 9), as.character(1:9))
      }
      
      action <- epsilon_greedy(q_table, state, legal_moves, epsilon)
      board[action] <- player
      new_state <- board_to_state(board, flipped = flipped)
      
      if (!new_state %in% names(q_table)) {
        q_table[[new_state]] <- setNames(rep(0, 9), as.character(1:9))
      }
      
      reward <- 0
      winner <- get_winner(board)
      
      if (!is.null(winner)) {
        reward <- 1  # Player just won
      } else {
        # Simulate opponent's best response
        opponent <- ifelse(player == "X", "O", "X")
        opp_moves <- available_moves(board)
        for (opp_move in opp_moves) {
          temp_board <- board
          temp_board[opp_move] <- opponent
          opp_win <- get_winner(temp_board)
          if (!is.null(opp_win) && opp_win == opponent) {
            reward <- -1  # Penalize for enabling an immediate loss
            break
          }
        }
      }
      # Q-learning update
      old_q <- q_table[[state]][as.character(action)]
      
      next_legal_moves <- available_moves(board)
      if (length(next_legal_moves) == 0 || is_terminal(board)) {
        future_q <- 0
      } else {
        future_q <- max(q_table[[new_state]][as.character(next_legal_moves)], na.rm = TRUE)
      }
      
      q_table[[state]][as.character(action)] <- old_q + alpha * (reward + gamma * future_q - old_q)

      if (is_terminal(board)) {
        break
      }
      
      player <- ifelse(player == "X", "O", "X")
    }
    
    if (ep %% 5000 == 0) cat("Episode:", ep, "/", episodes, "| epsilon:", round(epsilon, 4), "| Q-table size:", length(q_table), "\n")
  }
  
  return(q_table)
}

# Train and save
set.seed(42)
q_table <- train_q_learning()
saveRDS(q_table, "tic_tac_toe_qtable.rds")

