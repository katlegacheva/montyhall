#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title Contestant selects a door
#' @description Randomly selects a door
#' @details Randomly selects a door as initial pick for the contestant
#' @param . . . no arguments
#' @return contestant's initial pick
#' @examples select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Host opens a door with a goat behind it
#' @description After contestant selects a door, the host reveals a door with a goat behind it.
#' @details If the contestant's initial pick has a goat behind it, the host opens the only remaining door with a goat. If the contestant's initial pick has a car behind it, the host randomly selects one of the goat doors to open.
#' @param game The contents behind of the three doors corresponding to the numbers of the doors in order.
#' @return the number of the door that was opened by the host (goat behind the door)
#' @examples open_goat_door(c("car","goat","goat"),2)
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title Contestant choses to switch doors or stick with initial pick.
#' @description Contestant gets an option to switch his initial selection after host reveals a door with a goat behind it.
#' @details If the contestant chooses to stay with with the initial pick, the final pick is the same as the initial. If the contestant chooses to switch his pick, the function selects the only door that is left after eliminating the initial pick and the door opened by the host.
#' @param stay The strategy used (T = stay, F= switch)
#' @param opened.door Door opened by the host
#' @param a.pick initial pick of the contestant
#' @return The final pick of the contestant
#' @examples change_door(stay = T, 2, 1)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title Determine if the contest won or lost the game
#' @description Determines the results of the game
#' @details If the final pick of the contestant contains a "car" behind it, the contest won. If there a=is a "goat", then the contestant lost.
#' @param final.pick The final pick of the contestant
#' @param game the order of the contents of the doors.
#' @return Results of the game - "WIN" or "LOST"
#' @examples determine_winner(2, c("car","goat","goat"))
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title Play the whole game
#' @description Plays the whole game from the start to the end
#' @details Generates the contents of the three doors, has the contestant select an initial pick out of the three doors. Then has the host reveal a goat door. After that, the contestnat is given an option to use a "switch" or a "stay" strategy. This function explores both possibilities. Then collects thte results  into a data frame.
#' @param . . . no arguements
#' @return Results of the game based on the strategy used.
#' @examples play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Repeating the game n times to see results
#' @description Runs the game n amount of times and collects the results and strategy used.
#' @details Runs the game n amount of times, then collects the reults into a data frame based on if the results are "WON" or "LOST" and the strategy used - "stay" or "switch"
#' @param n amount of times the game will be run
#' @return Data frame of the results and the strategy used
#' @examples play_n_game(100)
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
