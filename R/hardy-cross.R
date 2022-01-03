#' Applies the Hardy-Cross method to solve for pipe flows in a network.
#'
#' This function uses the Hardy-Cross method to iteratively solve the
#' equations for conservation of mass and energy in a water pipe network.
#' The input consists of a data frame with the pipe characteristics and lists 
#' of the pipes in each loop (listed in a clockwise direction) and the initial
#' guesses of flows in each pipe (positive flows are in a clockwise direction).
#' 
#' The input data frame with the pipe data must contain a pipe ID column with 
#' the pipe numbers used in the loops input list. There are three options for 
#' input column of the pipe roughness data frame:
#' \tabular{ll}{
#'   \strong{Column Name} \tab \strong{Approach for Determining r} \cr
#'   ks \tab {f calculated using Colebrook equation, r using Darcy-Weisbach} \cr
#'   f \tab {f treated as fixed, r calculated using Darcy-Weisbach} \cr
#'   r \tab {r treated as fixed}
#'   }
#'   In the case where absolute pipe roughness, \eqn{ks} (in m or ft), is input, 
#'   the input pipe data frame must also include columns for the length, \eqn{L} and
#'   diameter, \eqn{D}, (both in m or ft) so \eqn{r} can be calculated. In this case, 
#'   a new \eqn{f} and \eqn{r} are calculated at each iteration, the final values of 
#'   which are included in the output. If input \eqn{r} or \eqn{f} columns are provided, values 
#'   for \eqn{ks} are ignored. If an input \eqn{r} column is provided, \eqn{ks} and \eqn{f} are 
#'   ignored. If the Colebrook equation is used to determine \eqn{f}, a water 
#'   temperature of 20^{o}C or 68^{o}F is used.
#'   
#'   The number of iterations to perform may be specified with the n_iter input
#'   value, but execution stops if the average flow adjustment becomes smaller 
#'   than 1% of the average flow in all pipes.
#'
#' @param dfpipes data frame with the pipe data. Format is described above, but must contain a column named _ID_.
#' @param loops integer list defining pipes in each loop of the network.
#' @param Qs numeric list of initial flows for each pipe in each loop [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#' @param n_iter integer identifying the number of iterations to perform.
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units.
#' @param ret_units If set to TRUE the value(s) returned for pipe flows are of 
#' class \code{units} with units attached to the value. [Default is FALSE]
#'
#' @return Returns a list of two data frames:
#' \itemize{
#'   \item dfloops - the final flow magnitude and direction (clockwise positive) for each loops and pipe
#'   \item dfpipes - the input pipe data frame, with additional columns including final Q
#' }
#'
#' @details The Darcy-Weisbach equation is used to estimate the head loss in each
#' pipe segment, expressed in a condensed form as \eqn{h_f = rQ^{2}}
#' where: \deqn{r = \frac{8fL}{\pi^{2}gD^{5}}}  
#' If needed, the friction factor \eqn{f} is calculted using the Colebrook 
#' equation. The flow adjustment in each loop is calculated at each iteration as: 
#' \deqn{\Delta{Q_i} = \frac{\sum_{j=1}^{p_i} r_{ij}Q_j|Q_j|}{\sum_{j=1}^{p_i} 2r_{ij}Q_j^2}}
#' where \eqn{i} is the loop number, \eqn{j} is the pipe number, and \eqn{\Delta{Q_i}} 
#' is the flow adjustment to be applied to each pipe in loop \eqn{i} for the next iteration.
#'
#' @examples
#'
#' #              A----------B --> 0.5m^3/s
#' #              |\   (4)   |
#' #              | \        |
#' #              |  \       |
#' #              |   \(2)   |
#' #              |    \     |(5)
#' #              |(1)  \    |
#' #              |      \   |
#' #              |       \  |
#' #              |        \ |
#' #              |   (3)   \|
#' # 0.5m^3/s --> C----------D
#' 
#' #Input pipe characteristics data frame. With r given other columns not needed
#' dfpipes <- data.frame(
#' ID = c(1,2,3,4,5),                     #pipe ID
#' r = c(200,2500,500,800,300)            #resistance used in hf=rQ^2
#' )
#' loops <- list(c(1,2,3),c(4,2,5))
#' Qs <- list(c(0.3,0.1,-0.2),c(0.2,-0.1,-0.3))
#' hardycross(dfpipes = dfpipes, loops = loops, Qs = Qs, n_iter = 1, units = "SI")
#'
#' @seealso \code{\link{colebrook}}, \code{\link{darcyweisbach}}
#'
#' @name hardycross
NULL
assign_pipe_r <- function(dfp, g) {
  dfp$r <-  8.0*dfp$f*dfp$L/(g*pi^2*dfp$D^5)
  return(dfp)
}

assign_pipe_Q <- function(dfp, lps, qs) {
  #add flow magnitudes to pipes data frame -- direction corresponds to clockwise
  # direction (+) in first loop with pipe segment in it
  dfp$Q <- NA
  for (i in 1:length(lps)) {
    pipe_Q <- unlist(qs[[i]])
    for(j in 1:length(pipe_Q)) {
      pipe_num <- unlist(lps[[i]])
      if(length(pipe_Q) != length(pipe_num)) {
        s <- sprintf("No. of pipes and No. of flows not equal, loop %i\n",i)
        stop(s)
      }
      #only assign flow if one doesn't already exist
      if( is.na(dfp$Q[dfp$ID == pipe_num[j]]) ) {
        dfp$Q[dfp$ID == pipe_num[j]] <- pipe_Q[j]
      } else {
        #shared pipes must have equal, opposite values
        if(pipe_Q[j] == 0) {
          reldiff <- 0
        } else {
          reldiff <- (dfp$Q[dfp$ID == pipe_num[j]]+pipe_Q[j])/abs(pipe_Q[j])
        }
        if(reldiff > 0.01) {
          txt <- sprintf("check flows at pipe %d\n Q1= %.4f Q2= %.4f\n", dfp$ID,
                         dfp$Q[dfp$ID == pipe_num[j]],-pipe_Q[j])
          message(txt)
          stop("Shared pipe must have same magnitude, opposite direction\n")
        }
      }
    }
  }
  #check that all pipes have a Q value
  if (sum(complete.cases(dfp$Q)) != nrow(dfp)) {
    stop("Not all pipes have assigned flows\n")
  }
  return(dfp)
}

assign_pipe_f <- function(dfp, nu) {
  dfp$f <- NA
  for (i in 1:nrow(dfp)) {
    #if flow in pipe is zero use fully rough approximation
    if(abs(dfp$Q[i]) < 0.000001) {
      dfp$f[i] <- (1/(2*log10(3.7/(dfp$ks[i]/dfp$D[i]))))^2
    } else {
      dfp$f[i] <- colebrook(ks = dfp$ks[i], V = velocity(dfp$D[i], dfp$Q[i]), 
                          D = dfp$D[i], nu = nu)
    }
  }
  return(dfp)
}

find_shared_pipes <- function(lps) {
  #input is a list with loop pipe IDs
  shared <- data.frame(loop=character(), shared_loop=character(), 
                       shared_pipe=integer())
  x <- gtools::permutations(length(lps),2)
  for(i in 1:nrow(x)) {
    lp_orig <- x[i,1]
    lp_shared <- x[i,2]
    shared_pipes <- intersect(unlist(lps[[lp_orig]]),unlist(lps[[lp_shared]]))
    if(length(shared_pipes>0))  {
      for(j in length(shared_pipes)) {
        shared <- rbind(shared,data.frame(loop=lp_orig,shared_loop=lp_shared,
                                          shared_pipe=shared_pipes[j]))
      }
    }
  }
  return(shared)
}

calc_loop_dQ <- function(lp, qs, dfp) {
  num <- 0.0
  denom <- 0.0
  for(m in 1:length(lp)) {
    pipenum <- lp[m]
    q <- qs[m]
    r <- dfp$r[dfp$ID == pipenum]
    num <- num + r*q*abs(q)
    denom <- denom + 2*r*abs(q)
  }
  return(-1*num/denom)
}

#' @export
#' @rdname hardycross
hardycross <- function (dfpipes = dfpipes, loops = loops, Qs = Qs, n_iter = 1, 
                      units = c("SI", "Eng"), ret_units = FALSE ) {

  #Check input data and formats
  units <- units
  if (units == "SI") {
    g <- 9.80665     # m / s^2
    nu = suppressMessages(kvisc(units = "SI"))
  } else if (units == "Eng") {
    g <- 32.2        #ft / s^2
    nu = suppressMessages(kvisc(units = "Eng"))
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Select either SI of Eng.")
  }
  if( !is.data.frame(dfpipes) ) {
    stop("dfpipes must be a data frame\n")
  }
  if( !is.list(loops) | !is.list(Qs) ) {
    stop("Loops and Qs must be lists\n")
  }
  if ( length(Qs) != length(loops )) {
    stop("Lists for Qs and loops must have equal length.")
  }
  #Drop units if they exist
  dfpipes <- units::drop_units(dfpipes)
  for( i  in 1:length(Qs) ) {
    if(class(Qs[[i]]) == "units" ) assign(Qs[[i]], units::drop_units(Qs[[i]]))
  }
  
  #assign initial flows to dfpipes
  dfpipes <- assign_pipe_Q(dfpipes, loops, Qs)
  Qavg <- mean(dfpipes$Q, na.rm = TRUE)
  
  #find if f, r missing and fill in pipe dataframe
  fixed_f <- FALSE
  fixed_r <- FALSE
  if ( is.null(dfpipes$r)){
    if ( is.null(dfpipes$f)){
      if ( is.null(dfpipes$ks)){
        stop("Input pipe data must include at least one of r, f or ks\n")
      } else { #ks has values, f does not
        dfpipes <- assign_pipe_f(dfpipes, nu)
        dfpipes <- assign_pipe_r(dfpipes, g)
        message("Using ks values to calculate f and r\n")
      }
    } else { #f has values, r does not
      dfpipes <- assign_pipe_r(dfpipes, g)
      fixed_f <- TRUE
      fixed_r <- TRUE
      message("Using fixed f values to calculate fixed r\n")
    }
  } else { #r has values, f, ks do not
    fixed_r <- TRUE
    message("Using fixed r values\n")
  }
  
  #find shared edge between two loops -- check that shared flows are reversed
  shared <- find_shared_pipes(loops)
  
  ############MAIN CALCULATIONS#################################

  for (k in 1:n_iter){
    #find dQ in each loop and apply it to each pipe in that loop
    dQ <- rep(0, length(loops))
    for (i in 1:length(loops)) {
      dQ[i] <- calc_loop_dQ(unlist(loops[[i]]), unlist(Qs[[i]]), dfpipes)
      message(sprintf("Iteration: %d, Loop: %d, dQ: %.5f",k,i,dQ[i]))
      for(j in 1:length(loops[[i]])) {
        Qs[[i]][j] <- Qs[[i]][j] + dQ[i]
      }
    }
    
    #apply flow corrections to shared segments
    for (i in 1:nrow(shared)) {
      loop_to_adj <- shared$loop[i]
      dQ_from_loop <- shared$shared_loop[i]
      pipe_to_adj <- shared$shared_pipe[i]
      #find index of shared pipe in loop to adjust
      idx <- match(pipe_to_adj,unlist(loops[[loop_to_adj]]))
      Qs[[loop_to_adj]][idx] <- Qs[[loop_to_adj]][idx] - dQ[dQ_from_loop]
    }
    #check for when to end calculations
    dQfrac <- mean(abs(dQ))/Qavg
    if(dQfrac < 0.01) {
      txt <- sprintf("Reached convergence criterion at iteration %d\n",k)
      message(txt)
      break
    }
    #if only ks specified, update f, r values
    if ( !(fixed_f) & !(fixed_r)) {
      dfpipes <- assign_pipe_Q(dfpipes, loops, Qs)
      dfpipes <- assign_pipe_f(dfpipes, nu)
      dfpipes <- assign_pipe_r(dfpipes, g)
    }
  }
  
  #assign final flows to dfpipes and create output data frame for loops
  dfpipes <- assign_pipe_Q(dfpipes, loops, Qs)
  #create output df for loops
  dfloops <- data.frame(loop=integer(), pipe=integer(), flow=numeric())
  for (i in 1:length(loops)) {
    dfloops <- rbind(dfloops,data.frame(loop=rep(i,length(loops[[i]])), 
                      pipe=unlist(loops[[i]]), flow=unlist(Qs[[i]])))
  }
  
  #assign units to dfloops columns if needed
  if( ret_units ) {
    units::units_options(allow_mixed = TRUE)
    if (units == "SI") {
      out_units <- c("1","1","m^3/s")
    } else {
      out_units <- c("1","1","ft^3/s")
    }
    for(i in 1:ncol(dfloops)){
      units(dfloops[ , i]) <- out_units[i]
    }
  }
 
  return(list(dfloops = dfloops, dfpipes = dfpipes))
}
