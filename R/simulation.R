# simulation

simulate_reservoir <- function(s_cap, s, i, params){


  I <- i[!is.na(i)]
  S <- vector("numeric", length = length(I) + 1)
  S[1] <- s[1]
  R <- vector("numeric", length = length(I))


  r_ <- params[1] * mean(I)
  hedge_factor <- params[2]

  # 0 means hedge is target
  # 1 means hedge is s_cap

  ((s_cap - r_) * hedge_factor) + r_ -> hedge_level



  for (t in 1:length(I)){

    # set the target release
    target <- r_#(r_[mth[t],][["r"]] * mean(I)) + r

    if(S[t] + I[t] < hedge_level){
      target <- (target / hedge_level) * (S[t] + I[t])
    }

    # simulate
    if ((S[t] - target + I[t]) > s_cap) {
      S[t + 1] <- s_cap
      R[t] <- S[t] + I[t] - s_cap
    }
    else {
      if (S[t] - target + I[t] < 0) {
        S[t + 1] <- 0
        R[t] <- max(0, S[t] + I[t])
      }
      else {
        S[t + 1] <- S[t] + I[t] - target
        R[t] <- target
      }
    }
  }

  return(
    tibble(s_sim = S[-length(S)], r_sim = R,
           s_obs = s[1:(length(I))])
  )
}


