library(tidyverse)
library(lubridate)
library(scales)
library(tidyquant)
theme_set(theme_minimal())
invisible(Sys.setlocale("LC_TIME", "en_US.UTF-8"))


#' Set Timesteps
#'
#' @param start data, default is ceilinged current date
#' @param by "year" (default) or "month"
#' @param n number of periods out
#' @param date_of_birth (optional) data of birth, used for computing age
#'
#' @return timesteps
set_timesteps <- function(start = ceiling_date(Sys.Date(), by), by = "year", n = 20,
                          date_of_birth = NULL) {
  date <- seq(from = start, length.out = n, by = by)
  if (!is.null(date_of_birth)) {
    age <- floor(interval(date_of_birth, date)/years(1))
  } else {
    age <- NULL
  }
  return(list(timestep = 1:n,
              date = date,
              age = age))
}

#' Set Expense
#'
#' @param timesteps
#' @param expense0
#' @param inflation
#'
#' @return expense
set_expense <- function(timesteps, expense0, exp_inflation = 0, exp_inflsd = 0) {
  list(expense0 = expense0,
       exp_inflation = exp_inflation,
       exp_inflsd = exp_inflsd)
}

set_income <- function(timesteps, income0, exp_increase = 0, exp_increasesd = 0) {
  list(income0 = income0,
       exp_increase = exp_increase,
       exp_increasesd = exp_increasesd)
}

set_investment <- function(name = NULL,
                           timesteps = timesteps,
                           exp_pret, exp_psd, exp_dret, exp_dsd,
                           from = 1, to = length(timesteps$timestep),
                           percentage = NULL, amount = NULL) {
  list(timesteps = timesteps,
       name = name,
       exp_pret = exp_pret,
       exp_psd = exp_psd,
       exp_dret = exp_dret,
       exp_dsd = exp_dsd,
       from = from,
       to = to,
       percentage = percentage,
       amount = amount)
}

set_cash <- function(timesteps = timesteps, exp_pret = 0, exp_psd = 0, exp_dret = 0.01, exp_dsd = 0,
                     from = 1, to = length(timesteps$timestep),
                     amount = NULL, percentage = NULL) {
  list(timesteps = timesteps,
       name = "CASH",
       exp_pret = exp_pret,
       exp_psd = exp_psd,
       exp_dret = exp_dret,
       exp_dsd = exp_dsd,
       from = from,
       to = to,
       percentage = percentage,
       amount = amount)
}

simulate_wealth <- function(timesteps, wealth0, expense, income,
                            cash, correlations = NULL,
                            investments) {
  wealth0_out <- c(); wealth0_out[1] <- wealth0
  cash0_out <- c(); cash0_out[1] <- max(wealth0*cash$percentage, min(cash$amount, wealth0), 0)
  cash_target <- cash0_out
  investment0_out <- matrix(nrow = length(timesteps$timestep), ncol = length(investment))
  investment0_out[1,] <- sapply(investment, function(item) max(item$amount, item$percentage*wealth0))
  # complete `age` in timesteps (if it's not set)
  if (length(timesteps$age) < length(timesteps$timestep)) {
    timesteps$age <- rep(NA, length(timesteps$timestep))
  }
  # name investments (if some are not set)
  counter <- 1
  investment_names <- sapply(investment, function(item) {
    if (is.null(item$name)) {
      name <- paste("name", counter, sep = "")
      counter <<- counter + 1
      return(name)
    } else {
      return(item$name)
    }
  })
  colnames(investment0_out) <- investment_names
  # use same names for liquidation also
  liquidation_out <- matrix(nrow = length(timesteps$timestep), ncol = length(investment))
  colnames(liquidation_out) <- investment_names
  # for later use
  income_out <- c(); income_out[1] <- income$income0
  expense_out <- c(); expense_out[1] <- expense$expense0
  capincome_out <- c()
  roi_out <- investment0_out
  roi_out[] <- NA
  wealth1_out <- c()
  cash1_out <- c()
  wealth1_out <- c()
  investment1_out <- investment0_out
  investment1_out[] <- NA

  # random returns
  p_rets <- matrix(rnorm(n = 2*length(timesteps$timestep),
                         mean = sapply(investment, function(item) item$exp_pret),
                         sd = sapply(investment, function(item) item$exp_psd)),
                   ncol = ncol(investment0_out), byrow = T)
  d_rets <- matrix(rnorm(n = 2*length(timesteps$timestep),
                         mean = sapply(investment, function(item) item$exp_dret),
                         sd = sapply(investment, function(item) item$exp_dsd)),
                   ncol = ncol(investment0_out), byrow = T)

  for (i in 1:length(timesteps$timestep)) {
    if (i > 1) {
      investment0_out[i,] <- investment1_out[i-1,]
      cash0_out[i] <- cash1_out[i-1]
      wealth0_out[i] <- wealth1_out[i-1]
      cash_target <- max(wealth0_out[i]*cash$percentage, min(cash$amount, wealth0_out[i]))

      income_out[i] <- income_out[i-1]*(1+income$exp_increase)
      expense_out[i] <- expense_out[i-1]*(1+expense$exp_inflation)
    }

    # remove from investment if wealth smaller than investment amounts
    excess <- sum(investment0_out[i,]) - (wealth0_out[i] - cash_target)
    if (excess > 0) {
      for (k in ncol(investment0_out):1) {
        reduction <- min(excess, investment0_out[i,k])
        investment0_out[i,k] <- investment0_out[i,k] - reduction
        excess <- excess - reduction
        if (excess <= 0) break
      }
    }
    cash0_out[i] <- cash_target

    capincome_out[i] <- sum(investment0_out[i,] * d_rets[i,])
    roi_out[i,] <- investment0_out[i,] * p_rets[i,]

    cash1_out[i] <- cash0_out[i] + income_out[i] + capincome_out[i] - expense_out[i]
    investment1_out[i,] <- investment0_out[i,] + roi_out[i]

    wealth1_out[i] <- cash1_out[i] + sum(investment1_out[i,])
  }

  return(list(timesteps = timesteps,
              income = income_out,
              expense = expense_out,
              cash0 = cash0_out,
              investment0 = investment0_out,
              wealth0 = wealth0_out,
              # liquidation = liquidation_out,
              capincome_out = capincome_out,
              roi_out = roi_out,
              cash1 = cash1_out,
              investment1 = investment1_out,
              wealth1 = wealth1_out))

  # cash1_out
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #   income_out[i] <- income_out[i-1]*(1+income$exp_increase)
  #   expense_out[i] <- expense_out[i-1]*(1+expense$exp_inflation)
  #
  #
  #
  #   wealth_out[i] <- cash_out[i] + sum(investment_out[i,])
  #
  #   # rebalancing
  #   cash_out[i] <- max(wealth_out[i]*cash$percentage, min(cash$amount, wealth_out[i]))
  #
  #   excess <- sum(investment_out[i,]) - (wealth_out[i] - cash_out[1])
  #   if (excess > 0) {
  #     for (k in ncol(investment_out):1) {
  #       reduction <- min(excess, investment_out[i,k])
  #       investment_out[i,k] <- investment_out[i,k] - reduction
  #       excess <- excess - reduction
  #       liquidation_out[i,k] <- reduction
  #       if (excess <= 0) break
  #     }
  #   }
  # }
  #

}



