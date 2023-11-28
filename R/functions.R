#' Set Timesteps
#'
#' @param start data, default is ceilinged current date
#' @param by "year" (default) or "month"
#' @param n number of periods out
#' @param date_of_birth (optional) data of birth, used for computing age
#'
#' @return timesteps
#' @export
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
#' @export
set_expense <- function(timesteps, expense0, exp_inflation = 0, exp_inflsd = 0) {
  list(expense0 = expense0,
       exp_inflation = exp_inflation,
       exp_inflsd = exp_inflsd)
}

#' Title
#'
#' @param timesteps
#' @param income0
#' @param exp_increase
#' @param exp_increasesd
#'
#' @return income list
#' @export
set_income <- function(timesteps, income0, exp_increase = 0, exp_increasesd = 0) {
  list(income0 = income0,
       exp_increase = exp_increase,
       exp_increasesd = exp_increasesd)
}

#' Title
#'
#' @param name
#' @param timesteps
#' @param exp_pret
#' @param exp_psd
#' @param exp_dret
#' @param exp_dsd
#' @param from
#' @param to
#' @param percentage
#' @param amount
#'
#' @return list
#' @export
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

#' Title
#'
#' @param timesteps
#' @param exp_dret
#' @param exp_dsd
#' @param from
#' @param to
#' @param amount
#' @param percentage
#'
#' @return list
#' @export
set_cash <- function(timesteps = timesteps, #exp_pret = 0, exp_psd = 0,
                     exp_dret = 0.01, exp_dsd = 0,
                     from = 1, to = length(timesteps$timestep),
                     amount = NULL, percentage = NULL) {
  list(timesteps = timesteps,
       name = "CASH",
       # exp_pret = exp_pret,
       # exp_psd = exp_psd,
       exp_dret = exp_dret,
       exp_dsd = exp_dsd,
       from = from,
       to = to,
       percentage = percentage,
       amount = amount)
}

#' Title
#'
#' @param tax_brackets
#' @param marginal_rates
#' @param deductable
#'
#' @return list
#' @export
set_incometax <- function(tax_brackets, marginal_rates, deductable = 0) {
  list(tax_brackets = tax_brackets,
       marginal_rates = marginal_rates,
       deductable = deductable)
}

#' Title
#'
#' @param tax_brackets
#' @param marginal_rates
#' @param deductable
#'
#' @return list
#' @export
set_wealthtax <- function(tax_brackets, marginal_rates, deductable = 0) {
  list(tax_brackets = tax_brackets,
       marginal_rates = marginal_rates,
       deductable = deductable)
}

#' Title
#'
#' @param x
#' @param tax
#'
#' @return list
#' @export
calculate_tax <- function(x, tax) {
  if (length(tax$tax_brackets) != length(tax$marginal_rates)) {
      stop("tax$tax_brackets and tax$marginal_rates must be of equal length.")
  }
  out <- 0
  x <- max(x-tax$deductable, 0)
  previous_bracket_limit <- 0
  for (i in 1:length(tax$tax_brackets)) {
    bracket_limit <- tax$tax_brackets[i]
    rate <- tax$marginal_rates[i]
    if (x > bracket_limit) {
      out <- out + (bracket_limit - previous_bracket_limit) * rate
    } else {
      out <- out + (x - previous_bracket_limit) * rate
      break
    }
    previous_bracket_limit <- bracket_limit
  }
  return(out)
}

#' Title
#'
#' @param timesteps
#' @param wealth0
#' @param expense
#' @param income
#' @param cash
#' @param correlations
#' @param investment
#' @param incometax
#' @param wealthtax
#'
#' @return x
#' @export
simulate_wealth <- function(timesteps, wealth0, expense, income,
                            cash, correlations = NULL,
                            investment, incometax, wealthtax) {
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
  incometax_out <- c()
  wealthtax_out <- c()
  wealth1_out <- c()
  cash1_out <- c()
  wealth1_out <- c()
  investment1_out <- investment0_out
  investment1_out[] <- NA

  investment_state <- investment0_out
  investment_state[] <- 0
  for (i in seq_along(investment)) {
    from <- investment[[i]]$from
    to <- investment[[i]]$to
    investment_state[from:to, i] <- 1
  }

  # random returns
  p_rets <- matrix(rnorm(n = (length(investment)+1)*length(timesteps$timestep),
                         mean = c(cash$exp_pret, sapply(investment, function(item) item$exp_pret)),
                         sd = c(cash$exp_psd, sapply(investment, function(item) item$exp_psd))),
                   ncol = 1+ncol(investment0_out), byrow = T)
  d_rets <- matrix(rnorm(n = (length(investment)+1)*length(timesteps$timestep),
                         mean = c(cash$exp_dret, sapply(investment, function(item) item$exp_dret)),
                         sd = c(cash$exp_dsd, sapply(investment, function(item) item$exp_dsd))),
                   ncol = 1+ncol(investment0_out), byrow = T)

  for (i in 1:length(timesteps$timestep)) {
    if (i > 1) {
      cash0_out[i] <- cash1_out[i-1]
      wealth0_out[i] <- wealth1_out[i-1]
      # investment0_out[i,] <- investment1_out[i-1,]
      investment0_out[i,] <- sapply(investment, function(item) max(item$amount, item$percentage*wealth0_out[i])) * investment_state[i,]
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
    cash0_out[i] <- max(cash_target, wealth0_out[i] - sum(investment0_out[i,]))

    capincome_out[i] <- cash0_out[i]* d_rets[i,1] + sum(investment0_out[i,] * d_rets[i,-1])
    roi_out[i,] <- investment0_out[i,] * p_rets[i,-1]

    incometax_out[i] <- calculate_tax(income_out[i] + capincome_out[i], tax = incometax)
    wealthtax_out[i] <- calculate_tax(wealth0_out[i], tax = wealthtax)

    cash1_out[i] <- cash0_out[i] + income_out[i] + capincome_out[i] - expense_out[i] - incometax_out[i] - wealthtax_out[i]
    investment1_out[i,] <- investment0_out[i,] + roi_out[i]

    wealth1_out[i] <- cash1_out[i] + sum(investment1_out[i,])
  }

  return(list(timesteps = timesteps,
              income = income_out,
              expense = expense_out,
              incometax = incometax_out,
              wealthtax = wealthtax_out,
              cash0 = cash0_out,
              investment0 = investment0_out,
              wealth0 = wealth0_out,
              capincome = capincome_out,
              roi_out = roi_out,
              cash1 = cash1_out,
              investment1 = investment1_out,
              wealth1 = wealth1_out))
}



