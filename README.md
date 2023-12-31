# Wealth Planning - Retirement dissaving

R functions retirement wealth planning and simulation

## Example

```
library(wealthplanning)

wealth0 <- 100
timesteps <- set_timesteps(by = "year", n = 10, start = ceiling_date(Sys.Date(), by), date_of_birth = as.Date("1960-01-01"))
expense <- set_expense(expense0 = 20)
income <- set_income(income0 = 10)
investment <- list(set_investment(name = "investment1",
                                  timesteps = timesteps,
                                  exp_pret = 0.1, exp_psd = 0.2, exp_dret = 0.5, exp_dsd = 0,
                                  from = 1, to = 2,
                                  percentage = 1, amount = 50),
                   set_investment(name = NULL,
                                  timesteps = timesteps,
                                  exp_pret = 0, exp_psd = 0, exp_dret = 0, exp_dsd = 0,
                                  from = 1, to = length(timesteps$timestep),
                                  percentage = 1, amount = 20))
cash <- set_cash(timesteps = timesteps, amount = 20, percentage = 0.2)
incometax <- set_incometax(tax_brackets = c(10, 50, Inf), marginal_rates = c(0.2, 0.3, 0.32), deductable = 10)
wealthtax <- set_wealthtax(tax_brackets = c(10, 50, Inf), marginal_rates = c(0.002, 0.003, 0.009), deductable = 10)

out <- simulate_wealth(timesteps = timesteps,
                       wealth0 = wealth0,
                       expense = expense,
                       income = income,
                       cash = cash,
                       investment = investment, 
                       incometax = incometax, wealthtax = wealthtax)

out |> 
  as.data.frame() |> 
  clipr::write_clip()
```
