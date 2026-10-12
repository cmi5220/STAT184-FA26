 
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)
 
 
 
# Problem 1
 

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)


# Part A

sales_clean <- sales_raw %>%
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

sales_clean %>%
  select(
    sale_id,
    item_clean,
    amount,
    discount_rate,
    net_amount,
    size
  )


# Part B

sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(
      code_clean,
      "^(ST|ON)-[0-9]{3}$"
    )
  )

invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(
    sale_id,
    code_raw,
    code_clean
  )

invalid_codes


valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )

valid_code_parts %>%
  select(
    sale_id,
    channel,
    number,
    valid_code
  )


sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(
    tags,
    delim = ";"
  )

sale_tags

# separate_wider_delim() splits one value into multiple columns annd separate_longer_delim() splits one value into multiple rows.


# Part C

customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = ifelse(
      known_n == 0,
      NA_real_,
      large_n / known_n
    ),
    .by = customer_id
  ) %>%
  arrange(customer_id)

customer_summary

# For C1, the numerator is 0 because C1 has no known net amount that is at least 20. The denominator is 2 because C1 has two sales with known net amounts.



sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

levels(sales_factors$item_factor)
levels(sales_factors$item_group)

count(sales_factors, item_factor)
count(sales_factors, item_group)

# fct_infreq() changes the order of the factor levels by frequency and fct_lump_n() combines less frequent categories into "Other".


 # Problem 2
 

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)


# Part A

reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id) %>%
  ungroup()

reports_labeled


reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean %>%
  select(
    form,
    line,
    customer_id,
    round,
    primary_amount,
    backup_amount,
    amount
  )

# A recorded primary value is kept because coalesce() uses the first non-missing value. Zero is not missing, so a recorded zero is kept instead of using the backup amount.


reports_marked <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE)


reports_complete <- reports_marked %>%
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  )

reports_complete


 
original_rows_before <- nrow(reports_marked)
added_rows_before <- 0
missing_before <- sum(is.na(reports_marked$amount))

original_rows_before
added_rows_before
missing_before


 
original_rows_after <- sum(reports_complete$recorded_row)
added_rows_after <- sum(!reports_complete$recorded_row)
missing_after <- sum(is.na(reports_complete$amount))

original_rows_after
added_rows_after
missing_after

# Before:
# 6 original rows, 0 added rows, and 1 missing amount.

# After:
# 6 original rows, 6 added rows, and 7 missing amounts.

# One existing missing amount is C3, round 1. One absent customer-round row is C1, round 2.


# Part B

customers$customer_id[duplicated(customers$customer_id)]

sum(is.na(customers$customer_id))


reports_clean %>%
  count(customer_id, round) %>%
  filter(n > 1)


customers_marked <- customers %>%
  mutate(customer_found = TRUE)


reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    customer_found = coalesce(customer_found, FALSE)
  )


reports_joined %>%
  select(
    customer_id,
    round,
    amount,
    region,
    customer_found
  )


reports_only <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers,
    by = join_by(customer_id),
    na_matches = "never"
  )

customers_only <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean,
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only
customers_only

# C3 has customer_found = TRUE even though its region is missing. which means C3 exists in the customers table.

# C4 has customer_found = FALSE because C4 does not exist in the customers table.

# A missing region only tells us that the region is unknown and customer_found tells us whether the customer ID was found.


# Part C

 

customers_bad <- tibble(
  customer_id = c("C1", "C1", "C2", "C3", "C5"),
  region = c("North", "East", "South", NA, "West")
)

reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = "customer_id"
  )

nrow(reports_clean)
nrow(reports_duplicated)

sum(reports_clean$amount, na.rm = TRUE)
sum(reports_duplicated$amount, na.rm = TRUE)

reports_duplicated %>%
  count(customer_id, round) %>%
  filter(n > 1)

# The C1 customer has two matching records in customers_bad, this causes some report rows to appear twice after the join.
# The number of rows and the total amount can increase.
 
 


 # Problem 3
 

# Part A

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  
  nx <- length(x)
  ny <- length(y)
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  s2x <- var(x)
  s2y <- var(y)
  
  if (equal_var == TRUE) {
    
    sp2 <- ((nx - 1) * s2x + (ny - 1) * s2y) /
      (nx + ny - 2)
    
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    
    df <- nx + ny - 2
    
    method <- "Pooled"
    
  } else {
    
    se <- sqrt(s2x / nx + s2y / ny)
    
    df <- (s2x / nx + s2y / ny)^2 /
      (((s2x / nx)^2 / (nx - 1)) +
         ((s2y / ny)^2 / (ny - 1)))
    
    method <- "Welch"
  }
  
  t_stat <- (x_bar - y_bar) / se
  
  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )
  
  reject <- p_value < alpha
  
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}


x_check <- c(12, 15, 14, 10, 13)

y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)


welch_result <- two_sample_t(
  x_check,
  y_check
)

pooled_result <- two_sample_t(
  x_check,
  y_check,
  equal_var = TRUE
)

welch_result
pooled_result


# Verification using t.test()

welch_test <- t.test(
  x_check,
  y_check,
  var.equal = FALSE
)

pooled_test <- t.test(
  x_check,
  y_check,
  var.equal = TRUE
)

welch_test
pooled_test


 
welch_t_difference <- abs(
  welch_result$t_stat - welch_test$statistic
)

welch_df_difference <- abs(
  welch_result$df - welch_test$parameter
)

welch_p_difference <- abs(
  welch_result$p_value - welch_test$p.value
)


pooled_t_difference <- abs(
  pooled_result$t_stat - pooled_test$statistic
)

pooled_df_difference <- abs(
  pooled_result$df - pooled_test$parameter
)

pooled_p_difference <- abs(
  pooled_result$p_value - pooled_test$p.value
)


welch_t_difference < 1e-8
welch_df_difference < 1e-8
welch_p_difference < 1e-8

pooled_t_difference < 1e-8
pooled_df_difference < 1e-8
pooled_p_difference < 1e-8


# Part B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05


scenarios <- data.frame(
  scenario = c(
    "equal_null",
    "unequal_null",
    "equal_shift",
    "unequal_shift"
  ),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)


set.seed(2026)


simulation_results <- data.frame(
  scenario = character(8000),
  replicate = integer(8000),
  method = character(8000),
  t_stat = numeric(8000),
  df = numeric(8000),
  p_value = numeric(8000),
  reject = logical(8000)
)


row_number <- 1


for (i in 1:nrow(scenarios)) {
  
  for (j in 1:B) {
    
    x <- rnorm(
      n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y <- rnorm(
      n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    
    welch <- two_sample_t(
      x,
      y,
      equal_var = FALSE,
      alpha = alpha
    )
    
    simulation_results[row_number, "scenario"] <-
      scenarios$scenario[i]
    
    simulation_results[row_number, "replicate"] <- j
    
    simulation_results[row_number, "method"] <-
      welch$method
    
    simulation_results[row_number, "t_stat"] <-
      welch$t_stat
    
    simulation_results[row_number, "df"] <-
      welch$df
    
    simulation_results[row_number, "p_value"] <-
      welch$p_value
    
    simulation_results[row_number, "reject"] <-
      welch$reject
    
    
    row_number <- row_number + 1
    
    
    pooled <- two_sample_t(
      x,
      y,
      equal_var = TRUE,
      alpha = alpha
    )
    
    simulation_results[row_number, "scenario"] <-
      scenarios$scenario[i]
    
    simulation_results[row_number, "replicate"] <- j
    
    simulation_results[row_number, "method"] <-
      pooled$method
    
    simulation_results[row_number, "t_stat"] <-
      pooled$t_stat
    
    simulation_results[row_number, "df"] <-
      pooled$df
    
    simulation_results[row_number, "p_value"] <-
      pooled$p_value
    
    simulation_results[row_number, "reject"] <-
      pooled$reject
    
    
    row_number <- row_number + 1
  }
}


head(simulation_results)

nrow(simulation_results)


simulation_summary <- simulation_results %>%
  group_by(scenario, method) %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .groups = "drop"
  )


simulation_summary %>%
  mutate(
    mean_t = round(mean_t, 3),
    sd_t = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  )


 
## When the population means are the same the rejection rates should be around 0.05, with slight changes due to random sampling. 
#When the means are different, the rates usually go up because the tests are more likely to find a significant difference.If the populations have unequal variances, the pooled and Welch tests may give different results. 
#A good test should not only detect differences when they exist but also keep the rejection rate low when there is no real difference.

