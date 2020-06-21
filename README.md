Covid-19 Hawai’i Rental Assistance
================
Isabelle Picciotto

The following analysis of the burden renters will face in HI for the
remainder of 2020 is presented in [this UHERO blog
post](https://uhero.hawaii.edu/?p=8990).

``` r
# Install required packages

# pkgs <- c(
#   "rmarkdown",
#   "tidyverse",
#   "lubridate",
#   "scales",
#   "svMIsc",
#   "gt"
# )
# 
# install.packages(pkgs)
```

``` r
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(scales)
library(svMisc)
library(gt)

options(scipen = 999)
```

### Data Sources

Individual level income and jobs data is sourced from the American
Community Survey (ACS) 2018 IPUMS microdata. Household survey weights
are utilized to scale the 4,800 surveyed households to the 455,000 total
households in the state.

Industry-level job losses for the remainder of 2020 are forecasted by
UHERO. Forecasted losses are randomly assigned to individuals by
industry. We assume that 65% of job losses will be assigned to
individuals who are below the median income of each industry.

### Number of Iterations

``` r
n_iterations <- 100
```

In order to avoid aberrations based on a single random draw, the
analysis is repeated 100 times and the results are averaged over all
simulations.

### UHERO Job Loss Forecasts

``` r
uhero_forecast<-read_csv("data/uhero_jobs_forecast.csv")

### Clean up job columns:
# List of UHERO job mnemonics to retain in the forecast file
job_mnemonics<-c("E", "E_NF", "EGV", "EGVFD", "E_GVSL", "ECT", "EMN", "E_TRADE", "E_TU",
                 "E_FIR", "E_SV", "EHC", "EAF", "E_ELSE")

uhero_jobs<-uhero_forecast %>%
  select(Date, starts_with(job_mnemonics)) %>%
  select(Date, ends_with("(new)"))

# Rename columns
new_colnames<-str_remove_all(string = colnames(uhero_jobs), pattern = "@HI \\(new\\)*")
colnames(uhero_jobs)<-new_colnames

# Create the agricultural job series
uhero_jobs<-uhero_jobs %>%
  mutate(EAG = E - E_NF)

### Calculate year over year job losses by industry
uhero_job_losses<-uhero_jobs %>%
  mutate_at(.vars = vars(-Date), function(x) (x - lag(x, n = 4))*1000) %>%
  mutate_at(.vars = vars(-Date), function(x) if_else(x < 0, abs(x), 0))
```

### ACS (2018) IPUMS Microdata Cleaning

``` r
# Read IPUMS csv file
ipums_raw<-read_csv("data/ACS_2017-2018.csv")

### Filter the IPUMS data 
  # Restrict the sample to include 2018 survey results
  # Remove OWNERSHP = 0, these are neither renter or homeowner households and thus will not be assigned job losses
  # Per the IPUMS Coding Manual, recode NA entries as NA

ipums_2018<-filter(ipums_raw, YEAR == 2018 & OWNERSHP != 0) %>%
  mutate(inc_total = INCTOT %>% na_if(9999999),
         inc_wage = INCWAGE %>% na_if(999999) %>% na_if(999998),
         hh_income = HHINCOME %>% na_if(9999999),
         rent_grs = if_else(OWNERSHP == 2, RENTGRS, NA_real_),
         rent_burden = if_else(rent_grs == 0, 0, pmin(rent_grs/(hh_income/12)*100, 200))) 

### Apply household weightings to the data
  # Duplicate households based on household weight (HHWT)
  # Create a new unique HH identifier for the duplicated households
ipums_weighted<-ipums_2018 %>%
  group_by(SERIAL) %>%
  uncount(weights = HHWT, .id = "ID") %>%
  unite("hhid", c("SERIAL", "ID"), sep = "_", remove = F) %>%
  ungroup() %>%
  group_by(hhid)
```

### Data Cleaning and Joining

In order to assign UHERO job losses to individuals in the ACS (2018)
IPUMS Microdata, the UHERO job industry mnemonics are joined to the ACS
job categories using their Census Code. Original mapping of UHERO
mnemonic to Census Code was done using NAICS codes.

``` r
# Load the industry uhero mnemonic csv file
ind_uhero_match<-read_csv("data/industry_code_uhero_mnemonics.csv")

# Assign the uhero mnemonic to the ipums industry code
ipums_uhero<-left_join(ipums_weighted, ind_uhero_match, by = c("IND" = "2017 Census Code")) %>%
  rowid_to_column(var = "unique_id") %>%
  ungroup()

# Get unique list of industries in the ACS data
jobs<-unique(na.omit(ipums_uhero$UHERO_mnemonic))

# Choose which as of date you want for annual job losses
as_of_date<-"2020-07-01"

uhero_job_loss_reshape<-uhero_job_losses %>%
  pivot_longer(-Date, names_to = "UHERO_mnemonic", values_to = "job_losses") %>%
  filter(Date == as_of_date & UHERO_mnemonic %in% jobs) %>%
  select(-Date)

# Revise the number of job losses to equal the minimum of the total jobs or total job losses
n_jobs<-ipums_uhero %>%
  group_by(UHERO_mnemonic) %>%
  tally() %>%
  na.omit() %>%
  left_join(uhero_job_loss_reshape, by = "UHERO_mnemonic") %>%
  mutate(revised_losses = pmin(n, job_losses)) %>%
  select(-n, -job_losses) %>%
  mutate(n_low = round(revised_losses*0.65),
         n_high = round(revised_losses*0.35))

# Calculate the median individual income for each industry
median_income<-ipums_uhero %>%
  group_by(UHERO_mnemonic) %>%
  summarize(median_income = median(inc_total, na.rm = T)) %>%
  na.omit()

# Add a median income column to the individual level data to determine whether each individual falls into the low/high income category
# Exclude all individuals wtih wage income equal to zero so that they cannot randomly be assigned a job loss
simulation_dataset<-left_join(ipums_uhero, median_income, by = "UHERO_mnemonic") %>%
  mutate(income_cat = case_when(inc_total < median_income & inc_wage != 0 ~ "low",
                                inc_total >= median_income & inc_wage != 0 ~ "high",
                                is.na(UHERO_mnemonic) | inc_wage == 0 ~ "unassigned"))

# Create a table summarizing the UHERO forecasted job categories, number of wage earners per category, and number of forecasted job losses
job_summary_tbl<-simulation_dataset %>% 
  left_join(select(n_jobs, revised_losses, UHERO_mnemonic), by = "UHERO_mnemonic") %>% 
  select(unique_id, hhid, inc_wage, hh_income, revised_losses, Industry_Sector, UHERO_mnemonic) %>% 
  filter(!is.na(Industry_Sector)) %>% 
  group_by(UHERO_mnemonic) %>% 
  filter(inc_wage > 0) %>% 
  summarize("Sectors Included" = list(unique(Industry_Sector)), 
            "Number of Wage Earners" = n(), 
            "Annual Job Losses (as of 2020 Q3)" = mean(revised_losses)) %>%
  gt() %>%
  fmt_number(columns = vars("Number of Wage Earners", "Annual Job Losses (as of 2020 Q3)"), decimal = 0)

gtsave(job_summary_tbl, filename = "job_summary_tbl.png")
```

![](README_files/figure-gfm/data-join-1.png)<!-- -->

``` r
# Clean up variables tables prior to running simulation
rm(ipums_raw, ipums_2018, ipums_uhero, uhero_forecast, uhero_jobs, uhero_job_losses, uhero_job_loss_reshape)
```

### Job Loss Simulation

To simulate the impact of COVID-19 job losses on Hawai’i rent deficit,
we estimate the change to each household’s rental burden and how much
additional support would be required to return them to their original
burden level.

Four different policy scenarios are analyzed, each scenario is repeated
100 times and the results are the average of all 100 simulations:

1.  Federal Pandemic Unemployment Compensation (FPUC) expires and
    unemployment drops to standard levels.

2.  FPUC expires, but unemployed individuals receive $100 per week in
    unrestricted cash benefits.

3.  FPUC expires, but unemployed individuals receive $200 per week in
    unrestricted cash benefits.

4.  FPUC expires, but unemployed individuals receive $300 per week in
    unrestricted cash benefits.

<!-- end list -->

``` r
scenarios<-1:4

# Create empty dataframes for various simulation results
total_sim_cdf<-tibble()
total_burden_change<-tibble()
loss_assignment<-tibble()
total_affected_rental_support<-tibble()

# Loop over each scenario, 1 - 4, repeat 100 times
for (scenario in scenarios){
  
  # Assign Unemployment assistance for the current scenario
  add_UI<-if_else(scenario == 1, 0, if_else(scenario == 2, 400, if_else(scenario == 3, 800, if(scenario == 4) 1200)))
  
  for (i in 1:n_iterations){
    progress(i)
    
    ### Low income:
      # Generate the sampled/unsampled individuals and randomly assign job losses
    sampled_low<-simulation_dataset %>%
      filter(income_cat == "low") %>%
      group_by(UHERO_mnemonic) %>%
      nest() %>%
      filter(!is.na(UHERO_mnemonic)) %>%
      ungroup() %>%
      left_join(select(n_jobs, UHERO_mnemonic, n_low), by = "UHERO_mnemonic") %>%
      mutate(samp = map2(data, n_low, sample_n)) %>%
      select(-data) %>%
      unnest(samp) %>%
      mutate(inc_wage_new = pmin(inc_wage*0.60, 648*52) + add_UI*12,
             inc_total_new = (inc_total - inc_wage) + inc_wage_new,
             affected = 1) %>%
      select(-n_low)
    
    unsampled_low<-simulation_dataset %>%
      filter(income_cat == "low") %>%
      filter(!(unique_id %in% sampled_low$unique_id)) %>%
      mutate(inc_wage_new = inc_total,
             inc_total_new = inc_total,
             affected = 0)
    
    ### High Income
      # Generate the sampled/unsampled individuals and randomly assign job losses
    sampled_high<-simulation_dataset %>%
      filter(income_cat == "high") %>%
      group_by(UHERO_mnemonic) %>%
      nest() %>%
      filter(!is.na(UHERO_mnemonic)) %>%
      ungroup() %>%
      left_join(select(n_jobs, UHERO_mnemonic, n_high), by = "UHERO_mnemonic") %>%
      mutate(samp = map2(data, n_high, sample_n)) %>%
      select(-data) %>%
      unnest(samp) %>%
      mutate(inc_wage_new = pmin(inc_wage*0.60, 648*52) + add_UI*12,
             inc_total_new = (inc_total - inc_wage) + inc_wage_new,
             affected = 1) %>%
      select(-n_high)
    
    unsampled_high<-simulation_dataset %>%
      filter(income_cat == "high") %>%
      filter(!(unique_id %in% sampled_high$unique_id)) %>%
      mutate(inc_wage_new = inc_wage,
             inc_total_new = inc_total,
             affected = 0)
    
    # Create the unsampled group, i.e., income category = "unassigned"
    unsampled_na<-simulation_dataset %>%
      filter(income_cat == "unassigned") %>%
      mutate(inc_wage_new = inc_wage,
             inc_total_new = inc_total,
             affected = 0)
    
    # Bind rows of sampled/unsampled to perform necessary aggregations/calculations
    final_sim<-bind_rows(sampled_low, unsampled_low, sampled_high, unsampled_high, unsampled_na)
    
    # Filter the simulation data to include only renter households, summarize primary variables, and calculate household level outcome variables
    final_renters<-final_sim %>%
      filter(OWNERSHP == 2) %>%
      group_by(hhid) %>%
      summarize(hh_income = mean(hh_income), 
                rent_grs = mean(rent_grs),
                rent_burden = mean(rent_burden, na.rm = T),
                hh_affected = max(affected),
                hh_income_new = if_else(hh_affected == 1, sum(inc_total_new, na.rm = T), hh_income)) %>%
      mutate(rent_burden_new = if_else(rent_grs == 0, 0, pmin(rent_grs/(hh_income_new/12)*100, 200)),
             burden_change = rent_burden_new - rent_burden,
             support_needed = if_else(hh_affected == 1, pmax(0, round(rent_grs - ((rent_burden/100)*(hh_income_new/12)),5)), 0))
    
    # Calculate the additional support needed to return affected renters to their prior level of rent burden
    affected_rental_support_row<-c("scenario" = scenario, "iteration" = i, 
                                  "AvgHHSupport" = mean(filter(final_renters, hh_affected == 1)$support_needed), 
                          "TotalHHSupport" = sum(filter(final_renters, hh_affected == 1)$support_needed))
    
    total_affected_rental_support<-bind_rows(total_affected_rental_support, affected_rental_support_row)
    
    # Calculate the change in rental burden for each household
    max_change<-round(max(final_renters$burden_change))+1
    
    burden_change<-tibble()
    
    for (k in 1:max_change){
      new_change_row<-c("scenario" = scenario, "iteration" = i, "burden_change" = k, 
                        "cdf_count" = nrow(filter(final_renters, burden_change <= k)))
      burden_change<-bind_rows(burden_change, new_change_row)
    }
    
    total_burden_change<-bind_rows(total_burden_change, burden_change)
    
    # Calculate the distribution of rental burden
    sim_dist<-tibble()
    
    for (j in 1:200){
      new_row<-c("scenario" = scenario, "iteration" = i, "burden" = j, "count" = nrow(filter(final_renters, rent_burden_new <= j)))
      sim_dist<-bind_rows(sim_dist, new_row)
    }
    
    sim_cdf<-sim_dist %>%
      mutate(pr_x = count/max(sim_dist$count))
    
    total_sim_cdf<-bind_rows(total_sim_cdf, sim_cdf)
    
    rm(final_sim, final_renters)
  }
}
```

    ## Progress:   1%  Progress:   2%  Progress:   3%  Progress:   4%  Progress:   5%  Progress:   6%  Progress:   7%  Progress:   8%  Progress:   9%  Progress:  10%  Progress:  11%  Progress:  12%  Progress:  13%  Progress:  14%  Progress:  15%  Progress:  16%  Progress:  17%  Progress:  18%  Progress:  19%  Progress:  20%  Progress:  21%  Progress:  22%  Progress:  23%  Progress:  24%  Progress:  25%  Progress:  26%  Progress:  27%  Progress:  28%  Progress:  29%  Progress:  30%  Progress:  31%  Progress:  32%  Progress:  33%  Progress:  34%  Progress:  35%  Progress:  36%  Progress:  37%  Progress:  38%  Progress:  39%  Progress:  40%  Progress:  41%  Progress:  42%  Progress:  43%  Progress:  44%  Progress:  45%  Progress:  46%  Progress:  47%  Progress:  48%  Progress:  49%  Progress:  50%  Progress:  51%  Progress:  52%  Progress:  53%  Progress:  54%  Progress:  55%  Progress:  56%  Progress:  57%  Progress:  58%  Progress:  59%  Progress:  60%  Progress:  61%  Progress:  62%  Progress:  63%  Progress:  64%  Progress:  65%  Progress:  66%  Progress:  67%  Progress:  68%  Progress:  69%  Progress:  70%  Progress:  71%  Progress:  72%  Progress:  73%  Progress:  74%  Progress:  75%  Progress:  76%  Progress:  77%  Progress:  78%  Progress:  79%  Progress:  80%  Progress:  81%  Progress:  82%  Progress:  83%  Progress:  84%  Progress:  85%  Progress:  86%  Progress:  87%  Progress:  88%  Progress:  89%  Progress:  90%  Progress:  91%  Progress:  92%  Progress:  93%  Progress:  94%  Progress:  95%  Progress:  96%  Progress:  97%  Progress:  98%  Progress:  99%  Progress: 100%  Progress:   1%  Progress:   2%  Progress:   3%  Progress:   4%  Progress:   5%  Progress:   6%  Progress:   7%  Progress:   8%  Progress:   9%  Progress:  10%  Progress:  11%  Progress:  12%  Progress:  13%  Progress:  14%  Progress:  15%  Progress:  16%  Progress:  17%  Progress:  18%  Progress:  19%  Progress:  20%  Progress:  21%  Progress:  22%  Progress:  23%  Progress:  24%  Progress:  25%  Progress:  26%  Progress:  27%  Progress:  28%  Progress:  29%  Progress:  30%  Progress:  31%  Progress:  32%  Progress:  33%  Progress:  34%  Progress:  35%  Progress:  36%  Progress:  37%  Progress:  38%  Progress:  39%  Progress:  40%  Progress:  41%  Progress:  42%  Progress:  43%  Progress:  44%  Progress:  45%  Progress:  46%  Progress:  47%  Progress:  48%  Progress:  49%  Progress:  50%  Progress:  51%  Progress:  52%  Progress:  53%  Progress:  54%  Progress:  55%  Progress:  56%  Progress:  57%  Progress:  58%  Progress:  59%  Progress:  60%  Progress:  61%  Progress:  62%  Progress:  63%  Progress:  64%  Progress:  65%  Progress:  66%  Progress:  67%  Progress:  68%  Progress:  69%  Progress:  70%  Progress:  71%  Progress:  72%  Progress:  73%  Progress:  74%  Progress:  75%  Progress:  76%  Progress:  77%  Progress:  78%  Progress:  79%  Progress:  80%  Progress:  81%  Progress:  82%  Progress:  83%  Progress:  84%  Progress:  85%  Progress:  86%  Progress:  87%  Progress:  88%  Progress:  89%  Progress:  90%  Progress:  91%  Progress:  92%  Progress:  93%  Progress:  94%  Progress:  95%  Progress:  96%  Progress:  97%  Progress:  98%  Progress:  99%  Progress: 100%  Progress:   1%  Progress:   2%  Progress:   3%  Progress:   4%  Progress:   5%  Progress:   6%  Progress:   7%  Progress:   8%  Progress:   9%  Progress:  10%  Progress:  11%  Progress:  12%  Progress:  13%  Progress:  14%  Progress:  15%  Progress:  16%  Progress:  17%  Progress:  18%  Progress:  19%  Progress:  20%  Progress:  21%  Progress:  22%  Progress:  23%  Progress:  24%  Progress:  25%  Progress:  26%  Progress:  27%  Progress:  28%  Progress:  29%  Progress:  30%  Progress:  31%  Progress:  32%  Progress:  33%  Progress:  34%  Progress:  35%  Progress:  36%  Progress:  37%  Progress:  38%  Progress:  39%  Progress:  40%  Progress:  41%  Progress:  42%  Progress:  43%  Progress:  44%  Progress:  45%  Progress:  46%  Progress:  47%  Progress:  48%  Progress:  49%  Progress:  50%  Progress:  51%  Progress:  52%  Progress:  53%  Progress:  54%  Progress:  55%  Progress:  56%  Progress:  57%  Progress:  58%  Progress:  59%  Progress:  60%  Progress:  61%  Progress:  62%  Progress:  63%  Progress:  64%  Progress:  65%  Progress:  66%  Progress:  67%  Progress:  68%  Progress:  69%  Progress:  70%  Progress:  71%  Progress:  72%  Progress:  73%  Progress:  74%  Progress:  75%  Progress:  76%  Progress:  77%  Progress:  78%  Progress:  79%  Progress:  80%  Progress:  81%  Progress:  82%  Progress:  83%  Progress:  84%  Progress:  85%  Progress:  86%  Progress:  87%  Progress:  88%  Progress:  89%  Progress:  90%  Progress:  91%  Progress:  92%  Progress:  93%  Progress:  94%  Progress:  95%  Progress:  96%  Progress:  97%  Progress:  98%  Progress:  99%  Progress: 100%  Progress:   1%  Progress:   2%  Progress:   3%  Progress:   4%  Progress:   5%  Progress:   6%  Progress:   7%  Progress:   8%  Progress:   9%  Progress:  10%  Progress:  11%  Progress:  12%  Progress:  13%  Progress:  14%  Progress:  15%  Progress:  16%  Progress:  17%  Progress:  18%  Progress:  19%  Progress:  20%  Progress:  21%  Progress:  22%  Progress:  23%  Progress:  24%  Progress:  25%  Progress:  26%  Progress:  27%  Progress:  28%  Progress:  29%  Progress:  30%  Progress:  31%  Progress:  32%  Progress:  33%  Progress:  34%  Progress:  35%  Progress:  36%  Progress:  37%  Progress:  38%  Progress:  39%  Progress:  40%  Progress:  41%  Progress:  42%  Progress:  43%  Progress:  44%  Progress:  45%  Progress:  46%  Progress:  47%  Progress:  48%  Progress:  49%  Progress:  50%  Progress:  51%  Progress:  52%  Progress:  53%  Progress:  54%  Progress:  55%  Progress:  56%  Progress:  57%  Progress:  58%  Progress:  59%  Progress:  60%  Progress:  61%  Progress:  62%  Progress:  63%  Progress:  64%  Progress:  65%  Progress:  66%  Progress:  67%  Progress:  68%  Progress:  69%  Progress:  70%  Progress:  71%  Progress:  72%  Progress:  73%  Progress:  74%  Progress:  75%  Progress:  76%  Progress:  77%  Progress:  78%  Progress:  79%  Progress:  80%  Progress:  81%  Progress:  82%  Progress:  83%  Progress:  84%  Progress:  85%  Progress:  86%  Progress:  87%  Progress:  88%  Progress:  89%  Progress:  90%  Progress:  91%  Progress:  92%  Progress:  93%  Progress:  94%  Progress:  95%  Progress:  96%  Progress:  97%  Progress:  98%  Progress:  99%  Progress: 100%

## Results

The results of the analysis are presented in the graphs and tables
below:

### Summary of Renter Support Needed

``` r
# Calculate the average 6 month needed support for affected households
total_renter_support<-total_affected_rental_support %>%
  group_by(scenario) %>%
  summarize("Individual Household Support" = mean(AvgHHSupport*6),
            "Total Support" = mean(TotalHHSupport*6)) %>%
  gt() %>%
  fmt_number(columns = vars("Individual Household Support", "Total Support"), decimal = 0) %>%
  tab_header(title = "Support Needed for Renter Households who Suffered a Job Loss",
             subtitle = "Mean of 100 simulations")

gtsave(total_renter_support, filename = "total_renter_support.png")
```

![](README_files/figure-gfm/results-support-needed-1.png)<!-- -->

### Summary of Rental Burden Changes

``` r
### Plot the PDF of the change in rental burden
burden_change_pdf<-total_burden_change %>%
  group_by(scenario, burden_change) %>%
  summarize(avg_burden_change = mean(cdf_count)) %>%
  mutate(pdf_count = case_when(burden_change == 1 ~ avg_burden_change, 
                               burden_change > 1 ~ avg_burden_change - lag(avg_burden_change))) %>%
  select(scenario, burden_change, pdf_count) 

burden_change_pdf %>%
    filter(burden_change > 1) %>%
    ggplot() +
    geom_line(aes(x = burden_change, y = pdf_count), color = "blue4") +
    facet_wrap(~ scenario, scales = "free") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    theme_minimal() +
    ggtitle("Percent Change in Rent Burden Household Count", subtitle = "Excludes rent burden change < 1%") +
    xlab("Rent Burden Change") +
    ylab("Number of Households")
```

![](README_files/figure-gfm/results-burden-change-1.png)<!-- -->

``` r
total_bucket_change<-total_burden_change %>%
  group_by(scenario) %>%
  mutate(bucket_labels = cut(burden_change, 
                             breaks = c(0, 1, seq(from = 10, to = max(burden_change), by = 10), max(burden_change)))) %>%
  filter(burden_change %in% c(0, 1, seq(from = 10, to = max(burden_change), by = 10), max(burden_change))) %>%
  mutate(burden_bucket_count = if_else(burden_change == 1, cdf_count, cdf_count - lag(cdf_count))) %>%
  group_by(scenario, bucket_labels) %>%
  summarize(burden_change = mean(burden_change),
            avg_bucket_count = mean(burden_bucket_count)) %>%
  arrange(scenario, burden_change)

total_bucket_change$bucket_labels<-factor(total_bucket_change$bucket_labels, levels = unique(total_bucket_change$bucket_labels))

total_bucket_change %>%
  filter(burden_change != 1) %>%
  ggplot() +
  geom_col(aes(x = bucket_labels, y = avg_bucket_count), fill = "blue4", alpha = 0.7) +
  facet_wrap(~ scenario, scales = "free") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  ggtitle("Number of Households by Rental Burden Change Bucket") +
  xlab("Rental Burden Change") +
  ylab("Number of Households") +
  theme_minimal() +
  coord_flip()
```

![](README_files/figure-gfm/results-burden-change-2.png)<!-- -->

``` r
rental_bucket_change<-total_bucket_change %>%
  select(-burden_change) %>%
  gt() %>%
  cols_label(bucket_labels = "Rental Burden Group",
             avg_bucket_count = "Number of Households") %>%
  fmt_number(columns = vars(avg_bucket_count), decimal = 0)

gtsave(rental_bucket_change, filename = "rental_bucket_change.png")
```

![](README_files/figure-gfm/results-burden-change-3.png)<!-- -->

### PDF of Rental Burden

The following plots a comparison of the PDF of household rental burden
after each scenario

``` r
### Calculate the initial PDF of rental burden
ipums_rental_hhcollapse<-ipums_weighted %>%
  filter(OWNERSHP == 2) %>%
  summarize(rent_grs = mean(rent_grs), rent_burden = mean(rent_burden, na.rm = T))

# Calculate the initial pdf of rent burden
initial_pdf<-ipums_rental_hhcollapse %>%
  mutate(burden = if_else(rent_burden == 0, 1, ceiling(rent_burden))) %>%
  group_by(burden) %>%
  summarize(initial_pdf_count = n())
  
# Calculate the pdf of rental burden for each scenario
sim_pdf<-total_sim_cdf %>%
  group_by(scenario, burden) %>%
  summarize(avg_sim_count = mean(count)) %>%
  mutate(sim_pdf_count = case_when(burden == 1 ~ avg_sim_count,
                                   burden > 1 ~ avg_sim_count - lag(avg_sim_count))) %>%
  select(scenario, burden, sim_pdf_count)
  
# Join the pre and post job loss pdfs of rental burden
joined_pdf<-inner_join(sim_pdf, initial_pdf, by = "burden") %>%
  pivot_longer(cols = c("sim_pdf_count", "initial_pdf_count"), names_to = "key", values_to = "value") %>%
  filter(burden != 1)
  
ggplot(data = joined_pdf, aes(x = burden, y = value, group = key, color = key)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = F) +
  facet_wrap(~ scenario, scales = "free") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_color_manual(values = c("indianred", "blue4"), labels = c("Initial Count", "Avg Simulation Count")) +
  theme_minimal() +
  ggtitle("PDF of Rental Burden", subtitle = "Excludes rental burden < 1%") +
  xlab("Rental Burden") +
  ylab("Number of Households") +
  theme(legend.position = "bottom")
```

![](README_files/figure-gfm/results-pdf-1.png)<!-- -->

### Summary of Household Risk Bins

Households are separated into general risk categories based on their
rental burden: 1. Safe \[0, 30\] 2. At Risk (30, 50\] 3. Support Needed
(50, 70\] 4. In Crisis (70, 200+)

``` r
# Order of risk bins
order<-c("[0,30]", "(30, 50]", "(50, 70]", "(70, 200+)")

sim_risk_bins<-sim_pdf %>%
  group_by(scenario) %>%
  mutate(bin_label = cut(burden, breaks = c(0, 30, 50, 70, 200), include.lowest = T, labels = order)) %>%
  group_by(scenario, bin_label) %>%
  summarize(sim_bin_count = sum(sim_pdf_count))

initial_risk_bins<-ipums_rental_hhcollapse %>%
  mutate(bin_label = cut(rent_burden, breaks = c(0, 30, 50, 70, 200), include.lowest = T, labels = order)) %>%
  group_by(bin_label) %>%
  summarize(initial_bin_count = n())

bin_join<-inner_join(initial_risk_bins, sim_risk_bins, by = "bin_label")


bin_join %>%
  gather(key = "key", value = "bin_counts", initial_bin_count, sim_bin_count) %>%
  group_by(key) %>%
  ggplot() +
  geom_col(aes(x = bin_label, y = bin_counts, group = key, fill = key), position = "dodge") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  facet_wrap(~ scenario, scales = "free") +
  theme_minimal() +
  ggtitle("Number of Households in Each Rent Burden Risk Category") +
  xlab("") +
  ylab("Number of Households") +
  scale_fill_manual(values = alpha(c("blue4", "lightblue"), 0.7), labels = c("Initial Count", "Avg Simulation Count")) +
  theme(legend.position = "bottom")
```

![](README_files/figure-gfm/bin-categories-1.png)<!-- -->

``` r
risk_bin_count<-bin_join %>%
  mutate(scenario_chr = paste0("scenario", scenario)) %>%
  select(scenario_chr, bin_label, initial_bin_count, sim_bin_count) %>% 
  spread(key = scenario_chr, value = sim_bin_count) %>%
  gt() %>%
  cols_label(bin_label = "Risk Bin",
             initial_bin_count = "Initial Household Count", 
             scenario1 = "Scenario 1",
             scenario2 = "Scenario 2",
             scenario3 = "Scenario 3",
             scenario4 = "Scenario 4") %>%
  fmt_number(columns = 2:6, decimal = 0)

gtsave(risk_bin_count, filename = "risk_bin_count.png")
```

![](README_files/figure-gfm/bin-categories-2.png)<!-- -->
