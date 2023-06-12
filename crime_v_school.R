# school_funding <- mutate(school_funding,
#     stud_exp <- INSTRUCTION_EXPENDITURE + SUPPORT_SERVICES_EXPENDITURE,
#     tot_exp <- INSTRUCTION_EXPENDITURE + SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE + CAPITAL_OUTLAY_EXPENDITURE
#     )

#colnames(school_funding)
#View(school_funding)
# well THAT didn't work. <- Has to be '=' not '<-' in mutate function 

#### Calculations didn't make sense, changed later.
school_funding <- mutate(
  school_funding,
  stud_exp = INSTRUCTION_EXPENDITURE + SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE,
  tot_exp_calc = INSTRUCTION_EXPENDITURE + SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE + CAPITAL_OUTLAY_EXPENDITURE # checking if calc = listed
)
View(school_funding)

school_funding <- mutate(
  school_funding,
  tot_exp_und = TOTAL_EXPENDITURE - tot_exp
  )
View(school_funding)

chk_tot_fnd <- school_funding %>%
  filter(YEAR == 2010)

ggplot(data = chk_tot_fnd, mapping = aes(x = STATE, y = tot_exp_calc, fill = tot_exp_calc)) +
  geom_col(position='dodge') 

school_funding <- mutate(
  school_funding,
  st_abr = state.abb[match(STATE, state.name)]
) %>%
  relocate(st_abr, .after = STATE)

View(school_funding)

# Writing to csv to manipulate in Numbers.  
# Deleted columns incorrectly created early on.
# Changed all abbreviations for Wash, D.C. to DC
write_csv(school_funding,"school_funding.csv")

colnames(school_funding)

plot_tot_fund <- school_funding %>%
  filter(YEAR == 2010)

colnames(plot_tot_fund)
View(plot_tot_fund)

ggplot(data = plot_tot_fund, mapping = aes(x = st_abr, y = stud_exp)) +
  geom_bar(stat = 'identity')
  

school_funding <- mutate(
  school_funding,
  per_stud_exp = stud_exp / ENROLL
)

View(school_funding)
colnames(school_funding)

############### Calculations and graph aren't looking correct. ############

school_funding <- mutate(
  school_funding,
  stud_instr_exp = INSTRUCTION_EXPENDITURE + SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE,
  per_stud_instr_exp = stud_instr_exp / ENROLL
)

View(school_funding)
colnames(school_funding)

### AH HA!!  ENROLL column units are student days, not students!! 
### Calculations are valid!

plot_tot_fund_2010 <- school_funding %>%
  filter(YEAR == 2010, !st_abr == "DC") ## Excluding DC as it throws off plot

# colnames(plot_tot_fund)
# View(plot_tot_fund)

ggplot(data = plot_tot_fund_2010, mapping = aes(x = st_abr, y = per_stud_instr_exp)) +
  geom_bar(stat = 'identity', aes(fill = st_abr)) +
  labs(title = "Per Student Spending 2010", x = "State", y = "Spending per Student per School Day")

colnames(crime_in_the_US)

View(crime_in_the_US)

View(state.name)

crime_in_the_US <- mutate(
  crime_in_the_US,
  st_abr = state.abb[match(str_to_title(State), state.name)]
) %>%
  relocate(st_abr, .after = State)

View(crime_in_the_US)

us_crime_2010 <- crime_in_the_US %>%
  filter(Year == 2010)

###  Initial plot to see how data is going
ggplot(us_crime_2010, mapping = aes(x = st_abr, y = Violent_crime, fill = st_abr)) +
  geom_bar(stat = "identity")

## Need to add columns for crime per 100k people.  easier to do in excel / Google sheets.
crime_in_the_US <- mutate(
  us_crime_per_100k,
  st_abr = state.abb[match(str_to_title(State), state.name)]
) %>%
  relocate(st_abr, .after = State)

View(crime_in_the_US)

us_crime_2010 <- crime_in_the_US %>%
  filter(Year == 2010)

### Texas is listed twice for 2010!!  Re-importing after deleting
crime_in_the_US <- mutate(
  us_crime_per_100k,
  st_abr = state.abb[match(str_to_title(State), state.name)]
) %>%
  relocate(st_abr, .after = State)

View(crime_in_the_US)

us_crime_2010 <- crime_in_the_US %>%
  filter(Year == 2010)

View(us_crime_2010)


###  Plotting with better formatting 
### 2010    <<<< Property Crime data not correct for Texas, starting from scratch for 2010.
ggplot(data = plot_tot_fund_2010, mapping = aes(x = st_abr, y = per_stud_instr_exp)) +
  geom_bar(stat = 'identity', fill = "blue") +
  labs(title = "School Spending 2010", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(us_crime_2010, mapping = aes(x = st_abr, y = Violent_crime)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Violent Crime in 2010",x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(us_crime_2010, mapping = aes(x = st_abr, y = Property_crime)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Property Crime in 2010",x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

#### 2011
plot_tot_fund_2011 <- school_funding %>%
  filter(YEAR == 2011, !st_abr == "DC") ## Excluding DC as it throws off plot

us_crime_2011 <- crime_in_the_US %>%
  filter(Year == 2011)

ggplot(data = plot_tot_fund_2011, mapping = aes(x = st_abr, y = per_stud_instr_exp)) +
  geom_bar(stat = 'identity', fill = "blue") +
  labs(title = "School Spending 2011", x = " ", y = " ") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(us_crime_2011, mapping = aes(x = st_abr, y = Violent_crime)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Violent Crime in 2011",x = " ", y = " ") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(us_crime_2011, mapping = aes(x = st_abr, y = Property_crime)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Property Crime in 2011",x = " ", y = " ") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")


### 2012
plot_tot_fund_2012 <- school_funding %>%
  filter(YEAR == 2012, !st_abr == "DC") ## Excluding DC as it throws off plot

us_crime_2012 <- crime_in_the_US %>%
  filter(Year == 2012)

ggplot(data = plot_tot_fund_2012, mapping = aes(x = st_abr, y = per_stud_instr_exp)) +
  geom_bar(stat = 'identity', fill = "blue") +
  labs(title = "School Spending 2012", x = " ", y = " ") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(us_crime_2012, mapping = aes(x = st_abr, y = Violent_crime)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Violent Crime in 2012",x = " ", y = " ") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(us_crime_2012, mapping = aes(x = st_abr, y = Property_crime)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Property Crime in 2012",x = " ", y = " ") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

######################## Scatter Plots instead of Bar Plots ####################



### Writing to CSV for use in Python
write.csv(crime_in_the_US, "crime_per_100k.csv")
write.csv(us_crime_2010, "crime_2010.csv")
write.csv(us_crime_2011, "crime_2011.csv")
write.csv(us_crime_2012, "crime_2012.csv")
write.csv(school_funding, "school_funding.csv")
write.csv(plot_tot_fund_2010, "sch_total_fund_2010.csv")
write.csv(plot_tot_fund_2011, "sch_total_fund_2011.csv")
write.csv(plot_tot_fund_2012, "sch_total_fund_2012.csv")

colnames(crime_in_the_US)

crime_2014 <- crime_in_the_US %>%
  filter(Year == 2014)
crime_2015 <- crime_in_the_US %>%
  filter(Year == 2015)
crime_2016 <- crime_in_the_US %>%
  filter(Year == 2016)
crime_2017 <- crime_in_the_US %>%
  filter(Year == 2017)

write.csv(crime_2014, "crime_2014.csv")
write.csv(crime_2015, "crime_2015.csv")
write.csv(crime_2016, "crime_2016.csv")
write.csv(crime_2017, "crime_2017.csv")



##################### Scatter plots instead of Bar plots #########################


###########  2010 vs. 2010
#colnames(merged_2010)

ggplot(data = merged_2010, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending vs Violent Crime 2010", 
    x = "Daily Spending per Student", 
    y = " "
    ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(data = merged_2010, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending vs Property Crime 2010", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")


###########  2011 vs. 2011
#colnames(merged_2011)

ggplot(data = merged_2011, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending vs Violent Crime 2011", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")



# ggplot(data = merged_2011, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
#   geom_smooth() +
#   geom_point(aes(color = st_abr)) +
#   labs(
#     title = "School Spending vs Property Crime 2011", 
#     x = "Daily Spending per Student", 
#     y = " "
#   ) +
#   theme(plot.title = element_text(hjust = 0.5), legend.position="none")

str(merged_2010)  # merged_2010[Property_crime] is int
str(merged_2011)  # merged_2011[Property_crime] is char  >>> Fixed in G Sheets by removing commas
str(merged_2012)  # merged_2012[Property_crime] is char    >>> Fixed in G Sheets by removing commas

# merged_2011["Property_crime"] = as.integer(merged_2011["Property_crime"])
str(merged_2010)  
str(merged_2011)  
str(merged_2010)  
str(merged_2011)  
str(merged_2012)  
str(merged_2010)  
str(merged_2011)

str(merged_2014)
str(merged_2015)
str(merged_2016)
str(merged_2017)

ggplot(data = merged_2011, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending vs Property Crime 2011", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

###########  2012 vs. 2012
#colnames(merged_2012)

ggplot(data = merged_2012, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending vs Violent Crime 2012", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(data = merged_2012, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending vs Property Crime 2012", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")


###########  2014 vs. 2010
 colnames(merged_2014)

ggplot(data = merged_2014, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2010 vs Violent Crime 2014", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(data = merged_2014, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2010 vs Property Crime 2014", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")


###########  2015 vs. 2011
colnames(merged_2015)

ggplot(data = merged_2015, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2011 vs Violent Crime 2015", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(data = merged_2015, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2011 vs Property Crime 2015", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")



###########  2016 vs. 2012
colnames(merged_2016)

ggplot(data = merged_2016, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2012 vs Violent Crime 2016", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(data = merged_2016, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2012 vs Property Crime 2016", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")



###########  2017 vs. 2013
colnames(merged_2017)

ggplot(data = merged_2017, mapping = aes(x = per_stud_instr_exp, y = Violent_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2013 vs Violent Crime 2017", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

ggplot(data = merged_2017, mapping = aes(x = per_stud_instr_exp, y = Property_crime)) +
  geom_smooth() +
  geom_point(aes(color = st_abr)) +
  labs(
    title = "School Spending 2013 vs Property Crime 2017", 
    x = "Daily Spending per Student", 
    y = " "
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

