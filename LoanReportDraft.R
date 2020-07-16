library(tidymodels)
#library(gridExtra)
library(tidyverse)

# Load Lending Club Data
loans <- read_csv("loan.csv")

# View Structure of Data
str(loans)
head(loans)
# Summary of some key character columns
summary(as.factor(loans$term))
summary(as.factor(loans$grade))
summary(as.factor(loans$sub_grade))
summary(as.factor(loans$emp_length))
summary(as.factor(loans$home_ownership))
summary(as.factor(loans$purpose))
# Loan status is the most important column
summary(as.factor(loans$loan_status))
# To simplify, loan status will be grouped into 3 categories: Current, Good, Bad
good_loans <- c("Does not meet the credit policy. Status:Fully Paid","Fully Paid")
bad_loans <- c("Charged Off","Does not meet the credit policy. Status:Charged Off","Default","Late (31-120 days)")
current_loans <- c("Current","In Grace Period","Late (16-30 days)")

loans <- loans %>%
  mutate(outcome = case_when(loan_status %in% good_loans ~ "Good Loan",
                             loan_status %in% bad_loans ~ "Bad Loan",
                             loan_status %in% current_loans ~ "Current",
                             TRUE ~ "Other"))
summary(as.factor(loans$outcome))
# Filter out current loans
finished_loans <- loans %>%
  filter(outcome != "Current")
# Let's make some plots
# Histogram of Interest Rate
finished_loans %>%
  ggplot(aes(x = int_rate,fill = outcome)) +
  geom_histogram(position = "stack") +
  theme_bw()
# Same thing as proportion
finished_loans %>%
  ggplot(aes(x = int_rate,fill = outcome)) +
  geom_histogram(position = "fill") +
  theme_bw()
# Histogram of loan Amount
finished_loans %>%
  ggplot(aes(x = loan_amnt, fill = outcome)) +
  geom_histogram(position = "stack") +
  theme_bw()
# Same thing as proportion
finished_loans %>%
  ggplot(aes(x = loan_amnt, fill = outcome)) +
  geom_histogram(position = "fill") +
  theme_bw()

table(finished_loans$term,finished_loans$outcome)

finished_loans %>%
  count(term,outcome) %>%
  mutate(sum = sum(n),prop = n/sum) %>%
  group_by(term) %>%
  mutate(sum_term = sum(n),prop_term = n/sum_term) %>%
  ggplot(aes(x = term, fill = outcome, y = prop_term)) +
  geom_col() +
  theme_bw()

finished_loans %>%
  ggplot(aes(x = as.factor(sub_grade), fill = outcome)) +
  geom_bar(position = "stack") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
finished_loans %>%
  ggplot(aes(x = as.factor(sub_grade), fill = outcome)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

finished_loans %>%
  mutate(emp_length = fct_relevel(emp_length,"10+ years",after = 10)) %>%
  ggplot(aes(x = emp_length, fill = outcome)) +
  geom_bar(position = "stack") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))
finished_loans %>%
  mutate(emp_length = fct_relevel(emp_length,"10+ years",after = 10)) %>%
  ggplot(aes(x = emp_length, fill = outcome)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))

finished_loans %>%
  ggplot(aes(x = as.factor(home_ownership), fill = outcome)) +
  geom_bar(position = "stack") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30))


plotData <- function(data,group,label = group) {
  colors <- c("#D33016","#1BA9C2") #Bad,Good
  if(is.numeric(data[[group]])) {
    data %>%
      ggplot(aes(x = .data[[group[[1]]]],fill = outcome)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = colors) +
      labs(x = label,y = NULL,fill = NULL) +
      theme_minimal()
  } else {
    data %>%
      group_by(.data[[group[[1]]]],outcome) %>%
      summarize(Count = n()) %>%
      mutate(Proportion = Count/sum(Count)) %>%
      pivot_longer(Count:Proportion) %>%
      ggplot(aes(x = .data[[group[[1]]]],y = value,fill = outcome))+
      geom_col()+
      labs(x = label,y = NULL,fill = NULL) +
      scale_fill_manual(values = colors) + 
      facet_wrap(~name,scales = "free_x") +
      coord_flip() +
      theme_minimal()
  }
}
plotData(finished_loans,"int_rate",label = "Interest Rate")
plotData(finished_loans,"sub_grade",label = "Sub Grade")
#plot2(finished_loans,"addr_state",label = "State")

library(plotly)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
finished_loans %>%
  count(addr_state,outcome) %>%
  group_by(addr_state) %>%
  mutate(prop = round(n/sum(n)*100,2),
         n_tot = sum(n)) %>%
  filter(outcome == "Bad Loan") %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(locations= ~addr_state,color = ~prop, z= ~prop,hoverinfo = "text",
            text= ~paste0("Number of Loans: ",n_tot,"\nGood Loans: ",100-prop,"%\nBad Loans: ",prop,"%"),
            colorscale = "RdBu") %>%
  colorbar(title = "") %>% 
  layout(
    title = 'Percentage of Loans that are "Bad"',
    geo = g
  )


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
finished_loans %>%
  count(addr_state, outcome) %>%
  group_by(addr_state) %>%
  mutate(prop = round(n/sum(n)*100,2),
         n_tot = sum(n)) %>%
  filter(outcome == "Bad Loan") %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(locations= ~addr_state,color = ~n_tot, z= ~n_tot,hoverinfo = "text",
            text= ~paste0("Number of Loans: ",n_tot,"\nGood Loans: ",100-prop,"%\nBad Loans: ",prop,"%"),
            colorscale = "RdBu") %>%
  colorbar(title = "") %>% 
  layout(
    title = 'Number of Loans ',
    geo = g
  )




map <- map_data("state")
state_link <-tibble(abbr = state.abb,name = tolower(state.name))
state_centers <- bind_cols(as.tibble(state.center),state_link)
popDF <- read_csv("nst-est2019-alldata.csv")
popDF <- popDF %>%
  transmute(name = tolower(NAME),pop = POPESTIMATE2019)
state_counts<- finished_loans %>%
  count(addr_state, outcome) %>%
  group_by(addr_state) %>%
  mutate(prop = round(n/sum(n)*100,1),
         n_tot = sum(n)) %>%
  filter(outcome == "Good Loan") 
midpoint <- (max(state_counts$prop) + min(state_counts$prop)) / 2
labelDF <- state_counts %>%
  left_join(state_centers,by = c("addr_state" = "abbr"))
state_counts %>%
  left_join(state_link,by = c("addr_state" = "abbr")) %>%
  right_join(map,by = c("name" = "region")) %>%
  ggplot(aes(x = long,y = lat,group = group,fill = prop)) +
  geom_polygon(color = "white") +
  geom_text(data = labelDF,aes(x = x,y = y,group = NULL,label = paste0(prop,"%"))) +
  scale_fill_gradient2(low = "red",mid = "gray50",high = "blue",midpoint = midpoint,
                       labels = function(x){paste0(x,"%")},
                       n.breaks = 4,name = 'Percentage of \n"Good Loans"') +
  theme_void() +
  coord_fixed(ratio = 1.3)
#View(state.x77)
state_counts_2 <- state_counts %>%
  left_join(state_link,by = c("addr_state" = "abbr")) %>%
  left_join(popDF, by = "name") %>%
  ungroup() %>%
  mutate(perCap = (n_tot/pop)*100000) 
labelDF_2 <- labelDF %>% left_join(state_counts_2,by = "addr_state") %>%
  na_if(0)
state_counts_2 %>%
  mutate(perCap = ifelse(perCap<1,NA,perCap)) %>%
  right_join(map,by = c("name" = "region")) %>%
  ggplot(aes(x = long,y = lat,group = group,fill = perCap)) +
  geom_polygon(color = "white") +
  geom_text(data = labelDF_2,aes(x = x,y = y,group = NULL,fill= NULL,label = round(perCap,0))) +
  scale_fill_gradient(low = muted("red"),high = "blue",
                       labels = function(x){paste0(x)},
                       n.breaks = 4,name = 'Loans Per \n100,000 people') +
  theme_void() +
  coord_fixed(ratio = 1.3)


finished_loans %>%
  ggplot(aes(y = sub_grade,x = int_rate)) +
  geom_boxplot(aes(outlier.color = outcome)) +
  theme_minimal()

finished_loans %>%
  mutate(prob = runif(nrow(finished_loans))) %>%
  filter(prob > .95) %>%
  ggplot(aes(x = loan_amnt,y = int_rate)) +
  geom_point(alpha = .2)

split_loans <- initial_split(finished_loans%>%
                               select(outcome,term,int_rate,sub_grade,emp_length,home_ownership,annual_inc)%>%
                               drop_na(),
                              
                             strata = outcome,prop = .8)
loans_train_cv <- training(split_loans)
loans_train_cv <- initial_split(loans_train_cv,prop = .75)
loans_train <- training(loans_train_cv)
loans_cv <- testing(loans_train_cv)
loans_test <- testing(split_loans)
rm(list=c("split_loans","loans_train_cv"))

recipe_loan <- loans_train %>%
  recipe(outcome ~ term+int_rate+sub_grade+emp_length+home_ownership+annual_inc) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal()) %>%
  #step_corr(all_numeric()) %>%
  prep()

loans_train_baked <- juice(recipe_loan) #%>% bind_cols(loans_train%>%select(outcome))
loans_cv_baked <- recipe_loan %>%
  bake(loans_cv)
model<-glm(outcome ~ .,
           family = "binomial",data = loans_train_baked)
summary(model)
pred <- predict(model,newdata = loans_cv_baked,type = "response")

f1<- function(p) {
table<-loans_cv_baked %>%select(outcome_Good.Loan)%>%bind_cols(pred) %>%
  mutate(pred = ifelse(`...2`>p,"Good Loan Pred","Bad Loan Pred")) %>%
  select(-`...2`) %>%
  table()

table[1,1]
prec <- table[1,1]/(table[1,1]+table[1,2])
rec <- table[1,1]/(table[1,1]+table[2,1])
f <- 2*prec*rec/(prec+rec)
return(f)
}
p<-30:95/100
f<-sapply(p,f1)
p[which.max(f)]

bind_cols(p,f) %>%
  rename(p = `...1`,
         f = `...2`) %>%
  ggplot(aes(x = p,y=f)) +
  geom_point() +
  geom_vline(aes(xintercept = p[which.max(f)])) +
  geom_text(x = p[which.max(f)]+.08,y=max(f)-.1,label = p[which.max(f)]) +
  theme_minimal()

f1(.78)

library(randomForest)
summary(as.factor(loans_train_baked$outcome_Good.Loan))
model_rf <- randomForest(outcome_Good.Loan ~ ., data = loans_train_baked)
pred <- predict(loan_model,loans_cv_baked,type = "class")
mean(loans_test$outcome_Good.Loan == pred)


library(rpart)
loan_model <- rpart(outcome_Good.Loan ~ .,data = loans_train_baked,control = rpart.control(cp = 0.0014))
pred <- predict(loan_model,loans_cv_baked)
mean(loans_cv_baked$outcome_Good.Loan == ifelse(pred>.5,1,0))
