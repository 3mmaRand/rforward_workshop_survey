library(likert)
library(psych)
library(tidyverse)
library(readxl)
library(gridExtra)

# data file .gitignore

# import
pre <- read_excel("pre-survey.xlsx") 
post <- read_excel("post-survey.xlsx") 

####################################
# columns about previous experience
###################################

pre_exp <- pre %>% 
  select(starts_with("Please indicate your previous" )) %>%
  drop_na() %>% 
  data.frame(check.names = FALSE)

exp_q <- "Please indicate your previous experience of the following: "

names(pre_exp) <- names(pre_exp) %>% 
  sub(pattern = paste0(exp_q, "\\["), 
      replacement = "") %>% 
  sub(pattern = "\\]", replacement = "")         

levels <- c("No", "Uncertain", "Yes")

pre_exp <- pre_exp %>% 
  mutate(`I know where to find R packages on my compute` =
          factor(`I know where to find R packages on my compute`, 
                 levels = levels) ,
         `I've made R Projects before` =
          factor(`I've made R Projects before`, 
                 levels = levels) ,
         `I have a Github account` =
           factor(`I have a Github account`, 
                  levels = levels) ,
         `I've written a unit test before (in any language)` =
           factor(`I've written a unit test before (in any language)`, 
                  levels = levels) ,
         `I regularly work with R in the academic or professional setting` =
           factor(`I regularly work with R in the academic or professional setting`, 
                  levels = levels) )

post_exp <- post %>% 
  select(starts_with("Please indicate your current" )) %>% 
  drop_na() %>% 
  data.frame(check.names = FALSE)

exp_q <- "Please indicate your current experience of the following: "

names(post_exp) <- names(post_exp) %>% 
  sub(pattern = paste0(exp_q, "\\["), 
      replacement = "") %>% 
  sub(pattern = "\\]", replacement = "")         

post_exp <- post_exp %>% 
  mutate(`I know where to find R packages on my compute` =
           factor(`I know where to find R packages on my compute`, 
                  levels = levels) ,
         `I've made R Projects before` =
           factor(`I've made R Projects before`, 
                  levels = levels) ,
         `I have a Github account` =
           factor(`I have a Github account`, 
                  levels = levels) ,
         `I've written a unit test before (in any language)` =
           factor(`I've written a unit test before (in any language)`, 
                  levels = levels) ,
         `I regularly work with R in the academic or professional setting` =
           factor(`I regularly work with R in the academic or professional setting`, 
                  levels = levels) )


presum <- likert(pre_exp)
postsum <- likert(post_exp)

order <- names(pre_exp)

a <- plot(presum,
          group.order = order,
          legend.position = "top",
          legend = "",
          text.size = 4,
          low.color = "lightblue",
          high.col = "lightgreen")
b <- plot(postsum,
          group.order = order,
          legend.position = "top",
          legend = "",
          text.size = 4,
          low.color = "lightblue",
          high.col = "lightgreen")

grid.arrange(a, b)

####################################
# columns about difficulty
###################################

pre_diff <- pre %>% 
  select(starts_with("How do you rate" )) %>%
  drop_na() %>% 
  data.frame(check.names = FALSE)

diff_q <- "How do you rate the difficulty of the following tasks"

names(pre_diff) <- names(pre_diff) %>% 
  sub(pattern = paste0(diff_q, "\\?\\s+\\["), 
      replacement = "") %>% 
  sub(pattern = "\\]", replacement = "")         

levels <- c("Much harder than average",
            "A little harder than average",
            "Average R difficulty level",
            "A little easier than average",
            "Much easier than average")

pre_diff <- pre_diff %>% 
  mutate(`Building R Functions` =
           factor(`Building R Functions`, 
                  levels = levels) ,
         `Constructing R Packages` =
           factor(`Constructing R Packages`, 
                  levels = levels) ,
         `Creating Unit Tests` =
           factor(`Creating Unit Tests`, 
                  levels = levels) ,
         `Using Git and Github` =
           factor(`Using Git and Github`, 
                  levels = levels) ,
         `Using R Projects` =
           factor(`Using R Projects`, 
                  levels = levels) )

post_diff <- post %>% 
  select(starts_with("How do you rate" )) %>% 
  drop_na() %>% 
  data.frame(check.names = FALSE)

names(post_diff) <- names(post_diff) %>% 
  sub(pattern = paste0(diff_q, "\\?\\s+\\["), 
      replacement = "") %>% 
  sub(pattern = "\\]", replacement = "")         


post_diff <- post_diff %>% 
  mutate(`Building R Functions` =
           factor(`Building R Functions`, 
                  levels = levels) ,
         `Constructing R Packages` =
           factor(`Constructing R Packages`, 
                  levels = levels) ,
         `Creating Unit Tests` =
           factor(`Creating Unit Tests`, 
                  levels = levels) ,
         `Using Git and Github` =
           factor(`Using Git and Github`, 
                  levels = levels) ,
         `Using R Projects` =
           factor(`Using R Projects`, 
                  levels = levels) )


presum <- likert(pre_diff)
postsum <- likert(post_diff)

order <- names(pre_diff)

a <- plot(presum,
          group.order = order,
          legend.position = "top",
          legend = "",
          text.size = 4,
          low.color = "lightblue",
          high.col = "lightgreen")
b <- plot(postsum,
          group.order = order,
          legend.position = "top",
          legend = "",
          text.size = 4,
          low.color = "lightblue",
          high.col = "lightgreen")

grid.arrange(a, b)
