library(plotly)
library(dplyr)
library(shiny)
demo = read.csv("\Users\aditi\OneDrive\Desktop\Winter Sem\Data Visualisation\Project\Project\EDA_and_Preproc\demographic.csv")
View(demo)

groupM <- demo %>% filter(sex == "Male")
groupF <- demo %>% filter(sex == "Female")





## RACE AND GENDER

gBarChart <- data.frame(race = names(table(groupM$race)),
                        GroupM = as.numeric(table(groupM$race)),
                        GroupF = as.numeric(table(groupF$race)))

gBarChart <- gBarChart %>% arrange(desc(GroupM))


 plot_ly(gBarChart,
              x = ~race,
              y = ~GroupM,
              type = "bar",
              name = "Male",
              marker = list(color = "rgba(53, 61, 219, 0.7)",
                            line = list(color = "rgba(53, 61, 219, 0.5)",
                                        width = 1.5))
              ) %>% 
  add_trace(y = ~GroupF,
            name = "Female",
            marker = list(color = "rgba(219, 53, 133, 0.7)",
                          line = list(color = "rgba(219, 53, 133, 0.5)",
                                      width = 1.5))
      ) %>% 
  layout(yaxis = list(title = "Students"),
         barmode = "stack",
         xaxis = list(title = "Percent",
                      categoryorder = "array",
                      categoryarray = gBarChart$race))
 
 
 
 
# AGE AND GENDER
 
 ABarChart <- data.frame(age = names(table(groupM$age)),
                         GroupM = as.numeric(table(groupM$age)),
                         GroupF = as.numeric(table(groupF$age)))
 
 
 plot_ly(ABarChart,
         x = ~age,
         y = ~GroupM,
         type = "bar",
         name = "Male",
         marker = list(color = "rgba(53, 61, 219, 0.7)",
                       line = list(color = "rgba(53, 61, 219, 0.5)",
                                   width = 1.5))
 ) %>% 
   add_trace(y = ~GroupF,
             name = "Female",
             marker = list(color = "rgba(219, 53, 133, 0.7)",
                           line = list(color = "rgba(219, 53, 133, 0.5)",
                                       width = 1.5))
   ) %>% 
   layout(yaxis = list(title = "Students"),
          barmode = "stack",
          xaxis = list(title = "Percent",
                       categoryorder = "array",
                       categoryarray = gBarChart$age))

 
 
 
 
 # GRADE AND GENDER  // MAYBE GRADE AND AGE 
 
 GBarChart <- data.frame(grade = names(table(groupM$grade)),
                         GroupM = as.numeric(table(groupM$grade)),
                         GroupF = as.numeric(table(groupF$grade)))
 
 
 plot_ly(GBarChart,
         x = ~grade,
         y = ~GroupM,
         type = "bar",
         name = "Male",
         marker = list(color = "rgba(53, 61, 219, 0.7)",
                       line = list(color = "rgba(53, 61, 219, 0.5)",
                                   width = 1.5))
 ) %>% 
   add_trace(y = ~GroupF,
             name = "Female",
             marker = list(color = "rgba(219, 53, 133, 0.7)",
                           line = list(color = "rgba(219, 53, 133, 0.5)",
                                       width = 1.5))
   ) %>% 
   layout(yaxis = list(title = "Students"),
          barmode = "stack",
          xaxis = list(title = "Percent",
                       categoryorder = "array",
                       categoryarray = gBarChart$grade))
 
###############

 