library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


shinyServer(function(input, output){
    
    
    output$students <- renderValueBox({
         valueBox(value = total,subtitle = "Surveyed Students (Ages 12-18)",icon = icon("user-graduate"),color = "light-blue")
    })
    output$girls <- renderValueBox({
        valueBox(value = girls,subtitle = "Girls in Survey",icon = icon("female"),color = "maroon")
    })
    output$boys <- renderValueBox({
        valueBox(value = boys,subtitle = "Boys in Survey",icon = icon("male"),color = "blue")
    })
    output$mgrade <- renderValueBox({
        valueBox(value = paste0(median_grade,"th grade"),subtitle = "Meidan Grade of Students",icon = icon("school"),color = "olive")
    })
    output$mage <- renderValueBox({
        valueBox(value = median_age,subtitle = "Median Age of Students",icon = icon("birthday-cake"),color = "orange")
    })
    output$gen_race <- renderPlotly({gender_race_plot})
    output$gen_age <- renderPlotly({age_gender_plot})
    output$kde_h <- renderPlotly({kde_height})
    output$kde_w <- renderPlotly({kde_weight})
    
##### weapons
    output$All <- renderValueBox({
      valueBox(value = formatC(wep_all, format = "d", digits = 5),subtitle = "Responding Students Reported Carrying Any Weapon (Past 30 Days)",icon = icon("exclamation-triangle"),color = "maroon")
    })
    output$Gun <- renderValueBox({
      valueBox(value = formatC(wep_gun, format = "d", digits = 5),subtitle = "Responding Students Reported Carrying a Gun (Past 12 Months)",icon = icon("exclamation"),color = "orange")
    })
    output$To_sch <- renderValueBox({
      valueBox(value = formatC(wep_tosch, format = "d", digits = 5),subtitle = "Responding Students Reported Carrying Any Weapon to School (Past 30 Days)",icon = icon("school"),color = "light-blue")
    })
    output$Injured <- renderValueBox({
      valueBox(value = formatC(wep_inj, format = "d", digits = 5),subtitle = "Responding Students Reported being Injured by Any Weapon at School (Past 12 Months)",icon = icon("plus-square"),color = "green")
    })
    
         output$weapons_all <- renderPlotly({ 
            cdc_data2%>%
            ggplot(aes(x = weapons_all, weight = weight)) +
            geom_bar(aes(fill = cdc_data2[,input$var_select10]), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Carried Weapon", fill = input$var_select10)})
         
    
         output$weapons_guns <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = weapons_gun, weight = weight)) +
            geom_bar(aes(fill = cdc_data2[,input$var_select11]), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Carried Gun",fill = input$var_select11)})
    
        output$weapon_sch <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = weapons_toschool, weight = weight)) +
            geom_bar(aes(fill = cdc_data2[,input$var_select12]), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Carried Weapon to School",fill = input$var_select12)})
    
        output$weapons_inj <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = inj_weapon, weight = weight)) +
            geom_bar(aes(fill = cdc_data2[,input$var_select13]), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Injured by Weapon at School",fill = input$var_select13)})
    
    
 ### bully  
         output$Electronic <- renderValueBox({
             valueBox(value = formatC(elec_risk, format = "d", digits = 5),subtitle = "Responding Students Electronically Bullied (Past 12 Months)",icon = icon("mobile-alt"),color = "light-blue")
         })
         output$School <- renderValueBox({
             valueBox(value = formatC(sch_risk, format = "d", digits = 5),subtitle = "Responding Students Bullied at School (Past 12 Months)",icon = icon("school"),color = "light-blue")
         })
         output$Elec_resp <- renderValueBox({
             valueBox(value = formatC(elec_resp, format = "d", digits = 5),subtitle = "Response Rate of Students Electronicly Bullied",icon = icon("mobile-alt"),color = "maroon")
         })
         output$School_resp <- renderValueBox({
             valueBox(value = formatC(sch_resp, format = "d", digits = 5),subtitle = "Response Rate of Students Bullied at School",icon = icon("school"),color = "maroon")
         })
         
         
         output$bully_elec <- renderPlotly( {cdc_data2 %>%
            ggplot(aes(x = bullied_elec, weight = weight)) +
            geom_bar(aes(fill = cdc_data2[,input$var_select5]), position = "fill") +
            labs(y="Proportion", x = " Bullied ", fill = input$var_select5)})
         
         output$bully_sch <- renderPlotly({cdc_data2 %>%
                 ggplot(aes(x = bullied_sch, weight = weight)) +
                 geom_bar(aes(fill = cdc_data2[,input$var_select6]), position = "fill") +
                 labs(y="Proportion", x = " Bullied",fill = input$var_select6)})
         
         

    output$times_used <- renderPlot({times_used})
    output$lf_use <- renderPlot({lf_use_plot})
    output$drug_wt <- renderPlot({drug_wt})
    output$drug_eth <- renderPlot({drug_eth})
    output$drug_grade <- renderPlot({drug_grade})
    output$drug_age <- renderPlot({drug_age})
    output$drug_gen <- renderPlot({drug_gen})

    output$heroin <- renderValueBox({
        valueBox(value = formatC( hero_risk, format = "d", digits = 5),subtitle = "Students Using Heroin (Lifetime)",icon = icon("syringe"),color = "light-blue")
        })
    output$Inhalants <- renderValueBox({
        valueBox(value = formatC( inha_risk, format = "d", digits = 5),subtitle = "Students Using Inhalants (Lifetime)",icon = icon("spray-can"),color = "maroon")
         })
    output$MDMA <- renderValueBox({
        valueBox(value = formatC( mdma_risk, format = "d", digits = 5),subtitle = "Students Using MDMA (Lifetime)",icon = icon("tablets"),color = "blue")
        })
    output$Methamphetamines <- renderValueBox({
        valueBox(value = formatC( meth_risk, format = "d", digits = 5),subtitle = "Students Using Methamphetamines (Lifetime)",icon = icon("bong"),color = "olive")
        })
    output$Opioids<- renderValueBox({
        valueBox(value = formatC( opi_risk, format = "d", digits = 5),subtitle = "Students Using Opioids (Illicit)",icon = icon("pillsf"),color = "orange")
        })

    output$gen_drug_use <- renderPlotly({gen_drug_use})
    
    output$mytable <- DT::renderDataTable({code})
    
    output$all_var <- renderPlotly({cdc_data2 %>%
        ggplot(aes(x = cdc_data2[,input$var_select19], weight = weight)) +
        geom_bar(aes(fill = cdc_data2[,input$var_select20]), position = "fill") +
        labs(y="Proportion", x = " Variable Levels",fill = input$var_select20)})

    
    output$PerSchool<- renderValueBox({
        valueBox(value = formatC( per_school, format = "d", digits = 5),subtitle = "School Response Rate",icon = icon("school"),color = "light-blue")
    })
    output$PerSubmit<- renderValueBox({
        valueBox(value = formatC( per_submit, format = "d", digits = 5),subtitle = "Student Response Rate",icon = icon("user-graduate"),color = "maroon")
    })
    output$OverallPer<- renderValueBox({
        valueBox(value = formatC( overall_per, format = "d", digits = 5),subtitle = "Overall Response Rate",icon = icon("poll-h"),color = "blue")
    })
    
})

