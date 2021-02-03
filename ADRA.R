library(DT)
library(shiny)
library(caret)
# Define the fields we want to save from the form
fields <- c("AGE", "GENDER", "BMI","SLEEP")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    
    titlePanel("Weight Management"),
    sidebarLayout(
      sidebarPanel(
        DT::dataTableOutput("responses", width = 300), tags$hr(),
        textInput(inputId = "AGE",label = "AGE",value = ""),
        selectInput(inputId = "GENDER",label = "GENDER" , choices = c("Male","Female")),
        textInput(inputId = "BMI",label = "BMI",value = ""),
        selectInput(inputId = "SLEEP",label = "SLEEP PATTERNS" , choices = c("LIGHT","DEEP","DISTURBED")),
        actionButton("submit", "Submit")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("FPlot", plotOutput("fplot")), 
          tabPanel("Summary",textOutput("text2")), 
          tabPanel("Table", tableOutput("rftab") , tableOutput("nbtab"), tableOutput("svmtab"),tableOutput("hmtab"))
      ))
  )),
  server = function(input, output, session) {
    
    #outputDir <- "responses"
    
    saveData <- function(data) {
      data <- t(data)
      # Write the file to the local system
      write.csv(
        x = data,
        file = file.path("C:/Users/Admin/Desktop","data.csv"), 
        row.names = FALSE, quote = TRUE
      )
    }
    
    # Load all previous responses
    # ---- This is one of the two functions we will change for every storage type ----
    loadData <- function() {
      if (exists("responses")) {
        #responses
      }
    }
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })
    
    observeEvent(input$submit, {
      library(randomForest)
      data1 <- read.table("C:/Users/Admin/Desktop/KARE/datasetold.csv",header = TRUE,sep = ",")
      data1_train <- data1[1:379,]
      data1_test <- data1[380:399,]
      a_test <-read.table("C:/Users/Admin/Desktop/data.csv",header = TRUE,sep = ",")
      user_data <- a_test[,]
      rfm <- randomForest(RECOMMENDATIONS ~ .,data = data1_train)
      levels(user_data$AGE) <- levels(data1_train$AGE)
      levels(user_data$GENDER) <- levels(data1_train$GENDER)
      levels(user_data$BMI) <- levels(data1_train$BMI)
      levels(user_data$SLEEP) <- levels(data1_train$SLEEP)
      a1 <- predict(rfm,data1_test)
      a11 <- predict(rfm,data1_train)
      rfmean <- mean(data1_test[,5]==a1)
      
      tab1 <- table(data1_test[,5],a1)
      tab11 <- table(data1_train[,5],a11)
      
      # Precision: tp/(tp+fp):
      rfprecision <- tab11[1,1]/sum(tab11[1,1:3])
      # Recall: tp/(tp + fn):
      rfrecall <- tab11[1,1]/sum(tab11[1:3,1])
      rfF_Score <- 2 * rfprecision * rfrecall /(rfprecision + rfrecall)
      
      output$rftab <- renderTable(tab1)
      p1 <- predict(rfm,user_data)
      
      #NAIVE BAYES:
      
      #install.packages('e1071',dependencies = TRUE)
      library(e1071)
      nbm <- naiveBayes(data1_train[,1:4],data1_train[,5])
      a2 <- predict(nbm,data1_test)
      a21 <- predict(rfm,data1_train)
      nbmean <- mean(data1_test[,5]==a2)
      
      tab2 <- table(data1_test[,5],a2)
      tab21 <- table(data1_train[,5],a21)
      
      # Precision: tp/(tp+fp):
      nbprecision <- tab21[1,1]/sum(tab21[1,1:3])
      # Recall: tp/(tp + fn):
      nbrecall <- tab21[1,1]/sum(tab21[1:3,1])
      nbF_Score <- 2 * nbprecision * nbrecall /(nbprecision + nbrecall)
      
      output$nbtab <- renderTable(tab2)
      p2 <- predict(nbm,user_data)
      
      #SVM
      library(e1071)
      model <- svm(RECOMMENDATIONS ~ ., data = data1_train)
      a3<- predict(object = model, newdata = data1_test, type = "class")
      a31 <- predict(object = model, newdata = data1_train, type = "class")
      svmmean <- mean(data1_test[,5]==a3)
      
      tab3 <- table(data1_test[,5],a3)
      tab31 <- table(data1_train[,5],a31)
      
      # Precision: tp/(tp+fp):
      svmprecision <- tab31[1,1]/sum(tab31[1,1:3])
      # Recall: tp/(tp + fn):
      svmrecall <- tab31[1,1]/sum(tab31[1:3,1])
      svmF_Score <- 2 * svmprecision * svmrecall /(svmprecision + svmrecall)
      
      output$svmtab <- renderTable(tab3)
      p3<- predict(object = model, newdata = user_data, type = "class")
      
      h <- 1:379
      for(i in 1:379)
      {
        if(a11[i]==a21[i] || a11[i]==a31[i]){
          h[i] <- a11[i]
        }
        else if(a21[i]==a31[i]){
          h[i] <- a21[i]
        }
        else{
          fmax <- max(rfF_Score,nbF_Score,svmF_Score)
          if(fmax == rfF_Score){
            h[i] <- a11[i]
          }
          else if(fmax == nbF_Score){
            h[i] <- a21[i]
          }
          else{
            h[i] <- a31[i]
          }
        }
      }
      
      h1 <- 1:20
      
      for(i in 1:20)
      {
        if(a1[i]==a2[i] || a1[i]==a3[i]){
          h1[i] <- a1[i]
        }
        else if(a2[i]==a3[i]){
          h1[i] <- a2[i]
        }
        else{
          fmax <- max(rfF_Score,nbF_Score,svmF_Score)
          if(fmax == rfF_Score){
            h1[i] <- a1[i]
          }
          else if(fmax == nbF_Score){
            h1[i] <- a2[i]
          }
          else{
            h1[i] <- a3[i]
          } 
        }
      }
      
      tab <- table(data1_train[,5],h)
      tab4 <- table(data1_test[,5],h1)
      
      # Precision: tp/(tp+fp):
      hprecision <- tab[1,1]/sum(tab[1,1:3])
      # Recall: tp/(tp + fn):
      hrecall <- tab[1,1]/sum(tab[1:3,1])
      hF_Score <- 2 * hprecision * hrecall /(hprecision + hrecall)
      
      output$hmtab <- renderTable(tab4)
      
      output$fplot <- renderPlot({
        a <- c("RF","NB","SVM","HYBRID")
        b <- c(rfF_Score,nbF_Score,svmF_Score,hF_Score)
        bubba <- data.frame(algm=a,f_score=b)
        plot(bubba)
      })
      
      if(p1==p2 || p1==p3){
        p <- p1
      }
      else if(p2==p3){
        p <- p2
      }
      else{
        fmax <- max(rfF_Score,nbF_Score,svmF_Score)
        if(fmax == rfF_Score){
          p <- p1
        }
        else if(fmax == nbF_Score){
          p <- p2
        }
        else{
          p <- p3
        }}
      
        output$text2 <- renderText({p1})
      
        if(p == "CONTROL")
        {
         if(user_data$AGE < 30 & user_data$GENDER == "Male"){
           output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL.To maintain normal BMI : 1.Have your breakfast regularly 2.Drink lots of water 3.For dinner have light food 4. Do Exercise atleast for 10 minutes daily" }) 
         }
         else if(user_data$AGE <30 & user_data$GENDER=="Female"){
           output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL.To maintain normal BMI : 1.Have your breakfast regularly 2.Drink lots of water 3.For dinner have light food 4. Do yoga and meditation daily" })       
         }  
         else if(user_data$AGE > 30 & user_data$AGE < 40 & user_data$GENDER=="Female"){
           output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL.To maintain normal BMI : 1.Have your breakfast regularly 2.Prefer having honey with warm water in the mornings 3.Drink lots of water 4.For dinner have light food 5.Do yoga and meditation daily 6.Eliminate processed carbohydrates" })     
         }
         else if(user_data$AGE > 30 & user_data$AGE < 40 & user_data$GENDER=="Male"){
           output$text1 <- renderText({ "Your BMI is NORMAL.To maintain normal BMI : 1.Have your breakfast regularly 2.Drink lots of water 3.For dinner have light food 4.Do Exercise atleast for 15 minutes daily 5.Avoid high calorie fast foods 6.Do yoga and meditation" }) 
         }
         else{
           output$text1 <- renderText({"Problem"})
         }
        }
        else if(p == "DIET"){
          if(user_data$SLEEP=="LIGHT")
          {  
            if(user_data$AGE <20 & user_data$GENDER=='Male'){
              output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL but to maintain this normality : 1.Try sleeping for a minimum of 6 hours daily 2.Replace one meal a day with a large salad and lean proteins 3.Cut down the consumption of junk foods 4.Eat bulk of foods in the mornings 5.Replace side dishes with steamed veggies 6.Prefer Green tea to regular drinks 7.Stock up on healthy foods" }) 
            }
            else if(user_data$AGE > 20 & user_data$AGE < 30 & user_data$GENDER=='Male'){
              output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL but to maintain this normality : 1.Try sleeping for a minimum of 6 hours daily 2.Replace one meal a day with a large salad and lean proteins 3.Skip desserts 4.Eat bulk of foods in the mornings 5.Replace side dishes with steamed veggies 6.Skip the venti lattes and opt for Green tea 7.Embrace oats 8.Prefer the stairs instead of Elevators" }) 
            }
            else if(user_data$AGE <20 & user_data$GENDER=='Female'){
              output$text1 <- renderText({"Congratulations!! Your BMI is NORMAL but to maintain this normality : 1.Try sleeping for a minimum of 6 hours daily 2.Replace one meal a day with a large salad and lean proteins 3.Cut down the consumption of junk foods 4.Eat bulk of foods in the mornings 5.Replace side dishes with steamed veggies 6.Try consuming tons of blueberries and honey 7.Embrace oats 8.Prefer the stairs instead of Elevators" }) 
            }
            else if(user_data$AGE > 20 & user_data$AGE < 25 & user_data$GENDER=='Female'){
              output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL but to maintain this normality : 1.Try sleeping for a minimum of 6 hours daily 2.Replace one meal a day with a large salad and lean proteins 3.Skip desserts 4.Eat bulk of foods in the mornings 5.Find a healthy meal and try having it all times 6.Skip the venti lattes and opt for Green tea 7.Embrace oats 8.Prefer the stairs instead of Elevators" }) 
            }
            else{
              output$text1 <- renderText({"problem1"})
            }
          }
          else
          {
            if(user_data$AGE <20){
              output$text1 <- renderText({ "Congratulations!! Your BMI is NORMAL but to maintain this normality : 1.Try following a regular sleeping schedule 2.Have a silent sleep environment 3.Replace one meal a day with a large salad and lean proteins 4.Skip desserts 5.Eat bulk of foods in the mornings 6.Find a healthy meal and try having it all times 7.Skip the venti lattes and opt for Green tea 8.Embrace oats 9.Prefer the stairs instead of Elevators" }) 
            }
            else if(user_data$AGE > 20 & user_data$AGE < 35){
              output$text1 <- renderText({"Congratulations!! Your BMI is NORMAL but to maintain this normality : 1.Try following a regular sleeping schedule 2.Have a silent sleep environment 3.Eat before 2 hours of sleep to avoid indigestion 4.Replace one meal a day with a large salad and lean proteins 5.Skip desserts 6.Eat bulk of foods in the mornings 7.Find a healthy meal and try having it all times 8.Skip the venti lattes and opt for Green tea 9.Embrace oats 10.Prefer the stairs instead of Elevators" }) 
            }
            else{
              output$text1 <- renderText({"problem2"})
            }
          }
        }
        else
          {
            if(user_data$BMI < 30){
              
            if(user_data$AGE > 30 & user_data$GENDER=='Male' & user_data$SLEEP=="LIGHT"){
              output$text1 <- renderText({"Hey! Time to burn calories... Try following the below for a healthy lifestyle: 1.Do exercise for a minimum of 30 minutes regularly 2.Cycle your carb intake based on your activity level 3.Start your meals with salad 4.Switch to calorie free drinks 5.Consume baked foods rather than fried items 6.Prefer taking the stairs and not the elevators 7.While making telephone calls walk up and down 8.Exercise intelligently and reduce upto 100 calories per day 9.Try Sleeping for a minimum of 6 hours " }) 
            }
            else if(user_data$AGE > 25 & user_data$GENDER=='Female' & user_data$SLEEP=="LIGHT"){
              output$text1 <- renderText({ "Hey! Time to burn calories... Try following the below for a healthy lifestyle: 1.Do exercise for a minimum of 30 minutes regularly 2.Do meditation to control your stress 3.Take hot water with lemon and honey before breakfast 4.Switch to calorie free drinks 5.Give more pressure to hands to reduce arm fat 6.Prefer taking the stairs and not the elevators 7.While making telephone calls walk up and down 8.Try Sleeping for a minimum of 6 hours"}) 
            }
            else if(user_data$AGE > 35 & user_data$SLEEP=="DISTURBED"){
              output$text1 <- renderText({ "Hey! Time to burn calories... Try following the below for a healthy lifestyle: 1.Do exercise for a minimum of 30 minutes regularly 2.Do meditation to control your stress 3.Prefer oat meals 4.Increase the intake of protein rich food 5.Listening to Light music can help you for a deep sleep 6.Night walk for 10 minutes 9.Relax yourself before sleep so that you can avoid disturbed sleep" }) 
            }
            else if(user_data$AGE > 40 & user_data$SLEEP=="DEEP"){
              output$text1 <- renderText({ "Hey! Time to burn calories... Try following the below for a healthy lifestyle: 1.Do light exercises for a minimum of 30 minutes regularly 2.Never skip breakfast 3.Minimize the carbohydrates intake during dinner times 4.Increase the intake of protein rich food 5.Avoid high calorie foods 6.Drink lots of water "}) 
            }
            else{
              output$text1 <- renderText({"problem 3"})
            }
           }
           else
           {
            if(user_data$AGE > 30 & user_data$GENDER=='Male' & user_data$SLEEP=="LIGHT"){
              output$text1 <- renderText({ "Hey! Time to burn lots of calories... Try following the below for a healthy lifestyle: 1.Do heavy exercises like walking,swimming,jogging 2.Never skip breakfast 3.Prefer oat meals 4.Prefer alternative grains like ragi,wheat,barley and brown rice to rice 5.Reduce the intake of meat and prefer veggies and fish 7.Avoid consumption of Alcohol and drugs" })   
            }
            else if(user_data$AGE > 25 & user_data$GENDER=='Female' & user_data$SLEEP=="LIGHT"){
              output$text1 <- renderText({ "Hey! Time to burn lots of calories... Try following the below for a healthy lifestyle: 1.Do heavy exercises like walking,skipping,jogging 2.Never skip breakfast 3.Prefer oat meals 4.Prefer brown rice to white rice 5.Reduce the intake of meat and prefer veggies and fish 7.Avoid drinking too much of water at night" })   
            }
            else if(user_data$AGE > 35 & user_data$SLEEP=="DISTURBED"){
              output$text1 <- renderText({ "Hey! Time to burn lots of calories... Try following the below for a healthy lifestyle: 1.Listening to light music can help you having a deep sleep 2.Have a proper sleep for a mininum of 6 hours 3.Never skip breakfast 4.Drink lots of water which helps you for digestion 5.Reduce the intake of meat and prefer fish 6.Avoid high calorie foods like cheese,butter and ghee 7.Avoid consumption of Alcohol and drugs 8.Do heavy execises twice a day" })
            }  
            else if(user_data$AGE > 40 & user_data$SLEEP == "DEEP"){
              output$text1 <- renderText({ "Hey! Time to burn lots of calories... Try following the below for a healthy lifestyle: 1.Do exercises like walking,jogging 2.Never skip breakfast 3.Prefer oat meals 4.Prefer alternative grains like ragi,wheat,barley and brown rice to rice 5.Reduce the intake of meat and prefer veggies and fish 7.Avoid consumption of Alcohol and drugs" })   
            }
            else{
              output$text1 <- renderText({"problem4"})
            }
           }
        }
        })
    }
)

    
  
