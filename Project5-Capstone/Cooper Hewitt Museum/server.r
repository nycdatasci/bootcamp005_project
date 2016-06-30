# COOPER HEWITT MUSEUM PROJECT
# server.r

# library(shiny)
# library(RColorBrewer)
# library(dplyr)
# library(ggplot2)
# library(DT) # library for datatables
# library(shinythemes)

library(recommenderlab)

server <- function(input, output, session) {
  
  #################################
  ###### RECOMMENDATION TAB  ######
  #################################
  
  # user selection to subset dataframe
  recom_data = reactive({
    # for debugging
    print(input$input_decade)
    print(input$input_country)
    
    # filtering dataframe
    if (input$input_decade == "" & input$input_country == "") {
      # print ('1st if')
      ret_nulls = sample_n(objects_info, 5, replace = F)
      return(ret_nulls)
      
    } else if ((input$input_decade != "") & (input$input_country != "")) {
      # print ('2nd if')
      print (input$input_decade)
      print (input$input_country)

      fil_both = filter(objects_info, 
                        decade %in% input$input_decade & decade != " ", 
                        (woe.country_name %in% input$input_country & woe.country_name != " " & woe.country_name != ""))

      # print("After fil_both")      
      if (nrow(fil_both) > 5) {
        print("In if")
        fil_both = fil_both %>% sample_n(size = 5, replace = F)        
      }
      
      return(fil_both)
      
    } else if (input$input_decade != "") {
      print ('3rd if')
      fil_decade = filter(objects_info, decade %in% input$input_decade & decade != " ") %>%
        sample_n(size = 5, replace = F)
      return(fil_decade)
      
    } else if (input$input_country != "") {
      # print ('4th if')
      fil_country = filter(objects_info,
                           woe.country_name %in% input$input_country &
                             woe.country_name != " " &
                             woe.country_name != "") %>%
        sample_n(size = 5, replace = F)
      return(fil_country)
      
    } else {
      # print ('5th if')
      ret_nulls = sample_n(objects_info, 5, replace = F)
      return(ret_nulls)
    }
  })
  
  # outputting images for codestart
  # still need to deal with missing primary image fields
  output$picture1 <- renderText({
    c('<img src="',as.character(recom_data()$primary_image.1[1]),'" width = "150" >')
  })
  output$picture2 <- renderText({
    c('<img src="',as.character(recom_data()$primary_image.1[2]),'" width = "150" >')
  })
  output$picture3 <- renderText({
    c('<img src="',as.character(recom_data()$primary_image.1[3]),'" width = "150" >')
  })
  output$picture4 <- renderText({
    c('<img src="',as.character(recom_data()$primary_image.1[4]),'" width = "150" >')
  })
  output$picture5 <- renderText({
    c('<img src="',as.character(recom_data()$primary_image.1[5]),'" width = "150" >')
  })
  
  ## creating recommendations ##
  obj_recom <- eventReactive(input$recommend,  {
    # collecting object_id from filtered and sampled data above
    id1 <- as.character(recom_data()$id[1])
    id2 <- as.character(recom_data()$id[2])
    id3 <- as.character(recom_data()$id[3])
    id4 <- as.character(recom_data()$id[4])
    id5 <- as.character(recom_data()$id[5])
    
    # concantinating object ids
    id_s <- c(id1, id2, id3, id4, id5)
    # creating list of 0/1 from radio inputs
    id_bools <- c(input$radio1, input$radio2, input$radio3, input$radio4, input$radio5)
    # print('id_s')
    # print(id_s)
    
    # looping to collect the object ids that user liked
    id_real <- c()
    count = 0
    for (i in 1:length(id_bools)) {
      if (id_bools[i] == 1) {
        count = count + 1
        id_real[count] = id_s[i]
      }
    }
    # print(length(objects_info$date))
    print('id_real is here')
    print(id_real)
    # print(length(vect_names))
    
    # for loop to collect the index of matching objects that user liked and vect_names
    # use this to construct binary matrix for user likes
    like_1s <- c()
    print(length(id_real))
    for (i in 1:length(id_real)) {
      print(i)
      for (j in 1:length(vect_names)) {
        if (id_real[i] == vect_names[j]) {
          like_1s = c(like_1s, j)
          print(like_1s)
        }
      }
    }
    
    print('like_1s')
    print(like_1s)
    print('length(like_1s)')
    print(length(like_1s))
    
    # initializeing an empty matrix
    user_likes = matrix(0, nrow = 1, ncol = ncol(visitsCF))
    
    # putting value of 1 in cols that user liked
    for (i in 1:length(like_1s)) {
      user_likes[1, like_1s[i]] = 1
    }
    
    # coercing the matrix into recommender lab object for predicting
    user_likes = as(user_likes, "binaryRatingMatrix")
    print('user_likes')
    print(user_likes)
    
    # predicting by user likes
    user_pred = recommenderlab::predict(itembased, user_likes, n = 5)
    print ('user_pred')
    print (getList(user_pred))
    print (getList(user_pred)[[1]])
    # collecting the recommendations into list
    user_recs = recommenderlab::getList(user_pred)[[1]]
    print ('user_recs')
    print (as.numeric(vect_names[as.numeric(user_recs[1])]))
    # filtering out recomended objects from object_info
    # returns a filtered dataframe
    recommendations = filter(objects_info,
                             id == as.numeric(vect_names[as.numeric(user_recs[1])])|
                               id == as.numeric(vect_names[as.numeric(user_recs[2])]) |
                               id == as.numeric(vect_names[as.numeric(user_recs[3])]) |
                               id == as.numeric(vect_names[as.numeric(user_recs[4])]) |
                               id == as.numeric(vect_names[as.numeric(user_recs[5])])
    )
    print('length recommendation$id')
    print(length(recommendations$id))
    print(recommendations)
    return(recommendations)
  })
  
  
  # outputting images for recommendations
  output$rec_pic1 <- renderText({
    c('<img src="',as.character(obj_recom()$primary_image.1[1]),'" width = "150" >')
  })
  output$rec_pic2 <- renderText({
    c('<img src="',as.character(obj_recom()$primary_image.1[2]),'" width = "150" >')
  })
  output$rec_pic3 <- renderText({
    c('<img src="',as.character(obj_recom()$primary_image.1[3]),'" width = "150" >')
  })
  output$rec_pic4 <- renderText({
    c('<img src="',as.character(obj_recom()$primary_image.1[4]),'" width = "150" >')
  })
  output$rec_pic5 <- renderText({
    c('<img src="',as.character(obj_recom()$primary_image.1[5]),'" width = "150" >')
  })
  
  
}

