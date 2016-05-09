library(shiny)
library(ggplot2)
library(reshape)
library(plotly)
library(maps)
library(readxl)
library(dplyr)

Hometype <- c("All Homes" = "All Homes", 
              "Single Family" = "Single Fam",
              "Condo" = "Condo", 
              "One Bed" = "One Bed",
              "Two Bed"="Two Bed",
              "Three Bed"="Three Bed")

CA_county<-c("CA"="CA","Alameda"="Alameda","Amador"="Amador","Butte"="Butte",                            
             "Calaveras"="Calaveras", "Contra-Costa"="Contra-Costa","Del_Norte"="Del_Norte",                            "El_Dorado"="El_Dorado","Fresno"="Fresno","Glenn"="Glenn","Humboldt"="Humboldt",                           "Kern"="Kern","Kings"="Kings","Lake"="Lake","Los Angeles"="Los_Angeles",                                   "Madera"="Madera","Marin"="Marin","Mariposa"="Mariposa","Mendocino"="Mendocino",                           "Merced"="Merced","Monterey"="Monterey","Napa"="Napa","Nevada"="Nevada",
             "Orange"="Orange","Placer"="Placer","Plumas"="Plumas","Riverside"="Riverside",
             "Sacramento"="Sacramento","San Benito"="San_Benito","San Bernardino"="San_Bernardino",
             "San Diego"="San_Diego","San Francisco"="San_Francisco","San Joaquin"="San_Joaquin",
             "San Luis Obispo"="San_Luis_Obispo","San Mateo"="San_Mateo","Santa Barbara"="Santa_Barbara",
             "Santa Clara"="Santa_Clara","Santa Cruz"="Santa_Cruz","Shasta"="Shasta",
             "Siskiyou"="Siskiyou","Solano"="Solano","Sonoma"="Sonoma","Stanislaus"="Stanislaus",
             "Sutter"="Sutter","Tehama"="Tehama","Tulare"="Tulare","Tuolumne","Tuolumne",
             "Ventura"="Ventura","Yolo"="Yolo", "Yuba"="Yuba",
             "Los Angeles Metropolitan Area"="Los_Angeles_Metropolitan_Area", 
             "S.F.Bay Area"="S.F._Bay_Area","Inland_Empire"="Inland_Empire")
rentzipdata<-read.csv("Zip_Zri_AllHomes_rent.csv")
housingzipdata<-read.csv("Zip_Zhvi_AllHomes.csv")

shinyServer(function(input, output) {

    output$text1 <- renderText({
      paste("Monthly Payment for own home is $",(summarydata()[1,3]+summarydata()[1,2])/12)
    })
    output$text2 <- renderText({
      paste("Monthly Payment for rent home is $",input$rent_rate_monthly)
    })
    output$text3 <- renderText({
      #  paste("This app is",input$downpayment,"%",input$var,"!!!")
#      paste("Monthly Payment is $",(summarydata()[1,3]+summarydata()[1,2])/12 )
      home_cost<-sum(summarydata()[1:input$years_stay,5])+
        input$home_price*(1+input$home_price_growth_rate*0.01)^input$years_stay*input$cost_sell_rate*0.01
      paste("Total cost for",input$years_stay, "years own home is $",round(home_cost,0))
#      paste("Total Interest $",total_interest<-sum(summarydata()[3]) )
#      paste("Total payments for",input$years_stay, "years is $",(summarydata()[1,3]+summarydata()[1,2]) )
    })
    output$text4 <- renderText({
      rent_cost<-sum(summarydata()[1:input$years_stay,6])
      paste("Total cost for",input$years_stay, "years rent home is $",round(rent_cost,0))
    })    

    output$text5 <- renderText({
      home_intrest<-sum(summarydata()[,3])
      paste("Total interest you will pay is $",round(home_intrest,0))
    })    
    output$text6 <- renderText({
        years_term<-dim(summarydata())[1]
        for (i in 1:years_term) {
          home_cost<-sum(summarydata()[1:i,5])+
         input$home_price*(1+input$home_price_growth_rate*0.01)^i*input$cost_sell_rate*0.01
          rent_cost<-sum(summarydata()[1:i,6])
          year_breakeven<-i
          if (home_cost<rent_cost){
            break
          }
        }
      paste("If you will stay longer than",year_breakeven, "years, then it's better to buy")
    
    })
    
  
summarydata<-  reactive({
  #loan_term=Home_Global$loan_term
  if (input$loan_term=="10 years") {
    loan_term=10
  } else if (input$loan_term=="15 years")
  {
    loan_term=15
  } else if (input$loan_term=="20 years")
  {
    loan_term=20
  } else if (input$loan_term=="30 years")
  {
    loan_term=30
  }  
  downpayment=input$downpayment*0.01
  interest_rate=input$interest_rate*0.01
  home_price_growth_rate=input$home_price_growth_rate*0.01
  rent_growth_rate=input$rent_growth_rate*0.01
  invest_rate=input$invest_rate*0.01
  Home_property_tax=input$Home_property_tax*0.01
  Income_tax_rate=input$Income_tax_rate*0.01
  cost_buy_rate=input$cost_buy_rate*0.01
  cost_sell_rate=input$cost_sell_rate*0.01
  
  
  
  loan_amount=input$home_price*(1.0-downpayment)
  n=loan_term*12
  im=interest_rate/12
  monthpay=loan_amount*(im*(im+1)^n)/((1+im)^n-1)
  Interest_month=rep(0,n)
  Pricipal_month=rep(0,n)
  Balance_month=rep(0,n)
  #  print(im)
  for (i in 1:n){
    Balance_month[i]=loan_amount*((1+im)^n-(1+im)^i)/((1+im)^n-1)
    if (i==1) {
      Pricipal_month[i]=loan_amount-Balance_month[1] 
    } else{
      Pricipal_month[i]=Balance_month[i-1]-Balance_month[i]
    }
    Interest_month[i]=monthpay-Pricipal_month[i]
  } 
  Interest_year=rep(0,loan_term)
  Pricipal_year=rep(0,loan_term)
  Balance_year=rep(0,loan_term)
  for (iy in 1:loan_term) {
    i=(iy-1)*12
    Interest_year[iy]=sum(Interest_month[(1+i):(12+i)]) 
    Pricipal_year[iy]=sum(Pricipal_month[(1+i):(12+i)]) 
    Balance_year[iy]=Balance_month[iy*12]
  }
  format(Sys.Date(), "%c")
  year=as.numeric(R.Version()$year)
  years=year+(1:loan_term)-1
##-------------------------------------
  rents_year=rep(0,loan_term)
  home_cost=rep(0,loan_term)
  home_invest_cost=input$home_price*input$downpayment*0.01
  newhome_price=input$home_price
  for (i in 1:loan_term) {
    rents_year[i]=input$rent_rate_monthly*12*(1+rent_growth_rate)^(i-1)
    home_invest_cost=Pricipal_year[i]+home_invest_cost
    home_cost[i]=-newhome_price*home_price_growth_rate+
      input$Home_insurance+
      Home_property_tax*input$home_price*(1+home_price_growth_rate)^i*(1-Income_tax_rate)+
      input$Home_Maintenance_year+
      Interest_year[i]*(1-Income_tax_rate)+
     home_invest_cost*invest_rate
    newhome_price=newhome_price*(1.0+home_price_growth_rate)
  }

   df<-data.frame(year=years,round(Pricipal_year,2),round(Interest_year,2),
  round(Balance_year,2),round(home_cost,2),round(rents_year,2))
  names(df) = c("year", "Pricipal","Interest","Balance","Cost Own Home","Cost Rent Home")
#  df<-data.frame(year=years,round(Pricipal_year,2),round(Interest_year,2),round(Balance_year,2))
#  names(df) = c("year", "Pricipal","Interest","Balance")
  df
  })


  output$plot <- renderPlotly({
    year_summary<-summarydata()[,1:4]
    year_summary_long<-melt(year_summary,id="year",measure=c("Pricipal","Interest"))
    names(year_summary_long) = c("year", "category", "value")
    g1<-ggplot(data=year_summary_long,aes(x=year,y=value, fill=category))+
      geom_bar(stat ="identity", colour = 'white', position = 'dodge')+
      xlab("Year")+ylab("Yearly Payment ($)") + ggtitle("Summary of payment for Home owner")
      scale_fill_manual(values = c("#ff0000", "#2372b2"), labels = c("Principal","Interest"))+ 
        theme(plot.title = element_text(size=22))
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
      g1
      ggplotly(g1)
     })
  
  output$rentplot<-renderPlotly({
    year_cost<-summarydata()[,c(1,5:6)]
    year_cost_long<-melt(year_cost,id="year",measure=c("Cost Own Home","Cost Rent Home"))
    names(year_cost_long) = c("year", "category", "value")
    g2<-ggplot(data=year_cost_long,aes(x=year,y=value, fill=category))+
      geom_bar(stat ="identity", colour = 'white', position = 'dodge')+
      xlab("Year")+ylab("Yearly Cost ($)") +ggtitle("Comparison of Own and Rent Home") 
      scale_fill_manual(values = c("#ff0000", "#2372b2"), labels = c("Own Home","Rent Home")) + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
        theme(plot.title = element_text(size=22))
    g2
    ggplotly(g2)
  
  })
  
  output$rentplotmod<-renderPlotly({
    year_cost<-summarydata()[1:input$years_stay,c(1,5:6)]
    sell_cost<-input$home_price*(1+input$home_price_growth_rate*0.01)^input$years_stay*input$cost_sell_rate*0.01
    year_cost[,2]=year_cost[,2]+sell_cost/input$years_stay
    year_cost_long<-melt(year_cost,id="year",measure=c("Cost Own Home","Cost Rent Home"))
    names(year_cost_long) = c("year", "category", "value")
    g3<-ggplot(data=year_cost_long,aes(x=year,y=value, fill=category))+
      geom_bar(stat ="identity", colour = 'white', position = 'dodge')+
      xlab("Year")+ylab("Yearly Cost ($)") +ggtitle("Comparison with Home Selling Cost") 
    scale_fill_manual(values = c("#ff0000", "#2372b2"), labels = c("Own Home","Rent Home")) + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
      theme(plot.title = element_text(size=22))
    g3
    ggplotly(g3)
    
  })
  
  
 
  output$housingpriceplot<-renderPlotly({
    housetype<-input$Hometype
    print(housetype)
    US_housing<-read_excel("united-states-states-Report.xls",sheet=housetype)    
#    US_housing<-read_excel("united-states-states-Report.xls",sheet="All Homes")
    #read_excel("my-spreadsheet.xls", sheet = "data")
    nl=nrow(US_housing)
    US_housing<-US_housing[2:nl,]
    state_price<-US_housing[3:nl,c(1,4)]
    names(state_price)<-c("region","price")
    #state_price[is.na(state_price$price),]$price<-0
    state_price$price<-as.numeric(state_price$price)
    #state_price[state_price$price=='---',]$price<-0
    state_price[is.na(state_price$price),]$price<-0
    state_price$region<-tolower(state_price$region)
    states.map =map_data("state")
    maped_price<-inner_join(states.map,state_price,by="region")
    #maped_price<-data.frame(maped_price$long,lat=maped_price$lat,maped_price$region,maped_price$price)
    #names(maped_price)<-c("long","lat","region","price")
    #states.map = state_map()
    p = ggplot(data = maped_price) +
      geom_polygon(aes(x = long, y = lat, fill = price, group = region), colour = 'black') + 
      coord_fixed(1.3) +
      ggtitle('State Median Housing Price') +
      theme(plot.title = element_text(size=22)) +xlab('')+ylab('')+
      scale_fill_gradient(low = "blue", high = "red")
    p
    ggplotly(p)
  }) 
  
  
  output$CAhousingplot<-renderPlotly({
    county<-input$CA_county
     ID<-grep(county, CA_county)
    CA_housing<-read_excel("Median_Prices_of_Existing_Detached_Homes_2016_03.xls")
    nl=nrow(CA_housing)
    CA_housing<-CA_housing[7:nl,]
    names(CA_housing)=gsub(' ','_',trimws(CA_housing[1,],which="both"))
    CA_housing<-CA_housing[-1,]
    names(CA_housing)[1]<-"Mon_Yr"
    CA_housing$Mon_Yr<-as.Date(as.numeric(CA_housing$Mon_Yr),origin="1899-12-30", tz="GMT")
    for (i in 2:ncol(CA_housing)){
      CA_housing[,i]<-as.numeric(unlist(CA_housing[,i]))
    }
     pca<-ggplot(data=CA_housing,aes(Mon_Yr,CA_housing[, county]))+geom_point()+
#    pca<-ggplot(data=CA_housing,aes(Mon_Yr,CA_housing[,ID]))+geom_point()+geom_line()+
      ggtitle(paste(county,'County Median Housing Price'))+
      theme(plot.title = element_text(size=22)) +xlab('Year')+ylab('Price ($)')+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
         pca
    ggplotly(pca)
  }) 
  output$zip_housing_rent1 <- renderText({
    ID2<-grep(input$zipcode,housingzipdata$RegionName)
    ziphousingprice<-housingzipdata[ID2,247]
    paste("Home price at zip code",input$zipcode," is ",ziphousingprice,'$')
  })
  output$zip_housing_rent2 <- renderText({
    ID<-grep(input$zipcode,rentzipdata$RegionName)
    ziprent<-rentzipdata[ID,72]
     paste("Monthly rent at zip code",input$zipcode," is ",ziprent,'$')
  })

    
  output$mytable1 = renderDataTable({summarydata()})
})
