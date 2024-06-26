## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, warning = FALSE, message = FALSE----------------------------------

library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyverse)
library(shiny)

# for the tables
library(reactable)
library(reactablefmtr)

# for the charts
library(highcharter)

# the library planr
library(planr)


## -----------------------------------------------------------------------------


Period <- c(
"1/1/2020", "2/1/2020", "3/1/2020", "4/1/2020", "5/1/2020", "6/1/2020", "7/1/2020", "8/1/2020", "9/1/2020", "10/1/2020", "11/1/2020", "12/1/2020","1/1/2021", "2/1/2021", "3/1/2021", "4/1/2021", "5/1/2021", "6/1/2021", "7/1/2021", "8/1/2021", "9/1/2021", "10/1/2021", "11/1/2021", "12/1/2021")

Demand <- c(360, 458,300,264,140,233,229,208,260,336,295,226,336,434,276,240,116,209,205,183,235,312,270,201)

Opening <- c(1310,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

Supply <- c(0,0,0,0,0,2500,0,0,0,0,0,0,0,0,0,2000,0,0,0,0,0,0,0,0)


# assemble
my_demand_and_suppply <- data.frame(Period,
                  Demand,
                  Opening,
                  Supply)

# let's add a Product
my_demand_and_suppply$DFU <- "Product A"

# format the Period as a date
my_demand_and_suppply$Period <- as.Date(as.character(my_demand_and_suppply$Period), format = '%m/%d/%Y')


# let's have a look at it
head(my_demand_and_suppply)







## -----------------------------------------------------------------------------

# calculate
calculated_projection <- light_proj_inv(dataset = my_demand_and_suppply, 
                                        DFU = DFU, 
                                        Period = Period, 
                                        Demand =  Demand, 
                                        Opening = Opening, 
                                        Supply = Supply)

# see results
head(calculated_projection)



## -----------------------------------------------------------------------------

# set a working df
df1 <- calculated_projection

# keep only the needed columns
df1 <- df1 %>% select(Period,
                      Demand,
                      Calculated.Coverage.in.Periods,
                      Projected.Inventories.Qty,
                      Supply)


# create a f_colorpal field
df1 <- df1 %>% mutate(f_colorpal = case_when( Calculated.Coverage.in.Periods > 6 ~ "#FFA500",
                                              Calculated.Coverage.in.Periods > 2 ~ "#32CD32",
                                              Calculated.Coverage.in.Periods > 0 ~ "#FFFF99",
                                              TRUE ~ "#FF0000" ))



# create reactable
reactable(df1, resizable = TRUE, showPageSizeOptions = TRUE,

              striped = TRUE, highlight = TRUE, compact = TRUE,
              defaultPageSize = 20,

              columns = list(


                Demand = colDef(
                  name = "Demand (units)",

                  cell = data_bars(df1,
                                   fill_color = "#3fc1c9",
                                   text_position = "outside-end"
                  )

                ),




              Calculated.Coverage.in.Periods = colDef(
                name = "Coverage (Periods)",
                maxWidth = 90,
                cell= color_tiles(df1, color_ref = "f_colorpal")
              ),


              f_colorpal = colDef(show = FALSE), # hidden, just used for the coverages



                `Projected.Inventories.Qty`= colDef(
                  name = "Projected Inventories (units)",
                  format = colFormat(separators = TRUE, digits=0),

                  style = function(value) {
                    if (value > 0) {
                      color <- "#008000"
                    } else if (value < 0) {
                      color <- "#e00000"
                    } else {
                      color <- "#777"
                    }
                    list(color = color
                         #fontWeight = "bold"
                    )
                  }
                ),




              Supply = colDef(
                name = "Supply (units)",
                cell = data_bars(df1,
                                 fill_color = "#3CB371",
                                 text_position = "outside-end"
                                 )
                )





              ), # close columns lits

              columnGroups = list(
                colGroup(name = "Projected Inventories", columns = c("Calculated.Coverage.in.Periods",
                                                                     "Projected.Inventories.Qty"))


              )

    ) # close reactable





## -----------------------------------------------------------------------------


# set a working df
df1 <- calculated_projection

# keep only the needed columns
df1 <- df1 %>% select(Period,
                      Projected.Inventories.Qty)


# create a value.index
df1$Value.Index <- if_else(df1$Projected.Inventories.Qty < 0, "Shortage", "Stock")
    
    
# spread
df1 <- df1 %>% spread(Value.Index, Projected.Inventories.Qty)
    
    
#----------------------------------------------------
# Chart
    
    u <- highchart() %>% 
      hc_title(text = "Projected Inventories") %>%
      hc_subtitle(text = "in units") %>% 
      hc_add_theme(hc_theme_google()) %>%
      
      hc_xAxis(categories = df1$Period) %>% 
      
      hc_add_series(name = "Stock", 
                    color = "#32CD32",
                    #dataLabels = list(align = "center", enabled = TRUE),
                    data = df1$Stock) %>% 
      
      hc_add_series(name = "Shortage", 
                    color = "#dc3220",
                    #dataLabels = list(align = "center", enabled = TRUE),
                    data = df1$Shortage) %>% 
      
      hc_chart(type = "column") %>% 
      hc_plotOptions(series = list(stacking = "normal"))
    
    u 




## -----------------------------------------------------------------------------

my_data_with_parameters <- my_demand_and_suppply

my_data_with_parameters$Min.Cov <- 2
my_data_with_parameters$Max.Cov <- 4

head(my_data_with_parameters)


## -----------------------------------------------------------------------------

df1 <- proj_inv(data = my_data_with_parameters, 
                DFU = DFU, 
                Period = Period, 
                Demand =  Demand, 
                Opening = Opening, 
                Supply = Supply,
                Min.Cov = Min.Cov, 
                Max.Cov = Max.Cov)

# see results
calculated_projection_and_analysis <- df1

head(calculated_projection_and_analysis)


## -----------------------------------------------------------------------------

# create a function status.PI.Index
status_PI.Index <- function(color = "#aaa", width = "0.55rem", height = width) {
  span(style = list(
    display = "inline-block",
    marginRight = "0.5rem",
    width = width,
    height = height,
    backgroundColor = color,
    borderRadius = "50%"
  ))
}


## -----------------------------------------------------------------------------


# set a working df
df1 <- calculated_projection_and_analysis


# remove not needed column
df1 <- df1[ , -which(names(df1) %in% c("DFU"))]
    
    
# create a f_colorpal field
df1 <- df1 %>% mutate(f_colorpal = case_when( Calculated.Coverage.in.Periods > 6 ~ "#FFA500", 
                                              Calculated.Coverage.in.Periods > 2 ~ "#32CD32",
                                              Calculated.Coverage.in.Periods > 0 ~ "#FFFF99",
                                              TRUE ~ "#FF0000" ))
    
    
    
#-------------------------
# Create Table
    
    
    
reactable(df1, resizable = TRUE, showPageSizeOptions = TRUE, 
              
              striped = TRUE, highlight = TRUE, compact = TRUE, 
              defaultPageSize = 20,
              
              columns = list(

                
                Demand = colDef(
                  name = "Demand (units)",
                  
                  cell = data_bars(df1, 
                                   #round_edges = TRUE
                                   #value <- format(value, big.mark = ","),
                                   #number_fmt = big.mark = ",",
                                   fill_color = "#3fc1c9",
                                   #fill_opacity = 0.8, 
                                   text_position = "outside-end"
                  )
                  
                ),
                

                
                Calculated.Coverage.in.Periods = colDef(
                  name = "Coverage (Periods)",
                  maxWidth = 90,
                  
                  cell= color_tiles(df1, color_ref = "f_colorpal")
                ),
                
                
                f_colorpal = colDef(show = FALSE), # hidden, just used for the coverages
                

                
                `Projected.Inventories.Qty`= colDef(
                  name = "Projected Inventories (units)",
                  format = colFormat(separators = TRUE, digits=0),
                  
                  style = function(value) {
                    if (value > 0) {
                      color <- "#008000"
                    } else if (value < 0) {
                      color <- "#e00000"
                    } else {
                      color <- "#777"
                    }
                    list(color = color
                         #fontWeight = "bold"
                    )
                  }
                ),
                

                
                Supply = colDef(
                  name = "Supply (units)",
                  cell = data_bars(df1, 
                                   
                                   #round_edges = TRUE
                                   #value <- format(value, big.mark = ","),
                                   #number_fmt = big.mark = ",",
                                   fill_color = "#3CB371",
                                   #fill_opacity = 0.8, 
                                   text_position = "outside-end"
                  )
                  #format = colFormat(separators = TRUE, digits=0)
                  #number_fmt = big.mark = ","
                ),
                
                
                
                PI.Index = colDef(
                  name = "Analysis",
                  
                  cell = function(value) {
                    color <- switch(
                      value,
                      TBC = "hsl(154, 3%, 50%)",
                      OverStock = "hsl(214, 45%, 50%)",
                      OK = "hsl(154, 64%, 50%)",
                      Alert = "hsl(30, 97%, 70%)",
                      Shortage = "hsl(3, 69%, 50%)"
                    )
                    PI.Index <- status_PI.Index(color = color)
                    tagList(PI.Index, value)
                  }),
                
                
                
                `Safety.Stocks`= colDef(
                  name = "Safety Stocks (units)",
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                `Maximum.Stocks`= colDef(
                  name = "Maximum Stocks (units)",
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                `Min.Cov`= colDef(name = "Min Stocks Coverage (Periods)"),
                
                `Max.Cov`= colDef(name = "Maximum Stocks Coverage (Periods)"),
                
                
                # ratios
                `Ratio.PI.vs.min`= colDef(name = "Ratio PI vs min"),
                
                `Ratio.PI.vs.Max`= colDef(name = "Ratio PI vs Max")
                
                
                
                
              ), # close columns lits
              
              columnGroups = list(
                colGroup(name = "Projected Inventories", columns = c("Calculated.Coverage.in.Periods", 
                                                                     "Projected.Inventories.Qty")),
                
                colGroup(name = "Stocks Levels Parameters", columns = c("Min.Cov", 
                                                                        "Max.Cov",
                                                                        "Safety.Stocks",
                                                                        "Maximum.Stocks")),
                
                colGroup(name = "Analysis Features", columns = c("PI.Index", 
                                                                        "Ratio.PI.vs.min",
                                                                        "Ratio.PI.vs.Max"))
                
              )
              
    ) # close reactable










## -----------------------------------------------------------------------------

# set a working df
df1 <- calculated_projection_and_analysis



# Chart
p <- highchart() %>% 
      hc_add_series(name = "Max", color = "crimson", data = df1$Maximum.Stocks) %>% 
      hc_add_series(name = "min", color = "lightblue", data = df1$Safety.Stocks) %>% 
      hc_add_series(name = "Projected Inventories", color = "gold", data = df1$Projected.Inventories.Qty) %>% 
      
      hc_title(text = "Projected Inventories") %>%
      hc_subtitle(text = "in units") %>% 
      hc_xAxis(categories = df1$Period) %>% 
      #hc_yAxis(title = list(text = "Sales (units)")) %>% 
      hc_add_theme(hc_theme_google())
    
    p




## -----------------------------------------------------------------------------

df1 <- my_demand_and_suppply

df1$SSCov <- 2
df1$DRPCovDur <- 3
df1$MOQ <- 1
df1$FH <- c("Frozen", "Frozen", "Frozen", "Frozen","Frozen","Frozen","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free","Free")


# get Results
my_drp_template <- df1

head(my_drp_template)


## -----------------------------------------------------------------------------

# set a working df
df1 <- my_drp_template

# calculate drp
demo_drp <- drp(data = df1,
           DFU = DFU,
           Period = Period,
           Demand =  Demand,
           Opening = Opening,
           Supply = Supply,
           SSCov = SSCov,
           DRPCovDur = DRPCovDur,
           MOQ = MOQ,
           FH = FH
)


glimpse(demo_drp)


## -----------------------------------------------------------------------------

# set a working df
df1 <- demo_drp

# keep only the needed columns
df1 <- df1 %>% select(Period,
                      Demand,
                      DRP.Calculated.Coverage.in.Periods,
                      DRP.Projected.Inventories.Qty,
                      DRP.plan)


# replace missing values by zero
df1$DRP.plan[is.na(df1$DRP.plan)] <- 0
df1$DRP.Projected.Inventories.Qty[is.na(df1$DRP.Projected.Inventories.Qty)] <- 0

# create a f_colorpal field
df1 <- df1 %>% mutate(f_colorpal = case_when( DRP.Calculated.Coverage.in.Periods > 8 ~ "#FFA500",
                                              DRP.Calculated.Coverage.in.Periods > 2 ~ "#32CD32",
                                              DRP.Calculated.Coverage.in.Periods > 0 ~ "#FFFF99",
                                              TRUE ~ "#FF0000" ))



# create reactable
reactable(df1, resizable = TRUE, showPageSizeOptions = TRUE,

              striped = TRUE, highlight = TRUE, compact = TRUE,
              defaultPageSize = 20,

              columns = list(


                Demand = colDef(
                  name = "Demand (units)",

                  cell = data_bars(df1,
                                   fill_color = "#3fc1c9",
                                   text_position = "outside-end"
                  )

                ),




              DRP.Calculated.Coverage.in.Periods = colDef(
                name = "Coverage (Periods)",
                maxWidth = 90,
                cell= color_tiles(df1, color_ref = "f_colorpal")
              ),


              f_colorpal = colDef(show = FALSE), # hidden, just used for the coverages



                `DRP.Projected.Inventories.Qty`= colDef(
                  name = "Projected Inventories (units)",
                  format = colFormat(separators = TRUE, digits=0),

                  style = function(value) {
                    if (value > 0) {
                      color <- "#008000"
                    } else if (value < 0) {
                      color <- "#e00000"
                    } else {
                      color <- "#777"
                    }
                    list(color = color
                         #fontWeight = "bold"
                    )
                  }
                ),




              DRP.plan = colDef(
                name = "Replenishment (units)",
                cell = data_bars(df1,
                                 fill_color = "#3CB371",
                                 text_position = "outside-end"
                                 )
                )





              ), # close columns lits

              columnGroups = list(
                colGroup(name = "Projected Inventories", columns = c("DRP.Calculated.Coverage.in.Periods",
                                                                     "DRP.Projected.Inventories.Qty"))


              )

    ) # close reactable





## -----------------------------------------------------------------------------

# set a working df
df1 <- demo_drp



# Chart
p <- highchart() %>% 
      hc_add_series(name = "Max", color = "crimson", data = df1$Maximum.Stocks) %>% 
      hc_add_series(name = "min", color = "lightblue", data = df1$Safety.Stocks) %>% 
      hc_add_series(name = "Projected Inventories", color = "gold", data = df1$DRP.Projected.Inventories.Qty) %>% 
      
      hc_title(text = "(DRP) Projected Inventories") %>%
      hc_subtitle(text = "in units") %>% 
      hc_xAxis(categories = df1$Period) %>% 
      #hc_yAxis(title = list(text = "Sales (units)")) %>% 
      hc_add_theme(hc_theme_google())
    
    p




