## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------

library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyverse)
library(shiny)

# for the tables
library(reactable)
library(reactablefmtr)
library(sparkline)
library(DT)

# for the charts
library(highcharter)

# the library planr
library(planr)


## -----------------------------------------------------------------------------

df1 <- blueprint_light

glimpse(df1)


## -----------------------------------------------------------------------------



#-----------------
# Get Summary of variables
#-----------------

# set a working df
df1 <- blueprint_light

# aggregate
df1 <- df1 %>% select(DFU, 
                         Demand,
                         Opening,
                         Supply) %>%
      group_by(DFU) %>%
      summarise(Demand = sum(Demand),
                Opening = sum(Opening),
                Supply = sum(Supply)
      )
    
# let's calculate the share of Demand
df1$Demand.pc <- df1$Demand / sum(df1$Demand)
    
    
# keep Results
Value_DB <- df1
    

 
    
#-----------------
# Get Sparklines Demand
#-----------------
    
# set a working df
df1 <- blueprint_light
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Demand)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Demand.Quantity = list(Quantity))
    
# keep Results
Demand_Sparklines_DB <- df1





    
#-----------------
# Get Sparklines Supply
#-----------------
    
# set a working df
df1 <- blueprint_light
    
# replace missing values by zero
df1$Supply[is.na(df1$Supply)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Supply)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Supply.Quantity = list(Quantity))
    
# keep Results
Supply_Sparklines_DB <- df1




#-----------------
# Merge dataframes
#-----------------

# merge
df1 <- left_join(Value_DB, Demand_Sparklines_DB)
df1 <- left_join(df1, Supply_Sparklines_DB)


# reorder columns
df1 <- df1 %>% select(DFU, Demand, Demand.pc, Demand.Quantity, Opening,
                      Supply, Supply.Quantity)


# get results
Summary_DB <- df1

glimpse(Summary_DB)


## -----------------------------------------------------------------------------

reactable(df1,compact = TRUE,
              
              defaultSortOrder = "desc",
              defaultSorted = c("Demand"),
              defaultPageSize = 20,
              
              columns = list(
                
                `DFU` = colDef(name = "DFU"),

                
                `Demand`= colDef(
                  name = "Total Demand (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0),
                  style = list(background = "yellow",fontWeight = "bold")
                ),
                
                
                `Demand.pc`= colDef(
                  name = "Share of Demand (%)",
                  format = colFormat(percent = TRUE, digits = 1)
                ), # close %
                
                
                `Supply`= colDef(
                  name = "Total Supply (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                Demand.Quantity = colDef(
                  name = "Projected Demand",
                  cell = function(value, index) {
                    sparkline(df1$Demand.Quantity[[index]])
                  }),
                

                
                
                Supply.Quantity = colDef(
                  name = "Projected Supply",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  })
                


 
                
                
              ), # close columns list
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              
              columnGroups = list(
                
                colGroup(name = "Demand",
                         columns = c("Demand",
                                     "Demand.pc",
                                     "Demand.Quantity")),
                
                colGroup(name = "Supply",
                         columns = c("Supply", "Supply.Quantity"))
                
                
              )
          
) # close reactable



## -----------------------------------------------------------------------------


# set a working df
df1 <- blueprint_light


df1 <- as.data.frame(df1)

glimpse(df1)

# calculate
calculated_projection <- light_proj_inv(dataset = df1, 
                                        DFU = DFU, 
                                        Period = Period, 
                                        Demand =  Demand, 
                                        Opening = Opening, 
                                        Supply = Supply)

# see results
head(calculated_projection)



## -----------------------------------------------------------------------------

calculated_projection <-as.data.frame(calculated_projection)

# filter data
Selected_DB <- filter(calculated_projection, calculated_projection$DFU == "Item 000001")


glimpse(Selected_DB)



## -----------------------------------------------------------------------------

# keep only the needed columns
df1 <- Selected_DB %>% select(Period,
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

#------------------------------
# Get data
df1 <- calculated_projection
df1 <- as.data.frame(df1)


#------------------------------
# Filter

# subset Period based on those Starting and Ending Periods
df1 <- subset(df1,df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")


#--------
# Keep Initial data
#--------
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
Initial_DB <- df1
   


#------------------------------
# Transform
    
    
    
#--------
# Create a Summary database
#--------
    
# set a working df
df1 <- Initial_DB
    
# aggregate
df1 <- df1 %>% select(
      DFU,
      Demand) %>%
      group_by(DFU
               ) %>%
      summarise(Demand.Qty = sum(Demand)
      )
    

# Get Results
Value_DB <- df1



#--------
# Create the SRA
#--------

# set a working df
df1 <- Initial_DB

#------------------------------
# keep only the needed columns
df1 <- df1[,c("DFU","Period","Calculated.Coverage.in.Periods")]

# format as numeric
df1$Calculated.Coverage.in.Periods <- as.numeric(df1$Calculated.Coverage.in.Periods)

# formatting 1 digit after comma
df1$Calculated.Coverage.in.Periods = round(df1$Calculated.Coverage.in.Periods, 1)

# spread data
df1 <- df1 %>% spread(Period, Calculated.Coverage.in.Periods)

# replace missing values by zero
df1[is.na(df1)] <- 0

# Get Results
SRA_DB <- df1
 



#--------
# Merge both database
#--------

# merge both databases
df1 <- left_join(Value_DB, SRA_DB)

# Sort by Demand.Qty descending
df1 <- df1[order(-df1$Demand.Qty),]


# rename column
df1 <- df1 %>% rename(
      "Total Demand (units)" = Demand.Qty
    )


# Get Results
Interim_DB <- df1





## -----------------------------------------------------------------------------

#------------------------------
# create DT
  
df1 <- Interim_DB

datatable(df1,
              #filter = list(position = 'top', clear = FALSE),

              options = list(
                searching = FALSE,
                pageLength = 20,
                columnDefs = list(list(width = '200px', targets = c(1,2)))
              ),rownames= FALSE) %>%

      formatRound(2:2, 1) %>%

      formatStyle(columns = c(1:100), fontSize = '85%') %>%

      formatStyle(
        3:20,
        backgroundColor = styleInterval(c(-0.1,0.0,1.0), c('#FF6347', 'orange', 'yellow','lightblue'))
      ) %>%



    formatStyle(
      2:2,
      backgroundColor = 'mediumseagreen'
    ) 



## -----------------------------------------------------------------------------

#--------
# Create a Delay.Analysis check
#--------

# set a working df
df1 <- Initial_DB

# aggregate
df1 <- df1 %>% select(DFU, Period,Projected.Inventories.Qty) %>%
      group_by(DFU) %>%
      summarise(min.Projected.Inventories.Qty = min(Projected.Inventories.Qty),
                max.Projected.Inventories.Qty = max(Projected.Inventories.Qty)
      )



#-----------------
# Identify where we are late to supply
#-----------------

# Add a character info to analyze whether there is an identified delay or not
df1$Delay.Analysis <- if_else(df1$min.Projected.Inventories.Qty <= 0, "Delay", "OK")

# Get Results
Check_DB <- df1

head(Check_DB)


## -----------------------------------------------------------------------------


#--------
# Merge
#--------

# merge
df1 <- left_join(Check_DB, Interim_DB)
df1 <- as.data.frame(df1)

# Note : we could use a filter to keep only those rows, in a shiny app for example
# filter on Delay.Analysis
# df1 <- filter(df1,df1$Delay.Analysis %in% input$Selected.Delay.Analysis)


# remove not needed columns
df1 <- df1[ , -which(names(df1) %in% c("min.Projected.Inventories.Qty",
                                           "max.Projected.Inventories.Qty"
                                           #"Delay.Analysis"
                                       ))]



    
    
#------------------------------
# create DT

datatable(df1,
              #filter = list(position = 'top', clear = FALSE),

              options = list(
                searching = FALSE,
                pageLength = 20,
                columnDefs = list(list(width = '200px', targets = c(1,2)))
              ),rownames= FALSE) %>%

      formatRound(3:3, 1) %>%

      formatStyle(columns = c(1:100), fontSize = '85%') %>%

      formatStyle(
        4:20,
        backgroundColor = styleInterval(c(-0.1,0.0,1.0), c('#FF6347', 'orange', 'yellow','lightblue'))
      ) %>%



    formatStyle(
      3:3,
      backgroundColor = 'mediumseagreen'
    ) 




## -----------------------------------------------------------------------------



#------------------------------
# Get data
df1 <- calculated_projection
df1 <- as.data.frame(df1)


#------------------------------
# Filter

# subset Period based on those Starting and Ending Periods
df1 <- subset(df1,df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")


# keep this initial dataset
Initial_DB <- df1





#-----------------
# Get Summary of variables
#-----------------

# set a working df
df1 <- Initial_DB

# aggregate
df1 <- df1 %>% select(DFU, 
                         Demand,
                         Opening,
                         Supply) %>%
      group_by(DFU) %>%
      summarise(Demand = sum(Demand),
                Opening = sum(Opening),
                Supply = sum(Supply)
      )
    
# let's calculate the share of Demand
df1$Demand.pc <- df1$Demand / sum(df1$Demand)
    
    
# keep Results
Value_DB <- df1
    

 
    
#-----------------
# Get Sparklines Demand
#-----------------
    
# set a working df
df1 <- Initial_DB
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Demand)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Demand.Quantity = list(Quantity))
    
# keep Results
Demand_Sparklines_DB <- df1





    
#-----------------
# Get Sparklines Supply
#-----------------
    
# set a working df
df1 <- Initial_DB
    
# replace missing values by zero
df1$Supply[is.na(df1$Supply)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Supply)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Supply.Quantity = list(Quantity))
    
# keep Results
Supply_Sparklines_DB <- df1






#-----------------
# Get Sparklines Projected Inventories
#-----------------
    
# set a working df
df1 <- Initial_DB
    
# replace missing values by zero
df1$Projected.Inventories.Qty[is.na(df1$Projected.Inventories.Qty)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Projected.Inventories.Qty)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(PI.Quantity = list(Quantity))
    
# keep Results
PI_Sparklines_DB <- df1





#--------
# Create a Delay.Analysis check
#--------

# set a working df
df1 <- Initial_DB

# aggregate
df1 <- df1 %>% select(DFU, Period,Projected.Inventories.Qty) %>%
      group_by(DFU) %>%
      summarise(min.Projected.Inventories.Qty = min(Projected.Inventories.Qty),
                max.Projected.Inventories.Qty = max(Projected.Inventories.Qty)
      )



#-----------------
# Identify where we are late to supply
#-----------------

# Add a character info to analyze whether there is an identified delay or not
df1$Delay.Analysis <- if_else(df1$min.Projected.Inventories.Qty <= 0, "Delay", "OK")

# Get Results
Check_DB <- df1














#-----------------
# Merge dataframes
#-----------------

# merge
df1 <- left_join(Value_DB, Demand_Sparklines_DB)
df1 <- left_join(df1, Supply_Sparklines_DB)
df1 <- left_join(df1, PI_Sparklines_DB)
df1 <- left_join(df1, Check_DB)


# reorder columns
df1 <- df1 %>% select(DFU, Demand, Demand.pc, Demand.Quantity,
                      Supply, Supply.Quantity,
                      Opening,
                      PI.Quantity,
                      Delay.Analysis)


# get results
Summary_DB <- df1

glimpse(Summary_DB)











## -----------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
#    A Function to define a Badge Status in the reactable
#--------------------------------------------------------------------------------------

status_badge <- function(color = "#aaa", width = "9px", height = width) {
  span(style = list(
    display = "inline-block",
    marginRight = "8px",
    width = width,
    height = height,
    backgroundColor = color,
    borderRadius = "50%"
  ))
}


## -----------------------------------------------------------------------------


reactable(df1,compact = TRUE,
              
              defaultSortOrder = "desc",
              defaultSorted = c("Demand"),
              defaultPageSize = 20,
              
              columns = list(
                
                `DFU` = colDef(name = "DFU"),

                
                `Demand`= colDef(
                  name = "Total Demand (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0),
                  style = list(background = "yellow",fontWeight = "bold")
                ),
                
                
                `Demand.pc`= colDef(
                  name = "Share of Demand (%)",
                  format = colFormat(percent = TRUE, digits = 1)
                ), # close %
                
                
                `Supply`= colDef(
                  name = "Total Supply (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                Demand.Quantity = colDef(
                  name = "Projected Demand",
                  cell = function(value, index) {
                    sparkline(df1$Demand.Quantity[[index]])
                  }),
                

                
                
                Supply.Quantity = colDef(
                  name = "Projected Supply",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                PI.Quantity = colDef(
                  name = "Projected Inventories",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                
                Delay.Analysis = colDef(
                  name = "Delay Analysis",
                  
                  cell = function(value) {
                    color <- switch(
                      value,
                      OK = "hsl(120,61%,50%)",
                      Delay = "hsl(39,100%,50%)"
                    )
                    badge <- status_badge(color = color)
                    tagList(badge, value)
                  })
                


 
                
                
              ), # close columns list
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              
              columnGroups = list(
                
                colGroup(name = "Demand",
                         columns = c("Demand",
                                     "Demand.pc",
                                     "Demand.Quantity")),
                
                colGroup(name = "Supply",
                         columns = c("Supply", "Supply.Quantity")),
                
                
                colGroup(name = "Inventories",
                         columns = c("Opening", "PI.Quantity", "Delay.Analysis"))
                
                
              )
          
) # close reactable




## -----------------------------------------------------------------------------

df1 <- blueprint

glimpse(df1)


## -----------------------------------------------------------------------------



#-----------------
# Get Summary of variables
#-----------------

# set a working df
df1 <- blueprint

# aggregate
df1 <- df1 %>% select(DFU,
                      Demand,
                      Opening,
                      Supply,
                      Min.Cov,
                      Max.Cov) %>%
      group_by(DFU) %>%
      summarise(Demand = sum(Demand),
                Opening = sum(Opening),
                Supply = sum(Supply),
                Min.Cov = mean(Min.Cov),
                Max.Cov = mean(Max.Cov)
      )
    
# let's calculate the share of Demand
df1$Demand.pc <- df1$Demand / sum(df1$Demand)
    
    
# keep Results
Value_DB <- df1
    

 
    
#-----------------
# Get Sparklines Demand
#-----------------
    
# set a working df
df1 <- blueprint_light
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Demand)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Demand.Quantity = list(Quantity))
    
# keep Results
Demand_Sparklines_DB <- df1





    
#-----------------
# Get Sparklines Supply
#-----------------
    
# set a working df
df1 <- blueprint_light
    
# replace missing values by zero
df1$Supply[is.na(df1$Supply)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Supply)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Supply.Quantity = list(Quantity))
    
# keep Results
Supply_Sparklines_DB <- df1




#-----------------
# Merge dataframes
#-----------------

# merge
df1 <- left_join(Value_DB, Demand_Sparklines_DB)
df1 <- left_join(df1, Supply_Sparklines_DB)


# reorder columns
df1 <- df1 %>% select(DFU,
                      Min.Cov, Max.Cov, 
                      Demand, Demand.pc, Demand.Quantity, Opening,
                      Supply, Supply.Quantity)


# get results
Summary_DB <- df1

glimpse(Summary_DB)


## -----------------------------------------------------------------------------


#--------------------------------------------------------------------------------------
#    A Function for a bar chart in the background of the cell
#--------------------------------------------------------------------------------------

# Render a bar chart in the background of the cell
bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%", align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color
  )
}




## -----------------------------------------------------------------------------

reactable(df1,compact = TRUE,
              
              defaultSortOrder = "desc",
              defaultSorted = c("Demand"),
              defaultPageSize = 20,
              
              columns = list(
                
                `DFU` = colDef(name = "DFU"),

                
                `Demand`= colDef(
                  name = "Total Demand (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0),
                  style = list(background = "yellow",fontWeight = "bold")
                ),
                
                
                `Demand.pc`= colDef(
                  name = "Share of Demand (%)",
                  format = colFormat(percent = TRUE, digits = 1)
                ), # close %
                
                
                `Supply`= colDef(
                  name = "Total Supply (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                Demand.Quantity = colDef(
                  name = "Projected Demand",
                  cell = function(value, index) {
                    sparkline(df1$Demand.Quantity[[index]])
                  }),
                

                
                
                Supply.Quantity = colDef(
                  name = "Projected Supply",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                
                `Min.Cov`= colDef(
                  name = "Min Coverage (Periods)",
                  style = function(value) {
                    bar_style(width = value / max(df1$Min.Cov), fill = "hsl(208, 70%, 90%)")
                  }
                ),
                
                
                `Max.Cov`= colDef(
                  name = "Max Coverage (Periods)",
                  style = function(value) {
                    bar_style(width = value / max(df1$Max.Cov), fill = "hsl(0,79%,72%)")
                  }
                )
                


 
                
                
              ), # close columns list
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              
              columnGroups = list(
                
                colGroup(name = "Demand",
                         columns = c("Demand",
                                     "Demand.pc",
                                     "Demand.Quantity")),
                
                colGroup(name = "Supply",
                         columns = c("Supply", "Supply.Quantity"))
                
                
              )
          
) # close reactable



## -----------------------------------------------------------------------------


# set a working df
df1 <- blueprint
df1 <- as.data.frame(df1)


# calculate
calculated_projection_and_analysis <- proj_inv(data = df1, 
                DFU = DFU, 
                Period = Period, 
                Demand =  Demand, 
                Opening = Opening, 
                Supply = Supply,
                Min.Cov = Min.Cov, 
                Max.Cov = Max.Cov)


head(calculated_projection_and_analysis)



## -----------------------------------------------------------------------------

calculated_projection_and_analysis <-as.data.frame(calculated_projection_and_analysis)

# filter data
Selected_DB <- filter(calculated_projection_and_analysis, calculated_projection_and_analysis$DFU == "Item 000001")


glimpse(Selected_DB)



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
df1 <- Selected_DB


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


# set a working dataframe
df1 <-as.data.frame(calculated_projection_and_analysis)


#------------------------------
# Filter

# subset Period based on those Starting and Ending Periods
df1 <- subset(df1,df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")


df1$PI.Index <- if_else(df1$PI.Index == "OverStock", "OverStock", "")


glimpse(df1)




## -----------------------------------------------------------------------------


#--------
# Keep Initial data
#--------
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
Initial_DB <- df1
   


#------------------------------
# Transform
    
    
    
#--------
# Create a Summary database
#--------
    
# set a working df
df1 <- Initial_DB
    
# aggregate
df1 <- df1 %>% select(
      DFU,
      Demand) %>%
      group_by(DFU
               ) %>%
      summarise(Demand.Qty = sum(Demand)
      )
    

# Get Results
Value_DB <- df1



#--------
# Create the SRA
#--------

# set a working df
df1 <- Initial_DB

#------------------------------
# keep only the needed columns
df1 <- df1[,c("DFU","Period","PI.Index")]

# spread data
df1 <- df1 %>% spread(Period, PI.Index)

# replace missing values by zero
df1[is.na(df1)] <- 0

# Get Results
SRA_DB <- df1
 



#--------
# Merge both database
#--------

# merge both databases
df1 <- left_join(Value_DB, SRA_DB)

# Sort by Demand.Qty descending
df1 <- df1[order(-df1$Demand.Qty),]


# rename column
df1 <- df1 %>% rename(
      "Total Demand (units)" = Demand.Qty
    )


# Get Results
Interim_DB <- df1



## -----------------------------------------------------------------------------

# set a working df
df1 <- Interim_DB

# create DT
datatable(df1,
              #filter = list(position = 'top', clear = FALSE),

              options = list(
                searching = FALSE,
                pageLength = 20,
                columnDefs = list(list(width = '200px', targets = c(1,2)))
              ),rownames= FALSE) %>%

      formatRound(2:2, 1) %>%

      formatStyle(columns = c(1:100), fontSize = '85%') %>%
  
  formatStyle(
    3:20,
    backgroundColor = styleEqual(
      c('OverStock'), c('orange')
    )) %>%




    formatStyle(
      2:2,
      backgroundColor = 'mediumseagreen'
    ) 



## -----------------------------------------------------------------------------



#------------------------------
# Get data
df1 <- calculated_projection_and_analysis
df1 <- as.data.frame(df1)


#------------------------------
# Filter

# subset Period based on those Starting and Ending Periods
df1 <- subset(df1,df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")


# keep this initial dataset
Initial_DB <- df1





#-----------------
# Get Summary of variables
#-----------------

# set a working df
df1 <- Initial_DB

# aggregate
df1 <- df1 %>% select(DFU, 
                         Demand,
                         Opening,
                         Supply) %>%
      group_by(DFU) %>%
      summarise(Demand = sum(Demand),
                Opening = sum(Opening),
                Supply = sum(Supply)
      )
    
# let's calculate the share of Demand
df1$Demand.pc <- df1$Demand / sum(df1$Demand)
    
    
# keep Results
Value_DB <- df1
    

 
    
#-----------------
# Get Sparklines Demand
#-----------------
    
# set a working df
df1 <- Initial_DB
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Demand)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Demand.Quantity = list(Quantity))
    
# keep Results
Demand_Sparklines_DB <- df1





    
#-----------------
# Get Sparklines Supply
#-----------------
    
# set a working df
df1 <- Initial_DB
    
# replace missing values by zero
df1$Supply[is.na(df1$Supply)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Supply)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Supply.Quantity = list(Quantity))
    
# keep Results
Supply_Sparklines_DB <- df1






#-----------------
# Get Sparklines Projected Inventories
#-----------------
    
# set a working df
df1 <- Initial_DB
    
# replace missing values by zero
df1$Projected.Inventories.Qty[is.na(df1$Projected.Inventories.Qty)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Projected.Inventories.Qty)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(PI.Quantity = list(Quantity))
    
# keep Results
PI_Sparklines_DB <- df1










#--------
# Check if OverStock
#--------

# set a working df
df1 <- Initial_DB

# focus on OverStocks, by filtering data
df1$PI.Index.Value <- if_else(df1$PI.Index == "OverStock", 1, 0)

# aggregate
df1 <- df1 %>% select(DFU, PI.Index.Value) %>%
      group_by(DFU) %>%
      summarise(OverStock = max(PI.Index.Value)
      )

# Get Results
OverStock_DB <- df1



#--------
# Check if Alert
#--------

# set a working df
df1 <- Initial_DB

# focus on Alert, by filtering data
df1$PI.Index.Value <- if_else(df1$PI.Index == "Alert", 1, 0)

# aggregate
df1 <- df1 %>% select(DFU, PI.Index.Value) %>%
      group_by(DFU) %>%
      summarise(Alert = max(PI.Index.Value)
      )

# Get Results
Alert_DB <- df1




#--------
# Check if Shortage
#--------

# set a working df
df1 <- Initial_DB

# focus on Shortage, by filtering data
df1$PI.Index.Value <- if_else(df1$PI.Index == "Shortage", 1, 0)

# aggregate
df1 <- df1 %>% select(DFU, PI.Index.Value) %>%
      group_by(DFU) %>%
      summarise(Shortage = max(PI.Index.Value)
      )

# Get Results
Shortage_DB <- df1









#-----------------
# Merge dataframes
#-----------------

# merge
df1 <- left_join(Value_DB, Demand_Sparklines_DB)
df1 <- left_join(df1, Supply_Sparklines_DB)
df1 <- left_join(df1, PI_Sparklines_DB)
df1 <- left_join(df1, OverStock_DB)
df1 <- left_join(df1, Alert_DB)
df1 <- left_join(df1, Shortage_DB)


# reorder columns
df1 <- df1 %>% select(DFU, Demand, Demand.pc, Demand.Quantity,
                      Supply, Supply.Quantity,
                      Opening,
                      PI.Quantity,
                      OverStock,
                      Alert,
                      Shortage)




# replace figures by values
df1$OverStock <- if_else(df1$OverStock == 1, "Y", "")
df1$Alert <- if_else(df1$Alert == 1, "Y", "")
df1$Shortage <- if_else(df1$Shortage == 1, "Y", "")



# get results
Summary_DB <- df1

glimpse(Summary_DB)











## -----------------------------------------------------------------------------


reactable(df1,compact = TRUE,
              
              defaultSortOrder = "desc",
              defaultSorted = c("Demand"),
              defaultPageSize = 20,
              
              columns = list(
                
                `DFU` = colDef(name = "DFU"),

                
                `Demand`= colDef(
                  name = "Total Demand (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0),
                  style = list(background = "yellow",fontWeight = "bold")
                ),
                
                
                `Demand.pc`= colDef(
                  name = "Share of Demand (%)",
                  format = colFormat(percent = TRUE, digits = 1)
                ), # close %
                
                
                `Supply`= colDef(
                  name = "Total Supply (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                Demand.Quantity = colDef(
                  name = "Projected Demand",
                  cell = function(value, index) {
                    sparkline(df1$Demand.Quantity[[index]])
                  }),
                

                
                
                Supply.Quantity = colDef(
                  name = "Projected Supply",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                PI.Quantity = colDef(
                  name = "Projected Inventories",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                
                OverStock = colDef(
                  name = "OverStock",
                  
                  cell = function(value) {
                    color <- switch(
                      value,
                      N = "hsl(120,61%,50%)",
                      Y = "rgb(135,206,250)"
                    )
                    badge <- status_badge(color = color)
                    tagList(badge, value)
                  }),
                
                
                Alert = colDef(
                  name = "Alert",
                  
                  cell = function(value) {
                    color <- switch(
                      value,
                      N = "hsl(120,61%,50%)",
                      Y = "hsl(39,100%,50%)"
                    )
                    badge <- status_badge(color = color)
                    tagList(badge, value)
                  }),
                
                
                Shortage = colDef(
                  name = "Shortage",
                  
                  cell = function(value) {
                    color <- switch(
                      value,
                      N = "hsl(120,61%,50%)",
                      Y = "hsl(16,100%,50%)"
                    )
                    badge <- status_badge(color = color)
                    tagList(badge, value)
                  })
                


 
                
                
              ), # close columns list
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              
              columnGroups = list(
                
                colGroup(name = "Demand",
                         columns = c("Demand",
                                     "Demand.pc",
                                     "Demand.Quantity")),
                
                colGroup(name = "Supply",
                         columns = c("Supply", "Supply.Quantity")),
                
                
                colGroup(name = "Inventories",
                         columns = c("Opening", "PI.Quantity")),
                
                
                colGroup(name = "Analysis",
                         columns = c("OverStock", "Alert", "Shortage"))
                
                
              )
          
) # close reactable




## -----------------------------------------------------------------------------

df1 <- blueprint_drp

glimpse(df1)


## -----------------------------------------------------------------------------



#-----------------
# Get Summary of variables
#-----------------

# set a working df
df1 <- blueprint_drp

# aggregate
df1 <- df1 %>% select(DFU,
                      Demand,
                      Opening,
                      Supply,
                      SSCov,
                      DRPCovDur,
                      MOQ) %>%
      group_by(DFU) %>%
      summarise(Demand = sum(Demand),
                Opening = sum(Opening),
                Supply = sum(Supply),
                SSCov = mean(SSCov),
                DRPCovDur = mean(DRPCovDur),
                MOQ = mean(MOQ)
      )
    
# let's calculate the share of Demand
df1$Demand.pc <- df1$Demand / sum(df1$Demand)
    
    
# keep Results
Value_DB <- df1
    

 
    
#-----------------
# Get Sparklines Demand
#-----------------
    
# set a working df
df1 <- blueprint_drp
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Demand)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Demand.Quantity = list(Quantity))
    
# keep Results
Demand_Sparklines_DB <- df1





    
#-----------------
# Get Sparklines Supply
#-----------------
    
# set a working df
df1 <- blueprint_drp
    
# replace missing values by zero
df1$Supply[is.na(df1$Supply)] <- 0
    
# aggregate
df1 <- df1 %>%
      group_by(
        DFU,
        Period
      ) %>%
      summarise(
        Quantity = sum(Supply)
      )
    
    # generate Sparkline
    df1 <- df1 %>%
      group_by(DFU) %>%
      summarise(Supply.Quantity = list(Quantity))
    
# keep Results
Supply_Sparklines_DB <- df1




#-----------------
# Merge dataframes
#-----------------

# merge
df1 <- left_join(Value_DB, Demand_Sparklines_DB)
df1 <- left_join(df1, Supply_Sparklines_DB)


# reorder columns
df1 <- df1 %>% select(DFU,
                      SSCov,
                      DRPCovDur,
                      MOQ, 
                      Demand, Demand.pc, Demand.Quantity, Opening,
                      Supply, Supply.Quantity)


# get results
Summary_DB <- df1

glimpse(Summary_DB)


## -----------------------------------------------------------------------------

reactable(df1,compact = TRUE,
              
              defaultSortOrder = "desc",
              defaultSorted = c("Demand"),
              defaultPageSize = 20,
              
              columns = list(
                
                `DFU` = colDef(name = "DFU"),

                
                `Demand`= colDef(
                  name = "Total Demand (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0),
                  style = list(background = "yellow",fontWeight = "bold")
                ),
                
                
                `Demand.pc`= colDef(
                  name = "Share of Demand (%)",
                  format = colFormat(percent = TRUE, digits = 1)
                ), # close %
                
                
                `Supply`= colDef(
                  name = "Total Supply (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                
                `Opening`= colDef(
                  name = "Opening Inventories (units)",
                  aggregate = "sum", footer = function(values) formatC(sum(values),format="f", big.mark=",", digits=0),
                  format = colFormat(separators = TRUE, digits=0)
                ),
                
                
                Demand.Quantity = colDef(
                  name = "Projected Demand",
                  cell = function(value, index) {
                    sparkline(df1$Demand.Quantity[[index]])
                  }),
                

                
                
                Supply.Quantity = colDef(
                  name = "Projected Supply",
                  cell = function(values) {
                    sparkline(values, type = "bar"
                              #chartRangeMin = 0, chartRangeMax = max(chickwts$weight)
                    )
                  }),
                
                
                
                `SSCov`= colDef(
                  name = "Safety Stock (Periods)",
                  style = function(value) {
                    bar_style(width = value / max(df1$Min.Cov), fill = "hsl(208, 70%, 90%)")
                  }
                ),
                
                
                `DRPCovDur`= colDef(
                  name = "Frequency of Supply (Periods)",
                  style = function(value) {
                    bar_style(width = value / max(df1$Max.Cov), fill = "hsl(0,79%,72%)")
                  }
                )
                


 
                
                
              ), # close columns list
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              
              columnGroups = list(
                
                
                colGroup(name = "DRP parameters",
                         columns = c("SSCov", "DRPCovDur", "MOQ")),
                
                colGroup(name = "Demand",
                         columns = c("Demand",
                                     "Demand.pc",
                                     "Demand.Quantity")),
                
                colGroup(name = "Supply",
                         columns = c("Supply", "Supply.Quantity"))
                
                
              )
          
) # close reactable



## -----------------------------------------------------------------------------

# keep only needed columns
df1 <- blueprint_drp %>% select(DFU, Period, FH)

# spread
df1 <- df1 %>% spread(Period, FH)


# create DT
datatable(df1,
              #filter = list(position = 'top', clear = FALSE),

              options = list(
                searching = FALSE,
                pageLength = 20
                #columnDefs = list(list(width = '200px', targets = c(1,2)))
              ),rownames= FALSE) %>%

      #formatRound(2:2, 1) %>%

      #formatStyle(columns = c(1:100), fontSize = '85%') %>%
  
  formatStyle(
    2:20,
    backgroundColor = styleEqual(
      c('Frozen'), c('yellow')
    )) 



## -----------------------------------------------------------------------------


# set a working df
df1 <- blueprint_drp
df1 <- as.data.frame(df1)


# calculate drp
calculated_drp <- drp(data = df1,
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


head(calculated_drp)



## -----------------------------------------------------------------------------

calculated_drp <-as.data.frame(calculated_drp)

# filter data
Selected_DB <- filter(calculated_drp, calculated_drp$DFU == "Item 000004")


glimpse(Selected_DB)



## -----------------------------------------------------------------------------

# keep only the needed columns
df1 <- Selected_DB %>% select(Period,
                              FH,
                              Demand,
                              DRP.Calculated.Coverage.in.Periods,
                              DRP.Projected.Inventories.Qty,
                              DRP.plan)



# replace missing values by zero
df1$DRP.Projected.Inventories.Qty[is.na(df1$DRP.Projected.Inventories.Qty)] <- 0
df1$DRP.plan[is.na(df1$DRP.plan)] <- 0


# create a f_colorpal field
df1 <- df1 %>% mutate(f_colorpal = case_when( DRP.Calculated.Coverage.in.Periods > 6 ~ "#FFA500",
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
                name = "Calculated Supply (units)",
                cell = data_bars(df1,
                                 fill_color = "#3CB371",
                                 text_position = "outside-end"
                                 )
                )





              ), # close columns lits

              columnGroups = list(
                colGroup(name = "Projected Inventories", columns = c("DRP.Calculated.Coverage.in.Periods", "DRP.Projected.Inventories.Qty"))


              )

    ) # close reactable



## -----------------------------------------------------------------------------

#------------------------------
# Get data
df1 <- calculated_drp
df1 <- as.data.frame(df1)


#------------------------------
# Filter

# subset Period based on those Starting and Ending Periods
df1 <- subset(df1,df1$Period >= "2022-07-03" & df1$Period <= "2022-09-25")


#--------
# Keep Initial data
#--------
    
# replace missing values by zero
df1$Demand[is.na(df1$Demand)] <- 0
    
Initial_DB <- df1
   


#------------------------------
# Transform
    
    
    
#--------
# Create a Summary database
#--------
    
# set a working df
df1 <- Initial_DB
    
# aggregate
df1 <- df1 %>% select(
      DFU,
      Demand) %>%
      group_by(DFU
               ) %>%
      summarise(Demand.Qty = sum(Demand)
      )
    

# Get Results
Value_DB <- df1



#--------
# Create the SRA
#--------

# set a working df
df1 <- Initial_DB

#------------------------------
# keep only the needed columns
df1 <- df1[,c("DFU","Period","DRP.Calculated.Coverage.in.Periods")]

# format as numeric
df1$DRP.Calculated.Coverage.in.Periods <- as.numeric(df1$DRP.Calculated.Coverage.in.Periods)

# formatting 1 digit after comma
df1$DRP.Calculated.Coverage.in.Periods = round(df1$DRP.Calculated.Coverage.in.Periods, 1)

# spread data
df1 <- df1 %>% spread(Period, DRP.Calculated.Coverage.in.Periods)

# replace missing values by zero
df1[is.na(df1)] <- 0

# Get Results
SRA_DB <- df1
 



#--------
# Merge both database
#--------

# merge both databases
df1 <- left_join(Value_DB, SRA_DB)

# Sort by Demand.Qty descending
df1 <- df1[order(-df1$Demand.Qty),]


# rename column
df1 <- df1 %>% rename(
      "Total Demand (units)" = Demand.Qty
    )


# Get Results
Interim_DB <- df1





## -----------------------------------------------------------------------------

#------------------------------
# create DT
  


df1 <- Interim_DB

datatable(df1,
              #filter = list(position = 'top', clear = FALSE),

              options = list(
                searching = FALSE,
                pageLength = 20,
                columnDefs = list(list(width = '200px', targets = c(1,2)))
              ),rownames= FALSE) %>%

      formatRound(2:2, 1) %>%

      formatStyle(columns = c(1:100), fontSize = '85%') %>%

      formatStyle(
        3:20,
        backgroundColor = styleInterval(c(-0.1,0.0,4.0), c('#FF6347', 'orange', 'yellow','lightblue'))
      ) %>%



    formatStyle(
      2:2,
      backgroundColor = 'mediumseagreen'
    ) 



