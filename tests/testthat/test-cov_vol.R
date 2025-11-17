

test_that("cov_vol() returns the expected result", {


  #--------------------------------
  # Input values for the test
  #--------------------------------

  # create dataset for the test
  Period <- c(
    "1/1/2020", "2/1/2020", "3/1/2020", "4/1/2020", "5/1/2020", "6/1/2020", "7/1/2020", "8/1/2020", "9/1/2020", "10/1/2020", "11/1/2020", "12/1/2020","1/1/2021", "2/1/2021", "3/1/2021", "4/1/2021", "5/1/2021", "6/1/2021", "7/1/2021", "8/1/2021", "9/1/2021", "10/1/2021", "11/1/2021", "12/1/2021")

  Demand <- c(360, 458,300,264,140,233,229,208,260,336,295,226,336,434,276,240,116,209,205,183,235,312,270,201)


  # assemble
  input <- data.frame(Period,
                      Demand)

  # let's add a Product
  input$DFU <- "Product A"

  # add a Coverage
  input$Coverage <- 4

  # format the Period as a date
  input$Period <- as.Date(as.character(input$Period), format = '%m/%d/%Y')




  #--------------------------------
  # Expected outputs
  #--------------------------------


  # expected output for the calculated Coverage.Volume
  expected_output_Coverage.Volume <- c(1162,  937,  866,  810,  930, 1033, 1099, 1117, 1193, 1291, 1272,
    1286, 1066,  841,  770,  713,  832,  935, 1000,   NA,   NA,   NA, NA,   NA)


  #--------------------------------
  # Run function
  #--------------------------------


  # Call the function
  calculated_dataset <- cov_vol(input)

  # extract the Coverage.Volume
  output_Coverage.Volume <- calculated_dataset$Coverage.Volume


  #--------------------------------
  # Run Checks
  #--------------------------------


  # Check if the output matches the expected result for the Coverage.Volume
  expect_equal(output_Coverage.Volume, expected_output_Coverage.Volume)


})




