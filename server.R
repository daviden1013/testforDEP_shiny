
library(testforDEP)

myresult = data.frame(matrix(0, nrow = 0, ncol = 7))
colnames(myresult) = c("data", "method", "test statistic", "p-value", "Normal CI", "Pivotal CI", "Percentile CI")

flag = reactiveValues(v = FALSE, i = FALSE)


#############################################################################
server =function(input, output, session) {
  
  #p.opt option
  output$p.opt = renderUI({
    if(input$test == "PEARSON" || input$test == "KENDALL" ||input$test == "SPEARMAN")
      radioButtons('p.opt', 'p value option', 
        c('Monte Carlo' = "MC", Distribution = "dist"), inline = TRUE)
    else if(input$test == "VEXLER" ||input$test == "MIC" ||input$test == "HOEFFD" ||input$test == "EL" )
      radioButtons('p.opt', 'p value option', 
        c('Monte Carlo' = "MC", table = "table"), inline = TRUE)
    else
      radioButtons('p.opt', 'p value option', 
        c('Monte Carlo' = "MC"))
  })


  
  #MC.num option
  output$ifMC = renderUI({
    if(!is.null(input$p.opt)){
      if(input$p.opt == "MC"){
          return (sliderInput('num.MC', "MC simulation number",
                    min=100, max=10000, value=5000, step = 100))
      }
      else
        return (NULL)
    }
   return (NULL)
  })
  
  #BS for Spearman or others
  output$BS.CI = renderUI({
    if(input$test == "SPEARMAN")
      return (NULL)
      
    else
      sliderInput('BS.CI', "alpha for Bootstrap CI",
                  min=0, max=1, value=0.05, step = 0.01)
  })


  #get data
  getData = reactive({
    inFile = input$file1

    if(is.null(inFile))
      return (NULL)

    data = read.csv(inFile$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote)
    

    if(ncol(data) < 2)
      stop("Uploaded data must have two columns.")
    
    ncol = ncol(data)
    out = cbind(data[,ncol-1], data[,ncol])
    colnames(out) = colnames(data)[c(ncol-1, ncol)]
    
    if(!is.numeric(out[,1]) || !is.numeric(out[,2]))
      stop("Uploaded data must be numeric.")
    
    return (out)
  })
  
  #action listener for compute button 
  #(also prevent other changes form triggering computations)

  observeEvent(input$compute, {
      flag$v <<- TRUE
  })
  observeEvent(input$test, {
      flag$v <<- FALSE
  })
  observeEvent(input$p.opt, {
      flag$v <<- FALSE
  })
  observeEvent(input$BS.CI, {
      flag$v <<- FALSE
  })
  observeEvent(input$rm.na, {
      flag$v <<- FALSE
  })
  observeEvent(input$file1, {
      flag$v <<- FALSE
  })
  observeEvent(input$header, {
      flag$v <<- FALSE
  })
  observeEvent(input$sep, {
      flag$v <<- FALSE
  })
  observeEvent(input$quote, {
      flag$v <<- FALSE
  })
  #scatter plot
  output$scatter = renderPlot({
    data = getData()
    if(is.null(data))
      plot(1, type="n", xlab="x", ylab="y", main = "Scatter plot (empty)")
    else
      plot(data[,1], data[,2], xlab= colnames(data)[1], ylab=colnames(data)[2], 
        main = "Scatter plot")
  })
  
  
  #Kendall plot
  output$kplot = renderPlot({
      data = getData()
      
      if (is.null(data)){
        g <- function(w) {
          w - w * log(w)
        }
        plot(g, main = "Kendall Plot (empty)", xlim = c(0, 1), ylim = c(0, 1), pch = "x", xlab = expression(W[1:n]),
               ylab = "H")  
        abline(a = 0, b = 1) 
      }
      else 
        return (AUK(data[,1], data[,2], plot = T))

   })

  
  #test
  output$result = renderTable({

      if(!flag$v){
        return (myresult)
      }

      data = getData()
         
      if (is.null(data))
        return(myresult)
      
      test = input$test
      BSCI = ifelse(input$test != "SPEARMAN", input$BS.CI, 0)
      
      obj = testforDEP(data[,1], data[,2], test = test, p.opt = input$p.opt,
            num.MC = input$num.MC, BS.CI = BSCI, rm.na = input$rm.na)
     
      out = data.frame(matrix(0, nrow = 1, ncol = 7))
      out[1,1] = input$file1[1]
      out[1,2] = input$test
      out[1,3] = obj@TS
      out[1,4] = ifelse(obj@p_value <0.001, "<0.001", round(obj@p_value, digits = 3)) 
        
      if(BSCI > 0){
        normal = paste(round(obj@CI[[1]][1], digits = 2), round(obj@CI[[1]][2], digits = 2), sep = " ~ ")
        pivotal = paste(round(obj@CI[[2]][1], digits = 2), round(obj@CI[[2]][2], digits = 2), sep = " ~ ")
        percentile = paste(round(obj@CI[[3]][1], digits = 2), round(obj@CI[[3]][2], digits = 2), sep = " ~ ")
        out[1,5] = normal
        out[1,6] = pivotal
        out[1,7] = percentile
      }
      else{
        for(i in 5:7)
          out[1,i] = NA
      }
          
      colnames(out) = c("data", "method", "test statistic", "p-value", "Normal CI", "Pivotal CI", "Percentile CI")
      myresult <<- rbind(myresult, out)
      return (myresult)
  })

  output$downloadData <- downloadHandler(
    filename = function() { 
		  return ("result.csv")
	 },
    content = function(file) {
      write.csv(myresult, file)
    }
  )
  
  #info msg
  observeEvent(input$info, {
    if(flag$i)
      flag$i <<- FALSE
    else
      flag$i <<-TRUE
  })
  
  output$infoText = renderUI({
    if(flag$i){
      fileName = paste(input$test, ".txt", sep = "")
      return (pre(readChar(fileName, file.info(fileName)$size)))
    }
    else
      return (NULL)
  })
  
     
}
