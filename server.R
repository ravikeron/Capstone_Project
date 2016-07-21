#
# This is the server logic of a Shiny web application for predicting the next word in a sentence
#
library(tm)
library(stringr)
library(shiny)

# load the 2-gram, 3-gram and 4-gram models created separately from the given input files
# These are built into term document matrix after taking a sample and cleaning 
# These would be used to predict the next word

#load("E:/DataScience/Capstone/Project/freq2.RData");
#load("E:/DataScience/Capstone/Project/freq3.RData");
#load("E:/DataScience/Capstone/Project/freq4.RData");

load("freq2.RData");
load("freq3.RData");
load("freq4.RData");

Inputcleaning <- function(instr)
{
  
  
  # First remove the non-alphabatical characters
  instr <- iconv(instr, "latin1", "ASCII", sub=" ");
  instr <- gsub("[^[:alpha:][:space:][:punct:]]", "", instr);
  
  # Then convert to a Corpus
  instrCorpus <- VCorpus(VectorSource(instr))
  
  # Apply cleaning steps
  instrCorpus <- tm_map(instrCorpus, content_transformer(removePunctuation))
  instrCorpus <- tm_map(instrCorpus, content_transformer(removeNumbers))
  instrCorpus <- tm_map(instrCorpus, content_transformer(stripWhitespace))
  instrCorpus <- tm_map(instrCorpus, content_transformer(tolower))
  instr <- as.character(instrCorpus[[1]])
  instr <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", instr)
  
  # Return the cleaned sentence
  if (nchar(instr) > 0) {
    return(instr); 
  } else {
    return("");
  }
}

Predictnext <- function(instr)
{
  #assign("mesg", "in Predictnext", envir = .GlobalEnv)
  
  # Clean the input string and extract the words removing other characters
  instr <- Inputcleaning(instr);
  
  # Split the input string and extract the length
  instr <- unlist(strsplit(instr, split=" "));
  instrlen <- length(instr);
  
  nxttermfound <- FALSE;
  Predictnext <- as.character(NULL);
  
  # Check the four gram 
  if (instrlen >= 3 & !nxttermfound)
  {
    # capture the 3 words from the end
    instr1 <- paste(instr[(instrlen-2):instrlen], collapse=" ");
    
    searchStr <- paste("^",instr1, sep = "");
    
    freq4temp <- freq4[grep (searchStr, freq4$terms), ];
    msg41 <- "before 4 matching check"
    # check if any matching record returned
    if ( length(freq4temp[, 1]) >= 1 )
    {
      msg1 <- "in record found"
      Predictnext <- as.character(freq4temp[1,1]);
      nxttermfound <- TRUE;
      freq4temp <- NULL;
    }
    
    
  }
  
  # Check the three gram using the three gram data frame
  if (instrlen >= 2 & !nxttermfound)
  {
    # identify the 2 words from the end
    instr1 <- paste(instr[(instrlen-1):instrlen], collapse=" ");
    
    
    searchStr <- paste("^",instr1, sep = "");
    freq3temp <- freq3[grep (searchStr, freq3$terms), ];
    msg31 <- "before 3 matching check"
    # check if any matching record returned
    if ( length(freq3temp[, 1]) >= 1 )
    {
      Predictnext <- freq3temp[1,1];
      nxttermfound <- TRUE;
      freq3temp <- NULL;
    }
    
  }
  
  # Check the two gram using the two gram data frame
  if (instrlen >= 1 & !nxttermfound)
  {
    # get the last word
    instr1 <- instr[instrlen];
    
    searchStr <- paste("^",instr1, sep = "");
    freq2temp <- freq2[grep (searchStr, freq2$terms), ];
    msg21 <- "before 2 matching check"
    # check if any matching record returned
    if ( length(freq2temp[, 1]) >= 1 )
    {
      Predictnext <- freq2temp[1,1];
      nxttermfound <- TRUE;
      freq2temp <- NULL;
    }
    
  }

  
  # If no next term found in Four, Three and Two Grams return No phrase matches
  if (!nxttermfound & instrlen > 0)
  {
    Predictnext <- "No phrase matches"
  }
  
  nextterm <- word(Predictnext, -1);
  
  if (instrlen > 0){
    #dftemp1 <- data.frame(nextterm, mesg);
    dftemp1 <- data.frame(nextterm);
    return(dftemp1);
  } else {
    nextterm <- "";
    #mesg <-"";
    #dftemp1 <- data.frame(nextterm, mesg);
    dftemp1 <- data.frame(nextterm);
    return(dftemp1);
  }
}


shinyServer(function(input, output) {
   
  output$wordprediction <- renderPrint({
    
    str1 <- Inputcleaning(input$inputId)
    str2 <- Predictnext(str1)
    input$action;
    msg <<- as.character(str2[1,2]);
    cat("", as.character(str2[1,1]))
    #cat("\n\t");
    #cat("\n\t");
    #cat("Note: ", as.character(str2[1,2]));
  })
  #output$text1 <- renderText({paste("The input sentence is : ", input$inputId)});
  output$text1 <- renderText({input$action;})
  
})
