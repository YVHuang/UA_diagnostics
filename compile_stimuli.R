args = commandArgs(trailingOnly=TRUE)
if (length(args)!=2){
  stop("Use: 'rscript compile_stimuli plugin subqs' where 'plugin' is the name of jspsych plugin and 'subqs' is boolean reflecting existence of sub-questions.")
  }

plugin <- gsub("-","_",args[1])
subqs <- args[2]

stimuli <- read.csv("stimuli/stimuli.csv", colClasses="character")
output<-c("var stimuli = {stims: [")
for (i in 1:length(stimuli$sentence)){
  if (subqs){
    temp<-paste('{ superq:["',stimuli$sentence[i],' ',stimuli$question[i],'"], questions:["',stimuli$NP1[i],'"',sep="")
  }else{
    temp<-paste('{ questions:["',stimuli$sentence[i],' ',stimuli$question[i],'"], options:[["',stimuli$NP1[i],'"',sep="")
  }
  for (np in c(2:9)){
    if (is.na(stimuli[i,paste("NP",np,sep="")])==FALSE){
      if (stimuli[i,paste("NP",np,sep="")]!=""){
        temp<-paste(temp,', "',stimuli[i,paste("NP",np,sep="")],'"',sep="")
      }
    }
  }
  if (subqs){
    output<-paste(output,temp,'],',sep="")
    tempo<-paste('options:[[',stimuli$NP1options[i],']',sep="")
    tempc<-switch(plugin,
                  survey_multi_choice = paste('correct:[',stimuli$NP1correct[i],sep=""),
                  survey_multi_select = paste('correct:[[',stimuli$NP1correct[i],']',sep=""))
    for (np in c(2:9)){
      if (is.na(stimuli[i,paste("NP",np,"options",sep="")])==FALSE){
        if (stimuli[i,paste("NP",np,sep="")]!=""){
          tempc<-switch(plugin,
                        survey_multi_choice = switch(as.character(stimuli[i,paste("NP",np,"correct",sep="")]),
                                                     none = paste(tempc,', ""',sep=""),
                                                     paste(tempc,', ',stimuli[i,paste("NP",np,"correct",sep="")],sep="")),
                        survey_multi_select = switch(as.character(stimuli[i,paste("NP",np,"correct",sep="")]),
                                                     none = paste(tempc,', []',sep=""),
                                                     paste(tempc,', [',stimuli[i,paste("NP",np,"correct",sep="")],']',sep="")))
          tempo<- paste(tempo,', [',stimuli[i,paste("NP",np,"options",sep="")],']',sep="")
        }
      }
    }
    output<-paste(output,tempo,'], ',tempc,']},',sep="")
  }else{    
    tempc<-switch(plugin,
                  survey_multi_choice = switch(as.character(stimuli$correct[i]),
                                               none = '""',
                                               stimuli$correct[i]),
                  survey_multi_select = switch(as.character(stimuli$correct[i]),
                                               none = '[]',
                                               paste('[',stimuli$correct[i],']',sep="")))
    output<-paste(output,temp,']], correct:[',tempc,']},',sep="")
  }
}
output<-paste(output,"], ", sep="")

practice <- read.csv("stimuli/practice.csv", colClasses="character")
output<-paste(output,"practice: [",sep="")
for (i in 1:length(practice$sentence)){
  if (subqs){
    temp<-paste('{ superq:["',practice$sentence[i],' ',practice$question[i],'"], questions:["',practice$NP1[i],'"',sep="")
  }else{
    temp<-paste('{ questions:["',practice$sentence[i],' ',practice$question[i],'"], options:[["',practice$NP1[i],'"',sep="")
  }
  for (np in c(2:9)){
    if (is.na(practice[i,paste("NP",np,sep="")])==FALSE){
      if (practice[i,paste("NP",np,sep="")]!=""){
        temp<-paste(temp,', "',practice[i,paste("NP",np,sep="")],'"',sep="")
      }
    }
  }
  if (subqs){
    output<-paste(output,temp,'],',sep="")
    tempo<-paste('options:[[',practice$NP1options[i],']',sep="")
    tempc<-switch(plugin,
                  survey_multi_choice = paste('correct:[',practice$NP1correct[i],sep=""),
                  survey_multi_select = paste('correct:[[',practice$NP1correct[i],']',sep=""))
    for (np in c(2:9)){
      if (is.na(practice[i,paste("NP",np,"options",sep="")])==FALSE){
        if (practice[i,paste("NP",np,sep="")]!=""){
          tempc<-switch(plugin,
                        survey_multi_choice = switch(as.character(practice[i,paste("NP",np,"correct",sep="")]),
                                                     none = paste(tempc,', ""',sep=""),
                                                     paste(tempc,', ',practice[i,paste("NP",np,"correct",sep="")],sep="")),
                        survey_multi_select = switch(as.character(practice[i,paste("NP",np,"correct",sep="")]),
                                                     none = paste(tempc,', []',sep=""),
                                                     paste(tempc,', [',practice[i,paste("NP",np,"correct",sep="")],']',sep="")))
          tempo<- paste(tempo,', [',practice[i,paste("NP",np,"options",sep="")],']',sep="")
        }
      }
    }
    output<-paste(output,tempo,'], ',tempc,']},',sep="")
  }else{    
    tempc<-switch(plugin,
                  survey_multi_choice = switch(as.character(practice$correct[i]),
                                               none = '""',
                                               practice$correct[i]),
                  survey_multi_select = switch(as.character(practice$correct[i]),
                                               none = '[]',
                                               paste('[',practice$correct[i],']',sep="")))
    output<-paste(output,temp,']], correct:[',tempc,']},',sep="")
  }
}
output<-paste(output,"], ", sep="")

output<-paste(output,"question: ['",stimuli$question[1],"']};", sep="")
cat(output,file="static/js/stims.js")
