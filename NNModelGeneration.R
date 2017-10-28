nnmodel_generation <- function (wts, tag, NoOfHiddenNeuron, outputfilename,activationflag)
{
  #wts=round(nnetmodel$wts,4)
#   wts=nnetmodel$wts
#   tag=names(mydata[,subset])
  tag=c("1",tag)
  #tag[9]="mysine"
  input_hidden_wt=wts[1:(length(wts)-NoOfHiddenNeuron-1)]
  hidden_output_wt=wts[(length(wts)-NoOfHiddenNeuron):length(wts) ]
  wt_str=cbind(input_hidden_wt,tag)
  chk=paste("(",wt_str[,1],")*",wt_str[,2],sep='')
  eq=NULL
  heq=NULL
  score = paste("overallMatchScore = " , hidden_output_wt[1],sep="")
  for (j in 1:NoOfHiddenNeuron )
  {
    node=paste("H",j,  " = ",sep="")
    heq[j]=paste("H",j,  " = 1.0/(1+exp(-H" ,j,"))", sep="")
    hnode=paste("(",hidden_output_wt[j+1],")*", "H",j, sep="")
    score=paste(score,hnode,sep=" + ")
    for (i in 1:length(tag))
    {
      if (i>1)
      {
        node=paste(node,chk[(j-1)*(length(tag))+i],sep=" + ")
      }else
      {
        node=paste(node,chk[(j-1)*(length(tag))+i],sep="  ")
      }

    }
    eq[j]=node

  }
  write.table(eq,outputfilename,row.names=FALSE,append=TRUE,quote=FALSE,col.names=FALSE)
  write.table(heq,outputfilename,row.names=FALSE,append=TRUE,quote=FALSE,col.names=FALSE)
  write.table(score,outputfilename,row.names=FALSE,append=TRUE,quote=FALSE,col.names=FALSE)

  if (activationflag==1){
    activationflag="overallMatchScore=1.0/(1+exp(-overallMatchScore))"
    write.table(activationflag,outputfilename,row.names=FALSE,append=TRUE,quote=FALSE,col.names=FALSE)
  }


  #outputfilename=paste("./", outputfilename,sep='')


}



#####  
#####  set.seed(25)
#####  NoOfHiddenNeuron=7
#####  mydata=infoGain
#####  
#####  f=f2
#####  NNmodel=nnet(f,mydata,entropy=TRUE, rang =0.3,decay=0.1, maxit=100,size=NoOfHiddenNeuron);#classification entropy linout=TRue regression
#####  #summary(NNmodel)
#####  nn_score=NNmodel$fitted.values;
#####  ks_compute(nn_score,mydata$IsPaid); 
#####  
#####  
#####  KS_Nnet=ks_compute(nn_score,mydata$IsPaid); 
#####  wts=NNmodel$wts
#####  tag=attr(NNmodel$terms,"term.labels")
#####  
#####  activationflag=1
#####  
#####  outputfilename="//home/analytics/scheduledCodes/JS/NeverPaid_NNmode4.txt"
#####  source("/home/analytics/Project/LeadScroingNg/RCode/nnmodel_generation.R")
#####  
#####  nnmodel_generation (wts, tag, NoOfHiddenNeuron, outputfilename,activationflag)
