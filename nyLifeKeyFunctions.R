library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)
library(stringr)
library(dplyr)

# this file contains all the key functions required for the analysis

printCharts2Var = function ( df, var1, var2, rot, t10 )
{
  
  xcol = var1
  title = paste("Summary",eval(var1),eval(var2),sep="-" )
  var1 = grep(eval(var1), colnames(df))
  var2 = grep(eval(var2), colnames(df))
  
  if( t10)
  {
    if( is.factor(df[,var1]) & length(levels(df[,var1])))
    {
      abc=as.data.frame(table(df[,var1]))
      top10ValsVar1 = abc[with(abc,order(-Freq)),][1:10,1]
      chk = 1
      idxTop10ValsVar1 = df[,var1] %in% top10ValsVar1
      
    }
    else
    {
      idxTop10ValsVar1 = df[,var1] %in% df[,var1]
    }
    
    if( is.factor(df[,var2]) & length(levels(df[,var2])))
    {
      abc=as.data.frame(table(df[,var2]))
      top10ValsVar2 = abc[with(abc,order(-Freq)),][1:10,1]
      idxTop10ValsVar2 = df[,var2] %in% top10ValsVar2
      chk = 1
    }
    else
    {
      idxTop10ValsVar2 = df[,var2] %in% df[,var2]
    }
    
    idxTop10Vals = idxTop10ValsVar1 & idxTop10ValsVar2
    table(idxTop10Vals)
    df=df[idxTop10Vals,]
  }
  
  tempFrame =aggregate( cnt ~ df[,var1] + df[,var2],df,sum  )
  colnames(tempFrame) = c("v1","v2","tot")
  g = ggplot(tempFrame, aes(v1,tot)) +  geom_point(size=3)
  
  g + facet_wrap( ~ v2, ncol = 4,scales="free_y") + 
    #facet_wrap( . ~ v2, ncol = 3) +
    theme(axis.text.x=element_text(angle=eval(rot))) +
    labs(title = eval(title) ) + 
    xlab(eval(xcol)) +
    scale_y_continuous(breaks=seq(0, 25000, 500)) +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
}


printCharts1Var = function ( df, var1, rot, t10 )
{
  
  xcol = var1
  title = paste("Summary",eval(var1),sep="-" )
  var1 = grep(eval(var1), colnames(df))
  
  if( t10)
  {
    abc=as.data.frame(table(df[,var1]))
    top10Vals = abc[with(abc,order(-Freq)),][1:10,1]
    idxTop10Vals = df[,var1] %in% top10Vals
    table(idxTop10Vals)
    df=df[idxTop10Vals,]
  }
  
  tempFrame =aggregate( cnt ~ df[,var1],df,sum  )
  colnames(tempFrame) = c("v1","tot")
  g = ggplot(tempFrame, aes(v1,tot)) +   geom_bar(stat="identity",width=0.5) 
  #g + facet_wrap( ~ v1, ncol = 3) + 
  g + theme(axis.text.x=element_text(angle=eval(rot))) +
  labs(title = eval(title) ) + 
  xlab(eval(xcol)) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
}

# Function to identify missing data

fnNumMissing <- function(x) sum(is.na(x))

# Get the unique length of every colum that is not an empty string

fnUniqueElements <- function(x) 
{
  fun.x.df = data.frame(as.character(x), stringsAsFactors=FALSE)
  ss=subset(fun.x.df, str_trim(fun.x.df[,1]) != "")
  length(unique(ss[,1]))
}


plotStackedPercChartDiscrete <- function(fpstack.df,fpstack.var1,fpstack.var2,fpstack.facetVector)
{
  localenv <- environment()
  fpstack.titlePlot = paste("Summary",eval(fpstack.var1),eval(fpstack.var2),sep="-" )
  fpstack.v1 = grep(eval(fpstack.var1), colnames(fpstack.df))
  fpstack.v2 = grep(eval(fpstack.var2), colnames(fpstack.df))
  
  fpstack.g = ggplot(data=fpstack.df, aes_string(x=fpstack.var1,fill=fpstack.var2),
                     position="fill") + 
    geom_bar(position="fill") +
    #    geom_bar() +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90)) +
    labs(x=NA,y="perc")
  
  print(fpstack.g)
  
  for ( fpstack.fvar in fpstack.facetVector )
  {
    fpstak.varFacet = grep(eval(fpstack.fvar), colnames(fpstack.df))
    print(
            fpstack.g +
            facet_wrap(  ~ eval(fpstack.fvar),scales="free_y") 
            #theme(strip.text.x=element_text(size=8))
          )
  }
}

plotStackedPercChartDiscreteNew <- function(fun.df, fun.vect )
{
  # convert the data set into a smaller set to make the plots small
  fun.df.new = fun.df[fun.vect]

  # Get the length of the vector
  fun.l.df = length(fun.vect)
  
  # give some generic column names that can be easily used
  colnames(fun.df.new) = paste(rep("var",fun.l.df),seq(1:fun.l.df),sep="")

  # create a ggplot with two categorical variables and faceted by the third
  # the idea is to create a proportion 100% staked bar chart
  
  fun.g = ggplot(data=fun.df.new, aes(x=as.factor(var1),fill=as.factor(var3),
                     position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[3],title = paste(fun.vect[1], fun.vect[2], fun.vect[3],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[3])) +
    facet_wrap( ~ var2)

  print(fun.g)

}

plotStackedPercChartDiscreteNoFaceting <- function(fun.df, fun.vect )
{
  fun.vect = c(fun.vect, "premiums_from_policies_purchased_in_past_3_years","own_any")
  # convert the data set into a smaller set to make the plots small
  fun.df.new = fun.df[fun.vect]
  
  # Get the length of the vector
  fun.l.df = length(fun.vect)
  
  # give some generic column names that can be easily used
  colnames(fun.df.new) = paste(rep("var",fun.l.df),seq(1:fun.l.df),sep="")
  
  # create a ggplot with two categorical variables and faceted by the third
  # the idea is to create a proportion 100% staked bar chart
  
  fun.g = ggplot(data=fun.df.new, aes(x=as.factor(var1),fill=as.factor(var2),
                                      position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[2],title = paste(fun.vect[1], fun.vect[2],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[2]))

  print(fun.g)
  
  fun.g1 = ggplot(data=subset(fun.df.new, var3 > 0 & var4 == 1),
         aes(
           x=as.factor(var1),
           y=var3
         )
  ) + 
    geom_boxplot() + 
    scale_y_continuous ("Premimum Last 3 years",limits=c(0,2000)) +
    labs(x=fun.vect[1]) +
    stat_summary(fun.y = "mean", colour = "red", geom="point",size=5,position=position_dodge(0.75)) +
    theme(axis.text.x=element_text(angle=90)) +
    facet_wrap ( ~ var2 )
  
  print(fun.g1)
  
}

plotStackedPercChartDiscreteCompIns <- function(fun.df, fun.vect )
{
  fun.vect = c(fun.vect,"own_term","own_wl","own_vl","own_ul")
  # convert the data set into a smaller set to make the plots small
  fun.df.new = fun.df[fun.vect]
  
  # Get the length of the vector
  fun.l.df = length(fun.vect)
  
  # give some generic column names that can be easily used
  colnames(fun.df.new) = paste(rep("var",fun.l.df),seq(1:fun.l.df),sep="")
  
  # create a ggplot with two categorical variables and faceted by the third
  # the idea is to create a proportion 100% staked bar chart
  
  fun.g1 = ggplot(data=fun.df.new, aes(x=as.factor(var1),fill=as.factor(var2),
                                      position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[2],title = paste(fun.vect[1], fun.vect[2],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[2]))
  
  fun.g2 = ggplot(data=subset(fun.df.new,var2==1), aes(x=as.factor(var1),fill=as.factor(var3),
                                       position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[3],title = paste(fun.vect[1], fun.vect[3],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[3]))
  
  fun.g3 = ggplot(data=subset(fun.df.new,var2==1), aes(x=as.factor(var1),fill=as.factor(var4),
                                                       position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[4],title = paste(fun.vect[1], fun.vect[4],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[4]))

  fun.g4 = ggplot(data=subset(fun.df.new,var2==1), aes(x=as.factor(var1),fill=as.factor(var5),
                                                       position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[5],title = paste(fun.vect[1], fun.vect[5],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[5]))  
  
  fun.g5 = ggplot(data=subset(fun.df.new,var2==1), aes(x=as.factor(var1),fill=as.factor(var6),
                                                       position="fill")) + 
    geom_bar(position="fill") +
    scale_y_continuous(labels=percent_format()) +  
    theme(axis.text.x=element_text(angle=90) ) +
    labs(x=fun.vect[1],y=fun.vect[6],title = paste(fun.vect[1], fun.vect[6],sep=";")) +
    guides(fill=guide_legend(title=fun.vect[6])) 
  
  multiplot(fun.g2,fun.g3,fun.g4,fun.g5,cols=2)
  
}


plotExamples <- function ()
{

  df=data.frame(matrix(rnorm(500),100,5))
  colnames(df)=paste(rep("VAR",5),seq(1:5),sep="")
  df$cat1 = rep(paste(rep("C",10),seq(1:10),sep=""),10)
  df$cat2 = rep(paste(rep("K",4),seq(1:4),sep=""),each=25)
  df$cat3 = rep(paste(rep("D",2),seq(1:2),sep=""),each=50)

  g1=ggplot(df,aes(x=cat1, y=cat2,color=VAR2)) + geom_point() +
    stat_sum(show_guide=FALSE) + 
    scale_size(range = c(3,10))
  
  g1=ggplot(df,aes(x=cat1, y=cat2,color=VAR2)) + geom_point() +
    stat_sum(show_guide=FALSE) + 
    scale_size(range = c(3,10))
  
  g2=ggplot(df,aes(x=VAR1, y=VAR2,color=cat3)) +
    geom_point() + stat_smooth(method="lm")
  
  g3=ggplot(df,aes(x=VAR1, y=VAR2,color=cat3)) + facet_wrap( ~cat2) +
    geom_point() + stat_smooth(method="lm")
  
  g4=ggplot(df,aes(x=cat1, y=VAR2)) + geom_bar(stat="identity")
  
  g5=ggplot(df,aes(x=VAR1, y=VAR2,color=cat2)) + 
    geom_smooth(group=1,method="lm",alpha=0.4,size=2) +
    facet_wrap( ~ cat1,scales="free")
  
  ggplot(data=df,
         aes(
           x=as.factor(cat1),
           y=VAR1,
           fill=cat3
         )
  ) + 
    geom_boxplot() + 
    scale_x_discrete ("x") +
    stat_summary(fun.y = "mean", colour = "red", geom="point",size=5, position=position_dodge(width=0.75))
  
  
  multiplot(g1,g2,g3,g4, cols=2)

}

printingMultPlots <- function()
{
  library(ggplot2)
  library(gridExtra)
  
  dat = data.frame(x=1:10, y=1:10)
  
  plot_list = list()
  nplot = 1
  
  for (i in seq(nplot)) {
    new_plot = ggplot(dat, aes(x=x, y=y)) +
      geom_point() +
      labs(title=paste("plot", i))
    plot_list = c(plot_list, list(new_plot))
  }
  
  pdf("plots_naveen.pdf", width=6, height=3)
  do.call(grid.arrange, c(plot_list, list(ncol=nplot)))
  dev.off()
}

printMultPlots2 <- function()
{
  dev.off()
  library(ggplot2)
  library(gridExtra)
  
  pdf("plots_new1.pdf", onefile = TRUE)
  cuts <- unique(diamonds$cut)
  for(i in 1:length(cuts)){
    dat <- subset(diamonds, cut==cuts[i])
    top.plot <- ggplot(dat, aes(price,table)) + geom_point()
    bottom.plot <- ggplot(dat, aes(price,depth)) + geom_point() 
    grid.arrange(top.plot, bottom.plot)
  }
  dev.off()
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
{
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


recodeValuesByGrep <- function( fun.df, fun.fields, fun.grep.par )
{
  fun.lp = 'rev_'
  fun.feilds.new = NA
  
  #intialize the data frame
  
  fun.final.df = as.data.frame(fun.df[,1])
  fun.final.df[,1] = NULL
  
  for ( fun.field in fun.fields )
  {
    # get the column ID from the original dataframe
    fun.feild.id = grep(eval(fun.field), colnames(fun.df))
    
    #create a temp data frame with just one field
    
    fun.df.field = as.data.frame(fun.df[ , fun.feild.id])
    
    #initialize the field value to 0
    fun.df.field[,1] = 0
    
    # construct a name for the new field
    fun.df.new_field = paste(fun.lp, fun.field,sep="")
    
    # find the rows that are blank and that match the criteria set in the function
    
    k = grep('^$',fun.df[ , fun.feild.id])
    k = c(k, grep(fun.grep.par,fun.df[ , fun.feild.id]))
          
    # Update the values to 0 and 1
    fun.df.field[k,1] = 0
    fun.df.field[-k,1] = 1
    
    # Rename the column
    colnames(fun.df.field) = fun.df.new_field
    
    # Bind the new revised column to the dataframe
    fun.final.df = cbind(fun.final.df, fun.df.field)
          
  }
  
  # return the final data frame
  fun.final.df
}

createCombinationsAndPrintGraphs_threeVar <- function( df, vectFields, pivotField,numComb, fileName )
{
  # get all possible combinations of two fields
  combMatrix = combn(vectFields,numComb)
  
  pdf(fileName, onefile = TRUE)
  
  
  # Loop through all the combinations and identify Trends
  for ( ii in seq(1:ncol(combMatrix)) ) 
  {
    graphFields = combMatrix[,ii]
    
    fieldList = NULL
    for (kk in 1:numComb)
    {
      
      fieldList = c(fieldList,graphFields[kk])
    }
    
    fieldList = c(fieldList,pivotField)

    plotStackedPercChartDiscreteNew(df, fieldList)
  }
  
  dev.off()

}

createCombinationsAndPrintGraphs_twoVar <- function( df, vectFields, pivotField,numComb, fileName )
{
  # get all possible combinations of two fields
  combMatrix = combn(vectFields,numComb)
  
  pdf(fileName, onefile = TRUE)
  
  # Loop through all the combinations and identify Trends
  for ( ii in seq(1:ncol(combMatrix)) ) 
  {
    graphFields = combMatrix[,ii]
    
    fieldList = NULL
    for (kk in 1:numComb)
    {
      
      fieldList = c(fieldList,graphFields[kk])
    }
    
    fieldList = c(fieldList,pivotField)
    
    plotStackedPercChartDiscreteNoFaceting(df, fieldList)
  }
  
  dev.off()
  
}

createCombinationsAndPrintGraphs_twoVar_CompIns <- function( df, vectFields, pivotField,numComb, fileName )
{
  # get all possible combinations of two fields
  combMatrix = combn(vectFields,numComb)
  
  pdf(fileName, onefile = TRUE)
  
  # Loop through all the combinations and identify Trends
  for ( ii in seq(1:ncol(combMatrix)) ) 
  {
    graphFields = combMatrix[,ii]
    
    fieldList = NULL
    for (kk in 1:numComb)
    {
      
      fieldList = c(fieldList,graphFields[kk])
    }
    
    fieldList = c(fieldList,pivotField)
    
    plotStackedPercChartDiscreteCompIns(df, fieldList)
  }
  
  dev.off()
  
}

findRanksForInsuranceCompanies <- function ( fun.dt )
{

  # Get the count of terms
  
  cntTermIns=nrow(fun.dt[str_trim(trmlf_inst_1_institution_name)!="",])
  cntVarIns=nrow(fun.dt[str_trim(varlf_inst_1_institution_name)!="",])
  cntWholeIns=nrow(fun.dt[str_trim(whlf_inst_1_institution_name)!="",])
  cntUnivIns=nrow(fun.dt[str_trim(unilf_inst_1_institution_name)!="",])
  cntAutoIns=nrow(fun.dt[str_trim(auto_inst_1_institution_name)!="",])
  cntResIns=nrow(fun.dt[str_trim(res_inst_1_institution_name)!="",])

  # grab the unique institutions name from all types of insurance
  
  uniqInst = unique(c(fun.dt[str_trim(trmlf_inst_1_institution_name)!="",str_trim(trmlf_inst_1_institution_name)],
                      fun.dt[str_trim(varlf_inst_1_institution_name)!="",str_trim(varlf_inst_1_institution_name)],
                      fun.dt[str_trim(whlf_inst_1_institution_name)!="",str_trim(whlf_inst_1_institution_name)],
                      fun.dt[str_trim(unilf_inst_1_institution_name)!="",str_trim(unilf_inst_1_institution_name)],
                      fun.dt[str_trim(auto_inst_1_institution_name)!="",str_trim(auto_inst_1_institution_name)],
                      fun.dt[str_trim(res_inst_1_institution_name)!="",str_trim(res_inst_1_institution_name)]))
  
  # convert the vector into a data table
  dtRefInst = data.table( instName=uniqInst)
  setkey(dtRefInst,instName)
  
  
  # Create table with percentages and numbers for each insurance type
  dtTerm=fun.dt[str_trim(trmlf_inst_1_institution_name)!="",
                   list(avgTerm=(.N/cntTermIns)*100,numTerm=.N),
                   by=str_trim(trmlf_inst_1_institution_name)][order(-numTerm)]
  
  dtVar=fun.dt[str_trim(varlf_inst_1_institution_name)!="",
                  list(avgVar=(.N/cntVarIns)*100,numVar=.N),
                  by=str_trim(varlf_inst_1_institution_name)][order(-numVar)]
  
  dtUniv=fun.dt[str_trim(unilf_inst_1_institution_name)!="",
                   list(avgUniv=(.N/cntUnivIns)*100,numUniv=.N),
                   by=str_trim(unilf_inst_1_institution_name)][order(-numUniv)]
  
  dtWhole=fun.dt[str_trim(whlf_inst_1_institution_name)!="",
                    list(avgWhole=(.N/cntWholeIns)*100,numWhole=.N),
                    by=str_trim(whlf_inst_1_institution_name)][order(-numWhole)]
  
  dtAuto=fun.dt[str_trim(auto_inst_1_institution_name)!="",
                   list(avgAuto=(.N/cntAutoIns)*100,numAuto=.N),
                   by=str_trim(auto_inst_1_institution_name)][order(-numAuto)]
  
  dtRes=fun.dt[str_trim(res_inst_1_institution_name)!="",
                  list(avgRes=(.N/cntResIns)*100,numRes=.N),
                  by=str_trim(res_inst_1_institution_name)][order(-numRes)]
  
  # Perform a bunch of Left Outer Joins by Assigning the Institution Name as the Key
  
  dTemp=data.table(data.table(data.table(dtRefInst[dtTerm,],key="instName")[dtVar,],key="instName")[dtUniv,],key="instName")
  dtFinalInst=data.table(data.table(data.table(dTemp[dtWhole],key="instName")[dtAuto,],key="instName")[dtRes],key="instName")
  dtFinalInst[,':='(rankTerm=rank(-numTerm),rankVar=rank(-numVar),
                    rankUniv=rank(-numUniv),rankWhole=rank(-numWhole),
                    rankAuto=rank(-numAuto),rankRes=rank(-numRes))]
  
  # Return the Final Institution List

  dtFinalInst
  ############# Analyze the data as a data.table ###################

}

