library(readxl)
list1 <- read_excel("data/anonymised_data.xlsx")
data<-list1[c(1:19)]


library(qgraph)
library(bootnet) 
library(dplyr)
library(psych)
library(ggplot2)
library(NetworkComparisonTest)
library(networktools)
library(glasso)
library(mgm)

network<-as.matrix(data)
p<-ncol(network)
fit<-mgm(data=network,
         type=rep('g',p),
         level = rep(1,p),
         lambdaSel = 'CV',
         ruleReg = 'OR',
         pbar=TRUE
         )

pred<-predict(object = fit,
              data=network,
              errorCon='R2'
)

pred$error

##glasso estimation
networkZ<-estimateNetwork(data=network,
                         default = "EBICglasso",
                         tuning=0.5,
                         corMethod="cor",
                         corArgs=list(method="spearman",use="pairwise.complete.obs"))
plot(network,layout="spring")


##plot
groupsZ <- list(
  "Seriousness" = 1:6,
  "Personal Rewards"=7:15,
  "Social Rewards"=16:18,
  "Life Satisfaction" = 19)


nodeNames <- c("Perseverance", "Effort", "Career progress", "Career contingencies", 
               "Identity", "Ethos", "Personal enrichment", "Self-actualization", 
               "Self-express ability", "Self-express individual", "Self-image", 
               "Self-gratification-satisfaction", "Self-gratification-enjoy", "Re-creation", 
               "Financial return", "Group attraction", "Group accomplishments", 
               "Group maintenance", "Life satisfaction")

photoZ <- plot(networkZ,layout="spring",
              negDashed=TRUE,
              legend = TRUE, 
              borders=TRUE,
              color=c("#CCCCCC","white","#999999","#727272"), vsize=6.1,
              groups = groupsZ,  
              nodeNames = nodeNames,
              legend.cex = 0.33,
              pie=pred$error[,2],
              legend.mode='style1')
##preview
makeBW(photoZ)

##edge weight
networkZ$graph
summary(networkZ)


##flow
photo1 <- plot(networkZ,layout="spring",
               negDashed=TRUE,
               legend = TRUE, 
               borders=TRUE,
               color=c("#CCCCCC","white","#999999","#595959"), vsize=5,
               groups = groupsZ,  
               nodeNames = nodeNames,
               legend.cex = 0.2,
               pie=pred$error[,2],
               legend.mode='style1')
flow(photo1, "LS", horizontal = FALSE,equalize=FALSE,maxCurve = 2.7)
photoflow<-flow(photo1, "LS", horizontal = TRUE,maxCurve = 2.7)
makeBW(photoflow)


##centrality plot
centralityPlot(networkZ,include=c("all"))
///or///
  centralityPlot(networkZ,orderBy="ExpectedInfluence",
                 scale = c("z-scores"),
                 include = c("ExpectedInfluence"))

centralityPlot(networkZ,orderBy="Strength",
               scale = c("z-scores"),
               include = c("Strength"))

#centrality indice
centrality_auto(networkZ)


##bridge 
bridge_symp <- bridge(networkZ$graph, 
                      communities=c('1','1','1','1','1','1',
                                    '2','2','2','2','2','2','2','2','2',
                                    '3','3','3',
                                    '4'))
plot(bridge_symp, 
     order="value",
     zscore=TRUE,
     include=c("Bridge Expected Influence (1-step)"))

###bridge
bridge_symp$`Bridge Expected Influence (1-step)`


###network accuracy

#way1
bootPro<-bootnet(networkZ,default =c("EBICglasso"),statistics =
                   c("bridgeStrength","strength","closeness","betweenness",
                     "ExpectedInfluence",
                     "bridgeExpectedInfluence"),
                 nBoots=2500, type="case",
                 nCores=8,
                 communities=c('1','1','1','1','1','1',
                               '2','2','2','2','2','2','2','2','2',
                               '3','3','3',
                               '4'))
corStability(bootPro)
plot(bootPro,c("all"))
plot(bootPro,c("bridgeExpectedInfluence"))
plot(bootPro,c("ExpectedInfluence"))

#way 2
bootnet_case_dropping<-bootnet(networkZ,
                               nBoots = 2500,
                               type = "case",
                               nCores = 8,
                               communities=c('1','1','1','1','1','1',
                                             '2','2','2','2','2','2','2','2','2',
                                             '3','3','3',
                                             '4'),
                               statistics = c("bridgeStrength","strength",
                                              "ExpectedInfluence",
                                              "bridgeExpectedInfluence"))
plot(bootnet_case_dropping,"all")
or
plot(bootnet_case_dropping,c("bridgeExpectedInfluence"))
plot(bootnet_case_dropping,c("ExpectedInfluence"))

#cs
corStability(bootnet_case_dropping)

#stability of differences in edge weights or in centrality measures
differenceTest(bootnet_nonpar,
               "n1",)


#stability of edge weights

##CIs
bootnet_nonpar<-bootnet(networkZ,
                        nBoots =2500,
                        nCores = 8,)
summary(bootnet_nonpar)
plot(bootnet_nonpar,
     labels=FALSE,
     order="sample")

print(summary(bootnet_nonpar) %>%
        filter(q2.5 > 0 | q97.5 < 0), n = Inf)

