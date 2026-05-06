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

##使用glasso法进行网络估计
networkZ<-estimateNetwork(data=network,
                         default = "EBICglasso",
                         tuning=0.5,
                         corMethod="cor",
                         corArgs=list(method="spearman",use="pairwise.complete.obs"))
plot(network,layout="spring")


##开始画图
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
##黑白打印preview
makeBW(photoZ)

##知道边权重
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


#############

##中心指标图
centralityPlot(networkZ,include=c("all"))
///or///
  centralityPlot(networkZ,orderBy="ExpectedInfluence",
                 scale = c("z-scores"),
                 include = c("ExpectedInfluence"))

centralityPlot(networkZ,orderBy="Strength",
               scale = c("z-scores"),
               include = c("Strength"))

#计算中心指标
centrality_auto(networkZ)


##桥接
bridge_symp <- bridge(networkZ$graph, 
                      communities=c('1','1','1','1','1','1',
                                    '2','2','2','2','2','2','2','2','2',
                                    '3','3','3',
                                    '4'))
plot(bridge_symp, 
     order="value",
     zscore=TRUE,
     include=c("Bridge Expected Influence (1-step)"))

#####bridge####
bridge_symp$`Bridge Expected Influence (1-step)`


###网络可靠性

##计算准确性####
#way1
bootPro<-bootnet(networkZ,default =c("EBICglasso"),statistics =
                   c("bridgeStrength","strength","closeness","betweenness",
                     "ExpectedInfluence",
                     "bridgeExpectedInfluence"),
                 nBoots=500, type="case",
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
                               nBoots = 1000,
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
#输出cs指数
corStability(bootnet_case_dropping)

#stability of differences in edge weights or in centrality measures
differenceTest(bootnet_nonpar,
               "n1",)


#stability of edge weights
##计算置信区间
bootnet_nonpar<-bootnet(networkZ,
                        nBoots =2500,
                        nCores = 8,)
summary(bootnet_nonpar)
plot(bootnet_nonpar,
     labels=FALSE,
     order="sample")



print(summary(bootnet_nonpar) %>%
        filter(q2.5 > 0 | q97.5 < 0), n = Inf)



##桥接网络图
# 提取子网络
AA1 <- network$graph[1:18, 1:18]  # 第一个量表的网络
BB1 <- network$graph[19:23, 19:23]  # 第二个量表的网络

# 创建零矩阵（连接部分）
AB1 <- matrix(0, 18, 5)  # 连接 AA1 和 BB1（18行 × 5列）
BA1 <- matrix(0, 5, 18)  # 连接 BB1 和 AA1（5行 × 18列）

# 构建完整矩阵
Matrix1 <- cbind(AA1, AB1)  # 先拼接第一个量表和连接部分
Matrix2 <- cbind(BA1, BB1)  # 再拼接第二个量表和连接部分
matrix_F <- rbind(Matrix1, Matrix2)  # 按行合并，得到完整矩阵

# 计算桥接网络矩阵（仅保留跨量表的连接）
matrix_bridge <- network$graph - matrix_F

#画图
# 设定 groups（分组）
groups2 <- list(
  "Bridge nodes" = c(5,6,16),  # 你可以调整具体的桥接项
  "Seriousness" = c(1:4),  # 第一个量表的变量（18 道题）
  "Personal Rewards"=(7:15),
  "Social Rewards"=(17:18),
  "Life satisfaction" = c(19)  # 第二个量表的变量（5 道题）
)

# 设定节点名称（需匹配题目数量）
nodeNames1 <- c("Perseverance", "Effort", "Career progress", "Career contingencies", 
                "Identity", "Ethos", "Personal enrichment", "Self-actualization", 
                "Self-express ability", "Self-express individual", "Self-image", 
                "Self-grat-satisfaction", "Self-grat-enjoy", "Re-creation", 
                "Financial return", "Group attraction", "Group accomplishments", 
                "Group maintenance", "Life satisfaction")


# 绘制网络图true
bb<-qgraph(matrix_bridge, 
           negDashed=TRUE,
           posCol="blue",negCol="purple",
           legend=TRUE, 
           layout=photo$layout,  
           legend = TRUE, 
           color=c("#727272","#CCCCCC","white",),
           groups = groups2, 
           vsize=6,
           nodeNames = nodeNames1,
           legend.cex = 0.34)

makeBW(bb)

# 绘制网络图false
bb<-plot(networkZ, 
         negDashed=TRUE,
         posCol="blue",negCol="purple",
         legend=TRUE, 
         layout="spring",  
         legend = TRUE, 
         color=c("#727272","#CCCCCC","white","#999999","#F2F2F2"),
         groups = groups2, 
         vsize=6,
         nodeNames = nodeNames1,
         legend.cex = 0.34)

makeBW(bb)
photo <- plot(networkZ,layout="spring",
              negDashed=TRUE,
              legend = TRUE, 
              borders=TRUE,
              color=c("#CCCCCC","white"), vsize=5.7,
              groups = groups1,  
              nodeNames = nodeNames,
              legend.cex = 0.34)
##黑白打印preview
makeBW(photo)

centralityPlot(network1,include = "all")

boot1<-bootnet_case_dropping<-bootnet(network1,
                                      nBoots = 1000,
                                      type = "case",
                                      nCores = 8)
plot(boot1)


#####?????????||||||||###### 3 dimension
data<-list1[c(1:19)]

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

##使用glasso法进行网络估计
network3<-estimateNetwork(data=network,
                          default = "EBICglasso",
                          tuning=0.5,
                          corMethod="cor",
                          corArgs=list(method="spearman",use="pairwise.complete.obs"))


##开始画图
groupsZ <- list(
  "Seriousness" = 1:6,
  "Personal Rewards"=7:18,
  "Life Satisfaction" = 19)

nodeNames <- c("Perseverance", "Effort", "Career progress", "Career contingencies", 
               "Identity", "Ethos", "Personal enrichment", "Self-actualization", 
               "Self-express ability", "Self-express individual", "Self-image", 
               "Self-gratification-satisfaction", "Self-gratification-enjoy", "Re-creation", 
               "Financial return", "Group attraction", "Group accomplishments", 
               "Group maintenance", "Life satisfaction")

photo3 <- plot(networkZ,layout="spring",
               negDashed=TRUE,
               legend = TRUE, 
               borders=TRUE,
               color=c("#CCCCCC","white","#999999"), vsize=6.1,
               groups = groupsZ,  
               nodeNames = nodeNames,
               legend.cex = 0.33,
               pie=pred$error[,2],
               legend.mode='style1')
##黑白打印preview
makeBW(photo3)

##知道边权重
network3$graph


##flow
photo1 <- plot(network3,layout="spring",
               negDashed=TRUE,
               legend = TRUE, 
               borders=TRUE,
               color=c("#CCCCCC","white","#999999","#595959"), vsize=5,
               groups = groupsZ,  
               nodeNames = nodeNames,
               legend.cex = 0.2,
               pie=pred$error[,2],
               legend.mode='style1')
flow(photo1, "LS", horizontal = TRUE,maxCurve = 2.7)
photoflow<-flow(photo1, "LS", horizontal = TRUE,maxCurve = 2.7)
makeBW(photoflow)


#############

##中心指标图
centralityPlot(network3,include=c("all"))
///or///
  centralityPlot(network3,orderBy="ExpectedInfluence",
                 scale = c("z-scores"),
                 include = c("ExpectedInfluence"))

centralityPlot(network3,orderBy="Strength",
               scale = c("z-scores"),
               include = c("Strength"))

#计算中心指标
centrality_auto(network3)


##桥接
bridge_symp <- bridge(network3$graph, 
                      communities=c('1','1','1','1','1','1',
                                    '2','2','2','2','2','2','2','2','2',
                                    '2','2','2',
                                    '3'))
plot(bridge_symp, 
     order="value",
     zscore=TRUE,
     include=c("Bridge Expected Influence (1-step)"))
