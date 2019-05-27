library(ggplot2)
library(reshape2)

#导入数据
mydata <- read.table("data.txt", sep="\t", header=TRUE)

mydata$Sub <- factor(1:length(rownames(mydata)), labels=rownames(mydata))	
#将行名转化为factor类型数据，且保留原顺序
print(levels(mydata$Sub)) #levels函数显示factor的顺序

print(mydata)

#用melt将mydata转换成long型，便于两组数据一起作图
mydata_long <- melt(mydata, id=c("ID","Class","Sub"), variable.name="ScoreNo", value.name="Score")
print(mydata_long)

fig <- ggplot(data=mydata_long, aes(x=Sub, y=Score, fill=ScoreNo))

fig <- fig + geom_bar(stat="identity", position=position_dodge(0.8), width=0.7) #default: width=0.9
#stat="identity": 指定了柱状图的高度(y)，还可以是"count"计数
#position="dodge": 不同颜色的柱子并排放置，且不同颜色柱子之间没有空隙，默认是position="stack"
#position=position_dodge(0.X): 调整不同颜色柱子间的距离，对应也可以用position=position_stack(vjust=0.X)

fig <- fig + scale_y_continuous(breaks=seq(0,150,50), limits=c(0,150))

fig <- fig + labs(title="Title标题", fill="LegendLabel")#图例标题

fig <- fig + scale_fill_discrete(labels=c("label1", "label2"))
#图例小标签，因为图例是fill区分的所以scale后面是fill

#在柱状图上添加数字标签 
fig <- fig + geom_text(aes(x=Sub, y=Score+3, label=Score, color=ScoreNo),  	 #标签的坐标和颜色
				position=position_dodge(0.8), show.legend=FALSE) #同样dodge排列，且不显示text标签的图例

fig <- fig + xlab(label="xlabel") + ylab(label="ylabel")

fig <- fig + theme(plot.title=element_text(family="STKaiti", hjust=0.5))#标题居中,STKaiti可以显示中文

fig <- fig + theme(title=element_text(size=15, color='black'),
					axis.title.x=element_text(size=15),	#x轴label字体
					axis.title.y=element_text(size=15),	#y轴label字体
					axis.text.x=element_text(size=15),	#x轴标签刻度字体
					axis.text.y=element_text(size=15))	#y轴标签刻度字体

#调整图例
fig <- fig + theme(legend.position="right",	#bottom/top/left
					legend.title=element_text(size=15),
					legend.text=element_text(size=13))

#导出图像
ggsave("histogram.png", dpi=300, width=12, height=6)

























