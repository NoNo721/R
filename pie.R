library(ggplot2)

#导入数据
mydata <- read.table("data.txt", sep="\t", header=TRUE)

orderSub <- factor(1:length(rownames(mydata)), labels=rownames(mydata))	
#将行名转化为factor类型，且保留原顺序，用于改变图例顺序时使用

mydata <- mydata[order(mydata$Score1, decreasing=TRUE),]
#order函数让数据框数据按Score1列数值降序排列,后面的那个","不能省略！！！

mydata$Sub <- factor(1:length(rownames(mydata)), labels=rownames(mydata))
#将降序排列后的行名再转化为数据框中的factor类型数据，因为作图时时是按此factor顺序作图的，从而以保证降序

print(mydata)

#所占百分比，用于图中数字标签
percentage <- paste(round(mydata$Score1/sum(mydata$Score1)*100, 2), "%", sep=" ")

fig <- ggplot(data=mydata, aes(x="", y=Score1, fill=Sub)) 

fig <- fig + geom_bar(stat="identity", width=1) + coord_polar(theta="y")	#把柱状图折叠成极坐标饼图
#stat="identity": 指定了柱状图的高度(y)，还可以是"count"计数

fig <- fig + labs(title="Title标题", fill="Subject")#图例标题

fig <- fig + scale_fill_discrete(breaks=orderSub)
#breaks可以交换图例小标签顺序，因为图例是fill区分的所以scale后面是fill！！！

#在饼状图上添加数字标签 
fig <- fig + geom_text(aes(label=percentage), size=3,  	 #标签的坐标和颜色
			position=position_stack(vjust=0.5), show.legend=FALSE) #数字标签stack排列，且不显示text标签的图例


fig <- fig + theme(plot.title=element_text(family="STKaiti", hjust=0.5))#标题居中,STKaiti可以显示中文

fig <- fig + theme(title=element_text(size=15, color='black'),
					axis.title.x=element_blank(),	#x轴label字体
					axis.title.y=element_blank(),	#y轴label字体
					axis.ticks=element_blank(),		#坐标轴刻度线长短大小
					axis.text.x=element_blank(),	#x轴标签刻度字体
					axis.text.y=element_blank())	#y轴标签刻度字体

#调整图例
fig <- fig + theme(legend.position="right",	#bottom/top/left
					legend.title=element_text(size=15),
					legend.text=element_text(size=13))

#界面调整
fig <- fig + theme(panel.grid=element_blank(),	#去掉grid
					panel.border=element_blank())	#去掉边界

#导出图像
ggsave("pie.png", dpi=300, width=6, height=6)

























