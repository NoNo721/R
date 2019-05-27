library(ggplot2)
library(reshape2)

#导入数据
mydata <- read.table("data.txt", sep="\t", header=TRUE)
print(mydata)

#用melt将mydata转换成long型，便于两组数据一起画图
mydata_long <- melt(mydata, id=c("ID","Class"), variable.name="ScoreNo", value.name="Score")
print(mydata_long)

fig <- ggplot(data=mydata_long, aes(x=ID, y=Score)) 
fig <- fig + geom_point(size=4, aes(color=ScoreNo)) 

#折线图
#fig <- fig + geom_line(lwd=2, aes(color=ScoreNo)) 

#光滑曲线(spline后面的8可调节)
fig <- fig + geom_smooth(lwd=2, se=FALSE, formula = y ~ splines::ns(x,8), method="lm", aes(color=ScoreNo))
#se:在图中显示灰色置信区间，FALSE代表隐藏

#fig <- fig + xlim(0,9) + ylim(80,160)
fig <- fig + scale_x_continuous(breaks=seq(0,10,1))
fig <- fig + scale_y_continuous(breaks=seq(80,150,10))

fig <- fig + labs(title="Title标题", color="LegendLabel") #图例标题

fig <- fig + scale_color_discrete(labels=c("label1","label2"))	
#图例小标签，因为图例是color区分的所以scale后面是color

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
ggsave("lines.png", dpi=300, width=9, height=6)

























