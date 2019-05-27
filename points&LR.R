library(ggplot2)

#导入数据
mydata <- read.table("data.txt", sep="\t", header=TRUE)
print(mydata)

fig <- ggplot(data=mydata) + geom_point(size=5, aes(x=ID, y=Score1, color=Class)) 

#线性回归方程
#==================================================================================

fig <- fig + geom_smooth(lwd=1, method = "lm", se=TRUE, formula = y ~ x, aes(x=ID, y=Score1)) 
#se:在图中显示灰色置信区间，FALSE代表隐藏

lm_eqn <- function(df)
#定义显示回归方程的函数
{
	x <- df$ID;
	y <- df$Score1;
	#确定回归方程的x和y

	m <- lm(y ~ x);

	eq <- substitute(italic(y) == b %.% italic(x)*~ + a~", "~~italic(r)^2~"="~r2, #符号之间用~隔开

                   list(a = as.numeric(format(coef(m)[1]), digits = 2), 

                        b = as.numeric(format(coef(m)[2]), digits = 2), 

                        r2 = format(summary(m)$r.squared, digits = 3)))

	as.character(as.expression(eq));                 
}

#显示回归方程
fig <- fig + geom_text(x = 6, y = 125, label = lm_eqn(mydata), size=6, parse = TRUE)
#x和y为回归方程在图中的坐标

#==================================================================================


#fig <- fig + xlim(0,9) + ylim(80,160)
fig <- fig + scale_x_continuous(breaks=seq(0,10,1), limits=c(1,9))
fig <- fig + scale_y_continuous(breaks=seq(0,200,10), limits=c(60,160))

fig <- fig + labs(title="Title标题", color="LegendLabel")#图例标题

fig <- fig + scale_color_discrete(labels=c("label1","label2"))	
#图例小标签，因为图例是color区分的所以scale后面是color

fig <- fig + xlab(label="xlabel")
fig <- fig + ylab(label="ylabel")

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
ggsave("points&LR.png", dpi=300, width=9, height=6)













