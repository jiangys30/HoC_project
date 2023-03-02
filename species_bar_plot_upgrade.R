# 加载需要使用的 R 包
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# 从文件中读入数据
result <- read.table("2b16s_bench_species.txt", sep = "\t", header = T)

# 将数据转换为长格式，以便后续绘图使用
data <- melt(result, id.vars="SampleID")

# 对数据进行排序
data <- data[order(data$variable), ]

#查看共有多少不同的分类单元（genus），便于分配颜色
nc <- length(unique(data$variable))
print(nc)

# 定义一组颜色，用于绘制不同的分类单元，注意“假阳性信号”应位于导入数据的第一列，对应颜色ghostwhite
colors = c("ghostwhite", brewer.pal(4, "Set3"), 
           brewer.pal(8, "Pastel2"), 
           brewer.pal(8, "Pastel1"))

# 创建一个 ggplot 对象，并指定 x 轴、y 轴、以及分类单元（genus）的填充颜色
plot <- ggplot(data=data,aes(SampleID,value,fill=variable)) +
  scale_fill_manual(values = colors) +  # 使用自定义颜色填充分类单元
  # 绘制堆积柱状图，每个柱子表示一个样本，每个分类单元的相对丰度通过柱子高度来表示
  geom_bar(stat="identity",position="stack", color="black", width=0.5,size=0.1)+
  coord_flip() +  # 将 x 轴和 y 轴交换位置
  labs(x = "",y = "Relative Abundance")+  # 设置 x 轴和 y 轴的标签
  scale_y_continuous(expand = c(0,0))+  # 设置 y 轴的范围
  theme_classic()+
  # 设置图形主题，包括填充框、轴线、轴标签、图例等
  theme(panel.background=element_rect(fill="white",colour="black",linewidth=0.25), # 填充框内主题颜色，边框颜色和边框线条粗细
        axis.line=element_line(colour="black",linewidth=0.25), # x,y轴颜色，粗细
        axis.title=element_text(size=16,color="black"), # x,y轴名设置
        axis.text=element_text(size=20,color="black"), # x,y轴文本设置
        legend.title=element_text(size=20),
        legend.text=element_text(size=16),
        legend.position="bottom", legend.box = "horizontal")

# 展示绘制出的图形
plot(plot)

# 将图形保存为 PDF 文件
#ggsave(filename="species_bar_plot.pdf", plot=plot, width=19, height=15)
