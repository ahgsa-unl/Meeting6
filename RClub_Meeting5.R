##R-Club meeting #5

#Agenda and topics
#Graphs: Box plot; Bar plot; and Scatter plot  
#Improve the aesthetics of the plot: X and Y axis-text, legends etc
#Save high-resolution plots

################################################################################
##Box plot
rm(list=ls()) #clears console
ls() #lists items in the console

#Set working directory
setwd("/Users/vikas/R_Club_UNL/05_September27_2016") #Change this to refect the path to your folder

#Install and load libraries required for plotting
#install.packages("reshape")
library(reshape2)
library(ggplot2)

#Read the input file
Correlation_data <- read.table("BoxPlot_data.txt", header=TRUE, sep='\t')

#Understand the structure of the data
head(Correlation_data)
str(Correlation_data)
View(Correlation_data)

#Melt data for box plots
Correlation_data_melt <- melt(Correlation_data, id.vars='Runs')
head(Correlation_data_melt)
View(Correlation_data_melt)

#Calculate the mean for each group
Correlation_data_melt_means<-aggregate(value~variable, Correlation_data_melt, mean)
Correlation_data_melt_means
Correlation_data_melt_means$value <- round(Correlation_data_melt_means$value, digits=3)

##########Box plot##############
ggplot(Correlation_data_melt, aes(x=variable, y=value)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=8, size=2) + #display mean as an asterisk
  geom_text(data=Correlation_data_melt_means, aes(label=value, y=value+0.4), colour="navyblue", size=7) + #display mean on top of box plot
  labs(y="Correlation", x="Groups of lines") + #x and y-axis legends
  theme_bw() + #change background of the plot
  theme(axis.text.x=element_text(colour='black', size=12, face="bold")) + #asthetics of x-axis text
  theme(axis.text.y=element_text(colour='black', size=12, face="bold")) + #asthetics of y-axis text
  theme(axis.title.x = element_text(colour='black', size=15, vjust=0.0, face="bold")) + #asthetics of x-axis title
  theme(axis.title.y = element_text(colour='black', size=15, face="bold")) + #aesthetics of y-axis title
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.3, binwidth=0.025) + #display all data points making up the box plot
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0)) #control the y-axis scale
#Save the box plot
ggsave("Boxplot.pdf", dpi=300, units="in", width=11, height=8.5)
ggsave("Boxplot.tiff", dpi=300, units="in", width=11, height=8.5)

###################################################################################################
#Barplot
#Clear console
rm(list=ls())
ls()

#install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
library(reshape)

display.brewer.all() #Display different color combinations available in R color brewer

#Set working directory
setwd("/Users/vikas/R_Club_UNL/05_September27_2016")

#Barplot - Structure plot
data_k4<-read.table("Barplot_Data.txt", header=TRUE, sep="\t")

View(data_k4)
str(data_k4)

#Melt data - Covert the data from wide-format to long-format
df_k4 <- melt(data_k4,  id.vars="Genotype", variable_name='series')
head(df_k4)

#http://stackoverflow.com/questions/14402242/keep-same-order-as-in-data-files-when-using-ggplot
#Important to match the order in the X-axis
#df_k4<-transform(df_k4, Genotype=factor(Genotype, levels=unique(Genotype)))

#More information on bar graph is available here:
#http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

# Bar plot, each cluster in the population colored differently
ggplot(df_k4, aes(x=Genotype, y=value, fill=series)) + 
  geom_bar(stat="identity", colour="black") + #stat=identity leaves the y values unchanged
  theme_bw() +
  theme(axis.text.x = element_text(colour = 'black', face="bold", angle = 90, size = 16, vjust=0.5, hjust=1)) + #Note the use of vjust and hjust
  theme(axis.text.y = element_text(colour = 'black', face="bold", size = 16)) +
  theme(axis.title.x = element_text(colour = 'black', face="bold", size = 16, vjust=0)) +
  theme(axis.title.y = element_text(colour = 'black', face="bold", size = 16, angle=90)) +
  labs(y="Proportion of membership", x="Genotypes", fill="Subgroups") +
  scale_fill_brewer(palette = "Accent") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) + #Getting rid of the plot background completely
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(size = 16, colour = "black", face="bold")) + 
  theme(legend.title = element_text(size = 16, colour = "black", face="bold")) +
  scale_y_continuous(expand = c(0,0)) + #Deletes the extra space above and below bar plots
  theme(legend.position="bottom") 
#Save the above plot in tiff format and 300 dpi resolution
ggsave("Barplot.tiff", dpi=300, width=11, height=8.5, units="in")

################################################################################
##Scatter plot; PCA
rm(list=ls())
ls()

#install.packages("svglite")
library(svglite) #To save plot in svg format. This format will allow moving overlapping labels in the plot
library(ggplot2)

setwd("/Users/vikas/R_Club_UNL/05_September27_2016")

data=read.table("PCA_data.txt", header=TRUE, sep="\t", row.names=1)
View(data)
head(data)

#Plot using ggplot
ggplot(data, aes(x=PC1, y=PC2, label=row.names(data))) +
  geom_point(size=4, aes(shape=factor(Subgroups))) +
  geom_text(size=7,hjust=0,vjust=-0.6) + #Note the use of vjust to avoid names overlapping on shapes
  scale_shape_discrete(name="Subgroups", labels=c("Subgroup1","Subgroup2","Subgroup3","Subgroup4")) +
  labs(x="PC1 (12.8%)", y="PC2 (10.1%)") +
  theme_classic() + #Makes the plot background blank in this example; will add axis lines later
  theme(axis.line.y=element_line(colour="black",size=1.0,linetype="solid")) + 
  theme(axis.line.x=element_line(colour="black",size=1.0,linetype="solid")) +
  theme(axis.text.x = element_text(colour = 'black', face="bold", size = 25, vjust=0.5,hjust=0.6)) +
  theme(axis.text.y = element_text(colour = 'black', face="bold", size = 25)) +
  theme(axis.title.x = element_text(colour = 'black', face="bold", size = 30, vjust=-0.5)) +
  theme(axis.title.y = element_text(colour = 'black', face="bold", size = 30, angle=90, vjust=1.5)) +
  theme(legend.title=element_text(colour="black",size=23,face="bold")) +
  theme(legend.text=element_text(colour="black",size=18,face="bold")) +
  theme(legend.position="bottom") +
  coord_cartesian(xlim = c(-2, 4)) #Control the x-axis scale
  
#Save the above plot
ggsave("PCA_1.tiff", dpi=300, width=11, height=8.5, units="in")
ggsave("PCA_2.svg", height=8.5, width=11, units="in", dpi=300)
ggsave("PCA_3.svg", height=10.5, width=13, units="in", dpi=300)
#Save the above plot in svg format and 300 dpi resolution
#Input above file in Inkscape and arrange the labels so that they do not overlap.
