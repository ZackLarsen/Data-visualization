
perception <-PerceptionExperiment2007.2015Fall
head(perception)
abs_error <- abs(perception$Response-perception$TrueValue)
abs_error
test <- perception$Test
perception$abs_error <- abs_error
test2e <- perception$Test
test2e
subset2e <- subset(perception,Display==1 & Subject >55 & Subject <65 & test2e=='Vertical Distance, Non-Aligned')
subset2e
vert_disp_one <- subset(perception,Display==1 & test2e=='Vertical Distance, Non-Aligned')
vert_disp_one
ggplot(vert_disp_one,aes(x=vert_disp_one$Trial,y=vert_disp_one$Response)) + geom_boxplot() + ggtitle("Problem #2e: looking for outliers")

#Violin
ggplot(vert_disp_one,aes(x=vert_disp_one$Trial,y=vert_disp_one$Response)) + geom_violin(fill = "skyblue", colour = "red") + ggtitle("Problem #2e: looking for outliers") + xlab("Trial") + ylab("Response") + theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),axis.text.y  = element_text(angle=90, vjust=0.5, size=16)) + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),axis.text.x  = element_text(angle=90, vjust=0.5, size=16))  

# Horizontal Violin
ggplot(vert_disp_one,aes(x=vert_disp_one$Trial,y=vert_disp_one$Response)) + geom_violin(fill = "skyblue", colour = "red") + geom_jitter() + ggtitle("Horizontal version of the violin plot for problem #2e") + coord_flip()

#Bean plot
beanplot(vert_disp_one$Response ~ vert_disp_one$Trial, data = vert_disp_one, log="",
         ylab = "Response", 
         border = NA, beanlinewd = 0.5,  overallline = "median", 
         col = list( "brown2", "cadetblue3","green"))

legend("topright", fill = c("brown2", "cadetblue3","green"), c("Trial B", "Trial C","Trial D"))



