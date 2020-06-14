
###Would Recommed loading these libraries  by default for every project## 

library(tidyverse)
library(ggpmisc)
library(car)
library(FSA)
library(MASS)
library(gmodels)

###Tery likes these libraries but they annoy the hell out of me - you need to specify a blocking factor . 
library(lme4)
library(lmerTest)
library(emmeans)

##Other random Libraries###
library(effects)


library(WRS2)

###### read and set up data ######
exp1 <- read_csv("/media/sf_vmshare/Forebrain_R/MicroCT_HGJ_2019_fb_vol_lum_fixed.csv", 
                 col_types = cols(fb_cube_normalised = col_number(), 
                                  lum_cube_norm = col_number(), lum_norm_ew = col_number()))

exp1 <- exp1 %>% mutate(Group = paste(Genotype,Water.supply), lum_norm_double_otsu = lum_norm_otsu/otsu_wev) 


exp1$Dam<-factor(exp1$Dam)
exp1$Water.supply<-factor(exp1$Water.supply, levels=c("Normal","Ethanol"))

exp1 <- arrange(transform(exp1, Genotype=factor(Genotype,levels=c("WT","HET"))) )

##### set up some cool functions ####
variable_names <- list(
  "WT" = expression(bolditalic("Zic2")^bolditalic("+/+")),
  "HET" = expression(bolditalic("Zic2")^bolditalic("Ku/+"))
)

group_names <- list(
  "HET Ethanol" = expression(bolditalic("Zic2")^bolditalic("Ku/+")|"Ethanol" ),
  "HET Normal" = expression(bolditalic("Zic2")^bolditalic("Ku/+")|"Vehicle" ),
  "WT Ethanol" = expression(bolditalic("Zic2")^bolditalic("+/+")|"Ethanol"),
   "WT Normal" = expression(bolditalic("Zic2")^bolditalic("+/+")|"Vehicle") )

variable_labeller <- function(variable,value){return(variable_names[value])}

group_labeller <- function(variable,value){return(group_names[value])}


terry_group_names <- list(
  "HET.Ethanol" = expression(bolditalic("Zic2")^bolditalic("Ku/+")|"Ethanol" ),
  "HET.Normal" = expression(bolditalic("Zic2")^bolditalic("Ku/+")|"Vehicle" ),
  "WT.Ethanol" = expression(bolditalic("Zic2")^bolditalic("+/+")|"Ethanol"),
  "WT.Normal" = expression(bolditalic("Zic2")^bolditalic("+/+")|"Vehicle") )

terry_labeller <- function(variable,value){return(terry_group_names[value])}
##Summary of Data###
Summarize(lum_norm_otsu~Genotype+Water.supply,
          data=exp1,
          digits=4)

###Some Cool Plots###
ggplot(data=subset(exp1,!(is.na(Shape_forebrain_lobes))),aes(otsu_wev,lum_norm_otsu,colour=Shape_forebrain_lobes))+
  geom_point(position=position_dodge(width=0.8))+
  facet_wrap(~Group, labeller = group_labeller)+
  theme(axis.text.x = element_text(angle = 0))+
  ylab(expression("Volume of Lumen Normalised to Whole Embryo Volume" ))+
  xlab(expression("Whole Embryo Volume (mm)"^"3" ))+
  labs(fill = "Shape of Third Ventricles")

ggplot(exp1,aes(Group,lum_norm_otsu,colour=Water.supply))+
  geom_boxplot(position=position_dodge(width=10))+
  facet_wrap(~Genotype, labeller = variable_labeller, scales = "free_x")+
  scale_color_manual(name="Treatment", labels=c("Vehicle","Ethanol"), values=c("#00BFC4", "#F8766D"))+
  theme(axis.title.x=element_blank(), legend.position = "bottom", legend.text = element_text(size = 10))+
  ylab("Volume of Lumen Normalised to Whole Embryo Volume")+
  scale_x_discrete(labels= unlist(group_names))
  

###Main Plot##
ggplot(exp1,aes(otsu_wev,lum_norm_otsu,colour=Water.supply), by = Group)+
  geom_point()+
  geom_smooth(method ="lm")+
  facet_wrap(~Genotype, labeller = variable_labeller)+ 
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom", legend.text = element_text(size = 10))+
  scale_color_manual(name="Treatment", labels=c("Vehicle","Ethanol"), values=c("#00BFC4", "#F8766D"))+
  ylab("Volume of Lumen Normalised to Whole Embryo Volume")+
  xlab(expression("Whole Embryo Volume (mm)"^"3" ))+
  stat_poly_eq(formula = y~x,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste( ..rr.label.., sep = "~~~")), 
               parse = TRUE)


#33 Double WEV division
ggplot(exp1,aes(otsu_wev,lum_norm_double_otsu,colour=Water.supply), by = Group)+
  geom_point()+
  geom_smooth(method ="lm")+
  facet_wrap(~Genotype, labeller = variable_labeller)+ 
  theme(axis.text.x = element_text(angle = 0))+
  scale_color_manual(name="Treatment", labels=c("Vehicle","Ethanol"), values=c("#00BFC4", "#F8766D"))+
  ylab("Volume of Lumen Normalised to Whole Embryo Volume")+
  xlab(expression("Whole Embryo Volume (mm)"^"3" ))+
  stat_poly_eq(formula = y~x,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste( ..rr.label.., sep = "~~~")), 
               parse = TRUE)



ggplot(exp1,aes(Dam,lum_norm_otsu,colour=Water.supply), by = Group)+
  geom_point()+
  facet_wrap(~Group, labeller = group_labeller, scales = "free_x")+ 
  theme(axis.text.x = element_text(angle = 45))+
  scale_color_manual(name="Treatment", labels=c("Vehicle","Ethanol"), values=c("#00BFC4", "#F8766D"))+
  ylab(expression("Volume of Lumen Normalised to Whole Embryo Volume" ))+
  xlab(expression("Dam ID" ))


##Hannah's Significant plot###

ggplot(exp1.shape, aes(x=interaction(Genotype,Water.supply), y=percent,fill=Shape_forebrain_lobes))+ 
  geom_bar(stat="identity")+ 
  ylab(expression("Pecentage" ))+
  labs(fill = "Shape of Forebrain Lobes")+
  scale_x_discrete(labels = unlist(terry_group_names))
  
  
  
  

############Statistics###############


#First ANOVA  that brakes due to non-normal residuals
bad_anova<-lm(lum_norm_otsu~Genotype+Water.supply+Water.supply*Genotype+otsu_wev, data = exp1)

#Diagnostic plots for that ANOVA - This ANOVA is bad as the residuals are not normally distributed which breaks the ANOVA (look at the Q-Q Quantile plot)
plot(bad_anova)


#####Option 1 to fix data - use whole embryo volumes as a co-variate instead of a blocking factor#####

##Note: If you specify an interaction effect (i.e Water.supply*Genotype), using +other_variable will remove any varation explained by other_variable 
# while (1|other_varaible) will use it as a co-variate

real_effect<-lm(lum_norm_otsu~Water.supply*Genotype+(1|otsu_wev), data = exp1)

anova(real_effect)


model = lm(lum_norm_otsu~Genotype*Water.supply+(1|otsu_wev),
            data=exp1,
            method="REML", 
            na.action=na.omit)
#The Q-Q plot is now much better

plot(model)
Anova(model)


###Option 2 - Perform a BoxCox transformation of the data - taking from unused code within LAMA for single organ analysis####
# A boxcox is a transformation used to fix non-normally distributed data by finding an optimal parameter (lamba) and multiplying the volumes to the power of lambda


Box <- boxcox(exp1$lum_norm_otsu ~ exp1$Genotype+exp1$Water.supply+exp1$Genotype*exp1$Water.supply+exp1$otsu_wev, plotit = TRUE, lambda = seq(-2, 2, len = 1000))


   Cox = data.frame(Box$x, Box$y)
   CoxSorted = Cox[with(Cox, order(-Cox$Box.y)),]
   lambda = CoxSorted[1, "Box.x"]
   tformed <- bcPower(exp1$lum_norm_otsu, lambda)
  
   exp1 <- exp1 %>% mutate(tformed = tformed)
##This is the data after the transformation, looks pretty similar   
   ggplot(exp1,aes(otsu_wev,tformed,colour=Water.supply), by = Group)+
     geom_point()+
     geom_smooth(method ="lm")+
     facet_wrap(~Genotype, labeller = variable_labeller)+ 
     theme(axis.text.x = element_text(angle = 90))+
     scale_color_manual(name="Treatment", labels=c("Vehicle","Ethanol"), values=c("#00BFC4", "#F8766D"))+
     ylab(expression("Normalised Lumen Volumes after BoxCox Transformation"))+
     xlab(expression("Whole Embryo Volume (mm)"^"3" ))

   fit <- lm(tformed ~ exp1$Genotype+exp1$Water.supply+exp1$Genotype*exp1$Water.supply)

   
  
   plot(fit)
   
   summary(fit)
   
   Anova(fit)
   
   

#####BoxCox for lm
Box2 <- boxcox(exp1$lum_norm_otsu ~ exp1$Genotype+exp1$Water.supply+exp1$Genotype*exp1$Water.supply, plotit = TRUE, lambda = seq(-2, 2, len = 1000))
   
Cox = data.frame(Box2$x, Box2$y)
CoxSorted = Cox[with(Cox, order(-Cox$Box2.y)),]
lambda = CoxSorted[1, "Box.x"]
tformed2 <- bcPower(exp1$lum_norm_otsu, lambda)
   
exp1 <- exp1 %>% mutate(tformed2 = tformed2)

fit <- lm(tformed2 ~ exp1$Genotype+exp1$Water.supply+exp1$Genotype*exp1$Water.supply)

ggplot(exp1,aes(otsu_wev,tformed2,colour=Water.supply), by = Group)+
  geom_point()+
  geom_smooth(method ="lm")+
  facet_wrap(~Genotype, labeller = variable_labeller)+ 
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(name="Treatment", labels=c("Vehicle","Ethanol"), values=c("#00BFC4", "#F8766D"))+
  ylab(expression("Normalised Lumen Volumes after BoxCox Transformation"))+
  xlab(expression("Whole Embryo Volume (mm)"^"3" ))+
  stat_poly_eq(formula = y~x,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste( ..rr.label.., sep = "~~~")), 
               parse = TRUE)

plot(fit)

summary(fit)

Anova(fit)

   
   
####Other stats that can be ignored###
library(nlme)
model = lme(lum_norm_otsu~Genotype*Water.supply, random=~1|otsu_wev,
             data=exp1,
             method="REML", 
             na.action=na.omit)
plot(model)
Anova(model)

anova(model)



#Just a quick check test to see if the variance is equal between genotype and treatment: 
var.test(exp1$lum_norm_otsu~exp1$Genotype, exp1, alternative = "two.sided")

var.test(exp1$lum_norm_otsu~exp1$Water.supply, exp1, alternative = "two.sided")






exp1.shape<-exp1%>% group_by(Genotype, Water.supply)%>% count(Shape_forebrain_lobes)



exp1.shape <-na.omit(exp1.shape)%>% group_by(Genotype,Water.supply)%>% mutate(sum=sum(n), percent=n/sum*100)





