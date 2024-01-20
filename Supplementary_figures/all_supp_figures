# Load_plotting_package ---------------------------------------------------


rm(list = ls())

require(dplyr)
require(lubridate)
require(ggplot2)


# Fig_1 -------------------------------------------------------------------


dat = read.csv("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS1_source_data.csv")
#dat$OR = exp(dat$coef * 10)
#dat$upper = exp((dat$coef + 1.96*dat$se) * 10)
#dat$lower = exp((dat$coef - 1.96*dat$se) * 10)
#dat$group = factor(dat$group,levels = c("singe_pollutant","dual_pollutant"), labels = c("Model 1", "Model 2"))



dat$eth_group = factor(dat$eth_group,levels = c("White","Black","Asian","Others", "Hispanic","Non-Hispanic"))
#dat$age_group = factor(dat$age_group, levels = c("5-17","18-64","65+","all"), labels = c("5-17","18-64","65+","All"))
dat$sex = factor(dat$sex,levels = c("All","Males","Females"),labels = c("All","Males","Females"))

#write.csv(dat,"H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS1_source_data.csv")

pd = position_dodge(0.5)

plot1 = dat %>% ggplot(., aes(x = eth_group, y = OR, color = group, group = group)) + 
  geom_abline(slope = 0, intercept =1,linetype = "dashed") + 
  geom_point(position = pd) +
  geom_errorbar(aes(x = eth_group, ymax = CI_upper,ymin = CI_lower),position = pd) +
  facet_wrap(sex~.) + 
  xlab("Age groups") + ylab(paste0("OR (95% CI)")) + 
  #scale_color_manual(values = c("steelblue","orange")) 
  theme_bw() + scale_y_continuous(breaks = c(0.96,0.98,1.00,1.02,1.04)) +
  theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) +
  theme(axis.title.x = element_text(size = 13, face = "bold"))  +
  theme(axis.title.y = element_text(size = 11, face = "bold"))  +
  theme(axis.text.x = element_text(angle = 45, hjust=0.9, size = 12)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("steelblue","orange"))
plot1



# FigS2 --------------------------------------------------------------------


dat =  read.csv("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS2_source_data.csv")

#dat$group = factor(dat$group,levels = c("singe_pollutant","dual_pollutant"), labels = c("Model 1", "Model 2"))
#dat$cat = factor(dat$cat, levels = c("Smoke events",'Major smoke events'), labels = c("Smoke events","Major smoke events"))


#dat$eth_group = factor(dat$eth_group,levels = c("White","Black","Asian","Others", "Hispanic","Non-Hispanic"))
dat$age_group = factor(dat$age_group, levels = c("5-17","18-64","65+","all"), labels = c("5-17","18-64","65+","All"))
dat$sex = factor(dat$sex,levels = c("All","Males","Females"),labels = c("All","Males","Females"))

#write.csv(dat,"H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS2_source_data.csv")

pd = position_dodge(0.5)

plot2 = dat %>% ggplot(., aes(x = age_group, y = OR, color = group, group = group)) + 
  geom_abline(slope = 0, intercept =1,linetype = "dashed") + 
  geom_point(position = pd) +
  geom_errorbar(aes(x = age_group, ymax = CI_upper,ymin = CI_lower),position = pd) +
  facet_wrap(sex~.) + 
  xlab("Age groups") + ylab(paste0("OR (95% CI)")) + 
  #scale_color_manual(values = c("steelblue","orange")) 
  theme_bw() + #scale_y_continuous(breaks = c(0.96,0.98,1.00,1.02,1.04)) +
  theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) +
  theme(axis.title.x = element_text(size = 13, face = "bold"))  +
  theme(axis.title.y = element_text(size = 11, face = "bold"))  +
  theme(axis.text.x = element_text(angle = 45, hjust=0.9, size = 12)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(strip.text.y = element_text(size = 10, face = "bold")) + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("steelblue","orange"))
plot2

#ggsave("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/final_figures/fig2.pdf")

#write.csv(dat,"H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Main/fig2_source_data.csv")



# FigS3 --------------------------------------------------------------------

dat = read.csv("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS3_source_data.csv")
dat$lag = factor(dat$lag, levels = c("24h","48h","72h","96h","120h"))

pd = position_dodge(0.5)

plot3 = dat %>% ggplot(., aes(x = lag, y = OR, color = group, group = group)) + 
  geom_abline(slope = 0, intercept =1,linetype = "dashed") + 
  geom_point(position = pd) +
  geom_errorbar(aes(x = lag, ymax = CI_upper,ymin = CI_lower),position = pd) +
  xlab("Exposure windows") + ylab(paste0("OR (95% CI)")) + 
  #scale_color_manual(values = c("steelblue","orange")) 
  theme_bw() + #scale_y_continuous(breaks = c(0.96,0.98,1.00,1.02,1.04)) +
  theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) +
  theme(axis.title.x = element_text(size = 13, face = "bold"))  +
  theme(axis.title.y = element_text(size = 11, face = "bold"))  +
  theme(axis.text.x = element_text(angle = 45, hjust=0.9, size = 12)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(strip.text.y = element_text(size = 10, face = "bold")) + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("steelblue","orange"))
plot3

#ggsave("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/final_figures/fig3.pdf")



# FigS4 -------------------------------------------------------------------

dat = read.csv("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS4_source_data.csv")
dat$df.temp = factor(dat$df.temp)

pd = position_dodge(0.5)

plot4 = dat %>% ggplot(., aes(x = df.temp, y = OR, color = group, group = group)) + 
  geom_abline(slope = 0, intercept =1,linetype = "dashed") + 
  geom_point(position = pd) +
  geom_errorbar(aes(x = df.temp, ymax = CI_upper,ymin = CI_lower),position = pd) +
  xlab("Degrees of freedom (temperature") + ylab(paste0("OR (95% CI)")) + 
  #scale_color_manual(values = c("steelblue","orange")) 
  theme_bw() + #scale_y_continuous(breaks = c(0.96,0.98,1.00,1.02,1.04)) +
  theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) +
  theme(axis.title.x = element_text(size = 13, face = "bold"))  +
  theme(axis.title.y = element_text(size = 11, face = "bold"))  +
  theme(axis.text.x = element_text(angle = 45, hjust=0.9, size = 12)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(strip.text.y = element_text(size = 10, face = "bold")) + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("steelblue","orange"))
plot4


# FigS5 -------------------------------------------------------------------

dat = read.csv("H:/aim3/Paper/submission_nat_mental_health/revision_3_AIP/figure_source_data/Supplemental/FigS5_source_data.csv")
dat$df.RH = factor(dat$df.RH)

pd = position_dodge(0.5)

plot5 = dat %>% ggplot(., aes(x = df.RH, y = OR, color = group, group = group)) + 
  geom_abline(slope = 0, intercept =1,linetype = "dashed") + 
  geom_point(position = pd) +
  geom_errorbar(aes(x = df.RH, ymax = CI_upper,ymin = CI_lower),position = pd) +
  xlab("Degrees of freedom (RH)") + ylab(paste0("OR (95% CI)")) + 
  #scale_color_manual(values = c("steelblue","orange")) 
  theme_bw() + #scale_y_continuous(breaks = c(0.96,0.98,1.00,1.02,1.04)) +
  theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) +
  theme(axis.title.x = element_text(size = 13, face = "bold"))  +
  theme(axis.title.y = element_text(size = 11, face = "bold"))  +
  theme(axis.text.x = element_text(angle = 45, hjust=0.9, size = 12)) + 
  theme(axis.text.y = element_text(size = 10)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(strip.text.y = element_text(size = 10, face = "bold")) + 
  theme(legend.title=element_blank()) + 
  scale_color_manual(values = c("steelblue","orange"))
plot5
