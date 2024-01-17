dat = read.csv("H:/aim3/plot_data/smk_event48h.csv")
dat$cat = "Smoke events"
dat2 = read.csv("H:/aim3/plot_data/smk_mjevent48h.csv")
dat2$cat = "Major smoke events"
dat = rbind(dat,dat2)
dat$OR = exp(dat$coef)
dat$upper = exp((dat$coef + 1.96*dat$se))
dat$lower = exp((dat$coef - 1.96*dat$se))
dat$group = factor(dat$group,levels = c("singe_pollutant","dual_pollutant"), labels = c("Model 1", "Model 2"))
dat$cat = factor(dat$cat, levels = c("Smoke events",'Major smoke events'), labels = c("Smoke events","Major smoke events"))


#dat$eth_group = factor(dat$eth_group,levels = c("White","Black","Asian","Others", "Hispanic","Non-Hispanic"))
dat$age_group = factor(dat$age_group, levels = c("5-17","18-64","65+","all"), labels = c("5-17","18-64","65+","All"))
dat$sex = factor(dat$sex,levels = c("all","male","female"),labels = c("All","Males","Females"))

pd = position_dodge(0.5)

plot2 = dat %>% ggplot(., aes(x = age_group, y = OR, color = group, group = group)) + 
  geom_abline(slope = 0, intercept =1,linetype = "dashed") + 
  geom_point(position = pd) +
  geom_errorbar(aes(x = age_group, ymax = upper,ymin = lower),position = pd) +
  facet_grid(cat~sex, scales="free_y") + 
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
