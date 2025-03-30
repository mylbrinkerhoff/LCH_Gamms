#----------------------------------------------------------------------------------------
# File: .R
# Project: 
# Author: Mykel Brinkerhoff
# Date: YYYY-MM-DD (M-Su)
# Description: What does this script do?
#
# Usage:
#   Rscript .R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.
#----------------------------------------------------------------------------------------

# load in the dataframe
slz_low <- slz_normalized %>%
  filter(Tone == "L")

# Set start time 
slz_low$start_event <- slz_low$measurement_no == 1

# linear modal with a1c
f0_base <- mgcv::bam(
    formula = f0z ~ Phonation, 
    data = slz_low,
    method = "ML"
)
summary(f0_base)
AIC(f0_base)

# gamm_f0_model
gamm_f0 <- mgcv::bam(
    formula = f0z ~ Phonation + 
        s(measurement_no) +
        s(measurement_no, by = Phonation, k = 10) +
        s(Speaker.f1, bs = "re") +
        s(Speaker.f1, Phonation, bs = "re") +
        te(measurement_no, Iter, by = Phonation, k = c(10,6)),
    data = slz_low,
    method = "fREML"
)
summary(gamm_f0)
mgcv::gam.check(gamm_f0)
# Model comparison
# itsadug::compareML(alc_time_phon_speaker6, a1c_time_phon_speaker5)

# plotting the final model with jeremy steffman's method
modal_breathy <- itsadug::plot_diff(gamm_f0,
    view = "measurement_no",
    comp = list(Phonation = c("modal","breathy")),
    rm.ranef=T
)
modal_checked <- itsadug::plot_diff(gamm_f0,
    view = "measurement_no",
    comp = list(Phonation = c("modal","checked")),
    rm.ranef=T
)
modal_rearticulated <- itsadug::plot_diff(gamm_f0,
    view = "measurement_no",
    comp = list(Phonation = c("modal","rearticulated")),
    rm.ranef=T
)
model_smooth <- itsadug::plot_smooth(gamm_f0,
    view = "measurement_no",
    plot_all = "Phonation",
    rm.ranef=T
)


model_smooth <- model_smooth$fv

# modal_breathy
highlights_mb <- ifelse(modal_breathy$est + modal_breathy$CI < 0 | modal_breathy$est-modal_breathy$CI > 0,1,0)
modal_breathy$inds <- diff(c(0, highlights_mb))
start <- modal_breathy$measurement_no[modal_breathy$inds == 1];start<-start[!is.na(start)]
end <- c(modal_breathy$measurement_no[modal_breathy$inds == -1]);end<-end[!is.na(end)]
if (length(start) > length(end)) end <- c(end, tail(modal_breathy$measurement_no, 1))
rects_modal_breathy <- data.frame(start=start, end=end, group=seq_along(start))

# modal_checked
highlights_mc <- ifelse(modal_checked$est + modal_checked$CI < 0 | modal_checked$est-modal_checked$CI > 0,1,0)
modal_checked$inds <- diff(c(0, highlights_mc))
start <- modal_checked$measurement_no[modal_checked$inds == 1];start<-start[!is.na(start)]
end <- c(modal_checked$measurement_no[modal_checked$inds == -1]);end<-end[!is.na(end)]
if (length(start) > length(end)) end <- c(end, tail(modal_checked$measurement_no, 1))
rects_modal_checked <- data.frame(start=start, end=end, group=seq_along(start))
# modal_rearticulated
highlights_mr <- ifelse(modal_rearticulated$est + modal_rearticulated$CI < 0 | modal_rearticulated$est-modal_rearticulated$CI > 0,1,0)
modal_rearticulated$inds <- diff(c(0, highlights_mr))
start <- modal_rearticulated$measurement_no[modal_rearticulated$inds == 1];start<-start[!is.na(start)]
end <- c(modal_rearticulated$measurement_no[modal_rearticulated$inds == -1]);end<-end[!is.na(end)]
if (length(start) > length(end)) end <- c(end, tail(modal_rearticulated$measurement_no, 1))
rects_modal_rearticulated <- data.frame(start=start, end=end, group=seq_along(start))

cowplot::plot_grid(
model_smooth %>%
    ggplot(aes(x=measurement_no,y = fit, color = Phonation))+ 
    geom_ribbon(aes(ymin = ll,ymax = ul,fill = Phonation),alpha=0.2)+
    # coord_cartesian(ylim = c(-1,2))+
    theme(legend.position = "bottom")+
    ggtitle("Model fit for f0 at Low tone")+
    geom_line(aes(color = Phonation))+xlab("measurement number")+ylab("A1* (z-score)") +
    scale_x_continuous(n.breaks = 10) +
    scale_colour_manual(values = colorblind) + 
    scale_fill_manual(values = colorblind),
modal_breathy %>%
    ggplot(aes(x=measurement_no,y=est))+ geom_hline(yintercept=0,linetype=2,color="gray50")+
    geom_rect(data=rects_modal_breathy, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-10,ymax=10, group=1), color="transparent", fill="red", alpha=0.15)+
    coord_cartesian(ylim=c(-1,1))+
    geom_ribbon(aes(ymin=est-CI,ymax=est+CI,color=NULL),alpha=0.2)+
    geom_line()+
    scale_x_continuous(n.breaks = 10) +
    ggtitle("Difference smooth for modal vs. breathy")+
    xlab("prop. word duration")+ ylab("est. smooth difference"),
modal_checked %>%
    ggplot(aes(x=measurement_no,y=est))+ geom_hline(yintercept=0,linetype=2,color="gray50")+
    geom_rect(data=rects_modal_checked, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-10,ymax=10, group=1), color="transparent", fill="red", alpha=0.15)+
    coord_cartesian(ylim=c(-1,1))+
    geom_ribbon(aes(ymin=est-CI,ymax=est+CI,color=NULL),alpha=0.2)+
    geom_line()+
    scale_x_continuous(n.breaks = 10) +
    ggtitle("Difference smooth for modal vs. checked")+
    xlab("prop. word duration")+ ylab("est. smooth difference"),
modal_rearticulated %>%
    ggplot(aes(x=measurement_no,y=est))+ geom_hline(yintercept=0,linetype=2,color="gray50")+
    geom_rect(data=rects_modal_rearticulated, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-10,ymax=10, group=1), color="transparent", fill="red", alpha=0.15)+
    coord_cartesian(ylim=c(-1,1))+
    geom_ribbon(aes(ymin=est-CI,ymax=est+CI,color=NULL),alpha=0.2)+
    geom_line()+
    scale_x_continuous(n.breaks = 10) +
    ggtitle("Difference smooth for modal vs. rearticulated")+
    xlab("prop. word duration")+ ylab("est. smooth difference"),
    align = "h",
    axis = "tb",
    ncol = 2,
    nrow = 2
)

# Checking for autocorrelation
itsadug::acf_resid(gamm_f0)

rho1 <- itsadug::start_value_rho(gamm_f0) 

# adding autocorrelation to the model
gamm_f0_ar1 <- mgcv::bam(
    formula = f0z ~ Phonation + 
        s(measurement_no) +
        s(measurement_no, by = Phonation, k = 10) +
        s(Speaker.f1, bs = "re") +
        s(Speaker.f1, Phonation, bs = "re") +
        te(measurement_no, Iter, by = Phonation, k = c(10,6)),
        rho = rho1,
        AR.start = slz_low$start_event,
    data = slz_low,
    method = "fREML"
)

summary(gamm_f0_ar1)
mgcv::gam.check(gamm_f0_ar1)
itsadug::acf_resid(gamm_f0_ar1)
itsadug::compareML(gamm_f0_ar1, gamm_f0)

# plotting the final model with jeremy steffman's method
modal_breathy <- itsadug::plot_diff(gamm_f0_ar1,
    view = "measurement_no",
    comp = list(Phonation = c("modal","breathy")),
    rm.ranef=T
)
modal_checked <- itsadug::plot_diff(gamm_f0_ar1,
    view = "measurement_no",
    comp = list(Phonation = c("modal","checked")),
    rm.ranef=T
)
modal_rearticulated <- itsadug::plot_diff(gamm_f0_ar1,
    view = "measurement_no",
    comp = list(Phonation = c("modal","rearticulated")),
    rm.ranef=T
)
model_smooth <- itsadug::plot_smooth(gamm_f0_ar1,
    view = "measurement_no",
    plot_all = "Phonation",
    rm.ranef=T
)

model_smooth <- model_smooth$fv

# modal_breathy
highlights_mb <- ifelse(modal_breathy$est + modal_breathy$CI < 0 | modal_breathy$est-modal_breathy$CI > 0,1,0)
modal_breathy$inds <- diff(c(0, highlights_mb))
start <- modal_breathy$measurement_no[modal_breathy$inds == 1];start<-start[!is.na(start)]
end <- c(modal_breathy$measurement_no[modal_breathy$inds == -1]);end<-end[!is.na(end)]
if (length(start) > length(end)) end <- c(end, tail(modal_breathy$measurement_no, 1))
rects_modal_breathy <- data.frame(start=start, end=end, group=seq_along(start))

# modal_checked
highlights_mc <- ifelse(modal_checked$est + modal_checked$CI < 0 | modal_checked$est-modal_checked$CI > 0,1,0)
modal_checked$inds <- diff(c(0, highlights_mc))
start <- modal_checked$measurement_no[modal_checked$inds == 1];start<-start[!is.na(start)]
end <- c(modal_checked$measurement_no[modal_checked$inds == -1]);end<-end[!is.na(end)]
if (length(start) > length(end)) end <- c(end, tail(modal_checked$measurement_no, 1))
rects_modal_checked <- data.frame(start=start, end=end, group=seq_along(start))
# modal_rearticulated
highlights_mr <- ifelse(modal_rearticulated$est + modal_rearticulated$CI < 0 | modal_rearticulated$est-modal_rearticulated$CI > 0,1,0)
modal_rearticulated$inds <- diff(c(0, highlights_mr))
start <- modal_rearticulated$measurement_no[modal_rearticulated$inds == 1];start<-start[!is.na(start)]
end <- c(modal_rearticulated$measurement_no[modal_rearticulated$inds == -1]);end<-end[!is.na(end)]
if (length(start) > length(end)) end <- c(end, tail(modal_rearticulated$measurement_no, 1))
rects_modal_rearticulated <- data.frame(start=start, end=end, group=seq_along(start))

cowplot::plot_grid(
model_smooth %>%
    ggplot(aes(x=measurement_no,y = fit, color = Phonation))+ 
    geom_ribbon(aes(ymin = ll,ymax = ul,fill = Phonation),alpha=0.2)+
    # coord_cartesian(ylim = c(-1,2))+
    theme(legend.position = "bottom")+
    ggtitle("Model fit for f0")+
    geom_line(aes(color = Phonation))+xlab("measurement number")+ylab("f0 (z-scored)") +
    scale_x_continuous(n.breaks = 10) +
    scale_colour_manual(values = colorblind) + 
    scale_fill_manual(values = colorblind),
modal_breathy %>%
    ggplot(aes(x=measurement_no,y=est))+ geom_hline(yintercept=0,linetype=2,color="gray50")+
    geom_rect(data=rects_modal_breathy, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-10,ymax=10, group=1), color="transparent", fill="red", alpha=0.15)+
    coord_cartesian(ylim=c(-1.5,1.5))+
    geom_ribbon(aes(ymin=est-CI,ymax=est+CI,color=NULL),alpha=0.2)+
    geom_line()+
    scale_x_continuous(n.breaks = 10) +
    ggtitle("Difference smooth for modal vs. breathy")+
    xlab("measurement number")+ ylab("est. smooth difference"),
modal_checked %>%
    ggplot(aes(x=measurement_no,y=est))+ geom_hline(yintercept=0,linetype=2,color="gray50")+
    geom_rect(data=rects_modal_checked, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-10,ymax=10, group=1), color="transparent", fill="red", alpha=0.15)+
    coord_cartesian(ylim=c(-1.5,1.5))+
    geom_ribbon(aes(ymin=est-CI,ymax=est+CI,color=NULL),alpha=0.2)+
    geom_line()+
    scale_x_continuous(n.breaks = 10) +
    ggtitle("Difference smooth for modal vs. checked")+
    xlab("measurement number")+ ylab("est. smooth difference"),
modal_rearticulated %>%
    ggplot(aes(x=measurement_no,y=est))+ geom_hline(yintercept=0,linetype=2,color="gray50")+
    geom_rect(data=rects_modal_rearticulated, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-10,ymax=10, group=1), color="transparent", fill="red", alpha=0.15)+
    coord_cartesian(ylim=c(-1.5,1.5))+
    geom_ribbon(aes(ymin=est-CI,ymax=est+CI,color=NULL),alpha=0.2)+
    geom_line()+
    scale_x_continuous(n.breaks = 10) +
    ggtitle("Difference smooth for modal vs. rearticulated")+
    xlab("measurement number")+ ylab("est. smooth difference"),
    align = "h",
    axis = "tb",
    ncol = 2,
    nrow = 2
)

# plot the model using tidygam
gamm_f0_ar1 %>%
    tidygam::predict_gam() %>%
    plot(series = "measurement_no", comparison = "Phonation") + 
    scale_x_continuous(n.breaks = 10) +
    scale_colour_manual(values = colorblind) + 
    scale_fill_manual(values = colorblind) + 
    labs(title = "Model fit for f0 at Low tone",
        x = "Measurement Number",
        y = "f0 (z-score)")

gamm_f0_ar1 %>%
    tidygam::get_difference(series = "measurement_no", compare = list(Phonation = c("modal", "breathy"))) %>%
    plot(series = "measurement_no") +
    scale_x_continuous(n.breaks = 10)

gamm_f0_ar1 %>%
    tidygam::get_difference(series = "measurement_no", compare = list(Phonation = c("modal", "checked"))) %>%
    plot(series = "measurement_no") +
    scale_x_continuous(n.breaks = 10)

gamm_f0_ar1 %>%
    tidygam::get_difference(series = "measurement_no", compare = list(Phonation = c("modal", "rearticulated"))) %>%
    plot(series = "measurement_no") +
    scale_x_continuous(n.breaks = 10)

gamm_f0_ar1 %>%
    tidygam::get_difference(series = "measurement_no", compare = list(Phonation = c("breathy", "checked"))) %>%
    plot(series = "measurement_no") +
    scale_x_continuous(n.breaks = 10)

gamm_f0_ar1 %>%
    tidygam::get_difference(series = "measurement_no", compare = list(Phonation = c("breathy", "rearticulated"))) %>%
    plot(series = "measurement_no") + 
    scale_x_continuous(n.breaks = 10)
