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
hnr15_base <- mgcv::bam(
    formula = hnr15z ~ Phonation, 
    data = slz_low,
    method = "ML"
)
summary(hnr15_base)
AIC(hnr15_base)

# gamm_soe_model
gamm_hnr15 <- mgcv::bam(
    formula = hnr15z ~ Phonation + 
        s(measurement_no) +
        s(measurement_no, by = Phonation, k = 10) +
        s(Speaker.f1, bs = "re") +
        s(Speaker.f1, Phonation, bs = "re"),
    data = slz_low,
    method = "fREML"
)
summary(gamm_hnr15)
mgcv::gam.check(gamm_hnr15)
# Model comparison
# itsadug::compareML(alc_time_phon_speaker6, a1c_time_phon_speaker5)


# Checking for autocorrelation
itsadug::acf_resid(gamm_hnr15)

rho1 <- itsadug::start_value_rho(gamm_hnr15) 

# adding autocorrelation to the model
gamm_hnr15_ar1 <- mgcv::bam(
    formula = hnr15z ~ Phonation + 
        s(measurement_no) +
        s(measurement_no, by = Phonation, k = 10) +
        s(Speaker.f1, bs = "re") +
        s(Speaker.f1, Phonation, bs = "re"),
        rho = rho1,
        AR.start = slz_low$start_event,
    data = slz_low,
    method = "fREML"
)
summary(gamm_hnr15_ar1)
mgcv::gam.check(gamm_hnr15_ar1)
itsadug::acf_resid(gamm_hnr15_ar1)
itsadug::compareML(gamm_hnr15_ar1, gamm_hnr15)

# Plotting the results
hnr15_preds <- gamm_hnr15_ar1  %>% 
    tidygam::predict_gam(length_out = 20, 
        tran_fun = exp, 
        series = "measurement_no",
        exclude_terms = to_exclude) 

figure_hnr15 <- hnr15_preds  %>% 
    ggplot2::ggplot(aes(x = measurement_no, y = hnr15z, color = Phonation, fill = Phonation, group = Phonation, linetype = Phonation)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25, color = NA) +
    geom_line(linewidth = 1.5) +
    scale_colour_manual(values = colorblind) +
    scale_fill_manual(values = colorblind) +
    scale_linetype_manual(values = c(1, 2, 4, 6)) +
    labs(title = "Model fit for HNR < 1500 Hz",
         x = "Measurement Number",
         y = "HNR < 1500 Hz (normalized)") +
    theme(legend.position = "bottom") +
    scale_x_continuous(n.breaks = 10)
figure_hnr15

# Plotting the differences
# Get the smooths differences for each phonation
modal_rearticulated  <- tidymv::get_smooths_difference(gamm_hnr15_ar1, measurement_no, 
    list(Phonation = c("modal", "rearticulated"))) %>% 
    mutate(contrast = "modal-rearticulated")

modal_check <- tidymv::get_smooths_difference(gamm_hnr15_ar1, measurement_no, 
    list(Phonation = c("modal", "checked"))) %>% 
    mutate(contrast = "modal-checked")

modal_breathy  <- tidymv::get_smooths_difference(gamm_hnr15_ar1, measurement_no,
    list(Phonation = c("modal", "breathy"))) %>% 
    mutate(contrast = "modal-breathy")

diff_hnr15  <- dplyr::bind_rows(modal_rearticulated, modal_check, modal_breathy) 

diff_hnr15$contrast  <- factor(diff_hnr15$contrast, levels = c("modal-breathy", "modal-checked", "modal-rearticulated"))

diff_hnr15$sig_diff  <- factor(diff_hnr15$sig_diff, levels = c("TRUE", "FALSE"))

figure_diff_hnr15  <- diff_hnr15 %>%
   ggplot(aes(measurement_no, difference, group = group)) +
   geom_hline(aes(yintercept = 0), colour = "#8f5f3f") +
   geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = sig_diff), alpha = 0.3) +
   geom_line(aes(colour = sig_diff), size = 1) +
   scale_colour_manual(values = c("#e35760","#6f849c")) +
   scale_fill_manual(values = c("#e35760","#6f849c")) +
   labs(colour = "Significant", fill = "Significant") +
   scale_x_continuous(n.breaks = 10) +
   xlab("Measurement number") +
   ylab("\U0394f0") +
   facet_wrap(.~contrast) +
   labs(color = "significant",
        fill = "significant")+
   theme(legend.position="bottom")
figure_diff_hnr15

ggplot2::ggsave(here("figures", "hnr15_model_fit.eps"),
    plot = figure_hnr15,
    device = cairo_ps,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
)

ggplot2::ggsave(here("figures", "hnr15_model_diff.eps"),
    plot = figure_diff_hnr15,
    device = cairo_ps,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
)
