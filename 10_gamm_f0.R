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
slz_normalized$start_event <- slz_normalized$measurement_no == 1


# linear modal with a1c
f0_base <- mgcv::bam(
    formula = f0z ~ Phonation, 
    data = slz_normalized,
    method = "fREML"
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
    data = slz_normalized,
    method = "fREML"
)
summary(gamm_f0)
mgcv::gam.check(gamm_f0)

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
        AR.start = slz_normalized$start_event,
    data = slz_normalized,
    method = "fREML"
)

summary(gamm_f0_ar1)
mgcv::gam.check(gamm_f0_ar1)
itsadug::acf_resid(gamm_f0_ar1)
itsadug::compareML(gamm_f0_ar1, gamm_f0)

# Plotting the results
f0_preds <- gamm_f0_ar1  %>% 
    tidygam::predict_gam(length_out = 20, 
        tran_fun = exp, 
        series = "measurement_no",
        exclude_terms = to_exclude) 

figure_f0 <- f0_preds  %>% 
    ggplot2::ggplot(aes(x = measurement_no, y = f0z, color = Phonation, fill = Phonation, group = Phonation, linetype = Phonation)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25, color = NA) +
    geom_line(linewidth = 1.5) +
    scale_colour_manual(values = colorblind) +
    scale_fill_manual(values = colorblind) +
    scale_linetype_manual(values = c(1, 2, 4, 6)) +
    labs(title = "Model fit for f0",
         x = "Measurement Number",
         y = "f0 (normalized)") +
    theme(legend.position = "bottom") +
    scale_x_continuous(n.breaks = 10)
figure_f0


modal_rearticulated  <- tidymv::get_smooths_difference(gamm_f0_ar1, measurement_no, 
    list(Phonation = c("modal", "rearticulated"))) %>% 
    mutate(contrast = "modal-rearticulated")

modal_check <- tidymv::get_smooths_difference(gamm_f0_ar1, measurement_no, 
    list(Phonation = c("modal", "checked"))) %>% 
    mutate(contrast = "modal-checked")

modal_breathy  <- tidymv::get_smooths_difference(gamm_f0_ar1, measurement_no,
    list(Phonation = c("modal", "breathy"))) %>% 
    mutate(contrast = "modal-breathy")

diff_f0  <- dplyr::bind_rows(modal_rearticulated, modal_check, modal_breathy) 

diff_f0$contrast  <- factor(diff_f0$contrast, levels = c("modal-breathy", "modal-checked", "modal-rearticulated"))

diff_f0$sig_diff  <- factor(diff_f0$sig_diff, levels = c("TRUE", "FALSE"))

figure_diff_f0  <- diff_f0 %>%
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
figure_diff_f0

ggplot2::ggsave(here("figures", "f0_model_fit.eps"),
    plot = figure_f0,
    device = cairo_ps,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
)

ggplot2::ggsave(here("figures", "f0_model_diff.eps"),
    plot = figure_diff_f0,
    device = cairo_ps,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
)
