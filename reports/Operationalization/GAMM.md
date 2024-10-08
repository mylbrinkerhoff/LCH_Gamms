# GAMM analysis

## Purpose
- Purpose of this study is to test to what extent SLZ shows laryngeal complexity. 
- SLZ is a laryngeally complex language that contains:
  - five contrastive tonal patterns
  - four contrastive types of voice quality
- SLZ is an ideal language to test the claims of phasing and ordering made by Silverman 1997a,b and Blankenship 1997, 2002. 
- There have been some more recent studies that have supported this claim about laryngeal complexity in Mazatec (Garellek & Keating 2011) and Itunyoso Trique (DiCanio 2012).
- These previous studies made use of traditional linear mixed effects regression models to show that there is an interaction between tone and laryngealization. 
  - However, this requires the linguist to look at vowel piecemeal/linearly instead of looking at it dynamically. 
- Using GAMMs (Hastie & Tibshirani 1986) allow us to look at these interactions dynamically through the whole length of the vowel instead of linear interactions. 
  - This is much better for investigating measurements with temporal or spatial structure such as formant, pitch or tongue contours.
- In the case of SLZ, this will be helpful for looking at pitch and voice quality interactions. 

## Predictions
- Because SLZ has four phonation types we should expect to see certain interactions between the phonation types and tones. 
- There are two laryngealized phonation types, which are characterized as being creaky voice or having a glottal occlusion. These two phonation types are temporally ordered with respect to one another.  
  - Checked vowels have creakiness or a glottal stop at the end of the vowel. 
  - Rearticulated vowels have creakiness or a glottal stop in the middle of the vowel. 
- It is predicted that tone will be found on the modal portions of the laryngealized vowels
  - Pitch will be perturbed at the position for creakiness/glottal occulusion. 
    - This is because creakiness tyoically involves a greater amount of glottal closure (lower OQ/higher CQ). 
- Breathy vowels are predicted to not show this ordering effect.
  - This is because breathiness is not clearly associated with one position of the vowel
  - Instead we typically see breathiness throughout the whole vowel. 
    -  From data exploration, it does appear like some speakers associate breathiness either at the beginning of the vowel or at the end. 
       -  F3 shows prevocalic breathiness
       -  M4 shows postvocalic breathiness

## Steps and rational
- There are a couple of ways that this can be organized or analyzed. 
    1. I can look at the interaction between f0 and the phonation categories
    2. I can look at the interaction between HNR/CPP and the phonation categories
       - CPP/HNR is a measurement about periodicity.
         - Modal vowels show the greatest amount of periodicity
         - Non-modal vowels show the least amount of periodicty.
       - This would allow me to analyze the Silverman's claim most directly
         - He claims that you see an ordering of modal and non-modal phonation during the production of the vowel. 
       - This is the route that I will be taking. 
       - Observations during data exploration
         - CPP shows breathy vowels being the most periodic, followed closely by modal vowels. 
         - HNR 500Hz shows the expected behavior of all phonation types
           - Breathy is still relatively high but is still lower than modal
         - HNR 1500Hz shows the most potential. 
           - Everything looks right. 
           - Breathy is separated out more fully. 
       - Ran linear regression models over the data from the Residual H1* study and HNR 1500Hz performed the best of the CPP/HNR measures. 
    3. I can look at the interaction between SoE and the phonation categories. 

## Tests
- Model needs to capture:
  1. Fit separate smooths to the phonations
  2. Want to use model comparison and difference smooths to see if they are different
  3. Include certain interaction
    - 
  4. Random smooths needed
     - between Speakers, Word, and Repition of the word
