---
title: "Brand Hate Detector: An R Shiny Application for Automated Detection and Multilevel Classification of Brand Hate in Consumer Reviews"
tags:
  - Brand hate
  - Natural language Processing
  - Shiny App
  - R
  - Open source
authors:
  - name: "Mohamed Assoud"
    orcid: "0000-0003-3425-2639"
    affiliation: 1
affiliations:
  - name: "Hassan First University of Settat, Morocco"
    index: 1
date: "2025-08-22"
---

# Summary

The **Brand Hate Detector** is an open-source R Shiny application designed to identify, classify, and visualize brand hate in online consumer reviews. Unlike traditional sentiment analysis tools that reduce feedback to simple polarity, the Brand Hate Detector captures multiple levels of hostility (*Mild, Moderate, Strong, Hybrid*) and profiles their emotional composition.  

The workflow integrates lexicon-based sentiment scoring, NRC Emotion Lexicon profiling, and a two-stage rule-based classification framework. Results are provided through interactive visualizations, including category distributions, reason-specific breakdowns, word clouds, and sentiment–intensity bubble plots.  

Designed for transparency and reproducibility, the tool bridges academic research and managerial practice by providing actionable insights into consumer hostility toward brands. Its modular design supports extensions to other datasets, languages, and machine-learning-based approaches.

# Statement of Need

Online reviews have become a decisive market force, shaping reputations and influencing consumer behavior (Kucuk, 2020; Parveen, 2025). Negative reviews spread faster and resonate longer than positive ones, often escalating into organized campaigns against brands (Lis & Fischer, 2020).  

Brand hate is more than dissatisfaction: it reflects deep rejection driven by symbolic, ideological, or personal factors (Zhang & Laroche, 2020). Although prior research has mapped antecedents and emotional drivers (Fetscherin, 2019; Hegner et al., 2017), most methods rely on surveys or interviews, which cannot capture hostility in real time or at scale (Assoud & Berbou, 2023).  

Existing computational tools often act as black boxes, lack emotional granularity, or require heavy resources (Mednini et al., 2024), limiting adoption by small firms and researchers. The **Brand Hate Detector** addresses these gaps by offering an open-source, interpretable, and lightweight application. It empowers marketing scholars to explore consumer–brand hostility empirically and enables practitioners to monitor and respond to hate dynamics.

# Implementation

The Brand Hate Detector is implemented in **R Shiny** and consists of five modules:

- **Data Acquisition**: Scrapes brand reviews from ConsumerAffairs.com using the *rvest* package.  
- **Preprocessing**: Filters for negative reviews, removes stopwords and noise, and preserves both raw and cleaned text.  
- **Emotion Profiling**: Uses the NRC Emotion Lexicon to detect anger, sadness, fear, disgust, and surprise.  
- **Classification**: A two-stage framework assigns reviews to *Mild, Moderate, Strong,* or *Hybrid* hate.  
- **Visualization & Reporting**: Interactive dashboards present category distributions, reasons, emotion word clouds, and bubble plots, all exportable.  

[![Analytical pipeline of the Brand Hate Detector](https://raw.githubusercontent.com/MEDASSOUD/brandhatedetector/Figures/Figure1.svg)]
All outputs are downloadable in CSV or publication-quality image formats. The app is platform-independent, requires only R and standard packages, and prioritizes reproducibility through transparent rules and visible mappings.

# Acknowledgements

The development of the Brand Hate Detector benefited from discussions with colleagues in marketing analytics and computational linguistics. The author acknowledges ConsumerAffairs.com as the source of publicly available reviews used in this study.


# References

- Albladi, A., Islam, M., & Seals, C. (2025). Sentiment Analysis of Twitter Data Using NLP Models: A Comprehensive Review. *IEEE Access, 13*, 30444–30468. https://doi.org/10.1109/access.2025.3541494  
- Assoud, M., & Berbou, L. (2023). Brand Hate: One Decade of Research – A Systematic Review. *International Journal of Latest Research in Humanities and Social Science, 6*(11), 146–165. https://doi.org/10.2139/ssrn.5037055  
- Assoud, M., & Berbou, L. (2025a). Conceptualizing Brand Hate Escalation: A Dual SEM-PLS / Necessary Condition Analysis Approach. *African Journal of Human Resources, Marketing and Organisational Studies, 2*(1), 55–81. https://hdl.handle.net/10520/ejc-aa_ajhrmos_v2_n1_a4  
- Assoud, M., & Berbou, L. (2025b). Mapping the Evolution of Brand Hate: A Comprehensive Bibliometric Analysis. *Journal of Consumer Satisfaction, Dissatisfaction and Complaining Behavior, 38*(1), 21–58. https://www.jcsdcb.com/index.php/JCSDCB/article/view/1086  
- Fetscherin, M. (2019). The five types of brand hate: How they affect consumer behavior. *Journal of Business Research, 101*, 116–127. https://doi.org/10.1016/j.jbusres.2019.04.017  
- Hegner, S. M., Fetscherin, M., & van Delzen, M. (2017). Determinants and outcomes of brand hate. *Journal of Product & Brand Management, 26*(1), 13–25. https://doi.org/10.1108/JPBM-01-2016-1070  
- Kang, Y., Cai, Z., Tan, C.-W., Huang, Q., & Liu, H. (2020). Natural language processing (NLP) in management research: A literature review. *Journal of Management Analytics, 7*(2), 139–172. https://doi.org/10.1080/23270012.2020.1756939  
- Kucuk, S. U. (2019). Consumer Brand Hate: Steam rolling whatever I see. *Psychology and Marketing, 36*(5), 431–443. https://doi.org/10.1002/mar.21175  
- Kucuk, S. U. (2020). *Consumer Voice: The Democratization of Consumption Markets in the Digital Age*. Springer. https://doi.org/10.1007/978-3-030-53983-2  
- Lis, B., & Fischer, M. (2020). Analyzing different types of negative online consumer reviews. *Journal of Product & Brand Management, 29*(5), 637–653. https://doi.org/10.1108/jpbm-05-2018-1876  
- Mednini, L., Noubigh, Z., & Turki, M. D. (2024). Natural Language Processing for Detecting Brand Hate Speech. *Journal of Telecommunications and the Digital Economy, 12*(1), 486–509. https://doi.org/10.18080/jtde.v12n1.859  
- Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a word–emotion association lexicon. *Computational Intelligence, 29*(3), 436–465. https://doi.org/10.1111/j.1467-8640.2012.00460.x  
- Mushtaq, F. M., Ghazali, E. M., & Hamzah, Z. L. (2024). Brand hate: a systematic literature review and future perspectives. *Management Review Quarterly*, 1–34. https://doi.org/10.1007/s11301-023-00402-z  
- Parveen, U. (2025). An Analysis of Linguistics Patterns in Online Product or Service Reviews and their Influence on Customer Behavior. *Qlantic Journal of Social Sciences and Humanities, 6*(2), 111–117. https://doi.org/10.55737/qjssh.vi-ii.25358  
- Rinker, T. (2017). Package `sentimentr`. Retrieved August 31, 2017.  
- Sternberg, R. J., & Sternberg, K. (2008). *The Nature of Hate*. Cambridge University Press.  
- Wickham, H., & Bryan, J. (2023). *R packages*. O’Reilly Media.  
- Yadav, A., & Chakrabarti, S. (2022). Brand hate: A systematic literature review and future research agenda. *International Journal of Consumer Studies, 46*(5), 1992–2019. https://doi.org/10.1111/ijcs.12772  
- Yu, J. H., & Chauhan, D. (2025). Trends in NLP for personalized learning: LDA and sentiment analysis insights. *Education and Information Technologies, 30*(4), 4307–4348. https://doi.org/10.1007/s10639-024-12988-2  
- Zarantonello, L., Romani, S., Grappi, S., & Bagozzi, R. P. (2016). Brand hate. *Journal of Product & Brand Management, 25*(1), 11–25. https://doi.org/10.1108/jpbm-01-2015-0799  
- Zhang, C., & Laroche, M. (2020). Brand hate: A multidimensional construct. *Journal of Product & Brand Management, 30*(3), 392–414. https://doi.org/10.1108/JPBM-11-2018-2103  
- Zhang, Q., Su, L., Zhou, L., & Dai, Y. (2025). Distance Brings About Beauty: When Does the Influence of Positive Travel Online Reviews Grow Stronger Relative to Negative Reviews? *Journal of Travel Research, 64*(1), 172–188. https://doi.org/10.1177/00472875231209426  
