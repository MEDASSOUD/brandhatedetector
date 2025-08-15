**Brand Hate Detector – Shiny App**

An explainable NLP tool for multidimensional brand hate detection from online consumer reviews

⸻

📌 Overview
The Brand Hate Detector is an open-source R Shiny application that scrapes ConsumerAffairs.com reviews and classifies brand hate into Mild, Moderate, Strong, or Hybrid categories. It is grounded in psychological models (Sternberg’s Triangular Theory of Hate, Kucuk’s typology, Zhang & Laroche’s emotion model) and uses a hybrid lexicon + sentiment approach for interpretable, low-cost, real-time analysis.

⸻

🔍 Key Features
	•	Live scraping of verified ConsumerAffairs reviews
	•	Negative review filtering (sentimentr)
	•	Emotion detection using NRC Emotion Lexicon (anger, sadness, fear, disgust, surprise)
	•	Hybrid classification of hate intensity (rule-based + sentiment)
	•	Visual analytics: bar/pie charts, word clouds, bubble plots
	•	Reason mapping for each hate level

⸻

🛠 Technical Stack
	•	Language: R
	•	Framework: Shiny
	•	Core Packages: shiny, rvest, tidytext, sentimentr, syuzhet, ggplot2, wordcloud, RColorBrewer, stopwords, dplyr, stringr, purrr, tidyr

⸻

▶️ How It Works
	1.	Data Collection – Enter brand URL → scrape metadata + reviews
	2.	Preprocessing – Filter negative sentiment, tokenize, remove stopwords
	3.	Analysis – Detect emotions, classify hate intensity, reclassify hybrids
	4.	Visualization – Generate charts, word clouds, and bubble plots

⸻

📄 License
MIT License – see LICENSE for details.

⸻

⚠️ Copyright & Privacy Notice
This application accesses publicly available reviews from ConsumerAffairs.com for research and demonstration purposes only.
	•	All ConsumerAffairs brand names, trademarks, and review content remain the property of ConsumerAffairs.
	•	Users must comply with ConsumerAffairs’ Terms of Use and Privacy Policy.
	•	No personal identifying information (PII) is stored; all analysis is performed on aggregated, anonymized data.
	•	This tool is not affiliated with or endorsed by ConsumerAffairs.
