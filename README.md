# COP_Corpus

A corpus of news articles about the COP conferences, with analysis looking at the frequency of words related to the energy trilemma.

The raw files form Lexis Nexis are in `data/LEXIS/textsDiachronic`. 

The file `processing/extractLexisData.R` cleans the texts and places them in `data/LEXIS/DiachronicClean`.

The file `processLexisData.R` extracts the key term frequencies. These are then analysed in `analysis/analyseLexisResults.Rmd`.

The human judgement files are kept in `data/HumanJudgements` and are analysed in `analysis/analyseHumanJudgements.Rmd`.
