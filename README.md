# COP Corpus

The doi of this data repository is: 10.5281/zenodo.16994414

[![DOI](https://zenodo.org/badge/566725517.svg)](https://doi.org/10.5281/zenodo.16994413)

A corpus of news articles about the COP conferences, with analysis looking at the frequency of words related to the energy trilemma.

This is the data for the following publication:

Roberts, S.G., Krykoniuk, K., Handford, M., Zhou, Y., Wu, J. and Chen, C.F., 2025. The energy trilemma COP-out: accessibility is under-reported in international English-language media coverage of United Nations Climate Change Conferences. Energy Research & Social Science, 127, p.104275.

[Link to paper](https://www.sciencedirect.com/science/article/pii/S2214629625003561).

The raw files form Lexis Nexis are in `data/LEXIS/textsDiachronic`. 

The file `processing/extractLexisData.R` cleans the texts and places them in `data/LEXIS/DiachronicClean`.

The file `processLexisData.R` extracts the key term frequencies. These are then analysed in `analysis/analyseLexisResults.Rmd`.

The human judgement files are kept in `data/HumanJudgements` and are analysed in `analysis/analyseHumanJudgements.Rmd`.
