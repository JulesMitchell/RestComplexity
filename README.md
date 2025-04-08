# RestComplexity
Code (Matlab, R, Python) and BRMS supplementary output for analysis of EEG resting state complexity (Lempel-Ziv, Multiscale Entropy) following oral low-dose ketamine treatment, published in: Mitchell, Jules. S., Anijärv, Toomas. E., Can, Adem. T., Dutton, Megan., Hermens, Daniel. F., & Lagopoulos, Jim. (2024; Awaiting Review). Resting State Complexity is Associated with Oral Ketamine Treatment Response: A Bayesian Analysis of Lempel-Ziv Complexity and Multi-Scale Entropy.

Order of code applied:
1. Matlab: EEG recordings (Biosemi) processed with EEGLAB and files exported in .mat, .csv, and .set formats.
2. Python: Processed data (.set files) epoched with MNE, and complexity measures calculated using Neurokit2. Data exported as csv.
3. R: Data analysed using BRMS (STAN backend) mixed effect models.

Note, the code is structured according to the file types specific to our data. These may not match yours, so remember to adjust code accordingly. 
- EEG raw files imported in EEGLAB were .bdf (biosemi) and output files were .set
- EEG files epoched with MNE were imported as .set files, and exported as .fif
- EEG files for complexity calculations were .fif

You are free to use this or any other code from this repository for your own projects and publications. Citation or reference to the repository is not required, but would be much appreciated (see more on README.md).

Any code feedback is welcome. 

Code created by Toomas Erik Anijärv and Jules Mitchell January 2024.
