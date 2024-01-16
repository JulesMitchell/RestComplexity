# RestComplexity
Code (Matlab, R, Python) and BRMS supplementary output for analysis of EEG resting state complexity (Lempel-Ziv, Multiscale Entropy) following oral low-dose ketamine treatment, published in: Mitchell, Jules. S., Anijärv, Toomas. E., Can, Adem. T., Dutton, Megan., Hermens, Daniel. F., & Lagopoulos, Jim. (2024; Awaiting Review). Resting State Complexity is Associated with Oral Ketamine Treatment Response: A Bayesian Analysis of Lempel-Ziv Complexity and Multi-Scale Entropy.

Order of code applied:

1. Matlab: EEG recordings processed with EEGLAB and files exported in .mat, .csv, and .set formats.
2. Python: Processed data (.set files) epoched with MNE, and complexity measures calculated using Neurokit2.Data exported as csv.
3. R: Data analysed using BRMS (STAN backend) mixed effect models.