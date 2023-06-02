# Welcome to the Polkanalyzer R-package

The package contains functions used by the [polkanalyzer-app](https://github.com/filippoweb3/polkanalyzer-app). GitHub Actions use the package functions to daily fetch Polkadot's Nominated-Proof-of-Stake (NPoS) eras data from the [polkadot watcher csv exporter](https://github.com/w3f/polkadot-watcher-csv-exporter) and the [candidates dataset](https://polkadot.w3f.community/candidates), and prepare two datasets that are used by the app:

- `candidates.rda` is a data frame with 16 variables about nodes in the 1KV Programme.
- `eras_data.rda`: is a list of three objects:
  - `eras`: a data frame with 12 variables about all validators in the past eras.
  - `chain`: the chain where eras data come from (only Polkadot supported)
  - `interval`: eras interval

Because the two datasets are merged, only the 1KV nodes are currently used in the Polkanalyzer app.
