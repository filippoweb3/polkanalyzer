# Welcome to the Polkanalyzer Tool

Polkanalyzer is a tool that allows to easily visualize [Polkadot](https://polkadot.network/)'s NPoS data, and make a fair selection of validator nodes. It is a [staking](https://wiki.polkadot.network/docs/learn-staking) tool for [nominaotrs](https://wiki.polkadot.network/docs/learn-nominator) and [nomination pools](https://wiki.polkadot.network/docs/learn-nomination-pools) admins.

## Installation

Polkanalyzer is a shiny app written in [R](https://posit.co/download/rstudio-desktop/). In the back-end of the app there is an R package containing all the functions necessary to analyse the data and select the validators.

Before installing R, open you terminal and run:

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

This will install [Homebrew](https://brew.sh/) on you laptop. Now we can use Homebrew to install R as follows:

```
brew install r
```




