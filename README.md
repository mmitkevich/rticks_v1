# rticks

## Documentation is tests:
- here [tests/](./tests)

## Usage example:

```R
# redefine configuration file url
options(ticks.config.url="~/Exante_R_config.yaml")

# load the library
library(rticks)

# the config automatically was loaded
print(config(db))
print(config(db, password))

# query symbols (with first notice days)
query_symbols("VIX%", limit=5)
```

## Installation:
1. in ssh do the following
```bash
ssh username@quant1....
cd ~
git clone git@bitbucket.org:exante_quant/rticks.git
cd ~/rticks/scripts
./rticks-install.sh
```
2. in R-studio do menu Session-Restart R. In new session type
```
library(devtools)
load_all("~/rticks")
config()
```
