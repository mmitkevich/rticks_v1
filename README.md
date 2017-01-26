# rticks

Documentation is tests:
- here [tests/](./tests)

Usage example:

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



