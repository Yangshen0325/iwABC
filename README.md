# iwABC


```
git clone https://github.com/Yangshen0325/iwABC.git
cd iwABC
git checkout hanno
```

## Before we submit any jobs

*Source* the [preparation script](./bash/prep.sh).<br>
R packages will be installed in `~/.R/libs` - persistant and available cluster-wide.<br>
We need to do this only once (or when we want to update packages).<br>
Consider to do this on a interactive node since DAISIE tent to install half of CRAN.'<br>
Btw. pulling from 'DAISIE/develop' seems to be a bit brittle...<br>

```
. ./bash/prep.sh    # note the '.' for 'source'
```
