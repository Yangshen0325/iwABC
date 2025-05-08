# iwABC


```
git clone https://github.com/Yangshen0325/iwABC.git
cd iwABC
git checkout hanno
```

## Before we submit any jobs

Do you have `.Rprofile` setup? If not, create one with:

```
echo ".libPaths( c('~/.R/libs', .libPaths()))" >> ~/.Rprofile
```


*Source* the [preparation script](./bash/prep.sh).<br>
We need to do this only once.<br>
R packages will be installed in `~/.R/libs` - available cluster-wide.

```
. ./bash/prep.sh
```
