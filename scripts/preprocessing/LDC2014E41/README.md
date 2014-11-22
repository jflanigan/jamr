`patch.sh` patches the LDC2014E41 data to fix broken AMRs (duplicate variable names).  The following
changes are made:

```
# ::id PROXY_AFP_ENG_20020518_0107.6
- :name (n / name :op1 "Pakistan"))))
+ :name (n4 / name :op1 "Pakistan"))))

# ::id PROXY_AFP_ENG_20040728_0433.5 (submitted)
- :time (a / after
+ :time (a2 / after

# ::id PROXY_AFP_ENG_20070521_0178.15
- :time (b / before
+ :time (b3 / before
```

`make_splits.sh` puts all the training, dev, and test corpora into `training.txt`, `dev.txt` and `test.txt`.
