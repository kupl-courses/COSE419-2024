# Build
```
$ make
```

# Run
1. Partial correctness 
```
$ dune exec -- ./main.exe --input examples/partial/linearSearch
```

2. Total correctness 
```
$ dune exec -- ./main.exe --input examples/total/linearSearch --termination
```

3. Output CFG in dot
```
$ dune exec -- ./main.exe --input examples/partial/linearSearch --print-cfg > /tmp/cfg.dot
$ dot -Tpng /tmp/cfg.dot > /tmp/cfg.png
$ open /tmp/cfg.png
```