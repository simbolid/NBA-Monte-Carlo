#!/bin/bash

echo Sequential Runtimes
python elo_sequential.py 5000
python elo_sequential.py 10000
python elo_sequential.py 50000
python elo_sequential.py 100000
python elo_sequential.py 500000

echo Parallel Runtimes
python elo_parallel.py 5000
python elo_parallel.py 10000
python elo_parallel.py 50000
python elo_parallel.py 100000
python elo_parallel.py 500000

