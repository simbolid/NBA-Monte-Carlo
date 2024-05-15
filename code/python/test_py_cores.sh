#!/bin/bash

python elo_parallel.py 500000 --workers 2
python elo_parallel.py 500000 --workers 4 
python elo_parallel.py 500000 --workers 8
python elo_parallel.py 500000 --workers 16

