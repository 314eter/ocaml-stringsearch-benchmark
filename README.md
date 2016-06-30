# Benchmark of string searching algorithms in OCaml

## Usage

```sh
./download.sh
ocamlbuild benchmark.native
./benchmark.native
./plot.py
```

## Algorithms

### Naive

Naive brute-force algorithm.

### Specialized

Specialized naive implementations for patternlengths from 1 to 10.

### Hash

Rabin-Karp algorithm with the sum of the characters as hash sum.

### Horspool

Horspool algorithm.

### Kmp

Knuth-Morris-Pratt algorithm.

### Boyermoore

Boyer-Moore algorithm.

## Benchmarks

### Dorian

A copy of _The Picture of Dorian Gray_.

### Wikipedia

A list of Wikipedia articles related to functional programming.

### Random

A string of random characters.

### DNA

Part of the human genome.

### AAB

A string with only `A`'s. The pattern consists of `A`'s with one `B` at the end.

## Results

![benchmark](benchmark.png)
