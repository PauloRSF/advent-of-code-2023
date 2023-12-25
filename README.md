# Advent of Code 2023

These are my solutions for the [2023 edition of Advent of Code](https://adventofcode.com/2023). I'll implement each solution in a different programming language (that i never used, preferrably) following an alphabetic order.

## How to run

### Requirements
  - [Docker](https://docs.docker.com/get-docker)
  - [Make](https://www.gnu.org/software/make)

First of all, you'll need to build the docker image to run the solutions:

```sh
$ docker build -t advent-of-code-2023 .
```

Then, to run a solution, you need to create a file called `input.txt` in the respective challenge directory and then run `make NUMBER`, where `number` is the two-digit (zero-padded to the left) challenge's day number.

For example, if you want to run the day 3 challenge:

```sh
$ touch 03-gear-ratios/input.txt # You'll need to copy your input from the website to this file
$ make 03
```

## Languages used

- Day 1: [Ada](https://learn.adacore.com/index.html)
- Day 2: [Basic (FreeBasic)](https://www.freebasic.net)
- Day 3: [Clojure](https://clojure.org)
