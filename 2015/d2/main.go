package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Part 1: %v\n", part1(string(input)))
}

type dimension struct {
	l int
	w int
	h int
}

func (d dimension) surfaceArea() int {
	return 2*(d.l*d.w) + 2*(d.l*d.h) + 2*(d.w*d.h)
}

func (d dimension) slack() int {
	min := d.l * d.w
	if n := d.l * d.h; n < min {
		min = n
	}

	if n := d.w * d.h; n < min {
		min = n
	}

	return min
}

func parseInput(input string) []dimension {
	lines := strings.Split(input, "\n")
	ds := make([]dimension, len(lines))

	for i, line := range lines {
		ds[i] = parseDimension(line)
	}

	return ds
}

func parseDimension(line string) dimension {
	parts := strings.Split(line, "x")
	return dimension{
		l: mustParseInt(parts[0]),
		w: mustParseInt(parts[1]),
		h: mustParseInt(parts[2]),
	}
}

func mustParseInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func part1(input string) int {
	dims := parseInput(input)

	sum := 0
	for _, d := range dims {
		sum += d.surfaceArea() + d.slack()
	}

	return sum
}
