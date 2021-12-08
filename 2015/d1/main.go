package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Part 1: %v\n", part1(string(input)))
	fmt.Printf("Part 2: %v\n", part2(string(input)))
}

func part1(input string) int {
	floor := 0
	for _, c := range input {
		if c == '(' {
			floor++
		} else {
			floor--
		}
	}

	return floor
}

func part2(input string) int {
	floor := 0
	for i, c := range input {
		if c == '(' {
			floor++
		} else {
			floor--
		}

		if floor < 0 {
			return i + 1
		}
	}

	return floor
}
