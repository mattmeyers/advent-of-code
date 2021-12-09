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

type house struct {
	x, y int
}

func part1(input string) int {
	pos := house{}
	houses := map[house]int{pos: 1}
	for _, d := range input {
		switch d {
		case '^':
			pos.y++
		case '>':
			pos.x++
		case 'v':
			pos.y--
		case '<':
			pos.x--
		}
		houses[pos]++
	}

	return len(houses)
}

func part2(input string) int {
	pos := house{}
	roboPos := house{}
	houses := map[house]int{pos: 2}
	for i, d := range input {
		p := &pos
		if i%2 == 0 {
			p = &roboPos
		}
		switch d {
		case '^':
			p.y++
		case '>':
			p.x++
		case 'v':
			p.y--
		case '<':
			p.x--
		}
		houses[*p]++
	}

	return len(houses)
}
