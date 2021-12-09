package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Part 1: %v\n", part1(string(input)))
	// fmt.Printf("Part 2: %v\n", part2(string(input)))
}

func part1(input string) int {
	words := strings.Split(input, "\n")
	count := 0
	for _, word := range words {
		if isNice(word) {
			count++
		}
	}

	return count
}

func isNice(word string) bool {
	return hasThreeVowels(word) &&
		hasDuplicateNeighbors(word) &&
		!containsBlacklistedStrings(word)
}

func isVowel(c rune) bool {
	return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
}

func hasThreeVowels(word string) bool {
	count := 0
	for _, c := range word {
		if isVowel(c) {
			count++
		}
	}

	return count >= 3
}

func hasDuplicateNeighbors(word string) bool {
	for i := 0; i < len(word)-1; i++ {
		if word[i] == word[i+1] {
			return true
		}
	}

	return false
}

func containsBlacklistedStrings(word string) bool {
	return strings.Index(word, "ab") >= 0 ||
		strings.Index(word, "cd") >= 0 ||
		strings.Index(word, "pq") >= 0 ||
		strings.Index(word, "xy") >= 0
}
