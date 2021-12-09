package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	input := "yzbqklnj"

	fmt.Printf("Part 1: %v\n", part1(input))
	fmt.Printf("Part 2: %v\n", part2(string(input)))
}

func part1(input string) int {
	i := 1
	for {
		s := input + strconv.Itoa(i)
		sum := md5.Sum([]byte(s))
		if strings.HasPrefix(hex.EncodeToString(sum[:]), "00000") {
			return i
		}

		i++
	}
}

func part2(input string) int {
	i := 1
	for {
		s := input + strconv.Itoa(i)
		sum := md5.Sum([]byte(s))
		if strings.HasPrefix(hex.EncodeToString(sum[:]), "000000") {
			return i
		}

		i++
	}
}
