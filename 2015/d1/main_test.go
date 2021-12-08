package main

import (
	"testing"
)

func Test_part1(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{input: "(())", want: 0},
		{input: "()()", want: 0},
		{input: "(((", want: 3},
		{input: "(()(()(", want: 3},
		{input: "))(((((", want: 3},
		{input: "())", want: -1},
		{input: ")))", want: -3},
		{input: ")())())", want: -3},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := part1(tt.input); got != tt.want {
				t.Errorf("part1() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_part2(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{input: ")", want: 1},
		{input: "()())", want: 5},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := part2(tt.input); got != tt.want {
				t.Errorf("part2() = %v, want %v", got, tt.want)
			}
		})
	}
}
