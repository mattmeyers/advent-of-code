package main

import (
	"testing"
)

func Test_part1(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{input: "2x3x4", want: 58},
		{input: "1x1x10", want: 43},
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
		{input: "2x3x4", want: 34},
		{input: "1x1x10", want: 14},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := part2(tt.input); got != tt.want {
				t.Errorf("part2() = %v, want %v", got, tt.want)
			}
		})
	}
}
