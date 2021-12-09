package main

import "testing"

func Test_part1(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{input: ">", want: 2},
		{input: "^>v<", want: 4},
		{input: "^v^v^v^v^v", want: 2},
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
		{input: "^v", want: 3},
		{input: "^>v<", want: 3},
		{input: "^v^v^v^v^v", want: 11},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := part2(tt.input); got != tt.want {
				t.Errorf("part2() = %v, want %v", got, tt.want)
			}
		})
	}
}
