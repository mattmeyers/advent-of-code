package main

import "testing"

func Test_part1(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{input: "ugknbfddgicrmopn", want: 1},
		{input: "aaa", want: 1},
		{input: "jchzalrnumimnmhp", want: 0},
		{input: "haegwjzuvuyypxyu", want: 0},
		{input: "dvszwmarrgswjxmb", want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := part1(tt.input); got != tt.want {
				t.Errorf("part1() = %v, want %v", got, tt.want)
			}
		})
	}
}
