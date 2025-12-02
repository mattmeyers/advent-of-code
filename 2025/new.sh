#!/usr/bin/env bash

set -e

day="$1"
if [ $# = 0 ]; then
    echo "Day number required"
    exit 1
elif [[ $day -le 9 ]]; then
    day="0$day"
fi

directory="d$day"

mkdir "$directory"
touch "$directory/sample.txt"
touch "$directory/input.txt"
cat >"$directory/main.py" <<CODE
P1_SAMPLE_SOLUTION = 0
P1_SOLUTION = 0
P2_SAMPLE_SOLUTION = 0
P2_SOLUTION = 0


def parse_input(filename: str) -> list[str]:
    with open(filename, "r") as f:
        return [line.strip() for line in f]


def part1(input: list[str]) -> int:
    return 0


def part2(input: list[str]) -> int:
    return 0


if __name__ == "__main__":
    p1_sample_input = parse_input("sample.txt")
    p1_input = parse_input("input.txt")

    p1_sample_solution = part1(p1_sample_input)
    p1_solution = part1(p1_input)

    assert p1_sample_solution == P1_SAMPLE_SOLUTION, f"Expected {P1_SAMPLE_SOLUTION}, got {p1_sample_solution}"  # noqa
    # assert p1_solution == P1_SOLUTION, f"Expected {P1_SOLUTION}, got {p1_solution}"  #noqa

    p2_sample_input = parse_input("sample.txt")
    p2_input = parse_input("input.txt")

    p2_sample_solution = part2(p2_sample_input)
    p2_solution = part2(p2_input)

    assert p2_sample_solution == P2_SAMPLE_SOLUTION, f"Expected {P2_SAMPLE_SOLUTION}, got {p2_sample_solution}"  # noqa
    # assert p2_solution == P2_SOLUTION, f"Expected {P2_SOLUTION}, got {p2_solution}"  #noqa

    print(f"Part 1: {p1_solution}\nPart 2: {p2_solution}")
CODE
