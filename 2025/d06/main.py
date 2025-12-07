from collections import namedtuple
from math import prod

P1_SAMPLE_SOLUTION = 4277556
P1_SOLUTION = 4309240495780
P2_SAMPLE_SOLUTION = 3263827
P2_SOLUTION = 9170286552289

InputType = namedtuple("InputType", ["numbers", "operations"])


def parse_input(filename: str) -> InputType:
    with open(filename, "r") as f:
        input = [line.rstrip("\n") for line in f]

    return InputType(
        numbers=input[:-1],
        operations=[c for c in input[-1] if c != " "],
    )


def transpose[T](arr: list[T]) -> list[T]:
    return [*zip(*arr)]


def calculate_total(numbers, operations) -> int:
    total = 0
    for line in zip(numbers, operations):
        match line[1]:
            case "+":
                total += sum(line[0])
            case "*":
                total += prod(line[0])
            case _:
                raise Exception(f"Unknown operation: {line[0]}")

    return total


def part1(input: InputType) -> int:
    numbers = [line.split(" ") for line in input.numbers]
    numbers = [[int(n) for n in line if n != ""] for line in numbers]
    numbers = transpose(numbers)

    return calculate_total(numbers, input.operations)


def split_array(input: list[str]) -> list[list[int]]:
    number_lists = []
    current = []
    for n in input:
        if n == '':
            number_lists.append(current)
            current = []
        else:
            current.append(int(n))

    number_lists.append(current)

    return number_lists


def part2(input: InputType) -> int:
    numbers = [(list(line)) for line in input.numbers]
    numbers = transpose(numbers)
    numbers = [(''.join(line)).strip() for line in numbers]

    return calculate_total(split_array(numbers), input.operations)


if __name__ == "__main__":
    p1_sample_input = parse_input("sample.txt")
    p1_input = parse_input("input.txt")

    p1_sample_solution = part1(p1_sample_input)
    p1_solution = part1(p1_input)

    assert p1_sample_solution == P1_SAMPLE_SOLUTION, f"Expected {P1_SAMPLE_SOLUTION}, got {p1_sample_solution}"  # noqa
    assert p1_solution == P1_SOLUTION, f"Expected {P1_SOLUTION}, got {p1_solution}"  # noqa

    p2_sample_input = parse_input("sample.txt")
    p2_input = parse_input("input.txt")

    p2_sample_solution = part2(p2_sample_input)
    p2_solution = part2(p2_input)

    assert p2_sample_solution == P2_SAMPLE_SOLUTION, f"Expected {P2_SAMPLE_SOLUTION}, got {p2_sample_solution}"  # noqa
    assert p2_solution == P2_SOLUTION, f"Expected {P2_SOLUTION}, got {p2_solution}"  # noqa

    print(f"Part 1: {p1_solution}\nPart 2: {p2_solution}")
