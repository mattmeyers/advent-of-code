P1_SAMPLE_SOLUTION = 50
P1_SOLUTION = 4758598740
P2_SAMPLE_SOLUTION = 0
P2_SOLUTION = 0

type InputType = list[tuple[int, int]]


def parse_input(filename: str) -> InputType:
    with open(filename, "r") as f:
        return [tuple(map(int, line.strip().split(","))) for line in f]


def area(left: tuple[int, int], right: tuple[int, int]) -> int:
    return (abs(left[0] - right[0]) + 1) * (abs(left[1] - right[1]) + 1)


def part1(input: InputType) -> int:
    m = 0
    for i in range(len(input)):
        for j in range(i, len(input)):
            n = area(input[i], input[j])
            if n > m:
                m = n

    return m


def part2(input: InputType) -> int:
    return 0


if __name__ == "__main__":
    p1_sample_input = parse_input("sample.txt")
    p1_input = parse_input("input.txt")

    p1_sample_solution = part1(p1_sample_input)
    p1_solution = part1(p1_input)

    assert p1_sample_solution == P1_SAMPLE_SOLUTION, (
        f"Expected {P1_SAMPLE_SOLUTION}, got {p1_sample_solution}"
    )  # noqa
    assert p1_solution == P1_SOLUTION, f"Expected {P1_SOLUTION}, got {p1_solution}"  # noqa

    p2_sample_input = parse_input("sample.txt")
    p2_input = parse_input("input.txt")

    p2_sample_solution = part2(p2_sample_input)
    p2_solution = part2(p2_input)

    assert p2_sample_solution == P2_SAMPLE_SOLUTION, (
        f"Expected {P2_SAMPLE_SOLUTION}, got {p2_sample_solution}"
    )  # noqa
    # assert p2_solution == P2_SOLUTION, f"Expected {P2_SOLUTION}, got {p2_solution}"  # noqa

    print(f"Part 1: {p1_solution}\nPart 2: {p2_solution}")
