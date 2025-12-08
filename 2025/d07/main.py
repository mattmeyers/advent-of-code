P1_SAMPLE_SOLUTION = 21
P1_SOLUTION = 1672
P2_SAMPLE_SOLUTION = 40
P2_SOLUTION = 231229866702355

type InputType = list[list[str]]


def parse_input(filename: str) -> InputType:
    with open(filename, "r") as f:
        return [list(line.strip()) for line in f]


def print_grid(grid: list[list[str | int]]) -> None:
    print("\n".join(["".join([str(c) for c in line]) for line in grid]))


def part1(input: InputType) -> int:
    starting_pos = input[0].index("S")
    input[1][starting_pos] = "|"

    total_splits = 0
    for i in range(1, len(input) - 1):
        beam_indexes = [i for i, c in enumerate(input[i]) if c == "|"]
        for j in beam_indexes:
            if input[i + 1][j] == "^":
                input[i + 1][j - 1] = "|"
                input[i + 1][j + 1] = "|"
                total_splits += 1
            else:
                input[i + 1][j] = "|"

    return total_splits


def part2(input: InputType) -> int:
    grid = [[0 if c == "." else c for c in line] for line in input]

    starting_pos = grid[0].index("S")
    grid[1][starting_pos] = 1

    for i in range(1, len(grid) - 1):
        beam_indexes = [i for i, c in enumerate(grid[i]) if c not in {"^", 0}]
        for j in beam_indexes:
            if grid[i + 1][j] == "^":
                grid[i + 1][j - 1] = grid[i + 1][j - 1] + grid[i][j]
                grid[i + 1][j + 1] = grid[i + 1][j + 1] + grid[i][j]
            else:
                grid[i + 1][j] = grid[i][j] + grid[i + 1][j]

    return sum(grid[-1])


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
    assert p2_solution == P2_SOLUTION, f"Expected {P2_SOLUTION}, got {p2_solution}"  # noqa

    print(f"Part 1: {p1_solution}\nPart 2: {p2_solution}")
