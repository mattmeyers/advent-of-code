P1_SAMPLE_SOLUTION = 13
P1_SOLUTION = 1419
P2_SAMPLE_SOLUTION = 43
P2_SOLUTION = 8739


def parse_input(filename: str) -> list[list[str]]:
    with open(filename, "r") as f:
        return [list(line.strip()) for line in f]


def check_neighbor(grid: list[list[str]], row: int, col: int) -> int:
    if row < 0 or row >= len(grid) or col < 0 or col >= len(grid[0]):
        return 0

    return 1 if grid[row][col] == '@' else 0


def part1(input: list[list[str]]) -> int:
    total = 0
    for row, line in enumerate(input):
        for col, c in enumerate(line):
            if c == '.':
                continue

            neighbors = sum([
                check_neighbor(input, row-1, col-1),
                check_neighbor(input, row-1, col),
                check_neighbor(input, row-1, col+1),
                check_neighbor(input, row, col-1),
                check_neighbor(input, row, col+1),
                check_neighbor(input, row+1, col-1),
                check_neighbor(input, row+1, col),
                check_neighbor(input, row+1, col+1),
            ])

            if neighbors < 4:
                total += 1

    return total


def part2(input: list[list[str]]) -> int:
    total = 0
    removals = []
    while True:
        for row, line in enumerate(input):
            for col, c in enumerate(line):
                if c == '.':
                    continue

                neighbors = sum([
                    check_neighbor(input, row-1, col-1),
                    check_neighbor(input, row-1, col),
                    check_neighbor(input, row-1, col+1),
                    check_neighbor(input, row, col-1),
                    check_neighbor(input, row, col+1),
                    check_neighbor(input, row+1, col-1),
                    check_neighbor(input, row+1, col),
                    check_neighbor(input, row+1, col+1),
                ])

                if neighbors < 4:
                    removals.append((row, col))

        if len(removals) == 0:
            break

        for roll in removals:
            input[roll[0]][roll[1]] = '.'

        total += len(removals)
        removals = []

    return total


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
