P1_SAMPLE_SOLUTION = 357
P1_SOLUTION = 17301
P2_SAMPLE_SOLUTION = 3121910778619
P2_SOLUTION = 172162399742349


def parse_input(filename: str) -> list[list[int]]:
    with open(filename, "r") as f:
        return [[int(c) for c in line.strip()] for line in f]


def part1(input: list[list[int]]) -> int:
    total = 0
    for line in input:
        max_1 = max(line[0:-1])
        max_2 = max(line[line.index(max_1)+1:])

        total += 10 * max_1 + max_2

    return total


def part2(input: list[list[int]]) -> int:
    total = 0
    for line in input:
        m = max(line[:-11])
        left_bound = line.index(m)
        total += 10**11 * m

        for right_bound in range(-10, 0):
            m = max(line[left_bound+1:right_bound])
            left_bound = line.index(m, left_bound+1)
            total += 10**(right_bound * -1) * m

        total += max(line[left_bound+1:])

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
