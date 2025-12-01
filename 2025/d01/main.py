def parse_input(filename: str) -> list[tuple[str, int]]:
    with open(filename, "r") as f:
        return [(line[0], int(line[1:].strip())) for line in f]


def part1(input: list[tuple[str, int]]) -> int:
    value = 50
    counter = 0

    for line in input:
        if line[0] == 'L':
            value = (value - line[1]) % 100
        elif line[0] == 'R':
            value = (value + line[1]) % 100
        else:
            raise Exception(f"unknown direction: {line[0]}")

        if value == 0:
            counter += 1

    return counter


def part2(input: list[tuple[str, int]]) -> int:
    value = 50
    counter = 0

    for line in input:
        direction = line[0]
        delta = line[1]
        zeroes, delta = divmod(delta, 100)
        counter += zeroes

        if direction == 'L':
            new_value = (value - delta) % 100
            if value != 0 and (new_value == 0 or new_value > value):
                counter += 1
            value = new_value
        elif direction == 'R':
            new_value = (value + delta) % 100
            if value != 0 and new_value < value:
                counter += 1

            value = new_value
        else:
            raise Exception(f"unknown direction: {line[0]}")

    return counter


if __name__ == "__main__":
    p1_sample_input = parse_input("sample.txt")
    p1_input = parse_input("input.txt")

    p1_sample_solution = part1(p1_sample_input)
    p1_solution = part1(p1_input)

    assert p1_sample_solution == 3, f"Expected 3, got {p1_sample_solution}"
    assert p1_solution == 989, f"Expected 989, got {p1_solution}"

    p2_sample_input = parse_input("sample.txt")
    p2_input = parse_input("input.txt")

    p2_sample_solution = part2(p2_sample_input)
    p2_solution = part2(p2_input)

    assert p2_sample_solution == 6, f"Expected 6, got {p2_sample_solution}"
    assert p2_solution == 5941, f"Expected 5941, got {p2_solution}"

    print(f"Part 1: {p1_solution}\nPart 2: {p2_solution}")
