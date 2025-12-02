P1_SAMPLE_SOLUTION = 1227775554
P1_SOLUTION = 26255179562
P2_SAMPLE_SOLUTION = 4174379265
P2_SOLUTION = 31680313976


def parse_input(filename: str) -> list[tuple[int, int]]:
    with open(filename, "r") as f:
        return [tuple(map(int, p.split("-"))) for p in f.read().strip().split(",")]


def part1(input: list[tuple[int, int]]) -> int:
    sum = 0

    for pair in input:
        for i in range(pair[0], pair[1] + 1):
            v = str(i)
            if len(v) % 2 != 0:
                continue

            if v[0:int(len(v)/2)] == v[int(len(v)/2):]:
                sum += i

    return sum


def part2(input: list[str]) -> int:
    s = 0

    for pair in input:
        for i in range(pair[0], pair[1] + 1):
            v = str(i)
            invalid_values = set()
            for window_size in range(len(v) // 2, 0, -1):
                if window_is_valid(v, window_size):
                    continue
                invalid_values.add(i)

            s += sum(invalid_values)

    return s


def window_is_valid(value: str, window_size: int) -> bool:
    if len(value) % window_size != 0:
        return True

    current = value[0:window_size]
    for j in range(1, len(value) // window_size):
        next = value[window_size * j:window_size*(j+1)]
        if current != next:
            return True

        current = next

    return False


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
