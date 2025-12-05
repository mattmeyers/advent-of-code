from dataclasses import dataclass

P1_SAMPLE_SOLUTION = 3
P1_SOLUTION = 643
P2_SAMPLE_SOLUTION = 14
P2_SOLUTION = 342018167474526

type InputType = tuple[list["Span"], list[int]]


@dataclass
class Span:
    l: int
    r: int

    def __contains__(self, v: int) -> bool:
        return self.l <= v <= self.r

    def __len__(self) -> int:
        return self.r - self.l + 1

    def __lt__(self, other: "Span") -> bool:
        return self.l < other.l

    def __le__(self, other: "Span") -> bool:
        return self.l <= other.l

    def intersects(self, other: "Span") -> bool:
        return (self.l <= other.l <= self.r) or (other.l <= self.l <= other.r)

    def merge(self, other: "Span") -> "Span":
        return Span(l=min(self.l, other.l), r=max(self.r, other.r))


def parse_input(filename: str) -> InputType:
    with open(filename, "r") as f:
        spans = []
        for line in f:
            line = line.strip()
            if line == "":
                break

            l, r = line.split("-")
            spans.append(Span(l=int(l), r=int(r)))

        ids = [int(line.strip()) for line in f]

        return (spans, ids)


def part1(input: InputType) -> int:
    return sum((any((id_ in span for span in input[0])) for id_ in input[1]))


def reduce_spans(spans: list[Span]) -> list[Span]:
    span = spans[0]
    others = spans[1:]

    for i, other in enumerate(others):
        if not span.intersects(other):
            return [span] + others[i:]

        span = span.merge(other)

    return [span]


def part2(input: InputType) -> int:
    spans = sorted(input[0])
    reduced_spans = []

    while len(spans) > 0:
        s, *spans = reduce_spans(spans)
        reduced_spans.append(s)

    return sum((len(span) for span in reduced_spans))


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
