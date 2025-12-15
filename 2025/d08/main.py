import math
from dataclasses import dataclass

P1_SAMPLE_SOLUTION = 40
P1_SOLUTION = 175440
P2_SAMPLE_SOLUTION = 25272
P2_SOLUTION = 3200955921


@dataclass
class Point:
    x: int
    y: int
    z: int

    def __eq__(self, other: "Point") -> bool:
        return self.x == other.x and self.y == other.y and self.z == other.z

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __str__(self):
        return f"({self.x}, {self.y}, {self.z})"

    def __repr__(self):
        return f"Point({self.x=}, {self.y=}, {self.z=})"

    def distance_from(self, other: "Point") -> float:
        return math.sqrt(
            (self.x - other.x) ** 2
            + abs(self.y - other.y) ** 2
            + abs(self.z - other.z) ** 2
        )


@dataclass
class Connection:
    left: Point
    right: Point
    distance: float

    def __hash__(self):
        return hash((self.left, self.right))

    def __str__(self):
        return f"[{self.left}, {self.right}]"

    def __repr__(self):
        return f"Connection({self.left=}, {self.right=}, {self.distance})"

    def __lt__(self, other: "Connection") -> bool:
        return self.distance < other.distance

    def __le__(self, other: "Connection") -> bool:
        return self.distance <= other.distance

    def connects_to(self, other: "Connection") -> bool:
        return (
            self.left == other.left
            or self.left == other.right
            or self.right == other.left
            or self.right == other.right
        )


class Circuit:
    id_: int
    connections: set[Connection]
    junction_boxes: set[Point]

    def __init__(self, id_: int, conn: Connection):
        self.id_ = id_
        self.connections = {conn}
        self.junction_boxes = {conn.left, conn.right}

    def __repr__(self):
        return f"Circuit({self.connections=})"

    def __len__(self):
        return len(self.connections)

    def contains_junction_boxes(self, conn: Connection) -> bool:
        return conn.left in self.junction_boxes and conn.right in self.junction_boxes

    def can_add(self, conn: Connection) -> bool:
        return any(
            (box == conn.left or box == conn.right for box in self.junction_boxes)
        )

    def add(self, conn: Connection) -> None:
        self.connections.add(conn)
        self.junction_boxes.add(conn.left)
        self.junction_boxes.add(conn.right)

    def merge(self, link: Connection, other: "Circuit") -> "Circuit":
        c = Circuit(self.id_, link)
        for conn in self.connections:
            c.add(conn)

        for conn in other.connections:
            c.add(conn)

        return c


type InputType = list[Point]


def parse_input(filename: str) -> InputType:
    with open(filename, "r") as f:
        return [Point(*map(int, line.strip().split(","))) for line in f]


def calculate_distances(input: InputType) -> list[Connection]:
    connections = []
    for i in range(len(input)):
        for j in range(i + 1, len(input)):
            connections.append(
                Connection(input[i], input[j], input[i].distance_from(input[j]))
            )

    return sorted(connections, key=lambda c: c.distance)


def update_circuits(conn: Connection, circuits: list[Circuit]) -> list[Circuit]:
    containing_circuits = [
        i for i, circuit in enumerate(circuits) if circuit.can_add(conn)
    ]
    match containing_circuits:
        case []:
            circuits.append(Circuit(len(circuits), conn))
        case [idx]:
            circuits[idx].add(conn)
        case [idx1, idx2]:
            circuits[idx1] = circuits[idx1].merge(conn, circuits[idx2])
            del circuits[idx2]
        case _:
            raise Exception(f"3+ circuits contain connection {conn}")

    return circuits


def part1(input: InputType, connection_limit: int) -> int:
    connections = calculate_distances(input)

    circuits = []
    for conn in connections[:connection_limit]:
        circuits = update_circuits(conn, circuits)

    lengths = sorted(map(lambda c: len(c.junction_boxes), circuits), reverse=True)
    return math.prod(lengths[:3])


def part2(input: InputType) -> int:
    connections = calculate_distances(input)
    point_count = len(input)

    circuits = []
    for conn in connections:
        circuits = update_circuits(conn, circuits)

        if len(circuits) == 1 and len(circuits[0].junction_boxes) == point_count:
            return conn.left.x * conn.right.x


if __name__ == "__main__":
    p1_sample_input = parse_input("sample.txt")
    p1_input = parse_input("input.txt")

    p1_sample_solution = part1(p1_sample_input, 10)
    p1_solution = part1(p1_input, 1000)

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
