<?php

enum Direction: string
{
    case UP = "^";
    case RIGHT = ">";
    case DOWN = "v";
    case LEFT = "<";

    /** @return array{int, int} */
    function delta(): array
    {
        return match ($this) {
            Direction::UP => [0, -1],
            Direction::RIGHT => [1, 0],
            Direction::DOWN => [0, 1],
            Direction::LEFT => [-1, 0],
        };
    }

    function rotate(): Direction
    {
        return match ($this) {
            Direction::UP => Direction::RIGHT,
            Direction::RIGHT => Direction::DOWN,
            Direction::DOWN => Direction::LEFT,
            Direction::LEFT => Direction::UP,
        };
    }
}

class Tracker
{
    /** @var array<int, array<int, string>> */
    public array $map;

    /** @var array<string, array<string, bool>> */
    public array $seen = [];

    private int $x;
    private int $y;
    private Direction $d;

    private int $maxX;
    private int $maxY;

    public int $turnPoints = 0;

    /** @param array<int, array<int, string>> $map */
    function __construct(array $map)
    {
        $this->map = $map;
        $this->maxX = count($map[0]);
        $this->maxY = count($map);

        foreach ($map as $i => $row) {
            foreach ($row as $j => $col) {
                if ($col !== "." && $col !== "#") {
                    $this->d = Direction::from($col);
                    $this->x = $j;
                    $this->y = $i;
                    break 2;
                }
            }
        }

        $this->addSeen();
    }

    public function run(): void
    {
        while (!$this->isFinished()) {
            $this->step();
        }
    }

    public function step(): void
    {
        if ($this->peek() === "#") {
            $this->d = $this->d->rotate();
            // For the niche case where a guard hits an obstruction, turns, and
            // immediately hits another obstruction.
            if ($this->peek() === "#") {
                $this->d = $this->d->rotate();
            }
        }

        $this->advance();
        if (!$this->isFinished()) {
            $this->addSeen();
        }
    }

    public function isFinished(): bool
    {
        return $this->x < 0
            || $this->x >= $this->maxX
            || $this->y < 0
            || $this->y >= $this->maxY;
    }

    private function peek(): string
    {
        [$dx, $dy] = $this->d->delta();
        return $this->map[$this->y + $dy][$this->x + $dx] ?? "";
    }

    private function advance(): void
    {
        [$dx, $dy] = $this->d->delta();
        $this->x += $dx;
        $this->y += $dy;
    }

    private function addSeen(): void
    {
        if ($this->seen["$this->x,$this->y"][$this->d->value] ?? false) {
            throw new Exception("Looping");
        }

        $this->seen["$this->x,$this->y"][$this->d->value] = true;
    }
}

/** @return array<int, array<int, string>> */
function parseInput(string $filename): array
{
    $map = [];
    foreach (file($filename) as $line) {
        $map[] = str_split(trim($line));
    }

    return $map;
}

/** @param array<int, array<int, string)> $map */
function part1(array $map): int
{
    $tracker = new Tracker($map);
    $tracker->run();

    return count($tracker->seen);
}

/** @param array<int, array<int, string)> $map */
function part2(array $map): int
{
    $tracker = new Tracker($map);
    $tracker->run();

    $count = 0;
    foreach (array_keys($tracker->seen) as $turnPoint) {
        [$x, $y] = explode(",", $turnPoint);
        $newMap = $map;
        if ($newMap[(int)$y][(int)$x] !== ".") {
            continue;
        }

        $newMap[(int)$y][(int)$x] = "#";
        try {
            (new Tracker($newMap))->run();
        } catch (Exception) {
            $count++;
        }
    }

    return $count;
}

$sampleMap = parseInput("sample.txt");
$inputMap = parseInput("input.txt");

$p1Sample = part1($sampleMap);
$p1Input = part1($inputMap);
assert($p1Sample == 41);
assert($p1Input == 5239);

$p2Sample = part2($sampleMap);
$p2Input = part2($inputMap);
assert($p2Sample == 6);
assert($p2Input == 1753);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
