<?php

class Equation
{
    public readonly int $target;
    /** @var array<int, int> */
    public readonly array $values;
    /** array<int, callable(int, int): int */
    private array $operations;

    /** @param array<int, int> $values*/
    function __construct(
        int $target,
        array $values,
    ) {
        $this->target = $target;
        $this->values = $values;

        $this->operations = [
            fn(int $l, int $r) => $l + $r,
            fn(int $l, int $r) => $l * $r,
        ];
    }

    public static function fromLine(string $line): self
    {
        [$target, $values] = explode(":", $line);
        $values = explode(" ", trim($values));

        return new self(
            (int)$target,
            array_map('intval', $values),
        );
    }

    /** @param callable(int, int): int $op */
    public function registerOperation(callable $op): void
    {
        $this->operations[] = $op;
    }

    public function isSolvable(): bool
    {
        return $this->trySolve($this->values[0], array_slice($this->values, 1));
    }

    /** @param array<int, int> $values */
    private function trySolve(int $answer, array $values): bool
    {
        if (count($values) == 0) {
            return $answer === $this->target;
        } else if ($answer > $this->target) {
            // The answer can only increase. If we've already passed the target
            // then there's no point in evaluating the rest of the subtree.
            return false;
        }

        foreach ($this->operations as $op) {
            $isSolvable = $this->trySolve(
                $op($answer, $values[0]),
                array_slice($values, 1),
            );

            if ($isSolvable) {
                // Short circuit once we realize our equation is solvable to 
                // avoid computing additional subtrees.
                return true;
            }
        }

        return false;
    }
}

/** @return array<int, Equation> */
function parseInput(string $filename): array
{
    $lines = [];
    foreach (file($filename) as $line) {
        $lines[] = Equation::fromLine($line);
    }

    return $lines;
}

/** @param array<int, Equation> $equations */
function part1(array $equations): int
{
    return array_reduce(
        $equations,
        fn($acc, $eq) => $acc + ($eq->isSolvable() ? $eq->target : 0),
        0,
    );
}

/** @param array<int, Equation> $equations */
function part2(array $equations): int
{
    foreach ($equations as $eq) {
        $eq->registerOperation(fn(int $l, int $r) => (int)((string)$l . (string)$r));
    }

    return array_reduce(
        $equations,
        fn($acc, $eq) => $acc + ($eq->isSolvable() ? $eq->target : 0),
        0,
    );
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 3749);
assert($p1Input == 1985268524462);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert($p2Sample == 11387);
assert($p2Input == 150077710195188);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
