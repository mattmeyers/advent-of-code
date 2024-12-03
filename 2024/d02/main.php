<?php

/**
 * @return array<int, array<int, int>>
 */
function parseInput(string $filename): array
{
    $lines = [];
    foreach (file($filename) as $line) {
        $lines[] = array_map('intval', explode(" ", trim($line)));
    }

    return $lines;
}

/**
 * @param array<int, int> $levels
 */
function diffLevels(array $levels)
{
    $diffs = [];
    for ($i = 0; $i < count($levels) - 1; $i++) {
        $diffs[] = $levels[$i + 1] - $levels[$i];
    }

    return $diffs;
}

/**
 * @param array<int, int> $report
 */
function isSafe(array $report): bool
{
    $diffs = diffLevels($report);
    $monotonic = array_all($diffs, fn($v) => $v > 0) || array_all($diffs, fn($v) => $v < 0);
    $gradual = array_all($diffs, fn($v) => abs($v) >= 1 && abs($v) <= 3);
    return $monotonic && $gradual;
}

/**
 * @param array<int, array<int, int>> $reports
 */
function part1(array $reports): int
{
    $count = 0;
    foreach ($reports as $report) {
        $count += (int)(isSafe($report));
    }
    return $count;
}


/**
 * @param array<int, array<int, int>> $reports
 */
function part2(array $reports): int
{
    $count = 0;
    foreach ($reports as $report) {
        if (isSafe($report)) {
            $count++;
            continue;
        }
        for ($i = 0; $i < count($report); $i++) {
            $v = $report;
            unset($v[$i]);
            if (isSafe(array_values($v))) {
                $count++;
                continue 2;
            }
        }
    }
    return $count;
}

$sampleReports = parseInput("sample.txt");
$inputReports = parseInput("input.txt");

$p1Sample = part1($sampleReports);
$p1Input = part1($inputReports);
assert($p1Sample == 2);
assert($p1Input == 510);

$p2Sample = part2($sampleReports);
$p2Input = part2($inputReports);
assert($p2Sample == 4);
assert($p2Input == 553);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
