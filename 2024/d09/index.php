<?php

/** @return array<int, int> */
function parseInput(string $filename): array
{
    $line = file($filename)[0];
    $numbers = str_split(trim($line));
    return array_map('intval', $numbers);
}

/** @param array<int, int> $values */
function part1(array $values): int
{
    $disk = [];
    $id = 0;
    foreach ($values  as $i => $val) {
        if ($i % 2 === 0) {
            $disk = array_merge($disk, array_fill(0, $val, $id));
            $id++;
        } else {
            $disk = array_merge($disk, array_fill(0, $val, null));
        }
    }
    /* printf("%s\n", json_encode($disk)); */

    $r = count($disk) - 1;
    $newDisk = [];
    foreach ($disk as $i => $val) {
        if ($i > $r) {
            break;
        }

        if ($val === null) {
            $newDisk[$i] = $disk[$r];
            do {
                $r -= 1;
            } while (is_null($disk[$r]));
        } else {
            $newDisk[$i] = $disk[$i];
        }
    }
    /* printf("%s\n", json_encode($newDisk)); */

    $sum = 0;
    foreach ($newDisk as $i => $v) {
        $sum += $i * $v;
    }

    return $sum;
}

/** @param array<int, int> $values */
function findFreeSpace(array $values, int $size): ?int
{
    /* printf("Looking for free space of size %d\n", $size); */
    for ($i = 0; $i < count($values); $i++) {
        if (!is_null($values[$i])) {
            continue;
        }

        $j = 0;
        while (is_null($values[$i + $j] ?? null)) {
            $j++;
            if ($j == $size) {
                return $i;
            }
        }
        $i += $j;
    }

    /* printf("Could not find space\n"); */
    return null;
}

/** @param array<int, int> $values */
function part2(array $values): int
{
    $disk = [];
    $id = 0;
    foreach ($values  as $i => $val) {
        if ($i % 2 === 0) {
            $disk = array_merge($disk, array_fill(0, $val, $id));
            $id++;
        } else {
            $disk = array_merge($disk, array_fill(0, $val, null));
        }
    }
    /* printf("%s\n", json_encode($disk)); */

    $r = count($disk);
    while ($r > findFreeSpace($disk, 1)) {
        /* print("\n"); */

        $fileSize = 1;
        $r--;
        $c = $disk[$r];
        if (is_null($c)) {
            continue;
        }

        while ($r > 0 && $disk[$r - 1] == $c) {
            $fileSize++;
            $r--;
        }
        /* printf("Considering: %d, filesize %d\n", $c, $fileSize); */

        $freeSlot = findFreeSpace($disk, $fileSize);
        if (is_null($freeSlot)) {
            /* printf("File %d cannot move\n", $c); */
            continue;
        } elseif ($freeSlot > $r) {
            /* printf("File %d already moved\n", $c); */
            continue;
        }

        /* printf("Replacing free space with %d, size %d, starting at %d\n", $c, $fileSize, $freeSlot); */
        for ($i = $fileSize; $i > 0; $i--) {
            $disk[$freeSlot + $i - 1] = $c;
            $disk[$r + $i - 1] = null;
        }
        $disk = array_values($disk);


        /* printf("%s\n", json_encode($disk)); */
    }
    /* printf("%s\n", json_encode($disk)); */

    $sum = 0;
    foreach ($disk as $i => $v) {
        if (is_null($v)) continue;
        $sum += $i * $v;
    }

    return $sum;
}

$sampleValues = parseInput("sample.txt");
$inputValues = parseInput("input.txt");

$p1Sample = part1($sampleValues);
$p1Input = part1($inputValues);
assert($p1Sample == 1928);
assert($p1Input == 6356833654075);

$p2Sample = part2($sampleValues);
$p2Input = part2($inputValues);
assert($p2Sample == 2858);
assert($p2Input == 6389911791746);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
