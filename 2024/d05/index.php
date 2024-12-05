<?php

/** @return array<int, array<int, string>> */
function parseInput(string $filename): array
{
    $f = file($filename);

    $rules = [];
    $updates = [];
    $parsingRules = true;
    foreach ($f as $line) {
        if ($line === "\n") {
            $parsingRules = false;
            continue;
        }

        if ($parsingRules) {
            $rules[] = trim($line);
        } else {
            $updates[] = trim($line);
        }
    }


    return [$rules, $updates];
}

/**
 * @param array<int, string> $input
 *
 * @return array<string, array<int, string>>
 */
function parseRules(array $input): array
{
    $rules = [];
    foreach ($input as $line) {
        [$l, $r] = explode("|", $line);
        if (!array_key_exists($l, $rules)) {
            $rules[$l] = [];
        }

        $rules[$l][] = $r;
    }

    return $rules;
}

/**
 * @param array<int, string> $input
 *
 * @return array<int, array<int, string>>
 */
function parseUpdates(array $input): array
{
    $updates = [];
    foreach ($input as $line) {
        $updates[] = explode(",", $line);
    }

    return $updates;
}

/**
 * @param array<string, array<int, string>> $rules
 * @param array<int, string> $update
 *
 * @return array<int, string>
 */
function getBrokenRules(array $rules, array $update, string $value): array
{

    $brokenRules = array_intersect(
        $rules[$value] ?? [],
        array_slice($update, 0, array_search($value, $update)),
    );

    return array_values($brokenRules);
}

/**
 * @param array<string, array<int, string>> $rules
 * @param array<int, string> $update
 */
function isGood(array $rules, array $update): bool
{
    foreach (array_reverse($update) as $u) {
        if (getBrokenRules($rules, $update, $u) != []) {
            return false;
        }
    }

    return true;
}

/**
 * @param array<string, array<int, string>> $rules
 * @param array<int, string> $update
 *
 * @return array<int, string>
 */
function reorder(array $rules, array $update): array
{
    while (!isGood($rules, $update)) {
        foreach (array_reverse($update) as $u) {
            $brokenRules = getBrokenRules($rules, $update, $u);
            if ($brokenRules != []) {
                $l = array_search($brokenRules[0], $update);
                $r = array_search($u, $update);

                [$update[$l], $update[$r]] = [$update[$r], $update[$l]];

                continue 2;
            }
        }
    }

    return $update;
}

/**
 * @param array<string, array<int, string>> $rules
 * @param array<int, array<int, string>> $updates
 */
function part1(array $rules, array $updates): int
{
    $count = 0;
    foreach ($updates as $update) {
        if (isGood($rules, $update)) {
            $count += (int)$update[floor(count($update) / 2)];
        }
    }

    return $count;
}


/**
 * @param array<string, array<int, string>> $rules
 * @param array<int, array<int, string>> $updates
 */
function part2(array $rules, array $updates): int
{
    $count = 0;
    foreach ($updates as $update) {
        if (isGood($rules, $update)) {
            continue;
        }
        $update = reorder($rules, $update);
        $count += (int)$update[floor(count($update) / 2)];
    }

    return $count;
}

[$sampleRules, $sampleUpdates] = parseInput("sample.txt");
$sampleRules = parseRules($sampleRules);
$sampleUpdates = parseUpdates($sampleUpdates);

[$inputRules, $inputUpdates] = parseInput("input.txt");
$inputRules = parseRules($inputRules);
$inputUpdates = parseUpdates($inputUpdates);

$p1Sample = part1($sampleRules, $sampleUpdates);
$p1Input = part1($inputRules, $inputUpdates);
assert($p1Sample == 143);
assert($p1Input == 6242);

$p2Sample = part2($sampleRules, $sampleUpdates);
$p2Input = part2($inputRules, $inputUpdates);
assert($p2Sample == 123);
assert($p2Input == 5169);

printf("PART 1:\n");
printf("    Sample: %d\n", $p1Sample);
printf("    Input:  %d\n", $p1Input);

printf("PART 2:\n");
printf("    Sample: %d\n", $p2Sample);
printf("    Input:  %d\n", $p2Input);
