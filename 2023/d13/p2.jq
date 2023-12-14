def parse_input:
    rtrimstr("\n")
    | split("\n\n")
    | map(split("\n") | map(. / ""))
;

def diff($a; $b):
    if ($a | length) == 0 then
        0
    else
        (if $a[0] != $b[0] then 1 else 0 end) + diff($a[1:]; $b[1:])
    end
;

def diff_range($as; $bs):
    if ($as | length) == 0 then
        0
    else
        diff($as[0]; $bs[0]) + diff_range($as[1:]; $bs[1:])
    end
;

def find_mirrors:
    . as $rows
    | length as $len
    | reduce range(0; (length-1)) as $i (
        [];
        ([($len-$i-2), $i] | min) as $n
        | if diff_range(($rows[($i-$n):($i+1)]); (($rows[($i+1):($i + 2 + $n)]) | reverse)) == 1 then
            . + [$i + 1]
        else
            .
        end
      )
;

parse_input
| map((find_mirrors | map(. * 100) | add) + (transpose | find_mirrors | add))
| add
