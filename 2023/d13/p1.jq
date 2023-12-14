def parse_input:
    rtrimstr("\n")
    | split("\n\n")
    | map(split("\n"))
;

def transpose_grid:
    map(. / "") | transpose | map(add)
;

def find_mirrors:
    . as $rows
    | length as $len
    | reduce range(0; (length-1)) as $i ([];
        ([($len-$i-2), $i] | min) as $n
        | if ($rows[($i-$n):($i+1)]) == (($rows[($i+1):($i + 2 + $n)]) | reverse) then
            . + [$i + 1]
        else
            .
        end
      )
;

parse_input
| map((find_mirrors | map(. * 100) | add) + (transpose_grid | find_mirrors | add))
| add
