def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(. / "")
;

def expand:
    to_entries
    | reduce .[] as $row ([]; if ($row.value | all(. == ".")) then . + [$row.key] else . end)
;


def find_galaxies:
    to_entries
    | map(
        .key as $row
        | .value
        | to_entries
        | map(select(.value == "#") | {"y": $row, "x": .key})
      )
    | flatten 
;

def get_expansions($min; $max; $exps):
    ($exps | length) - (($exps - [range($min+1;$max)]) | length)
;

def calc_distances($galaxies; $expansions):
    $galaxies[0] as $g1
    | 1000000 as $rate
    | reduce $galaxies[1:][] as $g (0; 
        get_expansions(([$g1.x, $g.x] | min); ([$g1.x, $g.x] | max); $expansions.cols) as $x_exps
        | get_expansions(([$g1.y, $g.y] | min); ([$g1.y, $g.y] | max); $expansions.rows) as $y_exps
        | .
            + (($g1.x - $g.x) | abs | . - $x_exps + ($x_exps * $rate))
            + (($g1.y - $g.y) | abs | . - $y_exps + ($y_exps * $rate)))
;

def sum_distances($galaxies; $expansions):
    if ($galaxies | length) == 1 then
        0
    else
        calc_distances($galaxies; $expansions) + sum_distances($galaxies[1:]; $expansions)    
    end
;

parse_input
| {"rows": expand, "cols": (transpose | expand)} as $expansions
| find_galaxies as $galaxies
| sum_distances($galaxies; $expansions)

