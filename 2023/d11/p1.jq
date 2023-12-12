def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(. / "")
;

def expand:
    reduce .[] as $row ([]; if ($row | all(. == ".")) then . + [$row, $row] else . + [$row] end)
;


def find_galaxies:
    to_entries
    | map(
        .key as $row
        | .value
        | to_entries
        | map(select(.value == "#") | {"x": $row, "y": .key})
      )
    | flatten 
;

def calc_distances($galaxies):
    $galaxies[0] as $g1
    | reduce $galaxies[1:][] as $g (0; . + (($g1.x - $g.x) | abs) + (($g1.y - $g.y) | abs))
;

def sum_distances($galaxies):
    if ($galaxies | length) == 1 then
        0
    else
        calc_distances($galaxies) + sum_distances($galaxies[1:])    
    end
;

parse_input
| expand
| transpose
| expand
| transpose
| find_galaxies
| sum_distances(.)
