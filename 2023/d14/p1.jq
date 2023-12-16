def parse_input: rtrimstr("\n") | split("\n");

def transpose_grid: map(explode) | transpose | map(reverse | implode);

def slide_row:
    split("#")
    | map(split("") | sort | join(""))
    | join("#")
;

def calculate_load:
    split("")
    | to_entries
    | map(select(.value == "O") | .key + 1)
    | add
;

parse_input
| transpose_grid
| map(slide_row | calculate_load)
| add
