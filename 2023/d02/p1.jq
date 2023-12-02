def parse_group:
    split(" ") | last | tonumber
;

def parse_round:
    split(", ")
    | reduce .[] as $item (
        {};
        . + ($item | split(" ") | {(last): (first | tonumber)})
      )
;

def parse_rounds:
    split("; ")
    | map(parse_round)
;

def parse_line:
    split(": ")
    | {"id": (first | parse_group), "rounds": (last  | parse_rounds)}
;

def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(parse_line)
;

def max_cubes($color):
    { "red": 12, "green": 13, "blue": 14 }
    | .[$color]
;

def is_valid_round:
    (.["red"]? // 0) <= max_cubes("red")
    and (.["blue"]? // 0) <= max_cubes("blue")
    and (.["green"]? // 0) <= max_cubes("green")
;

def is_valid_game:
    .id as $id
    | reduce .rounds[] as $round (true; . and ($round | is_valid_round))
    | if . then $id else 0 end
;

parse_input
| map(is_valid_game)
| add

