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
    | last
    | parse_rounds
;

def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(parse_line)
;

def determine_maxima:
    reduce .[] as $round (
      {"red": 0, "green": 0, "blue": 0};
      {
          "red": ([.["red"], $round.red] | max),
          "green": ([.["green"], $round.green] | max),
          "blue": ([.["blue"], $round.blue] | max),
      }
    )
;

def calculate_power:
    [.[]]
    | reduce .[] as $n (1; . * $n)
;

parse_input
| map(determine_maxima)
| map(calculate_power)
| add

