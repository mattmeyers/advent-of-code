def parse_input:
    rtrimstr("\n") | split("\n")
;

def map_number:
    if . == "one" then "1"
    elif . == "two" then "2"
    elif . == "three" then "3"
    elif . == "four" then "4"
    elif . == "five" then "5"
    elif . == "six" then "6"
    elif . == "seven" then "7"
    elif . == "eight" then "8"
    elif . == "nine" then "9"
    else .
    end
;

def str_reverse:
    explode | reverse | implode
;

def first_num:
    capture("(?<num>one|two|three|four|five|six|seven|eight|nine|[0-9])"; "")
    | .num
    | map_number
;

def first_num_rev:
    capture("(?<num>enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|[0-9])"; "")
    | .num
    | str_reverse
    | map_number
;


parse_input
| map(first_num + (str_reverse | first_num_rev))
| map(tonumber)
| add
