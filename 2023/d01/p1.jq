def parse_input:
    rtrimstr("\n") | split("\n")
;

def is_digit:
    . >= "0" and . <= "9"   
;

def first_num:
    [split("") | .[] | select(is_digit)] | first
;

def str_reverse:
    explode | reverse | implode
;


parse_input
| map(first_num + (str_reverse | first_num))
| map(tonumber)
| add
