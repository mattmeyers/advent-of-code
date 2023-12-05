def parse_input:
    rtrimstr("\n") | split("\n")
;

def parse_line:
    split(": ")
    | last
    | split(" | ")
    | map(
        ltrimstr(" ")
        | split(" +"; "")
        | map(tonumber)
      ) 
;

def count_winners:
    last as $nums
    | first as $winners
    | $nums - $winners
    | ($nums | length) - length
;

def calc_score:
    if . > 0 then pow(2; .-1) else 0 end
;

parse_input
| map(parse_line | count_winners | calc_score)
| add

