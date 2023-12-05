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

def tail:
    if length > 1 then .[1:] else [] end
;

def increment_run($arr; $amount; $len):
    ($arr[0:$len] | map(. + $amount)) + $arr[$len:]
;

def rep($num; $val):
    reduce range($num) as $_ ([]; . + [$val])
;

def propagate($counts; $winners):
    if ($counts | length) == 0 then
        [($counts | first)]
    else
        increment_run(($counts | tail); ($counts | first); ($winners | first))
        | [$counts | first] + propagate(.; ($winners | tail))
    end
;

parse_input
| map(parse_line)
| map(count_winners)
| propagate(rep(length; 1); .)
| add

