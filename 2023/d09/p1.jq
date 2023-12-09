def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(split(" ") | map(tonumber))
;

def diff_row:
    if (. | length) == 1 then
        []
    else
        [nth(1) - first] + (.[1:] | diff_row)
    end
;

def process($arr):
    if ($arr | all(. == 0)) then
        0
    else
        diff_row
        | ($arr | last) + process(.)
    end
;

parse_input  | map(process(.)) | add
