def tail:
    if length then .[1:] else [] end
;

def parse_seeds:
    split(": ")
    | last
    | split(" ")
    | map(tonumber)
;

def expand_range:
    (split(" ") | map(tonumber)) as [$dst, $src, $len]
    | reduce range($len) as $n ({}; . + {($src+$n | tostring): ($dst+$n | tostring)})
;

def parse_mapping:
    split("\n")
    | tail
    | map(split(" ") | map(tonumber))
;

def parse_input:
    rtrimstr("\n")
    | split("\n\n")
    | [(first | parse_seeds), (tail | map(parse_mapping))]
;

def in_range($n; $l; $r): $n >= $l and $n <= $r;

def map_num($n; $mappings):
    if ($mappings | isempty(.[])) then
        $n
    else
        ($mappings | first) as [$dst, $src, $len]
        | if in_range($n; $src; $src+$len-1) then
              (($n-$src)+$dst)
          else
              map_num($n; ($mappings | tail))
          end
    end
;

def map_seed($n; $mappings):
    if ($mappings | length) == 0 then
        $n
    else
        map_num($n; ($mappings | first)) as $m
        | map_seed($m; ($mappings | tail))
    end
;

parse_input
| . as [$seeds, $mappings]
| $seeds
| map(map_seed(.; $mappings))
| min



