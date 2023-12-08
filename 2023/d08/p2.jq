def map_direction: if . == "L" then 0 else 1 end ;

def parse_next:
    ltrimstr("(")
    | rtrimstr(")")
    | split(", ")
;

def parse_mapping:
   split(" = ")
   | {(first): (last | parse_next)}
;

def parse_input:
    rtrimstr("\n")
    | split("\n\n")
    | {
        "directions": (first | . / "" | map(map_direction)),
        "mappings": (last | split("\n") | map(parse_mapping) | add)
      }
;

def walk:
    .directions as $base_directions
    | def _walk($current; $directions; $mappings):
        if ($current | endswith("Z")) then
            0
        else
            1 + _walk(
                    $mappings[$current][$directions[0]];
                    if ($directions | length) == 1 then $base_directions else $directions[1:] end;
                    $mappings
                )
        end
    ;

     .mappings as $mappings
    | .mappings
    | keys
    | map(select(endswith("A")))
    | map(_walk(.; $base_directions; $mappings))
;

def gcd($a; $b):
    if $b == 0 then
        $a
    else
        gcd($b; $a % $b)
    end
;

def lcm: reduce .[1:][] as $n (.[0]; ($n * .) / (gcd($n; .))) ;

parse_input | walk | lcm
