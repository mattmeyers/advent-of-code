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
        if $current == "ZZZ" then
            0
        elif ($directions | length) == 0 then
            _walk($current; $base_directions; $mappings)
        else
            1 + _walk($mappings[$current][$directions[0]]; $directions[1:]; $mappings)
        end
    ;

    _walk("AAA"; .directions; .mappings)
;

parse_input | walk
