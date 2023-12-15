def parse_input: rtrimstr("\n") | split(",");

def hash_word:
    explode | reduce .[] as $c (0; (17 * (. + $c)) % 256)
;

parse_input | map(hash_word) | add
