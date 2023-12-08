def map_card:
    {
        "A": 14,
        "K": 13,
        "Q": 12,
        "J": 11,
        "T": 10
    }[.] // tonumber
;

def classify_hand:
    . / ""
    | group_by(.)
    | map(length)
    | sort
    | reverse
    | if first == 5 then # Five of a kind
        6
      elif first == 4 then # Four of a kind
        5
      elif first == 3 and last == 2 then # Full house
        4
      elif first == 3 then # Three of a kind
        3
      elif first == 2 and length == 3 then # Two pair
        2
      elif first == 2 then # One pair
        1
      else # High card
        0
      end
;

def parse_line:
    split(" ")
    | {
        "hand": first,
        "hand_values": (first | . / "" | map(map_card)),
        "hand_type": (first | classify_hand),
        "bid": (last | tonumber)
      }
;

def parse_input:
    rtrimstr("\n")
    | split("\n")
    | map(parse_line)
;

def calculate_winnings:
    def f($bids; $n):
        if ($bids | length) == 0 then
            0
        else
            ($bids[0] * $n) + f($bids[1:]; $n + 1)
        end
    ;

    f(.;1)
;

parse_input
| sort_by(.hand_type, .hand_values)
| map(.bid)
| calculate_winnings


