# Algorithm

| n | len |
|---|-----|
| 0 |  6  |
| 1 |  2  |
| 2 |  5  |
| 3 |  5  |
| 4 |  4  |
| 5 |  5  |
| 6 |  6  |
| 7 |  3  |
| 8 |  7  |
| 9 |  6  |
 
1. Diff of 1 and 7 is the top
2. Find 6 (len of 6, doesn't contain 1)
3. Bottom right is intersection of 1 and 6
4. Top right is difference of 1 and 6
5. Find 2 (len is 5, doesn't contain top right)
6. Middle is intersection of 2 and 4 without 1
7. Top left is remaining part of 4
8. Find 3 (len is 5, contains 1)
9. Bottom is intersection of 8 and 3 with 4
10. Bottom left is remaining part of 8
