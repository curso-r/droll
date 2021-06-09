# roll_outcome_count() works

    Code
      roll_outcome_count("3*2d6 + d20 + 1d10 - 1 +    4")
    Output
      # A tibble: 59 x 2
         outcome count
           <dbl> <dbl>
       1      11     1
       2      12     2
       3      13     3
       4      14     6
       5      15     9
       6      16    12
       7      17    18
       8      18    24
       9      19    30
      10      20    40
      # ... with 49 more rows

---

    Code
      roll_outcome_count("40d6")
    Output
      # A tibble: 201 x 2
         outcome      count
           <dbl>      <dbl>
       1      40          1
       2      41         40
       3      42        820
       4      43      11480
       5      44     123410
       6      45    1086008
       7      46    8145020
       8      47   53523080
       9      48  314424695
      10      49 1676647440
      # ... with 191 more rows

