(program
  1.0.0
  [
    (lam i_2 [ [ i_2 i_2 ] (con integer 0) ])
    (lam
      i_3
      (lam
        i_4
        [
          (lam
            i_6
            (force
              [
                [
                  [
                    (force (builtin ifThenElse))
                    [ [ (builtin lessThanEqualsInteger) i_4 ] (con integer 1) ]
                  ]
                  (delay i_4)
                ]
                (delay
                  [
                    [
                      (builtin addInteger)
                      [
                        i_6
                        [ [ (builtin subtractInteger) i_4 ] (con integer 1) ]
                      ]
                    ]
                    [
                      i_6 [ [ (builtin subtractInteger) i_4 ] (con integer 2) ]
                    ]
                  ]
                )
              ]
            )
          )
          [ i_3 i_3 ]
        ]
      )
    )
  ]
)
