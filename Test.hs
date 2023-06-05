module Test where

import Expr

tests = [test1, test2, test3, test4, test5, test6, test7]

test1 :: Expr
test1 = Or (And (Var "x") (Var "y")) (Or (Var "z") (Not (Var "w")))

test2 :: Expr
test2 = Or
  (And
    (Or
      (And (Var "x1") (Var "x2"))
      (And (Var "x3") (Var "x4"))
    )
    (Or
      (And (Var "x5") (Var "x6"))
      (And (Var "x7") (Var "x8"))
    )
  )
  (And
    (Or
      (And (Var "x9") (Var "x10"))
      (And (Var "x11") (Var "x12"))
    )
    (Or
      (And (Var "x13") (Var "x14"))
      (And (Var "x15") (Var "x16"))
    )
  )

test3 :: Expr
test3 = Or
  (And
    (Or
      (And (Var "x1") (Var "x2"))
      (And (Var "x3") (Var "x4"))
    )
    (Or
      (And (Var "x5") (Var "x6"))
      (And (Var "x7") (Var "x8"))
    )
  )
  (Or
    (And
      (Or
        (And (Var "x9") (Var "x10"))
        (And (Var "x11") (Var "x12"))
      )
      (Or
        (And (Var "x13") (Var "x14"))
        (And (Var "x15") (Var "x16"))
      )
    )
    (Or
      (And
        (Or
          (And (Var "x17") (Var "x18"))
          (And (Var "x19") (Var "x20"))
        )
        (Or
          (And (Var "x21") (Var "x22"))
          (And (Var "x23") (Var "x24"))
        )
      )
      (And
        (Or
          (And (Var "x25") (Var "x26"))
          (And (Var "x27") (Var "x28"))
        )
        (Or
          (And (Var "x29") (Var "x30"))
          (And (Var "x31") (Var "x32"))
        )
      )
    )
  )

test4 :: Expr
test4 = Or
  (And
    (Or
      (And (Var "x1") (Var "x2"))
      (And (Var "x3") (Var "x4"))
    )
    (Or
      (And (Var "x5") (Var "x6"))
      (And (Var "x7") (Var "x8"))
    )
  )
  (And
    (Or
      (And (Var "x9") (Var "x10"))
      (And (Var "x11") (Var "x12"))
    )
    (Or
      (And (Var "x13") (Var "x14"))
      (And (Var "x15") (Var "x16"))
    )
  )

-- Bigger tests are currently in CNF form, bacause
-- converting a huge clause to the CNF is very slow.

test5 :: Expr
test5 = foldr1 And
  [ foldr1 Or
    [ Var ("x" ++ show i),
      Not (Var ("y" ++ show i)),
      Var ("z" ++ show i),
      Not (Var ("t" ++ show i))
    ]
  | i <- [1..800]
  ]

test6 :: Expr
test6 = foldr1 And
  [ foldr1 Or
    [ Var ("x" ++ show i),
      Not (Var ("x" ++ show (i - 1))),
      Var ("y" ++ show (i - 1)),
      Not (Var ("y" ++ show i))
    ]
  | i <- [1..500]
  ]

test7 :: Expr
test7 = foldr1 And
  [ foldr1 Or
    [ Var ("x"),
      Var ("y"),
      Var ("z")
    ],
    foldr1 Or
    [ Not (Var ("x")),
      Var ("y"),
      Var ("z")
    ],
    foldr1 Or
    [ Var ("x"),
      Not (Var ("y")),
      Var ("z")
    ],
    foldr1 Or
    [ Var ("x"),
      Var ("y"),
      Not (Var ("z"))
    ],
    foldr1 Or
    [ Not (Var ("x")),
      Not (Var ("y")),
      Var ("z")
    ],
    foldr1 Or
    [ Not (Var ("x")),
      Var ("y"),
      Not (Var ("z"))
    ],
    foldr1 Or
    [ Var ("x"),
      Not (Var ("y")),
      Not (Var ("z"))
    ],
    foldr1 Or
    [ Not (Var ("x")),
      Not (Var ("y")),
      Not (Var ("z"))
    ]
  ]
