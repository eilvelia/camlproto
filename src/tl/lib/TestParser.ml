open Parser

let p str = match parse_string str with
  | Ok ast -> print_endline @@ Ast.show ast
  | Error e -> prerr_endline @@ Err.Parse.show e

let%expect_test "empty file" =
  p "";
  [%expect {| { constructors = []; functions = [] } |}]

let%expect_test "sections: ---types--- and ---functions---" =
  p "
a = A;
---types---
b = B;
---types---
c = C;
---functions---
d = D;
---functions---
e = E;
---types---
f = F;
  ";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:1-2:7,
           { comb_id = (:2:1-2:2, (CombIdShort (Name "a"))); comb_opt_args = [];
             comb_args = [];
             comb_result = (:2:5-2:6, (EIdent (:2:5-2:6, (IdBoxed (Name "A")))))
             }));
        (CombinatorDecl
           (:4:1-4:7,
            { comb_id = (:4:1-4:2, (CombIdShort (Name "b"))); comb_opt_args = [];
              comb_args = [];
              comb_result = (:4:5-4:6, (EIdent (:4:5-4:6, (IdBoxed (Name "B")))))
              }));
        (CombinatorDecl
           (:6:1-6:7,
            { comb_id = (:6:1-6:2, (CombIdShort (Name "c"))); comb_opt_args = [];
              comb_args = [];
              comb_result = (:6:5-6:6, (EIdent (:6:5-6:6, (IdBoxed (Name "C")))))
              }));
        (CombinatorDecl
           (:12:1-12:7,
            { comb_id = (:12:1-12:2, (CombIdShort (Name "f")));
              comb_opt_args = []; comb_args = [];
              comb_result =
              (:12:5-12:6, (EIdent (:12:5-12:6, (IdBoxed (Name "F"))))) }))
        ];
      functions =
      [(CombinatorDecl
          (:8:1-8:7,
           { comb_id = (:8:1-8:2, (CombIdShort (Name "d"))); comb_opt_args = [];
             comb_args = [];
             comb_result = (:8:5-8:6, (EIdent (:8:5-8:6, (IdBoxed (Name "D")))))
             }));
        (CombinatorDecl
           (:10:1-10:7,
            { comb_id = (:10:1-10:2, (CombIdShort (Name "e")));
              comb_opt_args = []; comb_args = [];
              comb_result =
              (:10:5-10:6, (EIdent (:10:5-10:6, (IdBoxed (Name "E"))))) }))
        ]
      } |}]

let%expect_test "combinator with magic and named arguments" =
  p "
p_q_inner_data_temp#3c6a84d4 pq:bytes p:bytes expires_in:int = P_Q_inner_data;
  ";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:1-2:79,
           { comb_id =
             (:2:1-2:29, (CombIdFull ((Name "p_q_inner_data_temp"), "3c6a84d4")));
             comb_opt_args = [];
             comb_args =
             [(:2:30-2:38,
               { arg_id = (Some (:2:30-2:32, "pq")); arg_cond_def = None;
                 arg_type =
                 (:2:33-2:38, (EIdent (:2:33-2:38, (IdBare (Name "bytes"))))) });
               (:2:39-2:46,
                { arg_id = (Some (:2:39-2:40, "p")); arg_cond_def = None;
                  arg_type =
                  (:2:41-2:46, (EIdent (:2:41-2:46, (IdBare (Name "bytes"))))) });
               (:2:47-2:61,
                { arg_id = (Some (:2:47-2:57, "expires_in"));
                  arg_cond_def = None;
                  arg_type =
                  (:2:58-2:61, (EIdent (:2:58-2:61, (IdBare (Name "int"))))) })
               ];
             comb_result =
             (:2:64-2:78,
              (EIdent (:2:64-2:78, (IdBoxed (Name "P_Q_inner_data")))))
             }))
        ];
      functions = [] } |}]

let%expect_test "combinator with namespace" =
  p "namespace.name pq:bytes p:bytes = P_Q_inner_data;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:50,
           { comb_id = (:1:1-1:15, (CombIdShort (NameNs ("namespace", "name"))));
             comb_opt_args = [];
             comb_args =
             [(:1:16-1:24,
               { arg_id = (Some (:1:16-1:18, "pq")); arg_cond_def = None;
                 arg_type =
                 (:1:19-1:24, (EIdent (:1:19-1:24, (IdBare (Name "bytes"))))) });
               (:1:25-1:32,
                { arg_id = (Some (:1:25-1:26, "p")); arg_cond_def = None;
                  arg_type =
                  (:1:27-1:32, (EIdent (:1:27-1:32, (IdBare (Name "bytes"))))) })
               ];
             comb_result =
             (:1:35-1:49,
              (EIdent (:1:35-1:49, (IdBoxed (Name "P_Q_inner_data")))))
             }))
        ];
      functions = [] } |}]

let%expect_test "combinator with namespace and magic" =
  p "namespace.name#3c6a84d4 pq:bytes p:bytes = P_Q_inner_data;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:59,
           { comb_id =
             (:1:1-1:24,
              (CombIdFull ((NameNs ("namespace", "name")), "3c6a84d4")));
             comb_opt_args = [];
             comb_args =
             [(:1:25-1:33,
               { arg_id = (Some (:1:25-1:27, "pq")); arg_cond_def = None;
                 arg_type =
                 (:1:28-1:33, (EIdent (:1:28-1:33, (IdBare (Name "bytes"))))) });
               (:1:34-1:41,
                { arg_id = (Some (:1:34-1:35, "p")); arg_cond_def = None;
                  arg_type =
                  (:1:36-1:41, (EIdent (:1:36-1:41, (IdBare (Name "bytes"))))) })
               ];
             comb_result =
             (:1:44-1:58,
              (EIdent (:1:44-1:58, (IdBoxed (Name "P_Q_inner_data")))))
             }))
        ];
      functions = [] } |}]

let%expect_test "magic with 4 hexdigits" =
  p "name#abcd pq:bytes p:bytes = P_Q_inner_data;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:45,
           { comb_id = (:1:1-1:10, (CombIdFull ((Name "name"), "abcd")));
             comb_opt_args = [];
             comb_args =
             [(:1:11-1:19,
               { arg_id = (Some (:1:11-1:13, "pq")); arg_cond_def = None;
                 arg_type =
                 (:1:14-1:19, (EIdent (:1:14-1:19, (IdBare (Name "bytes"))))) });
               (:1:20-1:27,
                { arg_id = (Some (:1:20-1:21, "p")); arg_cond_def = None;
                  arg_type =
                  (:1:22-1:27, (EIdent (:1:22-1:27, (IdBare (Name "bytes"))))) })
               ];
             comb_result =
             (:1:30-1:44,
              (EIdent (:1:30-1:44, (IdBoxed (Name "P_Q_inner_data")))))
             }))
        ];
      functions = [] } |}]

let%expect_test "# as nat" =
  p "a flags:# = A;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:15,
           { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
             comb_args =
             [(:1:3-1:10,
               { arg_id = (Some (:1:3-1:8, "flags")); arg_cond_def = None;
                 arg_type =
                 (:1:9-1:10, (EIdent (:1:9-1:10, (IdBare (Name "nat"))))) })
               ];
             comb_result =
             (:1:13-1:14, (EIdent (:1:13-1:14, (IdBoxed (Name "A"))))) }))
        ];
      functions = [] } |}]

let%expect_test "optional arguments" =
  p "a {X:Type} {Y:#} {Z:nat} = A X;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:32,
           { comb_id = (:1:1-1:2, (CombIdShort (Name "a")));
             comb_opt_args =
             [(:1:3-1:11,
               { opt_arg_id = (:1:4-1:5, "X");
                 opt_arg_type =
                 (:1:6-1:10, (EIdent (:1:6-1:10, (IdBoxed (Name "Type"))))) });
               (:1:12-1:17,
                { opt_arg_id = (:1:13-1:14, "Y");
                  opt_arg_type =
                  (:1:15-1:16, (EIdent (:1:15-1:16, (IdBare (Name "nat"))))) });
               (:1:18-1:25,
                { opt_arg_id = (:1:19-1:20, "Z");
                  opt_arg_type =
                  (:1:21-1:24, (EIdent (:1:21-1:24, (IdBare (Name "nat"))))) })
               ];
             comb_args = [];
             comb_result =
             (:1:28-1:29,
              (EAppl ((:1:28-1:29, (EIdent (:1:28-1:29, (IdBoxed (Name "A"))))),
                 [(:1:30-1:31, (EIdent (:1:30-1:31, (IdBoxed (Name "X")))))])))
             }))
        ];
      functions = [] } |}]

let%expect_test "application in an optional argument type" =
  p "a {X : T t } = A;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:18,
           { comb_id = (:1:1-1:2, (CombIdShort (Name "a")));
             comb_opt_args =
             [(:1:3-1:13,
               { opt_arg_id = (:1:4-1:5, "X");
                 opt_arg_type =
                 (:1:8-1:11,
                  (EAppl ((:1:8-1:9, (EIdent (:1:8-1:9, (IdBoxed (Name "T"))))),
                     [(:1:10-1:11, (EIdent (:1:10-1:11, (IdBare (Name "t")))))])))
                 })
               ];
             comb_args = [];
             comb_result =
             (:1:16-1:17, (EIdent (:1:16-1:17, (IdBoxed (Name "A"))))) }))
        ];
      functions = [] } |}]

let%expect_test "function as a type param" =
  p "invokeWithLayer#da9b0d0d {X:Type} layer:int query:!X = X;";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:1:1-1:58,
           { comb_id =
             (:1:1-1:25, (CombIdFull ((Name "invokeWithLayer"), "da9b0d0d")));
             comb_opt_args =
             [(:1:26-1:34,
               { opt_arg_id = (:1:27-1:28, "X");
                 opt_arg_type =
                 (:1:29-1:33, (EIdent (:1:29-1:33, (IdBoxed (Name "Type"))))) })
               ];
             comb_args =
             [(:1:35-1:44,
               { arg_id = (Some (:1:35-1:40, "layer")); arg_cond_def = None;
                 arg_type =
                 (:1:41-1:44, (EIdent (:1:41-1:44, (IdBare (Name "int"))))) });
               (:1:45-1:53,
                { arg_id = (Some (:1:45-1:50, "query")); arg_cond_def = None;
                  arg_type =
                  (:1:51-1:53,
                   (EOperator ((:1:51-1:52, OpBang),
                      [(:1:52-1:53, (EIdent (:1:52-1:53, (IdBoxed (Name "X")))))]
                      )))
                  })
               ];
             comb_result =
             (:1:56-1:57, (EIdent (:1:56-1:57, (IdBoxed (Name "X"))))) }))
        ];
      functions = [] } |}]

let%expect_test "! in result type" =
  p "
  a = !T;
  b = !T c;
  ";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:3-2:10,
           { comb_id = (:2:3-2:4, (CombIdShort (Name "a"))); comb_opt_args = [];
             comb_args = [];
             comb_result =
             (:2:7-2:9,
              (EOperator ((:2:7-2:8, OpBang),
                 [(:2:8-2:9, (EIdent (:2:8-2:9, (IdBoxed (Name "T")))))])))
             }));
        (CombinatorDecl
           (:3:3-3:12,
            { comb_id = (:3:3-3:4, (CombIdShort (Name "b"))); comb_opt_args = [];
              comb_args = [];
              comb_result =
              (:3:7-3:11,
               (EOperator ((:3:7-3:8, OpBang),
                  [(:3:8-3:9,
                    (EAppl (
                       (:3:8-3:9, (EIdent (:3:8-3:9, (IdBoxed (Name "T"))))),
                       [(:3:10-3:11, (EIdent (:3:10-3:11, (IdBare (Name "c")))))]
                       )))
                    ]
                  )))
              }))
        ];
      functions = [] } |}]

let%expect_test "bare types: %" =
  p "
a %T = A;
b %(T a) = B;
c %T<a> = C;
d (%T a) = D;
  ";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:1-2:10,
           { comb_id = (:2:1-2:2, (CombIdShort (Name "a"))); comb_opt_args = [];
             comb_args =
             [(:2:3-2:10,
               { arg_id = None; arg_cond_def = None;
                 arg_type =
                 (:2:3-2:5,
                  (EOperator ((:2:3-2:4, OpBare),
                     [(:2:4-2:5, (EIdent (:2:4-2:5, (IdBoxed (Name "T")))))])))
                 })
               ];
             comb_result = (:2:8-2:9, (EIdent (:2:8-2:9, (IdBoxed (Name "A")))))
             }));
        (CombinatorDecl
           (:3:1-3:14,
            { comb_id = (:3:1-3:2, (CombIdShort (Name "b"))); comb_opt_args = [];
              comb_args =
              [(:3:3-3:14,
                { arg_id = None; arg_cond_def = None;
                  arg_type =
                  (:3:3-3:9,
                   (EOperator ((:3:3-3:4, OpBare),
                      [(:3:5-3:8,
                        (EAppl (
                           (:3:5-3:6, (EIdent (:3:5-3:6, (IdBoxed (Name "T"))))),
                           [(:3:7-3:8, (EIdent (:3:7-3:8, (IdBare (Name "a")))))]
                           )))
                        ]
                      )))
                  })
                ];
              comb_result =
              (:3:12-3:13, (EIdent (:3:12-3:13, (IdBoxed (Name "B"))))) }));
        (CombinatorDecl
           (:4:1-4:13,
            { comb_id = (:4:1-4:2, (CombIdShort (Name "c"))); comb_opt_args = [];
              comb_args =
              [(:4:3-4:13,
                { arg_id = None; arg_cond_def = None;
                  arg_type =
                  (:4:3-4:8,
                   (EOperator ((:4:3-4:4, OpBare),
                      [(:4:4-4:8,
                        (EAppl (
                           (:4:4-4:5, (EIdent (:4:4-4:5, (IdBoxed (Name "T"))))),
                           [(:4:6-4:7, (EIdent (:4:6-4:7, (IdBare (Name "a")))))]
                           )))
                        ]
                      )))
                  })
                ];
              comb_result =
              (:4:11-4:12, (EIdent (:4:11-4:12, (IdBoxed (Name "C"))))) }));
        (CombinatorDecl
           (:5:1-5:14,
            { comb_id = (:5:1-5:2, (CombIdShort (Name "d"))); comb_opt_args = [];
              comb_args =
              [(:5:3-5:14,
                { arg_id = None; arg_cond_def = None;
                  arg_type =
                  (:5:4-5:8,
                   (EAppl (
                      (:5:4-5:6,
                       (EOperator ((:5:4-5:5, OpBare),
                          [(:5:5-5:6, (EIdent (:5:5-5:6, (IdBoxed (Name "T")))))]
                          ))),
                      [(:5:7-5:8, (EIdent (:5:7-5:8, (IdBare (Name "a")))))])))
                  })
                ];
              comb_result =
              (:5:12-5:13, (EIdent (:5:12-5:13, (IdBoxed (Name "D"))))) }))
        ];
      functions = [] } |}]

let%test_module "arguments" = (module struct
  let%expect_test "uppercase arguments" =
    p "a a:string B:string = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:25,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:11,
                 { arg_id = (Some (:1:3-1:4, "a")); arg_cond_def = None;
                   arg_type =
                   (:1:5-1:11, (EIdent (:1:5-1:11, (IdBare (Name "string"))))) });
                 (:1:12-1:20,
                  { arg_id = (Some (:1:12-1:13, "B")); arg_cond_def = None;
                    arg_type =
                    (:1:14-1:20, (EIdent (:1:14-1:20, (IdBare (Name "string"))))) })
                 ];
               comb_result =
               (:1:23-1:24, (EIdent (:1:23-1:24, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "unnamed arguments" =
    p "
    t1 a b c d e = T;
    t2 a = T;
    t3 (T t) = T;
    ";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:2:5-2:22,
             { comb_id = (:2:5-2:7, (CombIdShort (Name "t1"))); comb_opt_args = [];
               comb_args =
               [(:2:8-2:22,
                 { arg_id = None; arg_cond_def = None;
                   arg_type = (:2:8-2:9, (EIdent (:2:8-2:9, (IdBare (Name "a")))))
                   });
                 (:2:8-2:22,
                  { arg_id = None; arg_cond_def = None;
                    arg_type =
                    (:2:10-2:11, (EIdent (:2:10-2:11, (IdBare (Name "b"))))) });
                 (:2:8-2:22,
                  { arg_id = None; arg_cond_def = None;
                    arg_type =
                    (:2:12-2:13, (EIdent (:2:12-2:13, (IdBare (Name "c"))))) });
                 (:2:8-2:22,
                  { arg_id = None; arg_cond_def = None;
                    arg_type =
                    (:2:14-2:15, (EIdent (:2:14-2:15, (IdBare (Name "d"))))) });
                 (:2:8-2:22,
                  { arg_id = None; arg_cond_def = None;
                    arg_type =
                    (:2:16-2:17, (EIdent (:2:16-2:17, (IdBare (Name "e"))))) })
                 ];
               comb_result =
               (:2:20-2:21, (EIdent (:2:20-2:21, (IdBoxed (Name "T"))))) }));
          (CombinatorDecl
             (:3:5-3:14,
              { comb_id = (:3:5-3:7, (CombIdShort (Name "t2")));
                comb_opt_args = [];
                comb_args =
                [(:3:8-3:14,
                  { arg_id = None; arg_cond_def = None;
                    arg_type = (:3:8-3:9, (EIdent (:3:8-3:9, (IdBare (Name "a")))))
                    })
                  ];
                comb_result =
                (:3:12-3:13, (EIdent (:3:12-3:13, (IdBoxed (Name "T"))))) }));
          (CombinatorDecl
             (:4:5-4:18,
              { comb_id = (:4:5-4:7, (CombIdShort (Name "t3")));
                comb_opt_args = [];
                comb_args =
                [(:4:8-4:18,
                  { arg_id = None; arg_cond_def = None;
                    arg_type =
                    (:4:9-4:12,
                     (EAppl (
                        (:4:9-4:10, (EIdent (:4:9-4:10, (IdBoxed (Name "T"))))),
                        [(:4:11-4:12, (EIdent (:4:11-4:12, (IdBare (Name "t")))))]
                        )))
                    })
                  ];
                comb_result =
                (:4:16-4:17, (EIdent (:4:16-4:17, (IdBoxed (Name "T"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "unnamed argument with application" =
    p "a (A s) = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:13,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:13,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:1:4-1:7,
                    (EAppl ((:1:4-1:5, (EIdent (:1:4-1:5, (IdBoxed (Name "A"))))),
                       [(:1:6-1:7, (EIdent (:1:6-1:7, (IdBare (Name "s")))))])))
                   })
                 ];
               comb_result =
               (:1:11-1:12, (EIdent (:1:11-1:12, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "full combinator with unnamed arguments" =
    p "ns.f#abc (T t) = ns.F;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:23,
             { comb_id = (:1:1-1:9, (CombIdFull ((NameNs ("ns", "f")), "abc")));
               comb_opt_args = [];
               comb_args =
               [(:1:10-1:15,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:1:11-1:14,
                    (EAppl (
                       (:1:11-1:12, (EIdent (:1:11-1:12, (IdBoxed (Name "T"))))),
                       [(:1:13-1:14, (EIdent (:1:13-1:14, (IdBare (Name "t")))))])))
                   })
                 ];
               comb_result =
               (:1:18-1:22, (EIdent (:1:18-1:22, (IdBoxed (NameNs ("ns", "F"))))))
               }))
          ];
        functions = [] } |}]

  let%expect_test "conditional definition with nat" =
    p "t flags:# arg2:flags.3?A<string> arg3:string = T;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:50,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "t"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:10,
                 { arg_id = (Some (:1:3-1:8, "flags")); arg_cond_def = None;
                   arg_type =
                   (:1:9-1:10, (EIdent (:1:9-1:10, (IdBare (Name "nat"))))) });
                 (:1:11-1:33,
                  { arg_id = (Some (:1:11-1:15, "arg2"));
                    arg_cond_def =
                    (Some (:1:16-1:24,
                           (CondDef ((:1:16-1:21, "flags"), (Some (:1:22-1:23, 3))
                              ))));
                    arg_type =
                    (:1:24-1:33,
                     (EAppl (
                        (:1:24-1:25, (EIdent (:1:24-1:25, (IdBoxed (Name "A"))))),
                        [(:1:26-1:32,
                          (EIdent (:1:26-1:32, (IdBare (Name "string")))))]
                        )))
                    });
                 (:1:34-1:45,
                  { arg_id = (Some (:1:34-1:38, "arg3")); arg_cond_def = None;
                    arg_type =
                    (:1:39-1:45, (EIdent (:1:39-1:45, (IdBare (Name "string"))))) })
                 ];
               comb_result =
               (:1:48-1:49, (EIdent (:1:48-1:49, (IdBoxed (Name "T"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "conditional definition without nat" =
    p "t flags:# arg2:flags?A<string> arg3:string = T;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:48,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "t"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:10,
                 { arg_id = (Some (:1:3-1:8, "flags")); arg_cond_def = None;
                   arg_type =
                   (:1:9-1:10, (EIdent (:1:9-1:10, (IdBare (Name "nat"))))) });
                 (:1:11-1:31,
                  { arg_id = (Some (:1:11-1:15, "arg2"));
                    arg_cond_def =
                    (Some (:1:16-1:22, (CondDef ((:1:16-1:21, "flags"), None))));
                    arg_type =
                    (:1:22-1:31,
                     (EAppl (
                        (:1:22-1:23, (EIdent (:1:22-1:23, (IdBoxed (Name "A"))))),
                        [(:1:24-1:30,
                          (EIdent (:1:24-1:30, (IdBare (Name "string")))))]
                        )))
                    });
                 (:1:32-1:43,
                  { arg_id = (Some (:1:32-1:36, "arg3")); arg_cond_def = None;
                    arg_type =
                    (:1:37-1:43, (EIdent (:1:37-1:43, (IdBare (Name "string"))))) })
                 ];
               comb_result =
               (:1:46-1:47, (EIdent (:1:46-1:47, (IdBoxed (Name "T"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "conditional definition inside parens" =
    p "t flags:# arg2:(flags.3?%T b) = T;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:35,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "t"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:10,
                 { arg_id = (Some (:1:3-1:8, "flags")); arg_cond_def = None;
                   arg_type =
                   (:1:9-1:10, (EIdent (:1:9-1:10, (IdBare (Name "nat"))))) });
                 (:1:11-1:30,
                  { arg_id = (Some (:1:11-1:15, "arg2"));
                    arg_cond_def =
                    (Some (:1:17-1:25,
                           (CondDef ((:1:17-1:22, "flags"), (Some (:1:23-1:24, 3))
                              ))));
                    arg_type =
                    (:1:25-1:29,
                     (EAppl (
                        (:1:25-1:27,
                         (EOperator ((:1:25-1:26, OpBare),
                            [(:1:26-1:27,
                              (EIdent (:1:26-1:27, (IdBoxed (Name "T")))))]
                            ))),
                        [(:1:28-1:29, (EIdent (:1:28-1:29, (IdBare (Name "b")))))]
                        )))
                    })
                 ];
               comb_result =
               (:1:33-1:34, (EIdent (:1:33-1:34, (IdBoxed (Name "T"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "multiple arguments with same type" =
    p "
a (a b _ d : string) = A;"; (* TODO: *)
    [%expect {| :2:8-2:9: Syntax error |}]

  let%expect_test "shouldn't accept `_` as the type of an unnamed argument" =
    p "a (_) = A;";
    [%expect {| :1:4-1:5: `_` is not allowed in application |}]

  let%expect_test "repetition (`[ ]`) with multiplicity" =
    p "a arg:2*[(A b) arg2:%C d] = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:31,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:26,
                 { arg_id = (Some (:1:3-1:6, "arg")); arg_cond_def = None;
                   arg_type =
                   (:1:9-1:26,
                    (EMultiArg ((Some (:1:7-1:8, (ENat 2))),
                       [(:1:10-1:15,
                         { arg_id = None; arg_cond_def = None;
                           arg_type =
                           (:1:11-1:14,
                            (EAppl (
                               (:1:11-1:12,
                                (EIdent (:1:11-1:12, (IdBoxed (Name "A"))))),
                               [(:1:13-1:14,
                                 (EIdent (:1:13-1:14, (IdBare (Name "b")))))]
                               )))
                           });
                         (:1:16-1:23,
                          { arg_id = (Some (:1:16-1:20, "arg2"));
                            arg_cond_def = None;
                            arg_type =
                            (:1:21-1:23,
                             (EOperator ((:1:21-1:22, OpBare),
                                [(:1:22-1:23,
                                  (EIdent (:1:22-1:23, (IdBoxed (Name "C")))))]
                                )))
                            });
                         (:1:24-1:25,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:24-1:25, (EIdent (:1:24-1:25, (IdBare (Name "d")))))
                            })
                         ]
                       )))
                   })
                 ];
               comb_result =
               (:1:29-1:30, (EIdent (:1:29-1:30, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "unnamed repetition with multiplicity" =
    p "a (T t)*[(A b) %C d] = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:26,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:21,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:1:9-1:21,
                    (EMultiArg (
                       (Some (:1:4-1:7,
                              (EAppl (
                                 (:1:4-1:5,
                                  (EIdent (:1:4-1:5, (IdBoxed (Name "T"))))),
                                 [(:1:6-1:7,
                                   (EIdent (:1:6-1:7, (IdBare (Name "t")))))]
                                 )))),
                       [(:1:10-1:15,
                         { arg_id = None; arg_cond_def = None;
                           arg_type =
                           (:1:11-1:14,
                            (EAppl (
                               (:1:11-1:12,
                                (EIdent (:1:11-1:12, (IdBoxed (Name "A"))))),
                               [(:1:13-1:14,
                                 (EIdent (:1:13-1:14, (IdBare (Name "b")))))]
                               )))
                           });
                         (:1:16-1:18,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:16-1:18,
                             (EOperator ((:1:16-1:17, OpBare),
                                [(:1:17-1:18,
                                  (EIdent (:1:17-1:18, (IdBoxed (Name "C")))))]
                                )))
                            });
                         (:1:19-1:20,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:19-1:20, (EIdent (:1:19-1:20, (IdBare (Name "d")))))
                            })
                         ]
                       )))
                   })
                 ];
               comb_result =
               (:1:24-1:25, (EIdent (:1:24-1:25, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "repetition without multiplicity" =
    p "a arg:[(A b) %C d] = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:24,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:19,
                 { arg_id = (Some (:1:3-1:6, "arg")); arg_cond_def = None;
                   arg_type =
                   (:1:7-1:19,
                    (EMultiArg (None,
                       [(:1:8-1:13,
                         { arg_id = None; arg_cond_def = None;
                           arg_type =
                           (:1:9-1:12,
                            (EAppl (
                               (:1:9-1:10,
                                (EIdent (:1:9-1:10, (IdBoxed (Name "A"))))),
                               [(:1:11-1:12,
                                 (EIdent (:1:11-1:12, (IdBare (Name "b")))))]
                               )))
                           });
                         (:1:14-1:16,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:14-1:16,
                             (EOperator ((:1:14-1:15, OpBare),
                                [(:1:15-1:16,
                                  (EIdent (:1:15-1:16, (IdBoxed (Name "C")))))]
                                )))
                            });
                         (:1:17-1:18,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:17-1:18, (EIdent (:1:17-1:18, (IdBare (Name "d")))))
                            })
                         ]
                       )))
                   })
                 ];
               comb_result =
               (:1:22-1:23, (EIdent (:1:22-1:23, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "unnamed repetition without multiplicity" =
    p "a [(A b) %C d] = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:20,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:15,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:1:3-1:15,
                    (EMultiArg (None,
                       [(:1:4-1:9,
                         { arg_id = None; arg_cond_def = None;
                           arg_type =
                           (:1:5-1:8,
                            (EAppl (
                               (:1:5-1:6, (EIdent (:1:5-1:6, (IdBoxed (Name "A"))))),
                               [(:1:7-1:8, (EIdent (:1:7-1:8, (IdBare (Name "b")))))
                                 ]
                               )))
                           });
                         (:1:10-1:12,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:10-1:12,
                             (EOperator ((:1:10-1:11, OpBare),
                                [(:1:11-1:12,
                                  (EIdent (:1:11-1:12, (IdBoxed (Name "C")))))]
                                )))
                            });
                         (:1:13-1:14,
                          { arg_id = None; arg_cond_def = None;
                            arg_type =
                            (:1:13-1:14, (EIdent (:1:13-1:14, (IdBare (Name "d")))))
                            })
                         ]
                       )))
                   })
                 ];
               comb_result =
               (:1:18-1:19, (EIdent (:1:18-1:19, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "repetition with _ as arg name" =
    p "a _:t*[] = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:14,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:9,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:1:7-1:9,
                    (EMultiArg (
                       (Some (:1:5-1:6, (EIdent (:1:5-1:6, (IdBare (Name "t")))))),
                       [])))
                   })
                 ];
               comb_result =
               (:1:12-1:13, (EIdent (:1:12-1:13, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]

  let%expect_test "unnamed repetition with multiplicity in full combinator" =
    p "a#abc = (T t)*[] = A;"; (* TODO: *)
    [%expect {| :1:9-1:10: Syntax error |}]

  let%expect_test "arguments with _ as arg name" =
    p "a _:string _:(T t) = A;";
    [%expect {|
      { constructors =
        [(CombinatorDecl
            (:1:1-1:24,
             { comb_id = (:1:1-1:2, (CombIdShort (Name "a"))); comb_opt_args = [];
               comb_args =
               [(:1:3-1:11,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:1:5-1:11, (EIdent (:1:5-1:11, (IdBare (Name "string"))))) });
                 (:1:12-1:19,
                  { arg_id = None; arg_cond_def = None;
                    arg_type =
                    (:1:15-1:18,
                     (EAppl (
                        (:1:15-1:16, (EIdent (:1:15-1:16, (IdBoxed (Name "T"))))),
                        [(:1:17-1:18, (EIdent (:1:17-1:18, (IdBare (Name "t")))))]
                        )))
                    })
                 ];
               comb_result =
               (:1:22-1:23, (EIdent (:1:22-1:23, (IdBoxed (Name "A"))))) }))
          ];
        functions = [] } |}]
end)

let%test_module "finalization declaration" = (module struct
  let%expect_test "New" =
    p "New X;";
    [%expect {|
      { constructors =
        [(FinalDecl
            (:1:1-1:7,
             { finalization = (:1:1-1:4, FinNew);
               final_id = (:1:5-1:6, (IdBoxed (Name "X"))) }))
          ];
        functions = [] } |}]

  let%expect_test "Final" =
    p "Final Y;";
    [%expect {|
      { constructors =
        [(FinalDecl
            (:1:1-1:9,
             { finalization = (:1:1-1:6, FinFinal);
               final_id = (:1:7-1:8, (IdBoxed (Name "Y"))) }))
          ];
        functions = [] } |}]

  let%expect_test "Empty" =
    p "Empty Z;";
    [%expect {|
      { constructors =
        [(FinalDecl
            (:1:1-1:9,
             { finalization = (:1:1-1:6, FinEmpty);
               final_id = (:1:7-1:8, (IdBoxed (Name "Z"))) }))
          ];
        functions = [] } |}]

  let%expect_test "shouldn't parse identifiers begin with lowercase" =
    p "New x;";
    [%expect {| :1:5-1:6: Syntax error |}]
end)

let%expect_test "builtin combinator declaration" =
  p "a ? = A;";
  [%expect {|
    { constructors =
      [(BuiltinCombinatorDecl
          (:1:1-1:9,
           { b_comb_id = (:1:1-1:2, (CombIdShort (Name "a")));
             b_comb_result = (:1:7-1:8, (IdBoxed (Name "A"))) }))
        ];
      functions = [] } |}]

let%expect_test "builtin combinator declaration with magic and namespace" =
  p "ns.a#abc ? = A;";
  [%expect {|
    { constructors =
      [(BuiltinCombinatorDecl
          (:1:1-1:16,
           { b_comb_id = (:1:1-1:9, (CombIdFull ((NameNs ("ns", "a")), "abc")));
             b_comb_result = (:1:14-1:15, (IdBoxed (Name "A"))) }))
        ];
      functions = [] } |}]

let%test_module "partial application declaration" = (module struct
  let%test_module "partial type application" = (module struct
    let%expect_test "E<E_1, ..., E_n> syntax" =
      p "
      Vector<Int>;
      Vector<A, b, %C>;
      Vector<C p>;
      ";
      [%expect {|
        { constructors =
          [(PartialApplicationDecl
              (:2:7-2:19,
               (PartialTypeAppl ((:2:7-2:13, (IdBoxed (Name "Vector"))),
                  [(:2:14-2:17, (EIdent (:2:14-2:17, (IdBoxed (Name "Int")))))]))));
            (PartialApplicationDecl
               (:3:7-3:24,
                (PartialTypeAppl ((:3:7-3:13, (IdBoxed (Name "Vector"))),
                   [(:3:14-3:15, (EIdent (:3:14-3:15, (IdBoxed (Name "A")))));
                     (:3:17-3:18, (EIdent (:3:17-3:18, (IdBare (Name "b")))));
                     (:3:20-3:22,
                      (EOperator ((:3:20-3:21, OpBare),
                         [(:3:21-3:22, (EIdent (:3:21-3:22, (IdBoxed (Name "C")))))]
                         )))
                     ]
                   ))));
            (PartialApplicationDecl
               (:4:7-4:19,
                (PartialTypeAppl ((:4:7-4:13, (IdBoxed (Name "Vector"))),
                   [(:4:14-4:17,
                     (EAppl (
                        (:4:14-4:15, (EIdent (:4:14-4:15, (IdBoxed (Name "C"))))),
                        [(:4:16-4:17, (EIdent (:4:16-4:17, (IdBare (Name "p")))))])))
                     ]
                   ))))
            ];
          functions = [] } |}]

    let%expect_test "(E (E_1) ... (E_n)) syntax" =
      p "
      Vector Int;
      Vector int B %C;
      Vector 2+3;
      Vector (C p);
      ";
      [%expect {|
        { constructors =
          [(PartialApplicationDecl
              (:2:7-2:18,
               (PartialTypeAppl ((:2:7-2:13, (IdBoxed (Name "Vector"))),
                  [(:2:14-2:17, (EIdent (:2:14-2:17, (IdBoxed (Name "Int")))))]))));
            (PartialApplicationDecl
               (:3:7-3:23,
                (PartialTypeAppl ((:3:7-3:13, (IdBoxed (Name "Vector"))),
                   [(:3:14-3:17, (EIdent (:3:14-3:17, (IdBare (Name "int")))));
                     (:3:18-3:19, (EIdent (:3:18-3:19, (IdBoxed (Name "B")))));
                     (:3:20-3:22,
                      (EOperator ((:3:20-3:21, OpBare),
                         [(:3:21-3:22, (EIdent (:3:21-3:22, (IdBoxed (Name "C")))))]
                         )))
                     ]
                   ))));
            (PartialApplicationDecl
               (:4:7-4:18,
                (PartialTypeAppl ((:4:7-4:13, (IdBoxed (Name "Vector"))),
                   [(:4:14-4:17,
                     (EOperator ((:4:15-4:16, OpPlus),
                        [(:4:14-4:15, (ENat 2)); (:4:16-4:17, (ENat 3))])))
                     ]
                   ))));
            (PartialApplicationDecl
               (:5:7-5:20,
                (PartialTypeAppl ((:5:7-5:13, (IdBoxed (Name "Vector"))),
                   [(:5:15-5:18,
                     (EAppl (
                        (:5:15-5:16, (EIdent (:5:15-5:16, (IdBoxed (Name "C"))))),
                        [(:5:17-5:18, (EIdent (:5:17-5:18, (IdBare (Name "p")))))])))
                     ]
                   ))))
            ];
          functions = [] } |}]

      let%expect_test "shouldn't parse without params" =
        p "Vector;";
        [%expect {| :1:7-1:8: Syntax error |}]
  end)

  let%test_module "partial combinator application" = (module struct
    let%expect_test "basic" =
      p "
      a a b c;
      b d;
      c %E;
      d (%E a);
      ";
      [%expect {|
        { constructors =
          [(PartialApplicationDecl
              (:2:7-2:15,
               (PartialCombAppl ((:2:7-2:8, (CombIdShort (Name "a"))),
                  [(:2:9-2:10, (EIdent (:2:9-2:10, (IdBare (Name "a")))));
                    (:2:11-2:12, (EIdent (:2:11-2:12, (IdBare (Name "b")))));
                    (:2:13-2:14, (EIdent (:2:13-2:14, (IdBare (Name "c")))))]
                  ))));
            (PartialApplicationDecl
               (:3:7-3:11,
                (PartialCombAppl ((:3:7-3:8, (CombIdShort (Name "b"))),
                   [(:3:9-3:10, (EIdent (:3:9-3:10, (IdBare (Name "d")))))]))));
            (PartialApplicationDecl
               (:4:7-4:12,
                (PartialCombAppl ((:4:7-4:8, (CombIdShort (Name "c"))),
                   [(:4:9-4:11,
                     (EOperator ((:4:9-4:10, OpBare),
                        [(:4:10-4:11, (EIdent (:4:10-4:11, (IdBoxed (Name "E")))))])))
                     ]
                   ))));
            (PartialApplicationDecl
               (:5:7-5:16,
                (PartialCombAppl ((:5:7-5:8, (CombIdShort (Name "d"))),
                   [(:5:10-5:14,
                     (EAppl (
                        (:5:10-5:12,
                         (EOperator ((:5:10-5:11, OpBare),
                            [(:5:11-5:12, (EIdent (:5:11-5:12, (IdBoxed (Name "E")))))
                              ]
                            ))),
                        [(:5:13-5:14, (EIdent (:5:13-5:14, (IdBare (Name "a")))))])))
                     ]
                   ))))
            ];
          functions = [] } |}]

    let%expect_test "should work with type application" =
      p "e (a b);";
      [%expect {|
        { constructors =
          [(PartialApplicationDecl
              (:1:1-1:9,
               (PartialCombAppl ((:1:1-1:2, (CombIdShort (Name "e"))),
                  [(:1:4-1:7,
                    (EAppl ((:1:4-1:5, (EIdent (:1:4-1:5, (IdBare (Name "a"))))),
                       [(:1:6-1:7, (EIdent (:1:6-1:7, (IdBare (Name "b")))))])))
                    ]
                  ))))
            ];
          functions = [] } |}]

    let%expect_test "shouldn't accept combinator with magic" =
      p "c#abcde a;";
      [%expect {| :1:10-1:11: Syntax error |}]

    let%expect_test "shouldn't accept '!'" =
      p "c !t;";
      [%expect {| :1:3-1:6: `!` is not allowed in partial combinator application |}]
  end)
end)

let%expect_test "comments" =
  p "
a = A;
// b = B;
c = C; // a
  //";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:1-2:7,
           { comb_id = (:2:1-2:2, (CombIdShort (Name "a"))); comb_opt_args = [];
             comb_args = [];
             comb_result = (:2:5-2:6, (EIdent (:2:5-2:6, (IdBoxed (Name "A")))))
             }));
        (CombinatorDecl
           (:4:1-4:7,
            { comb_id = (:4:1-4:2, (CombIdShort (Name "c"))); comb_opt_args = [];
              comb_args = [];
              comb_result = (:4:5-4:6, (EIdent (:4:5-4:6, (IdBoxed (Name "C")))))
              }))
        ];
      functions = [] } |}]

let%expect_test "multiline comments" =
  p "
a = A;
/* b = B;
c = C */ d = D;
e = E;
f /* arg: string */ = F;
  ";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:1-2:7,
           { comb_id = (:2:1-2:2, (CombIdShort (Name "a"))); comb_opt_args = [];
             comb_args = [];
             comb_result = (:2:5-2:6, (EIdent (:2:5-2:6, (IdBoxed (Name "A")))))
             }));
        (CombinatorDecl
           (:4:10-4:16,
            { comb_id = (:4:10-4:11, (CombIdShort (Name "d")));
              comb_opt_args = []; comb_args = [];
              comb_result =
              (:4:14-4:15, (EIdent (:4:14-4:15, (IdBoxed (Name "D"))))) }));
        (CombinatorDecl
           (:5:1-5:7,
            { comb_id = (:5:1-5:2, (CombIdShort (Name "e"))); comb_opt_args = [];
              comb_args = [];
              comb_result = (:5:5-5:6, (EIdent (:5:5-5:6, (IdBoxed (Name "E")))))
              }));
        (CombinatorDecl
           (:6:1-6:25,
            { comb_id = (:6:1-6:2, (CombIdShort (Name "f"))); comb_opt_args = [];
              comb_args = [];
              comb_result =
              (:6:23-6:24, (EIdent (:6:23-6:24, (IdBoxed (Name "F"))))) }))
        ];
      functions = [] } |}]

let%expect_test "unterminated multiline comment should fail" =
  p "
a = A;
/*
  ";
  [%expect {| :4:3-4:3: Unterminated comment |}]

let%expect_test "advanced.tl" =
  p "
user {flags:#} id:flags.0?string first_name:flags.1?string last_name:flags.2?string reserved3:flags.3?False reserved4:flags.4?False = User flags;
user_present {flags:#} info:%(User flags) = UserInfo flags;
user_absent {flags:#} = UserInfo flags;
getUser flags:# id:int = !UserInfo flags;

matrix {m n : #} a : m* [ n* [ double ] ] = Matrix m n;
aux_type {n : #} (_ : %Tuple double n) = AuxType n;
matrix {m : #} {n : #} (a : %Tuple %(AuxType n) m) = Matrix m n;

tnil {X : Type} = Tuple X 0;
tcons {X : Type} {n : #} hd:X tl:%(Tuple X n) = Tuple X (S n);
vector {X : Type} (n : #) (v : %(Tuple X n)) = Vector X;

vector {t : Type} # [ t ] = Vector t;
user {fields:#} id:int first_name:(fields.0?string) last_name:(fields.1?string) friends:(fields.2?%(Vector int)) = User fields;
get_users req_fields:# ids:%(Vector int) = Vector %(User req_fields);
  ";
  [%expect {|
    { constructors =
      [(CombinatorDecl
          (:2:1-2:146,
           { comb_id = (:2:1-2:5, (CombIdShort (Name "user")));
             comb_opt_args =
             [(:2:6-2:15,
               { opt_arg_id = (:2:7-2:12, "flags");
                 opt_arg_type =
                 (:2:13-2:14, (EIdent (:2:13-2:14, (IdBare (Name "nat"))))) })
               ];
             comb_args =
             [(:2:16-2:33,
               { arg_id = (Some (:2:16-2:18, "id"));
                 arg_cond_def =
                 (Some (:2:19-2:27,
                        (CondDef ((:2:19-2:24, "flags"), (Some (:2:25-2:26, 0))))));
                 arg_type =
                 (:2:27-2:33, (EIdent (:2:27-2:33, (IdBare (Name "string"))))) });
               (:2:34-2:59,
                { arg_id = (Some (:2:34-2:44, "first_name"));
                  arg_cond_def =
                  (Some (:2:45-2:53,
                         (CondDef ((:2:45-2:50, "flags"), (Some (:2:51-2:52, 1))
                            ))));
                  arg_type =
                  (:2:53-2:59, (EIdent (:2:53-2:59, (IdBare (Name "string"))))) });
               (:2:60-2:84,
                { arg_id = (Some (:2:60-2:69, "last_name"));
                  arg_cond_def =
                  (Some (:2:70-2:78,
                         (CondDef ((:2:70-2:75, "flags"), (Some (:2:76-2:77, 2))
                            ))));
                  arg_type =
                  (:2:78-2:84, (EIdent (:2:78-2:84, (IdBare (Name "string"))))) });
               (:2:85-2:108,
                { arg_id = (Some (:2:85-2:94, "reserved3"));
                  arg_cond_def =
                  (Some (:2:95-2:103,
                         (CondDef ((:2:95-2:100, "flags"),
                            (Some (:2:101-2:102, 3))))));
                  arg_type =
                  (:2:103-2:108,
                   (EIdent (:2:103-2:108, (IdBoxed (Name "False")))))
                  });
               (:2:109-2:132,
                { arg_id = (Some (:2:109-2:118, "reserved4"));
                  arg_cond_def =
                  (Some (:2:119-2:127,
                         (CondDef ((:2:119-2:124, "flags"),
                            (Some (:2:125-2:126, 4))))));
                  arg_type =
                  (:2:127-2:132,
                   (EIdent (:2:127-2:132, (IdBoxed (Name "False")))))
                  })
               ];
             comb_result =
             (:2:135-2:139,
              (EAppl (
                 (:2:135-2:139, (EIdent (:2:135-2:139, (IdBoxed (Name "User"))))),
                 [(:2:140-2:145, (EIdent (:2:140-2:145, (IdBare (Name "flags")))))
                   ]
                 )))
             }));
        (CombinatorDecl
           (:3:1-3:60,
            { comb_id = (:3:1-3:13, (CombIdShort (Name "user_present")));
              comb_opt_args =
              [(:3:14-3:23,
                { opt_arg_id = (:3:15-3:20, "flags");
                  opt_arg_type =
                  (:3:21-3:22, (EIdent (:3:21-3:22, (IdBare (Name "nat"))))) })
                ];
              comb_args =
              [(:3:24-3:42,
                { arg_id = (Some (:3:24-3:28, "info")); arg_cond_def = None;
                  arg_type =
                  (:3:29-3:42,
                   (EOperator ((:3:29-3:30, OpBare),
                      [(:3:31-3:41,
                        (EAppl (
                           (:3:31-3:35,
                            (EIdent (:3:31-3:35, (IdBoxed (Name "User"))))),
                           [(:3:36-3:41,
                             (EIdent (:3:36-3:41, (IdBare (Name "flags")))))]
                           )))
                        ]
                      )))
                  })
                ];
              comb_result =
              (:3:45-3:53,
               (EAppl (
                  (:3:45-3:53, (EIdent (:3:45-3:53, (IdBoxed (Name "UserInfo"))))),
                  [(:3:54-3:59, (EIdent (:3:54-3:59, (IdBare (Name "flags")))))]
                  )))
              }));
        (CombinatorDecl
           (:4:1-4:40,
            { comb_id = (:4:1-4:12, (CombIdShort (Name "user_absent")));
              comb_opt_args =
              [(:4:13-4:22,
                { opt_arg_id = (:4:14-4:19, "flags");
                  opt_arg_type =
                  (:4:20-4:21, (EIdent (:4:20-4:21, (IdBare (Name "nat"))))) })
                ];
              comb_args = [];
              comb_result =
              (:4:25-4:33,
               (EAppl (
                  (:4:25-4:33, (EIdent (:4:25-4:33, (IdBoxed (Name "UserInfo"))))),
                  [(:4:34-4:39, (EIdent (:4:34-4:39, (IdBare (Name "flags")))))]
                  )))
              }));
        (CombinatorDecl
           (:5:1-5:42,
            { comb_id = (:5:1-5:8, (CombIdShort (Name "getUser")));
              comb_opt_args = [];
              comb_args =
              [(:5:9-5:16,
                { arg_id = (Some (:5:9-5:14, "flags")); arg_cond_def = None;
                  arg_type =
                  (:5:15-5:16, (EIdent (:5:15-5:16, (IdBare (Name "nat"))))) });
                (:5:17-5:23,
                 { arg_id = (Some (:5:17-5:19, "id")); arg_cond_def = None;
                   arg_type =
                   (:5:20-5:23, (EIdent (:5:20-5:23, (IdBare (Name "int"))))) })
                ];
              comb_result =
              (:5:26-5:41,
               (EOperator ((:5:26-5:27, OpBang),
                  [(:5:27-5:35,
                    (EAppl (
                       (:5:27-5:35,
                        (EIdent (:5:27-5:35, (IdBoxed (Name "UserInfo"))))),
                       [(:5:36-5:41,
                         (EIdent (:5:36-5:41, (IdBare (Name "flags")))))]
                       )))
                    ]
                  )))
              }));
        (CombinatorDecl
           (:7:1-7:56,
            { comb_id = (:7:1-7:7, (CombIdShort (Name "matrix")));
              comb_opt_args =
              [(:7:8-7:17,
                { opt_arg_id = (:7:9-7:10, "m");
                  opt_arg_type =
                  (:7:15-7:16, (EIdent (:7:15-7:16, (IdBare (Name "nat"))))) });
                (:7:8-7:17,
                 { opt_arg_id = (:7:11-7:12, "n");
                   opt_arg_type =
                   (:7:15-7:16, (EIdent (:7:15-7:16, (IdBare (Name "nat"))))) })
                ];
              comb_args =
              [(:7:18-7:42,
                { arg_id = (Some (:7:18-7:19, "a")); arg_cond_def = None;
                  arg_type =
                  (:7:25-7:42,
                   (EMultiArg (
                      (Some (:7:22-7:23,
                             (EIdent (:7:22-7:23, (IdBare (Name "m")))))),
                      [(:7:27-7:40,
                        { arg_id = None; arg_cond_def = None;
                          arg_type =
                          (:7:30-7:40,
                           (EMultiArg (
                              (Some (:7:27-7:28,
                                     (EIdent (:7:27-7:28, (IdBare (Name "n")))))),
                              [(:7:32-7:38,
                                { arg_id = None; arg_cond_def = None;
                                  arg_type =
                                  (:7:32-7:38,
                                   (EIdent (:7:32-7:38, (IdBare (Name "double")))))
                                  })
                                ]
                              )))
                          })
                        ]
                      )))
                  })
                ];
              comb_result =
              (:7:45-7:51,
               (EAppl (
                  (:7:45-7:51, (EIdent (:7:45-7:51, (IdBoxed (Name "Matrix"))))),
                  [(:7:52-7:53, (EIdent (:7:52-7:53, (IdBare (Name "m")))));
                    (:7:54-7:55, (EIdent (:7:54-7:55, (IdBare (Name "n")))))]
                  )))
              }));
        (CombinatorDecl
           (:8:1-8:52,
            { comb_id = (:8:1-8:9, (CombIdShort (Name "aux_type")));
              comb_opt_args =
              [(:8:10-8:17,
                { opt_arg_id = (:8:11-8:12, "n");
                  opt_arg_type =
                  (:8:15-8:16, (EIdent (:8:15-8:16, (IdBare (Name "nat"))))) })
                ];
              comb_args =
              [(:8:18-8:39,
                { arg_id = None; arg_cond_def = None;
                  arg_type =
                  (:8:23-8:38,
                   (EAppl (
                      (:8:23-8:29,
                       (EOperator ((:8:23-8:24, OpBare),
                          [(:8:24-8:29,
                            (EIdent (:8:24-8:29, (IdBoxed (Name "Tuple")))))]
                          ))),
                      [(:8:30-8:36,
                        (EIdent (:8:30-8:36, (IdBare (Name "double")))));
                        (:8:37-8:38, (EIdent (:8:37-8:38, (IdBare (Name "n")))))]
                      )))
                  })
                ];
              comb_result =
              (:8:42-8:49,
               (EAppl (
                  (:8:42-8:49, (EIdent (:8:42-8:49, (IdBoxed (Name "AuxType"))))),
                  [(:8:50-8:51, (EIdent (:8:50-8:51, (IdBare (Name "n")))))])))
              }));
        (CombinatorDecl
           (:9:1-9:65,
            { comb_id = (:9:1-9:7, (CombIdShort (Name "matrix")));
              comb_opt_args =
              [(:9:8-9:15,
                { opt_arg_id = (:9:9-9:10, "m");
                  opt_arg_type =
                  (:9:13-9:14, (EIdent (:9:13-9:14, (IdBare (Name "nat"))))) });
                (:9:16-9:23,
                 { opt_arg_id = (:9:17-9:18, "n");
                   opt_arg_type =
                   (:9:21-9:22, (EIdent (:9:21-9:22, (IdBare (Name "nat"))))) })
                ];
              comb_args =
              [(:9:24-9:51,
                { arg_id = (Some (:9:25-9:26, "a")); arg_cond_def = None;
                  arg_type =
                  (:9:29-9:50,
                   (EAppl (
                      (:9:29-9:35,
                       (EOperator ((:9:29-9:30, OpBare),
                          [(:9:30-9:35,
                            (EIdent (:9:30-9:35, (IdBoxed (Name "Tuple")))))]
                          ))),
                      [(:9:36-9:48,
                        (EOperator ((:9:36-9:37, OpBare),
                           [(:9:38-9:47,
                             (EAppl (
                                (:9:38-9:45,
                                 (EIdent (:9:38-9:45, (IdBoxed (Name "AuxType"))))),
                                [(:9:46-9:47,
                                  (EIdent (:9:46-9:47, (IdBare (Name "n")))))]
                                )))
                             ]
                           )));
                        (:9:49-9:50, (EIdent (:9:49-9:50, (IdBare (Name "m")))))]
                      )))
                  })
                ];
              comb_result =
              (:9:54-9:60,
               (EAppl (
                  (:9:54-9:60, (EIdent (:9:54-9:60, (IdBoxed (Name "Matrix"))))),
                  [(:9:61-9:62, (EIdent (:9:61-9:62, (IdBare (Name "m")))));
                    (:9:63-9:64, (EIdent (:9:63-9:64, (IdBare (Name "n")))))]
                  )))
              }));
        (CombinatorDecl
           (:11:1-11:29,
            { comb_id = (:11:1-11:5, (CombIdShort (Name "tnil")));
              comb_opt_args =
              [(:11:6-11:16,
                { opt_arg_id = (:11:7-11:8, "X");
                  opt_arg_type =
                  (:11:11-11:15, (EIdent (:11:11-11:15, (IdBoxed (Name "Type")))))
                  })
                ];
              comb_args = [];
              comb_result =
              (:11:19-11:24,
               (EAppl (
                  (:11:19-11:24,
                   (EIdent (:11:19-11:24, (IdBoxed (Name "Tuple"))))),
                  [(:11:25-11:26, (EIdent (:11:25-11:26, (IdBoxed (Name "X")))));
                    (:11:27-11:28, (ENat 0))]
                  )))
              }));
        (CombinatorDecl
           (:12:1-12:63,
            { comb_id = (:12:1-12:6, (CombIdShort (Name "tcons")));
              comb_opt_args =
              [(:12:7-12:17,
                { opt_arg_id = (:12:8-12:9, "X");
                  opt_arg_type =
                  (:12:12-12:16, (EIdent (:12:12-12:16, (IdBoxed (Name "Type")))))
                  });
                (:12:18-12:25,
                 { opt_arg_id = (:12:19-12:20, "n");
                   opt_arg_type =
                   (:12:23-12:24, (EIdent (:12:23-12:24, (IdBare (Name "nat")))))
                   })
                ];
              comb_args =
              [(:12:26-12:30,
                { arg_id = (Some (:12:26-12:28, "hd")); arg_cond_def = None;
                  arg_type =
                  (:12:29-12:30, (EIdent (:12:29-12:30, (IdBoxed (Name "X"))))) });
                (:12:31-12:46,
                 { arg_id = (Some (:12:31-12:33, "tl")); arg_cond_def = None;
                   arg_type =
                   (:12:34-12:46,
                    (EOperator ((:12:34-12:35, OpBare),
                       [(:12:36-12:45,
                         (EAppl (
                            (:12:36-12:41,
                             (EIdent (:12:36-12:41, (IdBoxed (Name "Tuple"))))),
                            [(:12:42-12:43,
                              (EIdent (:12:42-12:43, (IdBoxed (Name "X")))));
                              (:12:44-12:45,
                               (EIdent (:12:44-12:45, (IdBare (Name "n")))))
                              ]
                            )))
                         ]
                       )))
                   })
                ];
              comb_result =
              (:12:49-12:54,
               (EAppl (
                  (:12:49-12:54,
                   (EIdent (:12:49-12:54, (IdBoxed (Name "Tuple"))))),
                  [(:12:55-12:56, (EIdent (:12:55-12:56, (IdBoxed (Name "X")))));
                    (:12:58-12:61,
                     (EAppl (
                        (:12:58-12:59,
                         (EIdent (:12:58-12:59, (IdBoxed (Name "S"))))),
                        [(:12:60-12:61,
                          (EIdent (:12:60-12:61, (IdBare (Name "n")))))]
                        )))
                    ]
                  )))
              }));
        (CombinatorDecl
           (:13:1-13:57,
            { comb_id = (:13:1-13:7, (CombIdShort (Name "vector")));
              comb_opt_args =
              [(:13:8-13:18,
                { opt_arg_id = (:13:9-13:10, "X");
                  opt_arg_type =
                  (:13:13-13:17, (EIdent (:13:13-13:17, (IdBoxed (Name "Type")))))
                  })
                ];
              comb_args =
              [(:13:19-13:26,
                { arg_id = (Some (:13:20-13:21, "n")); arg_cond_def = None;
                  arg_type =
                  (:13:24-13:25, (EIdent (:13:24-13:25, (IdBare (Name "nat")))))
                  });
                (:13:27-13:45,
                 { arg_id = (Some (:13:28-13:29, "v")); arg_cond_def = None;
                   arg_type =
                   (:13:32-13:44,
                    (EOperator ((:13:32-13:33, OpBare),
                       [(:13:34-13:43,
                         (EAppl (
                            (:13:34-13:39,
                             (EIdent (:13:34-13:39, (IdBoxed (Name "Tuple"))))),
                            [(:13:40-13:41,
                              (EIdent (:13:40-13:41, (IdBoxed (Name "X")))));
                              (:13:42-13:43,
                               (EIdent (:13:42-13:43, (IdBare (Name "n")))))
                              ]
                            )))
                         ]
                       )))
                   })
                ];
              comb_result =
              (:13:48-13:54,
               (EAppl (
                  (:13:48-13:54,
                   (EIdent (:13:48-13:54, (IdBoxed (Name "Vector"))))),
                  [(:13:55-13:56, (EIdent (:13:55-13:56, (IdBoxed (Name "X")))))]
                  )))
              }));
        (CombinatorDecl
           (:15:1-15:38,
            { comb_id = (:15:1-15:7, (CombIdShort (Name "vector")));
              comb_opt_args =
              [(:15:8-15:18,
                { opt_arg_id = (:15:9-15:10, "t");
                  opt_arg_type =
                  (:15:13-15:17, (EIdent (:15:13-15:17, (IdBoxed (Name "Type")))))
                  })
                ];
              comb_args =
              [(:15:19-15:20,
                { arg_id = None; arg_cond_def = None;
                  arg_type =
                  (:15:19-15:20, (EIdent (:15:19-15:20, (IdBare (Name "nat")))))
                  });
                (:15:21-15:26,
                 { arg_id = None; arg_cond_def = None;
                   arg_type =
                   (:15:21-15:26,
                    (EMultiArg (None,
                       [(:15:23-15:24,
                         { arg_id = None; arg_cond_def = None;
                           arg_type =
                           (:15:23-15:24,
                            (EIdent (:15:23-15:24, (IdBare (Name "t")))))
                           })
                         ]
                       )))
                   })
                ];
              comb_result =
              (:15:29-15:35,
               (EAppl (
                  (:15:29-15:35,
                   (EIdent (:15:29-15:35, (IdBoxed (Name "Vector"))))),
                  [(:15:36-15:37, (EIdent (:15:36-15:37, (IdBare (Name "t")))))]
                  )))
              }));
        (CombinatorDecl
           (:16:1-16:128,
            { comb_id = (:16:1-16:5, (CombIdShort (Name "user")));
              comb_opt_args =
              [(:16:6-16:16,
                { opt_arg_id = (:16:7-16:13, "fields");
                  opt_arg_type =
                  (:16:14-16:15, (EIdent (:16:14-16:15, (IdBare (Name "nat")))))
                  })
                ];
              comb_args =
              [(:16:17-16:23,
                { arg_id = (Some (:16:17-16:19, "id")); arg_cond_def = None;
                  arg_type =
                  (:16:20-16:23, (EIdent (:16:20-16:23, (IdBare (Name "int")))))
                  });
                (:16:24-16:52,
                 { arg_id = (Some (:16:24-16:34, "first_name"));
                   arg_cond_def =
                   (Some (:16:36-16:45,
                          (CondDef ((:16:36-16:42, "fields"),
                             (Some (:16:43-16:44, 0))))));
                   arg_type =
                   (:16:45-16:51,
                    (EIdent (:16:45-16:51, (IdBare (Name "string")))))
                   });
                (:16:53-16:80,
                 { arg_id = (Some (:16:53-16:62, "last_name"));
                   arg_cond_def =
                   (Some (:16:64-16:73,
                          (CondDef ((:16:64-16:70, "fields"),
                             (Some (:16:71-16:72, 1))))));
                   arg_type =
                   (:16:73-16:79,
                    (EIdent (:16:73-16:79, (IdBare (Name "string")))))
                   });
                (:16:81-16:113,
                 { arg_id = (Some (:16:81-16:88, "friends"));
                   arg_cond_def =
                   (Some (:16:90-16:99,
                          (CondDef ((:16:90-16:96, "fields"),
                             (Some (:16:97-16:98, 2))))));
                   arg_type =
                   (:16:99-16:112,
                    (EOperator ((:16:99-16:100, OpBare),
                       [(:16:101-16:111,
                         (EAppl (
                            (:16:101-16:107,
                             (EIdent (:16:101-16:107, (IdBoxed (Name "Vector"))))),
                            [(:16:108-16:111,
                              (EIdent (:16:108-16:111, (IdBare (Name "int")))))]
                            )))
                         ]
                       )))
                   })
                ];
              comb_result =
              (:16:116-16:120,
               (EAppl (
                  (:16:116-16:120,
                   (EIdent (:16:116-16:120, (IdBoxed (Name "User"))))),
                  [(:16:121-16:127,
                    (EIdent (:16:121-16:127, (IdBare (Name "fields")))))]
                  )))
              }));
        (CombinatorDecl
           (:17:1-17:70,
            { comb_id = (:17:1-17:10, (CombIdShort (Name "get_users")));
              comb_opt_args = [];
              comb_args =
              [(:17:11-17:23,
                { arg_id = (Some (:17:11-17:21, "req_fields"));
                  arg_cond_def = None;
                  arg_type =
                  (:17:22-17:23, (EIdent (:17:22-17:23, (IdBare (Name "nat")))))
                  });
                (:17:24-17:41,
                 { arg_id = (Some (:17:24-17:27, "ids")); arg_cond_def = None;
                   arg_type =
                   (:17:28-17:41,
                    (EOperator ((:17:28-17:29, OpBare),
                       [(:17:30-17:40,
                         (EAppl (
                            (:17:30-17:36,
                             (EIdent (:17:30-17:36, (IdBoxed (Name "Vector"))))),
                            [(:17:37-17:40,
                              (EIdent (:17:37-17:40, (IdBare (Name "int")))))]
                            )))
                         ]
                       )))
                   })
                ];
              comb_result =
              (:17:44-17:50,
               (EAppl (
                  (:17:44-17:50,
                   (EIdent (:17:44-17:50, (IdBoxed (Name "Vector"))))),
                  [(:17:51-17:69,
                    (EOperator ((:17:51-17:52, OpBare),
                       [(:17:53-17:68,
                         (EAppl (
                            (:17:53-17:57,
                             (EIdent (:17:53-17:57, (IdBoxed (Name "User"))))),
                            [(:17:58-17:68,
                              (EIdent
                                 (:17:58-17:68, (IdBare (Name "req_fields")))))
                              ]
                            )))
                         ]
                       )))
                    ]
                  )))
              }))
        ];
      functions = [] } |}]
