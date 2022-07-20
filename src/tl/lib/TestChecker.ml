open Parser

let t ?(empty = true) str =
  match parse_string str with
  | Ok ast ->
    let store = if empty then Some Store.empty else None in
    let (errors, store) = Checker.run ?store ast in
    let buf = Buffer.create 1024 in
    let add = Buffer.add_string buf in
    List.iter (fun e -> add (Err.Check.show e); add "\n") errors;
    add (Store.show store);
    print_endline @@ Buffer.contents buf
  | Error e -> prerr_endline @@ Err.Parse.show e

let%expect_test "type parameters" =
  t "
    e#00 = E;
    a#01 {X:Type} = A X;

    t1#02 v:(A E E) = T1; // error
    t2#03 v:A = T2; // error
    t3#04 v:(A) = T3; // error
    t4#05 v:(%A E E) = T4; // error
    t5#06 v:(a E E) = T5; // error
    t6#07 v:(A E) = T6;
    t7#08 v:(a E) = T7;
    t8#09 v:%(A E E) = T8; // error
    t9#10 v:%A = T9; // error

    b1#11 {X:Type} = B X;
    b2#12 {X:#} = B X; // error

    c1#13 {X:Type} = C X;
    c2#14 {X:Type} {Y:Type} = C X Y; // error
    c3#15 = C; // error

    f#15 {X:Type} = F X;
    t10#16 arg:E v:(F arg) = T10; // error
    t11#17 v:(F 3) = T11; // error
    t12#18 v:(F E) = T12;
    t13#19 v:(F %E) = T13;
    t14#20 v:(F e) = T14;

    g#21 {X:#} = G X;
    t15#22 v:(G 4) = T15;
    t16#22 v:(G e) = T16; // error
    t161 {Y:#} v:G<Y> = T161 Y;

    t17#23 {t:Type} v:A<t> = T17 t;

    ns.t18#24 {t:Type} = ns.T18 t;
    t19#25 {t:Type} v:!ns.T18<t> = T19;

    t20#26 = T20 X; // error

    t21#27 X:E = T21 X; // error

    t22#28 {X:Type} = T22 (X);
  ";
  [%expect {|
    :16:19-16:20: Wrong type parameters
    :19:31-19:32: Wrong type parameters
    :20:13-20:14: Wrong type parameters
    :39:18-39:19: Variable `X` is not defined
    :41:14-41:15: Type parameters must be of type `#` or `Type`, `Type` is used instead
    :5:18-5:15: The type constructor `A` expects 1 type parameters, but is applied to 2
    :6:13-6:14: The type constructor `A` expects 1 type parameters, but is applied to 0
    :7:14-7:15: The type constructor `A` expects 1 type parameters, but is applied to 0
    :8:19-8:16: The type constructor `A` expects 1 type parameters, but is applied to 2
    :9:18-9:15: The type constructor `a` expects 1 type parameters, but is applied to 2
    :12:19-12:16: The type constructor `A` expects 1 type parameters, but is applied to 2
    :13:14-13:15: The type constructor `A` expects 1 type parameters, but is applied to 0
    :23:12-23:17: Type `(:23:16-23:17,
     (Modified (None,
        (:23:16-23:17,
         (TypeExpr ((TypeRefUnd (:23:16-23:17, (TypeIdBoxed (Name "E")))), [])))
        )))` is incompatible with a type parameter of type `Type`
    :24:17-24:18: A nat expression is incompatible with a parameter of type `Type`
    :31:17-31:18: A type expression is incompatible with a parameter of type `nat`
    Types:
    (Type ((Name "A"), [(ParamType "X")]))
    (Type ((Name "B"), [(ParamType "X")]))
    (Type ((Name "C"), [(ParamType "X")]))
    (Type ((Name "E"), []))
    (Type ((Name "F"), [(ParamType "X")]))
    (Type ((Name "G"), [(ParamNat "X")]))
    (Type ((Name "T1"), []))
    (Type ((Name "T10"), []))
    (Type ((Name "T11"), []))
    (Type ((Name "T12"), []))
    (Type ((Name "T13"), []))
    (Type ((Name "T14"), []))
    (Type ((Name "T15"), []))
    (Type ((Name "T16"), []))
    (Type ((Name "T161"), [(ParamNat "Y")]))
    (Type ((Name "T17"), [(ParamType "t")]))
    (Type ((Name "T19"), []))
    (Type ((Name "T2"), []))
    (Type ((Name "T20"), [(ParamType "X")]))
    (Type ((Name "T21"), [(ParamType "X")]))
    (Type ((Name "T22"), [(ParamType "X")]))
    (Type ((Name "T3"), []))
    (Type ((Name "T4"), []))
    (Type ((Name "T5"), []))
    (Type ((Name "T6"), []))
    (Type ((Name "T7"), []))
    (Type ((Name "T8"), []))
    (Type ((Name "T9"), []))
    (Type ((NameNs ("ns", "T18")), [(ParamType "t")]))
    Constructors:
    (:3:5-3:25,
     { c_id = (:3:5-3:9, (Name "a")); c_magic = 0x1l;
       c_args =
       [(:3:10-3:18,
         { arg_id = (Some (:3:11-3:12, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:13-3:17,
            (Modified (None,
               (:3:13-3:17, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:15:5-15:26,
     { c_id = (:15:5-15:10, (Name "b1")); c_magic = 0x11l;
       c_args =
       [(:15:11-15:19,
         { arg_id = (Some (:15:12-15:13, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:15:14-15:18,
            (Modified (None,
               (:15:14-15:18, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 11; c_result_type = 11; c_builtin = false })
    (:16:5-16:23,
     { c_id = (:16:5-16:10, (Name "b2")); c_magic = 0x12l;
       c_args =
       [(:16:11-16:16,
         { arg_id = (Some (:16:12-16:13, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:16:14-16:15,
            (Modified (None,
               (:16:14-16:15, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 12; c_result_type = 11; c_builtin = false })
    (:18:5-18:26,
     { c_id = (:18:5-18:10, (Name "c1")); c_magic = 0x13l;
       c_args =
       [(:18:11-18:19,
         { arg_id = (Some (:18:12-18:13, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:18:14-18:18,
            (Modified (None,
               (:18:14-18:18, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 13; c_result_type = 12; c_builtin = false })
    (:19:5-19:37,
     { c_id = (:19:5-19:10, (Name "c2")); c_magic = 0x14l;
       c_args =
       [(:19:11-19:19,
         { arg_id = (Some (:19:12-19:13, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:19:14-19:18,
            (Modified (None,
               (:19:14-19:18, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:19:20-19:28,
          { arg_id = (Some (:19:21-19:22, "Y")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:19:23-19:27,
             (Modified (None,
                (:19:23-19:27, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
            arg_opt = (Some `ResultBang) })
         ];
       c_ref = 14; c_result_type = 12; c_builtin = false })
    (:20:5-20:15,
     { c_id = (:20:5-20:10, (Name "c3")); c_magic = 0x15l; c_args = [];
       c_ref = 15; c_result_type = 12; c_builtin = false })
    (:2:5-2:14,
     { c_id = (:2:5-2:9, (Name "e")); c_magic = 0x0l; c_args = []; c_ref = 0;
       c_result_type = 0; c_builtin = false })
    (:22:5-22:25,
     { c_id = (:22:5-22:9, (Name "f")); c_magic = 0x15l;
       c_args =
       [(:22:10-22:18,
         { arg_id = (Some (:22:11-22:12, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:22:13-22:17,
            (Modified (None,
               (:22:13-22:17, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 16; c_result_type = 13; c_builtin = false })
    (:29:5-29:22,
     { c_id = (:29:5-29:9, (Name "g")); c_magic = 0x21l;
       c_args =
       [(:29:10-29:15,
         { arg_id = (Some (:29:11-29:12, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:29:13-29:14,
            (Modified (None,
               (:29:13-29:14, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 22; c_result_type = 19; c_builtin = false })
    (:5:5-5:26,
     { c_id = (:5:5-5:10, (Name "t1")); c_magic = 0x2l;
       c_args =
       [(:5:11-5:20,
         { arg_id = (Some (:5:11-5:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:14-5:19,
            (Modified (None,
               (:5:18-5:15,
                (TypeExpr ((TypeRef (:5:14-5:15, (1, None))),
                   [(ExprType
                       (:5:16-5:17,
                        (TypeExpr ((TypeRef (:5:16-5:17, (0, None))), []))));
                     (ExprType
                        (:5:18-5:19,
                         (TypeExpr ((TypeRef (:5:18-5:19, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:23:5-23:34,
     { c_id = (:23:5-23:11, (Name "t10")); c_magic = 0x16l;
       c_args =
       [(:23:12-23:17,
         { arg_id = (Some (:23:12-23:15, "arg")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:23:16-23:17,
            (Modified (None,
               (:23:16-23:17,
                (TypeExpr ((TypeRef (:23:16-23:17, (0, None))), [])))
               )));
           arg_opt = None });
         (:23:18-23:27,
          { arg_id = (Some (:23:18-23:19, "v")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:23:21-23:26,
             (Modified (None,
                (:23:21-23:17,
                 (TypeExpr ((TypeRef (:23:21-23:22, (13, None))),
                    [(ExprType
                        (:23:12-23:17, (TypeVar (VarRef (0, (Some "arg"))))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 17; c_result_type = 14; c_builtin = false })
    (:24:5-24:26,
     { c_id = (:24:5-24:11, (Name "t11")); c_magic = 0x17l;
       c_args =
       [(:24:12-24:19,
         { arg_id = (Some (:24:12-24:13, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:24:15-24:18,
            (Modified (None,
               (:24:17-24:16,
                (TypeExpr ((TypeRef (:24:15-24:16, (13, None))),
                   [(ExprNat (:24:17-24:18, (NatConst 3)))])))
               )));
           arg_opt = None })
         ];
       c_ref = 18; c_result_type = 15; c_builtin = false })
    (:25:5-25:26,
     { c_id = (:25:5-25:11, (Name "t12")); c_magic = 0x18l;
       c_args =
       [(:25:12-25:19,
         { arg_id = (Some (:25:12-25:13, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:25:15-25:18,
            (Modified (None,
               (:25:17-25:16,
                (TypeExpr ((TypeRef (:25:15-25:16, (13, None))),
                   [(ExprType
                       (:25:17-25:18,
                        (TypeExpr ((TypeRef (:25:17-25:18, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 19; c_result_type = 16; c_builtin = false })
    (:26:5-26:27,
     { c_id = (:26:5-26:11, (Name "t13")); c_magic = 0x19l;
       c_args =
       [(:26:12-26:20,
         { arg_id = (Some (:26:12-26:13, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:26:15-26:19,
            (Modified (None,
               (:26:18-26:16,
                (TypeExpr ((TypeRef (:26:15-26:16, (13, None))),
                   [(ExprType
                       (:26:18-26:19,
                        (TypeExpr (
                           (TypeRef (:26:18-26:19, (0, (Some (ConstrRef 0))))),
                           []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 20; c_result_type = 17; c_builtin = false })
    (:27:5-27:26,
     { c_id = (:27:5-27:11, (Name "t14")); c_magic = 0x20l;
       c_args =
       [(:27:12-27:19,
         { arg_id = (Some (:27:12-27:13, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:27:15-27:18,
            (Modified (None,
               (:27:17-27:16,
                (TypeExpr ((TypeRef (:27:15-27:16, (13, None))),
                   [(ExprType
                       (:27:17-27:18,
                        (TypeExpr (
                           (TypeRef (:27:17-27:18, (0, (Some (ConstrRef 0))))),
                           []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 21; c_result_type = 18; c_builtin = false })
    (:30:5-30:26,
     { c_id = (:30:5-30:11, (Name "t15")); c_magic = 0x22l;
       c_args =
       [(:30:12-30:19,
         { arg_id = (Some (:30:12-30:13, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:30:15-30:18,
            (Modified (None,
               (:30:17-30:16,
                (TypeExpr ((TypeRef (:30:15-30:16, (19, None))),
                   [(ExprNat (:30:17-30:18, (NatConst 4)))])))
               )));
           arg_opt = None })
         ];
       c_ref = 23; c_result_type = 20; c_builtin = false })
    (:31:5-31:26,
     { c_id = (:31:5-31:11, (Name "t16")); c_magic = 0x22l;
       c_args =
       [(:31:12-31:19,
         { arg_id = (Some (:31:12-31:13, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:31:15-31:18,
            (Modified (None,
               (:31:17-31:16,
                (TypeExpr ((TypeRef (:31:15-31:16, (19, None))),
                   [(ExprType
                       (:31:17-31:18,
                        (TypeExpr (
                           (TypeRef (:31:17-31:18, (0, (Some (ConstrRef 0))))),
                           []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 24; c_result_type = 21; c_builtin = false })
    (:32:5-32:32,
     { c_id = (:32:5-32:9, (Name "t161")); c_magic = 0x87055319l;
       c_args =
       [(:32:10-32:15,
         { arg_id = (Some (:32:11-32:12, "Y")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:32:13-32:14,
            (Modified (None,
               (:32:13-32:14, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = (Some `ResultBang) });
         (:32:16-32:22,
          { arg_id = (Some (:32:16-32:17, "v")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:32:18-32:22,
             (Modified (None,
                (:32:20-32:19,
                 (TypeExpr ((TypeRef (:32:18-32:19, (19, None))),
                    [(ExprNat (:32:20-32:21, (NatVar (VarRef (0, (Some "Y"))))))]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 25; c_result_type = 22; c_builtin = false })
    (:34:5-34:36,
     { c_id = (:34:5-34:11, (Name "t17")); c_magic = 0x23l;
       c_args =
       [(:34:12-34:20,
         { arg_id = (Some (:34:13-34:14, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:34:15-34:19,
            (Modified (None,
               (:34:15-34:19, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:34:21-34:27,
          { arg_id = (Some (:34:21-34:22, "v")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:34:23-34:27,
             (Modified (None,
                (:34:23-34:20,
                 (TypeExpr ((TypeRef (:34:23-34:24, (1, None))),
                    [(ExprType (:34:12-34:20, (TypeVar (VarRef (0, (Some "t"))))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 26; c_result_type = 23; c_builtin = false })
    (:37:5-37:40,
     { c_id = (:37:5-37:11, (Name "t19")); c_magic = 0x25l;
       c_args =
       [(:37:12-37:20,
         { arg_id = (Some (:37:13-37:14, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:37:15-37:19,
            (Modified (None,
               (:37:15-37:19, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ArgBang) });
         (:37:21-37:33,
          { arg_id = (Some (:37:21-37:22, "v")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:37:23-37:33,
             (Modified ((Some ModBang),
                (:37:24-37:20,
                 (TypeExpr ((TypeRef (:37:24-37:30, (24, None))),
                    [(ExprType (:37:12-37:20, (TypeVar (VarRef (0, (Some "t"))))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 28; c_result_type = 25; c_builtin = false })
    (:6:5-6:20,
     { c_id = (:6:5-6:10, (Name "t2")); c_magic = 0x3l;
       c_args =
       [(:6:11-6:14,
         { arg_id = (Some (:6:11-6:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:13-6:14,
            (Modified (None,
               (:6:13-6:14, (TypeExpr ((TypeRef (:6:13-6:14, (1, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:39:5-39:20,
     { c_id = (:39:5-39:11, (Name "t20")); c_magic = 0x26l; c_args = [];
       c_ref = 29; c_result_type = 26; c_builtin = false })
    (:41:5-41:24,
     { c_id = (:41:5-41:11, (Name "t21")); c_magic = 0x27l;
       c_args =
       [(:41:12-41:15,
         { arg_id = (Some (:41:12-41:13, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:41:14-41:15,
            (Modified (None,
               (:41:14-41:15,
                (TypeExpr ((TypeRef (:41:14-41:15, (0, None))), [])))
               )));
           arg_opt = None })
         ];
       c_ref = 30; c_result_type = 27; c_builtin = false })
    (:43:5-43:31,
     { c_id = (:43:5-43:11, (Name "t22")); c_magic = 0x28l;
       c_args =
       [(:43:12-43:20,
         { arg_id = (Some (:43:13-43:14, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:43:15-43:19,
            (Modified (None,
               (:43:15-43:19, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 31; c_result_type = 28; c_builtin = false })
    (:7:5-7:22,
     { c_id = (:7:5-7:10, (Name "t3")); c_magic = 0x4l;
       c_args =
       [(:7:11-7:16,
         { arg_id = (Some (:7:11-7:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:14-7:15,
            (Modified (None,
               (:7:14-7:15, (TypeExpr ((TypeRef (:7:14-7:15, (1, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:8:5-8:27,
     { c_id = (:8:5-8:10, (Name "t4")); c_magic = 0x5l;
       c_args =
       [(:8:11-8:21,
         { arg_id = (Some (:8:11-8:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:8:14-8:20,
            (Modified (None,
               (:8:19-8:16,
                (TypeExpr ((TypeRef (:8:15-8:16, (1, (Some (ConstrRef 1))))),
                   [(ExprType
                       (:8:17-8:18,
                        (TypeExpr ((TypeRef (:8:17-8:18, (0, None))), []))));
                     (ExprType
                        (:8:19-8:20,
                         (TypeExpr ((TypeRef (:8:19-8:20, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    (:9:5-9:26,
     { c_id = (:9:5-9:10, (Name "t5")); c_magic = 0x6l;
       c_args =
       [(:9:11-9:20,
         { arg_id = (Some (:9:11-9:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:14-9:19,
            (Modified (None,
               (:9:18-9:15,
                (TypeExpr ((TypeRef (:9:14-9:15, (1, (Some (ConstrRef 1))))),
                   [(ExprType
                       (:9:16-9:17,
                        (TypeExpr ((TypeRef (:9:16-9:17, (0, None))), []))));
                     (ExprType
                        (:9:18-9:19,
                         (TypeExpr ((TypeRef (:9:18-9:19, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:10:5-10:24,
     { c_id = (:10:5-10:10, (Name "t6")); c_magic = 0x7l;
       c_args =
       [(:10:11-10:18,
         { arg_id = (Some (:10:11-10:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:10:14-10:17,
            (Modified (None,
               (:10:16-10:15,
                (TypeExpr ((TypeRef (:10:14-10:15, (1, None))),
                   [(ExprType
                       (:10:16-10:17,
                        (TypeExpr ((TypeRef (:10:16-10:17, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:11:5-11:24,
     { c_id = (:11:5-11:10, (Name "t7")); c_magic = 0x8l;
       c_args =
       [(:11:11-11:18,
         { arg_id = (Some (:11:11-11:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:11:14-11:17,
            (Modified (None,
               (:11:16-11:15,
                (TypeExpr ((TypeRef (:11:14-11:15, (1, (Some (ConstrRef 1))))),
                   [(ExprType
                       (:11:16-11:17,
                        (TypeExpr ((TypeRef (:11:16-11:17, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 8; c_result_type = 8; c_builtin = false })
    (:12:5-12:27,
     { c_id = (:12:5-12:10, (Name "t8")); c_magic = 0x9l;
       c_args =
       [(:12:11-12:21,
         { arg_id = (Some (:12:11-12:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:12:13-12:21,
            (Modified (None,
               (:12:19-12:16,
                (TypeExpr ((TypeRef (:12:15-12:16, (1, (Some (ConstrRef 1))))),
                   [(ExprType
                       (:12:17-12:18,
                        (TypeExpr ((TypeRef (:12:17-12:18, (0, None))), []))));
                     (ExprType
                        (:12:19-12:20,
                         (TypeExpr ((TypeRef (:12:19-12:20, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 9; c_result_type = 9; c_builtin = false })
    (:13:5-13:21,
     { c_id = (:13:5-13:10, (Name "t9")); c_magic = 0x10l;
       c_args =
       [(:13:11-13:15,
         { arg_id = (Some (:13:11-13:12, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:13:13-13:15,
            (Modified (None,
               (:13:14-13:15,
                (TypeExpr ((TypeRef (:13:14-13:15, (1, (Some (ConstrRef 1))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 10; c_result_type = 10; c_builtin = false })
    (:36:5-36:35,
     { c_id = (:36:5-36:14, (NameNs ("ns", "t18"))); c_magic = 0x24l;
       c_args =
       [(:36:15-36:23,
         { arg_id = (Some (:36:16-36:17, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:36:18-36:22,
            (Modified (None,
               (:36:18-36:22, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 27; c_result_type = 24; c_builtin = false })
    Functions: |}]

let%expect_test "optional arguments" =
  t "
    c1 {t1:Type} {t2:#} = C1 t1 t2;
    c2 {t1:Type} arg:!t1 = C2;
    c3 {t1:C2} = C3; // error
    c4 {t1:Type} arg:t1 = C4; // error
    c5 {X:Type} = C5 X;
    t6 {X:Type} = !C5 X; // error
    ---functions---
    f1 {T1:Type} = T1; // error
    f2 = C1 # #; // error
    ---types---
    c7 arg:(C1) = C7; // error
  ";
  [%expect {|
    :4:12-4:14: Optional arguments must be of type `#` or `Type`
    :5:8-5:17: An optional argument must be used at least once in the result type *not modified* by `!` or in an argument *modified* by `!`
    :7:8-7:16: An optional argument must be used at least once in the result type *not modified* by `!` or in an argument *modified* by `!`
    :9:8-9:17: An optional argument must be used at least once in the result type *not modified* by `!` or in an argument *modified* by `!`
    :12:13-12:15: The type constructor `C1` expects 2 type parameter, but is applied to 0
    :10:15-10:16: A type expression is incompatible with a parameter of type `nat`
    Types:
    (Type ((Name "C1"), [(ParamType "t1"); (ParamNat "t2")]))
    (Type ((Name "C2"), []))
    (Type ((Name "C3"), []))
    (Type ((Name "C4"), []))
    (Type ((Name "C5"), [(ParamType "X")]))
    (Type ((Name "C7"), []))
    Constructors:
    (:2:5-2:36,
     { c_id = (:2:5-2:7, (Name "c1")); c_magic = 0xB01E1F0El;
       c_args =
       [(:2:8-2:17,
         { arg_id = (Some (:2:9-2:11, "t1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:2:12-2:16,
            (Modified (None,
               (:2:12-2:16, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:2:18-2:24,
          { arg_id = (Some (:2:19-2:21, "t2")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:2:22-2:23,
             (Modified (None,
                (:2:22-2:23, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
            arg_opt = (Some `ResultBang) })
         ];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:3:5-3:31,
     { c_id = (:3:5-3:7, (Name "c2")); c_magic = 0x88A1AE21l;
       c_args =
       [(:3:8-3:17,
         { arg_id = (Some (:3:9-3:11, "t1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:12-3:16,
            (Modified (None,
               (:3:12-3:16, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ArgBang) });
         (:3:18-3:25,
          { arg_id = (Some (:3:18-3:21, "arg")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:3:22-3:25,
             (Modified ((Some ModBang),
                (:3:8-3:17, (TypeVar (VarRef (0, (Some "t1"))))))));
            arg_opt = None })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:4:5-4:21,
     { c_id = (:4:5-4:7, (Name "c3")); c_magic = 0x289BD8FDl; c_args = [];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:5:5-5:30,
     { c_id = (:5:5-5:7, (Name "c4")); c_magic = 0x1567326Dl;
       c_args =
       [(:5:8-5:17,
         { arg_id = (Some (:5:9-5:11, "t1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:12-5:16,
            (Modified (None,
               (:5:12-5:16, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:5:18-5:24,
          { arg_id = (Some (:5:18-5:21, "arg")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:5:22-5:24,
             (Modified (None, (:5:8-5:17, (TypeVar (VarRef (0, (Some "t1"))))))));
            arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:6:5-6:24,
     { c_id = (:6:5-6:7, (Name "c5")); c_magic = 0x24DF6D02l;
       c_args =
       [(:6:8-6:16,
         { arg_id = (Some (:6:9-6:10, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:11-6:15,
            (Modified (None,
               (:6:11-6:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:12:5-12:22,
     { c_id = (:12:5-12:7, (Name "c7")); c_magic = 0x2E341782l;
       c_args =
       [(:12:8-12:16,
         { arg_id = (Some (:12:8-12:11, "arg")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:12:13-12:15,
            (Modified (None,
               (:12:13-12:15,
                (TypeExpr ((TypeRef (:12:13-12:15, (0, None))), [])))
               )));
           arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    Functions:
    (:9:5-9:23,
     { f_id = (:9:5-9:7, (Name "f1")); f_magic = 0xC7CC9E6El;
       f_args =
       [(:9:8-9:17,
         { arg_id = (Some (:9:9-9:11, "T1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:12-9:16,
            (Modified (None,
               (:9:12-9:16, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       f_result_type = (:9:8-9:17, (TypeVar (VarRef (0, (Some "T1")))));
       f_builtin = false })
    (:10:5-10:17,
     { f_id = (:10:5-10:7, (Name "f2")); f_magic = 0x5433A9A2l; f_args = [];
       f_result_type =
       (:10:15-10:12,
        (TypeExpr ((TypeRef (:10:10-10:12, (0, None))),
           [(ExprType
               (:10:13-10:14, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))));
             (ExprType
                (:10:15-10:16, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))
             ]
           )));
       f_builtin = false })
    (:7:5-7:25,
     { f_id = (:7:5-7:7, (Name "t6")); f_magic = 0x3522DDCEl;
       f_args =
       [(:7:8-7:16,
         { arg_id = (Some (:7:9-7:10, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:11-7:15,
            (Modified (None,
               (:7:11-7:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       f_result_type =
       (:7:20-7:16,
        (TypeExpr ((TypeRef (:7:20-7:22, (4, None))),
           [(ExprType (:7:8-7:16, (TypeVar (VarRef (0, (Some "X"))))))])));
       f_builtin = false }) |}]

let%expect_test "vector" =
  t "myVector#1cb5c415 {t:Type} # [ t ] = MyVector t;";
  [%expect {|
    Types:
    (Type ((Name "MyVector"), [(ParamType "t")]))
    Constructors:
    (:1:1-1:49,
     { c_id = (:1:1-1:18, (Name "myVector")); c_magic = 0x1CB5C415l;
       c_args =
       [(:1:19-1:27,
         { arg_id = (Some (:1:20-1:21, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:1:22-1:26,
            (Modified (None,
               (:1:22-1:26, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:1:28-1:29,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:1:28-1:29,
             (Modified (None,
                (:1:28-1:29, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
            arg_opt = None });
         (:1:30-1:35,
          { arg_id = None; arg_ref = 2; arg_cond = None;
            arg_type =
            (:1:30-1:35,
             (Modified (None,
                (:1:30-1:35,
                 (TypeRepeat ((:1:30-1:35, (NatVar (VarRef (1, None)))),
                    [(:1:32-1:33,
                      { r_arg_id = None;
                        r_arg_type =
                        (:1:19-1:27, (TypeVar (VarRef (0, (Some "t"))))) })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    Functions: |}]

let%expect_test "use before define" =
  t "
    a#00 v:B = A;
    b#01 = B;
  ";
  [%expect {|
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "B"), []))
    Constructors:
    (:2:5-2:18,
     { c_id = (:2:5-2:9, (Name "a")); c_magic = 0x0l;
       c_args =
       [(:2:10-2:13,
         { arg_id = (Some (:2:10-2:11, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:2:12-2:13,
            (Modified (None,
               (:2:12-2:13, (TypeExpr ((TypeRef (:2:12-2:13, (1, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:3:5-3:14,
     { c_id = (:3:5-3:9, (Name "b")); c_magic = 0x1l; c_args = []; c_ref = 1;
       c_result_type = 1; c_builtin = false })
    Functions: |}]

let%expect_test "recursive types" =
  t "
    textEmpty#dc3d824f = RichText;
    textBold#6724abc4 text:RichText = RichText;

    textBold2#01 text:RichText2 = RichText2;
    textEmpty2#00 = RichText2;
  ";
  [%expect {|
    Types:
    (Type ((Name "RichText"), []))
    (Type ((Name "RichText2"), []))
    Constructors:
    (:3:5-3:48,
     { c_id = (:3:5-3:22, (Name "textBold")); c_magic = 0x6724ABC4l;
       c_args =
       [(:3:23-3:36,
         { arg_id = (Some (:3:23-3:27, "text")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:28-3:36,
            (Modified (None,
               (:3:28-3:36, (TypeExpr ((TypeRef (:3:28-3:36, (0, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 1; c_result_type = 0; c_builtin = false })
    (:5:5-5:45,
     { c_id = (:5:5-5:17, (Name "textBold2")); c_magic = 0x1l;
       c_args =
       [(:5:18-5:32,
         { arg_id = (Some (:5:18-5:22, "text")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:23-5:32,
            (Modified (None,
               (:5:23-5:32, (TypeExpr ((TypeRef (:5:23-5:32, (1, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 2; c_result_type = 1; c_builtin = false })
    (:2:5-2:35,
     { c_id = (:2:5-2:23, (Name "textEmpty")); c_magic = 0xDC3D824Fl;
       c_args = []; c_ref = 0; c_result_type = 0; c_builtin = false })
    (:6:5-6:31,
     { c_id = (:6:5-6:18, (Name "textEmpty2")); c_magic = 0x0l; c_args = [];
       c_ref = 3; c_result_type = 1; c_builtin = false })
    Functions: |}]

let%expect_test "mutually recursive types" =
  t "
    aEmpty#a0 = A;
    a#a1 b:B = A;
    bEmpty#b0 = B;
    b#b1 a:A = B;
  ";
  [%expect {|
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "B"), []))
    Constructors:
    (:3:5-3:18,
     { c_id = (:3:5-3:9, (Name "a")); c_magic = 0xA1l;
       c_args =
       [(:3:10-3:13,
         { arg_id = (Some (:3:10-3:11, "b")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:12-3:13,
            (Modified (None,
               (:3:12-3:13, (TypeExpr ((TypeRef (:3:12-3:13, (1, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 1; c_result_type = 0; c_builtin = false })
    (:2:5-2:19,
     { c_id = (:2:5-2:14, (Name "aEmpty")); c_magic = 0xA0l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:5:5-5:18,
     { c_id = (:5:5-5:9, (Name "b")); c_magic = 0xB1l;
       c_args =
       [(:5:10-5:13,
         { arg_id = (Some (:5:10-5:11, "a")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:12-5:13,
            (Modified (None,
               (:5:12-5:13, (TypeExpr ((TypeRef (:5:12-5:13, (0, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 3; c_result_type = 1; c_builtin = false })
    (:4:5-4:19,
     { c_id = (:4:5-4:14, (Name "bEmpty")); c_magic = 0xB0l; c_args = [];
       c_ref = 2; c_result_type = 1; c_builtin = false })
    Functions: |}]

let%expect_test "invokeAfterMsg" =
  t ~empty:false "
    ---functions---
    invokeAfterMsg#cb9f372d {X:Type} msg_id:long query:!X = X;
  ";
  [%expect {|
    Types:
    (Type ((Name "Bytes"), []))
    (Type ((Name "Double"), []))
    (Type ((Name "Int"), []))
    (Type ((Name "Int128"), []))
    (Type ((Name "Int256"), []))
    (Type ((Name "Long"), []))
    (Type ((Name "String"), []))
    (Type ((Name "Vector"), [(ParamType "t")]))
    Constructors:
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "bytes")); c_magic = 0xE937BB82l; c_args = [];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "double")); c_magic = 0x2210C154l; c_args = [];
       c_ref = 2; c_result_type = 2; c_builtin = true })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "int")); c_magic = 0xA8509BDAl; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = true })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "int128")); c_magic = 0x84CCF7B7l; c_args = [];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "int256")); c_magic = 0x7BEDEB5Bl; c_args = [];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "long")); c_magic = 0x22076CBAl; c_args = [];
       c_ref = 1; c_result_type = 1; c_builtin = true })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "string")); c_magic = 0xB5286E24l; c_args = [];
       c_ref = 3; c_result_type = 3; c_builtin = true })
    (:1:1-1:1,
     { c_id = (:1:1-1:1, (Name "vector")); c_magic = 0x1CB5C415l;
       c_args =
       [(:1:1-1:1,
         { arg_id = (Some (:1:1-1:1, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:1:1-1:1,
            (Modified (None,
               (:1:1-1:1, (TypeExpr ((TypeRef (:1:1-1:1, (0, None))), []))))));
           arg_opt = (Some `ResultBang) });
         (:1:1-1:1,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:1:1-1:1,
             (Modified (None,
                (:1:1-1:1,
                 (TypeExpr ((TypeRef (:1:1-1:1, (1, (Some (ConstrRef 0))))), [])))
                )));
            arg_opt = None });
         (:1:1-1:1,
          { arg_id = None; arg_ref = 2; arg_cond = None;
            arg_type =
            (:1:1-1:1,
             (Modified (None,
                (:1:1-1:1,
                 (TypeRepeat ((:1:1-1:1, (NatVar (VarRef (1, None)))),
                    [(:1:1-1:1,
                      { r_arg_id = None;
                        r_arg_type = (:1:1-1:1, (TypeVar (VarRef (0, None)))) })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    Functions:
    (:3:5-3:63,
     { f_id = (:3:5-3:28, (Name "invokeAfterMsg")); f_magic = 0xCB9F372Dl;
       f_args =
       [(:3:29-3:37,
         { arg_id = (Some (:3:30-3:31, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:32-3:36,
            (Modified (None,
               (:3:32-3:36, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ArgBang) });
         (:3:38-3:49,
          { arg_id = (Some (:3:38-3:44, "msg_id")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:3:45-3:49,
             (Modified (None,
                (:3:45-3:49,
                 (TypeExpr ((TypeRef (:3:45-3:49, (1, (Some (ConstrRef 1))))),
                    [])))
                )));
            arg_opt = None });
         (:3:50-3:58,
          { arg_id = (Some (:3:50-3:55, "query")); arg_ref = 2; arg_cond = None;
            arg_type =
            (:3:56-3:58,
             (Modified ((Some ModBang),
                (:3:29-3:37, (TypeVar (VarRef (0, (Some "X"))))))));
            arg_opt = None })
         ];
       f_result_type = (:3:29-3:37, (TypeVar (VarRef (0, (Some "X")))));
       f_builtin = false }) |}]

let%expect_test "% - bare modifier" =
  t "
    a = A;
    b {X:Type} = B X;

    t0 v:%A = T0;
    t1 v:%a = T1;
    t2 v1:(%B A) v2:%(B A) v3:%B<A> = T2;
    t3 {X:Type} v:%X = T3 X; // error
    t4 arg:A v1:%arg v2:%(arg) = T4; // error
    t5 v:%%%A = T5;

    c1 = C;
    c2 = C;
    t6 v:%C = T6; // error

    t7 v:%Type = T7; // error
    t8 v:# = T8;
  ";
  [%expect {|
    :8:8-8:16: Invalid use of the `%` modifier
    :9:8-9:13: Invalid use of the `%` modifier
    :9:8-9:13: Invalid use of the `%` modifier
    :14:11-14:12: The `%` modifier must be applied to types with only one constructor
    :16:11-16:15: Cannot apply the `%` modifier to a builtin type
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "B"), [(ParamType "X")]))
    (Type ((Name "C"), []))
    (Type ((Name "T0"), []))
    (Type ((Name "T1"), []))
    (Type ((Name "T2"), []))
    (Type ((Name "T3"), [(ParamType "X")]))
    (Type ((Name "T4"), []))
    (Type ((Name "T5"), []))
    (Type ((Name "T6"), []))
    (Type ((Name "T7"), []))
    (Type ((Name "T8"), []))
    Constructors:
    (:2:5-2:11,
     { c_id = (:2:5-2:6, (Name "a")); c_magic = 0x7AAE25B9l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:3:5-3:22,
     { c_id = (:3:5-3:6, (Name "b")); c_magic = 0x1BE09547l;
       c_args =
       [(:3:7-3:15,
         { arg_id = (Some (:3:8-3:9, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:10-3:14,
            (Modified (None,
               (:3:10-3:14, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:12:5-12:12,
     { c_id = (:12:5-12:7, (Name "c1")); c_magic = 0x5533A014l; c_args = [];
       c_ref = 8; c_result_type = 8; c_builtin = false })
    (:13:5-13:12,
     { c_id = (:13:5-13:7, (Name "c2")); c_magic = 0x1293DAC4l; c_args = [];
       c_ref = 9; c_result_type = 8; c_builtin = false })
    (:5:5-5:18,
     { c_id = (:5:5-5:7, (Name "t0")); c_magic = 0xF658EF54l;
       c_args =
       [(:5:8-5:12,
         { arg_id = (Some (:5:8-5:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:10-5:12,
            (Modified (None,
               (:5:11-5:12,
                (TypeExpr ((TypeRef (:5:11-5:12, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:6:5-6:18,
     { c_id = (:6:5-6:7, (Name "t1")); c_magic = 0x477D0534l;
       c_args =
       [(:6:8-6:12,
         { arg_id = (Some (:6:8-6:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:10-6:12,
            (Modified (None,
               (:6:11-6:12,
                (TypeExpr ((TypeRef (:6:11-6:12, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:7:5-7:42,
     { c_id = (:7:5-7:7, (Name "t2")); c_magic = 0x81379F48l;
       c_args =
       [(:7:8-7:17,
         { arg_id = (Some (:7:8-7:10, "v1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:12-7:16,
            (Modified (None,
               (:7:15-7:14,
                (TypeExpr ((TypeRef (:7:13-7:14, (1, (Some (ConstrRef 1))))),
                   [(ExprType
                       (:7:15-7:16,
                        (TypeExpr ((TypeRef (:7:15-7:16, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None });
         (:7:18-7:27,
          { arg_id = (Some (:7:18-7:20, "v2")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:7:21-7:27,
             (Modified (None,
                (:7:25-7:24,
                 (TypeExpr ((TypeRef (:7:23-7:24, (1, (Some (ConstrRef 1))))),
                    [(ExprType
                        (:7:25-7:26,
                         (TypeExpr ((TypeRef (:7:25-7:26, (0, None))), []))))
                      ]
                    )))
                )));
            arg_opt = None });
         (:7:28-7:36,
          { arg_id = (Some (:7:28-7:30, "v3")); arg_ref = 2; arg_cond = None;
            arg_type =
            (:7:31-7:36,
             (Modified (None,
                (:7:34-7:33,
                 (TypeExpr ((TypeRef (:7:32-7:33, (1, (Some (ConstrRef 1))))),
                    [(ExprType
                        (:7:34-7:35,
                         (TypeExpr ((TypeRef (:7:34-7:35, (0, None))), []))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:8:5-8:29,
     { c_id = (:8:5-8:7, (Name "t3")); c_magic = 0xE4F5C12l;
       c_args =
       [(:8:8-8:16,
         { arg_id = (Some (:8:9-8:10, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:8:11-8:15,
            (Modified (None,
               (:8:11-8:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:8:17-8:21,
          { arg_id = (Some (:8:17-8:18, "v")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:8:19-8:21,
             (Modified (None, (:8:8-8:16, (TypeVar (VarRef (0, (Some "X"))))))));
            arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    (:9:5-9:37,
     { c_id = (:9:5-9:7, (Name "t4")); c_magic = 0xC403A38El;
       c_args =
       [(:9:8-9:13,
         { arg_id = (Some (:9:8-9:11, "arg")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:12-9:13,
            (Modified (None,
               (:9:12-9:13, (TypeExpr ((TypeRef (:9:12-9:13, (0, None))), []))))));
           arg_opt = None });
         (:9:14-9:21,
          { arg_id = (Some (:9:14-9:16, "v1")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:9:17-9:21,
             (Modified (None, (:9:8-9:13, (TypeVar (VarRef (0, (Some "arg"))))))));
            arg_opt = None });
         (:9:22-9:31,
          { arg_id = (Some (:9:22-9:24, "v2")); arg_ref = 2; arg_cond = None;
            arg_type =
            (:9:25-9:31,
             (Modified (None, (:9:8-9:13, (TypeVar (VarRef (0, (Some "arg"))))))));
            arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:10:5-10:20,
     { c_id = (:10:5-10:7, (Name "t5")); c_magic = 0x1A813BDBl;
       c_args =
       [(:10:8-10:14,
         { arg_id = (Some (:10:8-10:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:10:10-10:14,
            (Modified (None,
               (:10:13-10:14,
                (TypeExpr ((TypeRef (:10:13-10:14, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:14:5-14:18,
     { c_id = (:14:5-14:7, (Name "t6")); c_magic = 0xBA462129l;
       c_args =
       [(:14:8-14:12,
         { arg_id = (Some (:14:8-14:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:14:10-14:12,
            (Modified (None,
               (:14:11-14:12,
                (TypeExpr ((TypeRef (:14:11-14:12, (8, None))), [])))
               )));
           arg_opt = None })
         ];
       c_ref = 10; c_result_type = 9; c_builtin = false })
    (:16:5-16:21,
     { c_id = (:16:5-16:7, (Name "t7")); c_magic = 0x3B074675l;
       c_args =
       [(:16:8-16:15,
         { arg_id = (Some (:16:8-16:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:16:10-16:15,
            (Modified (None,
               (:16:11-16:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = None })
         ];
       c_ref = 11; c_result_type = 10; c_builtin = false })
    (:17:5-17:17,
     { c_id = (:17:5-17:7, (Name "t8")); c_magic = 0x36A5AC37l;
       c_args =
       [(:17:8-17:11,
         { arg_id = (Some (:17:8-17:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:17:10-17:11,
            (Modified (None,
               (:17:10-17:11, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None })
         ];
       c_ref = 12; c_result_type = 11; c_builtin = false })
    Functions: |}]

let%expect_test "magic calculation" =
  t "
    a = A;
    builtin ? = Builtin;
    vector {t: Type} # [ t ] = Vector   t;
    t0 {X:#} /*comment*/ X*[A] = T0 X;
    t1 {t:Type} arg:!t = T1;
    t2 v:(%Vector A) = T2;
    t3 {t:Type} arg:Vector<t> = T3 t;
    user {fields:#} id:A first_name:fields.0?A friends:(fields.2?%(Vector A)) = User fields;
    //tnil {X : Type} = Tuple X 0;
    //tcons {X : Type} {n : #} hd:X tl:(Tuple X n) = Tuple X (n+1); // TODO:
    //vector2 {X : Type} (n : #) (v : (Tuple X n)) = Vector2 X;
    ---functions---
    get_users req_fields:# ids:%(Vector A) = Vector %(User req_fields);
  ";
  [%expect {|
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "Builtin"), []))
    (Type ((Name "T0"), [(ParamNat "X")]))
    (Type ((Name "T1"), []))
    (Type ((Name "T2"), []))
    (Type ((Name "T3"), [(ParamType "t")]))
    (Type ((Name "User"), [(ParamNat "fields")]))
    (Type ((Name "Vector"), [(ParamType "t")]))
    Constructors:
    (:2:5-2:11,
     { c_id = (:2:5-2:6, (Name "a")); c_magic = 0x7AAE25B9l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:3:5-3:25,
     { c_id = (:3:5-3:12, (Name "builtin")); c_magic = 0xD4BEEF60l; c_args = [];
       c_ref = 1; c_result_type = 1; c_builtin = true })
    (:5:5-5:39,
     { c_id = (:5:5-5:7, (Name "t0")); c_magic = 0xD4193219l;
       c_args =
       [(:5:8-5:13,
         { arg_id = (Some (:5:9-5:10, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:11-5:12,
            (Modified (None,
               (:5:11-5:12, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = (Some `ResultBang) });
         (:5:26-5:31,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:5:28-5:31,
             (Modified (None,
                (:5:28-5:31,
                 (TypeRepeat ((:5:26-5:27, (NatVar (VarRef (0, (Some "X"))))),
                    [(:5:29-5:30,
                      { r_arg_id = None;
                        r_arg_type =
                        (:5:29-5:30,
                         (TypeExpr ((TypeRef (:5:29-5:30, (0, None))), [])))
                        })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:6:5-6:29,
     { c_id = (:6:5-6:7, (Name "t1")); c_magic = 0x44676141l;
       c_args =
       [(:6:8-6:16,
         { arg_id = (Some (:6:9-6:10, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:11-6:15,
            (Modified (None,
               (:6:11-6:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ArgBang) });
         (:6:17-6:23,
          { arg_id = (Some (:6:17-6:20, "arg")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:6:21-6:23,
             (Modified ((Some ModBang),
                (:6:8-6:16, (TypeVar (VarRef (0, (Some "t"))))))));
            arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:7:5-7:27,
     { c_id = (:7:5-7:7, (Name "t2")); c_magic = 0xD975568Fl;
       c_args =
       [(:7:8-7:21,
         { arg_id = (Some (:7:8-7:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:11-7:20,
            (Modified (None,
               (:7:19-7:18,
                (TypeExpr ((TypeRef (:7:12-7:18, (2, (Some (ConstrRef 2))))),
                   [(ExprType
                       (:7:19-7:20,
                        (TypeExpr ((TypeRef (:7:19-7:20, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    (:8:5-8:38,
     { c_id = (:8:5-8:7, (Name "t3")); c_magic = 0x5F341E04l;
       c_args =
       [(:8:8-8:16,
         { arg_id = (Some (:8:9-8:10, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:8:11-8:15,
            (Modified (None,
               (:8:11-8:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:8:17-8:30,
          { arg_id = (Some (:8:17-8:20, "arg")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:8:21-8:30,
             (Modified (None,
                (:8:21-8:16,
                 (TypeExpr ((TypeRef (:8:21-8:27, (2, None))),
                    [(ExprType (:8:8-8:16, (TypeVar (VarRef (0, (Some "t"))))))]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:9:5-9:93,
     { c_id = (:9:5-9:9, (Name "user")); c_magic = 0xB376C2CBl;
       c_args =
       [(:9:10-9:20,
         { arg_id = (Some (:9:11-9:17, "fields")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:18-9:19,
            (Modified (None,
               (:9:18-9:19, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = (Some `ResultBang) });
         (:9:21-9:25,
          { arg_id = (Some (:9:21-9:23, "id")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:9:24-9:25,
             (Modified (None,
                (:9:24-9:25, (TypeExpr ((TypeRef (:9:24-9:25, (0, None))), [])))
                )));
            arg_opt = None });
         (:9:26-9:47,
          { arg_id = (Some (:9:26-9:36, "first_name")); arg_ref = 2;
            arg_cond =
            (Some (:9:10-9:20, (Cond ((VarRef (0, (Some "fields"))), 0))));
            arg_type =
            (:9:46-9:47,
             (Modified (None,
                (:9:46-9:47, (TypeExpr ((TypeRef (:9:46-9:47, (0, None))), [])))
                )));
            arg_opt = None });
         (:9:48-9:78,
          { arg_id = (Some (:9:48-9:55, "friends")); arg_ref = 3;
            arg_cond =
            (Some (:9:10-9:20, (Cond ((VarRef (0, (Some "fields"))), 2))));
            arg_type =
            (:9:66-9:77,
             (Modified (None,
                (:9:75-9:74,
                 (TypeExpr ((TypeRef (:9:68-9:74, (2, (Some (ConstrRef 2))))),
                    [(ExprType
                        (:9:75-9:76,
                         (TypeExpr ((TypeRef (:9:75-9:76, (0, None))), []))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:4:5-4:43,
     { c_id = (:4:5-4:11, (Name "vector")); c_magic = 0x1CB5C415l;
       c_args =
       [(:4:12-4:21,
         { arg_id = (Some (:4:13-4:14, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:16-4:20,
            (Modified (None,
               (:4:16-4:20, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:4:22-4:23,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:4:22-4:23,
             (Modified (None,
                (:4:22-4:23, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
            arg_opt = None });
         (:4:24-4:29,
          { arg_id = None; arg_ref = 2; arg_cond = None;
            arg_type =
            (:4:24-4:29,
             (Modified (None,
                (:4:24-4:29,
                 (TypeRepeat ((:4:24-4:29, (NatVar (VarRef (1, None)))),
                    [(:4:26-4:27,
                      { r_arg_id = None;
                        r_arg_type =
                        (:4:12-4:21, (TypeVar (VarRef (0, (Some "t"))))) })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    Functions:
    (:14:5-14:72,
     { f_id = (:14:5-14:14, (Name "get_users")); f_magic = 0x7D2C49BDl;
       f_args =
       [(:14:15-14:27,
         { arg_id = (Some (:14:15-14:25, "req_fields")); arg_ref = 0;
           arg_cond = None;
           arg_type =
           (:14:26-14:27,
            (Modified (None,
               (:14:26-14:27, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:14:28-14:43,
          { arg_id = (Some (:14:28-14:31, "ids")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:14:32-14:43,
             (Modified (None,
                (:14:41-14:40,
                 (TypeExpr ((TypeRef (:14:34-14:40, (2, (Some (ConstrRef 2))))),
                    [(ExprType
                        (:14:41-14:42,
                         (TypeExpr ((TypeRef (:14:41-14:42, (0, None))), []))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       f_result_type =
       (:14:60-14:52,
        (TypeExpr ((TypeRef (:14:46-14:52, (2, None))),
           [(ExprType
               (:14:60-14:59,
                (TypeExpr ((TypeRef (:14:55-14:59, (7, (Some (ConstrRef 7))))),
                   [(ExprNat
                       (:14:60-14:70, (NatVar (VarRef (0, (Some "req_fields"))))))
                     ]
                   ))))
             ]
           )));
       f_builtin = false }) |}]

let%expect_test "example from the site" =
  t "
    // built-in types
    int#a8509bda ? = Int;
    long ? = Long;
    double ? = Double;
    string ? = String;
    null = Null;

    vector {t:Type} # [ t ] = Vector t;
    coupleInt {alpha:Type} int alpha = CoupleInt<alpha>;
    coupleStr {gamma:Type} string gamma = CoupleStr gamma;
    /* The name of the type variable is irrelevant: \"gamma\" could be replaced with \"alpha\";
      However, the combinator number will depend on the specific choice. */

    intHash {alpha:Type} vector<coupleInt<alpha>> = IntHash<alpha>;
    strHash {alpha:Type} (vector (coupleStr alpha)) = StrHash alpha;
    intSortedHash {alpha:Type} intHash<alpha> = IntSortedHash<alpha>;
    strSortedHash {alpha:Type} (strHash alpha) = StrSortedHash alpha;

    // custom types
    //pair x:Object y:Object = Pair;
    //triple x:Object y:Object z:Object = Triple;

    user#d23c81a3 id:int first_name:string last_name:string = User;
    no_user#c67599d1 id:int = User;
    group id:int title:string last_name:string = Group;
    no_group = Group;

    ---functions---

    // API functions (aka RPC functions)
    getUser#b0f732d5 int = User;
    getUsers#2d84d5f5 (Vector int) = Vector User;
  ";
  [%expect {|
    Types:
    (Type ((Name "CoupleInt"), [(ParamType "alpha")]))
    (Type ((Name "CoupleStr"), [(ParamType "gamma")]))
    (Type ((Name "Double"), []))
    (Type ((Name "Group"), []))
    (Type ((Name "Int"), []))
    (Type ((Name "IntHash"), [(ParamType "alpha")]))
    (Type ((Name "IntSortedHash"), [(ParamType "alpha")]))
    (Type ((Name "Long"), []))
    (Type ((Name "Null"), []))
    (Type ((Name "StrHash"), [(ParamType "alpha")]))
    (Type ((Name "StrSortedHash"), [(ParamType "alpha")]))
    (Type ((Name "String"), []))
    (Type ((Name "User"), []))
    (Type ((Name "Vector"), [(ParamType "t")]))
    Constructors:
    (:10:5-10:57,
     { c_id = (:10:5-10:14, (Name "coupleInt")); c_magic = 0x7C3C934Dl;
       c_args =
       [(:10:15-10:27,
         { arg_id = (Some (:10:16-10:21, "alpha")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:10:22-10:26,
            (Modified (None,
               (:10:22-10:26, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:10:28-10:31,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:10:28-10:31,
             (Modified (None,
                (:10:28-10:31,
                 (TypeExpr ((TypeRef (:10:28-10:31, (0, (Some (ConstrRef 0))))),
                    [])))
                )));
            arg_opt = None });
         (:10:32-10:37,
          { arg_id = None; arg_ref = 2; arg_cond = None;
            arg_type =
            (:10:32-10:37,
             (Modified (None,
                (:10:15-10:27, (TypeVar (VarRef (0, (Some "alpha"))))))));
            arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:11:5-11:59,
     { c_id = (:11:5-11:14, (Name "coupleStr")); c_magic = 0xE6340DCFl;
       c_args =
       [(:11:15-11:27,
         { arg_id = (Some (:11:16-11:21, "gamma")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:11:22-11:26,
            (Modified (None,
               (:11:22-11:26, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:11:28-11:34,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:11:28-11:34,
             (Modified (None,
                (:11:28-11:34,
                 (TypeExpr ((TypeRef (:11:28-11:34, (3, (Some (ConstrRef 3))))),
                    [])))
                )));
            arg_opt = None });
         (:11:35-11:40,
          { arg_id = None; arg_ref = 2; arg_cond = None;
            arg_type =
            (:11:35-11:40,
             (Modified (None,
                (:11:15-11:27, (TypeVar (VarRef (0, (Some "gamma"))))))));
            arg_opt = None })
         ];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:5:5-5:23,
     { c_id = (:5:5-5:11, (Name "double")); c_magic = 0x2210C154l; c_args = [];
       c_ref = 2; c_result_type = 2; c_builtin = true })
    (:26:5-26:56,
     { c_id = (:26:5-26:10, (Name "group")); c_magic = 0x4387A1F4l;
       c_args =
       [(:26:11-26:17,
         { arg_id = (Some (:26:11-26:13, "id")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:26:14-26:17,
            (Modified (None,
               (:26:14-26:17,
                (TypeExpr ((TypeRef (:26:14-26:17, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None });
         (:26:18-26:30,
          { arg_id = (Some (:26:18-26:23, "title")); arg_ref = 1;
            arg_cond = None;
            arg_type =
            (:26:24-26:30,
             (Modified (None,
                (:26:24-26:30,
                 (TypeExpr ((TypeRef (:26:24-26:30, (3, (Some (ConstrRef 3))))),
                    [])))
                )));
            arg_opt = None });
         (:26:31-26:47,
          { arg_id = (Some (:26:31-26:40, "last_name")); arg_ref = 2;
            arg_cond = None;
            arg_type =
            (:26:41-26:47,
             (Modified (None,
                (:26:41-26:47,
                 (TypeExpr ((TypeRef (:26:41-26:47, (3, (Some (ConstrRef 3))))),
                    [])))
                )));
            arg_opt = None })
         ];
       c_ref = 14; c_result_type = 13; c_builtin = false })
    (:3:5-3:26,
     { c_id = (:3:5-3:17, (Name "int")); c_magic = 0xA8509BDAl; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = true })
    (:15:5-15:68,
     { c_id = (:15:5-15:12, (Name "intHash")); c_magic = 0x658A29E1l;
       c_args =
       [(:15:13-15:25,
         { arg_id = (Some (:15:14-15:19, "alpha")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:15:20-15:24,
            (Modified (None,
               (:15:20-15:24, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:15:26-15:50,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:15:26-15:50,
             (Modified (None,
                (:15:33-15:25,
                 (TypeExpr ((TypeRef (:15:26-15:32, (5, (Some (ConstrRef 5))))),
                    [(ExprType
                        (:15:33-15:25,
                         (TypeExpr (
                            (TypeRef (:15:33-15:42, (6, (Some (ConstrRef 6))))),
                            [(ExprType
                                (:15:13-15:25,
                                 (TypeVar (VarRef (0, (Some "alpha"))))))
                              ]
                            ))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 8; c_result_type = 8; c_builtin = false })
    (:17:5-17:70,
     { c_id = (:17:5-17:18, (Name "intSortedHash")); c_magic = 0xF5736F5El;
       c_args =
       [(:17:19-17:31,
         { arg_id = (Some (:17:20-17:25, "alpha")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:17:26-17:30,
            (Modified (None,
               (:17:26-17:30, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:17:32-17:46,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:17:32-17:46,
             (Modified (None,
                (:17:32-17:31,
                 (TypeExpr ((TypeRef (:17:32-17:39, (8, (Some (ConstrRef 8))))),
                    [(ExprType
                        (:17:19-17:31, (TypeVar (VarRef (0, (Some "alpha"))))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 10; c_result_type = 10; c_builtin = false })
    (:4:5-4:19,
     { c_id = (:4:5-4:9, (Name "long")); c_magic = 0x22076CBAl; c_args = [];
       c_ref = 1; c_result_type = 1; c_builtin = true })
    (:27:5-27:22,
     { c_id = (:27:5-27:13, (Name "no_group")); c_magic = 0x5702DAD8l;
       c_args = []; c_ref = 15; c_result_type = 13; c_builtin = false })
    (:25:5-25:36,
     { c_id = (:25:5-25:21, (Name "no_user")); c_magic = 0xC67599D1l;
       c_args =
       [(:25:22-25:28,
         { arg_id = (Some (:25:22-25:24, "id")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:25:25-25:28,
            (Modified (None,
               (:25:25-25:28,
                (TypeExpr ((TypeRef (:25:25-25:28, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 13; c_result_type = 12; c_builtin = false })
    (:7:5-7:17,
     { c_id = (:7:5-7:9, (Name "null")); c_magic = 0x56730BCCl; c_args = [];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:16:5-16:69,
     { c_id = (:16:5-16:12, (Name "strHash")); c_magic = 0x24D1761Fl;
       c_args =
       [(:16:13-16:25,
         { arg_id = (Some (:16:14-16:19, "alpha")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:16:20-16:24,
            (Modified (None,
               (:16:20-16:24, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:16:26-16:52,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:16:27-16:51,
             (Modified (None,
                (:16:35-16:25,
                 (TypeExpr ((TypeRef (:16:27-16:33, (5, (Some (ConstrRef 5))))),
                    [(ExprType
                        (:16:35-16:25,
                         (TypeExpr (
                            (TypeRef (:16:35-16:44, (7, (Some (ConstrRef 7))))),
                            [(ExprType
                                (:16:13-16:25,
                                 (TypeVar (VarRef (0, (Some "alpha"))))))
                              ]
                            ))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 9; c_result_type = 9; c_builtin = false })
    (:18:5-18:70,
     { c_id = (:18:5-18:18, (Name "strSortedHash")); c_magic = 0x386A14FBl;
       c_args =
       [(:18:19-18:31,
         { arg_id = (Some (:18:20-18:25, "alpha")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:18:26-18:30,
            (Modified (None,
               (:18:26-18:30, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:18:32-18:47,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:18:33-18:46,
             (Modified (None,
                (:18:33-18:31,
                 (TypeExpr ((TypeRef (:18:33-18:40, (9, (Some (ConstrRef 9))))),
                    [(ExprType
                        (:18:19-18:31, (TypeVar (VarRef (0, (Some "alpha"))))))
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 11; c_result_type = 11; c_builtin = false })
    (:6:5-6:23,
     { c_id = (:6:5-6:11, (Name "string")); c_magic = 0xB5286E24l; c_args = [];
       c_ref = 3; c_result_type = 3; c_builtin = true })
    (:24:5-24:68,
     { c_id = (:24:5-24:18, (Name "user")); c_magic = 0xD23C81A3l;
       c_args =
       [(:24:19-24:25,
         { arg_id = (Some (:24:19-24:21, "id")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:24:22-24:25,
            (Modified (None,
               (:24:22-24:25,
                (TypeExpr ((TypeRef (:24:22-24:25, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None });
         (:24:26-24:43,
          { arg_id = (Some (:24:26-24:36, "first_name")); arg_ref = 1;
            arg_cond = None;
            arg_type =
            (:24:37-24:43,
             (Modified (None,
                (:24:37-24:43,
                 (TypeExpr ((TypeRef (:24:37-24:43, (3, (Some (ConstrRef 3))))),
                    [])))
                )));
            arg_opt = None });
         (:24:44-24:60,
          { arg_id = (Some (:24:44-24:53, "last_name")); arg_ref = 2;
            arg_cond = None;
            arg_type =
            (:24:54-24:60,
             (Modified (None,
                (:24:54-24:60,
                 (TypeExpr ((TypeRef (:24:54-24:60, (3, (Some (ConstrRef 3))))),
                    [])))
                )));
            arg_opt = None })
         ];
       c_ref = 12; c_result_type = 12; c_builtin = false })
    (:9:5-9:40,
     { c_id = (:9:5-9:11, (Name "vector")); c_magic = 0x1CB5C415l;
       c_args =
       [(:9:12-9:20,
         { arg_id = (Some (:9:13-9:14, "t")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:15-9:19,
            (Modified (None,
               (:9:15-9:19, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) });
         (:9:21-9:22,
          { arg_id = None; arg_ref = 1; arg_cond = None;
            arg_type =
            (:9:21-9:22,
             (Modified (None,
                (:9:21-9:22, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
            arg_opt = None });
         (:9:23-9:28,
          { arg_id = None; arg_ref = 2; arg_cond = None;
            arg_type =
            (:9:23-9:28,
             (Modified (None,
                (:9:23-9:28,
                 (TypeRepeat ((:9:23-9:28, (NatVar (VarRef (1, None)))),
                    [(:9:25-9:26,
                      { r_arg_id = None;
                        r_arg_type =
                        (:9:12-9:20, (TypeVar (VarRef (0, (Some "t"))))) })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    Functions:
    (:32:5-32:33,
     { f_id = (:32:5-32:21, (Name "getUser")); f_magic = 0xB0F732D5l;
       f_args =
       [(:32:22-32:25,
         { arg_id = None; arg_ref = 0; arg_cond = None;
           arg_type =
           (:32:22-32:25,
            (Modified (None,
               (:32:22-32:25,
                (TypeExpr ((TypeRef (:32:22-32:25, (0, (Some (ConstrRef 0))))),
                   [])))
               )));
           arg_opt = None })
         ];
       f_result_type =
       (:32:28-32:32, (TypeExpr ((TypeRef (:32:28-32:32, (12, None))), [])));
       f_builtin = false })
    (:33:5-33:50,
     { f_id = (:33:5-33:22, (Name "getUsers")); f_magic = 0x2D84D5F5l;
       f_args =
       [(:33:23-33:35,
         { arg_id = None; arg_ref = 0; arg_cond = None;
           arg_type =
           (:33:24-33:34,
            (Modified (None,
               (:33:31-33:30,
                (TypeExpr ((TypeRef (:33:24-33:30, (5, None))),
                   [(ExprType
                       (:33:31-33:34,
                        (TypeExpr (
                           (TypeRef (:33:31-33:34, (0, (Some (ConstrRef 0))))),
                           []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       f_result_type =
       (:33:45-33:44,
        (TypeExpr ((TypeRef (:33:38-33:44, (5, None))),
           [(ExprType
               (:33:45-33:49,
                (TypeExpr ((TypeRef (:33:45-33:49, (12, None))), []))))
             ]
           )));
       f_builtin = false }) |}]

let%expect_test "nullary type constrs `nat` and `Type` shouldn't accept params" =
  t "
    e#0 = E;
    t0 v:(nat E) = T0; // error
    t1 v:(Type E) = T1; // error
    t3 v:(nat) = T3;
    t4 v:(Type) = T4;
    t5 v:Type = T5;
    t6 v:# = T6;
  ";
  [%expect {|
    :3:15-3:14: The type constructor `nat` expects 0 type parameter, but is applied to 1
    :4:16-4:15: The type constructor `Type` expects 0 type parameter, but is applied to 1
    Types:
    (Type ((Name "E"), []))
    (Type ((Name "T0"), []))
    (Type ((Name "T1"), []))
    (Type ((Name "T3"), []))
    (Type ((Name "T4"), []))
    (Type ((Name "T5"), []))
    (Type ((Name "T6"), []))
    Constructors:
    (:2:5-2:13,
     { c_id = (:2:5-2:8, (Name "e")); c_magic = 0x0l; c_args = []; c_ref = 0;
       c_result_type = 0; c_builtin = false })
    (:3:5-3:23,
     { c_id = (:3:5-3:7, (Name "t0")); c_magic = 0x43D67042l;
       c_args =
       [(:3:8-3:17,
         { arg_id = (Some (:3:8-3:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:11-3:16,
            (Modified (None,
               (:3:11-3:14, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:4:5-4:24,
     { c_id = (:4:5-4:7, (Name "t1")); c_magic = 0xF5A4D559l;
       c_args =
       [(:4:8-4:18,
         { arg_id = (Some (:4:8-4:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:11-4:17,
            (Modified (None,
               (:4:11-4:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:5:5-5:21,
     { c_id = (:5:5-5:7, (Name "t3")); c_magic = 0x59978E48l;
       c_args =
       [(:5:8-5:15,
         { arg_id = (Some (:5:8-5:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:11-5:14,
            (Modified (None,
               (:5:11-5:14, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:6:5-6:22,
     { c_id = (:6:5-6:7, (Name "t4")); c_magic = 0xB89162E7l;
       c_args =
       [(:6:8-6:16,
         { arg_id = (Some (:6:8-6:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:11-6:15,
            (Modified (None,
               (:6:11-6:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:7:5-7:20,
     { c_id = (:7:5-7:7, (Name "t5")); c_magic = 0x12008BF4l;
       c_args =
       [(:7:8-7:14,
         { arg_id = (Some (:7:8-7:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:10-7:14,
            (Modified (None,
               (:7:10-7:14, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    (:8:5-8:17,
     { c_id = (:8:5-8:7, (Name "t6")); c_magic = 0xCFD4B183l;
       c_args =
       [(:8:8-8:11,
         { arg_id = (Some (:8:8-8:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:8:10-8:11,
            (Modified (None,
               (:8:10-8:11, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    Functions: |}]

let%expect_test "duplicate argument names" =
  t "
    a#0 = A;
    b#1 = B;
    t0 v1:A v1:v1 v2:v1 = T0; // error
    t1 v1:A v1:B v2:v1 = T1; // error
  ";
  [%expect {|
    :4:13-4:15: Argument `v1` is already defined, skipping
    :5:13-5:15: Argument `v1` is already defined, skipping
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "B"), []))
    (Type ((Name "T0"), []))
    (Type ((Name "T1"), []))
    Constructors:
    (:2:5-2:13,
     { c_id = (:2:5-2:8, (Name "a")); c_magic = 0x0l; c_args = []; c_ref = 0;
       c_result_type = 0; c_builtin = false })
    (:3:5-3:13,
     { c_id = (:3:5-3:8, (Name "b")); c_magic = 0x1l; c_args = []; c_ref = 1;
       c_result_type = 1; c_builtin = false })
    (:4:5-4:30,
     { c_id = (:4:5-4:7, (Name "t0")); c_magic = 0xAB27C0ADl;
       c_args =
       [(:4:8-4:12,
         { arg_id = (Some (:4:8-4:10, "v1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:11-4:12,
            (Modified (None,
               (:4:11-4:12, (TypeExpr ((TypeRef (:4:11-4:12, (0, None))), []))))));
           arg_opt = None });
         (:4:19-4:24,
          { arg_id = (Some (:4:19-4:21, "v2")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:4:22-4:24,
             (Modified (None, (:4:8-4:12, (TypeVar (VarRef (0, (Some "v1"))))))));
            arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:5:5-5:29,
     { c_id = (:5:5-5:7, (Name "t1")); c_magic = 0x7E094497l;
       c_args =
       [(:5:8-5:12,
         { arg_id = (Some (:5:8-5:10, "v1")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:11-5:12,
            (Modified (None,
               (:5:11-5:12, (TypeExpr ((TypeRef (:5:11-5:12, (0, None))), []))))));
           arg_opt = None });
         (:5:18-5:23,
          { arg_id = (Some (:5:18-5:20, "v2")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:5:21-5:23,
             (Modified (None, (:5:8-5:12, (TypeVar (VarRef (0, (Some "v1"))))))));
            arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    Functions: |}]

let%expect_test "references to undefined types" =
  t "
    e#e = E;
    t0 v:U0 = T0; // error
    t1 v:u1 = T1; // error
    t2 v:%U2 = T2; // error
    t3 v:%(U3 e E) = T3; // error
    t4 v:(%U4 e E) = T4; // error
    t5 v:(u5 E e) = T5; // error
    t6 v:(U6 E e) = T6; // error
    t7 v:%nS.U7 = T7; // error
    t8 v:ns.U8 = T8; // error
    ---functions---
    f0 v:i0 = E; // error
  ";
  [%expect {|
    :3:10-3:12: Type `U0` is not defined
    :4:10-4:12: Constructor `u1` is not defined
    :5:11-5:13: Type `U2` is not defined
    :6:12-6:14: Type `U3` is not defined
    :7:12-7:14: Type `U4` is not defined
    :8:11-8:13: Constructor `u5` is not defined
    :9:11-9:13: Type `U6` is not defined
    :10:11-10:16: Type `nS.U7` is not defined
    :11:10-11:15: Type `ns.U8` is not defined
    :13:10-13:12: Constructor `i0` is not defined
    Types:
    (Type ((Name "E"), []))
    (Type ((Name "T0"), []))
    (Type ((Name "T1"), []))
    (Type ((Name "T2"), []))
    (Type ((Name "T3"), []))
    (Type ((Name "T4"), []))
    (Type ((Name "T5"), []))
    (Type ((Name "T6"), []))
    (Type ((Name "T7"), []))
    (Type ((Name "T8"), []))
    Constructors:
    (:2:5-2:13,
     { c_id = (:2:5-2:8, (Name "e")); c_magic = 0xEl; c_args = []; c_ref = 0;
       c_result_type = 0; c_builtin = false })
    (:3:5-3:18,
     { c_id = (:3:5-3:7, (Name "t0")); c_magic = 0xDA4EE11Bl;
       c_args =
       [(:3:8-3:12,
         { arg_id = (Some (:3:8-3:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:10-3:12,
            (Modified (None,
               (:3:10-3:12,
                (TypeExpr ((TypeRefUnd (:3:10-3:12, (TypeIdBoxed (Name "U0")))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:4:5-4:18,
     { c_id = (:4:5-4:7, (Name "t1")); c_magic = 0x6826E474l;
       c_args =
       [(:4:8-4:12,
         { arg_id = (Some (:4:8-4:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:10-4:12,
            (Modified (None,
               (:4:10-4:12,
                (TypeExpr ((TypeRefUnd (:4:10-4:12, (TypeIdBare (Name "u1")))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:5:5-5:19,
     { c_id = (:5:5-5:7, (Name "t2")); c_magic = 0x2FFB4483l;
       c_args =
       [(:5:8-5:13,
         { arg_id = (Some (:5:8-5:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:10-5:13,
            (Modified (None,
               (:5:11-5:13,
                (TypeExpr ((TypeRefUnd (:5:11-5:13, (TypeIdBare (Name "u2")))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:6:5-6:25,
     { c_id = (:6:5-6:7, (Name "t3")); c_magic = 0x625EFD8El;
       c_args =
       [(:6:8-6:19,
         { arg_id = (Some (:6:8-6:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:10-6:19,
            (Modified (None,
               (:6:17-6:14,
                (TypeExpr ((TypeRefUnd (:6:12-6:14, (TypeIdBare (Name "u3")))),
                   [(ExprType
                       (:6:15-6:16,
                        (TypeExpr (
                           (TypeRef (:6:15-6:16, (0, (Some (ConstrRef 0))))),
                           []))));
                     (ExprType
                        (:6:17-6:18,
                         (TypeExpr ((TypeRef (:6:17-6:18, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:7:5-7:25,
     { c_id = (:7:5-7:7, (Name "t4")); c_magic = 0x3A2B088Fl;
       c_args =
       [(:7:8-7:19,
         { arg_id = (Some (:7:8-7:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:11-7:18,
            (Modified (None,
               (:7:17-7:14,
                (TypeExpr ((TypeRefUnd (:7:12-7:14, (TypeIdBare (Name "u4")))),
                   [(ExprType
                       (:7:15-7:16,
                        (TypeExpr (
                           (TypeRef (:7:15-7:16, (0, (Some (ConstrRef 0))))),
                           []))));
                     (ExprType
                        (:7:17-7:18,
                         (TypeExpr ((TypeRef (:7:17-7:18, (0, None))), []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    (:8:5-8:24,
     { c_id = (:8:5-8:7, (Name "t5")); c_magic = 0x37A9E50Cl;
       c_args =
       [(:8:8-8:18,
         { arg_id = (Some (:8:8-8:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:8:11-8:17,
            (Modified (None,
               (:8:16-8:13,
                (TypeExpr ((TypeRefUnd (:8:11-8:13, (TypeIdBare (Name "u5")))),
                   [(ExprType
                       (:8:14-8:15,
                        (TypeExpr ((TypeRef (:8:14-8:15, (0, None))), []))));
                     (ExprType
                        (:8:16-8:17,
                         (TypeExpr (
                            (TypeRef (:8:16-8:17, (0, (Some (ConstrRef 0))))),
                            []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:9:5-9:24,
     { c_id = (:9:5-9:7, (Name "t6")); c_magic = 0xD276EA1Bl;
       c_args =
       [(:9:8-9:18,
         { arg_id = (Some (:9:8-9:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:11-9:17,
            (Modified (None,
               (:9:16-9:13,
                (TypeExpr ((TypeRefUnd (:9:11-9:13, (TypeIdBoxed (Name "U6")))),
                   [(ExprType
                       (:9:14-9:15,
                        (TypeExpr ((TypeRef (:9:14-9:15, (0, None))), []))));
                     (ExprType
                        (:9:16-9:17,
                         (TypeExpr (
                            (TypeRef (:9:16-9:17, (0, (Some (ConstrRef 0))))),
                            []))))
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:10:5-10:22,
     { c_id = (:10:5-10:7, (Name "t7")); c_magic = 0xF3D280A7l;
       c_args =
       [(:10:8-10:16,
         { arg_id = (Some (:10:8-10:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:10:10-10:16,
            (Modified (None,
               (:10:11-10:16,
                (TypeExpr (
                   (TypeRefUnd (:10:11-10:16, (TypeIdBare (NameNs ("nS", "u7"))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 8; c_result_type = 8; c_builtin = false })
    (:11:5-11:21,
     { c_id = (:11:5-11:7, (Name "t8")); c_magic = 0xF900BE2l;
       c_args =
       [(:11:8-11:15,
         { arg_id = (Some (:11:8-11:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:11:10-11:15,
            (Modified (None,
               (:11:10-11:15,
                (TypeExpr (
                   (TypeRefUnd
                      (:11:10-11:15, (TypeIdBoxed (NameNs ("ns", "U8"))))),
                   [])))
               )));
           arg_opt = None })
         ];
       c_ref = 9; c_result_type = 9; c_builtin = false })
    Functions:
    (:13:5-13:17,
     { f_id = (:13:5-13:7, (Name "f0")); f_magic = 0xCEA12153l;
       f_args =
       [(:13:8-13:12,
         { arg_id = (Some (:13:8-13:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:13:10-13:12,
            (Modified (None,
               (:13:10-13:12,
                (TypeExpr ((TypeRefUnd (:13:10-13:12, (TypeIdBare (Name "i0")))),
                   [])))
               )));
           arg_opt = None })
         ];
       f_result_type =
       (:13:15-13:16, (TypeExpr ((TypeRef (:13:15-13:16, (0, None))), [])));
       f_builtin = false }) |}]

let%expect_test "constructors with the !-modified result type" =
  t "
    a = A;
    b = B;

    f x:A = !B;
  ";
  [%expect {|
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "B"), []))
    Constructors:
    (:2:5-2:11,
     { c_id = (:2:5-2:6, (Name "a")); c_magic = 0x7AAE25B9l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:3:5-3:11,
     { c_id = (:3:5-3:6, (Name "b")); c_magic = 0xA4070ED3l; c_args = [];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    Functions:
    (:5:5-5:16,
     { f_id = (:5:5-5:6, (Name "f")); f_magic = 0xE5A04BD5l;
       f_args =
       [(:5:7-5:10,
         { arg_id = (Some (:5:7-5:8, "x")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:9-5:10,
            (Modified (None,
               (:5:9-5:10, (TypeExpr ((TypeRef (:5:9-5:10, (0, None))), []))))));
           arg_opt = None })
         ];
       f_result_type =
       (:5:14-5:15, (TypeExpr ((TypeRef (:5:14-5:15, (1, None))), [])));
       f_builtin = false }) |}]

let%expect_test "`!` must not matter in a function's result type" =
  t "
    e = E;
    ---functions---
    f0 {X:Type} v:!X = !E;
  ";
  [%expect {|
    Types:
    (Type ((Name "E"), []))
    Constructors:
    (:2:5-2:11,
     { c_id = (:2:5-2:6, (Name "e")); c_magic = 0x88434760l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    Functions:
    (:4:5-4:27,
     { f_id = (:4:5-4:7, (Name "f0")); f_magic = 0x47E92BF6l;
       f_args =
       [(:4:8-4:16,
         { arg_id = (Some (:4:9-4:10, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:11-4:15,
            (Modified (None,
               (:4:11-4:15, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ArgBang) });
         (:4:17-4:21,
          { arg_id = (Some (:4:17-4:18, "v")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:4:19-4:21,
             (Modified ((Some ModBang),
                (:4:8-4:16, (TypeVar (VarRef (0, (Some "X"))))))));
            arg_opt = None })
         ];
       f_result_type =
       (:4:25-4:26, (TypeExpr ((TypeRef (:4:25-4:26, (0, None))), [])));
       f_builtin = false }) |}]

let%expect_test "combinators with `_` name" =
  t "
    _ v:# = A;
  ";
  [%expect {|
    :2:5-2:6: Not supported: combinator without id
    Types:
    (Type ((Name "A"), []))
    Constructors:
    (:2:5-2:15,
     { c_id = (:2:5-2:6, (Name "_")); c_magic = 0x672E008l;
       c_args =
       [(:2:7-2:10,
         { arg_id = (Some (:2:7-2:8, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:2:9-2:10,
            (Modified (None,
               (:2:9-2:10, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None })
         ];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    Functions: |}]

let%expect_test "conditional definitions" =
  t "
    e = E;
    t0 flags:# arg:flags?E = T0; // error
    t1 v:# arg:v.51?E = T1; // error: bit shouldn't be more than 32
    t2 arg:vv.9?E = T2; // error: undefined variable
    t3 flags:# v:flags.8?E = T3;
  ";
  [%expect {|
    :3:20-3:26: Conditionals without bit are not supported, 0 is used instead
    :4:18-4:20: Cannot use more than 32 bits for a conditional
    :5:12-5:14: Variable `vv` is not defined
    Types:
    (Type ((Name "E"), []))
    (Type ((Name "T0"), []))
    (Type ((Name "T1"), []))
    (Type ((Name "T2"), []))
    (Type ((Name "T3"), []))
    Constructors:
    (:2:5-2:11,
     { c_id = (:2:5-2:6, (Name "e")); c_magic = 0x88434760l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:3:5-3:33,
     { c_id = (:3:5-3:7, (Name "t0")); c_magic = 0x1BF75951l;
       c_args =
       [(:3:8-3:15,
         { arg_id = (Some (:3:8-3:13, "flags")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:14-3:15,
            (Modified (None,
               (:3:14-3:15, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:3:16-3:27,
          { arg_id = (Some (:3:16-3:19, "arg")); arg_ref = 1;
            arg_cond =
            (Some (:3:8-3:15, (Cond ((VarRef (0, (Some "flags"))), 0))));
            arg_type =
            (:3:26-3:27,
             (Modified (None,
                (:3:26-3:27, (TypeExpr ((TypeRef (:3:26-3:27, (0, None))), [])))
                )));
            arg_opt = None })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:4:5-4:28,
     { c_id = (:4:5-4:7, (Name "t1")); c_magic = 0xF8367DA8l;
       c_args =
       [(:4:8-4:11,
         { arg_id = (Some (:4:8-4:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:10-4:11,
            (Modified (None,
               (:4:10-4:11, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:4:12-4:22,
          { arg_id = (Some (:4:12-4:15, "arg")); arg_ref = 1;
            arg_cond = (Some (:4:8-4:11, (Cond ((VarRef (0, (Some "v"))), 51))));
            arg_type =
            (:4:21-4:22,
             (Modified (None,
                (:4:21-4:22, (TypeExpr ((TypeRef (:4:21-4:22, (0, None))), [])))
                )));
            arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:5:5-5:24,
     { c_id = (:5:5-5:7, (Name "t2")); c_magic = 0x302EA0CFl;
       c_args =
       [(:5:8-5:18,
         { arg_id = (Some (:5:8-5:11, "arg")); arg_ref = 0;
           arg_cond = (Some (:5:12-5:14, (Cond ((VarRefUnd "vv"), 9))));
           arg_type =
           (:5:17-5:18,
            (Modified (None,
               (:5:17-5:18, (TypeExpr ((TypeRef (:5:17-5:18, (0, None))), []))))));
           arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:6:5-6:33,
     { c_id = (:6:5-6:7, (Name "t3")); c_magic = 0x2E428CD5l;
       c_args =
       [(:6:8-6:15,
         { arg_id = (Some (:6:8-6:13, "flags")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:14-6:15,
            (Modified (None,
               (:6:14-6:15, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:6:16-6:27,
          { arg_id = (Some (:6:16-6:17, "v")); arg_ref = 1;
            arg_cond =
            (Some (:6:8-6:15, (Cond ((VarRef (0, (Some "flags"))), 8))));
            arg_type =
            (:6:26-6:27,
             (Modified (None,
                (:6:26-6:27, (TypeExpr ((TypeRef (:6:26-6:27, (0, None))), [])))
                )));
            arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    Functions: |}]

let%expect_test "repetitions" =
  t "
    e = E;
    b {X:Type} = B X;
    t0 5*[e E vv:E] = T0;
    t1 v:# v2:[E e (B E)] = T1;
    t2 v:# a:E v2:v*[E] = T2;
    t3 v:E arg:v.9?E = T3; // error: `v` should be of type `#`
    t4 f:# v:f.1?# arg:v*[E] = T4; // error: cond vars can't be the multiplicity
    t5 [E] = T5; // error: repetitition w/o multiplicity isn't allowed as first arg
    t6 flags:# r:[v:flags.2?E] = T6; // error: repetition can't contain conditionals
  ";
  [%expect {|
    :8:12-8:19: A nat variable cannot be conditional
    :8:24-8:25: The multiplicity is not a valid nat expression
    :9:8-9:11: A repetition without multiplicity cannot be the first argument
    :10:21-10:29: Conditionals are not allowed inside repetitions
    Types:
    (Type ((Name "B"), [(ParamType "X")]))
    (Type ((Name "E"), []))
    (Type ((Name "T0"), []))
    (Type ((Name "T1"), []))
    (Type ((Name "T2"), []))
    (Type ((Name "T3"), []))
    (Type ((Name "T4"), []))
    (Type ((Name "T5"), []))
    (Type ((Name "T6"), []))
    Constructors:
    (:3:5-3:22,
     { c_id = (:3:5-3:6, (Name "b")); c_magic = 0x1BE09547l;
       c_args =
       [(:3:7-3:15,
         { arg_id = (Some (:3:8-3:9, "X")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:3:10-3:14,
            (Modified (None,
               (:3:10-3:14, (TypeExpr ((TypeRefBuiltin BuiltinType), []))))));
           arg_opt = (Some `ResultBang) })
         ];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:2:5-2:11,
     { c_id = (:2:5-2:6, (Name "e")); c_magic = 0x88434760l; c_args = [];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:4:5-4:26,
     { c_id = (:4:5-4:7, (Name "t0")); c_magic = 0x3AD1C2E4l;
       c_args =
       [(:4:8-4:20,
         { arg_id = None; arg_ref = 0; arg_cond = None;
           arg_type =
           (:4:10-4:20,
            (Modified (None,
               (:4:10-4:20,
                (TypeRepeat ((:4:8-4:9, (NatConst 5)),
                   [(:4:11-4:12,
                     { r_arg_id = None;
                       r_arg_type =
                       (:4:11-4:12,
                        (TypeExpr (
                           (TypeRef (:4:11-4:12, (0, (Some (ConstrRef 0))))),
                           [])))
                       });
                     (:4:13-4:14,
                      { r_arg_id = None;
                        r_arg_type =
                        (:4:13-4:14,
                         (TypeExpr ((TypeRef (:4:13-4:14, (0, None))), [])))
                        });
                     (:4:15-4:19,
                      { r_arg_id = (Some (:4:15-4:17, "vv"));
                        r_arg_type =
                        (:4:18-4:19,
                         (TypeExpr ((TypeRef (:4:18-4:19, (0, None))), [])))
                        })
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 2; c_result_type = 2; c_builtin = false })
    (:5:5-5:32,
     { c_id = (:5:5-5:7, (Name "t1")); c_magic = 0xCC9E222Dl;
       c_args =
       [(:5:8-5:11,
         { arg_id = (Some (:5:8-5:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:5:10-5:11,
            (Modified (None,
               (:5:10-5:11, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:5:12-5:26,
          { arg_id = (Some (:5:12-5:14, "v2")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:5:15-5:26,
             (Modified (None,
                (:5:15-5:26,
                 (TypeRepeat ((:5:15-5:26, (NatVar (VarRef (0, None)))),
                    [(:5:16-5:17,
                      { r_arg_id = None;
                        r_arg_type =
                        (:5:16-5:17,
                         (TypeExpr ((TypeRef (:5:16-5:17, (0, None))), [])))
                        });
                      (:5:18-5:19,
                       { r_arg_id = None;
                         r_arg_type =
                         (:5:18-5:19,
                          (TypeExpr (
                             (TypeRef (:5:18-5:19, (0, (Some (ConstrRef 0))))),
                             [])))
                         });
                      (:5:20-5:25,
                       { r_arg_id = None;
                         r_arg_type =
                         (:5:23-5:22,
                          (TypeExpr ((TypeRef (:5:21-5:22, (1, None))),
                             [(ExprType
                                 (:5:23-5:24,
                                  (TypeExpr ((TypeRef (:5:23-5:24, (0, None))),
                                     []))))
                               ]
                             )))
                         })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 3; c_result_type = 3; c_builtin = false })
    (:6:5-6:30,
     { c_id = (:6:5-6:7, (Name "t2")); c_magic = 0xE6D79A98l;
       c_args =
       [(:6:8-6:11,
         { arg_id = (Some (:6:8-6:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:6:10-6:11,
            (Modified (None,
               (:6:10-6:11, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:6:12-6:15,
          { arg_id = (Some (:6:12-6:13, "a")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:6:14-6:15,
             (Modified (None,
                (:6:14-6:15, (TypeExpr ((TypeRef (:6:14-6:15, (0, None))), [])))
                )));
            arg_opt = None });
         (:6:16-6:24,
          { arg_id = (Some (:6:16-6:18, "v2")); arg_ref = 2; arg_cond = None;
            arg_type =
            (:6:21-6:24,
             (Modified (None,
                (:6:21-6:24,
                 (TypeRepeat ((:6:19-6:20, (NatVar (VarRef (0, (Some "v"))))),
                    [(:6:22-6:23,
                      { r_arg_id = None;
                        r_arg_type =
                        (:6:22-6:23,
                         (TypeExpr ((TypeRef (:6:22-6:23, (0, None))), [])))
                        })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 4; c_result_type = 4; c_builtin = false })
    (:7:5-7:27,
     { c_id = (:7:5-7:7, (Name "t3")); c_magic = 0xDF8F5DE5l;
       c_args =
       [(:7:8-7:11,
         { arg_id = (Some (:7:8-7:9, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:7:10-7:11,
            (Modified (None,
               (:7:10-7:11, (TypeExpr ((TypeRef (:7:10-7:11, (0, None))), []))))));
           arg_opt = None });
         (:7:12-7:21,
          { arg_id = (Some (:7:12-7:15, "arg")); arg_ref = 1;
            arg_cond = (Some (:7:8-7:11, (Cond ((VarRef (0, (Some "v"))), 9))));
            arg_type =
            (:7:20-7:21,
             (Modified (None,
                (:7:20-7:21, (TypeExpr ((TypeRef (:7:20-7:21, (0, None))), [])))
                )));
            arg_opt = None })
         ];
       c_ref = 5; c_result_type = 5; c_builtin = false })
    (:8:5-8:35,
     { c_id = (:8:5-8:7, (Name "t4")); c_magic = 0x5D711663l;
       c_args =
       [(:8:8-8:11,
         { arg_id = (Some (:8:8-8:9, "f")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:8:10-8:11,
            (Modified (None,
               (:8:10-8:11, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:8:12-8:19,
          { arg_id = (Some (:8:12-8:13, "v")); arg_ref = 1;
            arg_cond = (Some (:8:8-8:11, (Cond ((VarRef (0, (Some "f"))), 1))));
            arg_type =
            (:8:18-8:19,
             (Modified (None,
                (:8:18-8:19, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
            arg_opt = None });
         (:8:20-8:29,
          { arg_id = (Some (:8:20-8:23, "arg")); arg_ref = 2; arg_cond = None;
            arg_type =
            (:8:26-8:29,
             (Modified (None,
                (:8:26-8:29,
                 (TypeRepeat ((:8:24-8:25, (NatConst 1)),
                    [(:8:27-8:28,
                      { r_arg_id = None;
                        r_arg_type =
                        (:8:27-8:28,
                         (TypeExpr ((TypeRef (:8:27-8:28, (0, None))), [])))
                        })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 6; c_result_type = 6; c_builtin = false })
    (:9:5-9:17,
     { c_id = (:9:5-9:7, (Name "t5")); c_magic = 0xD1539A98l;
       c_args =
       [(:9:8-9:11,
         { arg_id = None; arg_ref = 0; arg_cond = None;
           arg_type =
           (:9:8-9:11,
            (Modified (None,
               (:9:8-9:11,
                (TypeRepeat ((:9:8-9:11, (NatConst 1)),
                   [(:9:9-9:10,
                     { r_arg_id = None;
                       r_arg_type =
                       (:9:9-9:10,
                        (TypeExpr ((TypeRef (:9:9-9:10, (0, None))), [])))
                       })
                     ]
                   )))
               )));
           arg_opt = None })
         ];
       c_ref = 7; c_result_type = 7; c_builtin = false })
    (:10:5-10:37,
     { c_id = (:10:5-10:7, (Name "t6")); c_magic = 0x2225D109l;
       c_args =
       [(:10:8-10:15,
         { arg_id = (Some (:10:8-10:13, "flags")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:10:14-10:15,
            (Modified (None,
               (:10:14-10:15, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None });
         (:10:16-10:31,
          { arg_id = (Some (:10:16-10:17, "r")); arg_ref = 1; arg_cond = None;
            arg_type =
            (:10:18-10:31,
             (Modified (None,
                (:10:18-10:31,
                 (TypeRepeat ((:10:18-10:31, (NatVar (VarRef (0, None)))),
                    [(:10:19-10:30,
                      { r_arg_id = (Some (:10:19-10:20, "v"));
                        r_arg_type =
                        (:10:29-10:30,
                         (TypeExpr ((TypeRef (:10:29-10:30, (0, None))), [])))
                        })
                      ]
                    )))
                )));
            arg_opt = None })
         ];
       c_ref = 8; c_result_type = 8; c_builtin = false })
    Functions: |}]

let%expect_test "duplicate combinator names" =
  t "
    a v:# = A;
    a = A; // error
    b = B;
    ---functions---
    b = A;
    c = A;
    c = B; // error
    ---types---
    ns.d1 = D1;
    ns.d2 = D2;
    ns1.e = ns1.E;
    ns2.e = ns2.E;
    ns3.f = F;
    ns3.f = F; // error
  ";
  [%expect {|
    :3:5-3:6: Constructor `a` is already defined, skipping
    :15:5-15:10: Constructor `ns3.f` is already defined, skipping
    :8:5-8:6: Function `c` is already defined, skipping
    Types:
    (Type ((Name "A"), []))
    (Type ((Name "B"), []))
    (Type ((Name "D1"), []))
    (Type ((Name "D2"), []))
    (Type ((Name "F"), []))
    (Type ((NameNs ("ns1", "E")), []))
    (Type ((NameNs ("ns2", "E")), []))
    Constructors:
    (:2:5-2:15,
     { c_id = (:2:5-2:6, (Name "a")); c_magic = 0xE081FB48l;
       c_args =
       [(:2:7-2:10,
         { arg_id = (Some (:2:7-2:8, "v")); arg_ref = 0; arg_cond = None;
           arg_type =
           (:2:9-2:10,
            (Modified (None,
               (:2:9-2:10, (TypeExpr ((TypeRefBuiltin BuiltinNat), []))))));
           arg_opt = None })
         ];
       c_ref = 0; c_result_type = 0; c_builtin = false })
    (:4:5-4:11,
     { c_id = (:4:5-4:6, (Name "b")); c_magic = 0xA4070ED3l; c_args = [];
       c_ref = 1; c_result_type = 1; c_builtin = false })
    (:10:5-10:16,
     { c_id = (:10:5-10:10, (NameNs ("ns", "d1"))); c_magic = 0x87AFAC9Cl;
       c_args = []; c_ref = 2; c_result_type = 2; c_builtin = false })
    (:11:5-11:16,
     { c_id = (:11:5-11:10, (NameNs ("ns", "d2"))); c_magic = 0x98328F88l;
       c_args = []; c_ref = 3; c_result_type = 3; c_builtin = false })
    (:12:5-12:19,
     { c_id = (:12:5-12:10, (NameNs ("ns1", "e"))); c_magic = 0x46CFF604l;
       c_args = []; c_ref = 4; c_result_type = 4; c_builtin = false })
    (:13:5-13:19,
     { c_id = (:13:5-13:10, (NameNs ("ns2", "e"))); c_magic = 0xDD6B2E5Cl;
       c_args = []; c_ref = 5; c_result_type = 5; c_builtin = false })
    (:14:5-14:15,
     { c_id = (:14:5-14:10, (NameNs ("ns3", "f"))); c_magic = 0x64929875l;
       c_args = []; c_ref = 6; c_result_type = 6; c_builtin = false })
    Functions:
    (:6:5-6:11,
     { f_id = (:6:5-6:6, (Name "b")); f_magic = 0x3D0E5F69l; f_args = [];
       f_result_type =
       (:6:9-6:10, (TypeExpr ((TypeRef (:6:9-6:10, (0, None))), [])));
       f_builtin = false })
    (:7:5-7:11,
     { f_id = (:7:5-7:6, (Name "c")); f_magic = 0x6E76D9l; f_args = [];
       f_result_type =
       (:7:9-7:10, (TypeExpr ((TypeRef (:7:9-7:10, (0, None))), [])));
       f_builtin = false }) |}]
