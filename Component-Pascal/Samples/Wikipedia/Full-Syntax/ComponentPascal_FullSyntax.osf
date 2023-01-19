Module = MODULE ident ";" 
           [ImportList] DeclSeq 
           [BEGIN StatementSeq] 
           [CLOSE StatementSeq] 
         END ident ".".

ImportList = IMPORT [ident ":="] ident {"," [ident ":="] ident} ";".

DeclSeq = { CONST {ConstDecl ";" } 
          | TYPE {TypeDecl ";"} 
          | VAR {VarDecl ";"}} 
          { ProcDecl ";" | ForwardDecl ";"}.

ConstDecl = IdentDef "=" ConstExpr.

TypeDecl = IdentDef "=" Type.

VarDecl = IdentList ":" Type.

ProcDecl = PROCEDURE [Receiver] IdentDef [FormalPars] MethAttributes 
           [";" DeclSeq [BEGIN StatementSeq] 
           END ident].

MethAttributes = ["," NEW] ["," (ABSTRACT | EMPTY | EXTENSIBLE)].

ForwardDecl = PROCEDURE "^" [Receiver] IdentDef [FormalPars] MethAttributes.

FormalPars = "(" [FPSection {";" FPSection}] ")" [":" Type].

FPSection = [VAR | IN | OUT] ident {"," ident} ":" Type.

Receiver = "(" [VAR | IN] ident ":" ident ")".

Type = Qualident
    | ARRAY [ConstExpr {"," ConstExpr}] OF Type
    | [ABSTRACT | EXTENSIBLE | LIMITED] RECORD ["("Qualident")"] FieldList {";" FieldList} END
    | POINTER TO Type
    | PROCEDURE [FormalPars].

FieldList = [IdentList ":" Type].

StatementSeq = Statement {";" Statement}.

Statement = [ Designator ":=" Expr
    | Designator ["(" [ExprList] ")"]
    | IF Expr THEN StatementSeq
        {ELSIF Expr THEN StatementSeq}
        [ELSE StatementSeq] 
      END
    | CASE Expr OF 
        Case {"|" Case}
        [ELSE StatementSeq] 
      END
    | WHILE Expr DO StatementSeq END
    | REPEAT StatementSeq UNTIL Expr
    | FOR ident ":=" Expr TO Expr [BY ConstExpr] DO StatementSeq END
    | LOOP StatementSeq END
    | WITH [ Guard DO StatementSeq ] 
       {"|" [ Guard DO StatementSeq ] } 
       [ELSE StatementSeq] 
      END
    | EXIT
    | RETURN [Expr]
    ].

Case = [CaseLabels {"," CaseLabels} ":" StatementSeq].

CaseLabels = ConstExpr [".." ConstExpr].

Guard = Qualident ":" Qualident.

ConstExpr = Expr.

Expr = SimpleExpr [Relation SimpleExpr].

SimpleExpr = ["+" | "-"] Term {AddOp Term}.

Term = Factor {MulOp Factor}.

Factor = Designator | number | character | string | NIL | Set | "(" Expr ")" | " ~ " Factor.

Set = "{" [Element {"," Element}] "}".

Element = Expr [".." Expr].

Relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.

AddOp = "+" | "-" | OR.

MulOp = "*" | "/" | DIV | MOD | "&".

Designator = Qualident {"." ident 
             | "[" ExprList "]" 
             | "^" 
             | "(" Qualident ")" 
             | "(" [ExprList] ")"} [ "$" ].

ExprList = Expr {"," Expr}.

IdentList = IdentDef {"," IdentDef}.

Qualident = [ident "."] ident.

IdentDef = ident ["*" | "-"].

