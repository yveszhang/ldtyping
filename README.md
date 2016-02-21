LDTT type checking algorithm

LdTerm ::= <var> | <num> | Int | <term> * <term>
         | Pi (<var> : <term>) <term> 
         | lambda (<var> : <term>)  <term>
         | Pi (<var> :: <term>) <term>
         | lambda (<var> :: <term>) <term>
         | <term> <term>
         | Eq(<term>, <term>, <term>)
         | refl(<term>)
         | subst(var, <term>, <term>, <term>)

Preserved words:
    Int, Bool, True, False, if, then, else, Pi, lambda, Eq, refl, subst
    fst, snd, injfst, injsnd, case, of, in, fix

!!!Notice: The algorithm requires that all bound variables differ from each 
other and from free variables.
