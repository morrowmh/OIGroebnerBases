-- Compute the S-polynomial of two VectorInWidth objects
-- Args: v = VectorInWidth, w = VectorInWidth
-- Comment: expects class v === class w
SPolynomial := (v, w) -> (
    cls := class v;

    if isZero v or isZero w then return makeZero cls;

    ltv := keyedLeadTerm v;
    ltw := keyedLeadTerm w;
    ltvelt := ltv.vec#(ltv.key);
    ltwelt := ltw.vec#(ltw.key);
    lcmlmvw := lcm(leadMonomial ltvelt, leadMonomial ltwelt);

    (lcmlmvw // ltvelt) * v - (lcmlmvw // ltwelt) * w
)