-- Compute the S-polynomial of two VectorInWidth objects
-- Args: v = VectorInWidth, w = VectorInWidth
-- Comment: expects class v === class w
SPolynomial := (v, w) -> (
    cls := class v;

    if isZero v or isZero w then return makeZero cls;

    ltv := leadTerm v;
    ltw := leadTerm w;
    ltvelt := ltv.vec#(ltv.cache);
    ltwelt := ltw.vec#(ltw.cache);
    lcmlmvw := lcm(leadMonomial ltvelt, leadMonomial ltwelt);

    (lcmlmvw // ltvelt) * v - (lcmlmvw // ltwelt) * w
)