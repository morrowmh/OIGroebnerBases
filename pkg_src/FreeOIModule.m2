-- Should be of the form {basisSym => Symbol, genWidths => List, degShifts => List, polyOIAlg => PolynomialOIAlgebra, monOrder => Thing, modules => MutableHashTable, maps => MutableHashTable}
FreeOIModule = new Type of HashTable

toString FreeOIModule := F -> "(" | toString F.basisSym | ", " | toString F.genWidths | ", " | toString F.degShifts | ")"

net FreeOIModule := F -> (
    monOrderNet := if F.monOrder === Lex then net Lex
    else if instance(F.monOrder, List) then "Schreyer"
    else error "invalid monomial order";

    "Basis symbol: " | net F.basisSym ||
    "Generator widths: " | net F.genWidths ||
    "Degree shifts: " | net F.degShifts ||
    "Polynomial OI-algebra: " | toString F.polyOIAlg ||
    "Monomial order: " | monOrderNet
)

makeFreeOIModule = method(TypicalValue => FreeOIModule, Options => {DegreeShifts => null, MonomialOrder => Lex})
makeFreeOIModule(Symbol, List, PolynomialOIAlgebra) := opts -> (e, W, P) -> (
    shifts := if opts.DegreeShifts === null then toList(#W : 0)
    else if instance(opts.DegreeShifts, List) then opts.DegreeShifts
    else error "invalid DegreeShifts option";

    -- Validate the monomial order
    -- if not opts.MonomialOrder === Lex and not (
    -- instance(opts.MonomialOrder, List) and 
    -- W === apply(opts.MonomialOrder, widthOfElement) and 
    --  #set apply(opts.MonomialOrder, freeOIModuleFromElement) == 1) then error "invalid monomial order";

    new FreeOIModule from {
        basisSym => e,
        genWidths => W,
        degShifts => shifts,
        polyOIAlg => P,
        monOrder => opts.MonomialOrder,
        modules => new MutableHashTable,
        maps => new MutableHashTable}
)

-- Should be of the form {wid => ZZ, rawMod => Module, freeOIMod => FreeOIModule}
ModuleInWidth = new Type of HashTable

net ModuleInWidth := M -> net M.rawMod | " in width " | net M.wid

-- Get the module of F in width n
-- Args: F = FreeOIModule, n = ZZ
getModuleInWidth := (F, n) -> (
    -- Return the module if it already exists
    if F.modules#?n then return F.modules#n;

    -- Generate the degrees
    alg := getAlgebraInWidth(F.polyOIAlg, n);
    degList := for i to #F.genWidths - 1 list binomial(n, F.genWidths#i) : F.degShifts#i;

    -- Generate and store the module
    F.modules#n = new ModuleInWidth of VectorInWidth from hashTable {
        wid => n,
        rawMod => alg^degList,
        freeOIMod => F
    }
)

FreeOIModule _ ZZ := (F, n) -> getModuleInWidth(F, n)

use ModuleInWidth := M -> (use getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid); M)

VectorInWidth = new Type of HashTable

-- TODO: net VectorInWidth

-- Make a VectorInWidth term
-- Args: M = ModuleInWidth, K = List, key = Sequence, elt = RingElement
makeTerm := (M, K, key, elt) -> (
    assignments := for keyj in K list
        keyj => if key === keyj then elt
        else 0_(class elt);
    
    new M from new VectorInWidth from assignments
)

-- Get the basis element keys in a given width
-- Args: F = FreeOIModule, n = ZZ
getBasisKeys := (F, n) -> flatten for i to #F.genWidths - 1 list
    for oiMap in getOIMaps(F.genWidths#i, n) list (oiMap.targWidth, oiMap.img, i + 1)

-- Install the basis elements in a given width
installBasisElements = method();
installBasisElements(FreeOIModule, ZZ) := (F, n) -> (
    M := getModuleInWidth(F, n);
    K := getBasisKeys(F, n);
    
    for key in K do F.basisSym_key <- makeTerm(M, K, key, 1_(getAlgebraInWidth(F.polyOIAlg, key#0)))
)

-- Check if a VectorInWidth is zero
-- Args: v = VectorInWidth
isZero := v -> (
    ze := true;
    for val in values v do if not zero val then ( ze = false; break );
    ze
)

-- Cache for storing VectorInWidth term comparisons
compCache = new MutableHashTable

-- Comparison method for VectorInWidth terms
-- Args: keyv = Sequence, keyw = Sequence, v = VectorInWidth, w = VectorInWidth, eltv = RingElement, eltw = RingElement
compareTerms := (keyv, keyw, v, w, eltv, eltw) -> (
    -- Return the comparison if it already exists
    if compCache#?(keyv, keyw, v, w, eltv, eltw) then return compCache#(keyv, keyw, v, w, eltv, eltw);

    -- Generate the comparison
    targWidthv := keyv#0;
    targWidthw := keyw#0;
    imgv := keyv#1;
    imgw := keyw#1;
    idxv := keyv#2;
    idxw := keyw#2;
    clsv := class v;
    clsw := class w;
    fmodv := clsv.freeOIMod;
    fmodw := clsw.freeOIMod;

    local ret;
    if clsv === clsw and keyv === keyw and eltv === eltw then ret = symbol ==
    else if not fmodv === fmodw then ret = symbol incomparable
    else if fmodv.monOrder === Lex then ( -- Lex order
        if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not targWidthv === targWidthw then ret = targWidthv ? targWidthw
        else if not imgv === imgw then ret = imgv ? imgw
        else ret = eltv ? eltw
    )
    else if instance(fmodv.monOrder, List) then ( -- Schreyer order
        -- TODO: Add this
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(keyv, keyw, v, w, eltv, eltw) = ret
)

-- Get the terms of a VectorInWidth
terms VectorInWidth := v -> flatten for key in keys v list
    for term in terms v#key list makeTerm(class v, keys v, key, term)

-- Helper method for leadTerm, leadMonomial, and leadCoefficient
-- Args: v = VectorInWidth
largest := v -> (
    K := keys v;
    larg := if zero v#(K#0) then {K#0, v#(K#0)} else {K#0, (terms v#(K#0))#0};
    for key in K do
        for term in terms v#key do
            if compareTerms(larg#0, key, v, v, larg#1, term) === symbol < then
                larg = {key, term};
    
    larg
)

-- Get the lead term of a VectorInWidth
leadTerm VectorInWidth := v -> (
    larg := largest v;
    makeTerm(class v, keys v, larg#0, larg#1)
)

-- Get the lead coefficient of a VectorInWidth
leadCoefficient VectorInWidth := v -> (
    larg := largest v;
    leadCoefficient larg#1
)

-- Get the lead monomial of a VectorInWidth
leadMonomial VectorInWidth := v -> (
    if isZero v then error "the zero element has no lead monomial";
    larg := largest v;
    cls := class v;
    makeTerm(cls, keys v, larg#0, leadMonomial larg#1)
)

-- Addition method for VectorInWidth
VectorInWidth + VectorInWidth := (v, w) -> (
    if not class v === class w then error("cannot add " | net v | " and " | net w);
    if isZero v then return w else if isZero w then return v;

    cls := class v;
    K := getBasisKeys(cls.freeOIMod, cls.wid);
    assignments := for key in K list key => v#key + w#key;

    new cls from new VectorInWidth from assignments
)

-- Module multiplication method for VectorInWidth
RingElement * VectorInWidth := (r, v) -> (
    clsv := class v;
    fmod := clsv.freeOIMod;
    wid := clsv.wid;

    if not class r === getAlgebraInWidth(fmod.polyOIAlg, wid) then error("cannot multiply " | net r | " and " | net v);
    if isZero v then return v;

    K := getBasisKeys(fmod, wid);
    assignments := for key in K list key => r * v#key;

    new clsv from new VectorInWidth from assignments
)

-- Number multiplication method for VectorInWidth
Number * VectorInWidth := (n, v) -> (
    cls := class v;
    n_(getAlgebraInWidth(cls.freeOIMod.polyOIAlg, cls.wid)) * v
)

-- Subtraction method for VectorInWidth
VectorInWidth - VectorInWidth := (v, w) -> v + (-1)*w