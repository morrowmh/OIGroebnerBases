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

VectorInWidth = new Type of BasicList

-- TODO: net VectorInWidth

-- Install the basis elements in a given width
installBasisElements = method();
installBasisElements(FreeOIModule, ZZ) := (F, n) -> (
    M := getModuleInWidth(F, n);

    for i to #F.genWidths - 1 do
        for oiMap in getOIMaps(F.genWidths#i, n) do (
            one := 1_(getAlgebraInWidth(F.polyOIAlg, oiMap.targWidth));
            v := new VectorInWidth from { {one, oiMap.targWidth, oiMap.img, i + 1} };
            F.basisSym_(oiMap.targWidth, oiMap.img, i + 1) <- new M from v
        )
)

-- Check if a VectorInWidth is zero
-- Args: v = VectorInWidth
isZero := v -> #v === 0

-- Should be a VectorInWidth with exactly one monomial
MonomialInWidth = new Type of VectorInWidth

-- Cache for storing ModuleInWidth comparisons
compCache = new MutableHashTable

-- Comparison method for MonomialInWidth
MonomialInWidth ? MonomialInWidth := (v, w) -> (
    -- Return the comparison if it already exists
    if compCache#?(v, w) then return compCache#(v, w);

    -- Generate the comparison
    clsv := class v;
    clsw := class w;
    fmodv := clsv.freeOIMod;
    fmodw := clsw.freeOIMod;

    local ret;
    if v === w then ret = symbol ==
    else if clsv === clsw and isZero v then ret = symbol <
    else if clsv === clsw and isZero w then ret = symbol >
    else if isZero v or isZero w or not fmodv === fmodw then ret = symbol incomparable
    else if fmodv.monOrder === Lex then ( -- Lex order
        ringEltv := v#0; ringEltw := w#0;
        targWidthv := v#1; targWidthw := w#1;
        imgv := v#2; imgw := w#2;
        idxv := v#3; idxw := w#3;

        if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not targWidthv === targWidthw then ret = targWidthv ? targWidthw
        else if not imgv === imgw then ret = imgv ? imgw
        else ret = ringEltv ? ringEltw
    )
    else if instance(fmodv.monOrder, List) then ( -- Schreyer order
        -- TODO: Add this
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(v, w) = ret
)

-- Addition method for VectorInWidth
VectorInWidth + VectorInWidth := (v, w) -> (
    if not class v === class w then error("cannot add " | net v | " and " | net w);
    if isZero v then return w else if isZero w then return v;

    -- TODO: Finish this
)