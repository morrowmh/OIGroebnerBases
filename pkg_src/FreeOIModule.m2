-- Should be of the form {basisSym => Symbol, genWidths => List, degShifts => List, polyOIAlg => PolynomialOIAlgebra, monOrder => Thing, modules => MutableHashTable, maps => MutableHashTable, basisKeys => MutableHashTable}
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
    else if instance(opts.DegreeShifts, List) and #opts.DegreeShifts === #W then opts.DegreeShifts
    else error "invalid DegreeShifts option";

    -- Validate the monomial order
    if not opts.MonomialOrder === Lex and not (
        instance(opts.MonomialOrder, List) and 
        W === apply(opts.MonomialOrder, getWidth) and 
        #set apply(opts.MonomialOrder, getFreeOIModule) == 1) then error "invalid monomial order";

    new FreeOIModule from {
        basisSym => e,
        genWidths => W,
        degShifts => shifts,
        polyOIAlg => P,
        monOrder => opts.MonomialOrder,
        modules => new MutableHashTable,
        maps => new MutableHashTable,
        basisKeys => new MutableHashTable}
)

-- Check if a FreeOIModule is zero
isZero = method(TypicalValue => Boolean)
isZero FreeOIModule := F -> F.genWidths === {}

-- Should be of the form {wid => ZZ, rawMod => Module, freeOIMod => FreeOIModule}
ModuleInWidth = new Type of HashTable

net ModuleInWidth := M -> net M.rawMod | " in width " | net M.wid |
    if not set M.freeOIMod.degShifts === set {0} and not zero M.rawMod then ", degrees " | net flatten degrees M.rawMod else ""

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

use ModuleInWidth := M -> ( use getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid); M )

VectorInWidth = new Type of HashTable

-- Should be of the form {key => Sequence, vec => VectorInWidth}
KeyedVectorInWidth = new Type of HashTable

-- Get the width of a VectorInWidth
getWidth = method(TypicalValue => ZZ)
getWidth VectorInWidth := v -> (class v).wid

-- Get the FreeOIModule of a VectorInWidth
getFreeOIModule = method(TypicalValue => FreeOIModule)
getFreeOIModule VectorInWidth := v -> (class v).freeOIMod

-- Get the basis keys in a given width
-- Args: F = FreeOIModule, n = ZZ
getBasisKeys := (F, n) -> (
    -- Return the basis keys if they already exist
    if F.basisKeys#?n then return F.basisKeys#n;

    -- Store the basis keys
    F.basisKeys#n = flatten for i to #F.genWidths - 1 list
        for oiMap in getOIMaps(F.genWidths#i, n) list (oiMap, i + 1)
)

-- Make a VectorInWidth
-- Args: M = ModuleInWidth, A => List
makeVectorInWidth := (M, A) -> new M from new VectorInWidth from A

-- Make a VectorInWidth with a single basis key
-- Args: M = ModuleInWidth, key = Sequence, elt = RingElement
makeSingle := (M, key, elt) -> (
    A := for keyj in getBasisKeys(M.freeOIMod, M.wid) list keyj => if key === keyj then elt else 0_(class elt);
    makeVectorInWidth(M, A)
)

-- Make a KeyedVectorInWidth
-- Args: M = ModuleInWidth, key0 = Sequence, elt = RingElement
makeKeyedVectorInWidth := (M, key0, elt) -> new KeyedVectorInWidth from {key => key0, vec => makeSingle(M, key0, elt)}

-- Make the zero VectorInWidth
-- Args: M = ModuleInWidth
makeZero := M -> (
    A := for key in getBasisKeys(M.freeOIMod, M.wid) list key => 0_(getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid));
    makeVectorInWidth(M, A)
)

-- Install the basis elements in a given width
installBasisElements = method();
installBasisElements(FreeOIModule, ZZ) := (F, n) -> (
    M := getModuleInWidth(F, n);
    K := getBasisKeys(F, n);
    
    for key in K do F.basisSym_((key#0).targWidth, (key#0).img, key#1) <- makeSingle(M, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Check if a VectorInWidth is zero
isZero VectorInWidth := v -> (
    for val in values v do if not zero val then return false;
    true
)

-- Get the terms of a VectorInWidth
terms VectorInWidth := v -> flatten for key in keys v list
    for term in terms v#key list makeSingle(class v, key, term)

-- Get the keyed terms of a VectorInWidth
-- Args: v = VectorInWidth
keyedTerms := v -> flatten for key in keys v list
    for term in terms v#key list makeKeyedVectorInWidth(class v, key, term)

-- Get the keyed singles of a VectorInWidth
-- Args: v = VectorInWidth
keyedSingles := v -> flatten for key in keys v list
    if zero v#key then continue else makeKeyedVectorInWidth(class v, key, v#key)

-- Get the ith generator of a FreeOIModule
-- Args: F = FreeOIModule, i = ZZ
-- Comment: expects 0 <= i <= #F.genWidths - 1
getGenerator := (F, i) -> (
    n := F.genWidths#i;
    M := getModuleInWidth(F, n);
    key := (makeOIMap(n, toList(1..n)), i + 1);
    makeSingle(M, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Get the keyed lead term of a VectorInWidth
keyedLeadTerm := v -> (
    if isZero v then return new KeyedVectorInWidth from {key => null, vec => v};

    T := keyedTerms v;
    if #T === 1 then return T#0;

    largest := T#0;
    for term in T do if largest < term then largest = term;
    largest
)

-- Get the lead term of a VectorInWidth
leadTerm VectorInWidth := v -> (keyedLeadTerm v).vec

-- Get the keyed lead monomial of a VectorInWidth
keyedLeadMonomial := v -> (
    if isZero v then error "the zero element has no lead monomial";
    lt := keyedLeadTerm v;
    makeKeyedVectorInWidth(class v, lt.key, leadMonomial lt.vec#(lt.key))
)

-- Get the lead monomial of a VectorInWidth
leadMonomial VectorInWidth := v -> (keyedLeadMonomial v).vec

-- Get the lead coefficient of a VectorInWidth
leadCoefficient VectorInWidth := v -> (
    if isZero v then return 0_(getAlgebraInWidth((class v).freeOIMod.polyOIAlg, (class v).wid));
    lt := keyedLeadTerm v;
    leadCoefficient lt.vec#(lt.key)
)

-- Cache for storing KeyedVectorInWidth term comparisons
compCache = new MutableHashTable

-- Comparison method for KeyedVectorInWidth terms
KeyedVectorInWidth ? KeyedVectorInWidth := (v, w) -> (
    keyv := v.key;
    keyw := w.key;
    monv := leadMonomial(v.vec#keyv);
    monw := leadMonomial(w.vec#keyw);
    oiMapv := keyv#0;
    oiMapw := keyw#0;
    idxv := keyv#1;
    idxw := keyw#1;
    fmod := (class v.vec).freeOIMod;
    ord := fmod.monOrder;

    -- Return the comparison if it already exists
    if compCache#?(keyv, monv, keyw, monw, ord) then return compCache#(keyv, monv, keyw, monw, ord);

    -- Generate the comparison
    local ret;
    if v === w then ret = symbol ==
    else if ord === Lex then ( -- Lex order
        if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ret = oiMapv.targWidth ? oiMapw.targWidth
        else if not oiMapv.img === oiMapw.img then ret = oiMapv.img ? oiMapw.img
        else ret = monv ? monw
    )
    else if instance(ord, List) then ( -- Schreyer order
        fmodMap := new FreeOIModuleMap from {srcMod => fmod, targMod => getFreeOIModule ord#0, genImages => ord};
        lmimgv := keyedLeadMonomial fmodMap v.vec;
        lmimgw := keyedLeadMonomial fmodMap w.vec;

        if not lmimgv === lmimgw then ret = lmimgv ? lmimgw
        else if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ( if oiMapv.targWidth < oiMapw.targWidth then ret = symbol > else ret = symbol < )
        else if not oiMapv.img === oiMapw.img then ( if oiMapv.img < oiMapw.img then ret = symbol > else ret = symbol < )
        else ret = symbol ==
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(keyv, monv, keyw, monw, ord) = ret
)

-- Addition method for VectorInWidth
VectorInWidth + VectorInWidth := (v, w) -> (
    if not class v === class w then error("cannot add " | net v | " and " | net w);
    if isZero v then return w else if isZero w then return v;

    cls := class v;
    K := getBasisKeys(cls.freeOIMod, cls.wid);
    A := for key in K list key => v#key + w#key;

    makeVectorInWidth(cls, A)
)

-- Module multiplication method for VectorInWidth
RingElement * VectorInWidth := (r, v) -> (
    clsv := class v;
    fmod := clsv.freeOIMod;
    wid := clsv.wid;

    if not class r === getAlgebraInWidth(fmod.polyOIAlg, wid) then error("cannot multiply " | net r | " and " | net v);
    if isZero v then return v;

    K := getBasisKeys(fmod, wid);
    A := for key in K list key => r * v#key;

    makeVectorInWidth(clsv, A)
)

-- Number multiplication method for VectorInWidth
Number * VectorInWidth := (n, v) -> (
    cls := class v;
    n_(getAlgebraInWidth(cls.freeOIMod.polyOIAlg, cls.wid)) * v
)

-- Negative method for VectorInWidth
- VectorInWidth := v -> (-1) * v

-- Subtraction method for VectorInWidth
VectorInWidth - VectorInWidth := (v, w) -> v + -w

-- Get the degree of a VectorInWidth
degree VectorInWidth := v -> (
    if isZero v then return 0;

    lt := keyedLeadTerm v;
    elt := lt.vec#(lt.key);
    degElt := (degree elt)#0;

    basisIdx := lt.key#1;
    degBasisElt := -(class v).freeOIMod.degShifts#(basisIdx - 1);

    degElt + degBasisElt
)

-- Check if a VectorInWidth is homogeneous
isHomogeneous VectorInWidth := v -> (
    if isZero v then return true;

    #set apply(terms v, degree) === 1
)

-- Make a VectorInWidth monic
-- Args: v = VectorInWidth
-- Comment: assumes v is nonzero
makeMonic := v -> (1 / leadCoefficient v) * v

-- Display a VectorInWidth with terms in order
net VectorInWidth := v -> (
    if isZero v then return net 0;
    
    fmod := (class v).freeOIMod;
    sorted := reverse sort keyedTerms v;

    firstTerm := sorted#0;
    N := net firstTerm.vec#(firstTerm.key) | net fmod.basisSym_(toString (firstTerm.key#0).targWidth, toString (firstTerm.key#0).img, toString firstTerm.key#1);
    
    for i from 1 to #sorted - 1 do (
        term := sorted#i;
        elt := term.vec#(term.key);
        coeff := leadCoefficient elt;
        basisNet := net fmod.basisSym_(toString (term.key#0).targWidth, toString (term.key#0).img, toString term.key#1);

        N = N | if coeff > 0 then " + " | net elt | basisNet else " - " | net(-elt) | basisNet
    );

    N
)

-- Should be of the form {freeOIMod => FreeOIModule, oiMap => OIMap, img => HashTable}
InducedModuleMap = new Type of HashTable

-- Get the module map induced by an OI-map
-- Args: F = FreeOIModule, f = OIMap
getInducedModuleMap := (F, f) -> (
    -- Return the map if it already exists
    if F.maps#?(F, f) then return F.maps#(F, f);

    -- Generate the basis element assignments
    m := #f.img;
    K := getBasisKeys(F, m);
    H := hashTable for key in K list key => (f key#0, key#1);

    -- Store the map
    F.maps#(F, f) = new InducedModuleMap from {freeOIMod => F, oiMap => f, img => H}
)

-- Apply an InducedModuleMap to a VectorInWidth
-- Comment: expects v to belong to the domain of f
InducedModuleMap VectorInWidth := (f, v) -> (
    fmod := f.freeOIMod;
    targWidth := f.oiMap.targWidth;
    targMod := getModuleInWidth(fmod, targWidth);

    -- Handle the zero vector
    if isZero v then return makeZero targMod;

    algMap := getInducedAlgebraMap(fmod.polyOIAlg, f.oiMap);

    sum for single in keyedSingles v list makeSingle(targMod, f.img#(single.key), algMap single.vec#(single.key))
)

-- Should be of the form {srcMod => FreeOIModule, targMod => FreeOIModule, genImages => List}
FreeOIModuleMap = new Type of HashTable

net FreeOIModuleMap := f -> "Source: " | toString f.srcMod | " Target: " | toString f.targMod || "Generator images: " | net f.genImages

-- Check if a FreeOIModuleMap is zero
isZero FreeOIModuleMap := f -> isZero f.srcMod or isZero f.targMod or set apply(f.genImages, isZero) === set {true}

-- Apply a FreeOIModuleMap to a VectorInWidth
-- Comment: expects v to belong to the domain of f
FreeOIModuleMap VectorInWidth := (f, v) -> (
    -- Handle the zero vector or zero map
    if isZero f or isZero v then return makeZero getModuleInWidth(f.targMod, getWidth v);

    sum for single in keyedSingles v list (
        elt := single.vec#(single.key);
        oiMap := single.key#0;
        basisIdx := single.key#1;
        modMap := getInducedModuleMap(f.targMod, oiMap);
        elt * modMap f.genImages#(basisIdx - 1)
    )
)

-- Check if a FreeOIModuleMap is a graded map
isHomogeneous FreeOIModuleMap := f -> (
    if isZero f then return true;

    for elt in f.genImages do if not isHomogeneous elt then return false;

    -f.srcMod.degShifts === apply(f.genImages, degree)
)

-- Compute the n-orbit of a List of VectorInWidth objects
oiOrbit = method(TypicalValue => List)
oiOrbit(List, ZZ) := (L, n) -> (
    if n < 0 then error "expected a nonnegative integer";

    unique flatten for elt in L list for oimap in getOIMaps(getWidth elt, n) list (getInducedModuleMap(getFreeOIModule elt, oimap)) elt
)