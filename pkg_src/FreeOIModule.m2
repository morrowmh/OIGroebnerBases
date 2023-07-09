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

use ModuleInWidth := M -> ( use getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid); M )

-- Should be of the form {cache => Thing, vec => HashTable}
VectorInWidth = new Type of HashTable

-- Get the basis element keys in a given width
-- Args: F = FreeOIModule, n = ZZ
getBasisKeys := (F, n) -> flatten for i to #F.genWidths - 1 list
    for oiMap in getOIMaps(F.genWidths#i, n) list (oiMap, i + 1)

-- Make a VectorInWidth
-- Args: M = ModuleInWidth, c = Thing, a = List
makeVectorInWidth := (M, c, a) -> new M from new VectorInWidth from {cache => c, vec => hashTable a}

-- Make a VectorInWidth with a single basis element
-- Args: M = ModuleInWidth, K = List, key = Sequence, elt = RingElement
-- Comment: sets cache => key
makeSingle := (M, K, key, elt) -> (
    assignments := for keyj in K list
        keyj => if key === keyj then elt else 0_(class elt);
    
    makeVectorInWidth(M, key, assignments)
)

-- Make the zero VectorInWidth
-- Args: M = ModuleInWidth
makeZero := M -> (
    K := getBasisKeys(M.freeOIMod, M.wid);
    assignments := for key in K list key => 0_(getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid));

    makeVectorInWidth(M, null, assignments)
)

-- Install the basis elements in a given width
installBasisElements = method();
installBasisElements(FreeOIModule, ZZ) := (F, n) -> (
    M := getModuleInWidth(F, n);
    K := getBasisKeys(F, n);
    
    for key in K do F.basisSym_((key#0).targWidth, (key#0).img, key#1) <- makeSingle(M, K, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Check if a VectorInWidth is zero
-- Args: v = VectorInWidth
isZero := v -> (
    ret := true;
    for val in values v.vec do if not zero val then ( ret = false; break );
    ret
)

-- Get the degree of a VectorInWidth
-- Args: v = VectorInWidth
degree VectorInWidth := v -> (
    -- TODO: finish this
)

-- Get the terms of a VectorInWidth
terms VectorInWidth := v -> flatten for key in keys v.vec list
    for term in terms v.vec#key list makeSingle(class v, keys v.vec, key, term)

-- Get the combined terms of a VectorInWidth
combinedTerms := v -> for key in keys v.vec list (
    if zero v.vec#key then continue
    else makeSingle(class v, keys v.vec, key, v.vec#key)
)

-- Cache for storing VectorInWidth term comparisons
compCache = new MutableHashTable

-- Comparison function for VectorInWidth terms
-- Args: elt0 = RingElement, key0 = Sequence, elt1 = RingElement, key1 = Sequence, v = VectorInWidth
-- Comment: compares elt0 on key0 to elt1 on key1 in v
compareTerms := (elt0, key0, elt1, key1, v) -> (
    -- Return the comparison if it already exists
    if compCache#?(elt0, key0, elt1, key1, v) then return compCache#(elt0, key0, elt1, key1, v);

    -- Generate the comparison
    oiMap0 := key0#0;
    oiMap1 := key1#0;
    img0 := oiMap0.img;
    img1 := oiMap1.img;
    idx0 := key0#1;
    idx1 := key1#1;
    fmod := (class v).freeOIMod;

    local ret;
    if key0 === key1 and elt0 === elt1 then ret = symbol ==
    else if fmod.monOrder === Lex then ( -- Lex order
        if not idx0 === idx1 then ( if idx0 < idx1 then ret = symbol > else ret = symbol < )
        else if not img0 === img1 then ret = img0 ? img1
        else ret = elt0 ? elt1
    )
    else if instance(fmod.monOrder, List) then ( -- Schreyer order
        -- TODO: Add this
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(elt0, key0, elt1, key1, v) = ret
)

-- Get the largest basis key and term in a VectorInWidth
-- Args: v = VectorInWidth
-- Comment: helper function for leadTerm, leadMonomial, and leadCoefficient
largest := v -> (
    K := keys v.vec;
    larg := if zero v.vec#(K#0) then (K#0, v.vec#(K#0)) else (K#0, (terms v.vec#(K#0))#0);
    for key in K do
        for term in terms v.vec#key do
            if zero larg#1 or compareTerms(larg#1, larg#0, term, key, v) === symbol < then
                larg = (key, term);
    
    larg
)

-- Get the lead term of a VectorInWidth
leadTerm VectorInWidth := v -> (
    larg := largest v;
    makeSingle(class v, keys v.vec, larg#0, larg#1)
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
    makeSingle(class v, keys v.vec, larg#0, leadMonomial larg#1)
)

-- Addition method for VectorInWidth
VectorInWidth + VectorInWidth := (v, w) -> (
    if not class v === class w then error("cannot add " | net v | " and " | net w);
    if isZero v then return w else if isZero w then return v;

    cls := class v;
    K := getBasisKeys(cls.freeOIMod, cls.wid);
    assignments := for key in K list key => v.vec#key + w.vec#key;

    makeVectorInWidth(cls, null, assignments)
)

-- Module multiplication method for VectorInWidth
RingElement * VectorInWidth := (r, v) -> (
    clsv := class v;
    fmod := clsv.freeOIMod;
    wid := clsv.wid;

    if not class r === getAlgebraInWidth(fmod.polyOIAlg, wid) then error("cannot multiply " | net r | " and " | net v);
    if isZero v then return v;

    K := getBasisKeys(fmod, wid);
    assignments := for key in K list key => r * v.vec#key;

    makeVectorInWidth(clsv, null, assignments)
)

-- Number multiplication method for VectorInWidth
Number * VectorInWidth := (n, v) -> (
    cls := class v;
    n_(getAlgebraInWidth(cls.freeOIMod.polyOIAlg, cls.wid)) * v
)

-- Subtraction method for VectorInWidth
VectorInWidth - VectorInWidth := (v, w) -> v + (-1) * w

-- Net for a VectorInWidth with a single basis key
-- Args: v = VectorInWidth
-- Comment: expects v to have cache => key
singleNet := v -> net v.vec#(v.cache) | net (class v).freeOIMod.basisSym_(toString (v.cache#0).targWidth, toString (v.cache#0).img, toString v.cache#1)

-- Should be of the form {viw => VectorInWidth, term => VectorInWidth}
VIWTerm = new Type of HashTable

-- Comparison method for VIWTerm objects
-- Comment: expects V.viw === W.viw
VIWTerm ? VIWTerm := (V, W) -> compareTerms(V.term.vec#(V.term.cache), V.term.cache, W.term.vec#(W.term.cache), W.term.cache, V.viw)

net VectorInWidth := v -> (
    if isZero v then return net 0;
    termsv := terms v;
    if #termsv === 1 then return singleNet termsv#0;

    viwterms := for term0 in termsv list new VIWTerm from {viw => v, term => term0};
    sorted := reverse sort viwterms;

    N := net "";
    for i to #sorted - 2 do N = N | singleNet (sorted#i).term | " + ";
    N | singleNet (sorted#-1).term
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
    F.maps#(F, f) = new InducedModuleMap from hashTable {freeOIMod => F, oiMap => f, img => H}
)

-- Apply an InducedModuleMap to a VectorInWidth
-- Args: f = InducedModuleMap, v = VectorInWidth
-- Comment: expects v to belong to the domain of f
InducedModuleMap VectorInWidth := (f, v) -> (
    fmod := f.freeOIMod;
    targWidth := f.oiMap.targWidth;
    targMod := getModuleInWidth(fmod, targWidth);

    -- Handle the zero vector
    if isZero v then return makeZero targMod;

    algMap := getInducedAlgebraMap(fmod.polyOIAlg, f.oiMap);
    targKeys := getBasisKeys(fmod, targWidth);

    sum for term in combinedTerms v list makeSingle(targMod, targKeys, f.img#(term.cache), algMap term.vec#(term.cache))
)