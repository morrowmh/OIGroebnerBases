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
        maps => new MutableHashTable,
        basisKeys => new MutableHashTable}
)

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

-- Should be of the form {cache => Thing, vec => HashTable}
VectorInWidth = new Type of HashTable

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
-- Args: M = ModuleInWidth, c = Thing, a = List
makeVectorInWidth := (M, c, a) -> new M from new VectorInWidth from {cache => c, vec => hashTable a}

-- Make a VectorInWidth with a single basis key
-- Args: M = ModuleInWidth, key = Sequence, elt = RingElement
-- Comment: sets cache => key
makeSingle := (M, key, elt) -> (
    assignments := for keyj in getBasisKeys(M.freeOIMod, M.wid) list keyj => if key === keyj then elt else 0_(class elt);
    makeVectorInWidth(M, key, assignments)
)

-- Make the zero VectorInWidth
-- Args: M = ModuleInWidth
makeZero := M -> (
    assignments := for key in getBasisKeys(M.freeOIMod, M.wid) list key => 0_(getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.wid));
    makeVectorInWidth(M, null, assignments)
)

-- Install the basis elements in a given width
installBasisElements = method();
installBasisElements(FreeOIModule, ZZ) := (F, n) -> (
    M := getModuleInWidth(F, n);
    K := getBasisKeys(F, n);
    
    for key in K do F.basisSym_((key#0).targWidth, (key#0).img, key#1) <- makeSingle(M, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Check if a VectorInWidth is zero
-- Args: v = VectorInWidth
isZero := v -> (
    ret := true;
    for val in values v.vec do if not zero val then ( ret = false; break );
    ret
)

-- Get the terms of a VectorInWidth
terms VectorInWidth := v -> flatten for key in keys v.vec list
    for term in terms v.vec#key list makeSingle(class v, key, term)

-- Cache for storing VectorInWidth term comparisons
compCache = new MutableHashTable

-- Comparison method for VectorInWidth terms
-- Args: v = VectorInWidth, w = VectorInWidth
-- Comment: expects v and w to have cache => key
compareTerms := (v, w) -> (
    -- Return the comparison if it already exists
    if compCache#?(v, w) then return compCache#(v, w);

    -- Generate the comparison
    keyv := v.cache;
    keyw := w.cache;
    oiMapv := keyv#0;
    oiMapw := keyw#0;
    idxv := keyv#1;
    idxw := keyw#1;
    eltv := v.vec#keyv;
    eltw := w.vec#keyw;

    local ret;
    if keyv === keyw and eltv === eltw then ret = symbol ==
    else if (class v).freeOIMod.monOrder === Lex then ( -- Lex order
        if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ret = oiMapv.targWidth ? oiMapw.targWidth
        else if not oiMapv.img === oiMapw.img then ret = oiMapv.img ? oiMapw.img
        else ret = eltv ? eltw
    )
    else if instance((class v).freeOIMod.monOrder, List) then ( -- Schreyer order
        -- TODO: Finish this
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(v, w) = ret
)

-- Get the lead term of a VectorInWidth
leadTerm VectorInWidth := v -> (
    if isZero v then return v;

    T := terms v;
    if #T === 1 then return T#0;

    largest := T#0;
    for term in T do if compareTerms(largest, term) === symbol < then largest = term;
    largest
)

-- Get the lead monomial of a VectorInWidth
leadMonomial VectorInWidth := v -> (
    if isZero v then error "the zero element has no lead monomial";
    lt := leadTerm v;
    makeSingle(class v, lt.cache, leadMonomial lt.vec#(lt.cache))
)

-- Get the lead coefficient of a VectorInWidth
leadCoefficient VectorInWidth := v -> (
    if isZero v then return 0_(getAlgebraInWidth((class v).freeOIMod.polyOIAlg, (class v).wid));
    lt := leadTerm v;
    leadCoefficient lt.vec#(lt.cache)
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

-- Negative method for VectorInWidth
- VectorInWidth := v -> (-1) * v

-- Subtraction method for VectorInWidth
VectorInWidth - VectorInWidth := (v, w) -> v + -w

-- Get the degree of a VectorInWidth
degree VectorInWidth := v -> (
    if isZero v then return 0;

    lt := leadTerm v;
    elt := lt.vec#(lt.cache);
    degElt := (degree elt)#0;

    basisIdx := lt.cache#1;
    degBasisElt := -(class v).freeOIMod.degShifts#(basisIdx - 1);

    degElt + degBasisElt
)

-- Check if a VectorInWidth is homogeneous
isHomogeneous VectorInWidth := v -> (
    if isZero v then return true;

    #set apply(terms v, degree) === 1
)

-- Helper type for net VectorInWidth
-- Comment: should be a List with one element, namely a VectorInWidth
TermInWidth = new Type of List

-- Comparison function for TermInWidth objects
TermInWidth ? TermInWidth := (v, w) -> compareTerms(v#0, w#0)

net VectorInWidth := v -> (
    if isZero v then return net 0;
    
    fmod := (class v).freeOIMod;
    sorted := flatten reverse sort for term in terms v list new TermInWidth from {term};

    firstTerm := sorted#0;
    N := net firstTerm.vec#(firstTerm.cache) | net fmod.basisSym_(toString (firstTerm.cache#0).targWidth, toString (firstTerm.cache#0).img, toString firstTerm.cache#1);

    for i from 1 to #sorted - 1 do (
        term := sorted#i;
        elt := term.vec#(term.cache);
        coeff := leadCoefficient elt;
        basisNet := net fmod.basisSym_(toString (term.cache#0).targWidth, toString (term.cache#0).img, toString term.cache#1);

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
-- Args: f = InducedModuleMap, v = VectorInWidth
-- Comment: expects v to belong to the domain of f
InducedModuleMap VectorInWidth := (f, v) -> (
    fmod := f.freeOIMod;
    targWidth := f.oiMap.targWidth;
    targMod := getModuleInWidth(fmod, targWidth);

    -- Handle the zero vector
    if isZero v then return makeZero targMod;

    algMap := getInducedAlgebraMap(fmod.polyOIAlg, f.oiMap);

    sum for term in terms v list makeSingle(targMod, f.img#(term.cache), algMap term.vec#(term.cache))
)