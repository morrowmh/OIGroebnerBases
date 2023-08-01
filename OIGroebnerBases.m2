-- -*- coding: utf-8 -*-

-*
Copyright 2023 Michael Morrow

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-

newPackage("OIGroebnerBases",
    Headline => "OI-modules over Noetherian polynomial OI-algebras",
    Version => "1.0.0",
    Date => "TBD",
    Keywords => { "Commutative Algebra" },
    Authors => {
        { Name => "Michael Morrow", HomePage => "https://michaelhmorrow.com", Email => "michaelhmorrow98@gmail.com" }
    },
    DebuggingMode => true,
    HomePage => "https://github.com/morrowmh/OIGroebnerBases"
)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- EXPORT AND PROTECT ----------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

export {
    -- Types
        -- From PolynomialOIAlgebra.m2
        "PolynomialOIAlgebra",

        -- From FreeOIModule.m2
        "ModuleInWidth", "VectorInWidth", "FreeOIModuleMap",

        -- From OIResolution.m2
        "OIResolution",
    
    -- Keys
        -- From PolynomialOIAlgebra.m2
        "ColUpRowUp", "ColUpRowDown", "ColDownRowUp", "ColDownRowDown",
        "RowUpColUp", "RowUpColDown", "RowDownColUp", "RowDownColDown",
    
    -- Methods
        -- From PolynomialOIAlgebra.m2
        "makePolynomialOIAlgebra",

        -- From FreeOIModule.m2
        "makeFreeOIModule", "isZero", "installBasisElements", "getWidth", "getFreeOIModule",

        -- From OIGB.m2
        "oiGB", "minimizeOIGB", "isOIGB",
    
        -- From oiSyz.m2
        "oiSyz",

        -- From OIResolution.m2
        "oiRes", "isComplex",
    
    -- Options
        -- From PolynomialOIAlgebra.m2
        "VariableOrder",

        -- From FreeOIModule.m2
        "DegreeShifts",

        -- From OIGB.m2
        "CacheSPolynomials", "MinimizeOIGB"
}

scan({
    -- Keys
        -- From OIMap.2
        targWidth, img,

        -- From PolynomialOIAlgebra.m2
        varRows, varSym, baseField, varOrder, algebras, maps,

        -- From FreeOIModule.m2
        basisSym, genWidths, degShifts, polyOIAlg, monOrder, modules, basisKeys, wid, rawMod, freeOIMod, vec, oiMap, srcMod, targMod, genImages,

        -- From Division.m2
        quo, rem, divTuples,

        -- From OIPair.m2
        map0, idx0, im0, map1, idx1, im1
}, protect)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- BODY ------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Should be of the form {targWidth => ZZ, img => List}
OIMap = new Type of HashTable

net OIMap := f -> "Source: [" | net(#f.img) | "] Target: [" | net f.targWidth | "]" || "Image: " | net f.img

-- Evaluate an OI-map f at an integer n
OIMap ZZ := (f, n) -> f.img#(n - 1)

-- Make a new OIMap
-- Args: n = ZZ, L = List
makeOIMap := (n, L) -> new OIMap from {targWidth => n, img => L}

-- Get the OI-maps between two widths
-- Args: m = ZZ, n = ZZ
getOIMaps := (m, n) -> (
    if n < m then return {};

    sets := subsets(1..n, m);
    for i to #sets - 1 list makeOIMap(n, sets#i)
)

-- Compose two OI-maps
OIMap OIMap := (f, g) -> makeOIMap(f.targWidth, for i from 1 to #g.img list f g i)

-- Should be of the form {varRows => ZZ, varSym => Symbol, baseField => Ring, varOrder => Symbol, algebras => MutableHashTable, maps => MutableHashTable}
PolynomialOIAlgebra = new Type of HashTable

toString PolynomialOIAlgebra := P -> "(" | toString P.varRows | ", " | toString P.varSym | ", " | toString P.baseField | ", " | toString P.varOrder | ")"

net PolynomialOIAlgebra := P -> "Number of variable rows: " | net P.varRows ||
    "Variable symbol: " | net P.varSym ||
    "Base field: " | net P.baseField ||
    "Variable order: " | net P.varOrder

makePolynomialOIAlgebra = method(TypicalValue => PolynomialOIAlgebra, Options => {VariableOrder => RowUpColUp})
makePolynomialOIAlgebra(ZZ, Symbol, Ring) := opts -> (c, x, K) -> (
    if c < 1 then error "expected at least one row of variables";

    v := opts.VariableOrder;
    if not member(v, {
        ColUpRowUp, ColUpRowDown, ColDownRowUp, ColDownRowDown,
        RowUpColUp, RowUpColDown, RowDownColUp, RowDownColDown
    }) then error "invalid variable order";

    new PolynomialOIAlgebra from {
            varRows => c,
            varSym => x,
            baseField => K,
            varOrder => v,
            algebras => new MutableHashTable,
            maps => new MutableHashTable}
)

-- Lookup table for linearFromRowCol
orderTable := new HashTable from {
    ColUpRowUp => (P, n, i, j) -> P.varRows * (n - j + 1) - i,      -- x_(i',j') < x_(i,j) if j'<j or j'=j and i'<i
    ColUpRowDown => (P, n, i, j) -> P.varRows * (n - j) + i - 1,    -- x_(i',j') < x_(i,j) if j'<j or j'=j and i'>i
    ColDownRowUp => (P, n, i, j) -> P.varRows * j - i,              -- x_(i',j') < x_(i,j) if j'>j or j'=j and i'<i
    ColDownRowDown => (P, n, i, j) -> P.varRows * (j - 1) + i - 1,  -- x_(i',j') < x_(i,j) if j'>j or j'=j and i'>i
    RowUpColUp => (P, n, i, j) -> n * (P.varRows - i + 1) - j,      -- x_(i',j') < x_(i,j) if i'<i or i'=i and j'<j
    RowUpColDown => (P, n, i, j) -> n * (P.varRows - i) + j - 1,    -- x_(i',j') < x_(i,j) if i'<i or i'=i and j'>j
    RowDownColUp => (P, n, i, j) -> n * i - j,                      -- x_(i',j') < x_(i,j) if i'>i or i'=i and j'<j
    RowDownColDown => (P, n, i, j) -> n * (i - 1) + j - 1           -- x_(i',j') < x_(i,j) if i'>i or i'=i and j'>j
}

-- Linearize the variables based on P.varOrder
-- Args: P = PolynomialOIAlgebra, n = ZZ, i = ZZ, j = ZZ
linearFromRowCol := (P, n, i, j) -> (orderTable#(P.varOrder))(P, n, i, j)

-- Get the algebra of P in width n
-- Args: P = PolynomialOIAlgebra, n = ZZ
getAlgebraInWidth := (P, n) -> (
    -- Return the algebra if it already exists
    if P.algebras#?n then return P.algebras#n;

    -- Generate the variables
    local ret;
    variables := new MutableList;
    for j from 1 to n do
        for i from 1 to P.varRows do variables#(linearFromRowCol(P, n, i, j)) = P.varSym_(i, j);

    -- Make the algebra
    ret = P.baseField[toList variables, Degrees => {#variables:1}, MonomialOrder => {Lex}];

    -- Store the algebra
    P.algebras#n = ret
)

PolynomialOIAlgebra _ ZZ := (P, n) -> getAlgebraInWidth(P, n)

-- Get the algebra map induced by an OI-map
-- Args: P = PolynomialOIAlgebra, f = OIMap
getInducedAlgebraMap := (P, f) -> (
    -- Return the map if it already exists
    if P.maps#?f then return P.maps#f;

    -- Generate the assignments
    m := #f.img;
    n := f.targWidth;
    src := P_m;
    targ := P_n;
    subs := flatten for j from 1 to m list
        for i from 1 to P.varRows list src_(linearFromRowCol(P, m, i, j)) => targ_(linearFromRowCol(P, n, i, f j)); -- Permute the second index

    -- Make the map
    ret := map(targ, src, subs);

    -- Store the map
    P.maps#f = ret
)

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

-- Should be of the form {cache => Thing, vec => HashTable}
VectorInWidth = new Type of HashTable

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
isZero VectorInWidth := v -> (
    ret := true;
    for val in values v.vec do if not zero val then ( ret = false; break );
    ret
)

-- Get the terms of a VectorInWidth
terms VectorInWidth := v -> flatten for key in keys v.vec list
    for term in terms v.vec#key list makeSingle(class v, key, term)

-- Get the combined terms of a VectorInWidth
-- Args: v = VectorInWidth
getSingles := v -> flatten for key in keys v.vec list
    if zero v.vec#key then continue else makeSingle(class v, key, v.vec#key)

-- Get the ith generator of a FreeOIModule
-- Args: F = FreeOIModule, i = ZZ
-- Comment: expects 0 <= i <= #F.genWidths - 1
getGenerator := (F, i) -> (
    n := F.genWidths#i;
    M := getModuleInWidth(F, n);
    key := (makeOIMap(n, toList(1..n)), i + 1);
    makeSingle(M, key, 1_(getAlgebraInWidth(F.polyOIAlg, n)))
)

-- Cache for storing VectorInWidth term comparisons
compCache = new MutableHashTable

-- Comparison method for VectorInWidth terms
-- Args: v = VectorInWidth, w = VectorInWidth
-- Comment: expects v and w to have cache => key
compareTerms := (v, w) -> (
    -- Return the comparison if it already exists
    if compCache#?(hash v, hash w) then return compCache#(hash v, hash w);

    -- Generate the comparison
    keyv := v.cache;
    keyw := w.cache;
    oiMapv := keyv#0;
    oiMapw := keyw#0;
    idxv := keyv#1;
    idxw := keyw#1;
    eltv := v.vec#keyv;
    eltw := w.vec#keyw;
    fmod := (class v).freeOIMod;
    ord := fmod.monOrder;

    local ret;
    if v === w then ret = symbol ==
    else if ord === Lex then ( -- Lex order
        if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ret = oiMapv.targWidth ? oiMapw.targWidth
        else if not oiMapv.img === oiMapw.img then ret = oiMapv.img ? oiMapw.img
        else ret = eltv ? eltw
    )
    else if instance(ord, List) then ( -- Schreyer order
        fmodMap := new FreeOIModuleMap from {srcMod => fmod, targMod => getFreeOIModule ord#0, genImages => ord};
        lmimgv := leadMonomial fmodMap v;
        lmimgw := leadMonomial fmodMap w;

        if not lmimgv === lmimgw then ret = compareTerms(lmimgv, lmimgw)
        else if not idxv === idxw then ( if idxv < idxw then ret = symbol > else ret = symbol < )
        else if not oiMapv.targWidth === oiMapw.targWidth then ( if oiMapv.targWidth < oiMapw.targWidth then ret = symbol > else ret = symbol < )
        else if not oiMapv.img === oiMapw.img then ( if oiMapv.img < oiMapw.img then ret = symbol > else ret = symbol < )
        else ret = symbol ==
    )
    else error "invalid monomial order";

    -- Store the comparison
    compCache#(hash v, hash w) = ret
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

-- Make a VectorInWidth monic
-- Args: v = VectorInWidth
-- Comment: assumes v is nonzero
makeMonic := v -> (1 / leadCoefficient v) * v

-- Helper type for net VectorInWidth
-- Comment: should be a List with one element, namely a VectorInWidth
TermInWidth = new Type of List

-- Comparison function for TermInWidth objects
TermInWidth ? TermInWidth := (v, w) -> compareTerms(v#0, w#0)

-- Display a VectorInWidth with terms in order
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
-- Comment: expects v to belong to the domain of f
InducedModuleMap VectorInWidth := (f, v) -> (
    fmod := f.freeOIMod;
    targWidth := f.oiMap.targWidth;
    targMod := getModuleInWidth(fmod, targWidth);

    -- Handle the zero vector
    if isZero v then return makeZero targMod;

    algMap := getInducedAlgebraMap(fmod.polyOIAlg, f.oiMap);

    sum for single in getSingles v list makeSingle(targMod, f.img#(single.cache), algMap single.vec#(single.cache))
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
    if isZero f or isZero v then return 0_(getModuleInWidth(f.targMod, getWidth v));

    sum for single in getSingles v list (
        elt := single.vec#(single.cache);
        oiMap := single.cache#0;
        basisIdx := single.cache#1;
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

-- Division function for VectorInWidth terms
-- Args: v = VectorInWidth, w = VectorInWidth
-- Comment: tries to divide v by w and returns a HashTable of the form {quo => RingElement, oiMap => OIMap}
-- Comment: expects v and w to have cache => key
termDiv := (v, w) -> (
    clsv := class v;
    clsw := class w;
    fmod := clsv.freeOIMod;

    if isZero v then return hashTable {quo => 0_(getAlgebraInWidth(fmod.polyOIAlg, clsv.wid)), oiMap => null};

    widv := clsv.wid;
    widw := clsw.wid;
    keyv := v.cache;
    keyw := w.cache;

    if widv === widw then (
        if keyv === keyw and zero(v.vec#keyv % w.vec#keyw) then
            return hashTable {quo => v.vec#keyv // w.vec#keyw, oiMap => (getOIMaps(widw, widv))#0}
    )
    else for oiMap0 in getOIMaps(widw, widv) do (
        modMap := getInducedModuleMap(fmod, oiMap0);
        imgw := modMap w;
        if keyv === imgw.cache and zero(v.vec#keyv % imgw.vec#(imgw.cache)) then
            return hashTable {quo => v.vec#keyv // imgw.vec#(imgw.cache), oiMap => oiMap0}
    );

    return hashTable {quo => 0_(getAlgebraInWidth(fmod.polyOIAlg, clsv.wid)), oiMap => null}
)

-- Divide a VectorInWidth by a List of VectorInWidth objects
-- Args: v = VectorInWidth, L = List
-- Comment: returns a HashTable of the form {quo => VectorInWidth, rem => VectorInWidth, divTuples => List}
-- Comment: expects L to consist of nonzero elements
polyDiv := (v, L) -> (
    if isZero v then return new HashTable from {quo => v, rem => v, divTuples => {}};

    cls := class v;
    quo0 := makeZero cls;
    rem0 := v;

    done := false;
    divTuples0 := while not done list (
        divTuple := null;
        for i to #L - 1 do (
            elt := L#i;
            div := termDiv(leadTerm rem0, leadTerm elt);
            if zero div.quo then continue;

            modMap := getInducedModuleMap(cls.freeOIMod, div.oiMap);
            q := modMap elt;
            quo0 = quo0 + div.quo * q;
            rem0 = rem0 - div.quo * q;

            divTuple = (div, i);
            break
        );

        if divTuple === null then break;
        if isZero rem0 then done = true;

        divTuple
    );

    new HashTable from {quo => quo0, rem => rem0, divTuples => divTuples0}
)

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

-- Should be of the form {map0 => OIMap, vec0 => VectorInWidth, im0 => VectorInWidth, map1 => OIMap, vec1 => VectorInWidth, im1 => VectorInWidth}
OIPair = new Type of HashTable

-- Compute the critical pairs for a List of VectorInWidth objects
-- Args: L = List, V = Boolean
-- Comment: map0 and map1 are the OI-maps applied to vec0 and vec1 to make im0 and im1
oiPairs := (L, V) -> unique flatten flatten flatten flatten for fIdx to #L - 1 list (
    f := L#fIdx;
    ltf := leadTerm f;
    for gIdx from fIdx to #L - 1 list (
        g := L#gIdx;
        ltg := leadTerm g;
        clsf := class f;
        clsg := class g;

        if not ltf.cache#1 === ltg.cache#1 then continue; -- These will have lcm zero

        widf := clsf.wid;
        widg := clsg.wid;
        searchMin := max(widf, widg);
        searchMax := widf + widg;
        for i to searchMax - searchMin list (
            k := searchMax - i;
            oiMapsFromf := getOIMaps(widf, k);

            -- Given an OI-map from f, we construct the corresponding OI-maps from g
            for oiMapFromf in oiMapsFromf list (
                base := set(1..k) - set oiMapFromf.img; -- Get the starting set

                -- Add back in the i-element subsets of oiMapFromf.img and make the pairs
                for subset in subsets(oiMapFromf.img, i) list (
                    oiMapFromg := makeOIMap(k, sort toList(base + set subset));

                    if not oiMapFromf ltf.cache#0 === oiMapFromg ltg.cache#0 then continue; -- These will have lcm zero
                    if fIdx === gIdx and oiMapFromf === oiMapFromg then continue; -- These will yield trivial S-polynomials and syzygies

                    if V then print("Found suitable OI-maps " | net oiMapFromf | " and " | net oiMapFromg);

                    modMapFromf := getInducedModuleMap(clsf.freeOIMod, oiMapFromf);
                    modMapFromg := getInducedModuleMap(clsg.freeOIMod, oiMapFromg);

                    new OIPair from {map0 => oiMapFromf, idx0 => fIdx, im0 => modMapFromf f, map1 => oiMapFromg, idx1 => gIdx, im1 => modMapFromg g}
                )
            )
        )
    )
)

-- Cache for storing OI-Groebner bases
oiGBCache = new MutableHashTable

-- Compute an OI-Groebner basis for a List of VectorInWidth objects
oiGB = method(TypicalValue => List, Options => {Verbose => false, CacheSPolynomials => true, MinimizeOIGB => true})
oiGB List := opts -> L -> (
    if #L === 0 then error "expected a nonempty List";

    if opts.Verbose then print "Computing OIGB...";

    -- Return the GB if it already exists
    if oiGBCache#?(L, opts.MinimizeOIGB) then return oiGBCache#(L, opts.MinimizeOIGB);

    encountered := new List;
    totalAdded := 0;
    ret := L;

    -- Enter the main loop: terminates by an equivariant Noetherianity argument
    while true do (
        oipairs := oiPairs(ret, opts.Verbose);

        remToAdd := null;
        for i to #oipairs - 1 do (
            s := SPolynomial((oipairs#i).im0, (oipairs#i).im1);
            if isZero s then continue;

            if opts.CacheSPolynomials then (
                if member(s, encountered) then continue
                else encountered = append(encountered, s)
            );

            if opts.Verbose then print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));

            rem := (polyDiv(s, ret)).rem;
            if not isZero rem and not member(rem, ret) then (
                if opts.Verbose then (
                    print("Found nonzero remainder: " | net rem);
                    totalAdded = totalAdded + 1;
                    print("Elements added total: " | net totalAdded);
                );

                remToAdd = rem;
                break
            )
        );

        if remToAdd === null then break;
        ret = append(ret, remToAdd)
    );

    -- Minimize the basis
    if opts.MinimizeOIGB then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\n";
        ret = minimizeOIGB(ret, Verbose => opts.Verbose)
    );

    -- Store the GB
    oiGBCache#(L, opts.MinimizeOIGB) = ret
)

-- Minimize an OI-Groebner basis in the sense of monic and lt(p) not in <lt(L - {p})> for all p in L
minimizeOIGB = method(TypicalValue => List, Options => {Verbose => false})
minimizeOIGB List := opts -> L -> (
    if opts.Verbose then print "Computing minimal OIGB...";

    nonRedundant := new List;
    currentBasis := apply(unique L, makeMonic);

    while true do (
        redundantFound := false;

        for p in currentBasis do (
            if member(p, nonRedundant) then continue; -- Skip elements already verified to be nonredundant

            minusp := toList((set currentBasis) - set {p});
            ltp := leadTerm p;
            for elt in minusp do if not zero (termDiv(ltp, leadTerm elt)).quo then (
                if opts.Verbose then print("Found redundant element: " | net p);
                redundantFound = true;
                currentBasis = minusp;
                break
            );

            if redundantFound then break;
            nonRedundant = append(nonRedundant, p);
        );

        if not redundantFound then break
    );

    currentBasis
)

-- Check if a List is an OI-Groebner basis
isOIGB = method(TypicalValue => Boolean, Options => {Verbose => false, CacheSPolynomials => true})
isOIGB List := opts -> L -> (
    if #L === 0 then error "expected a nonempty List";

    encountered := new List;
    oipairs := oiPairs(L, opts.Verbose);
    for i to #oipairs - 1 do (
        s := SPolynomial((oipairs#i).im0, (oipairs#i).im1);
        if isZero s then continue;

        if opts.CacheSPolynomials then (
            if member(s, encountered) then continue
            else encountered = append(encountered, s)
        );

        if opts.Verbose then print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));
    
        rem := (polyDiv(s, L)).rem;
        if not isZero rem then (
            if opts.Verbose then print("Found nonzero remainder: " | net rem);
            return false
        )
    );

    true
)

-- Cache for storing Groebner bases computed with oiSyz
oiSyzCache = new MutableHashTable

-- Compute an OI-Groebner basis for the syzygy module of a List of VectorInWidth objects
oiSyz = method(TypicalValue => List, Options => {Verbose => false, MinimizeOIGB => true})
oiSyz(List, Symbol) := opts -> (L, d) -> (
    if #L === 0 then error "expected a nonempty List";

    if opts.Verbose then print "Computing syzygies...";
    
    -- Return the GB if it already exists
    if oiSyzCache#?(L, d, opts.MinimizeOIGB) then return oiSyzCache#(L, d, opts.MinimizeOIGB);

    fmod := getFreeOIModule L#0;
    shifts := for elt in L list -degree elt;
    widths := for elt in L list getWidth elt;
    G := makeFreeOIModule(d, widths, fmod.polyOIAlg, DegreeShifts => shifts, MonomialOrder => L);

    oipairs := oiPairs(L, opts.Verbose);
    if opts.Verbose then print "Iterating through critical pairs...";
    i := 0;
    ret := for pair in oipairs list (        
        if opts.Verbose then (
            print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));
            print("Pair: (" | net pair.im0 | ", " | net pair.im1 | ")");
            print("Current compCache size: " | net(#keys compCache));
            i = i + 1
        );

        ltf := leadTerm pair.im0;
        ltg := leadTerm pair.im1;
        ltfelt := ltf.vec#(ltf.cache);
        ltgelt := ltg.vec#(ltg.cache);
        lcmlmfg := lcm(leadMonomial ltfelt, leadMonomial ltgelt);
        s := SPolynomial(pair.im0, pair.im1);
        M := getModuleInWidth(G, getWidth s);
        thingToSubtract := makeZero M;

        -- Calculate the stuff to subtract off
        if not isZero s then for tuple in (polyDiv(s, L)).divTuples do
            thingToSubtract = thingToSubtract + makeSingle(M, ((tuple#0).oiMap, 1 + tuple#1), (tuple#0).quo);
        
        -- Make the syzygy
        syzygy := makeSingle(M, (pair.map0, 1 + pair.idx0), lcmlmfg // ltfelt) - makeSingle(M, (pair.map1, 1 + pair.idx1), lcmlmfg // ltgelt) - thingToSubtract;

        if opts.Verbose then print("Generated syzygy: " | net syzygy);

        syzygy
    );

    -- Minimize the basis
    if opts.MinimizeOIGB then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\n";
        ret = minimizeOIGB(ret, Verbose => opts.Verbose)
    );

    -- Store the GB
    oiSyzCache#(L, d, opts.MinimizeOIGB) = ret
)

-- Cache for storing OI-resolutions
oiResCache = new MutableHashTable

-- Should be of the form {dd => List, modules => List}
OIResolution = new Type of HashTable

net OIResolution := C -> (
    N := "0: " | toString C.modules#0;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": " | toString C.modules#i;
    N
)

describe OIResolution := C -> (
    N := "0: Module: " | net C.modules#0 || "Differential: " | net C.dd#0;
    for i from 1 to #C.modules - 1 do N = N || toString i | ": Module: " | net C.modules#i || "Differential: " | net C.dd#i;
    N
)

OIResolution _ ZZ := (C, n) -> C.modules#n

-- Remove a single element from a List
-- Args: L = List, i = ZZ
sdrop := (L, i) -> drop(L, {i, i})

-- Compute an OI-resolution of length n for the OI-module generated by L
oiRes = method(TypicalValue => OIResolution, Options => {Verbose => true, MinimizeOIGB => true})
oiRes(List, ZZ) := opts -> (L, n) -> (
    if n < 0 then error "expected a nonnegative integer";
    if #L === 0 then error "expected a nonempty List";

    -- Return the resolution if it already exists
    if oiResCache#?(L, n, opts.MinimizeOIGB) then return oiResCache#(L, n, opts.MinimizeOIGB);

    ddMut := new MutableList;
    modulesMut := new MutableList;
    groundFreeOIMod := getFreeOIModule L#0;
    e := groundFreeOIMod.basisSym;

    oigb := oiGB(L, Verbose => opts.Verbose, MinimizeOIGB => opts.MinimizeOIGB);
    currentGB := oigb;
    currentSymbol := getSymbol concatenate(e, "0");
    count := 0;

    if n > 0 then for i to n - 1 do (
            if opts.Verbose then print "----------------------------------------\n----------------------------------------\n";
            syzGens := oiSyz(currentGB, currentSymbol, Verbose => opts.Verbose, MinimizeOIGB => opts.MinimizeOIGB);

            if #syzGens === 0 then break;
            count = count + 1;

            targFreeOIMod := getFreeOIModule currentGB#0;
            srcFreeOIMod := getFreeOIModule syzGens#0;

            modulesMut#i = srcFreeOIMod;
            ddMut#i = new FreeOIModuleMap from {srcMod => srcFreeOIMod, targMod => targFreeOIMod, genImages => currentGB};

            currentGB = syzGens;
            currentSymbol = getSymbol concatenate(e, toString count)
    );

    -- Append the last term in the sequence
    shifts := for elt in currentGB list -degree elt;
    widths := for elt in currentGB list getWidth elt;
    modulesMut#count = makeFreeOIModule(currentSymbol, widths, groundFreeOIMod.polyOIAlg, DegreeShifts => shifts, MonomialOrder => currentGB);
    ddMut#count = new FreeOIModuleMap from {srcMod => modulesMut#count, targMod => if count === 0 then groundFreeOIMod else modulesMut#(count - 1), genImages => currentGB};

    -- Cap the sequence with zeros
    for i from count + 1 to n do (
        currentSymbol = getSymbol concatenate(e, toString i);
        modulesMut#i = makeFreeOIModule(currentSymbol, {}, groundFreeOIMod.polyOIAlg);
        ddMut#i = new FreeOIModuleMap from {srcMod => modulesMut#i, targMod => modulesMut#(i - 1), genImages => {}}
    );

    -- Minimize the resolution
    if #ddMut > 1 and not isZero ddMut#1 then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\n\nMinimizing resolution...";

        done := false;
        while not done do (
            done = true;

            -- Look for units on identity basis elements
            unitFound := false;
            local data;
            for i from 1 to #ddMut - 1 do (
                ddMap := ddMut#i;
                if isZero ddMap then continue;
                
                srcFreeOIMod := ddMap.srcMod;
                targFreeOIMod := ddMap.targMod;
                for j to #ddMap.genImages - 1 do (
                    if isZero ddMap.genImages#j then continue;

                    for single in getSingles ddMap.genImages#j do if (single.cache#0).img === toList(1..(single.cache#0).targWidth) and isUnit single.vec#(single.cache) then (
                        unitFound = true;
                        done = false;
                        data = {i, j, single};
                        if opts.Verbose then print("Unit found on term: " | net single);
                        break
                    );

                    if unitFound then break
                );

                if unitFound then break
            );

            -- Prune the sequence
            if unitFound then (
                if opts.Verbose then print "Pruning...";

                unitSingle := data#2;
                targBasisPos := unitSingle.cache#1 - 1;
                srcBasisPos := data#1;
                ddMap := ddMut#(data#0);
                srcFreeOIMod := ddMap.srcMod;
                targFreeOIMod := ddMap.targMod;

                -- Make the new free OI-modules
                newSrcWidths := sdrop(srcFreeOIMod.genWidths, srcBasisPos);
                newSrcShifts := sdrop(srcFreeOIMod.degShifts, srcBasisPos);
                newTargWidths := sdrop(targFreeOIMod.genWidths, targBasisPos);
                newTargShifts := sdrop(targFreeOIMod.degShifts, targBasisPos);
                newSrcFreeOIMod := makeFreeOIModule(srcFreeOIMod.basisSym, newSrcWidths, srcFreeOIMod.polyOIAlg, DegreeShifts => newSrcShifts);
                newTargFreeOIMod := makeFreeOIModule(targFreeOIMod.basisSym, newTargWidths, targFreeOIMod.polyOIAlg, DegreeShifts => newTargShifts);

                -- Compute the new differential
                newGenImages := for i to #srcFreeOIMod.genWidths - 1 list (
                    if i === srcBasisPos then continue;

                    -- Calculate the stuff to subtract off
                    thingToSubtract := makeZero getModuleInWidth(srcFreeOIMod, srcFreeOIMod.genWidths#i);
                    for single in getSingles ddMap.genImages#i do (
                        if not single.cache#1 === targBasisPos + 1 then continue;

                        modMap := getInducedModuleMap(srcFreeOIMod, single.cache#0);
                        basisElt := getGenerator(srcFreeOIMod, srcBasisPos);
                        thingToSubtract = thingToSubtract + single.vec#(single.cache) * modMap basisElt
                    );

                    -- Calculate the new image
                    basisElt := getGenerator(srcFreeOIMod, i);
                    newGenImage0 := ddMap(basisElt - lift(1 // unitSingle.vec#(unitSingle.cache), srcFreeOIMod.polyOIAlg.baseField) * thingToSubtract);
                    M := getModuleInWidth(newTargFreeOIMod, getWidth newGenImage0);
                    newGenImage := makeZero M;
                    for newSingle in getSingles newGenImage0 do (
                        idx := newSingle.cache#1;
                        if idx > targBasisPos + 1 then idx = idx - 1; -- Relabel
                        newGenImage = newGenImage + makeSingle(M, (newSingle.cache#0, idx), newSingle.vec#(newSingle.cache))
                    );

                    newGenImage
                );

                ddMut#(data#0) = new FreeOIModuleMap from {srcMod => newSrcFreeOIMod, targMod => newTargFreeOIMod, genImages => newGenImages};
                modulesMut#(data#0) = newSrcFreeOIMod;
                modulesMut#(data#0 - 1) = newTargFreeOIMod;

                -- Adjust the map to the right
                ddMap = ddMut#(data#0 - 1);
                ddMut#(data#0 - 1) = new FreeOIModuleMap from {srcMod => newTargFreeOIMod, targMod => ddMap.targMod, genImages => sdrop(ddMap.genImages, targBasisPos)}; -- Restriction

                -- Adjust the map to the left
                if data#0 < #ddMut - 1 then (
                    ddMap = ddMut#(data#0 + 1);
                    newGenImages = new MutableList;

                    for i to #ddMap.genImages - 1 do (
                        M := getModuleInWidth(newSrcFreeOIMod, getWidth ddMap.genImages#i);
                        newGenImage := makeZero M;
                        for single in getSingles ddMap.genImages#i do (
                            idx := single.cache#1;
                            if idx === srcBasisPos + 1 then continue; -- Projection
                            if idx > srcBasisPos + 1 then idx = idx - 1; -- Relabel
                            newGenImage = newGenImage + makeSingle(M, (single.cache#0, idx), single.vec#(single.cache))
                        );

                        newGenImages#i = newGenImage
                    );

                    ddMut#(data#0 + 1) = new FreeOIModuleMap from {srcMod => ddMap.srcMod, targMod => newSrcFreeOIMod, genImages => new List from newGenImages}
                )
            )
        )
    );

    -- Store the resolution
    oiResCache#(L, n, opts.MinimizeOIGB) = new OIResolution from {dd => new List from ddMut, modules => new List from modulesMut}
)

-- Verify that an OIResolution is a complex
isComplex = method(TypicalValue => Boolean, Options => {Verbose => true})
isComplex OIResolution := opts -> C -> (
    if #C.dd < 2 then error "expected a sequence with at least two maps";

    -- Check if the maps compose to zero
    for i from 1 to #C.dd - 1 do (
        modMap0 := C.dd#(i - 1);
        modMap1 := C.dd#i;
        if isZero modMap0 or isZero modMap1 then continue;

        srcFreeOIMod := modMap1.srcMod;
        basisElts := for i to #srcFreeOIMod.genWidths - 1 list getGenerator(srcFreeOIMod, i);

        for basisElt in basisElts do (
            result := modMap0 modMap1 basisElt;

            if opts.Verbose then print(net basisElt | " maps to " | net result);
            
            if not isZero result then (
                if opts.Verbose then print("Found nonzero image: " | net result);
                return false
            )
        )
    );

    true
)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- DOCUMENTATION ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

beginDocumentation()



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- TESTS -----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



end

-- GB example 1: one linear and one quadratic
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 3.20
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1,2}, P);
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
time B = oiGB({b1, b2}, Verbose => true)

-- Res example 1: single quadratic in width 3
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 5.5 (i)
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,2}, P);
installBasisElements(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
time C = oiRes({b}, 2, Verbose => true)

-- Res example 2: single quadratic in width 2
-- Comment: see https://arxiv.org/pdf/2303.06725.pdf example 5.5 (ii)
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installBasisElements(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},2);
time C = oiRes({b}, 0, Verbose => true)

-- Res example 3: single quadratic in width 2
-- Comment: compare with res example 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1}, P);
installBasisElements(F, 2);
b = x_(1,2)*x_(1,1)*e_(2,{2},1)+x_(2,2)*x_(2,1)*e_(2,{1},1);
time C = oiRes({b}, 0, Verbose => true)

-- Res example 4: single quadratic in width 3
-- Comment: compare with res example 1
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1}, P);
installBasisElements(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1},2);
time C = oiRes({b}, 2, Verbose => true)