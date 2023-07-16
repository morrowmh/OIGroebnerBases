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
        "ModuleInWidth", "VectorInWidth",
    
    -- Keys
        -- From PolynomialOIAlgebra.m2
        "ColUpRowUp", "ColUpRowDown", "ColDownRowUp", "ColDownRowDown",
        "RowUpColUp", "RowUpColDown", "RowDownColUp", "RowDownColDown",
    
    -- Methods
        -- From PolynomialOIAlgebra.m2
        "makePolynomialOIAlgebra",

        -- From FreeOIModule.m2
        "makeFreeOIModule", "installBasisElements",

        -- From OIGB.m2
        "oiGB", "minimizeOIGB",
    
    -- Options
        -- From FreeOIModule.m2
        "DegreeShifts"
}

scan({
    -- Keys
        -- From OIMap.2
        targWidth, img,

        -- From PolynomialOIAlgebra.m2
        varRows, varSym, baseField, varOrder, algebras, maps,

        -- From FreeOIModule.m2
        basisSym, genWidths, degShifts, polyOIAlg, monOrder, modules, basisKeys, wid, rawMod, freeOIMod, vec, oiMap,

        -- From Division.m2
        quo, rem, divTuples,

        -- From OIPair.m2
        map0, vec0, im0, map1, vec1, im1,
    
    -- Options
        -- From PolynomialOIAlgebra.m2
        VariableOrder,

        -- From OIGB.m2
        CacheSPolynomials, MinimizeOIGB
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
        for elt in L do (
            div := termDiv(leadTerm rem0, leadTerm elt);
            if zero div.quo then continue;

            modMap := getInducedModuleMap(cls.freeOIMod, div.oiMap);
            q := modMap elt;
            quo0 = quo0 + div.quo * q;
            rem0 = rem0 - div.quo * q;

            divTuple = (div, elt);
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
oiPairs := (L, V) -> toList set flatten flatten flatten flatten for fIdx to #L - 1 list (
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

                    if V then print("Found suitable OI-maps " | net oiMapFromf | " and " | net oiMapFromg);

                    modMapFromf := getInducedModuleMap(clsf.freeOIMod, oiMapFromf);
                    modMapFromg := getInducedModuleMap(clsg.freeOIMod, oiMapFromg);

                    new OIPair from {map0 => oiMapFromf, vec0 => f, im0 => modMapFromf f, map1 => oiMapFromg, vec1 => g, im1 => modMapFromg g}
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
    -- Return the GB if it already exists
    if oiGBCache#?(L, opts.MinimizeOIGB) then return oiGBCache#(L, MinimizeOIGB);

    if #L === 0 then error "expected a nonempty List";

    if opts.Verbose then print "Computing OIGB...";

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

-- Minimize an OI-Groebner basis in the sense of lt(p) not in <lt(L - {p})> for all p in L
minimizeOIGB = method(TypicalValue => List, Options => {Verbose => false})
minimizeOIGB List := opts -> L -> (
    if opts.Verbose then print "Computing minimal OIGB...";

    nonRedundant := new List;
    currentBasis := toList set L;
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

-- Small GB example
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,1,2}, DegreeShifts => {1, -1, 0}, P);
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1)+x_(2,1)*e_(1,{1},2);
use F_2; b2 = x_(1,2)*x_(1,1)*e_(2,{2},2)+x_(2,2)*x_(2,1)*e_(2,{1,2},3);
time B = oiGB({b1, b2}, Verbose => true)

-- Single quadratic in width 3
restart
P = makePolynomialOIAlgebra(2, x, QQ);
F = makeFreeOIModule(e, {1,2}, P);
installBasisElements(F, 3);
b = x_(1,2)*x_(1,1)*e_(3,{2},1)+x_(2,2)*x_(2,1)*e_(3,{1,3},2);
time B = oiGB({b}, Verbose => true)