-- -*- coding: utf-8 -*-

-- PURPOSE: Algorithms for computing Gröbner bases, syzygies and free resolutions for submodules of free OI-modules over Noetherian polynomial OI-algebras
-- PROGRAMMER: Michael Morrow
-- COMMENT: This package was made using Macaulay2-Package-Template, available here: https://github.com/morrowmh/Macaulay2-Package-Template

newPackage("OIGroebnerBases",
    Headline => "Computation in OI-modules over Noetherian polynomial OI-algebras",
    Version => "2.0.0-dev",
    Date => "April 4, 2022", -- Project birthday
    Keywords => { "Commutative Algebra" },
    Authors => {
        { Name => "Michael Morrow", HomePage => "https://michaelmorrow.org", Email => "michaelhmorrow98@gmail.com" }
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
        -- From OIMap.m2
        "OIMap",

        -- From PolynomialOIAlgebra.m2
        "PolynomialOIAlgebra",

        -- From FreeOIModule.m2
        "FreeOIModule", "ModuleInWidth", "VectorInWidth",

        -- From InducedModuleMap.m2
        "InducedModuleMap",

        -- From FreeOIModuleMap.m2
        "FreeOIModuleMap",

        -- From OIResolution.m2
        "OIResolution",

    -- Keys
        -- From PolynomialOIAlgebra.m2
        "ColUpRowUp", "ColUpRowDown", "ColDownRowUp", "ColDownRowDown", "RowUpColUp", "RowUpColDown", "RowDownColUp", "RowDownColDown",

    -- Methods
        -- From OIMap.m2
        "makeOIMap", "getOIMaps", "composeOIMaps",
    
        -- From PolynomialOIAlgebra.m2
        "makePolynomialOIAlgebra", "getAlgebraInWidth", "getInducedAlgebraMap",
    
        -- From FreeOIModule.m2
        "getGenWidths", "getDegShifts", "makeFreeOIModule", "getMonomialOrder", "isZero", "getFreeModuleInWidth", "widthOfElement", "freeOIModuleFromElement", "installBasisElements",
    
        -- From Terms.m2
        "makeMonic",
    
        -- From InducedModuleMap.m2
        "getInducedModuleMap",

        -- From FreeOIModuleMap.m2
        "makeFreeOIModuleMap",

        -- From Algorithms.m2
        "oiGB", "minimizeOIGB", "oiSyz", "isOIGB",

        -- From OIResolution.m2
        "oiRes", "isComplex",

    -- Options
        -- From PolynomialOIAlgebra.m2
        "VariableOrder",

        -- From FreeOIModule.m2
        "DegreeShifts",
    
        -- From Algorithms.m2
        "MinimalOIGB"
}

scan({
    -- Keys
        -- From OIMap.m2
        targWidth, img,

        -- From PolynomialOIAlgebra.m2
        baseField, varRows, varSym, varOrder, algebras, maps,
    
        -- From FreeOIModule.m2
        polyOIAlg, basisSym, genWidths, degShifts, monOrder, modules, freeOIMod, Width, basisElements, basisElementPositions,
    
        -- From Terms.m2
        ringElement, oiMap, idx, basisIndex, quo,
    
        -- From FreeOIModuleMap.m2
        srcMod, targMod, genImages,
    
        -- From Algorithms.m2
        rem, triples,

    -- Options
        -- From Terms.m2
        CombineLikeTerms
}, protect)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- BODY ------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Should be of the form {targWidth => ZZ, img => List}
OIMap = new Type of HashTable

net OIMap := f -> "Source: ["|net(#f.img)|"] Target: ["|net f.targWidth|"]" || "Image: "|net f.img

source OIMap := f -> toList(1..#f.img)
target OIMap := f -> toList(1..f.targWidth)
image OIMap := f -> f.img

-- Given an OI-map f, compute f(n)
OIMap ZZ := (f, n) -> f.img#(n - 1)

makeOIMap = method(TypicalValue => OIMap)
makeOIMap(ZZ, List) := (n, L) -> new OIMap from {targWidth => n, img => L}

-- Get the OI-maps between two widths
getOIMaps = method(TypicalValue => List)
getOIMaps(ZZ, ZZ) := (m, n) -> (
    if n < m then return {};

    sets := subsets(toList(1..n), m);
    for i to #sets - 1 list new OIMap from {targWidth => n, img => sets#i}
)

-- Given OI-maps f and g, compute f(g)
composeOIMaps = method(TypicalValue => OIMap)
composeOIMaps(OIMap, OIMap) := (f, g) -> (
    if not source f === target g then error "maps cannot be composed due to incompatible source and target";

    -- Compute the composition
    L := for i in source g list f g i;
    makeOIMap(f.targWidth, L)
)

-- Shorthand composition
OIMap OIMap := (f, g) -> composeOIMaps(f, g)

-- Should be of the form {baseField => Ring, varRows => ZZ, varSym => Symbol, varOrder => Symbol, algebras => MutableHashTable, maps => MutableHashTable}
PolynomialOIAlgebra = new Type of HashTable

toString PolynomialOIAlgebra := P -> "("|toString P.baseField|", "|toString P.varRows|", "|toString P.varSym|", "|toString P.varOrder|")"

net PolynomialOIAlgebra := P -> "Base field: "|net P.baseField ||
    "Number of variable rows: "|net P.varRows ||
    "Variable symbol: "|net P.varSym ||
    "Variable order: "|net P.varOrder

makePolynomialOIAlgebra = method(TypicalValue => PolynomialOIAlgebra, Options => {VariableOrder => RowUpColUp})
makePolynomialOIAlgebra(Ring, ZZ, Symbol) := opts -> (K, c, x) -> (
    if c < 1 then error "expected at least one row of variables";
    v := opts.VariableOrder;
    if not member(v, {
        ColUpRowUp, ColUpRowDown, ColDownRowUp, ColDownRowDown,
        RowUpColUp, RowUpColDown, RowDownColUp, RowDownColDown
    }) then error "invalid variable order";

    new PolynomialOIAlgebra from {
            baseField => K,
            varRows => c,
            varSym => x,
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
linearFromRowCol := (P, n, i, j) -> (orderTable#(P.varOrder))(P, n, i, j)

getAlgebraInWidth = method(TypicalValue => PolynomialRing)
getAlgebraInWidth(PolynomialOIAlgebra, ZZ) := (P, n) -> (
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

-- Shorthand for getAlgebraInWidth
PolynomialOIAlgebra _ ZZ := (P, n) -> getAlgebraInWidth(P, n)

-- Get the algebra map induced by an OI-map
getInducedAlgebraMap = method(TypicalValue => RingMap)
getInducedAlgebraMap(PolynomialOIAlgebra, OIMap) := (P, f) -> (
    -- Return the map if it already exists
    if P.maps#?f then return P.maps#f;

    -- Generate the assignment
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

-- Should be of the form {polyOIAlg => PolynomialOIAlgebra, basisSym => Symbol, genWidths => List, degShifts => List, monOrder => Thing, modules => MutableHashTable, maps => MutableHashTable}
FreeOIModule = new Type of HashTable

toString FreeOIModule := F -> "("|toString F.basisSym|", "|toString F.genWidths|", "|toString F.degShifts|")"

net FreeOIModule := F -> (
    local monOrderNet;
    if F.monOrder === Lex then monOrderNet = net Lex
    else if instance(F.monOrder, List) then monOrderNet = "Schreyer order induced by "|net F.monOrder;

    "Polynomial OI-algebra: "|toString F.polyOIAlg ||
    "Basis symbol: "|net F.basisSym ||
    "Generator widths: "|net F.genWidths ||
    "Degree shifts: "|net F.degShifts ||
    "Monomial order: "|monOrderNet
)

getGenWidths = method(TypicalValue => List)
getGenWidths FreeOIModule := F -> F.genWidths

getDegShifts = method(TypicalValue => List)
getDegShifts FreeOIModule := F -> F.degShifts

makeFreeOIModule = method(TypicalValue => FreeOIModule, Options => {DegreeShifts => null, MonomialOrder => Lex})
makeFreeOIModule(PolynomialOIAlgebra, Symbol, List) := opts -> (P, e, W) -> (
    local shifts;
    if opts.DegreeShifts === null then shifts = toList(#W : 0)
    else if instance(opts.DegreeShifts, List) then shifts = opts.DegreeShifts
    else error "invalid DegreeShifts option";

    -- Validate the monomial order
    if not opts.MonomialOrder === Lex and not (
        instance(opts.MonomialOrder, List) and 
        W === apply(opts.MonomialOrder, widthOfElement) and 
        #set apply(opts.MonomialOrder, freeOIModuleFromElement) == 1) then error "invalid monomial order";

    new FreeOIModule from {
        polyOIAlg => P,
        basisSym => e,
        genWidths => W,
        degShifts => shifts,
        monOrder => opts.MonomialOrder,
        modules => new MutableHashTable,
        maps => new MutableHashTable}
)

getMonomialOrder = method()
getMonomialOrder FreeOIModule := F -> F.monOrder

-- Should also contain the key-value pairs freeOIMod => FreeOIModule, Width => ZZ, basisElements => List, and basisElementPositions => HashTable
ModuleInWidth = new Type of Module

net ModuleInWidth := M -> (
    rawMod := new Module from M;
    net rawMod | " in width " | net rawMod.Width
)

-- Each element 'f' in a ModuleInWidth 'M' should have type VectorInWidth with class f === M
VectorInWidth = new Type of Vector

isZero = method(TypicalValue => Boolean)
isZero VectorInWidth := f -> f === 0_(class f)
isZero FreeOIModule := F -> #F.genWidths === 0

net VectorInWidth := f -> (
    if isZero f then return net 0;
    oiTerms := getOITermsFromVector(f, CombineLikeTerms => true);
    if #oiTerms == 1 then return net oiTerms#0;
    
    str := "";
    for i to #oiTerms - 2 do str = str|net oiTerms#i|" + "; -- TODO: Make negatives look better
    str = str|net oiTerms#-1
)

getFreeModuleInWidth = method(TypicalValue => ModuleInWidth)
getFreeModuleInWidth(FreeOIModule, ZZ) := (F, n) -> (
    -- Return the module if it already exists
    if F.modules#?n then return F.modules#n;

    -- Generate the degrees
    alg := getAlgebraInWidth(F.polyOIAlg, n);
    degList := for i to #F.genWidths - 1 list binomial(n, F.genWidths#i) : F.degShifts#i;

    -- Make the module
    retHash := new MutableHashTable from alg^degList;
    retHash.Width = n;
    retHash.freeOIMod = F;
    
    -- Generate the basis elements
    k := 0;
    mutPos := new MutableHashTable;
    retHash.basisElements = flatten for i to #F.genWidths - 1 list
        for oiMap in getOIMaps(F.genWidths#i, n) list (
            b := makeBasisElement makeBasisIndex(F, oiMap, i + 1);
            mutPos#b = k;
            k = k + 1;
            b
        );
    
    retHash.basisElementPositions = new HashTable from mutPos;

    ret := new ModuleInWidth of VectorInWidth from retHash;

    -- Store the module
    F.modules#n = ret
)

-- Shorthand for getFreeModuleInWidth
FreeOIModule _ ZZ := (F, n) -> getFreeModuleInWidth(F, n)

-- Use a ModuleInWidth
use ModuleInWidth := M -> ( use getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.Width); M )

widthOfElement = method(TypicalValue => ZZ)
widthOfElement VectorInWidth := f -> (class f).Width

freeOIModuleFromElement = method(TypicalValue => FreeOIModule)
freeOIModuleFromElement VectorInWidth := f -> (class f).freeOIMod

installBasisElements = method()
installBasisElements(FreeOIModule, ZZ) := (F, n) -> (
    freeMod := getFreeModuleInWidth(F, n);
    for i to #F.genWidths - 1 do
        for oiMap in getOIMaps(F.genWidths#i, n) do (
            b := makeBasisElement makeBasisIndex(F, oiMap, i + 1);
            F.basisSym_(oiMap.targWidth, oiMap.img, i + 1) <- freeMod_(freeMod.basisElementPositions#b)
        )
)

-- Should be of the form {freeOIMod => FreeOIModule, oiMap => OIMap, idx => ZZ}
BasisIndex = new Type of HashTable

makeBasisIndex = method(TypicalValue => BasisIndex)
makeBasisIndex(FreeOIModule, OIMap, ZZ) := (F, f, i) -> new BasisIndex from {freeOIMod => F, oiMap => f, idx => i}

-- Should be of the form {ringElement => RingElement, basisIndex => BasisIndex}
OITerm = new Type of HashTable

makeOITerm = method(TypicalValue => OITerm)
makeOITerm(RingElement, BasisIndex) := (elt, b) -> new OITerm from {ringElement => elt, basisIndex => b}

net OITerm := f -> (
    local ringElementNet;
    if #terms f.ringElement == 0 then return net 0;
    if #terms f.ringElement == 1 then ringElementNet = net f.ringElement
    else ringElementNet = "("|net f.ringElement|")";
    ringElementNet | net f.basisIndex.freeOIMod.basisSym_(toString f.basisIndex.oiMap.targWidth, toString f.basisIndex.oiMap.img, f.basisIndex.idx)
)

isZero RingElement := f -> f === 0_(class f)
isZero OITerm := f -> isZero f.ringElement

-- Cache for storing OITerm comparisons
oiTermCompCache = new MutableHashTable

-- Comparison method for OITerm
OITerm ? OITerm := (f, g) -> (
    -- Return the comparison if it already exists
    if oiTermCompCache#?(f, g) then return oiTermCompCache#(f, g);

    -- Generate the comparison
    if f === g or (isZero f and isZero g) then return oiTermCompCache#(f, g) = symbol ==;
    if isZero f then return oiTermCompCache#(f, g) = symbol <;
    if isZero g then return oiTermCompCache#(f, g) = symbol >;

    eltf := f.ringElement; eltg := g.ringElement;
    bf := f.basisIndex; bg := g.basisIndex;
    oiMapf := bf.oiMap; oiMapg := bg.oiMap;
    idxf := bf.idx; idxg := bg.idx;

    if not bf.freeOIMod === bg.freeOIMod then return oiTermCompCache#(f, g) = incomparable;
    
    freeOIMod := bf.freeOIMod;
    monOrder := getMonomialOrder freeOIMod;
    local ret;
    if monOrder === Lex then ( -- LEX ORDER
        if not idxf === idxg then ( if idxf < idxg then ret = symbol > else ret = symbol < )
        else if not oiMapf.targWidth === oiMapg.targWidth then ret = oiMapf.targWidth ? oiMapg.targWidth
        else if not oiMapf.img === oiMapg.img then ret = oiMapf.img ? oiMapg.img
        else ret = eltf ? eltg
    )
    else if instance(monOrder, List) then ( -- SCHREYER ORDER
        freeOIModMap := makeFreeOIModuleMap(freeOIModuleFromElement monOrder#0, freeOIMod, monOrder);
        imgf := applyFreeOIModuleMap(freeOIModMap, {f});
        imgg := applyFreeOIModuleMap(freeOIModMap, {g});
        loimimf := makeMonic leadOITerm imgf;
        loimimg := makeMonic leadOITerm imgg;

        if not loimimf === loimimg then ret = loimimf ? loimimg
        else if not idxf === idxg then ( if idxf < idxg then ret = symbol > else ret = symbol < )
        else if not oiMapf.targWidth === oiMapg.targWidth then ( if oiMapf.targWidth < oiMapg.targWidth then ret = symbol > else ret = symbol < )
        else if not oiMapf.img === oiMapg.img then ( if oiMapf.img < oiMapg.img then ret = symbol > else ret = symbol < )
        else ret = symbol ==
    )
    else error "monomial order not supported";

    -- Store the comparison
    oiTermCompCache#(f, g) = ret
)

-- Comparison method for VectorInWidth
VectorInWidth ? VectorInWidth := (f, g) -> if isZero f and isZero g then symbol == else if isZero f then symbol < else if isZero g then symbol > else leadOITerm f ? leadOITerm g

makeBasisElement = method(TypicalValue => OITerm)
makeBasisElement BasisIndex := b -> (
    one := 1_(getAlgebraInWidth(b.freeOIMod.polyOIAlg, b.oiMap.targWidth));
    new OITerm from {ringElement => one, basisIndex => b}
)

getOITermsFromVector = method(TypicalValue => List, Options => {CombineLikeTerms => false})
getOITermsFromVector VectorInWidth := opts -> f -> (
    if isZero f then error "the zero element has no OI-terms";

    freeMod := class f;
    entryList := entries f;
    
    if opts.CombineLikeTerms then reverse sort for i to #entryList - 1 list (
        if isZero entryList#i then continue;
        makeOITerm(entryList#i, (freeMod.basisElements#i).basisIndex)
    ) else reverse sort flatten for i to #entryList - 1 list (
        if isZero entryList#i then continue;
        for term in terms entryList#i list makeOITerm(term, (freeMod.basisElements#i).basisIndex)
    )
)

getVectorFromOITerms = method(TypicalValue => VectorInWidth)
getVectorFromOITerms List := L -> (
    if #L == 0 then error "getVectorFromOITerms expects a nonempty input";

    Width := (L#0).basisIndex.oiMap.targWidth;
    freeOIMod := (L#0).basisIndex.freeOIMod;
    freeMod := getFreeModuleInWidth(freeOIMod, Width);
    vect := 0_freeMod;

    for oiTerm in L do (
        ringElement := oiTerm.ringElement;
        basisElement := makeBasisElement oiTerm.basisIndex;
        vect = vect + ringElement * freeMod_(freeMod.basisElementPositions#basisElement)
    );
    
    vect
)

leadOITerm = method(TypicalValue => OITerm)
leadOITerm VectorInWidth := f -> (
    if isZero f then error "the zero element has no lead OI-term";
    oiTerms := getOITermsFromVector f;
    oiTerms#0
)

leadTerm VectorInWidth := f -> (
    if isZero f then return f;
    loitf := leadOITerm f;
    getVectorFromOITerms {loitf}
)

leadCoefficient OITerm := f -> leadCoefficient f.ringElement

leadCoefficient VectorInWidth := f -> (
    if isZero f then return 0_((freeOIModuleFromElement f).polyOIAlg.baseField);
    leadCoefficient leadOITerm f
)

leadMonomial VectorInWidth := f -> (
    if isZero f then error "the zero element has no lead monomial";
    loitf := leadOITerm f;
    getVectorFromOITerms {makeMonic loitf}
)

-- Scale an OITerm by a number
OITerm // Number := (f, r) -> (
    if isZero f then return f;
    makeOITerm(f.ringElement // r_(class f.ringElement), f.basisIndex)
)

-- Scale a VectorInWidth by a number
VectorInWidth // Number := (f, r) -> (
    if isZero f then return f;
    oiTerms := getOITermsFromVector f;
    getVectorFromOITerms for oiTerm in oiTerms list oiTerm // r
)

makeMonic = method(TypicalValue => VectorInWidth)
makeMonic OITerm := f -> if isZero f then error "cannot make the zero element monic" else f // leadCoefficient f
makeMonic VectorInWidth := f -> if isZero f then error "cannot make the zero element monic" else f // leadCoefficient f

-- Tries to divide f by g
-- Returns a HashTable of the form {quo => RingElement, oiMap => OIMap}
oiTermDiv = method(TypicalValue => HashTable)
oiTermDiv(OITerm, OITerm) := (f, g) -> (
    if isZero g then error "cannot divide an OI-term by zero";

    freeOIModf := f.basisIndex.freeOIMod;
    freeOIModg := g.basisIndex.freeOIMod;
    if not freeOIModf === freeOIModg then error "OI-terms do not belong to the same OI-module";
    if isZero f then return new HashTable from {quo => 0_(class f.ringElement), oiMap => null};

    Widthf := f.basisIndex.oiMap.targWidth;
    Widthg := g.basisIndex.oiMap.targWidth;
    if Widthf === Widthg then (
        if f.basisIndex === g.basisIndex and isZero(f.ringElement % g.ringElement) then
            return new HashTable from {quo => f.ringElement // g.ringElement, oiMap => (getOIMaps(Widthg, Widthf))#0}
    ) else (
        for oiMap0 in getOIMaps(Widthg, Widthf) do (
            modMap := getInducedModuleMap(freeOIModf, oiMap0);
            img := leadOITerm applyModuleMap(modMap, {g});
            if img.basisIndex === f.basisIndex and isZero(f.ringElement % img.ringElement) then
                return new HashTable from {quo => f.ringElement // img.ringElement, oiMap => oiMap0}
        )
    );

    new HashTable from {quo => 0_(class f.ringElement), oiMap => null}
)

-- Divide the lead terms of two VectorInWidths
VectorInWidth // VectorInWidth := (f, g) -> if isZero f then return f else if isZero g then error "cannot divide by zero" else values oiTermDiv(leadOITerm f, leadOITerm g)

lcm(OITerm, OITerm) := (f, g) -> (
    if not f.basisIndex === g.basisIndex then return makeOITerm(0_(class f.ringElement), f.basisIndex);
    makeOITerm(lcm(f.ringElement, g.ringElement), f.basisIndex)
)

lcm(VectorInWidth, VectorInWidth) := (f, g) -> if isZero f then f else if isZero g then g else getVectorFromOITerms {lcm(leadOITerm f, leadOITerm g)}

terms VectorInWidth := f -> (
    if isZero f then return {};
    oiTerms := getOITermsFromVector f;
    for oiTerm in oiTerms list getVectorFromOITerms {oiTerm}
)

-- Should be of the form {freeOIMod => FreeOIModule, oiMap => OIMap, img => HashTable}
InducedModuleMap = new Type of HashTable

net InducedModuleMap := f -> "Module map from "|net getFreeModuleInWidth(f.freeOIMod, #f.oiMap.img)|" to "|net getFreeModuleInWidth(f.freeOIMod, f.oiMap.targWidth)|" induced by the OI-map: "|net f.oiMap

source InducedModuleMap := f -> getFreeModuleInWidth(f.freeOIMod, #f.oiMap.img)
target InducedModuleMap := f -> getFreeModuleInWidth(f.freeOIMod, f.oiMap.targWidth)

getInducedModuleMap = method(TypicalValue => InducedModuleMap)
getInducedModuleMap(FreeOIModule, OIMap) := (F, f) -> (
    -- Return the map if it already exists
    if F.maps#?f then return F.maps#f;

    -- Generate the BasisIndex assignments
    m := #f.img;
    freeModm := getFreeModuleInWidth(F, m);
    basisElementsm := freeModm.basisElements;
    indexImages := new HashTable from for basisElementm in basisElementsm list basisElementm.basisIndex => makeBasisIndex(F, composeOIMaps(f, basisElementm.basisIndex.oiMap), basisElementm.basisIndex.idx);

    -- Make the map
    ret := new InducedModuleMap from {freeOIMod => F, oiMap => f, img => indexImages};

    -- Store the map
    F.maps#f = ret
)

-- Apply an InducedModuleMap to a List of OI-terms
applyModuleMap = method(TypicalValue => VectorInWidth)
applyModuleMap(InducedModuleMap, List) := (f, oiTerms) -> (
    if #oiTerms == 0 then error "cannot apply InducedModuleMap to an empty list";

    -- Generate the new terms
    algMap := getInducedAlgebraMap(f.freeOIMod.polyOIAlg, f.oiMap);
    getVectorFromOITerms for oiTerm in oiTerms list (
        ringElement := oiTerm.ringElement;
        basisIndex := oiTerm.basisIndex;
        newRingElement := algMap ringElement;
        newBasisIndex := f.img#basisIndex;
        makeOITerm(newRingElement, newBasisIndex)
    )
)

-- Juxtaposition method for InducedModuleMap and VectorInWidth
InducedModuleMap VectorInWidth := (f, v) -> (
    freeOIMod := f.freeOIMod;
    freeOIModFromVector := freeOIModuleFromElement v;
    if not freeOIMod === freeOIModFromVector then error "element does not belong to the required FreeOIModule";
    if not source f === class v then error "element does not belong to the source of the map";

    -- Handle the zero vector
    if isZero v then return 0_(getFreeModuleInWidth(freeOIMod, f.oiMap.targWidth));

    applyModuleMap(f, getOITermsFromVector v)
)

-- Should be of the form {srcMod => FreeOIModule, targMod => FreeOIModule, genImages => List}
FreeOIModuleMap = new Type of HashTable

net FreeOIModuleMap := f -> "Source module: "|toString f.srcMod ||
    "Target module: "|toString f.targMod ||
    "Generator images: "|net f.genImages

source FreeOIModuleMap := f -> f.srcMod
target FreeOIModuleMap := f -> f.targMod

makeFreeOIModuleMap = method(TypicalValue => FreeOIModuleMap)
makeFreeOIModuleMap(FreeOIModule, FreeOIModule, List) := (G, F, L) -> new FreeOIModuleMap from {srcMod => F, targMod => G, genImages => L}

isZero FreeOIModuleMap := f -> isZero f.srcMod or isZero f.targMod

-- Apply a FreeOIModuleMap to a List of OITerms
applyFreeOIModuleMap = method(TypicalValue => VectorInWidth)
applyFreeOIModuleMap(FreeOIModuleMap, List) := (f, oiTerms) -> (
    if #oiTerms == 0 then error "cannot apply FreeOIModuleMap to an empty list";

    -- Handle the zero map case
    if isZero f then return 0_(getFreeModuleInWidth(f.targMod, (oiTerms#0).basisIndex.oiMap.targWidth));

    -- Generate the new terms
    newTerms := for oiTerm in oiTerms list (
        ringElement := oiTerm.ringElement;
        basisIndex := oiTerm.basisIndex;
        oiMap := basisIndex.oiMap;
        idx := basisIndex.idx;
        modMap := getInducedModuleMap(f.targMod, oiMap);
        ringElement * modMap f.genImages#(idx - 1) -- x*d_(pi,i) ---> x*F(pi)(b_i)
    );

    -- Sum the terms up
    sum newTerms
)

-- Apply a FreeOIModuleMap to a List of VectorInWidths
FreeOIModuleMap List := (f, L) -> for i to #L - 1 list f L#i

-- Apply a FreeOIModuleMap to a VectorInWidth
FreeOIModuleMap VectorInWidth := (f, v) -> (
    freeOIMod := freeOIModuleFromElement v;
    if not source f === freeOIMod then error "element does not belong to the required FreeOIModule";

    -- Handle the zero map and zero vector cases
    if isZero f or isZero v then return 0_(getFreeModuleInWidth(f.targMod, widthOfElement v));

    oiTerms := getOITermsFromVector v;
    applyFreeOIModuleMap(f, oiTerms)
)

-- Check if a FreeOIModuleMap is a graded map
isHomogeneous FreeOIModuleMap := f -> (
    if isZero f then return true;

    for elt in f.genImages do (
        degs := for t in terms elt list degree t;
        if not #(set degs) == 1 then return false
    );

    -f.srcMod.degShifts === flatten apply(f.genImages, degree)
)

-- Compute a remainder of a VectorInWidth modulo a List of VectorInWidths
-- Returns a HashTable of the form {quo => VectorInWidth, rem => VectorInWidth, triples => List} where triples is a List describing how the quotient is constructed
oiPolyDiv = method(TypicalValue => HashTable)
oiPolyDiv(VectorInWidth, List) := (f, L) -> (
    if #L == 0 then error "expected a nonempty List";

    if isZero f then return new HashTable from {quo => f, rem => f, triples => {}};

    -- Division algorithm
    quo0 := 0_(class f);
    rem0 := f;
    triples0 := new MutableList;
    tripIndex := 0;
    while true do (
        divisionOccurred := false;
        for i to #L - 1 do (
            elt := L#i;
            if isZero elt then continue;
            div := oiTermDiv(leadOITerm rem0, leadOITerm elt);
            if isZero div.quo then continue;

            modMap := getInducedModuleMap(freeOIModuleFromElement f, div.oiMap);
            q := modMap elt;
            quo0 = quo0 + div.quo * q;
            triples0#tripIndex = {div.quo, div.oiMap, i};
            tripIndex = tripIndex + 1;

            rem0 = rem0 - div.quo * q;

            if isZero rem0 then break;
            divisionOccurred = true;
            break
        );

        if not divisionOccurred then break
    );

    new HashTable from {rem => rem0, quo => quo0, triples => new List from triples0}
)

-- User-exposed division algorithm
VectorInWidth // List := (f, L) -> {(oiPolyDiv(f, L)).quo, (oiPolyDiv(f, L)).rem}

-- Compute the S-polynomial of two VectorInWidths
spoly = method(TypicalValue => VectorInWidth)
spoly(VectorInWidth, VectorInWidth) := (f, g) -> (
    Widthf := widthOfElement f;
    Widthg := widthOfElement g;
    if not Widthf == Widthg then error "vectors must belong to the same width";

    freeOIModf := freeOIModuleFromElement f;
    freeOIModg := freeOIModuleFromElement g;
    if not freeOIModf === freeOIModg then error "vectors must belong to the same FreeOIModule";
    freeMod := getFreeModuleInWidth(freeOIModf, Widthf);

    if isZero f or isZero g then return 0_freeMod;

    loitf := leadOITerm f;
    loitg := leadOITerm g;
    lcmfg := lcm(makeMonic loitf, makeMonic loitg);
    if isZero lcmfg then return 0_freeMod;
    (lcmfg.ringElement // loitf.ringElement)*f - (lcmfg.ringElement // loitg.ringElement)*g
)

-- Compute the critical pairs for a List L
-- Returns a List of Lists of the form {VectorInWidth, VectorInWidth, OIMap, OIMap, ZZ, ZZ}
-- The first two VectorInWidths are the actual OI-pair. Then the OI-maps used to make them, and the indices of the elements of L used
oiPairs = method(TypicalValue => List, Options => {Verbose => false})
oiPairs List := opts -> L -> (
    if #L == 0 then error "Expected a nonempty List";

    ret := new MutableList;
    l := 0;
    for fIdx to #L - 1 do (
        f := L#fIdx;
        if isZero f then continue;
        loitf := leadOITerm f;
        Widthf := widthOfElement f;
        for gIdx from fIdx to #L - 1 do (
            g := L#gIdx;
            if isZero g then continue;
            Widthg := widthOfElement g;
            loitg := leadOITerm g;
            if not loitf.basisIndex.idx == loitg.basisIndex.idx then continue; -- These will have lcm zero

            if opts.Verbose then print("Generating critical pairs for "|net f|" and "|net g);

            searchMin := max(Widthf, Widthg);
            searchMax := Widthf + Widthg;
            for i to searchMax - searchMin do (
                k := searchMax - i;
                oiMapsFromf := getOIMaps(Widthf, k);

                -- Given an OI-map from f, we construct the corresponding OI-maps from g
                for oiMapFromf in oiMapsFromf do (
                    base := set(1..k) - set oiMapFromf.img; -- Get the starting set

                    -- Now add back in the i-element subsets of oiMapFromf.img and make the pairs
                    for subset in subsets(oiMapFromf.img, i) do (
                        
                        oiMapFromg := makeOIMap(k, sort toList(base + set subset));
                        if not composeOIMaps(oiMapFromf, loitf.basisIndex.oiMap) === composeOIMaps(oiMapFromg, loitg.basisIndex.oiMap) then continue; -- These will have lcm zero
                        if opts.Verbose then print("Found OI-maps "|net oiMapFromf|" and "|net oiMapFromg);

                        modMapFromf := getInducedModuleMap(freeOIModuleFromElement f, oiMapFromf);
                        modMapFromg := getInducedModuleMap(freeOIModuleFromElement g, oiMapFromg);

                        candidate := {modMapFromf f, modMapFromg g, oiMapFromf, oiMapFromg, fIdx, gIdx};
                        if not member(candidate, toList ret) then ( ret#l = candidate; l = l + 1 ) -- Avoid duplicates
                    )
                )
            )
        )
    );

    toList ret
)

-- Cache for storing OI-Groebner bases
oiGBCache = new MutableHashTable

-- Compute a Groebner basis for a List of VectorInWidths
oiGB = method(TypicalValue => List, Options => {Verbose => false, Strategy => 1, MinimalOIGB => true})
oiGB List := opts -> L -> (
    if not (opts.Strategy === 1 or opts.Strategy === 2) then error "expected Strategy => 1 or Strategy => 2";

    -- Return the GB if it already exists
    if oiGBCache#?(L, opts.Strategy, opts.MinimalOIGB) then return oiGBCache#(L, opts.Strategy, opts.MinimalOIGB);

    if #L == 0 then error "expected a nonempty List";
    
    ret := new MutableList from L;
    encountered := new MutableList;
    addedTotal := 0;
    encIndex := 0;
    retIndex := #ret;
    
    -- Enter the main loop: terminates by an equivariant Noetherianity argument
    while true do (
        retTmp := toList ret;
        addedThisPhase := 0;

        oipairs := oiPairs(retTmp, Verbose => opts.Verbose);
        for i to #oipairs - 1 do (
            s := spoly(oipairs#i#0, oipairs#i#1);

            if isZero s or member(s, toList encountered) then continue; -- Skip zero and redundant S-polynomials
            encountered#encIndex = s;
            encIndex = encIndex + 1;

            if opts.Verbose then (
                print("On critical pair "|toString(i + 1)|" out of "|toString(#oipairs));
                if opts.Strategy === 2 then print("Elements added so far this phase: "|toString addedThisPhase);
                print("Elements added total: "|toString addedTotal)
            );

            rem := (oiPolyDiv(s, toList ret)).rem;
            if not isZero rem and not member(rem, toList ret) then (
                if opts.Verbose then print("Found nonzero remainder: "|net rem);
                ret#retIndex = rem;
                retIndex = retIndex + 1;

                addedThisPhase = addedThisPhase + 1;
                addedTotal = addedTotal + 1;

                if opts.Strategy === 1 then break
            )
        );

        if toList ret === retTmp then break -- No new elements were added so we're done by the OI-Buchberger's Criterion
    );

    -- Minimize the basis
    local finalRet;
    if opts.MinimalOIGB then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\n";
        finalRet = minimizeOIGB(toList ret, Verbose => opts.Verbose)
    ) else finalRet = toList ret;

    -- Store the basis
    oiGBCache#(L, opts.Strategy, opts.MinimalOIGB) = finalRet;

    finalRet
)

-- Minimize an OI-Groebner basis in the sense of lt(p) not in <lt(L - {p})> for all p in L
minimizeOIGB = method(TypicalValue => List, Options => {Verbose => false})
minimizeOIGB List := opts -> L -> (
    if opts.Verbose then print "Computing minimal OIGB...";

    currentBasis := L;
    while true do (
        redundantFound := false;

        for p in currentBasis do (
            minusp := toList((set currentBasis) - set {p});
            for elt in minusp do if not isZero (oiTermDiv(leadOITerm p, leadOITerm elt)).quo then (
                if opts.Verbose then print("Found redundant element: "|net p);
                redundantFound = true;
                currentBasis = minusp;
                break
            );

            if redundantFound then break
        );

        if not redundantFound then break
    );

    currentBasis
)

-- Check if a List is an OI-Groebner basis
isOIGB = method(TypicalValue => Boolean, Options => {Verbose => false})
isOIGB List := opts -> L -> (
    if #L == 0 then error "expected a nonempty List";

    encountered := new MutableList;
    encIndex := 0;
    oipairs := oiPairs(L, Verbose => opts.Verbose);
    for i to #oipairs - 1 do (
        if opts.Verbose then (
            print("On critical pair "|toString(i + 1)|" out of "|toString(#oipairs));
            print("Pair: "|net oipairs#i)
        );

        s := spoly(oipairs#i#0, oipairs#i#1);
        if isZero s or member(s, toList encountered) then continue;

        encountered#encIndex = s;
        encIndex = encIndex + 1;
        rem := (oiPolyDiv(s, L)).rem;
        if not isZero rem then (if opts.Verbose then print("Found nonzero remainder: "|net rem); return false) -- If L were a GB, then every element would have a unique remainder of zero
    );
    
    true
)

-- Cache for storing Groebner bases computed with oiSyz
oiSyzCache = new MutableHashTable

-- Compute an OI-Groebner basis for the syzygy module of a List of VectorInWidths
oiSyz = method(TypicalValue => List, Options => {Verbose => false, MinimalOIGB => true})
oiSyz(List, Symbol) := opts -> (L, d) -> (
    if #L == 0 then error "Expected a nonempty list";
    
    -- Return the GB if it already exists
    if oiSyzCache#?(L, d, opts.MinimalOIGB) then return oiSyzCache#(L, d, opts.MinimalOIGB);
    
    freeOIMod := freeOIModuleFromElement L#0;
    shifts := for elt in L list -degree elt;
    widths := for elt in L list widthOfElement elt;
    G := makeFreeOIModule(freeOIMod.polyOIAlg, d, widths, DegreeShifts => flatten shifts, MonomialOrder => L);

    ret := new MutableList;
    retIndex := 0;
    oipairs := oiPairs(L, Verbose => opts.Verbose);
    if opts.Verbose then print "Iterating through critical pairs...";
    i := 0;
    for pair in oipairs do (
        loitf := leadOITerm pair#0;
        loitg := leadOITerm pair#1;
        lcmfg := lcm(makeMonic loitf, makeMonic loitg);
        if isZero lcmfg then continue; -- This will yield a trivial syzygy

        if opts.Verbose then ( print("On critical pair "|toString(i + 1)|" out of "|toString(#oipairs)); i = i + 1 );
        if opts.Verbose then print("Pair: "|net pair);

        s := spoly(pair#0, pair#1);
        sWidth := widthOfElement s;
        freeMod := getFreeModuleInWidth(G, sWidth);
        thingToSubtract := 0_freeMod;
        if not isZero s then (
            sdiv := oiPolyDiv(s, L);

            -- Calculate the stuff to subtract off
            for triple in sdiv.triples do (
                b := makeBasisElement makeBasisIndex(G, triple#1, 1 + triple#2);
                thingToSubtract = thingToSubtract + triple#0 * getVectorFromOITerms {b}
            )
        );

        bSigma := makeBasisElement makeBasisIndex(G, pair#2, 1 + pair#4);
        bTau := makeBasisElement makeBasisIndex(G, pair#3, 1 + pair#5);

        -- Make the syzygy
        syzygy := (lcmfg.ringElement // loitf.ringElement) * getVectorFromOITerms {bSigma} - (lcmfg.ringElement // loitg.ringElement) * getVectorFromOITerms {bTau} - thingToSubtract;
        
        if isZero syzygy then continue; -- Skip trivial syzygies

        ret#retIndex = syzygy;
        if opts.Verbose then print("Generated syzygy: "|net ret#retIndex);
        retIndex = retIndex + 1
    );

    -- Minimize the basis
    local finalRet;
    if opts.MinimalOIGB then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\n";
        finalRet = minimizeOIGB(toList ret, Verbose => opts.Verbose)
    ) else finalRet = toList ret; 

    -- Store the GB
    oiSyzCache#(L, d, opts.MinimalOIGB) = finalRet;

    finalRet
)

-- Cache for storing OI-resolutions computed with oiRes
oiResCache = new MutableHashTable

-- Should be of the form {dd => List, modules => List}
OIResolution = new Type of HashTable

net OIResolution := C -> (
    n := "0: "|toString C.modules#0;
    for i from 1 to #C.modules - 1 do n = n || toString i | ": "|toString C.modules#i;
    n
)

describe OIResolution := C -> (
    n := "0: Module: "|net C.modules#0||"Differential: "|net C.dd#0;
    for i from 1 to #C.modules - 1 do n = n || toString i | ": Module: "|net C.modules#i||"Differential: "|net C.dd#i;
    n
)

OIResolution _ ZZ := (C, n) -> C.modules#n

sremove := (L, i) -> drop(L, {i, i})

-- Compute an OIResolution of length n for the OI-module generated by the elements of L
oiRes = method(TypicalValue => OIResolution, Options => {FastNonminimal => false, Verbose => false, MinimalOIGB => true})
oiRes(List, ZZ) := opts -> (L, n) -> (
    if n < 0 then error "expected a nonnegative integer";
    if #L == 0 then error "expected a nonempty List";

    -- Return the resolution if it already exists
    if oiResCache#?(L, n, opts.FastNonminimal, opts.MinimalOIGB) then return oiResCache#(L, n, opts.FastNonminimal, opts.MinimalOIGB);

    ddMut := new MutableList;
    modulesMut := new MutableList;

    -- Make the initial resolution
    initialFreeOIMod := freeOIModuleFromElement L#0;
    e := initialFreeOIMod.basisSym;

    if opts.Verbose then print "Computing an OI-Groebner basis...";
    oigb := oiGB(L, Verbose => opts.Verbose, MinimalOIGB => opts.MinimalOIGB);
    currentGB := oigb;
    currentSymbol := getSymbol concatenate(e, "0");
    count := 0;

    if n > 0 then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\nComputing syzygies...";
        for i to n - 1 do (
            syzGens := oiSyz(currentGB, currentSymbol, Verbose => opts.Verbose, MinimalOIGB => opts.MinimalOIGB);

            if #syzGens == 0 then break;

            targFreeOIMod := freeOIModuleFromElement currentGB#0;
            srcFreeOIMod := freeOIModuleFromElement syzGens#0;

            modulesMut#i = srcFreeOIMod;
            ddMut#i = makeFreeOIModuleMap(targFreeOIMod, srcFreeOIMod, getMonomialOrder srcFreeOIMod);

            count = i + 1;
            currentGB = syzGens;
            currentSymbol = getSymbol concatenate(e, toString count)
        )
    );

    -- Append the last term in the sequence
    shifts := for elt in currentGB list -degree elt;
    widths := for elt in currentGB list widthOfElement elt;
    modulesMut#count = makeFreeOIModule(initialFreeOIMod.polyOIAlg, currentSymbol, widths, DegreeShifts => flatten shifts, MonomialOrder => currentGB);
    targFreeOIMod := if count == 0 then initialFreeOIMod else modulesMut#(count - 1);
    ddMut#count = makeFreeOIModuleMap(targFreeOIMod, modulesMut#count, currentGB);

    -- Cap the sequence with zero
    if count < n then (
        currentSymbol = getSymbol concatenate(e, toString(count + 1));
        modulesMut#(count + 1) = makeFreeOIModule(initialFreeOIMod.polyOIAlg, currentSymbol, {});
        ddMut#(count + 1) = makeFreeOIModuleMap(modulesMut#count, modulesMut#(count + 1), {})
    );

    -- Minimize the resolution
    if not opts.FastNonminimal and #ddMut > 1 and not isZero ddMut#1 then (
        if opts.Verbose then print "----------------------------------------\n----------------------------------------\nMinimizing resolution...";
        done := false;
        while not done do (
            done = true;

            -- Look for units on identity basis elements
            unitFound := false;
            local data;
            for i from 1 to #ddMut - 1 do (
                ddMap := ddMut#i;
                if isZero ddMap then continue; -- Skip any zero maps
                srcMod := source ddMap;
                targMod := target ddMap;
                for j to #ddMap.genImages - 1 do (
                    if isZero ddMap.genImages#j then continue;
                    oiTerms := getOITermsFromVector(ddMap.genImages#j, CombineLikeTerms => true);
                    for term in oiTerms do (
                        b := term.basisIndex;
                        if b.oiMap.img === toList(1..b.oiMap.targWidth) and isUnit term.ringElement then (
                            unitFound = true;
                            done = false;
                            data = {i, j, term};
                            if opts.Verbose then print("Unit found on term: "|net term);
                            break
                        );
                        if unitFound then break
                    );
                    if unitFound then break
                );
                if unitFound then break
            );

            -- Prune the sequence
            if unitFound then (
                term := data#2;
                targBasisPos := term.basisIndex.idx - 1;
                srcBasisPos := data#1;
                ddMap := ddMut#(data#0);
                srcMod := source ddMap;
                targMod := target ddMap;

                if opts.Verbose then print "Pruning...";

                newSrcWidths := sremove(srcMod.genWidths, srcBasisPos);
                newSrcShifts := sremove(srcMod.degShifts, srcBasisPos);
                newTargWidths := sremove(targMod.genWidths, targBasisPos);
                newTargShifts := sremove(targMod.degShifts, targBasisPos);

                -- Make the new modules
                newSrcMod := makeFreeOIModule(srcMod.polyOIAlg, srcMod.basisSym, newSrcWidths, DegreeShifts => newSrcShifts);
                newTargMod := makeFreeOIModule(targMod.polyOIAlg, targMod.basisSym, newTargWidths, DegreeShifts => newTargShifts);

                -- Compute the new differential
                newGenImages := new List;
                if not (isZero newSrcMod or isZero newTargMod) then (
                    targBasisOIMap := makeOIMap(targMod.genWidths#targBasisPos, toList(1..targMod.genWidths#targBasisPos));
                    srcBasisOIMap := makeOIMap(srcMod.genWidths#srcBasisPos, toList(1..srcMod.genWidths#srcBasisPos));
                    newGenImages = for i to #srcMod.genWidths - 1 list (
                        if i == srcBasisPos then continue;
                        stuff := 0_(getFreeModuleInWidth(srcMod, srcMod.genWidths#i));
                        oiMaps := getOIMaps(targMod.genWidths#targBasisPos, srcMod.genWidths#i);

                        -- Calculate the stuff to subtract off
                        if #oiMaps > 0 and not isZero ddMap.genImages#i then (
                            oiTerms := getOITermsFromVector(ddMap.genImages#i, CombineLikeTerms => true);
                            for term in oiTerms do (
                                b := term.basisIndex;
                                if not b.idx == targBasisPos + 1 then continue;

                                local oiMap;
                                for oimap in oiMaps do if b.oiMap === composeOIMaps(oimap, targBasisOIMap) then ( oiMap = oimap; break );

                                modMap := getInducedModuleMap(srcMod, oiMap);
                                basisElt := getVectorFromOITerms {makeBasisElement makeBasisIndex(srcMod, srcBasisOIMap, srcBasisPos + 1)};
                                stuff = stuff + term.ringElement * modMap basisElt
                            )
                        );

                        -- Calculate the new image
                        basisElt := getVectorFromOITerms {makeBasisElement makeBasisIndex(srcMod, makeOIMap(srcMod.genWidths#i, toList(1..srcMod.genWidths#i)), i + 1)};
                        newGenImage0 := ddMap(basisElt - lift(1 // term.ringElement, srcMod.polyOIAlg.baseField) * stuff);
                        newGenImage := 0_(getFreeModuleInWidth(newTargMod, widthOfElement newGenImage0));
                        if not isZero newGenImage0 then (
                            newOITerms := getOITermsFromVector(newGenImage0, CombineLikeTerms => true);
                            for newTerm in newOITerms do (
                                idx := newTerm.basisIndex.idx;
                                if idx > targBasisPos + 1 then idx = idx - 1; -- Relabel
                                newGenImage = newGenImage + getVectorFromOITerms {makeOITerm(newTerm.ringElement, makeBasisIndex(newTargMod, newTerm.basisIndex.oiMap, idx))}
                            )
                        );

                        newGenImage
                    )
                );

                ddMut#(data#0) = makeFreeOIModuleMap(newTargMod, newSrcMod, newGenImages);
                modulesMut#(data#0) = newSrcMod;
                modulesMut#(data#0 - 1) = newTargMod;

                -- Adjust the adjactent differentials
                -- Below map
                ddMap = ddMut#(data#0 - 1);
                ddMut#(data#0 - 1) = makeFreeOIModuleMap(target ddMap, newTargMod, sremove(ddMap.genImages, targBasisPos));

                -- Above map
                if data#0 < #ddMut - 1 then (
                    newGenImages = new MutableList;
                    ddMap = ddMut#(1 + data#0);
                    srcMod = source ddMap;
                    targMod = target ddMap;

                    if not (isZero srcMod or isZero targMod) then (
                        for i to #ddMap.genImages - 1 do (
                            if isZero ddMap.genImages#i then newGenImages#i = ddMap.genImages#i else (
                                oiTerms := getOITermsFromVector ddMap.genImages#i;
                                newTerms := for term in oiTerms list (
                                    idx := term.basisIndex.idx;
                                    if idx == srcBasisPos + 1 then continue; -- Projection
                                    if idx > srcBasisPos + 1 then idx = idx - 1; -- Relabel
                                    makeOITerm(term.ringElement, makeBasisIndex(newSrcMod, term.basisIndex.oiMap, idx))
                                );

                                newGenImages#i = getVectorFromOITerms newTerms
                            )
                        )
                    );

                    ddMut#(1 + data#0) = makeFreeOIModuleMap(newSrcMod, srcMod, new List from newGenImages)
                )
            )
        )
    );

    ret := new OIResolution from {dd => new List from ddMut, modules => new List from modulesMut};

    -- Store the resolution
    oiResCache#(L, n, opts.FastNonminimal, opts.MinimalOIGB) = ret;

    ret
)

-- Verify that an OIResolution is a complex
isComplex = method(TypicalValue => Boolean)
isComplex OIResolution := C -> (
    if #C.dd < 2 then error "expected a sequence with at least two maps";

    -- Check if the maps compose to zero
    for i from 1 to #C.dd - 1 do (
        modMap0 := C.dd#(i - 1);
        modMap1 := C.dd#i;
        if isZero modMap0 or isZero modMap1 then continue;
        srcMod := source modMap1;
        basisElts := for i to #srcMod.genWidths - 1 list makeBasisElement makeBasisIndex(srcMod, makeOIMap(srcMod.genWidths#i, toList(1..srcMod.genWidths#i)), i + 1);
        for basisElt in basisElts do (
            result := modMap0 modMap1 getVectorFromOITerms {basisElt};
            print net result;
            if not isZero result then return false
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

-- Test 0: Compute a Groebner basis
TEST ///
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1});
installBasisElements(F, 1);
installBasisElements(F, 2);
installBasisElements(F, 3);

use F_1; f = x_(1,1)^3*e_(1,{1}, 1);
use F_2; h = x_(1,2)^2*e_(2, {2}, 1) + x_(1,1)*x_(1,2)*e_(2, {2}, 1);
B = oiGB {f, h};

use F_2; elt1 = x_(1,2)*x_(1,1)^2*e_(2,{2},1);
use F_3; elt2 = (-x_(1,3)*x_(1,2)+x_(1,3)*x_(1,1))*e_(3,{3},1);

checkB = apply(B, makeMonic);
checkList = apply({f, h, elt1, elt2}, makeMonic);
assert(sort checkB == sort checkList)
///

-- Test 1: Compute first syzygies
TEST ///
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1});
installBasisElements(F, 1);
installBasisElements(F, 2);

use F_1; b1 = x_(1,1)^3*e_(1,{1},1);
use F_2; b2 = x_(1,1)^2*e_(2,{1},1); b3 = x_(1,2)^2*e_(2,{2},1); b4 = x_(1,1)*x_(1,2)*e_(2,{2},1);
B = oiGB {b1, b2, b3, b4};
C = oiSyz(B, d);
assert isOIGB C;

G = freeOIModuleFromElement C#0;
installBasisElements(G, 2);
installBasisElements(G, 3);

use G_2;
width2stuff = {
d_(2,{1},1)-x_(1,1)*d_(2,{1,2},2),
x_(1,1)*d_(2,{1,2},3)-x_(1,2)*d_(2,{1,2},4),
d_(2,{2},1)-x_(1,2)*d_(2,{1,2},3)
};

use G_3;
width3stuff = {
d_(3,{1,3},2)-d_(3,{1,2},2),
d_(3,{2,3},2)-d_(3,{1,2},3),
x_(1,1)*d_(3,{2,3},3)-x_(1,3)*d_(3,{1,3},4),
-d_(3,{2,3},3)+d_(3,{1,3},3),
x_(1,2)*d_(3,{1,3},4)-x_(1,1)*d_(3,{2,3},4)
};

checkC = apply(C, makeMonic);
checkList = apply(join(width2stuff, width3stuff), makeMonic);
assert(sort checkC == sort checkList)
///

-- Test 2: Compute length 2 resolution
TEST ///
P = makePolynomialOIAlgebra(QQ,1,x);
F = makeFreeOIModule(P, e, {1});
installBasisElements(F, 1);
installBasisElements(F, 2);
use F_1; b1 = x_(1,1)*e_(1,{1},1); b2 = x_(1,1)^2*e_(1,{1},1);
use F_2; b3 = x_(1,2)*e_(2,{1},1);
C = oiRes({b1, b2, b3}, 2, MinimalOIGB => false);
assert isComplex C;
assert(sort getGenWidths C_1 == sort {2,3});
assert(sort getDegShifts C_1 == sort {-2,-2});
assert(sort getGenWidths C_2 == sort {3, 3, 3, 3, 4, 4, 4, 4, 4, 4});
assert(sort getDegShifts C_2 == sort {-3, -3, -4, -4, -3, -3, -3, -3, -3, -3})
///

end

restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 3);
b = (x_(1,1)*x_(2,3)-x_(1,3)*x_(2,1))*e_(3,{},1);
time C = oiRes({b}, 3, Verbose => true)

restart
load "OIGroebnerBases.m2"
P = makePolynomialOIAlgebra(QQ,2,x);
F = makeFreeOIModule(P, e, {0});
installBasisElements(F, 2);
b = (x_(1,1)*x_(2,2)-x_(1,2)*x_(2,1))*e_(2,{},1);
time D = oiRes({b}, 3, Verbose => true)