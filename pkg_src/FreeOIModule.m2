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
    str = str|net oiTerms#-1;
    str
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
    F.modules#n = ret;

    ret
)

-- Shorthand for getFreeModuleInWidth
FreeOIModule _ ZZ := (F, n) -> getFreeModuleInWidth(F, n)

-- Use a ModuleInWidth
use ModuleInWidth := M -> use getAlgebraInWidth(M.freeOIMod.polyOIAlg, M.Width)

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