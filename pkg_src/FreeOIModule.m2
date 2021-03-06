-- Define the new type FreeOIModule
-- COMMENT: Should be of the form {polyOIAlg => PolynomialOIAlgebra, basisSym => Symbol, genWidths => List, degShifts => List, monOrder => MutableList, modules => MutableHashTable, maps => MutableHashTable}
FreeOIModule = new Type of HashTable

toString FreeOIModule := F -> "generator widths => "|toString F.genWidths |", degree shifts => "|toString F.degShifts

net FreeOIModule := F -> (
    local monOrderNet;
    if F.monOrder#0 === Lex then monOrderNet = net Lex;
    if instance(F.monOrder#0, FreeOIModuleMap) then monOrderNet = "Schreyer monomial order via the FreeOIModuleMap: "|net F.monOrder#0;
    "Polynomial OI-algebra: "|toString F.polyOIAlg ||
    "Basis symbol: "|net F.basisSym ||
    "Generator widths: "|net F.genWidths ||
    "Degree shifts: "|net F.degShifts ||
    "Monomial order: "|monOrderNet
)

-- PURPOSE: Get the generator widths of a FreeOIModule
-- INPUT: A FreeOIModule 'F'
-- OUTPUT: F.genWidths
getGenWidths = method(TypicalValue => List)
getGenWidths FreeOIModule := F -> F.genWidths

-- PURPOSE: Get the degree shifts of a FreeOIModule
-- INPUT: A FreeOIModule 'F'
-- OUTPUT: F.degShifts
getDegShifts = method(TypicalValue => List)
getDegShifts FreeOIModule := F -> F.degShifts

-- PURPOSE: Make a new FreeOIModule
-- INPUT: '(P, e, W)', a PolynomialOIAlgebra 'P', a Symbol 'e' and a List of generator widths 'W'
-- OUTPUT: A FreeOIModule made from P, e and W
-- COMMENT: Default monomial order is Lex
-- COMMENT: Default degree shifts are all zero
makeFreeOIModule = method(TypicalValue => FreeOIModule, Options => {DegreeShifts => null})
makeFreeOIModule(PolynomialOIAlgebra, Symbol, List) := opts -> (P, e, W) -> (
    local shifts;
    if opts.DegreeShifts === null then shifts = #W : 0
    else if instance(opts.DegreeShifts, List) then shifts = opts.DegreeShifts
    else error("Invalid DegreeShifts option: "|toString opts.DegreeShifts);

    new FreeOIModule from {
        polyOIAlg => P,
        basisSym => e,
        genWidths => W,
        degShifts => toList shifts,
        monOrder => new MutableList from {Lex},
        modules => new MutableHashTable,
        maps => new MutableHashTable}
)

-- PURPOSE: Get the monomial order from a FreeOIModule
-- INPUT: A FreeOIModule 'F'
-- OUTPUT: The monomial order on F
-- COMMENT: Returns either Lex or a FreeOIModuleMap
getMonomialOrder = method()
getMonomialOrder FreeOIModule := F -> F.monOrder#0

-- Define the new type ModuleInWidth
-- COMMENT: Should also contain the key-value pairs freeOIMod => FreeOIModule, Width => ZZ and basisElements => List
-- COMMENT: The order of basisElements matters, i.e. given a module M, basisElements#i should correspond to M_i
ModuleInWidth = new Type of Module

net ModuleInWidth := M -> (
    rawMod := new Module from M;
    net rawMod | " in width " | net rawMod.Width
)

-- Define the new type VectorInWidth
-- COMMENT: An instance f should have class f === (corresponding ModuleInWidth)
VectorInWidth = new Type of Vector

-- PURPOSE: Check if a VectorInWidth is zero
-- INPUT: A VectorInWidth 'f'
-- OUTPUT: true if f is zero, false otherwise
isZero = method(TypicalValue => Boolean)
isZero VectorInWidth := f -> f == 0_(class f)

load "FreeOIModuleMap.m2"

-- PURPOSE: Install a Schreyer monomial order on a FreeOIModule
-- INPUT: A FreeOIModuleMap 'f'
-- OUTPUT: Sets f.srcMod.monOrder#0 to f
installSchreyerMonomialOrder = method()
installSchreyerMonomialOrder FreeOIModuleMap := f -> f.srcMod.monOrder#0 = f

net VectorInWidth := f -> (
    if isZero f then return net 0;
    oiTerms := getOITermsFromVector(f, Combine => true);
    if #oiTerms == 0 then return net 0;
    if #oiTerms == 1 then return net oiTerms#0;
    
    str := "";
    for i to #oiTerms - 2 do str = str|net oiTerms#i|" + ";
    str = str|net oiTerms#-1;
    str
)

-- Comparison method for VectorInWidth
-- COMMENT: Compares vectors by looking at their lead terms
VectorInWidth ? VectorInWidth := (f, g) -> leadOITerm f ? leadOITerm g

-- PURPOSE: Make a List of VectorInWidths monic
-- INPUT: A List 'L'
-- OUTPUT: A List containing f // leadCoefficient f for all f in L
makeMonic = method(TypicalValue => List)
makeMonic List := L -> (
    ret := new MutableList;

    for i to #L - 1 do (
        f := L#i;
        if isZero f then (ret#i = f; continue);
        oiterms := getOITermsFromVector f;
        lotf := leadOITerm f;
        lcf := leadCoefficient lotf.ringElement;
        newTerms := for oiterm in oiterms list makeOITerm(oiterm.ringElement // lcf, oiterm.basisIndex);
        ret#i = getVectorFromOITerms newTerms
    );

    new List from ret
)

load "Terms.m2"

-- PURPOSE: Check if a FreeOIModule is zero
-- INPUT: A FreeOIModule 'F'
-- OUTPUT: true if #F.genWidths = 0, false otherwise
isZero FreeOIModule := F -> #F.genWidths == 0

-- PURPOSE: Get the free module from a FreeOIModule in a specified width
-- INPUT: '(F, n)', a FreeOIModule 'F' and a width 'n'
-- OUTPUT: F_n, the width n free module of F
getFreeModuleInWidth = method(TypicalValue => ModuleInWidth)
getFreeModuleInWidth(FreeOIModule, ZZ) := (F, n) -> (
    -- Return the module if it already exists
    if F.modules#?n then return F.modules#n;

    -- Generate the degrees
    alg := getAlgebraInWidth(F.polyOIAlg, n);
    degList := new MutableList;
    for i to #F.genWidths - 1 do degList#i = binomial(n, F.genWidths#i) : F.degShifts#i;

    -- Make the module
    retHash := new MutableHashTable from alg^(toList degList);
    retHash.Width = n;
    retHash.freeOIMod = F;
    
    -- Generate the basis elements
    k := 0;
    mutableBasisElements := new MutableList;
    for i to #F.genWidths - 1 do (
        oiMaps := getOIMaps(F.genWidths#i, n);
        for oiMap in oiMaps do ( mutableBasisElements#k = makeBasisElement makeBasisIndex(F, oiMap, i + 1); k = k + 1 )
    );
    retHash.basisElements = toList mutableBasisElements;

    ret := new ModuleInWidth of VectorInWidth from retHash;

    -- Store the module
    F.modules#n = ret;

    ret
)

-- Subscript version of getFreeModuleInWidth
FreeOIModule _ ZZ := (F, n) -> (
    freeMod := getFreeModuleInWidth(F, n);
    use ring freeMod;
    freeMod
)

-- PURPOSE: Install a basis element for user input
-- OUTPUT: Sets the desired IndexedVariable to the appropriate basis vector
installBasisElement = method()

-- INPUT: '(F, f, i)', a FreeOIModule 'F', an OIMap 'f' and an index 'i'
installBasisElement(FreeOIModule, OIMap, ZZ) := (F, f, i) -> (
    basisElement := makeBasisElement makeBasisIndex(F, f, i);
    installBasisElement basisElement
)

-- INPUT: An basis element OITerm 'b'
installBasisElement OITerm := b -> (
    freeOIMod := b.basisIndex.freeOIMod;
    Width := b.basisIndex.oiMap.Width;
    fmod := getFreeModuleInWidth(freeOIMod, Width);
    pos := position(fmod.basisElements, elt -> elt === b);

    if pos === null then error "Specified basis element does not exist";
    freeOIMod.basisSym_(Width, b.basisIndex.oiMap.assignment, b.basisIndex.idx) <- fmod_pos;
)

-- PURPOSE: Install all basis elements in a specified width
-- INPUT: '(F, n)', a FreeOIModule 'F' and a width 'n'
-- OUTPUT: Calls every installBasisElement() in F_n
-- COMMENT: This method is very slow for large n
installBasisElements = method()
installBasisElements(FreeOIModule, ZZ) := (F, n) ->
    for i to #F.genWidths - 1 do (
        oiMaps := getOIMaps(F.genWidths#i, n);
        for oiMap in oiMaps do installBasisElement(F, oiMap, i + 1)
    )

-- PURPOSE: Get the width of an element
-- INPUT: A Vector 'f'
-- OUTPUT: The width of f
widthOfElement = method(TypicalValue => ZZ)
widthOfElement VectorInWidth := f -> (class f).Width

-- PURPOSE: Get the FreeOIModule containing an element
-- INPUT: A Vector 'f'
-- OUTPUT: The FreeOIModule containing f
freeOIModuleFromElement = method(TypicalValue => FreeOIModule)
freeOIModuleFromElement VectorInWidth := f -> (class f).freeOIMod

freeOIModuleFromElement Vector := f -> (
    if not (class f).?freeOIMod then error "Element does not belong to a FreeOIModule";
    freeOIModuleFromElement f
)