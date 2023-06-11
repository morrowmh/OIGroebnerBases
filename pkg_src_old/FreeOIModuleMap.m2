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