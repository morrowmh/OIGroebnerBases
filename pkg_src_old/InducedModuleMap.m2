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