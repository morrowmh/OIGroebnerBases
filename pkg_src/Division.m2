-- Division function for KeyedVectorInWidth terms
-- Args: v = KeyedVectorInWidth, w = KeyedVectorInWidth
-- Comment: tries to divide v by w and returns a HashTable of the form {quo => RingElement, oiMap => OIMap}
termDiv := (v, w) -> (
    clsv := class v.vec;
    clsw := class w.vec;
    fmod := clsv.freeOIMod;

    if isZero v.vec then return hashTable {quo => 0_(getAlgebraInWidth(fmod.polyOIAlg, clsv.wid)), oiMap => null};

    widv := clsv.wid;
    widw := clsw.wid;
    keyv := v.key;
    keyw := w.key;

    if widv === widw then (
        if keyv === keyw and zero(v.vec#keyv % w.vec#keyw) then
            return hashTable {quo => v.vec#keyv // w.vec#keyw, oiMap => (getOIMaps(widw, widv))#0}
    )
    else for oiMap0 in getOIMaps(widw, widv) do (
        modMap := getInducedModuleMap(fmod, oiMap0);
        imgw := modMap w.vec;
        keyimgw := (oiMap0 w.key#0, w.key#1);

        if keyv === keyimgw and zero(v.vec#keyv % imgw#keyimgw) then
            return hashTable {quo => v.vec#keyv // imgw#keyimgw, oiMap => oiMap0}
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
            div := termDiv(keyedLeadTerm rem0, keyedLeadTerm elt);
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

-- Compute the normal form of a VectorInWidth modulo a List of VectorInWidth objects
-- Args: v = VectorInWidth, L = List
-- Comment: expects L to consist of nonzero elements
oiNormalForm := (v, L) -> (
    if isZero v then return v;

    cls := class v;
    rem := makeZero cls;

    while not isZero v do (
        divisionOccurred := false;

        for elt in L do (
            div := termDiv(keyedLeadTerm v, keyedLeadTerm elt);
            if zero div.quo then continue;

            modMap := getInducedModuleMap(cls.freeOIMod, div.oiMap);
            v = v - div.quo * modMap elt;

            divisionOccurred = true;
            break
        );

        if not divisionOccurred then (
            rem = rem + leadTerm v;
            v = v - leadTerm v
        )
    );

    rem
)