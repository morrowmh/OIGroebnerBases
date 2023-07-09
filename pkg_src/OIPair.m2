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