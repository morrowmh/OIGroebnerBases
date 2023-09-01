-- Should be of the form {map0 => OIMap, vec0 => VectorInWidth, im0 => VectorInWidth, map1 => OIMap, vec1 => VectorInWidth, im1 => VectorInWidth}
OIPair = new Type of HashTable

-- Comparison method for OIPair objects
OIPair ? OIPair := (p, q) -> getWidth p.im0 ? getWidth q.im0

-- Compute the critical pairs for a List of VectorInWidth objects
-- Args: L = List, V = Boolean
-- Comment: map0 and map1 are the OI-maps applied to vec0 and vec1 to make im0 and im1
oiPairs := (L, V) -> sort unique flatten flatten flatten flatten for fIdx to #L - 1 list (
    f := L#fIdx;
    ltf := keyedLeadTerm f;
    for gIdx from fIdx to #L - 1 list (
        g := L#gIdx;
        ltg := keyedLeadTerm g;
        clsf := class f;
        clsg := class g;

        if not ltf.key#1 === ltg.key#1 then continue; -- These will have lcm zero

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

                    if not oiMapFromf ltf.key#0 === oiMapFromg ltg.key#0 then continue; -- These will have lcm zero
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