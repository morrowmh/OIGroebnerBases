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