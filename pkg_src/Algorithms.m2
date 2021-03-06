-- PURPOSE: Compute a remainder of a VectorInWidth modulo a List of VectorInWidths
-- INPUT: '(f, L)', a VectorInWidth 'f' and a List 'L'
-- OUTPUT: A HashTable of the form {quo => VectorInWidth, rem => VectorInWidth, triples => List} where triples is a List describing how the quotient is constructed
-- COMMENT: If the elements of L are {l0, l1, l2} then triples may look like {{ringElt1, oiMap1, 0}, {ringElt2, oiMap2, 2}} and quo = ringElt1*F(oiMap1)(l0)+ringElt2*F(oiMap2)(l2)
-- COMMENT: "Verbose => true" will print more information
oiPolyDiv = method(TypicalValue => HashTable, Options => {Verbose => false})
oiPolyDiv(VectorInWidth, List) := opts -> (f, L) -> (
    if #L == 0 then error "Expected nonempty List";

    if opts.Verbose then print("Dividing "|net f|" by "|net L);

    if isZero f then return new HashTable from {quo => f, rem => f, triples => {}};

    local retTmp;
    retTmp = new MutableHashTable from {quo => 0_(class f), rem => f, triples => new MutableList};
    tripIndex := 0;
    while true do (
        divisionOccurred := false;
        for elt in L do (
            div := oiTermDiv(retTmp.rem, elt);
            if isZero div.quo then continue;

            moduleMap := getInducedModuleMap(freeOIModuleFromElement f, div.oiMap);
            q := moduleMap elt;
            retTmp.quo = retTmp.quo + div.quo * q;
            retTmp.triples#tripIndex = {div.quo, div.oiMap, position(L, l -> l === elt)};
            tripIndex = tripIndex + 1;

            retTmp.rem = retTmp.rem - div.quo * q;

            if isZero retTmp.rem then return new HashTable from {rem => retTmp.rem, quo => retTmp.quo, triples => new List from retTmp.triples};
            divisionOccurred = true;
            break
        );

        if not divisionOccurred then break
    );

    retTmp
)

-- PURPOSE: Compute the S-polynomial of two VectorInWidths
-- INPUT: '(f, g)', a VectorInWidth 'f' and a VectorInWidth 'g'
-- OUTPUT: The S-polynomial S(f, g)
-- COMMENT: Returns zero if either f or g is zero or if lcm(leadOITerm f, leadOITerm g) is zero
spoly = method(TypicalValue => VectorInWidth)
spoly(VectorInWidth, VectorInWidth) := (f, g) -> (
    Widthf := widthOfElement f;
    Widthg := widthOfElement g;
    if not Widthf == Widthg then error "Vectors must belong to the same width";

    freeOIModf := freeOIModuleFromElement f;
    freeOIModg := freeOIModuleFromElement g;
    if not freeOIModf === freeOIModg then error "Vectors must belong to the same free OI-module";
    freeMod := getFreeModuleInWidth(freeOIModf, Widthf);

    if isZero f or isZero g then return 0_freeMod;

    lotf := leadOITerm f;
    lotg := leadOITerm g;
    lcmfg := lcm(lotf, lotg);
    if isZero lcmfg then return 0_freeMod;
    (lcmfg.ringElement // lotf.ringElement)*f - (lcmfg.ringElement // lotg.ringElement)*g
)

-- PURPOSE: Generate a List of OI-pairs from a List of VectorInWidths
-- INPUT: A List 'L'
-- OUTPUT: A List of Lists of the form {VectorInWidth, VectorInWidth, OIMap, OIMap, ZZ, ZZ}
-- COMMENT: The first two VectorInWidths are the actual OI-pair. Then the OI-maps used to make them, and the indices of the elements of L used
-- COMMENT: "Verbose => true" will print more information
oiPairs = method(TypicalValue => List, Options => {Verbose => false})
oiPairs List := opts -> L -> (
    if #L == 0 then error "Expected a nonempty List";

    ret := new MutableList;
    l := 0;
    for fIdx to #L - 1 do (
        f := L#fIdx;
        lotf := leadOITerm f;
        Widthf := widthOfElement f;
        for gIdx from fIdx to #L - 1 do (
            g := L#gIdx;
            Widthg := widthOfElement g;
            lotg := leadOITerm g;
            if not lotf.basisIndex.idx == lotg.basisIndex.idx then continue; -- These will have lcm zero

            if opts.Verbose then print("Generating pairs for "|net f|" and "|net g);

            searchMin := max(Widthf, Widthg);
            searchMax := Widthf + Widthg;
            for i to searchMax - searchMin do (
                k := searchMax - i;
                oiMapsFromf := getOIMaps(Widthf, k);

                -- Given an OI-map out of f, we construct the corresponding OI-maps out of g
                for oiMapFromf in oiMapsFromf do (
                    base := set(1..k) - set oiMapFromf.assignment; -- Get the starting set

                    -- Now add back in the i-element subsets of oiMapFromf.assignment and make the pairs
                    for subset in subsets(oiMapFromf.assignment, i) do (
                        oiMapFromg := makeOIMap(k, sort toList(base + set subset));
                        if not composeOIMaps(oiMapFromf, lotf.basisIndex.oiMap) === composeOIMaps(oiMapFromg, lotg.basisIndex.oiMap) then continue; -- These will have lcm zero

                        if opts.Verbose then print("Found OI-maps "|net oiMapFromf|" and "|net oiMapFromg);

                        moduleMapFromf := getInducedModuleMap(freeOIModuleFromElement f, oiMapFromf);
                        moduleMapFromg := getInducedModuleMap(freeOIModuleFromElement g, oiMapFromg);

                        candidate := {moduleMapFromf f, moduleMapFromg g, oiMapFromf, oiMapFromg, fIdx, gIdx};
                        if not member(candidate, toList ret) then (ret#l = candidate; l = l + 1) -- Avoid duplicates
                    )
                )
            )
        )
    );

    toList ret
)

-- Cache for storing OI-Groebner bases
oiGBCache = new MutableHashTable

-- PURPOSE: Compute a Groebner basis
-- INPUT: A List 'L' of VectorInWidths
-- OUTPUT: A Groebner basis made from L
-- COMMENT: Uses the OI-Buchberger's Algorithm
-- COMMENT: "Verbose => true" will print more information
-- COMMENT: "Strategy => 1" recalculates the OI-pairs every time a nonzero remainder is found
-- COMMENT: "Strategy => 2" adds all nonzero remainders before recalculating the OI-pairs
-- COMMENT: "MinimalOIGB => false" will not minimize the basis at the end
oiGB = method(TypicalValue => List, Options => {Verbose => false, Strategy => 1, MinimalOIGB => true})
oiGB List := opts -> L -> (

    if not (opts.Strategy == 1 or opts.Strategy == 2) then error "Expected Strategy => 1 or Strategy => 2";

    -- Return the GB if it already exists
    if oiGBCache#?(L, opts.Strategy, opts.MinimalOIGB) then return oiGBCache#(L, opts.Strategy, opts.MinimalOIGB);

    if #L == 0 then error "Expected a nonempty List";
    if #L == 1 then if isZero L#0 then return false else return L;
    
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
                print("On pair "|toString(i + 1)|" out of "|toString(#oipairs));
                if opts.Strategy == 2 then print("Elements added so far this phase: "|toString addedThisPhase);
                print("Elements added total: "|toString addedTotal)
            );

            rem := (oiPolyDiv(s, toList ret, Verbose => opts.Verbose)).rem;
            if not isZero rem and not member(rem, toList ret) then (
                if opts.Verbose then print("Nonzero remainder: "|net rem);
                ret#retIndex = rem;
                retIndex = retIndex + 1;

                addedThisPhase = addedThisPhase + 1;
                addedTotal = addedTotal + 1;

                if opts.Strategy == 1 then break
            )
        );

        if toList ret === retTmp then break -- No new elements were added so we're done by the OI-Buchberger's Criterion
    );

    -- Minimize the basis
    local finalRet;
    if opts.MinimalOIGB then finalRet = minimizeOIGB(toList ret, Verbose => opts.Verbose) else finalRet = toList ret;

    -- Store the basis
    oiGBCache#(L, opts.Strategy, opts.MinimalOIGB) = finalRet;

    finalRet
)

-- PURPOSE: Minimize an OI-Groebner basis
-- INPUT: A List 'L'
-- OUTPUT: Assuming L is an OI-Groebner basis, returns a minimized basis made from L
-- COMMENT: "Minimal" in the sense of lt(p) not in <lt(L - {p})> for all p in L
-- COMMENT: "Verbose => true" will print more information
minimizeOIGB = method(TypicalValue => List, Options => {Verbose => false})
minimizeOIGB List := opts -> L -> (
    if opts.Verbose then print "Computing minimal OIGB...";

    currentBasis := L;
    while true do (
        redundantFound := false;

        for p in currentBasis do (
            minusp := toList((set currentBasis) - set {p});
            for elt in minusp do if not isZero (oiTermDiv(p, elt)).quo then (
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

-- PURPOSE: Check if a List is an OI-Groebner basis
-- INPUT: A List 'L' of VectorInWidths
-- OUTPUT: true if L is an OI-Groebner basis, false otherwise
-- COMMENT: "Verbose => true" will print more information
isOIGB = method(TypicalValue => Boolean, Options => {Verbose => false})
isOIGB List := opts -> L -> (
    if #L == 0 then return false;
    if #L == 1 then if isZero L#0 then return false else return true;

    encountered := new MutableList;
    encIndex := 0;
    oipairs := oiPairs(L, Verbose => opts.Verbose);
    for i to #oipairs - 1 do (
        if opts.Verbose then (
            print("On pair "|toString(i + 1)|" out of "|toString(#oipairs));
            print("Pair: "|net oipairs#i)
        );

        s := spoly(oipairs#i#0, oipairs#i#1);
        if isZero s or member(s, toList encountered) then continue;

        encountered#encIndex = s;
        encIndex = encIndex + 1;
        rem := (oiPolyDiv(s, L, Verbose => opts.Verbose)).rem;
        if not isZero rem then (if opts.Verbose then print("Nonzero remainder: "|net rem); return false) -- If L were a GB, then every element would have a unique remainder of zero
    );
    
    true
)

-- Cache for storing Groebner bases computed with oiSyz
oiSyzCache = new MutableHashTable

-- PURPOSE: Compute an OI-Groebner basis for the syzygy module of a List of VectorInWidths
-- INPUT: '(L, d)', a List 'L' of VectorInWidths and a basis Symbol 'd'
-- OUTPUT: Assuming L is an OI-Groebner basis, outputs an OI-Groebner basis for the syzygy module of L
-- COMMENT: Uses the OI-Schreyer's Theorem
oiSyz = method(TypicalValue => List, Options => {Verbose => false, MinimalOIGB => true})
oiSyz(List, Symbol) := opts -> (L, d) -> (
    if #L == 0 then error "Expected a nonempty list";
    
    -- Return the GB if it already exists
    if oiSyzCache#?(L, d, opts.MinimalOIGB) then return oiSyzCache#(L, d, opts.MinimalOIGB);
    
    freeOIMod := freeOIModuleFromElement L#0;
    shifts := for elt in L list -degree elt;
    widths := for elt in L list widthOfElement elt;
    G := makeFreeOIModule(freeOIMod.polyOIAlg, d, widths, DegreeShifts => flatten shifts);
    S := makeFreeOIModuleMap(freeOIMod, G, L);
    installSchreyerMonomialOrder S;

    ret := new MutableList;
    retIndex := 0;
    oipairs := oiPairs(L, Verbose => opts.Verbose);
    if opts.Verbose then print "Iterating through pairs...";
    i := 0;
    for pair in oipairs do (
        lotf := leadOITerm pair#0;
        lotg := leadOITerm pair#1;
        lcmfg := lcm(lotf, lotg);
        if isZero lcmfg then continue; -- This will yield a trivial syzygy

        if opts.Verbose then (print("On pair "|toString(i + 1)|" out of "|toString(#oipairs)); i = i + 1);
        if opts.Verbose then print("Pair: "|net pair);

        s := spoly(pair#0, pair#1);
        swidth := widthOfElement s;
        freeMod := getFreeModuleInWidth(G, swidth);
        thingToSubtract := 0_freeMod;
        if not isZero s then (
            sdiv := oiPolyDiv(s, L, Verbose => opts.Verbose);

            -- Calculate the stuff to subtract off
            for triple in sdiv.triples do (
                b := makeBasisElement makeBasisIndex(G, triple#1, 1 + triple#2);
                thingToSubtract = thingToSubtract + triple#0 * getVectorFromOITerms {b}
            )
        );

        bSigma := makeBasisElement makeBasisIndex(G, pair#2, 1 + pair#4);
        bTau := makeBasisElement makeBasisIndex(G, pair#3, 1 + pair#5);

        -- Make the syzygy
        syzygy := (lcmfg.ringElement // lotf.ringElement) * getVectorFromOITerms {bSigma} - (lcmfg.ringElement // lotg.ringElement) * getVectorFromOITerms {bTau} - thingToSubtract;
        
        if isZero syzygy then continue; -- Skip trivial syzygies

        ret#retIndex = syzygy;
        if opts.Verbose then print("Generated syzygy "|net ret#retIndex);
        retIndex = retIndex + 1
    );

    -- Minimize the basis
    local finalRet;
    if opts.MinimalOIGB then finalRet = minimizeOIGB(toList ret, Verbose => opts.Verbose) else finalRet = toList ret; 

    -- Store the GB
    oiSyzCache#(L, d, opts.MinimalOIGB) = finalRet;

    finalRet
)