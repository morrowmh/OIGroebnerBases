-- Cache for storing OI-Groebner bases
oiGBCache = new MutableHashTable

-- Compute an OI-Groebner basis for a List of VectorInWidth objects
oiGB = method(TypicalValue => List, Options => {Verbose => false, Strategy => Minimize})
oiGB List := opts -> L -> (
    if not (opts.Strategy === FastNonminimal or opts.Strategy === Minimize or opts.Strategy === Reduce) then
        error "expected Strategy => FastNonminimal or Strategy => Minimize or Strategy => Reduce";
    
    if opts.Verbose then print "Computing OIGB...";

    -- Return the GB if it already exists
    if oiGBCache#?(L, opts.Strategy) then return oiGBCache#(L, opts.Strategy);

    -- Throw out any repeated or zero elements
    ret := unique for elt in L list if isZero elt then continue else elt;
    if #ret === 0 then error "expected a nonempty list of nonzero elements";

    encountered := new List;
    totalAdded := 0;

    -- Enter the main loop: terminates by an equivariant Noetherianity argument
    while true do (
        oipairs := oiPairs(ret, opts.Verbose);

        remToAdd := null;
        for i to #oipairs - 1 do (
            s := SPolynomial((oipairs#i).im0, (oipairs#i).im1);
            if isZero s then continue;

            if member(s, encountered) then continue -- Skip S-Polynomials that have already appeared
            else encountered = append(encountered, s);

            if opts.Verbose then print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));

            rem := (polyDiv(s, ret)).rem;
            if not isZero rem and not member(rem, ret) then (
                if opts.Verbose then (
                    print("Found nonzero remainder: " | net rem);
                    totalAdded = totalAdded + 1;
                    print("Elements added total: " | net totalAdded);
                );

                remToAdd = rem;
                break
            )
        );

        if remToAdd === null then break;
        ret = append(ret, remToAdd)
    );

    -- Minimize the basis
    if opts.Strategy === Minimize then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = minimizeOIGB(ret, Verbose => opts.Verbose)
    );

    -- Reduce the basis
    if opts.Strategy === Reduce then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = reduceOIGB(ret, Verbose => opts.Verbose)
    );

    -- Store the GB
    oiGBCache#(L, opts.Strategy) = ret
)

-- Minimize an OI-Groebner basis in the sense of monic and lt(p) not in <lt(G - {p})> for all p in G
minimizeOIGB = method(TypicalValue => List, Options => {Verbose => false})
minimizeOIGB List := opts -> G -> (
    if opts.Verbose then print "Computing minimal OIGB...";

    -- Throw out any repeated or zero elements
    G = unique for elt in G list if isZero elt then continue else elt;
    if #G === 0 then error "expected a nonempty list of nonzero elements";

    nonRedundant := new List;
    currentBasis := unique apply(G, makeMonic); -- unique is used again because collisions may happen after makeMonic

    if #currentBasis === 1 then return currentBasis;

    while true do (
        redundantFound := false;

        for p in currentBasis do (
            if member(p, nonRedundant) then continue; -- Skip elements already verified to be nonredundant

            minusp := toList((set currentBasis) - set {p});
            ltp := keyedLeadTerm p;
            for elt in minusp do if not zero (termDiv(ltp, keyedLeadTerm elt)).quo then (
                if opts.Verbose then print("Found redundant element: " | net p);
                redundantFound = true;
                currentBasis = minusp;
                break
            );

            if redundantFound then break;
            nonRedundant = append(nonRedundant, p);
        );

        if not redundantFound then break
    );

    currentBasis
)

-- Remove a single element from a List
-- Args: L = List, i = ZZ
sdrop := (L, i) -> drop(L, {i, i})

-- Reduce an OI-Groebner basis in the sense of monic and no term of any element is OI-divisible by the lead term of any other
reduceOIGB = method(TypicalValue => List, Options => {Verbose => false})
reduceOIGB List := opts -> G -> (
    minG := minimizeOIGB(G, Verbose => opts.Verbose);

    if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n\nComputing reduced OIGB...";

    if #minG === 1 then return minG;

    -- Reduce the basis
    newG := new MutableList from minG;
    for i to #newG - 1 do (
        if opts.Verbose then print("Reducing element " | toString(i + 1) | " of " | toString(#newG));
        dropped := flatten sdrop(toList newG, i);
        red := oiNormalForm(newG#i, dropped);
        newG#i = if member(red, dropped) then {} else red
    );

    flatten toList newG
)

-- Check if a List is an OI-Groebner basis
isOIGB = method(TypicalValue => Boolean, Options => {Verbose => false})
isOIGB List := opts -> L -> (
    if opts.Verbose then print "Checking Buchberger's Criterion...";

    -- Throw out any repeated or zero elements
    L = unique for elt in L list if isZero elt then continue else elt;
    if #L === 0 then error "expected a nonempty list of nonzero elements";

    encountered := new List;
    oipairs := oiPairs(L, opts.Verbose);
    for i to #oipairs - 1 do (
        s := SPolynomial((oipairs#i).im0, (oipairs#i).im1);
        if isZero s then continue;

        if member(s, encountered) then continue -- Skip S-Polynomials that have already appeared
        else encountered = append(encountered, s);

        if opts.Verbose then print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));
    
        rem := (polyDiv(s, L)).rem;
        if not isZero rem then (
            if opts.Verbose then print("Found nonzero remainder: " | net rem);
            return false
        )
    );

    true
)