-- Cache for storing Groebner bases computed with oiSyz
oiSyzCache = new MutableHashTable

-- Compute an OI-Groebner basis for the syzygy module of a List of VectorInWidth objects
oiSyz = method(TypicalValue => List, Options => {Verbose => false, Strategy => Minimize})
oiSyz(List, Symbol) := opts -> (L, d) -> (
    if not (opts.Strategy === FastNonminimal or opts.Strategy === Minimize or opts.Strategy === Reduce) then
        error "expected Strategy => FastNonminimal or Strategy => Minimize or Strategy => Reduce";
    
    if opts.Verbose then print "Computing syzygies...";
    
    -- Return the GB if it already exists
    if oiSyzCache#?(L, d, opts.Strategy) then return oiSyzCache#(L, d, opts.Strategy);

    -- Throw out any repeated or zero elements
    L = unique for elt in L list if isZero elt then continue else elt;
    if #L === 0 then error "expected a nonempty list of nonzero elements";

    fmod := getFreeOIModule L#0;
    shifts := for elt in L list -degree elt;
    widths := for elt in L list getWidth elt;
    G := makeFreeOIModule(d, widths, fmod.polyOIAlg, DegreeShifts => shifts, OIMonomialOrder => L);

    oipairs := oiPairs(L, opts.Verbose);
    if opts.Verbose then print "Iterating through critical pairs...";
    i := 0;
    ret := for pair in oipairs list (        
        if opts.Verbose then (
            print("On critical pair " | toString(i + 1) | " out of " | toString(#oipairs));
            print("Pair: (" | net pair.im0 | ", " | net pair.im1 | ")");
            i = i + 1
        );

        ltf := keyedLeadTerm pair.im0;
        ltg := keyedLeadTerm pair.im1;
        ltfelt := ltf.vec#(ltf.key);
        ltgelt := ltg.vec#(ltg.key);
        lcmlmfg := lcm(leadMonomial ltfelt, leadMonomial ltgelt);
        s := SPolynomial(pair.im0, pair.im1);
        M := getModuleInWidth(G, getWidth s);
        thingToSubtract := makeZero M;

        -- Calculate the stuff to subtract off
        if not isZero s then for tuple in (polyDiv(s, L)).divTuples do
            thingToSubtract = thingToSubtract + makeSingle(M, ((tuple#0).oiMap, 1 + tuple#1), (tuple#0).quo);
        
        -- Make the syzygy
        sing1 := makeSingle(M, (pair.map0, 1 + pair.idx0), lcmlmfg // ltfelt);
        sing2 := makeSingle(M, (pair.map1, 1 + pair.idx1), lcmlmfg // ltgelt);
        syzygy := sing1 - sing2 - thingToSubtract;

        if opts.Verbose then print("Generated syzygy: " | net syzygy);

        syzygy
    );

    -- Throw out any repeated or zero elements
    ret = unique for elt in ret list if isZero elt then continue else elt;

    -- Minimize the basis
    if #ret > 0 and opts.Strategy === Minimize then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = minimizeOIGB(ret, Verbose => opts.Verbose)
    );

    -- Reduce the basis
    if #ret > 0 and opts.Strategy === Reduce then (
        if opts.Verbose then print "\n----------------------------------------\n----------------------------------------\n";
        ret = reduceOIGB(ret, Verbose => opts.Verbose)
    );

    -- Store the GB
    oiSyzCache#(L, d, opts.Strategy) = ret
)