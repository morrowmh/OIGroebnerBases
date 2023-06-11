-- Should be of the form {basisSym => Symbol, genWidths => List, degShifts => List, polyOIAlg => PolynomialOIAlgebra, monOrder => Thing, modules => MutableHashTable, maps => MutableHashTable}
FreeOIModule = new Type of HashTable

toString FreeOIModule := F -> "("|toString F.basisSym|", "|toString F.genWidths|", "|toString F.degShifts|")"

net FreeOIModule := F -> (
    monOrderNet := if F.monOrder === Lex then net Lex
    else if instance(F.monOrder, List) then "Schreyer"
    else error "invalid monomial order";

    "Basis symbol: "|net F.basisSym ||
    "Generator widths: "|net F.genWidths ||
    "Degree shifts: "|net F.degShifts ||
    "Polynomial OI-algebra: "|toString F.polyOIAlg ||
    "Monomial order: "|monOrderNet
)

makeFreeOIModule = method(TypicalValue => FreeOIModule, Options => {DegreeShifts => null, MonomialOrder => Lex})
makeFreeOIModule(Symbol, List, PolynomialOIAlgebra) := opts -> (e, W, P) -> (
    shifts := if opts.DegreeShifts === null then toList(#W : 0)
    else if instance(opts.DegreeShifts, List) then opts.DegreeShifts
    else error "invalid DegreeShifts option";

    -- Validate the monomial order
--    if not opts.MonomialOrder === Lex and not (
  --      instance(opts.MonomialOrder, List) and 
    --    W === apply(opts.MonomialOrder, widthOfElement) and 
      --  #set apply(opts.MonomialOrder, freeOIModuleFromElement) == 1) then error "invalid monomial order";

    new FreeOIModule from {
        basisSym => e,
        genWidths => W,
        degShifts => shifts,
        polyOIAlg => P,
        monOrder => opts.MonomialOrder,
        modules => new MutableHashTable,
        maps => new MutableHashTable}
)

-- Should be of the form {wid => ZZ, rawMod => Module, freeOIMod => FreeOIModule}
ModuleInWidth := new Type of HashTable