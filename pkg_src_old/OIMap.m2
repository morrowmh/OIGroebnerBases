-- Should be of the form {targWidth => ZZ, img => List}
OIMap = new Type of HashTable

net OIMap := f -> "Source: ["|net(#f.img)|"] Target: ["|net f.targWidth|"]" || "Image: "|net f.img

source OIMap := f -> toList(1..#f.img)
target OIMap := f -> toList(1..f.targWidth)
image OIMap := f -> f.img

-- Given an OI-map f, compute f(n)
OIMap ZZ := (f, n) -> f.img#(n - 1)

makeOIMap = method(TypicalValue => OIMap)
makeOIMap(ZZ, List) := (n, L) -> new OIMap from {targWidth => n, img => L}

-- Get the OI-maps between two widths
getOIMaps = method(TypicalValue => List)
getOIMaps(ZZ, ZZ) := (m, n) -> (
    if n < m then return {};

    sets := subsets(toList(1..n), m);
    for i to #sets - 1 list new OIMap from {targWidth => n, img => sets#i}
)

-- Given OI-maps f and g, compute f(g)
composeOIMaps = method(TypicalValue => OIMap)
composeOIMaps(OIMap, OIMap) := (f, g) -> (
    if not source f === target g then error "maps cannot be composed due to incompatible source and target";

    -- Compute the composition
    L := for i in source g list f g i;
    makeOIMap(f.targWidth, L)
)

-- Shorthand composition
OIMap OIMap := (f, g) -> composeOIMaps(f, g)