open Expecto
open PointRegionQuadtree.UnitTests.MainTests

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests
