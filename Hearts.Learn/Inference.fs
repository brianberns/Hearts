namespace Hearts.Learn

open Hearts.Model

module Inference =

    let private getStrategies infoSets modelOpt =
        match modelOpt with
            | Some model ->
                Strategy.getFromAdvantage infoSets model
            | None ->
                infoSets
                    |> Array.map (fun infoSet ->
                        Strategy.random infoSet.LegalActions.Length)

    type Complete = float32[] * AdvantageSample[]

    let complete modelOpt (nodes : Traversal[]) : AdvantageSample[] =
        // Expand all Initial nodes in a batch. For siblings,
        // we call api.GetChildIds once per batch.
        let rec expandBatch (nodes: Traversal[]) : Traversal[] =
            // Find indices of all Initial nodes at this sibling level.
            let indexedInitials =
                nodes 
                |> Array.mapi (fun i node ->
                    match node with
                    | GetStrategy init -> Some (i, init)
                    | _ -> None)
                |> Array.choose id
            if indexedInitials.Length = 0 then
                // No Initial nodes here. However, InProgress nodes may have children
                // that still need expansion.
                nodes |> Array.map (fun node ->
                    match node with
                    | GetUtility ip -> GetUtility {| ip with Results = expandBatch ip.Results |}
                    | _ -> node)
            else
                // Process all Initial nodes in one batch.
                let nodesUpdated = Array.copy nodes
                let ids = indexedInitials |> Array.map (fun (_, init) -> init.InformationSet)
                let childrenIdsBatch = getStrategies ids modelOpt
                for k in 0 .. indexedInitials.Length - 1 do
                    let (i, init) = indexedInitials.[k]
                    let childIds = childrenIdsBatch.[k]
                    nodesUpdated.[i] <- init.Continuation childIds
                // Recurse on InProgress children.
                nodesUpdated 
                |> Array.map (fun node ->
                    match node with
                    | GetUtility ip -> GetUtility {| ip with Results = expandBatch ip.Results |}
                    | _ -> node)
    
        // Keep expanding until no Initial nodes remain.
        let rec fullyExpandBatch (nodes: Traversal[]) : Traversal[] =
            let expanded = expandBatch nodes
            if expanded |> Array.exists (function GetStrategy _ -> true | _ -> false) then
                fullyExpandBatch expanded
            else
                expanded

        // Complete a node by recursively completing its children.
        // If a continuation returns an Initial node, we fully expand it first.
        let rec completeNode (node: Traversal) : Complete =
             match node with
             | Complete c -> c.Utilities, c.Samples
             | GetUtility ip ->
                 // Complete all children in order.
                 let childrenComplete = ip.Results |> Array.map completeNode
                 let childFsts = childrenComplete |> Array.map fst
                 let childSnds = childrenComplete |> Array.map snd
                 match ip.Continuation childFsts childSnds with
                 | Complete c -> c.Utilities, c.Samples
                 | GetStrategy _ as initNode ->
                     // Fully expand the returned Initial node and then complete it.
                     let expanded = fullyExpandBatch [| initNode |]
                     completeNode expanded.[0]
                 | unexpected -> failwithf "Unexpected node returned from InProgress continuation: %A" unexpected
             | GetStrategy _ as initNode ->
                 // If we ever see an Initial node, fully expand it.
                 let expanded = fullyExpandBatch [| initNode |]
                 completeNode expanded.[0]
    
        // Collect every Complete node in the tree.
        let rec collectCompletes (node: Traversal) : Complete list =
             match node with
             | Complete c -> [c.Utilities, c.Samples]
             | GetUtility ip ->
                 let selfComplete = completeNode node
                 let childrenCompletes = ip.Results |> Array.toList |> List.collect collectCompletes
                 selfComplete :: childrenCompletes
             | GetStrategy _ as initNode ->
                 // Fully expand an Initial node before collecting.
                 let c = completeNode initNode
                 [c]
    
        // Fully expand the entire forest of nodes.
        let expanded = fullyExpandBatch nodes
        // Traverse the expanded tree and collect every Complete node.
        expanded
        |> Array.toList
        |> List.collect collectCompletes
        // |> List.sortBy (fun c -> c.Id)  // Optionally sort the results.
        |> Seq.collect snd
        |> Seq.toArray
