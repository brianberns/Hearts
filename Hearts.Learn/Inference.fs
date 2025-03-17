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

        // First, “expand” Initial nodes into their continuation results.
        // At each level (i.e. within a given array of sibling nodes)
        // we batch–call GetChildIds on all Initial nodes (preserving order).
        let rec expandBatch (nodes: Traversal[]) : Traversal[] =
            // Find all indices where a node is still Initial.
            let indexedInitials =
                nodes 
                |> Array.mapi (fun i node ->
                    match node with
                    | GetStrategy init -> Some (i, init)
                    | _ -> None)
                |> Array.choose id
            if indexedInitials.Length = 0 then
                // No Initial nodes at this level.
                // But if any node is InProgress, its children may still be Initial.
                nodes 
                |> Array.map (fun node ->
                    match node with
                    | GetUtility ip -> GetUtility {| ip with Results = expandBatch ip.Results |}
                    | _ -> node)
            else
                // For all Initial nodes at this level, collect their ids (in the same order).
                let ids = indexedInitials |> Array.map (fun (_, init) -> init.InformationSet)
                // Batch call: one call per level.
                let childrenIdsBatch = getStrategies ids modelOpt
                // Replace each Initial node with the node returned by its continuation.
                let nodesUpdated = Array.copy nodes
                for k in 0 .. indexedInitials.Length - 1 do
                    let (i, init) = indexedInitials[k]
                    let childIds = childrenIdsBatch[k]
                    nodesUpdated[i] <- init.Continuation childIds
                // Recurse: some of the new nodes might be Initial or have InProgress children.
                nodesUpdated 
                |> Array.map (fun node ->
                    match node with
                    | GetUtility ip -> GetUtility {| ip with Results = expandBatch ip.Results |}
                    | _ -> node)
    
        // Now every node is either InProgress or Complete.
        // We define a function to “complete” an InProgress node by recursively processing its children.
        let rec completeNode (node: Traversal) : Complete =
             match node with
             | Complete c -> c.Utilities, c.Samples
             | GetUtility ip ->
                 // Complete all children (this preserves the original order).
                 let childrenComplete = ip.Results |> Array.map completeNode
                 let childFsts = childrenComplete |> Array.map fst
                 let childSnds = childrenComplete |> Array.map snd
                 match ip.Continuation childFsts childSnds with
                 | Complete c -> c.Utilities, c.Samples
                 | _ -> failwith "Continuation did not yield a Complete node"
             | GetStrategy _ -> failwith "Should not have any Initial node after expansion"
    
        // We now traverse the (expanded) tree to collect the complete nodes for every node.
        let rec collectCompletes (node: Traversal) : Complete list =
             match node with
             | Complete c -> [c.Utilities, c.Samples]
             | GetUtility ip ->
                 // First complete this node.
                 let selfComplete = completeNode node
                 // Then collect complete nodes from its children.
                 let childrenCompletes = ip.Results |> Array.toList |> List.collect collectCompletes
                 selfComplete :: childrenCompletes
             | GetStrategy _ -> failwith "Should not have any Initial node after expansion"
    
        // Expand all nodes (batched per sibling group).
        let expanded = expandBatch nodes
        // Collect complete nodes from the entire tree.
        let allCompletes = expanded |> Array.toList |> List.collect collectCompletes
        // (Optionally sort by id – here the sample expects node ids 1,2,3,4,5.)
        allCompletes 
        // |> List.sortBy (fun c -> c.Id) 
        |> Seq.collect snd
        |> Seq.toArray
