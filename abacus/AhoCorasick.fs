namespace Abacus

module private AhoCorasick =
    open System.Collections.Generic
    
    // Represents a state in the Aho-Corasick matching machine
    type State() =
        let symbolMap = new SymbolMap()
        let mutable fail = None
        // Map of symbols to transition states
        member this.SymbolMap = symbolMap
        // Keywords that end at this state
        member val Values = Set.empty<string> with get,set
        // Once the states have been added, the root state's goto function
        // is configured to loop to itself if the symbol does not to map to
        // another state. 
        member val LoopOnFail = false with get,set
        // The goto function: for a given state returns either the state to transition to, or Fail 
        member this.GoTo(symbol : char) =
            if symbolMap.ContainsKey(symbol) then Result symbolMap.[symbol]
            elif this.LoopOnFail then Result this
            else Fail
        // Add a symbol to the machine and the transition state 
        member this.AddSymbol symbol state =
            symbolMap.Add(symbol, state)
        // The state to transition to when goto returns Fail
        member this.Fail
            with get() =
                match fail with
                | Some f -> f
                | None -> this 
            and set f = fail <- Some f

    and GoToResult =
        | Result of State
        | Fail

    and SymbolMap = Dictionary<char, State>
    
    // Returns all keywords found in the search text 
    let rec find (state:State) (searchText:char list) = seq {
        match searchText with
        | [] -> ()
        | head::tail ->
            match state.GoTo head with
            | Result s ->
                yield! s.Values
                yield! find s tail 
            | Fail ->
                yield! find state.Fail searchText
        }
    
    // Adds a keyword to the state machine
    let enter (root:State) keyword =
        let mutable state = root
        for c in keyword do 
            match state.GoTo c with
            | Fail ->
                let newState = new State()
                newState.Fail <- root
                state.AddSymbol c newState
                state <- newState
            | Result s ->
                state <- s
        state.Values <- Set.add keyword state.Values
   
    // Performs a breadth-first traversal of the state tree
    let breadthFirstTraverse (state:State) =
        let rec traverse (states:seq<State>) = seq {
            let children = states |> Seq.collect (fun s -> s.SymbolMap.Values)
            if Seq.isEmpty children then ()
            else
                yield! children
                yield! traverse children
        }
        state |> Seq.singleton |> traverse
        
    // Constructs the fail transitions for each state
    let constructFailure root =
        // Follow the failure transitions until we find a state where
        // goto state a !== fail  
        let rec followFails (a:char) (state:State) =
            match state.Fail.GoTo a with
            | Result s -> s
            | Fail -> followFails a state.Fail 
        for state in (breadthFirstTraverse root) do
            for a in state.SymbolMap.Keys do
                let s = state.SymbolMap.[a]
                s.Fail <- followFails a state 
                s.Values <- Set.union s.Fail.Values s.Values
                
open AhoCorasick
// Class to represent the AhoCorasick machine
type AhoCorasickStringMatch(keywords) =
    let root = new State()
    do
        for keyword in keywords do
             enter root keyword
        root.LoopOnFail <- true
        constructFailure root
    member this.Find(searchText: string) =
        searchText |> Seq.toList |> find root
