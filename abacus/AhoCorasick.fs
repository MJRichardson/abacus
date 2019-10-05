namespace Abacus

module private AhoCorasick =
    open System.Collections.Generic
    
    type State() =
        let symbolMap = new SymbolMap()
        let mutable fail = None  
        member val Values = Set.empty<string> with get,set
        member this.SymbolMap = symbolMap
        member val LoopOnFail = false with get,set
        member this.GoTo(symbol : char) =
            if symbolMap.ContainsKey(symbol) then Result symbolMap.[symbol]
            elif this.LoopOnFail then Result this
            else Fail
        member this.AddChild symbol state =
            symbolMap.Add(symbol, state)
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
    
    let rec find (state:State) (searchTerm:char list) = seq {
        match searchTerm with
        | [] -> ()
        | head::tail ->
            match state.GoTo head with
            | Result s ->
                yield! s.Values
                yield! find s tail 
            | Fail ->
                yield! find state.Fail searchTerm
        }
    
    let enter (root:State) pattern =
        let mutable state = root
        for c in pattern do 
            match state.GoTo c with
            | Fail ->
                let newState = new State()
                newState.Fail <- root
                state.AddChild c newState
                state <- newState
            | Result s ->
                state <- s
        state.Values <- Set.add pattern state.Values
   
    let breadthFirstTraverse (state:State) =
        let rec traverse (states:seq<State>) = seq {
            let children = states |> Seq.collect (fun s -> s.SymbolMap.Values)
            if Seq.isEmpty children then ()
            else
                yield! children
                yield! traverse children
        }
        state |> Seq.singleton |> traverse
        
    let constructFailure root =
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
type AhoCorasick(keywords) =
    let root = new State()
    do
        for keyword in keywords do
             enter root keyword
        root.LoopOnFail <- true
        constructFailure root
    member this.Find(searchText: string) =
        find root (Seq.toList searchText)