namespace Abacus

module private AhoCorasick =
    open System.Collections.Generic
    
    type State(fail:State) =
        let symbolMap = new SymbolMap() 
        member val Fail = fail with get,set
        member val Values = Set.empty<string> with get,set
        member this.SymbolMap = symbolMap
        abstract member GoTo : char -> GoToResult
        default this.GoTo(symbol : char) =
            if symbolMap.ContainsKey(symbol) then Result symbolMap.[symbol]
            else Fail
        member this.AddChild symbol state =
            symbolMap.Add(symbol, state)

    and Root() as this =
        inherit State(this)
        override this.GoTo(symbol: char) =
            match base.GoTo symbol with
            | Result r -> Result r 
            | Fail -> Result this

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
                state <- new State(root)
                state.AddChild c state
            | Result s ->
                state <- s
        state.Values <- Set.add pattern state.Values
   
    let breadthFirstTraverse (state:State) =
        let rec traverse (states:seq<State>) = seq {
            let children = states |> Seq.collect (fun s -> s.SymbolMap.Values)
            yield! children
            yield! traverse children
        }
        seq state.SymbolMap.Values |> traverse
        
    let constructFailure root =
        let rec walkFails (state:State) = seq {
            yield state.Fail
            yield! walkFails state.Fail
        }
        for state in (breadthFirstTraverse root) do
            for a in state.SymbolMap.Keys do
                let s = state.SymbolMap.[a]
                s.Fail <- state |> walkFails |> Seq.pick
                                                    ( fun f ->
                                                         match f.GoTo a with
                                                         | Fail -> None
                                                         | Result x -> Some x)
                s.Values <- Set.union s.Fail.Values s.Values
                
open AhoCorasick
type AhoCorasick(keywords) =
    let root = new Root()
    do
        for keyword in keywords do
             enter root keyword
        constructFailure root
    member this.Find(searchText: string) =
        find root (Seq.toList searchText)