namespace FSharpBio.IO

open FSharpBio.IO.BlastXml
open FSharpBio.IO.SchemaReader.Attribute
open FSharpBio.IO.SchemaReader.Csv

module Taxonomy =

    type SequenceID =
    | GI of string
    /// Genbank
    | Genbank of string * string
    /// WUGenbank
    | WUGenbank of string * string
    | EMBL of string * string
    | WUEMBL of string * string
    | DDBJ of string * string
    | WUDDBJ of string * string
    /// PIR
    | NBRF of string * string
    /// Protein Research Foundation
    | PRF of string * string
    /// SwissProt
    | SP of string * string
    | TrEMBL of string * string
    /// Brookhaven Protein Data Bank
    | BPDB of string * string
    | Patents of string * string * string
    /// GenInfoBackbone
    | Bb of string
    /// GeneralDatabase
    | GD of string * string
    /// Ref
    | NCBI of string * string
    | Local of string
    /// NCBI GenInfo Backbone database identifier
    | BBM of string
    /// NCBI GenInfo Import identifier
    | GIM of string
    /// GenPept
    | GP of string * string
    /// Other (user-definable) identifier
    | Other of string * string * string
    | NotKnown of string []

    type GIToTaxID =
        { [<FieldAttribute(0)>] GiId : int
          [<FieldAttribute(1)>] TaxId : int}

    type TaxIDToWholeTax =
        { [<FieldAttribute(0)>] TaxID : int
          [<FieldAttribute(2)>] ParentTaxID : int
          [<FieldAttribute(4)>] RankNode : string
          }

    type TaxonomyInfo =
        { [<FieldAttribute(0)>] TaxID : int
          [<FieldAttribute(2)>] Name : string
          [<FieldAttribute(6)>] Source : string
          }

    type Range = {
        Min:int
        Max:int
        ID:int
        }

    let private getTaxOfGi (giToTax:Range[]) (giId:int) =
        giToTax 
        |> Array.tryFind (fun rangeElemt -> giId >= rangeElemt.Min && giId <= rangeElemt.Max )
        |> (fun x -> match x with 
                     | Some(v) -> Some (giId ,v.ID) 
                     | None    -> None
                    ) 

    let private createRange min max id = {
        Min = min;
        Max = max;
        ID = id}
    
    let private getlistOfTaxId (nodes:TaxIDToWholeTax[]) (tax:int) =
        let rec loop actualTax = seq{
            let taxo = nodes |> Array.find (fun x -> actualTax = x.TaxID)
            match taxo.TaxID with
                |1 -> yield (taxo.TaxID, taxo.RankNode)
                |_ -> yield (taxo.TaxID, taxo.RankNode)
                      yield! loop taxo.ParentTaxID
            }
        loop tax  
    
    let private translateTuplesToStrings (s:seq<string*string>) =
        s
        |> Seq.map (fun (x, y) -> sprintf "%s %s" x y)
        |> String.concat " | "

    


    let private takeIteration (l:BlastResult) =
        match l with
            | BlastResult.IterationResult(iter) -> Some(iter)
            | _                                 -> None

    let private tryHead (source : seq<_>) = 
        use e = source.GetEnumerator()
        if e.MoveNext()
        then Some(e.Current)
        else None //empty list

    let private checkIfNoHit (l:Iteration) =
        match tryHead l.Hits with
            |Some(_) -> Some(l.Hits)
            |None    -> None
    
    let private takeFirstHitId (input:seq<Hit>) =
        let en = input.GetEnumerator()
    
        match en.MoveNext() with
            | true  -> en.Current.Id
            | false -> failwithf "Error: Element not compatible!"

    let private takeFirstHitIdOneCondition (input:seq<Hit>) (first:string) =
        let en = input.GetEnumerator()
        let rec loop (en:System.Collections.Generic.IEnumerator<Hit>) =
            match en.MoveNext() with
                | true  -> match en.Current.Def.Contains(first) with
                            | true  -> loop en
                            | false -> Some(en.Current.Id)
                | false -> None
        loop en      

    let private takeFirstHitIdTwoConditions (input:seq<Hit>) (first:string) (second:string) =
        let en = input.GetEnumerator()
        let rec loop (en:System.Collections.Generic.IEnumerator<Hit>) =
            match en.MoveNext() with
                | true  -> match en.Current.Def.Contains(first) && en.Current.Def.Contains(second) with
                            | true  -> loop en
                            | false -> Some(en.Current.Id)
                | false -> None
        loop en      

    let private takeFirstHit (input:seq<Hit>) =
        let en = input.GetEnumerator()
    
        match en.MoveNext() with
            | true  -> en.Current
            | false -> failwithf "Error: Element not compatible!"

    let private takeFirstHitOneCondition (input:seq<Hit>) (first:string) =
        let en = input.GetEnumerator()
        let rec loop (en:System.Collections.Generic.IEnumerator<Hit>) =
            match en.MoveNext() with
                | true  -> match en.Current.Def.Contains(first) with
                            | true  -> loop en
                            | false -> Some(en.Current)
                | false -> None
        loop en      

    let private takeFirstHitTwoConditions (input:seq<Hit>) (first:string) (second:string) =
        let en = input.GetEnumerator()
        let rec loop (en:System.Collections.Generic.IEnumerator<Hit>) =
            match en.MoveNext() with
                | true  -> match en.Current.Def.Contains(first) && en.Current.Def.Contains(second) with
                            | true  -> loop en
                            | false -> Some(en.Current)
                | false -> None
        loop en
    
    let private filterBySimilarity (l:Hit) (similarity:float) =
        let h = Seq.head l.Hsps
        let le = float l.Lenght
        let po = float h.PositivesCount

        if po / le >= similarity then
            Some(l.Id)
        else None

    let private splitString (s:string) =
        let sep = [|"|"|]
        s.Split (sep, System.StringSplitOptions.None)
    
    let private makeSequenceID (l:string[]) =
        match l.[0] with
            |"gi"   -> GI(l.[1])
            |"gb"   -> Genbank(l.[1], l.[2])
            |"tpg"  -> WUGenbank(l.[1], l.[2])
            |"emb"  -> EMBL(l.[1], l.[2])
            |"tpe"  -> WUEMBL(l.[1], l.[2])
            |"dbj"  -> DDBJ(l.[1], l.[2])
            |"tpd"  -> WUDDBJ(l.[1], l.[2])
            |"ref"  -> NCBI(l.[1], l.[2])
            |"pir"  -> NBRF(l.[1], l.[2])
            |"prf"  -> PRF(l.[1], l.[2])
            |"sp"   -> SP(l.[1], l.[2])
            |"pdb"  -> BPDB(l.[1], l.[2])
            |"tr"   -> TrEMBL(l.[1], l.[2])
            |"pat"  -> Patents(l.[1], l.[2], l.[3])
            |"bbs"  -> Bb(l.[1])
            |"gnl"  -> GD(l.[1], l.[2])
            |"lcl"  -> Local(l.[1])
            |"bbm"  -> BBM(l.[1])
            |"gim"  -> GIM(l.[1])
            |"gp"   -> GP(l.[1], l.[2])
            |"oth"  -> Other(l.[1], l.[2], l.[3])
            |_      -> NotKnown(l)

    let private getGiNumber (l:string[]) =
        let t = if l.[0].Contains("gi") then
                    l.[0..1]
                else failwithf "Error: %s not compatible!" l.[0]
        makeSequenceID t

    let private getGIofSequenzID (g:SequenceID) =
        match g with
            | SequenceID.GI(gi) -> Some(int gi)
            | _ -> None
    
    /// Gives back a Sequence of the GI Numbers from the Blast Output (BlastXML). You can choose if you want to filter the Hits by similarity (between 0.0 and 1.0)
    /// and/or words (Example = giveBackGiNumber blastOutput "hypo" "botrytis" 0.75; Here you want to filter by a similarity of 75% and don't want Hits that contains hypo and botrytis).
    /// If you don't want to filter use empty strings and a similarity of 0.0 (Example = giveBackGiNumber blastOutput "" "" 0.0). 
    let giveBackGiNumber blastOut (first:string) (second:string) (similarity:float) =
        if similarity >= 0.0 && similarity <= 1.0 then
            match first with
            |"" -> match similarity with
                    |0.0 -> blastOut
                            |> Seq.choose takeIteration
                            |> Seq.choose checkIfNoHit
                            |> Seq.map takeFirstHitId
                            |> Seq.map splitString
                            |> Seq.map getGiNumber
                            |> Seq.choose getGIofSequenzID
                    |_ ->   blastOut
                            |> Seq.choose takeIteration
                            |> Seq.choose checkIfNoHit
                            |> Seq.map takeFirstHit
                            |> Seq.choose (fun x -> filterBySimilarity x similarity)
                            |> Seq.map splitString
                            |> Seq.map getGiNumber
                            |> Seq.choose getGIofSequenzID
            |_ -> match second with
                    |"" -> match similarity with
                            |0.0 -> blastOut
                                    |> Seq.choose takeIteration
                                    |> Seq.choose checkIfNoHit
                                    |> Seq.choose (fun x -> takeFirstHitIdOneCondition x first)
                                    |> Seq.map splitString
                                    |> Seq.map getGiNumber
                                    |> Seq.choose getGIofSequenzID
                            |_   -> blastOut
                                    |> Seq.choose takeIteration
                                    |> Seq.choose checkIfNoHit
                                    |> Seq.choose (fun x -> takeFirstHitOneCondition x first)
                                    |> Seq.choose (fun x -> filterBySimilarity x similarity)
                                    |> Seq.map splitString
                                    |> Seq.map getGiNumber
                                    |> Seq.choose getGIofSequenzID
                    |_   -> match similarity with
                            |0.0 -> blastOut
                                    |> Seq.choose takeIteration
                                    |> Seq.choose checkIfNoHit
                                    |> Seq.choose (fun x -> takeFirstHitIdTwoConditions x first second)
                                    |> Seq.map splitString
                                    |> Seq.map getGiNumber
                                    |> Seq.choose getGIofSequenzID
                            |_   -> blastOut
                                    |> Seq.choose takeIteration
                                    |> Seq.choose checkIfNoHit
                                    |> Seq.choose (fun x -> takeFirstHitTwoConditions x first second)
                                    |> Seq.choose (fun x -> filterBySimilarity x similarity)
                                    |> Seq.map splitString
                                    |> Seq.map getGiNumber
                                    |> Seq.choose getGIofSequenzID
        else failwithf "There could not be a similarity of %f" similarity

    /// Gives back a Sequence of Strings, that contains the GI-Number, Tax-Id and the whole Taxonomy from the Blast Output (BlastXML).
    /// You have to put in the files gi_taxid_prot.dmp, names.dmp and nodes.dmp (downloadable ftp://ftp.ncbi.nih.gov/pub/taxonomy).
    /// You can choose if you want to filter the Hits by similarity (between 0.0 and 1.0) and/or words (Example = giveBackTaxonomy blastOutput pathGiTaxId pathNames pathNodes "hypo" "otrytis cinerea" 0.75;
    /// Here you want to filter by a similarity of 75% and don't want Hits that contains hypo and Botrytis cinerea [Note that you should the first character of such a specific Name, because sometime it could start as well with an uppercase letter or a lower case letter]).
    /// If you don't want to filter use empty strings and a similarity of 0.0 (Example = giveBackTaxonomy blastOutput pathGiTaxId pathNames pathNodes "" "" 0.0). 
    let giveBackTaxonomy blastOut (giToTax:string) (names:string) (nodes:string) (first:string) (second:string) (similarity:float) =
        
        let reader = new CsvReader<GIToTaxID>(schemaMode = SchemaMode.Exact)
        let hasHeader = false
        let separator = '\t'
        let data = reader.ReadFile(giToTax, separator, hasHeader)

        let reader_2 = new CsvReader<TaxonomyInfo>(schemaMode = SchemaMode.Fill)
        let hasHeader_2 = false
        let separator_2 = '\t'
        let data_2 = reader_2.ReadFile(names, separator_2, hasHeader_2)

        let reader_nodes = new CsvReader<TaxIDToWholeTax>(schemaMode = SchemaMode.Fill)
        let hasHeader_nodes = false
        let separator_nodes = '\t'
        let data_nodes = reader_nodes.ReadFile(nodes, separator_nodes, hasHeader_nodes)

        let wholeNodes = data_nodes |> Seq.toArray

        let getRange (s:seq<GIToTaxID>) =
            let en = s.GetEnumerator()

            let rec loop (en:System.Collections.Generic.IEnumerator<GIToTaxID>) minGiNum actGiNum taxNum = seq{
                match en.MoveNext() with
                    |true   -> if en.Current.TaxId <> taxNum then
                                yield createRange minGiNum actGiNum taxNum
                                yield! loop en en.Current.GiId en.Current.GiId en.Current.TaxId
                               else yield! loop en minGiNum en.Current.GiId taxNum
                    |false  -> yield createRange minGiNum actGiNum taxNum
                    }
    
            loop en 0 0 0

        let wholeRange = getRange data |> Seq.toArray
        
        let getTaxaName (getTaxName:seq<TaxonomyInfo>) = 
            getTaxName
            |> Seq.find (fun name -> name.Source = "scientific name")
            |> (fun x -> x.Name)

        let collectTaxaNames =
            data_2
            |> Seq.groupBy (fun x -> x.TaxID)
            |> Seq.map (fun (k,v) ->  k, getTaxaName v )
            |> Seq.toArray
        
        let getTaxaResult (taxOut:int) (taxName:seq<int*string>) =
            let a = taxName
                    |> Seq.tryFind (fun (x, y) -> x = taxOut)
            match a with
                |Some(a,b) -> b
                |None -> failwith "Could not find TaxID %i in Namelist" taxOut

        let translateListToWholeTax (s:seq<int*string>) =
            s
            |> Seq.map (fun (x, y) -> (getTaxaResult x collectTaxaNames), y)

        giveBackGiNumber blastOut first second similarity
        |> Seq.choose (fun item -> getTaxOfGi wholeRange item)
        |> Seq.map (fun (gi,tax) -> (gi, tax, (getlistOfTaxId wholeNodes tax)))
        |> Seq.map (fun (gi,tax,taxo) -> (gi, tax, (translateListToWholeTax taxo)))
        |> Seq.map (fun (gi,tax, taxo) -> (gi, tax, (translateTuplesToStrings taxo)))
        |> Seq.map (fun (gi,tax, taxo) -> sprintf "Gi-Number: %i \t Tax-ID: %i \t %s" gi tax taxo)