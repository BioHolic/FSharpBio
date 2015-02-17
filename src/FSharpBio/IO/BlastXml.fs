namespace FSharpBio.IO

open System.Xml
open System.IO
open FSharp.CoreX

module BlastXml =

    // Defines the type Hsp
    type Hsp = {
        Number : int
        BitScore : float
        Score : float
        EValue : float
        QueryStart : int64
        QueryEnd : int64
        HitStart : int64
        HitEnd : int64
        QueryFrame : int64
        HitFrame : int64
        IdentitiesCount : int64
        PositivesCount : int64
        Gaps : int
        AlignmentLength : int64
        Density : int
        QuerySequence : string
        HitSequence : string
        Midline : string
        PatternFrom : int
        PatternTo : int
        }

    // Creates a value of type Hsp
    let private createHsp number bitScore score eValue queryStart queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount positivesCount gaps alignmentLength density querySequence hitSequence midline patternFrom patternTo =
        {Number = number;
        BitScore = bitScore;
        Score = score;
        EValue = eValue;
        QueryStart = queryStart;
        QueryEnd = queryEnd;
        HitStart = hitStart;
        HitEnd = hitEnd;
        QueryFrame = queryFrame;
        HitFrame = hitFrame;
        IdentitiesCount = identitiesCount;
        PositivesCount = positivesCount;
        Gaps = gaps;
        AlignmentLength = alignmentLength;
        Density = density;
        QuerySequence = querySequence;
        HitSequence = hitSequence;
        Midline = midline;
        PatternFrom = patternFrom;
        PatternTo = patternTo
    }

    // Defines the type Hit
    type Hit = {
        Number : int
        Id : string
        Def : string
        Accession : string
        Lenght : int64
        Hsps : seq<Hsp>}

    // Creates a value of type Hit
    let private createHit number id def accession lenght hsps =
        {Number = number;
        Id = id;
        Def = def;
        Accession = accession;
        Lenght = lenght;
        Hsps = hsps}

    // Defines the type Iteration
    type Iteration = {
        IterNumber : int
        IterID : string
        IterDef: string
        IterLenght: int
        Hits : seq<Hit>
        }

    // Creates a value of type Iteration
    let private createIteration iterNum iterId iterDef iterLen hits =
        {IterNumber = iterNum;
        IterID = iterId;
        IterDef = iterDef;
        IterLenght = iterLen;
        Hits = hits}

    // Defines the type BlastElements
    type BlastElements =
    | IterationElement of int * string * string * int
    | HitElement of int * string * string * string * int64
    | HspElement of Hsp
    | BlastInfoElement of string*string
    
    // Defines the type BlastPreResult
    type  BlastPreResult =
    | HitPreResult of Hit
    | BlastPreInfo of string*string
    | IterationPreResult of int * string * string * int

     // Defines the type BlastResult
    type BlastResult =
    | IterationResult of Iteration
    | BlastInfo of string*string

    // Reads all Hsp elements and forms a complete Hsp when the XmlReader reached the endelement Hsp
    let private getHsp (r:XmlReader) =
        let rec loop (r:XmlReader) eName number bitScore score eValue queryStart
                 queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                 positivesCount gaps alignmentLength density querySequence
                 hitSequence midline patternFrom patternTo =
            match r.Read() with
            | true -> match r.NodeType with
                      | XmlNodeType.Element                        -> loop r r.Name number bitScore score eValue queryStart
                                                                         queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                         positivesCount gaps alignmentLength density querySequence
                                                                         hitSequence midline patternFrom patternTo
                      | XmlNodeType.Text                           -> match eName with
                                                                        | "Hsp_num"             -> loop r r.Name (int r.Value) bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_bit-score"       -> loop r r.Name number (float r.Value) score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_score"           -> loop r r.Name number bitScore (float r.Value) eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_evalue"          -> loop r r.Name number bitScore score (float r.Value) queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_query-from"      -> loop r r.Name number bitScore score eValue (int64 r.Value)
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_query-to"        -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     (int64 r.Value) hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_hit-from"        -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd (int64 r.Value) hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_hit-to"          -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart (int64 r.Value) queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_query-frame"     -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd (int64 r.Value) hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_hit-frame"       -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame (int64 r.Value) identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_identity"        -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame (int64 r.Value)
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_positive"        -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     (int64 r.Value) gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_gaps"            -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount (int r.Value) alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_align-len"       -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps (int64 r.Value) density querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_density"         -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength (int r.Value) querySequence
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_qseq"            -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density (string r.Value)
                                                                                                     hitSequence midline patternFrom patternTo
                                                                        | "Hsp_hseq"            -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     (string r.Value) midline patternFrom patternTo
                                                                        | "Hsp_midline"         -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence (string r.Value) patternFrom patternTo
                                                                        | "Hsp_pattern-from"    -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline (int r.Value) patternTo
                                                                        | "Hsp_pattern-to"      -> loop r r.Name number bitScore score eValue queryStart
                                                                                                     queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                                                     positivesCount gaps alignmentLength density querySequence
                                                                                                     hitSequence midline patternFrom (int r.Value)                                                               
                                                                        | _                     -> failwithf "Error: Element %s is no Hsp-Element! Tough luck!" eName

                      | XmlNodeType.EndElement when r.Name = "Hsp" -> createHsp number bitScore score eValue queryStart queryEnd 
                                                                        hitStart hitEnd queryFrame hitFrame identitiesCount
                                                                        positivesCount gaps alignmentLength density querySequence
                                                                        hitSequence midline patternFrom patternTo
                      | _ -> loop r eName number bitScore score eValue queryStart
                                 queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount
                                 positivesCount gaps alignmentLength density querySequence
                                 hitSequence midline patternFrom patternTo
            | false -> failwithf "XmlError: Hsp Element was not closed."
                 

        loop r "" -1 -1. -1. -1. -1L -1L -1L -1L -1L -1L -1L -1L -1 -1L -1 "Not Found" "Not Found" "Not Found" -1 -1

    // Reads all Hit elements and forms a tuple of HitElements without any Hsp when the XmlReader reached the element Hit_hsps
    let private getHit (r:XmlReader) =
        let rec loop (r:XmlReader) eName number id def accession lenght =
            match r.Read() with
            | true -> match r.NodeType with
                      | XmlNodeType.Element when r.Name = "Hit_hsps"    -> HitElement(number,id, def, accession, lenght)
                      | XmlNodeType.Element                             -> loop r r.Name number id def accession lenght
                      | XmlNodeType.Text                                -> match eName with
                                                                            | "Hit_num"             -> loop r eName (int r.Value) id def accession lenght
                                                                            | "Hit_id"              -> loop r eName number (string r.Value) def accession lenght
                                                                            | "Hit_def"             -> loop r eName number id (string r.Value) accession lenght
                                                                            | "Hit_accession"       -> loop r eName number id def (string r.Value) lenght
                                                                            | "Hit_len"             -> loop r eName number id def accession (int64 r.Value)
                                                                            | _                     -> failwithf "Error: Element %s is no Hit-Element! Tough luck!" eName
                      | _                                               -> loop r eName number id def accession lenght
            | false -> failwithf "XmlError: Hit-Element was not closed."
        loop r "" -1 "Not Found" "Not Found" "Not Found" -1L

    // Reads all Iteration elements and forms a tuple of IterationElements without any Hit
    let private getIterationElement (r:XmlReader)  =
        let rec loop (r:XmlReader) eName iterNum iterId iterDef iterLen =
            match r.Read() with
            | true -> match r.NodeType with
                        | XmlNodeType.Element when r.Name = "Iteration_hits"    -> IterationElement(iterNum, iterId, iterDef, iterLen)
                        | XmlNodeType.Element                                   -> loop r r.Name iterNum iterId iterDef iterLen
                        | XmlNodeType.Text                                      -> match eName with
                                                                                    | "Iteration_iter-num"  -> loop r eName (int r.Value) iterId iterDef iterLen
                                                                                    | "Iteration_query-ID"  -> loop r eName iterNum (string r.Value) iterDef iterLen
                                                                                    | "Iteration_query-def" -> loop r eName iterNum iterId (string r.Value) iterLen
                                                                                    | "Iteration_query-len" -> loop r eName iterNum iterId iterDef (int r.Value)
                                                                                    | _                     -> failwithf "Error: Element %s is no Iteration-Element! Tough luck!" eName
                        | _                                                     -> loop r eName iterNum iterId iterDef iterLen
            | false -> failwithf "XmlError: Iteration-Element was not closed."
        loop r "Not Found" -1 "Not Found" "Not Found" -1

    // Saves all information in BlastInfoElement if it does not belong to Hit or Hsp
    let private getInfoElements (r:XmlReader) =
        let rec loop (r:XmlReader) name value =        
            match r.Read() with
            | true  ->  match r.NodeType with                            
                        | XmlNodeType.Element                                   -> loop r r.Name value
                        | XmlNodeType.Text                                      -> loop r name r.Value                    
                        | XmlNodeType.EndElement when r.Name = name             -> BlastInfoElement(name,value)
                        | _                                                     -> loop r name value  

            | false -> failwithf "Xml error: Element %s was not closed." name

        loop r r.Name ""

    // Returns true if the BlastElements is HitElement, IterationElement or BlastInfoElement.
    let private groupHit (l:BlastElements) =
        match l with
            | HitElement(_)         -> true
            | BlastInfoElement(_)   -> true
            | IterationElement(_)   -> true
            | _                     -> false
    
    // Returns true if the BlastPreResult is IterationPreResult or BlastPreInfo.
    let private groupIteration (l:BlastPreResult) =
        match l with
            | IterationPreResult(_) -> true
            | BlastPreInfo(_)       -> true
            | _                     -> false

    // Returns Hsp from HspElement
    let private getHspOfElements (l:BlastElements) =
        match l with
            | BlastElements.HspElement(hsp) -> Some(hsp)
            | _ -> None
       
    // Converts the BlastElements into BlastPreResult
    let private convertBlastElements (input:seq<BlastElements>) =
        let en = input.GetEnumerator()
        match en.MoveNext() with
        | true  -> match en.Current with
                   | HitElement(number, id, def, accession, lenght)         -> HitPreResult(createHit number id def accession lenght (input |> Seq.choose getHspOfElements))
                   | BlastInfoElement(string1, string2)                     -> BlastPreInfo(string1,string2)
                   | IterationElement(iterNum, iterId, iterDef, iterLen)    -> IterationPreResult(iterNum, iterId, iterDef, iterLen)
                   | _                                                      -> failwithf "Error: Element not compatible!"
        | false -> failwithf "Error: Element not compatible!"

    // Returns Hit from HitPreResult
    let private getHitOfPreResults (l: BlastPreResult) =
        match l with
            | BlastPreResult.HitPreResult(hit)  -> Some(hit)
            | _                                 -> None

    // Converts the BlastPreResult into BlastResult
    let private convertBlastPreResult (input:seq<BlastPreResult>) =
        let en = input.GetEnumerator()
        match en.MoveNext() with
        | true  -> match en.Current with
                    | IterationPreResult (iterNum, iterId, iterDef, iterLen)    -> IterationResult(createIteration iterNum iterId iterDef iterLen (input |> Seq.choose getHitOfPreResults))
                    | BlastPreInfo(string1, string2)                            -> BlastInfo(string1, string2)
                    | _                                                         -> failwithf "Error: Element not compatible!"
        | false -> failwithf "Error: Element not compatible!"

    /// Reads the Xml-file and goes through each element. Thereafter the BlastElements are grouped and gives back a Sequence of the type BlastResult
    let readBlastXML (xmlPath:string) =

        let settings = new XmlReaderSettings()
        let _ = settings.DtdProcessing <- DtdProcessing.Ignore
    
        let reader = XmlReader.Create(xmlPath,settings)
        let rec loop  (r:XmlReader) = seq { 
            match r.Read() with
            | true ->  match r.NodeType with
                       | XmlNodeType.Element when r.Name = "BlastOutput_iterations" -> ()
                       | XmlNodeType.Element when r.Name = "Iteration"              -> yield getIterationElement r
                       | XmlNodeType.Element when r.Name = "Hit"                    -> yield getHit r
                       | XmlNodeType.Element when r.Name = "Hsp"                    -> yield HspElement(getHsp r)
                       | XmlNodeType.Element                                        -> yield getInfoElements r
                       | _ -> ()
                       yield! loop r
            | false -> ()
        
            }    
        loop reader
        |> Seq.groupWhen groupHit
        |> Seq.map convertBlastElements
        |> Seq.groupWhen groupIteration
        |> Seq.map convertBlastPreResult