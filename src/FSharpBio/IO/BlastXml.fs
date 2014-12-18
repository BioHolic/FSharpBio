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
    let createHsp number bitScore score eValue queryStart queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount positivesCount gaps alignmentLength density querySequence hitSequence midline patternFrom patternTo =
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
    let createHit number id def accession lenght hsps =
        {Number = number;
        Id = id;
        Def = def;
        Accession = accession;
        Lenght = lenght;
        Hsps = hsps}

    // Defines the type BlastElements
    type BlastElements =
    | HitElement of int * string * string * string * int64
    | HspElement of Hsp
    | BlastInfoElement of string*string

    // Defines the type BlastResult
    type  BlastResult =
    | HitResult of Hit
    | BlastInfo of string*string

    // Reads all Hsp elements and forms a complete Hsp when the XmlReader reached the endelement Hsp
    let getHsp (r:XmlReader) =
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
    let getHit (r:XmlReader) =
        let rec loop (r:XmlReader) eName number id def accession lenght =
            match r.Read() with
            | true -> match r.NodeType with

                      | XmlNodeType.Element when r.Name = "Hit_hsps"    -> HitElement(number,id, def, accession, lenght)
                      | XmlNodeType.Element                             -> loop r r.Name number id def accession lenght
                      | XmlNodeType.Text                                -> match eName with
                                                                            | "Hit_num"             -> loop r r.Name (int r.Value) id def accession lenght
                                                                            | "Hit_id"              -> loop r r.Name number (string r.Value) def accession lenght
                                                                            | "Hit_def"             -> loop r r.Name number id (string r.Value) accession lenght
                                                                            | "Hit_accession"       -> loop r r.Name number id def (string r.Value) lenght
                                                                            | "Hit_len"             -> loop r r.Name number id def accession (int64 r.Value)
                                                                            | _                     -> failwithf "Error: Element %s is no Hit-Element! Tough luck!" eName
                      | _                                               -> loop r eName number id def accession lenght
            | false -> failwithf "XmlError: Hit Element was not closed."
        loop r "" -1 "Not Found" "Not Found" "Not Found" -1L

    // Saves all information in BlastInfoElement if it does not belong to Hit or Hsp
    let getInfoElements (r:XmlReader) =
        let rec loop (r:XmlReader) name value =        
            match r.Read() with
            | true  ->  match r.NodeType with                            
                        | XmlNodeType.Element                                   -> loop r r.Name value
                        | XmlNodeType.Text                                      -> loop r name r.Value                    
                        | XmlNodeType.EndElement when r.Name = name             -> BlastInfoElement(name,value)
                        | _                                                     -> loop r name value  

            | false -> failwithf "Xml error: Element %s was not closed." name

        loop r r.Name ""

    // Returns true if the BlastElements is HitElement or BlastInfoElement.
    let group (l:BlastElements) =
        match l with
            | HitElement(_) -> true
            | BlastInfoElement(_) -> true
            | _ -> false

    // Returns Hsp from HspElement
    let getHspOfElements (l:BlastElements) =
        match l with
            | BlastElements.HspElement(hsp) -> Some(hsp)
            | _ -> None
    
    // Converts the BlastElements into BlastResult
    let convertBlastElements (input:seq<BlastElements>) =
        let en = input.GetEnumerator()
        match en.MoveNext() with
        | true  -> match en.Current with
                   | HitElement(number, id, def, accession, lenght) -> HitResult(createHit number id def accession lenght (input |> Seq.choose getHspOfElements))
                   | BlastInfoElement(string1, string2)             -> BlastInfo(string1,string2)
                   |_                                               -> failwithf "Error: Element not compatible!"

        | false -> failwithf "Error: Element not compatible!"

    // Reads the Xml-file and goes through each element. Thereafter the BlastElements are grouped
    let readBlastXML (xmlPath:string) =

        let settings = new XmlReaderSettings()
        let _ = settings.DtdProcessing <- DtdProcessing.Ignore
    
        let reader = XmlReader.Create(xmlPath,settings)
        let rec loop  (r:XmlReader) = seq { 
            match r.Read() with
            | true ->  match r.NodeType with
                       | XmlNodeType.Element when r.Name = "Hit"               -> yield getHit r
                       | XmlNodeType.Element when r.Name = "Hsp"               -> yield HspElement(getHsp r)
                       | XmlNodeType.Element when r.Name = "Iteration_hits"    -> ()
                       | XmlNodeType.Element                                   -> yield getInfoElements r
                       | _ -> ()
                       yield! loop r
            | false -> ()
        
            }    
        loop reader
        |> Seq.groupWhen group
        |> Seq.map convertBlastElements

    


    



//open System.Xml
//open System.IO
//
//
//module BlastXml =
//    
//    // Definierung des Typs Hsp
//    type Hsp = {
//        Number : int
//        BitScore : float
//        Score : float
//        EValue : float
//        QueryStart : int64
//        QueryEnd : int64
//        HitStart : int64
//        HitEnd : int64
//        QueryFrame : int64
//        HitFrame : int64
//        IdentitiesCount : int64
//        PositivesCount : int64
//        Gaps : int
//        AlignmentLength : int64
//        Density : int
//        QuerySequence : string
//        HitSequence : string
//        Midline : string
//        PatternFrom : int
//        PatternTo : int
//        }
//
//
//    // Erstellt ein Value des Typs Hsp
//    let createHsp number bitScore score eValue queryStart queryEnd hitStart hitEnd queryFrame hitFrame identitiesCount positivesCount gaps alignmentLength density querySequence hitSequence midline patternFrom patternTo =
//        {Number = number;
//        BitScore = bitScore;
//        Score = score;
//        EValue = eValue;
//        QueryStart = queryStart;
//        QueryEnd = queryEnd;
//        HitStart = hitStart;
//        HitEnd = hitEnd;
//        QueryFrame = queryFrame;
//        HitFrame = hitFrame;
//        IdentitiesCount = identitiesCount;
//        PositivesCount = positivesCount;
//        Gaps = gaps;
//        AlignmentLength = alignmentLength;
//        Density = density;
//        QuerySequence = querySequence;
//        HitSequence = hitSequence;
//        Midline = midline;
//        PatternFrom = patternFrom;
//        PatternTo = patternTo
//    }
//
//    // Erstellt einen Value des Typs Hsp, wenn der Input nur aus Strings besteht
//    let createHspOfString (number:string) (bitScore:string) (score:string) (eValue:string) (queryStart:string) (queryEnd:string) (hitStart:string) (hitEnd:string) (queryFrame:string) (hitFrame:string) (identitiesCount:string) (positivesCount:string) (gaps:string) (alignmentLength:string) (density:string) (querySequence:string) (hitSequence:string) (midline:string) (patternFrom:string) (patternTo:string) =
//        createHsp (int number) (float bitScore) (float score) (float eValue) (int64 queryStart) (int64 queryEnd) (int64 hitStart) (int64 hitEnd) (int64 queryFrame) (int64 hitFrame) (int64 identitiesCount) (int64 positivesCount) (int gaps) (int64 alignmentLength) (int density) querySequence hitSequence midline (int patternFrom) (int patternTo)
//
//    // Definierung des Typs Hit
//    type Hit = {
//        Number : int
//        Id : string
//        Def : string
//        Accession : string
//        Lenght : int64
//        Hsp : seq<Hsp>}
//
//    // Erstellt ein Value des Typs Hit
//    let createHit number id def accession lenght hsp =
//        {Number = number;
//        Id = id;
//        Def = def;
//        Accession = accession;
//        Lenght = lenght;
//        Hsp = hsp}
//
//    // Erstellt einen Value des Typs Hit, wenn der Input aus Strings und einer Liste von Hsps besteht
//    let createHitOfStringAndHspList (number:string) (id:string) (def:string) (accession:string) (lenght:string) (hsp:List<Hsp>) =
//        createHit (int number) id def accession (int64 lenght) hsp
//
//    // Definierung des Typs BlastOutput
//    type BlastOutput = {
//        BlastInfo : List<string * string>
//        Hits : seq<Hit>}
//
//    // Erstellt einen Value des Typs BlastOutput, wenn der Input aus List<string*string> und seq<Hits> besteht 
//    let createBlastOutput blastInfo hits =
//        {
//        BlastInfo = blastInfo;
//        Hits = hits}
//
//
//    // Überprüft ob die Map den Key enthält. Bei Fehlen des Keys wird der defaultValue zurückgegeben
//    let private containKey (rawHspMap:Map<'Key,'T>) (defaultValue:'T)  (key:'Key)  = 
//        match rawHspMap.ContainsKey(key) with
//        | true  -> rawHspMap.[key]
//        | false -> defaultValue
//
//
//
//
//    // Umwandlung von Typ Maps in Typ Hsp
//    let private hspFromKeyValuePairs (data:List<string*string>) = 
//        let m = Map.ofList data
//        createHspOfString (containKey m "-1" "Hsp_num") (containKey m "-1" "Hsp_bit-score") (containKey m "-1" "Hsp_score")
//            (containKey m "-1" "Hsp_evalue") (containKey m "-1" "Hsp_query-from") (containKey m "-1" "Hsp_query-to") 
//            (containKey m "-1" "Hsp_hit-from") (containKey m "-1" "Hsp_hit-to") (containKey m "-1" "Hsp_query-frame")
//            (containKey m "-1" "Hsp_hit-frame") (containKey m "-1" "Hsp_identity") (containKey m "-1" "Hsp_positive")
//            (containKey m "-1" "Hsp_gaps") (containKey m "-1" "Hsp_align-len") (containKey m "-1" "Hsp_density")
//            (containKey m "Not Found" "Hsp_qseq") (containKey m "Not Found" "Hsp_hseq") (containKey m "Not Found" "Hsp_midline")
//            (containKey m "-1" "Hsp_pattern-from") (containKey m "-1" "Hsp_pattern-to")
//
//    // Umwandlung von Typ List<Hsp> und List<string*string> in Typ Hit
//    let private createNewHit (hsps:List<Hsp>) (data:List<string*string>) = 
//        let m = Map.ofList data
//        createHitOfStringAndHspList (containKey m "-1" "Hit_num") (containKey m "Not Found" "Hit_id") (containKey m "Not Found" "Hit_def")
//            (containKey m "Not Found" "Hit_accession") (containKey m "-1" "Hit_len") hsps
//
//
//    // Sammeln der Subelemente eines Hsps und die daraus folgende Erzeugung
//    let rec private parseHspSubElements (r:XmlReader) out name value =
//        match r.Read(), r.NodeType with
//        | true, XmlNodeType.Element                         -> parseHspSubElements r out r.Name value
//        | true, XmlNodeType.Text                            -> parseHspSubElements r out name r.Value
//        | true, XmlNodeType.EndElement when r.Name = "Hsp"  -> hspFromKeyValuePairs out
//        | true, XmlNodeType.EndElement                      -> parseHspSubElements r ((name, value)::out) name value
//        | false, _                                          -> failwithf "Xml error: Element %s was not closed." name
//        | _, _                                              -> parseHspSubElements r out name value
//
//    // Sammeln der Subelemente eines Hits und und die daraus folgende Erzeugung
//    let rec private parseHitSubElements (r:XmlReader) (outHsp:List<Hsp>) (out:List<string*string>) name value =
//        match r.Read(), r.NodeType with
//        | true, XmlNodeType.Element when r.Name = "Hsp"         -> parseHitSubElements r (parseHspSubElements r [] "" ""::outHsp) out name value
//        | true, XmlNodeType.Element                             -> parseHitSubElements r outHsp out r.Name value
//        | true, XmlNodeType.Text                                -> parseHitSubElements r outHsp out name r.Value
//        | true, XmlNodeType.EndElement when r.Name = "Hit"      -> createNewHit outHsp out
//        | true, XmlNodeType.EndElement                          -> parseHitSubElements r outHsp ((name, value)::out) name value
//        | false, _                                              -> failwithf "Xml file was corrupted"
//        | _, _                                                  -> parseHitSubElements r outHsp out name value
//
//    let readBlastXML (xmlPath:string) =
//    
//
//        let settings = new XmlReaderSettings()
//        let _ = settings.DtdProcessing <- DtdProcessing.Ignore
//        let xmlHspRead = XmlReader.Create(xmlPath, settings)
//
//
//        let rec parseHits (r:XmlReader) out (outHits:seq<Hit>) name value =
//            match r.Read(), r.NodeType with
//            | true, XmlNodeType.Element when r.Name = "Hit"                 -> parseHits r out (Seq.append outHits [parseHitSubElements r [] [] "" ""]) name value
//            | true, XmlNodeType.EndElement when r.Name = "BlastOutput"      -> createBlastOutput (out |> List.rev) outHits
//            | true, XmlNodeType.Element                                     -> parseHits r out outHits r.Name value
//            | true, XmlNodeType.Text                                        -> parseHits r out outHits name r.Value
//            | true, XmlNodeType.EndElement when r.Name = name               -> parseHits r ((name, value)::out) outHits name value
//            | true, _                                                       -> parseHits r out outHits name value
//            | false, _                                                      -> failwithf "Xml file was corrupted"
//
//        parseHits xmlHspRead [] [] "" ""
//
//    