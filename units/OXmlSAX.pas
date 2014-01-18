unit OXmlSAX;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlSAX.pas

  SAX implementation.

  Event-based XML parser.
    -> Events in FPC or older Delphi versions.
    -> Events + anonymous methods in D2009+.
    -> Use the StopParsing() procedure to pause the parsing.
       Parsing can be continued by calling TSAXParser.ContinueParsing() again.

}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  SysUtils, Classes, OWideSupp, OXmlUtils, OXmlReadWrite, OEncoding,
  OHashedStrings;

type
  TSAXParser = class;
  TSAXAttribute = record
    AttrName: OWideString;
    AttrValue: OWideString;
  end;
  TSAXAttributeEnum = class;
  TSAXAttributes = class(TOHashedStringDictionary)
  private
    function GetAttribute(const aIndex: OHashedStringsIndex): TSAXAttribute;
  public
    property Attributes[const aIndex: OHashedStringsIndex]: TSAXAttribute read GetAttribute;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TSAXAttributeEnum;
    {$ENDIF}
  end;
  TSAXAttributeEnum = class(TObject)
  private
    fIndex: OHashedStringsIndex;
    fSAXAttributes: TSAXAttributes;
  public
    constructor Create(aSAXAttributes: TSAXAttributes);
    function GetCurrent: TSAXAttribute;
    function MoveNext: Boolean;
  public
    property Current: TSAXAttribute read GetCurrent;
  end;

  TSAXNotifyEvent = procedure(Sender: TSAXParser) of Object;
  TSAXTextEvent = procedure(Sender: TSAXParser; const aText: OWideString) of Object;
  TSAXStartElementEvent = procedure(Sender: TSAXParser; const aName: OWideString;
    const aAttributes: TSAXAttributes) of Object;
  TSAXEndElementEvent = procedure(Sender: TSAXParser; const aName: OWideString) of Object;
  TSAXProcessingInstructionEvent = procedure(Sender: TSAXParser; const aTarget, aContent: OWideString) of Object;

  {$IFDEF O_ANONYMOUS_METHODS}
  TSAXNotifyProc = reference to procedure(aSaxParser: TSAXParser);
  TSAXTextProc = reference to procedure(aSaxParser: TSAXParser;
    const aText: OWideString);
  TSAXStartElementProc = reference to procedure(aSaxParser: TSAXParser;
    const aName: OWideString; const aAttributes: TSAXAttributes);
  TSAXElementProc = reference to procedure(aSaxParser: TSAXParser;
    const aName: OWideString);
  TSAXProcessingInstructionProc = reference to procedure(aSaxParser: TSAXParser;
    const aTarget, aContent: OWideString);
  {$ENDIF}

  TSAXParser = class(TObject)
  private
    fReader: TXMLReader;
    fReaderSettings: TXMLReaderSettings;
    fDataRead: Boolean;
    fStopParsing: Boolean;

    fOnStartDocument: TSAXNotifyEvent;
    fOnEndDocument: TSAXNotifyEvent;
    fOnCharacters: TSAXTextEvent;
    fOnComment: TSAXTextEvent;
    fOnProcessingInstruction: TSAXProcessingInstructionEvent;
    fOnStartElement: TSAXStartElementEvent;
    fOnEndElement: TSAXEndElementEvent;

    {$IFDEF O_ANONYMOUS_METHODS}
    fStartDocumentProc: TSAXNotifyProc;
    fEndDocumentProc: TSAXNotifyProc;
    fCharactersProc: TSAXTextProc;
    fCommentProc: TSAXTextProc;
    fProcessingInstructionProc: TSAXProcessingInstructionProc;
    fStartElementProc: TSAXStartElementProc;
    fEndElementProc: TSAXElementProc;
    {$ENDIF}

    procedure DoOnStartDocument;
    procedure DoOnEndDocument;
    procedure DoOnCharacters(const aText: OWideString);
    procedure DoOnComment(const aText: OWideString);
    procedure DoOnProcessingInstruction(const aTarget, aContent: OWideString);
    procedure DoOnStartElement(const aName: OWideString;
      const aAttributes: TSAXAttributes);
    procedure DoOnEndElement(const aName: OWideString);

    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
    function GetApproxStreamPosition: ONativeInt;
    function GetStreamSize: ONativeInt;
  protected
    function StartParsing: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //The Parse* functions open an XML document and start parsing it
    //  they return "True" if the document was sucessfully parsed to the end
    //  (they return "False" if the parsing has been stopped with aStop parameter)

    //parse document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseFile(const aFileName: String; const aForceEncoding: TEncoding = nil): Boolean;
    //parse document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //parse XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function ParseXML(const aXML: OWideString): Boolean;
    {$IFDEF O_RAWBYTESTRING}
    function ParseXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_GENERICBYTES}
    //parse document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}
  public
    //call StopParsing from an event or anonymous method to stop parsing
    //  When stopped, parsing cannot be continued again.
    procedure StopParsing;
  public
    //root element was found -> it's not possible to stop here!
    property OnStartDocument: TSAXNotifyEvent read fOnStartDocument write fOnStartDocument;
    //reached the end of the document
    property OnEndDocument: TSAXNotifyEvent read fOnEndDocument write fOnEndDocument;
    //text or CData
    property OnCharacters: TSAXTextEvent read fOnCharacters write fOnCharacters;
    //comment
    property OnComment: TSAXTextEvent read fOnComment write fOnComment;
    //Processing Instruction
    property OnProcessingInstruction: TSAXProcessingInstructionEvent read fOnProcessingInstruction write fOnProcessingInstruction;
    //start of an element
    property OnStartElement: TSAXStartElementEvent read fOnStartElement write fOnStartElement;//element header <a href="title">
    //end of an element
    property OnEndElement: TSAXEndElementEvent read fOnEndElement write fOnEndElement;//element end </a> or <a />

    {$IFDEF O_ANONYMOUS_METHODS}
    //root element was found -> it's not possible to stop here!
    property StartDocumentProc: TSAXNotifyProc read fStartDocumentProc write fStartDocumentProc;
    //reached the end of the document
    property EndDocumentProc: TSAXNotifyProc read fEndDocumentProc write fEndDocumentProc;
    //text or CData
    property CharactersProc: TSAXTextProc read fCharactersProc write fCharactersProc;
    //comment
    property CommentProc: TSAXTextProc read fCommentProc write fCommentProc;
    //Processing Instruction
    property ProcessingInstructionProc: TSAXProcessingInstructionProc read fProcessingInstructionProc write fProcessingInstructionProc;
    //start of an element
    property StartElementProc: TSAXStartElementProc read fStartElementProc write fStartElementProc;
    //end of an element
    property EndElementProc: TSAXElementProc read fEndElementProc write fEndElementProc;
    {$ENDIF}

    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read fReaderSettings;
  public
    //following functions and properties can be called only from events or anonymous methods during parsing

    //functions to work with the current path in the XML document
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathMatch(const aNodePath: Array of OWideString): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
    function NodePathAsString: OWideString;

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: ONativeInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: ONativeInt read GetStreamSize;
  end;

  ESAXParserException = class(Exception);

implementation

{ TSAXParser }

constructor TSAXParser.Create;
begin
  inherited Create;

  fReaderSettings := TXMLReaderSettings.Create;
  fReaderSettings.NodePathHandling := npFull;
end;

destructor TSAXParser.Destroy;
begin
  fReaderSettings.Free;

  inherited;
end;

procedure TSAXParser.DoOnCharacters(const aText: OWideString);
begin
  if Assigned(fOnCharacters) then
    fOnCharacters(Self, aText);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fCharactersProc) then
    fCharactersProc(Self, aText);
  {$ENDIF}
end;

procedure TSAXParser.DoOnComment(const aText: OWideString);
begin
  if Assigned(fOnComment) then
    fOnComment(Self, aText);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fCommentProc) then
    fCommentProc(Self, aText);
  {$ENDIF}
end;

procedure TSAXParser.DoOnEndDocument;
begin
  if Assigned(fOnEndDocument) then
    fOnEndDocument(Self);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fEndDocumentProc) then
    fEndDocumentProc(Self);
  {$ENDIF}
end;

procedure TSAXParser.DoOnEndElement(const aName: OWideString);
begin
  if Assigned(fOnEndElement) then
    fOnEndElement(Self, aName);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fEndElementProc) then
    fEndElementProc(Self, aName);
  {$ENDIF}
end;

procedure TSAXParser.DoOnProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if Assigned(fOnProcessingInstruction) then
    fOnProcessingInstruction(Self, aTarget, aContent);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fProcessingInstructionProc) then
    fProcessingInstructionProc(Self, aTarget, aContent);
  {$ENDIF}
end;

procedure TSAXParser.DoOnStartDocument;
begin
  if Assigned(fOnStartDocument) then
    fOnStartDocument(Self);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fStartDocumentProc) then
    fStartDocumentProc(Self);
  {$ENDIF}
end;

procedure TSAXParser.DoOnStartElement(const aName: OWideString;
  const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnStartElement) then
    fOnStartElement(Self, aName, aAttributes);

  {$IFDEF O_ANONYMOUS_METHODS}
  if Assigned(fStartElementProc) then
    fStartElementProc(Self, aName, aAttributes);
  {$ENDIF}
end;

function TSAXParser.GetApproxStreamPosition: ONativeInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TSAXParser.GetNodePath(const aIndex: Integer): OWideString;
begin
  Result := fReader.NodePath[aIndex];
end;

function TSAXParser.GetNodePathCount: Integer;
begin
  Result := fReader.NodePathCount;
end;

function TSAXParser.GetStreamSize: ONativeInt;
begin
  Result := fReader.StreamSize;
end;

{$IFDEF O_GENERICBYTES}
function TSAXParser.ParseBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
var
  xLength: Integer;
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xLength := Length(aBuffer);
    if xLength > 0 then
      xStream.SetPointer(@aBuffer[0], xLength);

    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

function TSAXParser.ParseFile(const aFileName: String;
  const aForceEncoding: TEncoding): Boolean;
var
  xStream: TFileStream;
begin
  xStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
begin
  fReader := TXMLReader.Create;
  try
    fReader.ReaderSettings.Assign(fReaderSettings);

    fReader.InitStream(aStream, aForceEncoding);

    Result := StartParsing;
  finally
    fReader.Free;
    fReader := nil;
  end;
end;

function TSAXParser.ParseXML(const aXML: OWideString): Boolean;
var
  xLength: Integer;
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xLength := Length(aXML);
    if xLength > 0 then
      xStream.SetPointer(@aXML[1], xLength * SizeOf(OWideChar));

    Result := ParseStream(xStream, TEncoding.OWideStringEncoding);
  finally
    xStream.Free;
  end;
end;

{$IFDEF O_RAWBYTESTRING}
function TSAXParser.ParseXML_UTF8(const aXML: ORawByteString): Boolean;
var
  xLength: Integer;
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xLength := Length(aXML);
    if xLength > 0 then
      xStream.SetPointer(@aXML[1], xLength);

    Result := ParseStream(xStream, TEncoding.UTF8);
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

procedure TSAXParser.NodePathAssignTo(const aNodePath: TOWideStringList);
begin
  fReader.NodePathAssignTo(aNodePath);
end;

function TSAXParser.NodePathAsString: OWideString;
begin
  Result := fReader.NodePathAsString;
end;

function TSAXParser.NodePathMatch(
  const aNodePath: array of OWideString): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.NodePathMatch(const aNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.NodePathMatch(const aNodePath: OWideString): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.StartParsing: Boolean;
var
  xAttributes: TSAXAttributes;
  xReaderToken: TXMLReaderToken;
begin
  fDataRead := False;
  fStopParsing := False;

  if fReader.ReaderSettings.NodePathHandling = npNo then//you cannot use npNo with SAXParser -> node names wouldn't be read
    fReader.ReaderSettings.NodePathHandling := npLastPath;

  fStopParsing := False;

  xAttributes := TSAXAttributes.Create;
  try
    xReaderToken := fReader.ReaderToken;
    while (not fStopParsing) and fReader.ReadNextToken do begin
      case xReaderToken.TokenType of
        rtOpenElement: begin
          xAttributes.Clear;
          if not fDataRead then begin
            DoOnStartDocument;
            fDataRead := True;
          end;
        end;
        rtAttribute: begin
          xAttributes.Add(xReaderToken.TokenName, xReaderToken.TokenValue);
        end;
        rtFinishOpenElementClose: begin
          DoOnStartElement(xReaderToken.TokenName, xAttributes);
          DoOnEndElement(xReaderToken.TokenName);
        end;
        rtFinishOpenElement: DoOnStartElement(xReaderToken.TokenName, xAttributes);
        rtCloseElement: DoOnEndElement(xReaderToken.TokenName);
        rtText, rtCData:
          if fDataRead or not OXmlIsWhiteSpace(xReaderToken.TokenValue)
          then//omit empty text before root node
            DoOnCharacters(xReaderToken.TokenValue);
        rtComment: DoOnComment(xReaderToken.TokenValue);
        rtProcessingInstruction: DoOnProcessingInstruction(xReaderToken.TokenName, xReaderToken.TokenValue);
      end;
    end;

    if fDataRead and not fStopParsing then
    begin
      DoOnEndDocument;
    end;
  finally
    xAttributes.Free;
  end;

  Result := not fStopParsing;
end;

function TSAXParser.RefIsChildOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.RefIsChildOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsParentOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.RefIsParentOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsChildOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
begin
  Result := fReader.RefIsChildOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsParentOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
begin
  Result := fReader.RefIsParentOfNodePath(aRefNodePath);
end;

procedure TSAXParser.StopParsing;
begin
  fStopParsing := True;
end;

{ TSAXAttributes }

function TSAXAttributes.GetAttribute(
  const aIndex: OHashedStringsIndex): TSAXAttribute;
begin
  Result.AttrName := Keys[aIndex];
  Result.AttrValue := Values[aIndex];
end;

{$IFDEF O_ENUMERATORS}
function TSAXAttributes.GetEnumerator: TSAXAttributeEnum;
begin
  Result := TSAXAttributeEnum.Create(Self);
end;
{$ENDIF}

{ TSAXAttributeEnum }

constructor TSAXAttributeEnum.Create(aSAXAttributes: TSAXAttributes);
begin
  inherited Create;

  fIndex := -1;
  fSAXAttributes := aSAXAttributes;
end;

function TSAXAttributeEnum.GetCurrent: TSAXAttribute;
begin
  Result := fSAXAttributes.Attributes[fIndex];
end;

function TSAXAttributeEnum.MoveNext: Boolean;
begin
  Result := (fIndex < fSAXAttributes.Count - 1);
  if Result then
    Inc(fIndex);
end;

end.
