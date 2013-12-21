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
    -> use the aStop parameter to pause the parsing
       parsing can be continued by calling TSAXParser.Parse() again.

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
  TSAXAttribute = TOHashedStringDictionaryEnumPair;//Key = NodeName, Value = NodeValue
  TSAXAttributes = TOHashedStringDictionary;

  TSAXNotifyEvent = procedure(Sender: TSAXParser) of Object;
  TSAXTextEvent = procedure(Sender: TSAXParser; const aText: OWideString; var aStop: Boolean) of Object;
  TSAXStartElementEvent = procedure(Sender: TSAXParser; const aName: OWideString;
    const aAttributes: TSAXAttributes; var aStop: Boolean) of Object;
  TSAXEndElementEvent = procedure(Sender: TSAXParser; const aName: OWideString; var aStop: Boolean) of Object;
  TSAXProcessingInstructionEvent = procedure(Sender: TSAXParser; const aTarget, aContent: OWideString; var aStop: Boolean) of Object;

  {$IFDEF O_DELPHI_2009_UP}
  TSAXNotifyProc = reference to procedure;
  TSAXTextProc = reference to procedure(const aText: OWideString; var aStop: Boolean);
  TSAXStartElementProc = reference to procedure(const aName: OWideString;
    const aAttributes: TSAXAttributes; var aStop: Boolean);
  TSAXElementProc = reference to procedure(const aName: OWideString; var aStop: Boolean);
  TSAXProcessingInstructionProc = reference to procedure(const aTarget, aContent: OWideString; var aStop: Boolean);
  {$ENDIF}

  TSAXParser = class(TObject)
  private
    fReader: TOXmlReader;
    fStream: TStream;
    fOwnsStream: Boolean;
    fDataRead: Boolean;

    fOnStartDocument: TSAXNotifyEvent;
    fOnEndDocument: TSAXNotifyEvent;
    fOnCharacters: TSAXTextEvent;
    fOnComment: TSAXTextEvent;
    fOnProcessingInstruction: TSAXProcessingInstructionEvent;
    fOnStartElement: TSAXStartElementEvent;
    fOnEndElement: TSAXEndElementEvent;

    {$IFDEF O_DELPHI_2009_UP}
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
    procedure DoOnCharacters(const aText: OWideString; var aStop: Boolean);
    procedure DoOnComment(const aText: OWideString; var aStop: Boolean);
    procedure DoOnProcessingInstruction(const aTarget, aContent: OWideString; var aStop: Boolean);
    procedure DoOnStartElement(const aName: OWideString;
      const aAttributes: TSAXAttributes; var aStop: Boolean);
    procedure DoOnEndElement(const aName: OWideString; var aStop: Boolean);

    function GetBreakReading: TXmlBreakReading;
    function GetEntityList: TOXmlReaderEntityList;
    function GetLineBreak: TXmlLineBreak;
    procedure SetBreakReading(const aBreakReading: TXmlBreakReading);
    procedure SetLineBreak(const aLineBreak: TXmlLineBreak);

    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
  protected
    procedure DoCreate(const aForceEncoding: TEncoding); virtual;
    procedure DoDestroy; virtual;
  public
    //Parse the document
    //  returns true if parsing was stopped before the end of the file
    function Parse: Boolean;

    //following are functions to work with the current path in the XML document
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathMatch(const aNodePath: Array of OWideString): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
    function NodePathAsString: OWideString;
  public
    //aForceEncoding - if set, parser will ignore <?xml encoding="???" ?>
    constructor Create(const aStream: TStream;
      const aForceEncoding: TEncoding = nil);
    constructor CreateFromFile(const aFileName: String;
      const aForceEncoding: TEncoding = nil);
    {$IFDEF O_DELPHI_2009_UP}
    constructor CreateFromBuffer(const aBuffer: TBytes;
      const aForceEncoding: TEncoding = nil);
    {$ENDIF}
    constructor CreateFromXML(const aXML: OWideString);

    destructor Destroy; override;
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

    {$IFDEF O_DELPHI_2009_UP}
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

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;

    //process known entities. add user-defined entities here
    property EntityList: TOXmlReaderEntityList read GetEntityList;
    //decide if you want to read the document after the root element has been closed
    property BreakReading: TXmlBreakReading read GetBreakReading write SetBreakReading;
    //process line breaks (#10, #13, #13#10) to the LineBreak character.
    //  set to empty string ('') if you don't want any processing
    //  default is your OS line break (sLineBreak)
    property LineBreak: TXmlLineBreak read GetLineBreak write SetLineBreak;
  end;

implementation

{ TSAXParser }

constructor TSAXParser.Create(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  inherited Create;

  fStream := aStream;
  fOwnsStream := False;

  DoCreate(aForceEncoding);
end;

constructor TSAXParser.CreateFromFile(const aFileName: String;
  const aForceEncoding: TEncoding);
begin
  inherited Create;

  fStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  fOwnsStream := True;

  DoCreate(aForceEncoding);
end;

constructor TSAXParser.CreateFromXML(const aXML: OWideString);
var
  xLength: Integer;
begin
  inherited Create;

  fStream := TMemoryStream.Create;
  fOwnsStream := True;

  xLength := Length(aXML);
  if xLength > 0 then
    fStream.WriteBuffer(aXML[1], xLength * SizeOf(OWideChar));
  fStream.Position := 0;

  DoCreate(TEncoding.OWideStringEncoding);
end;

{$IFDEF O_DELPHI_2009_UP}
constructor TSAXParser.CreateFromBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
var
  xLength: Integer;
begin
  inherited Create;

  fStream := TMemoryStream.Create;
  fOwnsStream := True;

  xLength := Length(aBuffer);
  if xLength > 0 then
    fStream.WriteBuffer(aBuffer[0], xLength);
  fStream.Position := 0;

  DoCreate(aForceEncoding);
end;
{$ENDIF}

destructor TSAXParser.Destroy;
begin
  DoDestroy;

  inherited;
end;

procedure TSAXParser.DoCreate(const aForceEncoding: TEncoding);
begin
  fReader := TOXmlReader.Create(fStream, aForceEncoding);
  fReader.BreakReading := brNone;
end;

procedure TSAXParser.DoDestroy;
begin
  fReader.Free;
  if fOwnsStream then
    fStream.Free;
end;

procedure TSAXParser.DoOnCharacters(const aText: OWideString; var aStop: Boolean);
begin
  if Assigned(fOnCharacters) then
    fOnCharacters(Self, aText, aStop);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fCharactersProc) then
    fCharactersProc(aText, aStop);
  {$ENDIF}
end;

procedure TSAXParser.DoOnComment(const aText: OWideString; var aStop: Boolean);
begin
  if Assigned(fOnComment) then
    fOnComment(Self, aText, aStop);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fCommentProc) then
    fCommentProc(aText, aStop);
  {$ENDIF}
end;

procedure TSAXParser.DoOnEndDocument;
begin
  if Assigned(fOnEndDocument) then
    fOnEndDocument(Self);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fEndDocumentProc) then
    fEndDocumentProc;
  {$ENDIF}
end;

procedure TSAXParser.DoOnEndElement(const aName: OWideString; var aStop: Boolean);
begin
  if Assigned(fOnEndElement) then
    fOnEndElement(Self, aName, aStop);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fEndElementProc) then
    fEndElementProc(aName, aStop);
  {$ENDIF}
end;

procedure TSAXParser.DoOnProcessingInstruction(const aTarget,
  aContent: OWideString; var aStop: Boolean);
begin
  if Assigned(fOnProcessingInstruction) then
    fOnProcessingInstruction(Self, aTarget, aContent, aStop);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fProcessingInstructionProc) then
    fProcessingInstructionProc(aTarget, aContent, aStop);
  {$ENDIF}
end;

procedure TSAXParser.DoOnStartDocument;
begin
  if Assigned(fOnStartDocument) then
    fOnStartDocument(Self);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fStartDocumentProc) then
    fStartDocumentProc;
  {$ENDIF}
end;

procedure TSAXParser.DoOnStartElement(const aName: OWideString;
  const aAttributes: TSAXAttributes; var aStop: Boolean);
begin
  if Assigned(fOnStartElement) then
    fOnStartElement(Self, aName, aAttributes, aStop);

  {$IFDEF O_DELPHI_2009_UP}
  if Assigned(fStartElementProc) then
    fStartElementProc(aName, aAttributes, aStop);
  {$ENDIF}
end;

function TSAXParser.GetBreakReading: TXmlBreakReading;
begin
  Result := fReader.BreakReading;
end;

function TSAXParser.GetEntityList: TOXmlReaderEntityList;
begin
  Result := fReader.EntityList;
end;

function TSAXParser.GetLineBreak: TXmlLineBreak;
begin
  Result := fReader.LineBreak;
end;

function TSAXParser.GetNodePath(const aIndex: Integer): OWideString;
begin
  Result := fReader.NodePath[aIndex];
end;

function TSAXParser.GetNodePathCount: Integer;
begin
  Result := fReader.NodePathCount;
end;

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

function TSAXParser.Parse: Boolean;
var
  xReaderNode: TOXmlReaderNode;
  xAttributes: TSAXAttributes;
  xStop: Boolean;
begin
  xStop := False;
  xReaderNode.NodeType := etDocumentStart;
  xReaderNode.NodeName := '';
  xReaderNode.NodeValue := '';

  xAttributes := TSAXAttributes.Create;
  try
    while (not xStop) and fReader.ReadNextNode(xReaderNode) do begin
      xStop := False;
      case xReaderNode.NodeType of
        etOpenElement: begin
          xAttributes.Clear;
          if not fDataRead then begin
            DoOnStartDocument;
            fDataRead := True;
          end;
        end;
        etAttribute: xAttributes.Add(xReaderNode.NodeName, xReaderNode.NodeValue);
        etFinishOpenElementClose: begin
          DoOnStartElement(xReaderNode.NodeName, xAttributes, xStop);
          DoOnEndElement(xReaderNode.NodeName, xStop);
        end;
        etFinishOpenElement: DoOnStartElement(xReaderNode.NodeName, xAttributes, xStop);
        etCloseElement: DoOnEndElement(xReaderNode.NodeName, xStop);
        etText, etCData:
          if fDataRead then//omit whitespace at the beginning of document
            DoOnCharacters(xReaderNode.NodeValue, xStop);
        etComment: DoOnComment(xReaderNode.NodeValue, xStop);
        etProcessingInstruction: DoOnProcessingInstruction(xReaderNode.NodeName, xReaderNode.NodeValue, xStop);
      end;
    end;

    if fDataRead and not xStop then
      DoOnEndDocument;
  finally
    xAttributes.Free;
  end;

  Result := xStop;
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

procedure TSAXParser.SetBreakReading(const aBreakReading: TXmlBreakReading);
begin
  fReader.BreakReading := aBreakReading;
end;

procedure TSAXParser.SetLineBreak(const aLineBreak: TXmlLineBreak);
begin
  fReader.LineBreak := aLineBreak;
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

end.
