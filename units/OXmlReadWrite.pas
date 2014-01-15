unit OXmlReadWrite;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlReadWrite.pas

  Basic XML reader/writer. OXmlIntfDOM.pas, OXmlPDOM.pas and OXmlSAX.pas use
  this unit to read and write XML.

  Use when performance is crucial for you.

  TXMLWriter
    - fast sequential XML writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      (if not stated differently) and the writer takes care of valid XML escaping
    - all line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to process them, set LineBreak to lbDoNotProcess
    - supports automatic indentation of XML

  TXMLReader
    - fast sequential XML reader/parser
    - the nodes are returned as they are found in the document
    - absolutely no whitespace handling - the document is parsed exactly 1:1
      -> white space is preserved also in the very beginning of the document
      -> you have to care for white space handling in end-level
    - only line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to process them, set LineBreak to lbDoNotProcess

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
  SysUtils, Classes, OWideSupp, OXmlUtils, OTextReadWrite, OEncoding
  {$IFDEF O_GENERICS}
    , Generics.Collections
  {$ENDIF}
  ;

type
  TXMLWriter = class;

  TXMLWriterElementMode = (
    stOpenOnly,    //"<node"
    stFinish,       //"<node>"
    stFinishClose); //"<node/>"

  PXMLWriterElement = ^TXMLWriterElement;
  TXMLWriterElement = {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fOwner: TXMLWriter;
    fElementName: OWideString;
    fOpenElementFinished: Boolean;
    fChildrenWritten: Boolean;

    procedure FinishOpenElement;
  public
    // <aElementName ... >, <aElementName ... /> etc.
    procedure OpenElement(const aElementName: OWideString; const aMode: TXMLWriterElementMode = stOpenOnly);
    function OpenElementR(const aElementName: OWideString; const aMode: TXMLWriterElementMode = stOpenOnly): TXMLWriterElement;

    // </fElementName> or "/>" if no children were written
    procedure CloseElement;

    // write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);

    // <![CDATA[aText]]>
    procedure CData(const aText: OWideString);
    // <!--aText-->
    procedure Comment(const aText: OWideString);
    // <?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);

    // write escaped text, do not escape quotes
    procedure Text(const aText: OWideString);
  public
    property ElementName: OWideString read fElementName;
  end;

  TXMLWriterSettings = class(TPersistent)
  private
    fIndentString: OWideString;
    fIndentType: TXmlIndentType;
    fLineBreak: TXmlLineBreak;
    fStrictXML: Boolean;
    fWriteBOM: Boolean;
    fOnSetWriteBOM: TNotifyEvent;
  protected
    procedure SetWriteBOM(const aWriteBOM: Boolean);

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  public
    //write BOM (if applicable)
    property WriteBOM: Boolean read fWriteBOM write SetWriteBOM;

    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to lbDoNotProcess if you don't want any processing
    //  default is your OS line break (XmlDefaultLineBreak)
    property LineBreak: TXmlLineBreak read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: element names & values checking
    //   = false: no element names & values checking
    property StrictXML: Boolean read fStrictXML write fStrictXML;

    //indent type - default is none (no indent)
    property IndentType: TXmlIndentType read fIndentType write fIndentType;
    //indent string - default are two space characters (#32#32)
    property IndentString: OWideString read fIndentString write fIndentString;
  end;

  TXMLWriter = class(TObject)
  private
    fWriter: TOTextWriter;
    fWriterSettings: TXMLWriterSettings;
    fDefaultIndentLevel: Integer;
    fWritten: Boolean;

    fIndentLevel: Integer;

    function GetEncoding: TEncoding;
    function GetOwnsEncoding: Boolean;
    procedure SetEncoding(const aEncoding: TEncoding);
    procedure SetOwnsEncoding(const aOwnsEncoding: Boolean);
    procedure SetDefaultIndentLevel(const aDefaultIndentLevel: Integer);
    procedure OnSetWriteBOM(Sender: TObject);
  protected
    //manual indentation support - you can use Indent+IncIndentLevel+DecIndentLevel
    //  manually if you want to. Set IndentType to itNone in this case.
    procedure Indent;
    procedure IncIndentLevel;
    procedure DecIndentLevel;
  protected
    procedure DoCreate;
    procedure DoInit; virtual;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLWriter or call ReleaseDocument!

    procedure InitFile(const aFileName: String);
    procedure InitStream(const aStream: TStream);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    // default <?xml ?> declaration
    procedure XMLDeclaration(const aEncodingAttribute: Boolean = True;
      const aVersion: OWideString = '1.0';
      const aStandAlone: OWideString = '');
    // <?xml
    procedure OpenXMLDeclaration;
    // ?>
    procedure FinishOpenXMLDeclaration;

    // <aElementName ... >, <aElementName ... /> etc.
    procedure OpenElement(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly);
    function OpenElementR(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly): TXMLWriterElement;
    // >
    procedure FinishOpenElement(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    // />
    procedure FinishOpenElementClose(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    // </aElementName>, decide if you want to indent
    procedure CloseElement(const aElementName: OWideString; const aIndent: Boolean = True);

    // write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);
    // <![CDATA[aText]]>
    procedure CData(const aText: OWideString);
    // <!--aText-->
    procedure Comment(const aText: OWideString);
    // <?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);
    // <!DOCTYPE aDocTypeRawText> - aDocTypeRawText must be escaped, it won't be processed
    procedure DocType(const aDocTypeRawText: OWideString);

    // write escaped text, escape also a quote if aQuoteChar specified
    procedure Text(const aText: OWideString; const aQuoteChar: OWideChar); overload;
    // write escaped text, decide if you want to indent
    procedure Text(const aText: OWideString; const aIndent: Boolean = True); overload;
    // write raw text, do not process it
    procedure RawText(const aText: OWideString);
  public
    //encoding of the text writer
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;

    //indentation level - you can change it only if nothing has been written yet
    //  and after the Init* call
    property DefaultIndentLevel: Integer read fDefaultIndentLevel write SetDefaultIndentLevel;

    property WriterSettings: TXMLWriterSettings read fWriterSettings;
  end;

  TXMLReaderTokenType = (
    rtDocumentStart,//start of reading
    rtOpenXMLDeclaration,//xml declaration open element: <?xml
    rtXMLDeclarationAttribute,//attribute in an xml declaration: name="value"
    rtFinishXMLDeclarationClose,//xml declaration element finished and closed: ?>
    rtOpenElement,//open element: <name
    rtAttribute,//attribute: name="value"
    rtFinishOpenElement,//open element finished but not closed: <node ... ">"
    rtFinishOpenElementClose,//open element finished and closed: <node ... "/>"
    rtCloseElement,//close element: "</node>"
    rtText,//text: value
    rtCData,//cdata: <![CDATA[value]]>
    rtComment,//comment: <!--value-->
    rtProcessingInstruction,//custom processing instruction: <?target content?>
    rtDocType//doctype: <!DOCTYPE value> -> value is not unescaped by reader!!!
    );

  TXMLReaderToken = class
  private
    fTokenType: TXMLReaderTokenType;
    fTokenName: OWideString;
    fTokenValue: OWideString;
  public
    property TokenType: TXMLReaderTokenType read fTokenType;
    property TokenName: OWideString read fTokenName;
    property TokenValue: OWideString read fTokenValue;
  end;

  TXMLReader = class;
  TXMLReaderSettings = class;

  TXMLNodePathHandling = (npFull, npLastPath, npNo);

  TXMLReaderSettings = class(TPersistent)
  private
    fBreakReading: TXmlBreakReading;
    fLineBreak: TXmlLineBreak;
    fStrictXML: Boolean;
    fEntityList: TXMLReaderEntityList;
    fRecognizeXMLDeclaration: Boolean;
    fNodePathHandling: TXMLNodePathHandling;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //process known entities. add user-defined entities here
    property EntityList: TXMLReaderEntityList read fEntityList;
    //decide if you want to read the document after the root element has been closed
    property BreakReading: TXMLBreakReading read fBreakReading write fBreakReading;
    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to lbDoNotProcess if you don't want any processing
    //  default is your OS line break (XmlDefaultLineBreak)
    property LineBreak: TXMLLineBreak read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: raise Exceptions when document is not valid
    //   = false: try to fix and go over document errors.
    property StrictXML: Boolean read fStrictXML write fStrictXML;
    //RecognizeXMLDeclaration
    //  if set to true the processing instruction "<?xml ... ?>" will be detected as XMLDeclaration
    //   and following element types will be fired:
    //   rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtFinishXMLDeclarationClose
    //  if set to false, it will be handled as a normal processing instruction
    //   rtProcessingInstruction
    property RecognizeXMLDeclaration: Boolean read fRecognizeXMLDeclaration write fRecognizeXMLDeclaration;
    //should nodepath handling be supported?
    //  npFull - the parser stores complete XML path (slow)
    //  npLastPath - the parser stores only last open element name
    //  npNo - the parser does not store anything (fastest, default)
    // -> for OXmlPDOM.pas, npNo is used regardless of your settings (node path is supported by OXmlPDOM directly).
    // -> for OXmlSAX.pas, use ONLY npLastPath or npFull - with npNo element names won't be read!
    // -> for OXmlSeq.pas, use ONLY npFull - otherwise parsing will fail!
    property NodePathHandling: TXMLNodePathHandling read fNodePathHandling write fNodePathHandling;
  end;

  TXMLReader = class(TObject)
  private
    fReaderSettings: TXMLReaderSettings;

    fReader: TOTextReader;
    fAllowSetEncodingFromFile: Boolean;//internal, for external use ForceEncoding

    fPreviousTokenType: TXMLReaderTokenType;
    fDocumentElementFound: Boolean;
    fForceEncoding: Boolean;

    fElementsToClose: Integer;
    fReaderToken: TXMLReaderToken;

    function GetEncoding: TEncoding;
    procedure SetEncoding(const aEncoding: TEncoding);
    function GetOwnsEncoding: Boolean;
    procedure SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
    function GetApproxStreamPosition: ONativeInt;
    function GetStreamSize: ONativeInt;
  private
    fNodePath: TOBufferWideStrings;
    fNodePathLevel: Integer;
    fLastOpenElementName: OWideString;

    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
  private
    procedure ProcessEntity;
    procedure ProcessNewLineChar(const aLastChar: OWideChar);
    procedure ChangeEncoding(const aEncodingAlias: OWideString);
    procedure OpenElement;
    procedure Attribute;
    procedure FinishOpenElement;
    procedure FinishOpenElementClose;
    procedure CloseElement;
    procedure Text(const aClearCustomBuffer: Boolean = True);

    procedure ExclamationNode(const aTokenType: TXMLReaderTokenType;
      const aBeginTag, aEndTag: OWideString; const aWhiteSpaceAfterBeginTag: Boolean);
    procedure CData;
    procedure Comment;
    procedure DocType;
    procedure ProcessingInstruction;
  protected
    procedure DoCreate;
    procedure DoDestroy;
    procedure DoInit(const aForceEncoding: TEncoding); virtual;
  protected
    procedure AddToNodePath(const aNodeName: OWideString);
    procedure RemoveLastFromNodePath(var ioNodeName: OWideString);
    function NodePathIsEmpty: Boolean;
  public
    //create and init
    constructor Create; overload;
    constructor Create(const aStream: TStream; const aForceEncoding: TEncoding = nil); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a XML document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached or you call ReleaseDocument!

    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitFile(const aFileName: String; const aForceEncoding: TEncoding = nil);
    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitStream(const aStream: TStream; const aForceEncoding: TEncoding = nil);
    //init XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    procedure InitXML(const aXML: OWideString);
    {$IFDEF O_RAWBYTESTRING}
    procedure InitXML_UTF8(const aXML: ORawByteString);
    {$ENDIF}
    {$IFDEF O_GENERICBYTES}
    //init document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil);
    {$ENDIF}

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //use ReadNextToken for reading next XML token
    function ReadNextToken: Boolean;

    property ReaderToken: TXMLReaderToken read fReaderToken;
  public
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

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;
  public
    //encoding of the text file, when set, the file will be read again from the start
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
    //if set to true, the encoding will not be changed automatically when
    //  <?xml encoding="..."?> is found
    property ForceEncoding: Boolean read fForceEncoding write fForceEncoding;

    //Reader Settings
    property ReaderSettings: TXMLReaderSettings read fReaderSettings;
  public
    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: ONativeInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: ONativeInt read GetStreamSize;
  end;

  EXmlWriterException = class(Exception);
  EXmlWriterInvalidString = class(EXmlWriterException);
  EXmlReaderException = class(Exception)
  public
    constructor Create(aReader: TXMLReader; const aMsg: string);
    constructor CreateFmt(aReader: TXMLReader;
      const aMsg: string; const aArgs: array of const);
  end;
  EXmlReaderInvalidString = class(EXmlReaderException);
  EXmlReaderInvalidCharacter = class(EXmlReaderException);
  EXmlReaderInvalidStructure = class(EXmlReaderException);

implementation

uses OXmlLng;

{ TXMLWriter }

procedure TXMLWriter.CData(const aText: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidCData(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidCData, [aText]);

  Indent;

  RawText('<![CDATA[');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawText(']]>');
end;

procedure TXMLWriter.Comment(const aText: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidComment(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidComment, [aText]);

  Indent;

  RawText('<!--');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawText('-->');
end;

constructor TXMLWriter.Create(const aStream: TStream);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream);
end;

constructor TXMLWriter.Create;
begin
  inherited Create;

  DoCreate;
end;

procedure TXMLWriter.DecIndentLevel;
begin
  Dec(fIndentLevel);
end;

destructor TXMLWriter.Destroy;
begin
  fWriter.Free;
  fWriterSettings.Free;

  inherited;
end;

procedure TXMLWriter.DoInit;
begin
  fWritten := False;
  fIndentLevel := fDefaultIndentLevel;
end;

procedure TXMLWriter.DoCreate;
begin
  fWriter := TOTextWriter.Create;

  Encoding := TEncoding.UTF8;
  fWriterSettings := TXMLWriterSettings.Create;
  fWriterSettings.WriteBOM := True;
  fWriterSettings.fOnSetWriteBOM := OnSetWriteBOM;
end;

procedure TXMLWriter.DocType(const aDocTypeRawText: OWideString);
begin
  Indent;

  RawText('<!DOCTYPE ');
  RawText(aDocTypeRawText);//MUST BE RAW ESCAPED TEXT - the programmer has to be sure that aDocTypeRawText is valid
  RawText('>');
end;

procedure TXMLWriter.CloseElement(const aElementName: OWideString; const aIndent: Boolean);
begin
  DecIndentLevel;
  if aIndent then
    Indent;
  RawText('</');
  RawText(aElementName);//can be rawtext, because validated (in OpenElement)!
  RawText('>');
end;

function TXMLWriter.GetEncoding: TEncoding;
begin
  Result := fWriter.Encoding;
end;

function TXMLWriter.GetOwnsEncoding: Boolean;
begin
  Result := fWriter.OwnsEncoding;
end;

procedure TXMLWriter.IncIndentLevel;
begin
  Inc(fIndentLevel);
end;

procedure TXMLWriter.Indent;
var I: Integer;
begin
  if (fWriterSettings.fIndentType in [itFlat, itIndent]) and
    fWritten//do not indent at the very beginning of the document
  then
    RawText(XmlLineBreak[fWriterSettings.fLineBreak]);

  if fWriterSettings.fIndentType = itIndent then
  for I := 1 to fIndentLevel do
    RawText(fWriterSettings.fIndentString);
end;

procedure TXMLWriter.InitFile(const aFileName: String);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLWriter.InitStream(const aStream: TStream);
begin
  fWriter.InitStream(aStream);

  DoInit;
end;

procedure TXMLWriter.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aTarget) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPITarget, [aTarget]);

  if fWriterSettings.fStrictXML and not OXmlValidPIContent(aContent) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPIContent, [aContent]);

  Indent;

  RawText('<?');
  RawText(aTarget);
  if aTarget <> '' then
    RawText(' ');
  RawText(aContent);//MUST BE RAWTEXT - must contain unescaped characters
  RawText('?>');
end;

procedure TXMLWriter.RawText(const aText: OWideString);
begin
  fWritten := True;

  fWriter.WriteString(aText);
end;

procedure TXMLWriter.ReleaseDocument;
begin
  fWriter.ReleaseDocument;
end;

procedure TXMLWriter.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aAttrName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidAttributeName, [aAttrName]);

  RawText(' ');
  RawText(aAttrName);//can be rawtext, because validated!
  RawText('="');
  Text(aAttrValue, OWideChar('"'));
  RawText('"');
end;

procedure TXMLWriter.SetEncoding(const aEncoding: TEncoding);
begin
  fWriter.Encoding := aEncoding;
end;

procedure TXMLWriter.SetDefaultIndentLevel(const aDefaultIndentLevel: Integer);
begin
  if fWritten then
    raise EXmlWriterException.Create(OXmlLng_CannotSetIndentLevelAfterWrite);
  fDefaultIndentLevel := aDefaultIndentLevel;
  fIndentLevel := fDefaultIndentLevel;
end;

procedure TXMLWriter.SetOwnsEncoding(const aOwnsEncoding: Boolean);
begin
  fWriter.OwnsEncoding := aOwnsEncoding;
end;

procedure TXMLWriter.OnSetWriteBOM(Sender: TObject);
begin
  if Assigned(fWriter) then
    fWriter.WriteBOM := fWriterSettings.fWriteBOM;
end;

procedure TXMLWriter.Text(const aText: OWideString; const aIndent: Boolean);
begin
  if aIndent then
    Indent;

  Text(aText, OWideChar(#0));
end;

procedure TXMLWriter.OpenXMLDeclaration;
begin
  Indent;
  RawText('<?xml');
end;

procedure TXMLWriter.FinishOpenXMLDeclaration;
begin
  RawText('?>');
end;

procedure TXMLWriter.OpenElement(const aElementName: OWideString; const aMode: TXMLWriterElementMode);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aElementName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidElementName, [aElementName]);

  Indent;

  RawText('<');
  RawText(aElementName);
  case aMode of
    stOpenOnly: IncIndentLevel;
    stFinish: begin
      RawText('>');
      IncIndentLevel;
    end;
    stFinishClose: RawText('/>');
  end;
end;

procedure TXMLWriter.FinishOpenElement(const aElementName: OWideString);
begin
  RawText('>');
end;

procedure TXMLWriter.FinishOpenElementClose(const aElementName: OWideString);
begin
  DecIndentLevel;
  RawText('/>');
end;

function TXMLWriter.OpenElementR(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode): TXMLWriterElement;
begin
  OpenElement(aElementName, aMode);

  if aMode = stFinishClose then begin
    Result.fOwner := nil;//do not use after close
  end else begin
    Result.fOwner := Self;
    Result.fElementName := aElementName;
    Result.fOpenElementFinished := (aMode = stFinish);
    Result.fChildrenWritten := False;
  end;
end;

procedure TXMLWriter.Text(const aText: OWideString; const aQuoteChar: OWideChar);
var
  xC: OWideChar;
  I, xLength: Integer;
begin
  xLength := Length(aText);
  if xLength = 0 then
    Exit;

  for I := 1 to xLength do begin
    xC := aText[I];
    case xC of
      '&': RawText('&amp;');
      '<': RawText('&lt;');
      '>': RawText('&gt;');
      '"':
        if aQuoteChar = '"' then
          RawText('&quot;')
        else
          RawText(xC);
      '''':
        if aQuoteChar = '''' then
          RawText('&apos;')
        else
          RawText(xC);
      #10:
        if (fWriterSettings.fLineBreak = lbDoNotProcess) then//no line break handling
          RawText(xC)
        else if ((I = 1) or (aText[I-1] <> #13)) then//previous character is not #13 (i.e. this is a simple #10 not #13#10) -> write fLineBreak
          RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
      #13:
        if fWriterSettings.fLineBreak = lbDoNotProcess then
          RawText(xC)
        else
          RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
    else
      RawText(xC);
    end;
  end;
end;

procedure TXMLWriter.XMLDeclaration(const aEncodingAttribute: Boolean;
  const aVersion, aStandAlone: OWideString);
begin
  OpenXMLDeclaration;

  if aVersion <> '' then
    Attribute('version', aVersion);
  if aEncodingAttribute then
    Attribute('encoding', fWriter.Encoding.EncodingAlias);
  if aStandAlone <> '' then
    Attribute('standalone', aStandAlone);

  FinishOpenXMLDeclaration;
end;

{ TXMLWriterElement }

procedure TXMLWriterElement.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fOpenElementFinished then
    raise EXmlDOMException.CreateFmt(OXmlLng_CannotWriteAttributesWhenFinished, [aAttrName, aAttrValue, fElementName])
  else
    fOwner.Attribute(aAttrName, aAttrValue);
end;

procedure TXMLWriterElement.CData(const aText: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.CData(aText);
end;

procedure TXMLWriterElement.Comment(const aText: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.Comment(aText);
end;

procedure TXMLWriterElement.CloseElement;
begin
  if fChildrenWritten then
    fOwner.CloseElement(fElementName, True)
  else
    fOwner.FinishOpenElementClose;

  //DO NOT USE THIS RECORD ANY MORE
  fOwner := nil;
  fElementName := '';
end;

procedure TXMLWriterElement.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.ProcessingInstruction(aTarget, aContent);
end;

procedure TXMLWriterElement.OpenElement(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.OpenElement(aElementName, aMode);
end;

procedure TXMLWriterElement.FinishOpenElement;
begin
  if not fOpenElementFinished then begin
    fOwner.FinishOpenElement;
    fOpenElementFinished := True;
  end;
end;

function TXMLWriterElement.OpenElementR(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode): TXMLWriterElement;
begin
  FinishOpenElement;
  fChildrenWritten := True;
  Result := fOwner.OpenElementR(aElementName, aMode);
end;

procedure TXMLWriterElement.Text(const aText: OWideString);
begin
  FinishOpenElement;
  fChildrenWritten := True;
  fOwner.Text(aText);
end;

procedure TXMLReader.FinishOpenElement;
begin
  fReaderToken.fTokenName := fLastOpenElementName;
  fReaderToken.fTokenValue := '';
  if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
    fReaderToken.fTokenType := rtFinishXMLDeclarationClose
  else
    fReaderToken.fTokenType := rtFinishOpenElement;
end;

procedure TXMLReader.FinishOpenElementClose;
var
  xC: OWideChar;
begin
  //opened after a '?' for PI or '/' for an element.

  fReader.ReadNextChar({%H-}xC);//must be '>'
  if xC <> '>' then begin
    if fReaderSettings.fStrictXML then begin
      if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
        raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, ['?'])
      else
        raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, ['/']);
    end else begin
      //let's be generous and go over this invalid character
      fReader.UndoRead;
      ReadNextToken;
      Exit;
    end;
  end;

  fReaderToken.fTokenValue := '';
  if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then begin
    fReaderToken.fTokenName := 'xml';
    fReaderToken.fTokenType := rtFinishXMLDeclarationClose;
  end else begin
    fReaderToken.fTokenName := fLastOpenElementName;
    fReaderToken.fTokenType := rtFinishOpenElementClose;

    RemoveLastFromNodePath(fReaderToken.fTokenName);
  end;
end;

procedure TXMLReader.Text(const aClearCustomBuffer: Boolean);
var
  xC: OWideChar;
begin
  if aClearCustomBuffer then
    fReader.ClearCustomBuffer;
  fReader.ReadNextChar({%H-}xC);

  while True do begin
    case xC of
      #0, '<': break;
      '&': ProcessEntity;
      #10, #13: ProcessNewLineChar(xC);
    else
      fReader.WritePreviousCharToBuffer;
    end;
    fReader.ReadNextChar(xC);
  end;

  if xC <> #0 then
    fReader.UndoRead;
  fReaderToken.fTokenType := rtText;
  fReaderToken.fTokenName := '';
  fReaderToken.fTokenValue := fReader.GetCustomBuffer;
end;

procedure TXMLReader.AddToNodePath(const aNodeName: OWideString);
begin
  Inc(fNodePathLevel);
  if fReaderSettings.fNodePathHandling in [npFull, npLastPath] then begin
    fLastOpenElementName := aNodeName;
    if fReaderSettings.fNodePathHandling = npFull then
      fNodePath.Add(fLastOpenElementName);
  end;
end;

procedure TXMLReader.Attribute;
var
  xC: OWideChar;
  xQuotationMark: OWideChar;
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar({%H-}xC);

  if fReaderSettings.fStrictXML and not OXmlIsNameStartChar(xC) then
    raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidAttributeStartChar, [xC]);

  if not fReaderSettings.fStrictXML then begin
    //not StrictXML

    repeat//read attribute name
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    until OXmlIsBreakChar(xC);
  end else begin
    //StrictXML
    while OXmlIsNameChar(xC) do begin//read attribute name
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    end;
  end;
  fReaderToken.fTokenName := fReader.GetCustomBuffer;

  if not fReaderSettings.fStrictXML then
    while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "attr ="
      fReader.ReadNextChar(xC);

  if xC <> '=' then begin
    //let's be generous and allow attributes without values - even if they are not allowed by xml spec
    if fReaderSettings.fStrictXML then begin
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_EqualSignMustFollowAttribute, [fReaderToken.TokenName]);
    end else begin
      fReaderToken.fTokenValue := '';
      fReader.UndoRead;
    end;
  end else begin
    fReader.ReadNextChar(xC);
    if not fReaderSettings.fStrictXML then
      while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "= value"
        fReader.ReadNextChar(xC);

    xQuotationMark := xC;
    if (xQuotationMark = '''') or (xQuotationMark = '"') then begin
      //read attribute value in quotation marks
      fReader.ClearCustomBuffer;
      fReader.ReadNextChar(xC);
      while xC <> xQuotationMark do begin
        case xC of
          #0: Break;
          '&': ProcessEntity;
          #10, #13: ProcessNewLineChar(xC);
        else
          fReader.WritePreviousCharToBuffer;
        end;
        fReader.ReadNextChar(xC);
      end;
    end else begin
      if fReaderSettings.fStrictXML then begin
        raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_AttributeValueMustBeEnclosed, [fReaderToken.TokenName]);
      end else begin
        //let's be generous and allow attribute values that are not enclosed in quotes
        fReader.ClearCustomBuffer;
        while not OXmlIsBreakChar(xC) do begin
          case xC of
            #0: Break;
            '&': ProcessEntity;
            #10, #13: ProcessNewLineChar(xC);
          else
            fReader.WritePreviousCharToBuffer;
          end;
          fReader.ReadNextChar(xC);
        end;
      end;
    end;

    fReaderToken.fTokenValue := fReader.GetCustomBuffer;
  end;

  if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then begin
    fReaderToken.fTokenType := rtXMLDeclarationAttribute;

    if not fForceEncoding and fAllowSetEncodingFromFile and
      (fReaderToken.fTokenName = 'encoding')
    then
      ChangeEncoding(fReaderToken.TokenValue);
  end else begin
    fReaderToken.fTokenType := rtAttribute;
  end;
end;

procedure TXMLReader.CData;
begin
  ExclamationNode(rtCData, '<![CDATA[', ']]>', False);
end;

procedure TXMLReader.ChangeEncoding(const aEncodingAlias: OWideString);
var
  xLastName: OWideString;
  xEncoding: TEncoding;
  xInXMLDeclaration: Boolean;
begin
  if
    GetCreateCodePage(aEncodingAlias, {%H-}xEncoding) and
    (fReader.Encoding <> xEncoding)
  then begin
    //reload document with new encoding
    fReader.Encoding := xEncoding;
    if fAllowSetEncodingFromFile then begin
      fAllowSetEncodingFromFile := False;
      fReader.UnblockFlushTempBuffer;//was blocked in TXMLReader.Create
    end;
    //go back to current position
    xInXMLDeclaration := False;
    xLastName := fReaderToken.TokenName;
    fPreviousTokenType := rtDocumentStart;
    //parse from beginning back to the encoding attribute
    while ReadNextToken do begin
      case fReaderToken.TokenType of
        rtOpenXMLDeclaration: xInXMLDeclaration := True;
        rtOpenElement: xInXMLDeclaration := False;
        rtXMLDeclarationAttribute:
        if xInXMLDeclaration and (fReaderToken.TokenName = xLastName) then begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TXMLReader.Comment;
begin
  ExclamationNode(rtComment, '<!--', '-->', False);
end;

constructor TXMLReader.Create;
begin
  inherited Create;

  DoCreate;
end;

constructor TXMLReader.Create(const aStream: TStream;
  const aForceEncoding: TEncoding = nil);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream, aForceEncoding);
end;

procedure TXMLReader.DoInit(const aForceEncoding: TEncoding);
begin
  fAllowSetEncodingFromFile := True;
  fDocumentElementFound := False;
  fElementsToClose := 0;
  fPreviousTokenType := rtDocumentStart;
  fNodePath.Clear;

  fReader.BlockFlushTempBuffer;//will be unblocked when fAllowSetEncodingFromFile is set to false
  if Assigned(aForceEncoding) then
    Self.Encoding := aForceEncoding
  else
    Self.fForceEncoding := False;
end;

destructor TXMLReader.Destroy;
begin
  DoDestroy;

  inherited;
end;

procedure TXMLReader.DoCreate;
begin
  fReader := TOTextReader.Create;
  fReaderToken := TXMLReaderToken.Create;
  fReaderSettings := TXMLReaderSettings.Create;

  fNodePath := TOBufferWideStrings.Create;
end;

procedure TXMLReader.DocType;
begin
  ExclamationNode(rtDocType, '<!DOCTYPE', '>', True);
end;

procedure TXMLReader.DoDestroy;
begin
  fReader.Free;
  fReaderToken.Free;
  fReaderSettings.Free;
  fNodePath.Free;

  inherited;
end;

procedure TXMLReader.SetEncoding(const aEncoding: TEncoding);
begin
  fReader.Encoding := aEncoding;
  fForceEncoding := True;
end;

procedure TXMLReader.SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
begin
  fReader.OwnsEncoding := aSetOwnsEncoding;
end;

procedure TXMLReader.ExclamationNode(
  const aTokenType: TXMLReaderTokenType; const aBeginTag, aEndTag: OWideString;
  const aWhiteSpaceAfterBeginTag: Boolean);
var
  I: Integer;
  xC: OWideChar;
  xPreviousC: OWideString;
  xResult: Boolean;
begin
  fReader.ClearCustomBuffer;
  fReader.WriteStringToBuffer('<!');
  xResult := True;
  for I := 3 to Length(aBeginTag) do begin
    fReader.ReadNextChar({%H-}xC);
    if aBeginTag[I] <> UpperCase(xC) then begin
      xResult := False;
      fReader.UndoRead;
      Break;
    end;
    fReader.WritePreviousCharToBuffer;
  end;

  if aWhiteSpaceAfterBeginTag and xResult then begin
    //must be followed by a whitespace character
    fReader.ReadNextChar(xC);
    fReader.WritePreviousCharToBuffer;
    xResult := OXmlIsWhiteSpaceChar(xC);
  end;

  if not xResult then begin
    //header not found
    if fReaderSettings.fStrictXML then begin
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidCharacterInText, ['<']);
    end else begin
      //output as text
      if xC <> '<' then begin
        Text(False);
      end else begin
        fReaderToken.fTokenType := rtText;
        fReaderToken.fTokenValue := fReader.GetCustomBuffer;
        fReaderToken.fTokenName := '';
      end;
      Exit;
    end;
  end else begin
    fReader.ClearCustomBuffer;
    SetLength(xPreviousC, Length(aEndTag));
    FillChar(xPreviousC[1], Length(xPreviousC)*SizeOf(OWideChar), 0);
    repeat
      if Length(xPreviousC) > 1 then
        Move(xPreviousC[2], xPreviousC[1], (Length(xPreviousC)-1)*SizeOf(OWideChar));
      fReader.ReadNextChar(xC);
      xPreviousC[Length(xPreviousC)] := xC;
      fReader.WritePreviousCharToBuffer;
    until ((xPreviousC = aEndTag) or (xC = #0));

    for I := 1 to Length(aEndTag) do
      fReader.RemovePreviousCharFromBuffer;

    fReaderToken.fTokenType := aTokenType;
    fReaderToken.fTokenName := '';
    fReaderToken.fTokenValue := fReader.GetCustomBuffer;
  end;
end;

procedure TXMLReader.OpenElement;
var
  xC: OWideChar;
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar({%H-}xC);

  case xC of
    '!': begin
      //comment or cdata
      fReader.ReadNextChar(xC);
      fReader.UndoRead;
      case xC of
        '[': CData;
        '-': Comment;
        'D', 'd': DocType;
      else
        if fReaderSettings.fStrictXML then begin
          raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInText, ['<']);
        end else begin
          fReader.WriteStringToBuffer('<!');
          if xC <> '<' then begin
            Text(False);
          end else begin
            fReaderToken.fTokenType := rtText;
            fReaderToken.fTokenValue := fReader.GetCustomBuffer;
            fReaderToken.fTokenName := '';
          end;
          Exit;
        end;
      end;
      Exit;
    end;
    '/': begin
      //close element
      CloseElement;
      Exit;
    end;
    '?': begin
      //processing instruction
      ProcessingInstruction;
      Exit;
    end;
  end;

  if fAllowSetEncodingFromFile then begin
    fAllowSetEncodingFromFile := False;// -> first Node found, encoding change is not allowed any more
    fReader.UnblockFlushTempBuffer;//was blocked in TXMLReader.Create
  end;

  if fReaderSettings.fStrictXML then begin
    if not OXmlIsNameStartChar(xC) then
      raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInText, ['<']);

    while OXmlIsNameChar(xC) do begin
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    end;

    if (xC = '/') or (xC = '>') then
      fReader.UndoRead
    else if not OXmlIsWhiteSpaceChar(xC) then
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidElementName, [fReader.GetCustomBuffer+xC]);
  end else begin
    if not OXmlIsNameChar(xC) then begin
      fReader.WriteCharToBuffer('<');
      fReader.UndoRead;
      if xC <> '<' then begin
        Text(False);
      end else begin
        fReaderToken.fTokenType := rtText;
        fReaderToken.fTokenValue := fReader.GetCustomBuffer;
        fReaderToken.fTokenName := '';
      end;
      Exit;
    end else begin
      while not OXmlIsBreakChar(xC) do begin
        fReader.WritePreviousCharToBuffer;
        fReader.ReadNextChar(xC);
      end;

      if (xC = '/') or (xC = '>') then
        fReader.UndoRead
    end;
  end;

  fDocumentElementFound := True;
  fReaderToken.fTokenName := fReader.GetCustomBuffer;
  fReaderToken.fTokenValue := '';
  fReaderToken.fTokenType := rtOpenElement;

  AddToNodePath(fReaderToken.fTokenName);
end;

function TXMLReader.ReadNextToken: Boolean;
var
  xC: OWideChar;
begin
  Result := True;

  if fElementsToClose > 0 then begin
    //close elements
    fReaderToken.fTokenType := rtCloseElement;
    if fNodePath.Count > 0 then
      fNodePath.Get(fNodePath.Count-1, fReaderToken.fTokenName)
    else
      fReaderToken.fTokenName := '';
    fReaderToken.fTokenValue := '';
    RemoveLastFromNodePath(fReaderToken.fTokenName);
    Dec(fElementsToClose);
    fPreviousTokenType := fReaderToken.TokenType;
    Exit;
  end;

  if (fReaderSettings.fBreakReading = brAfterDocumentElement) and
     (fDocumentElementFound) and
     NodePathIsEmpty
  then begin
    //end of root element is reached, release document and close
    Result := False;
    ReleaseDocument;
    Exit;
  end;

  fReader.ReadNextChar({%H-}xC);
  case xC of
    #0: begin
      //end of document
      Result := False;
      ReleaseDocument;
      Exit;
    end;
    '<': begin
      if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute] then
      begin
        if fReaderSettings.fStrictXML then begin
          raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, [xC]);
        end else begin
          fReader.UndoRead;
          Attribute;
        end;
      end else begin
        OpenElement;
      end;
    end;
    '?': begin
      if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
      begin
        FinishOpenElementClose
      end
      else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
    '/': begin
      if fPreviousTokenType in [rtOpenElement, rtAttribute] then
      begin
        FinishOpenElementClose
      end
      else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
    '>': begin
      if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute] then
      begin
        FinishOpenElement;
      end else begin
        if fReaderSettings.fStrictXML then begin
          raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInText, [xC]);
        end else begin
          //text
          fReader.UndoRead;
          Text;
        end;
      end;
    end;
  else
    if fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute]
    then begin
      while OXmlIsWhiteSpaceChar(xC) do
        fReader.ReadNextChar(xC);

      if ((xC = '/') and (fPreviousTokenType in [rtOpenElement, rtAttribute])) or
         ((xC = '?') and (fPreviousTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute]))
      then begin
        FinishOpenElementClose;
      end else if ((xC = '>') and (fPreviousTokenType in [rtOpenElement, rtAttribute])) then begin
        FinishOpenElement;
      end else begin
        fReader.UndoRead;
        Attribute;
      end;
    end else begin
      //text
      fReader.UndoRead;
      Text;
    end;
  end;

  fPreviousTokenType := fReaderToken.TokenType;
end;

procedure TXMLReader.CloseElement;
var
  xC: OWideChar;
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar({%H-}xC);

  if fReaderSettings.fStrictXML then begin
    if not OXmlIsNameStartChar(xC) then
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidStringInText, ['</'+xC]);

    while OXmlIsNameChar(xC) do begin
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    end;
    while OXmlIsWhiteSpaceChar(xC) do begin
      fReader.ReadNextChar(xC);
    end;
    if xC <> '>' then
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidStringInText, ['</'+fReader.GetCustomBuffer]);
  end else begin
    if not OXmlIsNameChar(xC) then begin
      fReader.WriteStringToBuffer('</');
      fReader.UndoRead;
      if xC <> '<' then begin
        Text(False);
      end else begin
        fReaderToken.fTokenType := rtText;
        fReaderToken.fTokenValue := fReader.GetCustomBuffer;
        fReaderToken.fTokenName := '';
      end;
      Exit;
    end else begin
      while not OXmlIsBreakChar(xC) do begin
        fReader.WritePreviousCharToBuffer;
        fReader.ReadNextChar(xC);
      end;
      while not((xC = '>') or (xC = #0)) do begin
        fReader.ReadNextChar(xC);
      end;
    end;
  end;

  fReaderToken.fTokenName := fReader.GetCustomBuffer;
  fReaderToken.fTokenValue := '';
  fReaderToken.fTokenType := rtCloseElement;

  RemoveLastFromNodePath(fReaderToken.fTokenName);
end;

function TXMLReader.GetApproxStreamPosition: ONativeInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TXMLReader.GetEncoding: TEncoding;
begin
  Result := fReader.Encoding;
end;

function TXMLReader.GetOwnsEncoding: Boolean;
begin
  Result := fReader.OwnsEncoding;
end;

function TXMLReader.GetStreamSize: ONativeInt;
begin
  Result := fReader.StreamSize;
end;

{$IFDEF O_GENERICBYTES}
procedure TXMLReader.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fReader.InitBuffer(aBuffer);
  DoInit(aForceEncoding);
end;
{$ENDIF}

procedure TXMLReader.InitFile(const aFileName: String;
  const aForceEncoding: TEncoding);
begin
  fReader.InitFile(aFileName);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fReader.InitStream(aStream);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitXML(const aXML: OWideString);
begin
  fReader.InitString(aXML);
  DoInit(TEncoding.OWideStringEncoding);
end;

function TXMLReader.NodePathIsEmpty: Boolean;
begin
  Result := (fNodePathLevel = 0);
end;

{$IFDEF O_RAWBYTESTRING}
procedure TXMLReader.InitXML_UTF8(const aXML: ORawByteString);
begin
  fReader.InitString_UTF8(aXML);
  DoInit(TEncoding.UTF8);
end;
{$ENDIF}

procedure TXMLReader.ProcessEntity;
  procedure _EntityError(bCustomEntityString: OWideString = '');
  begin
    if fReaderSettings.fStrictXML then begin
      if bCustomEntityString = '' then begin
        fReader.WritePreviousCharToBuffer(1);
        bCustomEntityString := fReader.GetCustomBuffer(1);
      end;
      raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidEntity, ['&'+bCustomEntityString]);
    end else begin
      if bCustomEntityString = '' then
        bCustomEntityString := fReader.GetCustomBuffer(1);

      fReader.UndoRead;
      fReader.WriteStringToBuffer('&'+bCustomEntityString, 0);
    end;
  end;
var
  xC: OWideChar;
  xEntityString: OWideString;
  xOutputChar: Integer;
  xIsHex: Boolean;
  xOutputEntity: OWideString;
begin
  xOutputEntity := '';
  xOutputChar := 0;
  fReader.ClearCustomBuffer(1);

  fReader.ReadNextChar({%H-}xC);
  if xC = '#' then begin
    fReader.ReadNextChar(xC);
    xIsHex := (xC = 'x');
    if xIsHex then begin
      fReader.ReadNextChar(xC);
      while OXmlIsHexadecimalChar(xC) do begin
        fReader.WritePreviousCharToBuffer(1);
        fReader.ReadNextChar(xC);
      end;

      if xC <> ';' then begin
        _EntityError;
        Exit;
      end;

      xEntityString := fReader.GetCustomBuffer(1);
      if not TryStrToInt('$'+xEntityString, xOutputChar) then begin
        _EntityError(xEntityString);
        Exit;
      end;
    end else begin
      while OXmlIsDecimalChar(xC) do begin
        fReader.WritePreviousCharToBuffer(1);
        fReader.ReadNextChar(xC);
      end;

      if xC <> ';' then begin
        _EntityError;
        Exit;
      end;

      xEntityString := fReader.GetCustomBuffer(1);
      if not TryStrToInt(xEntityString, xOutputChar) then begin
        _EntityError(xEntityString);
        Exit;
      end;
    end;

  end else begin
    if not OXmlIsNameStartChar(xC) then begin
      _EntityError;
      Exit;
    end;

    while OXmlIsNameChar(xC) do begin
      fReader.WritePreviousCharToBuffer(1);
      fReader.ReadNextChar(xC);
    end;

    if xC <> ';' then begin
      _EntityError;
      Exit;
    end;

    xEntityString := fReader.GetCustomBuffer(1);
    if not fReaderSettings.fEntityList.TryGetValue(xEntityString, xOutputEntity) then begin
      _EntityError(xEntityString);
      Exit;
    end;
  end;

  if (xOutputChar > 0) and (xOutputEntity = '') then begin
    {$IFDEF FPC}
    xOutputEntity := UTF8Encode(WideString(WideChar(xOutputChar)));
    {$ELSE}
    xOutputEntity := OWideString(OWideChar(xOutputChar));
    {$ENDIF}
  end;

  if xOutputEntity <> '' then
    fReader.WriteStringToBuffer(xOutputEntity, 0);
end;

procedure TXMLReader.ProcessingInstruction;
var
  xC: OWideChar;
  xPreviousC: OWideChar;
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar({%H-}xC);

  if fReaderSettings.fStrictXML then begin
    if not OXmlIsNameStartChar(xC) then
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidStringInText, ['<?'+xC]);

    while OXmlIsNameChar(xC) do begin
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    end;

    if not OXmlIsWhiteSpaceChar(xC) then begin
      //must be followed by a whitespace character
      fReader.WritePreviousCharToBuffer;
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidStringInText, ['<?'+fReader.GetCustomBuffer]);
    end;
  end else begin
    while not OXmlIsBreakChar(xC) do begin
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    end;
  end;

  fReaderToken.fTokenName := fReader.GetCustomBuffer;
  if (fPreviousTokenType = rtDocumentStart) and
    fReaderSettings.fRecognizeXMLDeclaration and
    SameText(fReaderToken.TokenName, 'xml')
  then begin
    //xml declaration: <?xml attr="value"?>
    fReaderToken.fTokenType := rtOpenXMLDeclaration;
    fReaderToken.fTokenValue := '';
    Exit;
  end;

  //custom processing instruction
  fReaderToken.fTokenType := rtProcessingInstruction;
  fReader.ClearCustomBuffer;
  if not fReaderSettings.fStrictXML and (fReaderToken.TokenName = '') then
    fReader.UndoRead;

  xPreviousC := #0;
  fReader.ReadNextChar(xC);
  while
    not((xPreviousC = '?') and (xC = '>')) and
    not(xC = #0)
  do begin
    fReader.WritePreviousCharToBuffer;
    xPreviousC := xC;
    fReader.ReadNextChar(xC);
  end;
  fReader.RemovePreviousCharFromBuffer;

  fReaderToken.fTokenValue := fReader.GetCustomBuffer;
end;

procedure TXMLReader.ProcessNewLineChar(const aLastChar: OWideChar);
var
  xC: OWideChar;
begin
  if fReaderSettings.fLineBreak <> lbDoNotProcess then begin
    if aLastChar = #13 then begin
      fReader.ReadNextChar({%H-}xC);
      if xC <> #10 then
        fReader.UndoRead;
    end;

    fReader.WriteStringToBuffer(XmlLineBreak[fReaderSettings.fLineBreak]);
  end else begin
    fReader.WriteCharToBuffer(aLastChar);
  end;
end;

procedure TXMLReader.ReleaseDocument;
begin
  fReader.ReleaseDocument;
end;

procedure TXMLReader.RemoveLastFromNodePath(var ioNodeName: OWideString);
var
  I: Integer;
begin
  Dec(fNodePathLevel);
  if fReaderSettings.fNodePathHandling <> npFull then
    Exit;

  if (fNodePath.Count = 0) then begin
    //there is no open element
    if fReaderSettings.fStrictXML then
      raise EXmlReaderInvalidStructure.Create(Self, OXmlLng_TooManyElementsClosed);
  end else begin
    if
       (ioNodeName <> '') and
       (fNodePath.Get(fNodePath.Count-1) <> ioNodeName)
    then begin
      //element names do not match
      if fReaderSettings.fStrictXML then begin
        raise EXmlReaderInvalidStructure.CreateFmt(Self, OXmlLng_WrongElementClosed, [ioNodeName, fNodePath.Get(fNodePath.Count-1)]);
      end else begin
        //trying to close parent element
        for I := fNodePath.Count-2 downto 0 do
        if (fNodePath.Get(I) = ioNodeName) then begin
          //parent element with the same name found, we have to close more elements in the future!!!
          fElementsToClose := (fNodePath.Count - I - 1);
          Break;
        end;

        //delete the last one from fNodePath
        //  + rename the element if names differ
        if ioNodeName <> '' then
          ioNodeName := fNodePath.Get(fNodePath.Count-1);
        fNodePath.DeleteLast;
      end;
    end else begin
      //everything is fine -> delete last from fNodePath
      fNodePath.DeleteLast;
    end;
  end;
end;

{ EXmlReaderException }

constructor EXmlReaderException.Create(aReader: TXMLReader;
  const aMsg: string);
begin
  inherited Create(
    aMsg+
    sLineBreak+sLineBreak+
    Format(OXmlLng_ReadingAt, [aReader.fReader.ReadPreviousString(10)+aReader.fReader.ReadString(30)]));
end;

constructor EXmlReaderException.CreateFmt(aReader: TXMLReader;
  const aMsg: string; const aArgs: array of const);
begin
  inherited Create(
    Format(aMsg, aArgs)+
    sLineBreak+sLineBreak+
    Format(OXmlLng_ReadingAt, [aReader.fReader.ReadPreviousString(10)+aReader.fReader.ReadString(30)]));
end;

{ TXMLWriterSettings }

procedure TXMLWriterSettings.AssignTo(Dest: TPersistent);
var
  xDest: TXMLWriterSettings;
begin
  if Dest is TXMLWriterSettings then begin
    xDest := TXMLWriterSettings(Dest);

    xDest.IndentString := Self.IndentString;
    xDest.IndentType := Self.IndentType;
    xDest.LineBreak := Self.LineBreak;
    xDest.StrictXML := Self.StrictXML;
    xDest.WriteBOM := Self.WriteBOM;
  end else
    inherited;
end;

constructor TXMLWriterSettings.Create;
begin
  inherited Create;

  fLineBreak := XmlDefaultLineBreak;
  fStrictXML := True;
  fWriteBOM := True;
  fIndentType := itNone;
  fIndentString := #32#32;
end;

procedure TXMLWriterSettings.SetWriteBOM(const aWriteBOM: Boolean);
begin
  fWriteBOM := aWriteBOM;
  if Assigned(fOnSetWriteBOM) then
    fOnSetWriteBOM(Self);
end;

{ TXMLReaderSettings }

procedure TXMLReaderSettings.AssignTo(Dest: TPersistent);
var
  xDest: TXMLReaderSettings;
{$IFDEF O_GENERICS}
  xEntity: TPair<OWideString,OWideString>;
{$ENDIF}
begin
  if Dest is TXMLReaderSettings then begin
    xDest := TXMLReaderSettings(Dest);

    xDest.BreakReading := Self.BreakReading;
    xDest.LineBreak := Self.LineBreak;
    xDest.StrictXML := Self.StrictXML;
    xDest.NodePathHandling := Self.NodePathHandling;
    {$IFDEF O_GENERICS}
    xDest.EntityList.Clear;
    for xEntity in Self.EntityList do
      xDest.EntityList.Add(xEntity.Key, xEntity.Value);
    {$ELSE}
    xDest.EntityList.Assign(Self.EntityList);
    {$ENDIF}
  end else
    inherited;
end;

constructor TXMLReaderSettings.Create;
begin
  inherited Create;

  fEntityList := TXMLReaderEntityList.Create;
  fEntityList.Add('quot', '"');
  fEntityList.Add('amp', '&');
  fEntityList.Add('apos', '''');
  fEntityList.Add('lt', '<');
  fEntityList.Add('gt', '>');

  fNodePathHandling := npFull;
  fBreakReading := brAfterDocumentElement;
  fLineBreak := XmlDefaultLineBreak;
  fStrictXML := True;
  fRecognizeXMLDeclaration := True;
end;

destructor TXMLReaderSettings.Destroy;
begin
  fEntityList.Free;

  inherited Destroy;
end;

{ TXMLReader }

function TXMLReader.GetNodePath(
  const aIndex: Integer): OWideString;
begin
  Result := fNodePath.Get(aIndex);
end;

function TXMLReader.GetNodePathCount: Integer;
begin
  Result := fNodePath.Count;
end;

procedure TXMLReader.NodePathAssignTo(
  const aNodePath: TOWideStringList);
begin
  aNodePath.Assign(fNodePath);
end;

function TXMLReader.NodePathAsString: OWideString;
var
  I: Integer;
begin
  if fNodePath.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := fNodePath.Get(0);
  for I := 1 to fNodePath.Count-1 do
    Result := Result + '/' + fNodePath.Get(I);
end;

function TXMLReader.NodePathMatch(
  const aNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aNodePath) = fNodePath.Count;
  if not Result then
    Exit;

  for I := 0 to Length(aNodePath)-1 do
  if aNodePath[I] <> fNodePath.Get(I) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aNodePath.Count = fNodePath.Count;
  if not Result then
    Exit;

  for I := 0 to aNodePath.Count-1 do
  if aNodePath[I] <> fNodePath.Get(I) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: OWideString): Boolean;
var
  xNodePath: TOWideStringList;
begin
  xNodePath := TOWideStringList.Create;
  try
    OExplode(aNodePath, '/', xNodePath);

    Result := NodePathMatch(xNodePath);
  finally
    xNodePath.Free;
  end;
end;

function TXMLReader.RefIsChildOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aRefNodePath) = fNodePath.Count-1;
  if not Result then
    Exit;

  for I := 0 to Length(aRefNodePath)-1 do
  if aRefNodePath[I] <> fNodePath.Get(I) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsChildOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aRefNodePath.Count = fNodePath.Count-1;
  if not Result then
    Exit;

  for I := 0 to aRefNodePath.Count-1 do
  if aRefNodePath[I] <> fNodePath.Get(I) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsParentOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aRefNodePath) = fNodePath.Count+1;
  if not Result then
    Exit;

  for I := 0 to fNodePath.Count-1 do
  if aRefNodePath[I] <> fNodePath.Get(I) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsParentOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aRefNodePath.Count = fNodePath.Count+1;
  if not Result then
    Exit;

  for I := 0 to fNodePath.Count-1 do
  if aRefNodePath[I] <> fNodePath.Get(I) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

end.
