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

  Basic XML reader/writer. OXmlDoc.pas, OXmlPDoc.pas and OXmlSAX.pas use
  this unit to read and write XML.

  Use when performance is crucial for you.

  TOXmlWriter
    - fast sequential XML writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      (if not stated differently) and the writer takes care of valid XML escaping
    - all line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is #10 (according to XML specification)
      -> if you don't want to change them, set LineBreak to empty string ('')


  TOXmlWriterIndentation
    - supports manual indentation of XML

  TOXmlReader
    - fast sequential XML reader/parser
    - the nodes are returned as they are found in the document
    - absolutely no whitespace handling - the document is parsed exactly 1:1
      -> white space is preserved also in the very beginning of the document
      -> you have to care for white space handling in end-level
    - only line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to change them, set LineBreak to empty string ('')

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
  SysUtils, Classes, OWideSupp, OXmlUtils, OTextReadWrite, OEncoding;

type
  TOXmlWriter = class;

  TOXmlWriterElementMode = (
    stOpenOnly,    //"<node"
    stFinish,       //"<node>"
    stFinishClose); //"<node/>"

  TOXmlWriterElement = {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fOwner: TOXmlWriter;
    fElementName: OFastString;
  public
    //write opening element
    procedure OpenElement(const aElementName: OWideString; const aMode: TOXmlWriterElementMode = stOpenOnly);
    function OpenElementR(const aElementName: OWideString; const aMode: TOXmlWriterElementMode = stOpenOnly): TOXmlWriterElement;
    procedure FinishOpenElement;
    procedure FinishOpenElementClose;
    procedure Attribute(const aAttrName, aAttrValue: OWideString);

    procedure Text(const aText: OWideString; const aQuoteChar: OWideChar = #0);
    procedure RawText(const aText: OWideString);
    procedure CData(const aText: OWideString);
    procedure Comment(const aText: OWideString);
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);

    procedure CloseElement;
  end;

  TOXmlWriter = class(TObject)
  private
    fWriter: TOTextWriter;
    fLineBreak: OWideString;
    fStrictXML: Boolean;
    function GetEncoding: TEncoding;
    function GetOwnsEncoding: Boolean;
    function GetWriteBOM: Boolean;
    procedure SetEncoding(const Value: TEncoding);
    procedure SetOwnsEncoding(const Value: Boolean);
    procedure SetWriteBOM(const Value: Boolean);
  public
    constructor Create(aStream: TStream; const aEncoding: TEncoding = nil;
      const aWriteBOM: Boolean = True);
    destructor Destroy; override;
  public
    procedure XMLDeclaration(const aEncodingAttribute: Boolean = True;
      const aVersion: OWideString = '1.0';
      const aStandAlone: OWideString = '');

    // <?xml ?> tags
    procedure OpenXMLDeclaration;
    procedure FinishOpenXMLDeclaration;

    //<aElementName ... >, <aElementName ... /> etc.
    procedure OpenElement(const aElementName: OWideString; const aMode: TOXmlWriterElementMode = stOpenOnly);
    function OpenElementR(const aElementName: OWideString; const aMode: TOXmlWriterElementMode = stOpenOnly): TOXmlWriterElement;
    procedure FinishOpenElement(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    procedure FinishOpenElementClose(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    procedure CloseElement(const aElementName: OWideString);

    //write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);
    //<![CDATA[aText]]>
    procedure CData(const aText: OWideString);
    //<!--aText-->
    procedure Comment(const aText: OWideString);
    //<?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);
    //<!DOCTYPE aDocTypeRawText> - aDocTypeRawText must be escaped, it won't be processed
    procedure DocType(const aDocTypeRawText: OWideString);

    //write escaped text, escape also a quote if aQuoteChar specified
    procedure Text(const aText: OWideString; const aQuoteChar: OWideChar = #0);
    //write raw text, do not process it
    procedure RawText(const aText: OWideString); virtual;
  public
    //encoding/BOM of the text writer
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;

    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to empty string ('') if you don't want any processing
    //  default is #10 (according to XML specification)
    property LineBreak: OWideString read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: element names & values checking
    //   = false: no element names & values checking
    property StrictXML: Boolean read fStrictXML write fStrictXML;
  end;

  TOXmlWriterIndentation = class(TOXmlWriter)
  private
    fIndentLevel: Integer;
    fIndentString: OWideString;
    fOutputFormat: TXmlOutputFormat;
    fWritten: Boolean;
  public
    constructor Create(aStream: TStream; const aDefaultLevel: Integer = 0);
  public
    procedure Indent;
    procedure IncIndentLevel;
    procedure DecIndentLevel;

    procedure RawText(const aText: OWideString); override;
  public
    property IndentLevel: Integer read fIndentLevel write fIndentLevel;
    property IndentString: OWideString read fIndentString write fIndentString;
    property OutputFormat: TXmlOutputFormat read fOutputFormat write fOutputFormat;
  end;

  TOXmlReaderNodeType = (
    etDocumentStart,//start of reading
    etOpenXMLDeclaration,//xml declaration open element: <?xml
    etXMLDeclarationAttribute,//attribute in an xml declaration: name="value"
    etXMLDeclarationFinishClose,//xml declaration element finished and closed: ?>
    etOpenElement,//open element: <name
    etAttribute,//attribute: name="value"
    etFinishOpenElement,//open element finished but not closed: <node ... ">"
    etFinishOpenElementClose,//open element finished and closed: <node ... "/>"
    etCloseElement,//close element: "</node>"
    etText,//text: value
    etCData,//cdata: <![CDATA[value]]>
    etComment,//comment: <!--value-->
    etProcessingInstruction,//custom processing instruction: <?target content?>
    etDocType//doctype: <!DOCTYPE value> -> value is not unescaped by reader!!!
    );

  TOXmlReaderNode = record
    NodeType: TOXmlReaderNodeType;
    NodeName: OWideString;
    NodeValue: OWideString;
  end;

  TOXmlReader = class(TObject)
  private
    fReader: TOTextReader;
    fBreakReading: TXmlBreakReading;
    fAllowSetEncodingFromFile: Boolean;//internal, for external use ForceEncoding

    fEntityList: TOXmlReaderEntityList;
    fLineBreak: OWideString;
    fStrictXML: Boolean;

    fLastNode: TOXmlReaderNodeType;
    fDocumentNodeFound: Boolean;
    fNodePath: TOWideStringList;
    fForceEncoding: Boolean;

    fElementsToClose: Integer;

    function GetEncoding: TEncoding;
    procedure SetEncoding(const aEncoding: TEncoding);
    function GetOwnsEncoding: Boolean;
    procedure SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
    function GetLastNodeName: OWideString;
    procedure AddToNodePath(const aNodeName: OWideString);
    procedure RemoveLastFromNodePath(var aNodeName: OWideString);
    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
  private
    procedure ProcessEntity;
    procedure ProcessNewLineChar(const aLastChar: OWideChar);
    procedure ChangeEncoding(const aEncodingAlias: OWideString; var aNode: TOXmlReaderNode);
    function OpenElement(var aNode: TOXmlReaderNode): Boolean;
    function Attribute(var aNode: TOXmlReaderNode): Boolean;
    function FinishOpenElement(var aNode: TOXmlReaderNode): Boolean;
    function FinishOpenElementClose(var aNode: TOXmlReaderNode): Boolean;
    function CloseElement(var aNode: TOXmlReaderNode): Boolean;
    function Text(var aNode: TOXmlReaderNode; const aClearCustomBuffer: Boolean = True): Boolean;

    function ExclamationNode(var aNode: TOXmlReaderNode; const aNodeType: TOXmlReaderNodeType;
      const aBeginTag, aEndTag: OWideString; const aWhiteSpaceAfterBeginTag: Boolean): Boolean;
    function CData(var aNode: TOXmlReaderNode): Boolean;
    function Comment(var aNode: TOXmlReaderNode): Boolean;
    function DocType(var aNode: TOXmlReaderNode): Boolean;

    function ProcessingInstruction(var aNode: TOXmlReaderNode): Boolean;
  public
    //create an XML reader above a stream. Use aForceEncoding if you want to
    //  enforce an encoding (aForceEncoding=nil means that the encoding will
    //  be estimated automatically from <?xml encoding="..."?> or the BOM
    constructor Create(aStream: TStream; const aForceEncoding: TEncoding = nil);
    destructor Destroy; override;
  public
    //use ReadNextNode for reading next XML node
    function ReadNextNode(var aNode: TOXmlReaderNode): Boolean;
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathIsParent(const aNodePath: TOWideStringList): Boolean;
    function NodePathIsChild(const aNodePath: TOWideStringList): Boolean;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
  public
    //encoding of the text file, when set, the file will be read again from the start
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
    //if set to true, the encoding will not be changed automatically when
    //  <?xml encoding="..."?> is found
    property ForceEncoding: Boolean read fForceEncoding write fForceEncoding;

    //process known entities. add user-defined entities here
    property EntityList: TOXmlReaderEntityList read fEntityList;
    //decide if you want to read the document after the root element has been closed
    property BreakReading: TXmlBreakReading read fBreakReading write fBreakReading;
    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to empty string ('') if you don't want any processing
    //  default is your OS line break (sLineBreak)
    property LineBreak: OWideString read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: raise Exceptions when document is not valid
    //   = false: try to fix and go over document errors.
    property StrictXML: Boolean read fStrictXML write fStrictXML;

    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    property NodePathCount: Integer read GetNodePathCount;
  end;

  EXmlWriterInvalidString = class(Exception);
  EXmlReaderException = class(Exception)
  public
    constructor Create(aReader: TOXmlReader; const aMsg: string);
    constructor CreateFmt(aReader: TOXmlReader;
      const aMsg: string; const aArgs: array of const);
  end;
  EXmlReaderInvalidString = class(EXmlReaderException);
  EXmlReaderInvalidCharacter = class(EXmlReaderException);
  EXmlReaderInvalidStructure = class(EXmlReaderException);

implementation

uses OXmlLng;

{ TOXmlWriter }

procedure TOXmlWriter.CData(const aText: OWideString);
begin
  if fStrictXML and not OXmlValidCData(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidCData, [aText]);

  RawText('<![CDATA[');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawText(']]>');
end;

procedure TOXmlWriter.Comment(const aText: OWideString);
begin
  if fStrictXML and not OXmlValidComment(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidComment, [aText]);

  RawText('<!--');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawText('-->');
end;

constructor TOXmlWriter.Create(aStream: TStream; const aEncoding: TEncoding;
  const aWriteBOM: Boolean);
begin
  inherited Create;

  fWriter := TOTextWriter.Create(aStream);
  if Assigned(aEncoding) then
    fWriter.Encoding := aEncoding
  else
    fWriter.Encoding := TEncoding.UTF8;
  fWriter.WriteBOM := aWriteBOM;

  fLineBreak := #10;

  fStrictXML := True;
end;

destructor TOXmlWriter.Destroy;
begin
  fWriter.Free;

  inherited;
end;

procedure TOXmlWriter.DocType(const aDocTypeRawText: OWideString);
begin
  RawText('<!DOCTYPE ');
  RawText(aDocTypeRawText);//MUST BE RAW ESCAPED TEXT - the programmer has to be sure that aDocTypeRawText is valid
  RawText('>');
end;

procedure TOXmlWriter.CloseElement(const aElementName: OWideString);
begin
  RawText('</');
  RawText(aElementName);//can be rawtext, because validated (in OpenElement)!
  RawText('>');
end;

function TOXmlWriter.GetEncoding: TEncoding;
begin
  Result := fWriter.Encoding;
end;

function TOXmlWriter.GetOwnsEncoding: Boolean;
begin
  Result := fWriter.OwnsEncoding;
end;

function TOXmlWriter.GetWriteBOM: Boolean;
begin
  Result := fWriter.WriteBOM;
end;

procedure TOXmlWriter.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if fStrictXML and not OXmlValidName(aTarget) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPITarget, [aTarget]);

  if fStrictXML and not OXmlValidPIContent(aContent) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPIContent, [aContent]);

  RawText('<?');
  RawText(aTarget);
  if aTarget <> '' then
    RawText(' ');
  RawText(aContent);//MUST BE RAWTEXT - must contain unescaped characters
  RawText('?>');
end;

procedure TOXmlWriter.RawText(const aText: OWideString);
begin
  fWriter.WriteString(aText);
end;

procedure TOXmlWriter.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fStrictXML and not OXmlValidName(aAttrName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidAttributeName, [aAttrName]);

  RawText(' ');
  RawText(aAttrName);//can be rawtext, because validated!
  RawText('="');
  Text(aAttrValue, '"');
  RawText('"');
end;

procedure TOXmlWriter.SetEncoding(const Value: TEncoding);
begin
  fWriter.Encoding := Value;
end;

procedure TOXmlWriter.SetOwnsEncoding(const Value: Boolean);
begin
  fWriter.OwnsEncoding := Value;
end;

procedure TOXmlWriter.SetWriteBOM(const Value: Boolean);
begin
  fWriter.WriteBOM := Value;
end;

procedure TOXmlWriter.OpenXMLDeclaration;
begin
  RawText('<?xml');
end;

procedure TOXmlWriter.FinishOpenXMLDeclaration;
begin
  RawText('?>');
end;

procedure TOXmlWriter.OpenElement(const aElementName: OWideString; const aMode: TOXmlWriterElementMode);
begin
  if fStrictXML and not OXmlValidName(aElementName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidElementName, [aElementName]);

  RawText('<');
  RawText(aElementName);
  case aMode of
    stFinish: RawText('>');
    stFinishClose: RawText('/>');
  end;
end;

procedure TOXmlWriter.FinishOpenElement(const aElementName: OWideString);
begin
  RawText('>');
end;

procedure TOXmlWriter.FinishOpenElementClose(const aElementName: OWideString);
begin
  RawText('/>');
end;

function TOXmlWriter.OpenElementR(const aElementName: OWideString;
  const aMode: TOXmlWriterElementMode): TOXmlWriterElement;
begin
  OpenElement(aElementName, aMode);

  Result.fOwner := Self;
  Result.fElementName := OWideToFast(aElementName);
end;

procedure TOXmlWriter.Text(const aText: OWideString; const aQuoteChar: OWideChar = #0);
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
        if (fLineBreak = '') then//no line break handling
          RawText(xC)
        else if ((I = 1) or (aText[I-1] <> #13)) then//previous character is not #13 (i.e. this is a simple #10 not #13#10) -> write fLineBreak
          RawText(fLineBreak);
      #13:
        if fLineBreak = '' then
          RawText(xC)
        else
          RawText(fLineBreak);
    else
      RawText(xC);
    end;
  end;
end;

procedure TOXmlWriter.XMLDeclaration(const aEncodingAttribute: Boolean;
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

{ TOXmlWriterElement }

procedure TOXmlWriterElement.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  fOwner.Attribute(aAttrName, aAttrValue);
end;

procedure TOXmlWriterElement.CData(const aText: OWideString);
begin
  fOwner.CData(aText);
end;

procedure TOXmlWriterElement.Comment(const aText: OWideString);
begin
  fOwner.Comment(aText);
end;

procedure TOXmlWriterElement.CloseElement;
begin
  fOwner.CloseElement(OFastToWide(fElementName));

  //DO NOT USE THIS RECORD ANY MORE
  fOwner := nil;
  fElementName := '';
end;

procedure TOXmlWriterElement.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  fOwner.ProcessingInstruction(aTarget, aContent);
end;

procedure TOXmlWriterElement.RawText(const aText: OWideString);
begin
  fOwner.RawText(aText);
end;

procedure TOXmlWriterElement.OpenElement(const aElementName: OWideString;
  const aMode: TOXmlWriterElementMode);
begin
  fOwner.OpenElement(aElementName, aMode);
end;

procedure TOXmlWriterElement.FinishOpenElement;
begin
  fOwner.FinishOpenElement;
end;

procedure TOXmlWriterElement.FinishOpenElementClose;
begin
  fOwner.FinishOpenElementClose;

  //DO NOT USE THIS RECORD ANY MORE
  fOwner := nil;
  fElementName := '';
end;

function TOXmlWriterElement.OpenElementR(const aElementName: OWideString;
  const aMode: TOXmlWriterElementMode): TOXmlWriterElement;
begin
  Result := fOwner.OpenElementR(aElementName, aMode);
end;

procedure TOXmlWriterElement.Text(const aText: OWideString;
  const aQuoteChar: OWideChar);
begin
  fOwner.Text(aText, aQuoteChar);
end;

{ TOXmlReader }

function TOXmlReader.FinishOpenElement(var aNode: TOXmlReaderNode): Boolean;
begin
  aNode.NodeName := GetLastNodeName;
  aNode.NodeValue := '';
  if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute] then
    aNode.NodeType := etXMLDeclarationFinishClose
  else
    aNode.NodeType := etFinishOpenElement;

  Result := True;
end;

function TOXmlReader.FinishOpenElementClose(
  var aNode: TOXmlReaderNode): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
begin
  //opened after a '?' for PI or '/' for an element.

  fReader.ReadNextChar(xC);//must be '>'
  if xC <> '>' then begin
    if fStrictXML then begin
      if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute] then
        raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, ['?'])
      else
        raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, ['/']);
    end else begin
      //let's be generous and go over this invalid character
      fReader.UndoRead;
      Result := ReadNextNode(aNode);
      Exit;
    end;
  end;

  aNode.NodeName := '';
  aNode.NodeValue := '';
  if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute] then begin
    aNode.NodeType := etXMLDeclarationFinishClose;
  end else begin
    aNode.NodeType := etFinishOpenElementClose;
    RemoveLastFromNodePath(aNode.NodeName);
  end;

  Result := True;
end;

function TOXmlReader.Text(var aNode: TOXmlReaderNode; const aClearCustomBuffer: Boolean): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
begin
  if aClearCustomBuffer then
    fReader.ClearCustomBuffer;
  fReader.ReadNextChar(xC);

  while True do begin
    case xC of
      #0, '<': break;
      '&': ProcessEntity;
      #10, #13: begin
        if fLineBreak <> '' then begin
          ProcessNewLineChar(xC);
        end else begin
          fReader.WritePreviousCharToBuffer;
        end;
      end;
    else
      fReader.WritePreviousCharToBuffer;
    end;
    fReader.ReadNextChar(xC);
  end;

  if xC <> #0 then
    fReader.UndoRead;
  aNode.NodeType := etText;
  aNode.NodeName := '';
  aNode.NodeValue := fReader.GetCustomBuffer;
  Result := True;
end;

procedure TOXmlReader.AddToNodePath(const aNodeName: OWideString);
begin
  fNodePath.Add(aNodeName);
end;

function TOXmlReader.Attribute(var aNode: TOXmlReaderNode): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
  xQuotationMark: OWideChar{$IFDEF FPC} = #0{$ENDIF};
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar(xC);

  if fStrictXML and not OXmlIsNameStartChar(xC) then
    raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, [xC]);

  if not fStrictXML then begin
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
  aNode.NodeName := fReader.GetCustomBuffer;

  if not fStrictXML then
    while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "attr ="
      fReader.ReadNextChar(xC);

  if xC <> '=' then begin
    //let's be generous and allow attributes without values - even if they are not allowed by xml spec
    if fStrictXML then begin
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_EqualSignMustFollowAttribute, [aNode.NodeName]);
    end else begin
      aNode.NodeValue := '';
      fReader.UndoRead;
    end;
  end else begin
    fReader.ReadNextChar(xC);
    if not fStrictXML then
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
      if fStrictXML then begin
        raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_AttributeValueMustBeEnclosed, [aNode.NodeName]);
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

    aNode.NodeValue := fReader.GetCustomBuffer;
  end;

  if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute] then begin
    aNode.NodeType := etXMLDeclarationAttribute;

    if not fForceEncoding and fAllowSetEncodingFromFile and
      (aNode.NodeName = 'encoding')
    then
      ChangeEncoding(aNode.NodeValue, aNode);
  end else begin
    aNode.NodeType := etAttribute;
  end;

  Result := True;
end;

function TOXmlReader.CData(var aNode: TOXmlReaderNode): Boolean;
begin
  Result := ExclamationNode(aNode, etCData, '<![CDATA[', ']]>', False);
end;

procedure TOXmlReader.ChangeEncoding(const aEncodingAlias: OWideString; var aNode: TOXmlReaderNode);
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
      fReader.UnblockFlushTempBuffer;//was blocked in TOXmlReader.Create
    end;
    //go back to current position
    xInXMLDeclaration := False;
    xLastName := aNode.NodeName;
    fLastNode := etDocumentStart;
    //parse from beginning back to the encoding attribute
    while ReadNextNode(aNode) do begin
      case aNode.NodeType of
        etOpenXMLDeclaration: xInXMLDeclaration := True;
        etOpenElement: xInXMLDeclaration := False;
        etXMLDeclarationAttribute:
        if xInXMLDeclaration and (aNode.NodeName = xLastName) then begin
          Exit;
        end;
      end;
    end;
  end;
end;

function TOXmlReader.Comment(var aNode: TOXmlReaderNode): Boolean;
begin
  Result := ExclamationNode(aNode, etComment, '<!--', '-->', False);
end;

constructor TOXmlReader.Create(aStream: TStream; const aForceEncoding: TEncoding);
begin
  fLastNode := etDocumentStart;
  fDocumentNodeFound := False;
  fReader := TOTextReader.Create(aStream, TEncoding.UTF8);
  fAllowSetEncodingFromFile := True;
  fReader.BlockFlushTempBuffer;//will be unblocked when fAllowSetEncodingFromFile is set to false
  if Assigned(aForceEncoding) then begin
    Self.Encoding := aForceEncoding;
  end;
  fBreakReading := brAfterDocumentNode;

  fEntityList := TOXmlReaderEntityList.Create;
  fEntityList.Add('quot', '"');
  fEntityList.Add('amp', '&');
  fEntityList.Add('apos', '''');
  fEntityList.Add('lt', '<');
  fEntityList.Add('gt', '>');

  fNodePath := TOWideStringList.Create;

  fLineBreak := sLineBreak;

  fStrictXML := True;
end;

destructor TOXmlReader.Destroy;
begin
  fReader.Free;
  fEntityList.Free;
  fNodePath.Free;

  inherited;
end;

function TOXmlReader.DocType(var aNode: TOXmlReaderNode): Boolean;
begin
  Result := ExclamationNode(aNode, etDocType, '<!DOCTYPE', '>', True);
end;

procedure TOXmlReader.SetEncoding(const aEncoding: TEncoding);
begin
  fReader.Encoding := aEncoding;
  fForceEncoding := True;
end;

procedure TOXmlReader.SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
begin
  fReader.OwnsEncoding := aSetOwnsEncoding;
end;

function TOXmlReader.ExclamationNode(var aNode: TOXmlReaderNode;
  const aNodeType: TOXmlReaderNodeType; const aBeginTag, aEndTag: OWideString;
  const aWhiteSpaceAfterBeginTag: Boolean): Boolean;
var
  I: Integer;
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
  xPreviousC: OWideString;
begin
  fReader.ClearCustomBuffer;
  fReader.WriteStringToBuffer('<!');
  Result := True;
  for I := 3 to Length(aBeginTag) do begin
    fReader.ReadNextChar(xC);
    if aBeginTag[I] <> UpperCase(xC) then begin
      Result := False;
      fReader.UndoRead;
      Break;
    end;
    fReader.WritePreviousCharToBuffer;
  end;

  if aWhiteSpaceAfterBeginTag and Result then begin
    //must be followed by a whitespace character
    fReader.ReadNextChar(xC);
    fReader.WritePreviousCharToBuffer;
    Result := OXmlIsWhiteSpaceChar(xC);
  end;

  if not Result then begin
    //header not found
    if fStrictXML then begin
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidCharacterInText, ['<']);
    end else begin
      //output as text
      if xC <> '<' then begin
        Result := Text(aNode, False);
      end else begin
        Result := True;
        aNode.NodeType := etText;
        aNode.NodeValue := fReader.GetCustomBuffer;
        aNode.NodeName := '';
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

    aNode.NodeType := aNodeType;
    aNode.NodeName := '';
    aNode.NodeValue := fReader.GetCustomBuffer;
  end;
end;

function TOXmlReader.OpenElement(var aNode: TOXmlReaderNode): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar(xC);

  case xC of
    '!': begin
      //comment or cdata
      fReader.ReadNextChar(xC);
      fReader.UndoRead;
      case xC of
        '[': Result := CData(aNode);
        '-': Result := Comment(aNode);
        'D', 'd': Result := DocType(aNode);
      else
        if fStrictXML then begin
          raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInText, ['<']);
        end else begin
          fReader.WriteStringToBuffer('<!');
          if xC <> '<' then begin
            Result := Text(aNode, False);
          end else begin
            Result := True;
            aNode.NodeType := etText;
            aNode.NodeValue := fReader.GetCustomBuffer;
            aNode.NodeName := '';
          end;
          Exit;
        end;
      end;
      Exit;
    end;
    '/': begin
      //close element
      Result := CloseElement(aNode);
      Exit;
    end;
    '?': begin
      //processing instruction
      Result := ProcessingInstruction(aNode);
      Exit;
    end;
  end;

  if fAllowSetEncodingFromFile then begin
    fAllowSetEncodingFromFile := False;// -> first Node found, encoding change is not allowed any more
    fReader.UnblockFlushTempBuffer;//was blocked in TOXmlReader.Create
  end;

  if fStrictXML then begin
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
        Result := Text(aNode, False);
      end else begin
        Result := True;
        aNode.NodeType := etText;
        aNode.NodeValue := fReader.GetCustomBuffer;
        aNode.NodeName := '';
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

  aNode.NodeName := fReader.GetCustomBuffer;
  aNode.NodeValue := '';
  aNode.NodeType := etOpenElement;
  AddToNodePath(aNode.NodeName);

  Result := True;
end;

function TOXmlReader.ReadNextNode(var aNode: TOXmlReaderNode): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
begin
  try
    if fElementsToClose > 0 then begin
      //close elements
      Result := True;
      aNode.NodeType := etCloseElement;
      aNode.NodeName := '';
      aNode.NodeValue := '';
      RemoveLastFromNodePath(aNode.NodeName);
      Dec(fElementsToClose);
      Exit;
    end;

    Result := False;

    if (fBreakReading = brAfterDocumentNode) and (fDocumentNodeFound) and (fNodePath.Count = 0) then
      Exit;

    fReader.ReadNextChar(xC);
    case xC of
      #0: Exit;
      '<': begin
        if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute, etOpenElement, etAttribute] then
        begin
          if fStrictXML then begin
            raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInElement, [xC]);
          end else begin
            fReader.UndoRead;
            Result := Attribute(aNode);
          end;
        end else begin
          Result := OpenElement(aNode);
        end;
      end;
      '?': begin
        if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute] then
        begin
          Result := FinishOpenElementClose(aNode)
        end
        else
        begin
          //text
          fReader.UndoRead;
          Result := Text(aNode);
        end;
      end;
      '/': begin
        if fLastNode in [etOpenElement, etAttribute] then
        begin
          Result := FinishOpenElementClose(aNode)
        end
        else
        begin
          //text
          fReader.UndoRead;
          Result := Text(aNode);
        end;
      end;
      '>': begin
        if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute, etOpenElement, etAttribute] then
        begin
          Result := FinishOpenElement(aNode);
        end else begin
          if fStrictXML then begin
            raise EXmlReaderInvalidCharacter.CreateFmt(Self, OXmlLng_InvalidCharacterInText, [xC]);
          end else begin
            //text
            fReader.UndoRead;
            Result := Text(aNode);
          end;
        end;
      end;
    else
      if fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute, etOpenElement, etAttribute]
      then begin
        while OXmlIsWhiteSpaceChar(xC) do
          fReader.ReadNextChar(xC);

        if ((xC ='/') and (fLastNode in [etOpenElement, etAttribute])) or
           ((xC = '?') and (fLastNode in [etOpenXMLDeclaration, etXMLDeclarationAttribute]))
        then begin
          Result := FinishOpenElementClose(aNode);
        end else if ((xC = '>') and (fLastNode in [etOpenElement, etAttribute])) then begin
          Result := FinishOpenElement(aNode);
        end else begin
          fReader.UndoRead;
          Result := Attribute(aNode);
        end;
      end else begin
        //text
        fReader.UndoRead;
        Result := Text(aNode);
      end;
    end;

  finally
    fLastNode := aNode.NodeType;
  end;
end;

procedure TOXmlReader.RemoveLastFromNodePath(var aNodeName: OWideString);
var
  I: Integer;
begin
  if (fNodePath.Count = 0) then begin
    //there is no open element
    if fStrictXML then
      raise EXmlReaderInvalidStructure.Create(Self, OXmlLng_TooManyElementsClosed);
  end else begin
    if (aNodeName <> '') and
       (fNodePath[fNodePath.Count-1] <> aNodeName)
    then begin
      //element names do not match
      if fStrictXML then begin
        raise EXmlReaderInvalidStructure.CreateFmt(Self, OXmlLng_WrongElementClosed, [aNodeName, fNodePath[fNodePath.Count-1]]);
      end else begin
        //trying to close parent element
        for I := fNodePath.Count-2 downto 0 do
        if (fNodePath[I] = aNodeName) then begin
          //parent element with the same name found, we have to close more elements in the future!!!
          fElementsToClose := (fNodePath.Count - I - 1);
          Break;
        end;

        //delete the last one from fNodePath
        //  + rename the element if names differ
        if aNodeName <> '' then
          aNodeName := fNodePath[fNodePath.Count-1];
        fNodePath.Delete(fNodePath.Count-1);
      end;
    end else begin
      //everything is fine -> delete last from fNodePath
      fNodePath.Delete(fNodePath.Count-1);
    end;
  end;
end;

function TOXmlReader.CloseElement(var aNode: TOXmlReaderNode): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar(xC);

  if fStrictXML then begin
    if not OXmlIsNameStartChar(xC) then
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidStringInText, ['</']);

    while OXmlIsNameChar(xC) do begin
      fReader.WritePreviousCharToBuffer;
      fReader.ReadNextChar(xC);
    end;
    while OXmlIsWhiteSpaceChar(xC) do begin
      fReader.ReadNextChar(xC);
    end;
    if xC <> '>' then
      raise EXmlReaderInvalidString.CreateFmt(Self, OXmlLng_InvalidStringInText, ['</']);
  end else begin
    if not OXmlIsNameChar(xC) then begin
      fReader.WriteStringToBuffer('</');
      fReader.UndoRead;
      if xC <> '<' then begin
        Result := Text(aNode, False);
      end else begin
        Result := True;
        aNode.NodeType := etText;
        aNode.NodeValue := fReader.GetCustomBuffer;
        aNode.NodeName := '';
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

  aNode.NodeName := fReader.GetCustomBuffer;
  aNode.NodeValue := '';
  aNode.NodeType := etCloseElement;

  RemoveLastFromNodePath(aNode.NodeName);

  Result := True;
end;

function TOXmlReader.GetEncoding: TEncoding;
begin
  Result := fReader.Encoding;
end;

function TOXmlReader.GetLastNodeName: OWideString;
begin
  if fNodePath.Count > 0 then
    Result := fNodePath[fNodePath.Count-1]
  else
    Result := '';
end;

function TOXmlReader.GetNodePath(const aIndex: Integer): OWideString;
begin
  Result := fNodePath[aIndex];
end;

function TOXmlReader.GetNodePathCount: Integer;
begin
  Result := fNodePath.Count;
end;

function TOXmlReader.GetOwnsEncoding: Boolean;
begin
  Result := fReader.OwnsEncoding;
end;

function TOXmlReader.NodePathMatch(const aNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aNodePath.Count = fNodePath.Count;
  if not Result then
    Exit;

  for I := 0 to aNodePath.Count-1 do
  if aNodePath[I] <> fNodePath[I] then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TOXmlReader.NodePathAssignTo(const aNodePath: TOWideStringList);
begin
  aNodePath.Assign(fNodePath);
end;

function TOXmlReader.NodePathIsChild(
  const aNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aNodePath.Count = fNodePath.Count+1;
  if not Result then
    Exit;

  for I := 0 to fNodePath.Count-1 do
  if aNodePath[I] <> fNodePath[I] then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TOXmlReader.NodePathIsParent(
  const aNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aNodePath.Count = fNodePath.Count-1;
  if not Result then
    Exit;

  for I := 0 to aNodePath.Count-1 do
  if aNodePath[I] <> fNodePath[I] then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TOXmlReader.NodePathMatch(const aNodePath: OWideString): Boolean;
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

procedure TOXmlReader.ProcessEntity;
  procedure _EntityError(bCustomEntityString: OWideString = '');
  begin
    if fStrictXML then begin
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
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
  xEntityString: OWideString;
  xOutputChar: Integer;
  xIsHex: Boolean;
  xOutputEntity: OWideString;
begin
  xOutputEntity := '';
  xOutputChar := 0;
  fReader.ClearCustomBuffer(1);

  fReader.ReadNextChar(xC);
  if xC = '#' then begin
    fReader.ClearCustomBuffer(1);
    fReader.ReadNextChar(xC);
    xIsHex := (xC = 'x');
    if xIsHex then begin
      fReader.ClearCustomBuffer(1);
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
    if not fEntityList.TryGetValue(xEntityString, xOutputEntity) then begin
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

function TOXmlReader.ProcessingInstruction(
  var aNode: TOXmlReaderNode): Boolean;
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
  xPreviousC: OWideChar;
begin
  fReader.ClearCustomBuffer;
  fReader.ReadNextChar(xC);

  if fStrictXML then begin
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

  aNode.NodeName := fReader.GetCustomBuffer;
  if (fLastNode = etDocumentStart) and SameText(aNode.NodeName, 'xml') then begin
    //xml declaration: <?xml attr="value"?>
    aNode.NodeType := etOpenXMLDeclaration;
    aNode.NodeValue := '';
    Result := True;
    Exit;
  end;

  //custom processing instruction
  aNode.NodeType := etProcessingInstruction;
  fReader.ClearCustomBuffer;
  if not fStrictXML and (aNode.NodeName = '') then
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

  aNode.NodeValue := fReader.GetCustomBuffer;
  Result := True;
end;

procedure TOXmlReader.ProcessNewLineChar(const aLastChar: OWideChar);
var
  xC: OWideChar{$IFDEF FPC} = #0{$ENDIF};
  I: Integer;
begin
  if aLastChar = #13 then begin
    fReader.ReadNextChar(xC);
    if xC <> #10 then
      fReader.UndoRead;
  end;

  for I := 1 to Length(fLineBreak) do
    fReader.WriteCharToBuffer(OWideChar(fLineBreak[I]));
end;

{ TOXmlWriterIndentation }

constructor TOXmlWriterIndentation.Create(aStream: TStream;
  const aDefaultLevel: Integer);
begin
  inherited Create(aStream);

  fIndentLevel := aDefaultLevel;
  fIndentString := '  ';//2 spaces
end;

procedure TOXmlWriterIndentation.DecIndentLevel;
begin
  Dec(fIndentLevel);
end;

procedure TOXmlWriterIndentation.Indent;
var I: Integer;
begin
  if (OutputFormat in [ofFlat, ofIndent]) and fWritten then
    RawText(#10);//do not indent at the very beginning of the document

  if OutputFormat = ofIndent then
  for I := 1 to fIndentLevel do
    RawText(fIndentString);
end;

procedure TOXmlWriterIndentation.IncIndentLevel;
begin
  Inc(fIndentLevel);
end;

procedure TOXmlWriterIndentation.RawText(const aText: OWideString);
begin
  inherited RawText(aText);

  fWritten := True;
end;

{ EXmlReaderException }

constructor EXmlReaderException.Create(aReader: TOXmlReader;
  const aMsg: string);
begin
  inherited Create(
    aMsg+
    sLineBreak+sLineBreak+
    Format(OXmlLng_ReadingAt, [aReader.fReader.ReadPreviousString(10)+aReader.fReader.ReadString(30)]));
end;

constructor EXmlReaderException.CreateFmt(aReader: TOXmlReader;
  const aMsg: string; const aArgs: array of const);
begin
  inherited Create(
    Format(aMsg, aArgs)+
    sLineBreak+sLineBreak+
    Format(OXmlLng_ReadingAt, [aReader.fReader.ReadPreviousString(10)+aReader.fReader.ReadString(30)]));
end;

end.
