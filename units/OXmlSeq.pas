unit OXmlSeq;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlSeq.pas

  Sequential DOM XML parser based on XmlPDOM.pas
    -> read particular XML elements into DOM and so parse huge XML documents
       with small memory usage but still take advantage of DOM capabilities.
    -> you can also omit some XML passages and get only the information
       that is insteresting to you
    -> OXmlSeq is about as fast as XmlPDOM - there is no significant performance
       penalty when using sequential parser instead of pure DOM
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
  OXmlDOM;

type

  TXMLSeqParser = class(TObject)
  private
    fReader: TOXmlReader;
    fOwnsReader: Boolean;
    fReaderNode: TOXmlReaderNode;
    fDataRead: Boolean;
    fTempReaderPath: TOWideStringList;
    fTempNodePath: TOWideStringList;
    fStream: TStream;
    fOwnsStream: Boolean;
    fXmlDoc: IXMLDocument;

    function ReadNextChildNodeCustom(const aOnlyElementHeader: Boolean;
      var aElementIsOpen: Boolean): Boolean;

    function GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    procedure SetWhiteSpaceHandling(const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
    function GetReaderSettings: TOXmlReaderSettings;
    function GetApproxStreamPosition: ONativeInt;
    function GetStreamSize: ONativeInt;
  protected
    procedure DoCreate(const aForceEncoding: TEncoding); virtual;
    procedure DoDestroy; virtual;
  public
    constructor CreateFromReader(const aReader: TOXmlReader);
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
    //seek forward through document to an element path.
    //  the path can be absolute or relative, no XPath support
    //  (with the exception of "//elementName" syntax)
    //  only XML elements supported, no attributes!
    //  (examples: "/root/node/child", "node/child", "//node" etc.)
    function GoToPath(const aPath: OWideString): Boolean;
    //seek to next child XML element and read its name, text nodes are ignored.
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element (i.e. no GoToPath('..') call
    //    is needed).
    function GoToNextChildElement(var aElementName: OWideString): Boolean;

    //seek to next child XML element and read the header, text nodes are ignored.
    //  (e.g. '<child attr="value">' will be read
    //  aElementIsOpen will be set to true if the element is open (<node>).
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element (i.e. no GoToPath('..') call
    //    is needed).
    function ReadNextChildElementHeader(var aNode: PXMLNode;
      var aElementIsOpen: Boolean): Boolean;
    //the same as ReadNextChildElementHeader, but no information is returned
    function PassNextChildElementHeader(var aElementIsOpen: Boolean): Boolean;

    //seek to next child XML element and read the header, text nodes are ignored.
    //  (e.g. '<child attr="value">' will be read
    //  if element has child nodes, the parser will seek to the closing element
    //  so that the same parent level is reached again
    function ReadNextChildElementHeaderClose(var aNode: PXMLNode): Boolean;

    //seek to next XML node (element, text, CData, etc.) and read the whole
    //  element contents with attributes and children.
    //  (e.g. '<child attr="value">my text<br />2nd line</child>' will be read.
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element.
    function ReadNextChildNode(var aNode: PXMLNode): Boolean;
    //the same as ReadNextChildNode, but no information is returned
    function PassNextChildNode: Boolean;

  public
    //document whitespace handling
    //  -> used only in "ReadNextChildNode" for the resulting document
    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;
    //XML reader settings
    property ReaderSettings: TOXmlReaderSettings read GetReaderSettings;
  public
    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: ONativeInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: ONativeInt read GetStreamSize;
  end;

implementation

{ TXMLSeqParser }

constructor TXMLSeqParser.Create(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  inherited Create;

  fStream := aStream;
  fOwnsStream := False;

  DoCreate(aForceEncoding);
end;

{$IFDEF O_DELPHI_2009_UP}
constructor TXMLSeqParser.CreateFromBuffer(const aBuffer: TBytes;
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

constructor TXMLSeqParser.CreateFromFile(const aFileName: String;
  const aForceEncoding: TEncoding);
begin
  inherited Create;

  fStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  fOwnsStream := True;

  DoCreate(aForceEncoding);
end;

constructor TXMLSeqParser.CreateFromReader(const aReader: TOXmlReader);
begin
  inherited Create;

  fStream := nil;
  fOwnsStream := False;
  fReader := aReader;
  fOwnsReader := False;

  DoCreate(nil);
end;

constructor TXMLSeqParser.CreateFromXML(const aXML: OWideString);
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

destructor TXMLSeqParser.Destroy;
begin
  DoDestroy;

  inherited;
end;

procedure TXMLSeqParser.DoCreate(const aForceEncoding: TEncoding);
begin
  if not Assigned(fReader) then begin
    fReader := TOXmlReader.Create(fStream, aForceEncoding);
    fReaderNode.NodeType := etDocumentStart;
    fReaderNode.NodeName := '';
    fReaderNode.NodeValue := '';

    fOwnsReader := True;
  end;

  fXmlDoc := TXMLDocument.Create;
  fTempNodePath := TOWideStringList.Create;
  fTempReaderPath := TOWideStringList.Create;
end;

procedure TXMLSeqParser.DoDestroy;
begin
  if fOwnsReader then
    fReader.Free;
  if fOwnsStream then
    fStream.Free;
  fTempNodePath.Free;
  fTempReaderPath.Free;
end;

function TXMLSeqParser.GetApproxStreamPosition: ONativeInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TXMLSeqParser.GetReaderSettings: TOXmlReaderSettings;
begin
  Result := fReader;//direct access to reader settings
end;

function TXMLSeqParser.GetStreamSize: ONativeInt;
begin
  Result := fReader.StreamSize;
end;

function TXMLSeqParser.GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
begin
  Result := fXmlDoc.WhiteSpaceHandling;
end;

function TXMLSeqParser.GoToNextChildElement(
  var aElementName: OWideString): Boolean;
begin
  while fReader.ReadNextNode(fReaderNode) do
  begin
    case fReaderNode.NodeType of
      etOpenElement: begin
        aElementName := fReaderNode.NodeName;
        Result := True;
        Exit;
      end;
      etCloseElement: begin
        Break;//Result = False
      end;
    end;
  end;

  aElementName := '';
  Result := False;
end;

function TXMLSeqParser.GoToPath(const aPath: OWideString): Boolean;
begin
  OExplode(aPath, '/', fTempNodePath);
  fReader.NodePathAssignTo(fTempReaderPath);
  OExpandPath(fTempReaderPath, fTempNodePath);

  if fReader.NodePathMatch(fTempNodePath) then begin
    Result := True;
    Exit;
  end;

  while fReader.ReadNextNode(fReaderNode) do
  if (fReaderNode.NodeType in [etOpenElement, etCloseElement, etFinishOpenElementClose]) and
      fReader.NodePathMatch(fTempNodePath)
  then begin
    fDataRead := True;
    Result := True;
    Exit;
  end;

  Result := False;
end;

function TXMLSeqParser.PassNextChildElementHeader(
  var aElementIsOpen: Boolean): Boolean;
var
  x: PXMLNode;
begin
  Result := ReadNextChildElementHeader({%H-}x, aElementIsOpen);
end;

function TXMLSeqParser.PassNextChildNode: Boolean;
var
  x: PXMLNode;
begin
  //use ReadNextChildElementHeaderClose instead of ReadNextChildNode
  //  -> the same result/functionality, but better performance because
  //  the inner nodes are not created
  Result := ReadNextChildElementHeaderClose({%H-}x);
end;

function TXMLSeqParser.ReadNextChildElementHeader(
  var aNode: PXMLNode; var aElementIsOpen: Boolean): Boolean;
begin
  Result := ReadNextChildNodeCustom(True, aElementIsOpen);
  if Result then
    aNode := fXmlDoc.DocumentElement;
end;

function TXMLSeqParser.ReadNextChildElementHeaderClose(
  var aNode: PXMLNode): Boolean;
var
  xElementIsOpen: Boolean;
begin
  Result := ReadNextChildElementHeader(aNode, {%H-}xElementIsOpen);
  if Result and xElementIsOpen then
    GoToPath('..');//go back to parent
end;

function TXMLSeqParser.ReadNextChildNode(var aNode: PXMLNode): Boolean;
var
  x: Boolean;
begin
  Result := ReadNextChildNodeCustom(False, {%H-}x);
  if Result then
    aNode := fXmlDoc.DocumentElement;
end;

function TXMLSeqParser.ReadNextChildNodeCustom(
  const aOnlyElementHeader: Boolean; var aElementIsOpen: Boolean): Boolean;
var
  xLastNode: PXMLNode;
begin
  Result := False;

  fXmlDoc.Loading := True;
  try
    fXmlDoc.Clear;

    if fReaderNode.NodeType = etOpenElement then begin
      //last found was opening element (most probably from GoToPath()), write it down!
      xLastNode := fXmlDoc.Node.AddChild(fReaderNode.NodeName)
    end else begin
      //last found was something else
      xLastNode := fXmlDoc.Node;

      //go to next child
      if aOnlyElementHeader then begin
        while fReader.ReadNextNode(fReaderNode) do begin
          case fReaderNode.NodeType of
            etOpenElement://new element found
            begin
              xLastNode := fXmlDoc.Node.AddChild(fReaderNode.NodeName);
              Break;
            end;
            etCloseElement://parent element may be closed
              Exit;
          end;
        end;

        if xLastNode = fXmlDoc.Node then//next child not found, exit
          Exit;
      end;
    end;

    if not aOnlyElementHeader then begin
      //read whole element contents
      Result := xLastNode.LoadFromReader(fReader);
    end else begin
      //read only element header
      while fReader.ReadNextNode(fReaderNode) do begin
        case fReaderNode.NodeType of
          etXMLDeclarationAttribute, etAttribute:
            xLastNode.Attributes[fReaderNode.NodeName] := fReaderNode.NodeValue;
          etFinishOpenElement:
          begin
            aElementIsOpen := True;
            Result := True;
            Exit;
          end;
          etXMLDeclarationFinishClose, etFinishOpenElementClose, etCloseElement:
          begin
            aElementIsOpen := False;
            Result := True;
            Exit;
          end;
        end;
      end;//while
    end;//if
  finally
    fXmlDoc.Loading := False;
  end;
end;

procedure TXMLSeqParser.SetWhiteSpaceHandling(
  const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
begin
  fXmlDoc.WhiteSpaceHandling := aWhiteSpaceHandling;
end;

end.
