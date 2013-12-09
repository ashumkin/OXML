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

  Sequential DOM XML parser based on XmlPDoc.pas
    -> read particular XML elements into DOM and so parse huge XML documents
       with small memory usage but still take advantage of DOM capabilities.
    -> you can also omit some XML passages and get only the information
       that is insteresting to you
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
  OXmlPDOM;

type

  TXMLSeqParser = class(TObject)
  private
    fReader: TOXmlReader;
    fReaderNode: TOXmlReaderNode;
    fDataRead: Boolean;
    fTempReaderPath: TOWideStringList;
    fTempNodePath: TOWideStringList;
    fStream: TStream;
    fOwnsStream: Boolean;
    fXmlDoc: IXMLDocument;

    function ReadNextChildNodeCustom(const aOnlyElementHeader: Boolean;
      var aElementIsOpen: Boolean): Boolean;
  protected
    procedure DoCreate(const aForceEncoding: TEncoding); virtual;
    procedure DoDestroy; virtual;
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
    //seek forward through document to an element path.
    //  the path can be absolute or relative, no XPath support.
    //  only XML elements supported, no attributes!
    //  (examples: "/root/node/child", "node/child" etc.)
    function GoToPath(const aPath: OWideString): Boolean;

    //seek to next child XML element and read the header, text nodes are ignored.
    //  (e.g. '<child attr="value">' will be read
    //  aElementIsOpen will be set to true if the element is open (<node>).
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element (i.e. no GoToParent call
    //    is needed).
    function ReadNextChildElementHeader(var aNode: PXMLNode;
      var aElementIsOpen: Boolean): Boolean;

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
  fReader := TOXmlReader.Create(fStream, aForceEncoding);
  fReader.BreakReading := brNone;

  fReaderNode.NodeType := etDocumentStart;
  fReaderNode.NodeName := '';
  fReaderNode.NodeValue := '';

  fXmlDoc := CreateXMLDoc;
  fTempNodePath := TOWideStringList.Create;
  fTempReaderPath := TOWideStringList.Create;
end;

procedure TXMLSeqParser.DoDestroy;
begin
  fReader.Free;
  if fOwnsStream then
    fStream.Free;
  fTempNodePath.Free;
  fTempReaderPath.Free;
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

function TXMLSeqParser.ReadNextChildElementHeader(
  var aNode: PXMLNode; var aElementIsOpen: Boolean): Boolean;
begin
  Result := ReadNextChildNodeCustom(True, aElementIsOpen);
  if Result then
    aNode := fXmlDoc.DocumentNode;
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
    aNode := fXmlDoc.DocumentNode;
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
    fReader.NodePathAssignTo(fTempNodePath);

    if fReaderNode.NodeType = etOpenElement then begin
      //last found was opening element, write it down!
      xLastNode := fXmlDoc.DOMDocument.AddChild(fReaderNode.NodeName)
    end else begin
      //last found was something else
      xLastNode := fXmlDoc.DOMDocument;

      //go to next child
      if aOnlyElementHeader then begin
        while fReader.ReadNextNode(fReaderNode) do begin
          case fReaderNode.NodeType of
            etOpenElement://new element found
            if fReader.RefIsChildOfNodePath(fTempNodePath)//fReaderNode is child of fTempNodePath
            then begin
              xLastNode := fXmlDoc.DOMDocument.AddChild(fReaderNode.NodeName);
              Break;
            end;
            etCloseElement://parent element may be closed
            if fReader.RefIsParentOfNodePath(fTempNodePath)//fReaderNode is parent of fTempNodePath -> exit!
            then begin
              Exit;
            end;
          end;
        end;

        if xLastNode = fXmlDoc.DOMDocument then begin//next child not found, exit
          Exit;
        end;
      end;

    end;

    while fReader.ReadNextNode(fReaderNode) do begin
      case fReaderNode.NodeType of
        etOpenXMLDeclaration: xLastNode := xLastNode.AddXMLDeclaration;
        etXMLDeclarationAttribute, etAttribute: xLastNode.Attributes[fReaderNode.NodeName] := fReaderNode.NodeValue;
        etXMLDeclarationFinishClose, etFinishOpenElementClose, etCloseElement: begin
          xLastNode := xLastNode.ParentNode;
          if not Assigned(xLastNode) then begin//the parent element was closed
            Exit;
          end;
        end;
        etOpenElement: begin
          fDataRead := True;
          xLastNode := xLastNode.AddChild(fReaderNode.NodeName);
        end;
        etText:
          if fDataRead or not OXmlIsWhiteSpace(fReaderNode.NodeValue) then//omit empty text before root node
            xLastNode.AddText(fReaderNode.NodeValue);
        etCData: xLastNode.AddCDATASection(fReaderNode.NodeValue);
        etComment: xLastNode.AddComment(fReaderNode.NodeValue);
        etDocType: xLastNode.AddDocType(fReaderNode.NodeValue);
        etProcessingInstruction: xLastNode.AddProcessingInstruction(fReaderNode.NodeName, fReaderNode.NodeValue);
      end;

      if not (fReaderNode.NodeType in [etAttribute, etOpenElement, etOpenXMLDeclaration])
        and (
          (fReader.NodePathMatch(fTempNodePath) or fReader.RefIsParentOfNodePath(fTempNodePath))//end of the element
          or
          (aOnlyElementHeader and fReader.RefIsChildOfNodePath(fTempNodePath))//next element was found, but we want to read only header
        )
      then begin
        if (fReaderNode.NodeType <> etFinishOpenElement) or
            aOnlyElementHeader
        then begin
          //next node was found, return true and exit!
          aElementIsOpen := (fReaderNode.NodeType = etFinishOpenElement);
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    fXmlDoc.Loading := False;
  end;
end;

end.
