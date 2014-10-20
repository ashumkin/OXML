unit OXmlSerialize;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    commercial
    Please see the /license.txt file for more information.

}

{
  OXmlSerialize.pas

  Automatic XML serializer/deserializer with basic properties supported
  by TypInfo (all Delphi versions and also FPC).

  Properties have to be published in order to be handled by SerDes!
  Objects must be descendants of TPersistent.

  Supported types:
    - Ordinal (Integer, enum, set, char, WideChar).
    - String (string, WideString).
    - Float (Date, Time, DateTime, Float).
    - Int64
    - Objects (TPersistent descendant).

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
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes, System.TypInfo,
    {$IFDEF O_GENERICS}
    System.Generics.Collections,
    {$ENDIF}
  {$ELSE}
  SysUtils, Classes, TypInfo,
    {$IFDEF O_GENERICS}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF O_GENERICS}
  ODictionary,
  {$ENDIF}
  OWideSupp, OEncoding, OHashedStrings, OTextReadWrite,
  OXmlReadWrite, OXmlPDOM, OXmlSeq;

type
  TXMLSerializer = class(TObject)
  private
    fWriter: TXMLWriter;
    fRootElementWritten: Boolean;

    fXMLDeclaration: TXMLWriterDeclaration;
    fUseRoot: Boolean;
    fRootNodeName: OWideString;
    fWriteDefaultValues: Boolean;
  private
    function GetWriterSettings: TXMLWriterSettings;
    procedure SetUseRoot(const aUseRoot: Boolean);
    procedure SetRootNodeName(const aRootNodeName: OWideString);
  protected
    procedure DoCreate;
    procedure DoInit;

    procedure WriteRootStartElement;
    procedure WriteRootEndElement;

    procedure WriteObjectProperties(const aObject: TPersistent;
      var ioElement: TXMLWriterElement);
    procedure WriteObjectProperty(const aObject: TPersistent; const aPropInfo: PPropInfo;
      var ioElement: TXMLWriterElement);
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLSerializer or call ReleaseDocument!

    procedure InitFile(const aFileName: string);
    procedure InitStream(const aStream: TStream);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //Write object to XML
    //  aElementName: use custom XML element name
    //  aWriteObjectType: if true, write the real object type to "type" XML attribute
    procedure WriteObject(const aObject: TPersistent); overload;
    procedure WriteObject(const aObject: TPersistent;
      const aElementName: OWideString; const aWriteObjectType: Boolean = True); overload;
  public
    //write XML declaration <?xml ?>
    property XMLDeclaration: TXMLWriterDeclaration read fXMLDeclaration;

    //use root
    //  - true: a document root (RootNodeName) will be written
    //  please note that if you disable it (UseRoot = false) and
    //  write more objects to the XML, the XML won't be valid because
    //  XML documents can have only one root
    property UseRoot: Boolean read fUseRoot write SetUseRoot;
    //custom root node
    property RootNodeName: OWideString read fRootNodeName write SetRootNodeName;
    //write object properties with default values?
    property WriteDefaultValues: Boolean read fWriteDefaultValues write fWriteDefaultValues;

    //XML writer settings
    property WriterSettings: TXMLWriterSettings read GetWriterSettings;
  end;

  TXMLDeserializer = class(TObject)
  private
    fXMLParser: TXMLSeqParser;
    fRootNode, fCurrentElementNode: PXMLNode;

    fUseRoot: Boolean;

    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
    function GetReaderSettings: TXMLReaderSettings;
    function GetParseError: IOTextParseError;
    procedure SetUseRoot(const aUseRoot: Boolean);
  protected
    procedure DoInit;

    procedure ReadObjectProperties(const aObject: TPersistent; const aElementNode: PXMLNode);
    procedure ReadObjectProperty(const aObject: TPersistent; const aPropInfo: PPropInfo;
      const aElementNode: PXMLNode; var ioPropNameIndex: TXMLNodeIndex);
  public
    constructor Create;
    destructor Destroy; override;
  public
    //The Init* procedures open and initialize a XML document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached or you call ReleaseDocument!

    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitFile(const aFileName: string; const aForceEncoding: TEncoding = nil);
    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitStream(const aStream: TStream; const aForceEncoding: TEncoding = nil);
    //init XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    procedure InitXML(const aXML: OWideString);
    {$IFDEF O_RAWBYTESTRING}
    procedure InitXML_UTF8(const aXML: ORawByteString);
    {$ENDIF}
    //init document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil); overload;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil); overload;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //use root - please use the same setting here as in TXMLSerialize
    property UseRoot: Boolean read fUseRoot write SetUseRoot;
  public
    //following functions and properties can be called only during parsing (after Init* has been called).

    //Find next element
    //  outElementName -> name of the XML element used (is the same with outType if TXMLSerializer.WriteObject was not executed with custom name)
    //  outType -> type of the object - differs from outElementName only if TXMLSerializer.WriteObject was executed with custom name
    function ReadObjectInfo(var outElementName: OWideString): Boolean; overload;
    function ReadObjectInfo(var outElementName, outType: OWideString): Boolean; overload;
    //If an element was found with ReadElementInfo, read it into an instance
    procedure ReadObject(const aObject: TPersistent);
    //Read object from any other (externally-defined) XML element node
    procedure ReadObjectFromNode(const aObject: TPersistent; const aElementNode: PXMLNode);

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    //ParseError has information about the error that occured when parsing a document
    property ParseError: IOTextParseError read GetParseError;
  end;

  EXMLSerializer = class(Exception);
  EXMLDeserializer = class(Exception);

implementation

uses
  OXmlLng, OXmlUtils
  {$IFDEF O_DELPHI_2007_DOWN}
  , Controls//definition of TTime and TDate
  {$ENDIF};

{ TXMLSerializer }

constructor TXMLSerializer.Create;
begin
  inherited Create;

  DoCreate;
end;

constructor TXMLSerializer.Create(const aStream: TStream);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream);
end;

destructor TXMLSerializer.Destroy;
begin
  ReleaseDocument;
  fWriter.Free;
  fXMLDeclaration.Free;

  inherited;
end;

procedure TXMLSerializer.DoCreate;
begin
  fWriter := TXMLWriter.Create;
  fXMLDeclaration := TXMLWriterDeclaration.Create;

  fRootNodeName := 'oxmlserializer';
  fUseRoot := True;
end;

procedure TXMLSerializer.DoInit;
begin
  fRootElementWritten := False;
end;

function TXMLSerializer.GetWriterSettings: TXMLWriterSettings;
begin
  Result := fWriter.WriterSettings;
end;

procedure TXMLSerializer.InitFile(const aFileName: string);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLSerializer.InitStream(const aStream: TStream);
begin
  fWriter.InitStream(aStream);

  DoInit;
end;

procedure TXMLSerializer.ReleaseDocument;
begin
  WriteRootEndElement;

  fWriter.ReleaseDocument;
end;

procedure TXMLSerializer.SetRootNodeName(const aRootNodeName: OWideString);
begin
  if fRootElementWritten then
    raise EXMLSerializer.Create(OXmlLng_CannotChangeRootNodeName)
  else
    fRootNodeName := aRootNodeName;
end;

procedure TXMLSerializer.SetUseRoot(const aUseRoot: Boolean);
begin
  if fRootElementWritten then
    raise EXMLSerializer.Create(OXmlLng_CannotChangeUseRootDataWritten)
  else
    fUseRoot := aUseRoot;
end;

procedure TXMLSerializer.WriteObject(const aObject: TPersistent);
begin
  WriteObject(aObject, aObject.ClassName, False);
end;

procedure TXMLSerializer.WriteObject(const aObject: TPersistent;
  const aElementName: OWideString; const aWriteObjectType: Boolean);
var
  xElement: TXMLWriterElement;
begin
  WriteRootStartElement;

  if GetTypeData(aObject.ClassInfo)^.PropCount = 0 then
    Exit;

  fWriter.OpenElementR(aElementName, {%H-}xElement);
  if aWriteObjectType and (aElementName <> aObject.ClassName) then
    xElement.Attribute('type', aObject.ClassName);
  WriteObjectProperties(aObject, xElement);
  xElement.CloseElement;
end;

procedure TXMLSerializer.WriteObjectProperties(const aObject: TPersistent;
  var ioElement: TXMLWriterElement);
var
  I: Integer;
  xPropCount: Integer;
  xPropList: PPropList;
  xPropInfo: PPropInfo;
begin
  xPropCount := GetTypeData(aObject.ClassInfo)^.PropCount;
  if xPropCount = 0 then
    Exit;

  GetMem(xPropList, xPropCount*SizeOf(Pointer));
  try
    GetPropInfos(aObject.ClassInfo, xPropList);
    for I := 0 to xPropCount-1 do
    begin
      xPropInfo := xPropList^[I];
      if Assigned(xPropInfo) and IsStoredProp(aObject, xPropInfo) then
        WriteObjectProperty(aObject, xPropInfo, ioElement);
    end;
  finally
    FreeMem(xPropList, xPropCount*SizeOf(Pointer));
  end;
end;

procedure TXMLSerializer.WriteObjectProperty(const aObject: TPersistent;
  const aPropInfo: PPropInfo; var ioElement: TXMLWriterElement);

  procedure _Write(const bValue: OWideString);
  var
    xPropElement: TXMLWriterElement;
  begin
    ioElement.OpenElementR(SymbolNameToString(@aPropInfo^.Name), {%H-}xPropElement);
    xPropElement.Text(bValue, False);
    xPropElement.CloseElement(False);
  end;

  procedure _WriteClass(const bClass: TObject);
  var
    xPropElement: TXMLWriterElement;
  begin
    if bClass is TPersistent then
    begin
      if GetTypeData(aObject.ClassInfo)^.PropCount = 0 then
        Exit;

      ioElement.OpenElementR(SymbolNameToString(@aPropInfo^.Name), {%H-}xPropElement);
      WriteObjectProperties(TPersistent(bClass), xPropElement);
      xPropElement.CloseElement;
    end;
  end;
var
  xPropType: PTypeInfo;
  xOrdValue: Integer;
  xFloatValue: Double;
begin
  if Assigned(aPropInfo^.GetProc) then
  begin
    xPropType := aPropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};
    case xPropType^.Kind of
      tkInteger, tkChar, tkWChar, {$IFDEF FPC}tkUChar,{$ENDIF} tkEnumeration, tkSet:
      begin
        xOrdValue := GetOrdProp(aObject, aPropInfo);
        if fWriteDefaultValues or (aPropInfo^.Default <> xOrdValue) then
        case xPropType^.Kind of
          tkInteger: _Write(IntToStr(xOrdValue));
          tkChar: _Write(OWideString(Char(xOrdValue)));
          tkWChar: _Write(OWideString(WideChar(xOrdValue)));
          {$IFDEF FPC}tkUChar: _Write(OWideString(UnicodeChar(xOrdValue)));{$ENDIF}
          tkEnumeration: _Write(GetEnumName(xPropType, xOrdValue));
          tkSet: _Write(GetSetProp(aObject, aPropInfo, False));
        end;
      end;
      tkString, tkLString
      {$IFDEF FPC}, tkAString{$ENDIF}
      {$IFDEF O_DELPHI_5_DOWN}, tkWString{$ENDIF}
      {$IFDEF O_DELPHI_2009_UP}, tkUString {$ENDIF}:
        _Write(GetStrProp(aObject, aPropInfo));
      {$IFDEF O_RAWBYTESTRING}{$IFNDEF O_DELPHI_5_DOWN}
      tkWString
      {$IFDEF FPC}, tkUString{$ENDIF}:
        _Write({$IFDEF FPC}UTF8Encode{$ENDIF}(GetWideStrProp(aObject, aPropInfo)));
      {$ENDIF}{$ENDIF}
      tkFloat:
      begin
        xFloatValue := GetFloatProp(aObject, aPropInfo);
        if (xPropType = System.TypeInfo(TDateTime)) then
          _Write(ISODateTimeToStr(xFloatValue))
        else if (xPropType = System.TypeInfo(TTime)) then
          _Write(ISOTimeToStr(xFloatValue))
        else if (xPropType = System.TypeInfo(TDate)) then
          _Write(ISODateToStr(xFloatValue))
        else
          _Write(ISOFloatToStr(xFloatValue));
      end;
      tkInt64:
        _Write(IntToStr(GetInt64Prop(aObject, aPropInfo)));
      tkClass:
        _WriteClass(GetObjectProp(aObject, aPropInfo));
    end;
  end;
end;

procedure TXMLSerializer.WriteRootEndElement;
begin
  if fUseRoot and fRootElementWritten then
    fWriter.CloseElement(fRootNodeName);
end;

procedure TXMLSerializer.WriteRootStartElement;
begin
  if not fRootElementWritten then
  begin
    fXMLDeclaration.WriteIfEnabled(fWriter);

    if fUseRoot then
      fWriter.OpenElement(fRootNodeName, stFinish);

    fRootElementWritten := True;
  end;
end;

{ TXMLDeserializer }

constructor TXMLDeserializer.Create;
begin
  inherited Create;

  fXMLParser := TXMLSeqParser.Create;
  fUseRoot := True;
end;

destructor TXMLDeserializer.Destroy;
begin
  fXMLParser.Free;

  inherited;
end;

procedure TXMLDeserializer.DoInit;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

function TXMLDeserializer.GetApproxStreamPosition: OStreamInt;
begin
  Result := fXMLParser.ApproxStreamPosition;
end;

function TXMLDeserializer.GetParseError: IOTextParseError;
begin
  Result := fXMLParser.ParseError;
end;

function TXMLDeserializer.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fXMLParser.ReaderSettings;
end;

function TXMLDeserializer.GetStreamSize: OStreamInt;
begin
  Result := fXMLParser.StreamSize;
end;

procedure TXMLDeserializer.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aBufferLength, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitFile(const aFileName: string;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitFile(aFileName, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitStream(aStream, aForceEncoding);
  DoInit;
end;

procedure TXMLDeserializer.InitXML(const aXML: OWideString);
begin
  fXMLParser.InitXML(aXML);
  DoInit;
end;

{$IFDEF O_RAWBYTESTRING}
procedure TXMLDeserializer.InitXML_UTF8(const aXML: ORawByteString);
begin
  fXMLParser.InitXML_UTF8(aXML);
  DoInit;
end;
{$ENDIF}

procedure TXMLDeserializer.ReadObject(const aObject: TPersistent);
begin
  if not Assigned(fCurrentElementNode) then
    raise EXMLDeserializer.Create(OXmlLng_WrongDeserializerSequence);

  ReadObjectFromNode(aObject, fCurrentElementNode);

  fCurrentElementNode := nil;
end;

procedure TXMLDeserializer.ReadObjectFromNode(const aObject: TPersistent;
  const aElementNode: PXMLNode);
begin
  ReadObjectProperties(aObject, aElementNode);
end;

function TXMLDeserializer.ReadObjectInfo(var outElementName,
  outType: OWideString): Boolean;
var
  xRootNodeOpen: Boolean;
begin
  if fUseRoot and not Assigned(fRootNode) then
  begin
    Result :=
      fXMLParser.ReadNextChildElementHeader({%H-}fRootNode, {%H-}xRootNodeOpen) and//no root element
      xRootNodeOpen;//there are no elements in root

    if not Result then
    begin
      ReleaseDocument;
      Exit;
    end;
  end;

  repeat
    Result := fXMLParser.ReadNextChildNode({%H-}fCurrentElementNode);
    if not Result then
    begin
      ReleaseDocument;
      Exit;
    end;

    Result := (fCurrentElementNode.NodeType = ntElement);
  until Result;

  //Result = true here
  outElementName := fCurrentElementNode.NodeName;
  outType := fCurrentElementNode.GetAttribute('type');
  if outType = '' then
    outType := outElementName;
end;

function TXMLDeserializer.ReadObjectInfo(var outElementName: OWideString): Boolean;
var
  xType: OWideString;
begin
  Result := ReadObjectInfo(outElementName, {%H-}xType);
end;

procedure TXMLDeserializer.ReadObjectProperties(const aObject: TPersistent;
  const aElementNode: PXMLNode);
var
  I: Integer;
  xPropCount: Integer;
  xPropList: PPropList;
  xPropInfo: PPropInfo;
  xPropNameIndex: TXMLNodeIndex;
begin
  xPropNameIndex := nil;
  try
    xPropCount := GetTypeData(aObject.ClassInfo)^.PropCount;
    if xPropCount > 0 then
    begin
      GetMem(xPropList, xPropCount*SizeOf(Pointer));
      try
        GetPropInfos(aObject.ClassInfo, xPropList);
        for I := 0 to xPropCount-1 do
        begin
          xPropInfo := xPropList^[I];
          if Assigned(xPropInfo) then
            ReadObjectProperty(aObject, xPropInfo, aElementNode, xPropNameIndex);
        end;
      finally
        FreeMem(xPropList, xPropCount*SizeOf(Pointer));
      end;
    end;
  finally
    xPropNameIndex.Free;
  end;
end;

procedure TXMLDeserializer.ReadObjectProperty(const aObject: TPersistent;
  const aPropInfo: PPropInfo; const aElementNode: PXMLNode;
  var ioPropNameIndex: TXMLNodeIndex);

  procedure _ReadClass(const bPropElement: PXMLNode);
  var
    xPropObject: TObject;
  begin
    xPropObject := GetObjectProp(aObject, aPropInfo);
    if Assigned(xPropObject) and (xPropObject is TPersistent) then
      ReadObjectProperties(TPersistent(xPropObject), bPropElement);
  end;
var
  xPropType: PTypeInfo;
  xPropElement: PXMLNode;
  xStrValue: OWideString;
  xOrdValue: Integer;
  xFloatValue: Double;
begin
  if not Assigned(aPropInfo^.GetProc) then
    Exit;

  if not aElementNode.FindChildWithIndex(SymbolNameToString(@aPropInfo^.Name),
    {%H-}xPropElement, ioPropNameIndex)
  then
    Exit;

  xPropType := aPropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};

  if xPropType^.Kind = tkClass then
  begin
    _ReadClass(xPropElement);
  end else
  begin
    xStrValue := xPropElement.Text;
    case xPropType^.Kind of
      tkInteger, tkChar, tkWChar, {$IFDEF FPC}tkUChar,{$ENDIF} tkEnumeration:
      begin
        case xPropType^.Kind of
          tkInteger: xOrdValue := StrToInt(xStrValue);
          tkChar {$IFNDEF FPC}, tkWChar{$ENDIF}: xOrdValue := Integer(xStrValue[1]);
          {$IFDEF FPC}tkWChar, tkUChar: xOrdValue := Integer(UTF8Decode(xStrValue)[1]);{$ENDIF}
          tkEnumeration: xOrdValue := GetEnumValue(xPropType, xStrValue);
        else
          xOrdValue := 0;
        end;
        SetOrdProp(aObject, aPropInfo, xOrdValue);
      end;
      tkSet:
        SetSetProp(aObject, aPropInfo, xStrValue);
      tkString, tkLString
      {$IFDEF FPC}, tkAString{$ENDIF}
      {$IFDEF O_DELPHI_5_DOWN}, tkWString{$ENDIF}
      {$IFDEF O_DELPHI_2009_UP}, tkUString{$ENDIF}:
        SetStrProp(aObject, aPropInfo, xStrValue);
      {$IFDEF O_RAWBYTESTRING}{$IFNDEF O_DELPHI_5_DOWN}
      tkWString
      {$IFDEF FPC}, tkUString{$ENDIF}:
        SetWideStrProp(aObject, aPropInfo, {$IFDEF FPC}UTF8Decode{$ENDIF}(xStrValue));
      {$ENDIF}{$ENDIF}
      tkFloat:
      begin
        if (xPropType = System.TypeInfo(TDateTime)) then
          xFloatValue := ISOStrToDateTime(xStrValue)
        else if (xPropType = System.TypeInfo(TTime)) then
          xFloatValue := ISOStrToTime(xStrValue)
        else if (xPropType = System.TypeInfo(TDate)) then
          xFloatValue := ISOStrToDate(xStrValue)
        else
          xFloatValue := ISOStrToFloat(xStrValue);
        SetFloatProp(aObject, aPropInfo, xFloatValue);
      end;
      tkInt64:
        SetInt64Prop(aObject, aPropInfo, StrToInt64(xStrValue));
      tkClass:
        _ReadClass(xPropElement);
    end;
  end;
end;

procedure TXMLDeserializer.ReleaseDocument;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

procedure TXMLDeserializer.SetUseRoot(const aUseRoot: Boolean);
begin
  if fXMLParser.ApproxStreamPosition > 0 then
    raise EXMLSerializer.Create(OXmlLng_CannotChangeUseRootDataRead)
  else
    fUseRoot := aUseRoot;
end;

end.
