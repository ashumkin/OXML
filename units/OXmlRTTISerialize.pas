unit OXmlRTTISerialize;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    commercial
    Please see the /license.txt file for more information.

}

{
  OXmlRTTISerialize.pas

  Automatic XML serializer/deserializer with enhanced RTTI
  (supported by Delphi 2010 and newer).

  Supported types:
    - Ordinal (Integer, enum, set, char, WideChar).
    - String (string, WideString).
    - Float (Date, Time, DateTime, Float).
    - Int64
    - Objects (TObject descendant).
    - Record
    - array[0..9] of T: constant arrays (one dimensional)
    - TArray<T>, array of T: dynamic arrays (one dimensional)
    - TList<T>, TList: generic and non-generic lists. The list MUST have
      an enumerator and add/clear methods defined.


  ! Properties in records are not supported due to a Delphi bug:
  ! http://qc.embarcadero.com/wc/qcmain.aspx?d=78110

  ! Properties in interfaces are not supported due to a Delphi bug:
  ! http://qc.embarcadero.com/wc/qcmain.aspx?d=90285

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
  System.SysUtils, System.Classes, System.TypInfo, System.RTTI,
  System.Generics.Collections,
  {$ELSE}
  SysUtils, Classes, TypInfo, RTTI, Generics.Collections,
  {$ENDIF}
  OWideSupp, OEncoding, OHashedStrings, ODictionary, OTextReadWrite,
  OXmlReadWrite, OXmlPDOM, OXmlSeq;

type
  TXMLRTTISerializer = class;
  
  TMemberVisibilitySet = set of TMemberVisibility;

  TXMLRTTISerializer = class(TObject)
  private
    fWriter: TXMLWriter;
    fRootElementWritten: Boolean;

    fRootNodeName: OWideString;
    fWriteDefaultValues: Boolean;
    fVisibility: TMemberVisibilitySet;

    fContext: TRttiContext;
  private
    function GetWriterSettings: TXMLWriterSettings;
    procedure SetRootNodeName(const Value: OWideString);

  protected
    procedure DoCreate; virtual;
    procedure DoInit; virtual;

    procedure WriteRootStartElement;
    procedure WriteRootEndElement;

    procedure WriteObjectEnumeration(const aObject: TObject; const aType: TRttiType);
    procedure WriteObjectProperty(
      const aTagName: OWideString; aType: TRttiType;
      const aValue: TValue;
      const aIsDefaultValue: Boolean);
    procedure _WriteObjectProperty(const aTagName, aValue: OWideString);
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLRTTISerializer or call ReleaseDocument!

    procedure InitFile(const aFileName: string);
    procedure InitStream(const aStream: TStream);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //Write an object to the XML file.
    procedure WriteObject<T>(const aObject: T);
  public
    //custom root node
    property RootNodeName: OWideString read fRootNodeName write SetRootNodeName;
    //write object properties with default values?
    property WriteDefaultValues: Boolean read fWriteDefaultValues write fWriteDefaultValues;
    //write properties only from a specific visibility
    property Visibility: TMemberVisibilitySet read fVisibility write fVisibility;

    //XML writer settings
    property WriterSettings: TXMLWriterSettings read GetWriterSettings;
  end;

  TXMLRTTIDeserializer = class(TObject)
  private
    fXMLParser: TXMLSeqParser;
    fRootNode, fCurrentElementNode: PXMLNode;

    fUseIndex: Boolean;
    fPropNameIndex: TDictionary<OHashedStringsIndex,PXMLNode>;

    fContext: TRttiContext;

    fCreateClasses: TDictionary<string,TClass>;

    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
    function GetReaderSettings: TXMLReaderSettings;
    function GetParseError: IOTextParseError;
  protected
    procedure DoInit; virtual;

    procedure BuildIndex(const aElementNode: PXMLNode);
    function CreateNewValue(const aType: TRttiType;
      const aTypeName: string): TValue;
    procedure ReadObjectEnumeration(const aObject: TObject;
      const aType: TRttiType; const aEnumerationNode: PXMLNode);
    procedure ReadObjectProperties(const aInstance: Pointer;
      const aType: TRttiType;
      const aElementNode: PXMLNode);
    procedure ReadObjectProperty(const aInstance: Pointer;
      const aMember: TRttiMember;
      const aType: TRttiType; const aValue: TValue;
      const aElementNode: PXMLNode);
    procedure ReadObjectPropertyValue(
      aType: TRttiType;
      const aElementValueNode: PXMLNode;
      var ioValue: TValue;
      var outValueDataChanged: Boolean); overload;
    function ReadObjectPropertyValue(
      const aType: TRttiType;
      const aElementValueNode: PXMLNode;
      var ioValue: TValue): Boolean; overload;
    procedure SetPropertyValue(const aInstance: Pointer;
      const aMember: TRttiMember; const aNewValue: TValue);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    //The Init* procedures open and initialize a XML document for parsing.
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
    //init document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil); overload;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil); overload;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;

    //Register class for creation
    procedure RegisterClass(const aClass: TClass);
  public
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //true: property names will be indexed for faster search (not necessary for most objects
    //  because the index creation overload is higher than speed gain for objects with little
    //  properties (typically less than 100).
    //false: no indexed search (default).
    property UseIndex: Boolean read fUseIndex write fUseIndex;
  public
    //following functions and properties can be called only during parsing (after Init* has been called).

    //Find next element
    function ReadObjectInfo(var outClassName: String): Boolean;
    //If an element was found with ReadElementInfo, read it into an instance
    procedure ReadObject<T>(const aObject: T);

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    //ParseError has information about the error that occured when parsing a document
    property ParseError: IOTextParseError read GetParseError;
  end;

  TRttiContextHelper = record helper for TRttiContext
  public
    //Get real object type -> especially for instances of classes
    function GetRealObjectType<T>(const aObject: T): TRttiType; overload;
    function GetRealObjectType(const aValue: TValue; const aDefType: TRttiType): TRttiType; overload;
  end;

  EXMLRTTISerializer = class(Exception);
  EXMLRTTIDeserializer = class(Exception);

type
  PObject = ^TObject;
  PInterface = ^IInterface;

implementation

uses
  OXmlLng, OXmlUtils;

type
  TValueHelper = record helper for TValue
  public
    function AsSet: Integer;
  end;

function TRttiPropertyHelper_IsDefaultValue(const aProperty: TRttiProperty; const aValue: TValue): Boolean; inline;
begin
  Result :=
    (aProperty is TRttiInstanceProperty) and
    (aProperty.PropertyType.IsOrdinal) and
    (TRttiInstanceProperty(aProperty).Default = aValue.AsOrdinal);
end;

{ TXMLRTTISerializer }

constructor TXMLRTTISerializer.Create;
begin
  inherited Create;

  DoCreate;
end;

constructor TXMLRTTISerializer.Create(const aStream: TStream);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream);
end;

destructor TXMLRTTISerializer.Destroy;
begin
  ReleaseDocument;
  fWriter.Free;
  fContext.Free;

  inherited;
end;

procedure TXMLRTTISerializer.DoCreate;
begin
  fWriter := TXMLWriter.Create;
  fContext := TRttiContext.Create;
  fVisibility := [mvPublic, mvPublished];

  fRootNodeName := 'oxmlserializer';
end;

procedure TXMLRTTISerializer.DoInit;
begin
  fRootElementWritten := False;
end;

function TXMLRTTISerializer.GetWriterSettings: TXMLWriterSettings;
begin
  Result := fWriter.WriterSettings;
end;

procedure TXMLRTTISerializer.InitFile(const aFileName: String);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLRTTISerializer.InitStream(const aStream: TStream);
begin
  fWriter.InitStream(aStream);

  DoInit;
end;

procedure TXMLRTTISerializer.ReleaseDocument;
begin
  if fRootElementWritten then
    WriteRootEndElement;

  fWriter.ReleaseDocument;
end;

procedure TXMLRTTISerializer.SetRootNodeName(const Value: OWideString);
begin
  if fRootElementWritten then
    raise EXMLRTTISerializer.Create(OXmlLng_CannotChangeRootNodeName)
  else
    fRootNodeName := Value;
end;

procedure TXMLRTTISerializer.WriteObject<T>(const aObject: T);
var
  xType: TRttiType;
  xValue: TValue;
begin
  if not fRootElementWritten then
    WriteRootStartElement;

  xType := fContext.GetRealObjectType<T>(aObject);

  WriteObjectProperty(xType.ToString, xType, TValue.From<T>(aObject), False);
end;

procedure TXMLRTTISerializer.WriteObjectEnumeration(const aObject: TObject;
  const aType: TRttiType);
var
  xGetEnumerator: TRttiMethod;
  xEnumObject: TObject;
  xEnumType, xItemType: TRttiType;
  xCurrent: TRttiProperty;
  xMoveNext: TRttiMethod;
  xValue: TValue;
begin
  xGetEnumerator := aType.GetMethod('GetEnumerator');
  if not Assigned(xGetEnumerator) or
     (xGetEnumerator.MethodKind <> mkFunction) or
     (xGetEnumerator.ReturnType.Handle.Kind <> tkClass)
  then
    Exit;

  xEnumObject := xGetEnumerator.Invoke(aObject, []).AsObject;
  if not Assigned(xEnumObject) then
    Exit;

  try
    xEnumType := fContext.GetType(xEnumObject.ClassInfo);

    xCurrent := xEnumType.GetProperty('Current');
    if not Assigned(xCurrent) or
       not (xCurrent.PropertyType.TypeKind in [tkString, tkUString, tkClass])
    then
      Exit;

    xMoveNext := xEnumType.GetMethod('MoveNext');
    if not Assigned(xMoveNext) or
       (Length(xMoveNext.GetParameters) <> 0) or
       (xMoveNext.MethodKind <> mkFunction) or
       (xMoveNext.ReturnType.Handle <> TypeInfo(Boolean))
    then
      Exit;

    xItemType := xCurrent.PropertyType;

    fWriter.OpenElement('_oxmldefenum', stFinish);
    while xMoveNext.Invoke(xEnumObject, []).AsBoolean do
    begin
      xValue := xCurrent.GetValue(xEnumObject);
      WriteObjectProperty(SymbolNameToString(@xValue.TypeInfo.Name),
        xItemType, xValue, False);
    end;
    fWriter.CloseElement('_oxmldefenum', True);

  finally
    xEnumObject.Free;
  end;
end;

procedure TXMLRTTISerializer.WriteObjectProperty(
  const aTagName: OWideString; aType: TRttiType;
  const aValue: TValue;
  const aIsDefaultValue: Boolean);
var
  xFloatValue: Extended;
  xProperty: TRttiProperty;
  xField: TRttiField;
  xValue: TValue;
  xInstance: Pointer;
  xElementType: TRttiType;
  I: Integer;
begin
  aType := fContext.GetRealObjectType(aValue, aType);

  case aType.TypeKind of
    tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
    begin
      if fWriteDefaultValues or not aIsDefaultValue then
      case aType.TypeKind of
        tkInteger: _WriteObjectProperty(aTagName, IntToStr(aValue.AsInteger));
        tkChar: _WriteObjectProperty(aTagName, OWideString(Char(aValue.AsOrdinal)));
        tkWChar: _WriteObjectProperty(aTagName, OWideString(WideChar(aValue.AsOrdinal)));
        tkEnumeration: _WriteObjectProperty(aTagName, GetEnumName(aType.Handle, aValue.AsOrdinal));
        tkSet: _WriteObjectProperty(aTagName, SetToString(aType.Handle, aValue.AsSet, False));
      end;
    end;
    tkString, tkLString, tkUString, tkWString:
      _WriteObjectProperty(aTagName, aValue.AsString);
    tkFloat:
    begin
      xFloatValue := aValue.AsExtended;
      if (aType = System.TypeInfo(TDateTime)) then
        _WriteObjectProperty(aTagName, ISODateTimeToStr(xFloatValue))
      else if (aType = System.TypeInfo(TTime)) then
        _WriteObjectProperty(aTagName, ISOTimeToStr(xFloatValue))
      else if (aType = System.TypeInfo(TDate)) then
        _WriteObjectProperty(aTagName, ISODateToStr(xFloatValue))
      else
        _WriteObjectProperty(aTagName, ISOFloatToStr(xFloatValue));
    end;
    tkInt64:
      _WriteObjectProperty(aTagName, IntToStr(aValue.AsInt64));
    tkClass, tkRecord, tkInterface:
    begin
      fWriter.OpenElement(aTagName, stFinish);

      case aType.TypeKind of
        tkClass: xInstance := aValue.AsObject;
        tkRecord: xInstance := aValue.GetReferenceToRawData;
        tkInterface: xInstance := Pointer(aValue.AsInterface);
      else
        xInstance := nil;
      end;

      for xField in aType.GetFields do
      if (xField.Visibility in fVisibility) then
        WriteObjectProperty(xField.Name, xField.FieldType, xField.GetValue(xInstance), False);

      for xProperty in aType.GetProperties do
      if (xProperty.Visibility in fVisibility) and
        (xProperty.IsWritable or xProperty.PropertyType.IsInstance)
      then
        WriteObjectProperty(xProperty.Name, xProperty.PropertyType, xProperty.GetValue(xInstance), False);

      if aType.TypeKind = tkClass then
        WriteObjectEnumeration(TObject(xInstance), aType);

      fWriter.CloseElement(aTagName, True);
    end;
    tkArray, tkDynArray:
    begin
      if aType is TRttiDynamicArrayType then
        xElementType := TRttiDynamicArrayType(aType).ElementType
      else
      if aType is TRttiArrayType then
        xElementType := TRttiArrayType(aType).ElementType
      else
        raise EXMLRTTISerializer.Create(OXmlLng_RTTIInternalError);

      fWriter.OpenElement(aTagName, stFinish);
      for I := 0 to aValue.GetArrayLength-1 do
      begin
        xValue := aValue.GetArrayElement(I);
        WriteObjectProperty(SymbolNameToString(@xValue.TypeInfo.Name), xElementType, xValue, False);
      end;
      fWriter.CloseElement(aTagName, True);
    end;
  end;
end;

procedure TXMLRTTISerializer.WriteRootEndElement;
begin
  fWriter.CloseElement(fRootNodeName);
end;

procedure TXMLRTTISerializer.WriteRootStartElement;
begin
  fWriter.OpenElement(fRootNodeName, stFinish);
  fRootElementWritten := True;
end;

procedure TXMLRTTISerializer._WriteObjectProperty(const aTagName,
  aValue: OWideString);
begin
  fWriter.OpenElement(aTagName, stFinish);
  fWriter.Text(aValue, False);
  fWriter.CloseElement(aTagName, False);
end;

{ TXMLRTTIDeserializer }

procedure TXMLRTTIDeserializer.BuildIndex(const aElementNode: PXMLNode);
var
  xPropNode: PXMLNode;
begin
  fPropNameIndex.Clear;

  xPropNode := nil;
  while aElementNode.GetNextChild(xPropNode) do
  begin
    fPropNameIndex.Add(xPropNode.NodeNameId, xPropNode);
  end;
end;

constructor TXMLRTTIDeserializer.Create;
begin
  inherited Create;

  fXMLParser := TXMLSeqParser.Create;
  fPropNameIndex := TDictionary<OHashedStringsIndex,PXMLNode>.Create;
  fCreateClasses := TDictionary<string,TClass>.Create;
end;

function TXMLRTTIDeserializer.CreateNewValue(const aType: TRttiType;
  const aTypeName: string): TValue;
var
  xCreateClass: TClass;
  xCreateClassType: TRttiType;
  xConstructorI, xConstructorFound: TRttiMethod;
  xConstructorParams: TArray<TRttiParameter>;
begin
  case aType.TypeKind of
    tkInteger: Result := TValue.From<Integer>(0);
    tkInt64: Result := TValue.From<Int64>(0);
    tkChar: Result := TValue.From<Char>(#0);
    tkWChar: Result := TValue.From<WideChar>(#0);
    tkFloat:
    begin
      if (aType = System.TypeInfo(TDateTime)) then
        Result := TValue.From<TDateTime>(0)
      else if (aType = System.TypeInfo(TTime)) then
        Result := TValue.From<TTime>(0)
      else if (aType = System.TypeInfo(TDate)) then
        Result := TValue.From<TDate>(0)
      else
        Result := TValue.From<Double>(0);
    end;
    tkString: Result := TValue.From<string>('');
    {$IFDEF O_RAWBYTESTRING}
    tkWString: Result := TValue.From<WideString>('');
    tkLString: Result := TValue.From<AnsiString>('');
    {$ENDIF}
    tkUString: Result := TValue.From<UnicodeString>('');
    tkClass:
    begin
      if aTypeName <> aType.Name then
      begin
        if not fCreateClasses.TryGetValue(aTypeName, xCreateClass) then
          raise EXMLRTTIDeserializer.CreateFmt(OXmlLng_DeserializerRegisterClass, [aTypeName]);
        xCreateClassType := fContext.GetType(xCreateClass);
      end else
      begin
        xCreateClassType := aType;
        xCreateClass := TClass(TRttiInstanceType(aType).MetaclassType);
      end;

      xConstructorFound := nil;
      for xConstructorI in xCreateClassType.GetMethods do
      if SameText(xConstructorI.Name, 'Create') and
         xConstructorI.IsConstructor
      then begin
        xConstructorParams := xConstructorI.GetParameters;
        if (
          (Length(xConstructorParams) = 0)
           or (
             (Length(xConstructorParams) = 1) and
             (xConstructorParams[0].ParamType.TypeKind = tkClass)
           ))
        then begin
          xConstructorFound := xConstructorI;
          Break;
        end;
      end;
      if not Assigned(xConstructorFound) or not (xCreateClassType is TRttiInstanceType) then
        raise EXMLRTTIDeserializer.CreateFmt(OXmlLng_DeserializerNotSupportedListItemType, [xCreateClassType.Name]);

      case Length(xConstructorParams) of
        0: Result := xConstructorFound.Invoke(xCreateClass, []);
        1: Result := xConstructorFound.Invoke(xCreateClass, [nil]);
      end;
    end;
  else
    //error reading
    raise EXMLRTTIDeserializer.CreateFmt(OXmlLng_DeserializerNotSupportedListItemType, [aType.Name]);
  end;
end;

destructor TXMLRTTIDeserializer.Destroy;
begin
  fPropNameIndex.Free;
  fXMLParser.Free;
  fContext.Free;
  fCreateClasses.Free;

  inherited;
end;

procedure TXMLRTTIDeserializer.DoInit;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
  fContext := TRttiContext.Create;
end;

function TXMLRTTIDeserializer.GetApproxStreamPosition: OStreamInt;
begin
  Result := fXMLParser.ApproxStreamPosition;
end;

function TXMLRTTIDeserializer.GetParseError: IOTextParseError;
begin
  Result := fXMLParser.ParseError;
end;

function TXMLRTTIDeserializer.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fXMLParser.ReaderSettings;
end;

function TXMLRTTIDeserializer.GetStreamSize: OStreamInt;
begin
  Result := fXMLParser.StreamSize;
end;

procedure TXMLRTTIDeserializer.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aBufferLength, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitFile(const aFileName: String;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitFile(aFileName, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitStream(aStream, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitXML(const aXML: OWideString);
begin
  fXMLParser.InitXML(aXML);
  DoInit;
end;

{$IFDEF O_RAWBYTESTRING}
procedure TXMLRTTIDeserializer.InitXML_UTF8(const aXML: ORawByteString);
begin
  fXMLParser.InitXML_UTF8(aXML);
  DoInit;
end;
{$ENDIF}

procedure TXMLRTTIDeserializer.ReadObject<T>(const aObject: T);
var
  xType: TRttiType;
  xInstance: Pointer;
begin
  if not Assigned(fCurrentElementNode) then
    raise EXMLRTTIDeserializer.Create(OXmlLng_WrongDeserializerSequence);

  xType := fContext.GetRealObjectType<T>(aObject);

  case xType.TypeKind of
    tkClass: xInstance := PObject(@aObject)^;
    tkInterface: xInstance := Pointer(PInterface(@aObject)^);
  else
    xInstance := @aObject;
  end;
  ReadObjectProperties(xInstance, xType, fCurrentElementNode);

  fCurrentElementNode := nil;
end;

procedure TXMLRTTIDeserializer.ReadObjectEnumeration(const aObject: TObject;
  const aType: TRttiType; const aEnumerationNode: PXMLNode);
var
  xClear, xAdd: TRttiMethod;
  xValue: TValue;
  xItemNode: PXMLNode;
  xItemType: TRttiType;
begin
  xClear := aType.GetMethod('Clear');
  if not Assigned(xClear) then
    Exit;

  xClear.Invoke(aObject, []);//clear the list

  xAdd := aType.GetMethod('Add');
  if not Assigned(xAdd) or
     (Length(xAdd.GetParameters) <> 1)
  then
    Exit;

  xItemType := xAdd.GetParameters[0].ParamType;

  xItemNode := aEnumerationNode.FirstChild;
  while Assigned(xItemNode) do
  begin
    xValue := CreateNewValue(xItemType, xItemNode.NodeName);//add whatever the result is!
    ReadObjectPropertyValue(xItemType, xItemNode, xValue);//add whatever the result is!
    xAdd.Invoke(aObject, [xValue]);

    xItemNode := xItemNode.NextSibling;
  end;
end;

function TXMLRTTIDeserializer.ReadObjectInfo(var outClassName: String): Boolean;
var
  xRootNodeOpen: Boolean;
begin
  if not Assigned(fRootNode) then
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
  outClassName := fCurrentElementNode.NodeName;
end;

procedure TXMLRTTIDeserializer.ReadObjectProperties(const aInstance: Pointer;
  const aType: TRttiType;
  const aElementNode: PXMLNode);
var
  xField: TRttiField;
  xProperty: TRttiProperty;
  xEnumerationNode: PXMLNode;
begin
  if fUseIndex then
    BuildIndex(aElementNode);

  for xField in aType.GetFields do
    ReadObjectProperty(aInstance, xField, xField.FieldType, xField.GetValue(aInstance), aElementNode);

  for xProperty in aType.GetProperties do
    ReadObjectProperty(aInstance, xProperty, xProperty.PropertyType, xProperty.GetValue(aInstance), aElementNode);

  if (aType.TypeKind = tkClass) and aElementNode.SelectNode('_oxmldefenum', xEnumerationNode) then
    ReadObjectEnumeration(TObject(aInstance), aType, xEnumerationNode);
end;

procedure TXMLRTTIDeserializer.ReadObjectProperty(const aInstance: Pointer;
  const aMember: TRttiMember;
  const aType: TRttiType; const aValue: TValue; const aElementNode: PXMLNode);
var
  xPropNameIndex: Integer;
  xPropElement: PXMLNode;
  xNewValue: TValue;
begin
  xPropNameIndex := aElementNode.OwnerDocument.IndexOfString(aMember.Name);
  if xPropNameIndex < 0 then
    Exit;

  if
    //if index used -> execute fPropNameIndex.TryGetValue (find node from index)
    not fUseIndex or
    (fPropNameIndex.Count = 0) or
    not fPropNameIndex.TryGetValue(xPropNameIndex, {%H-}xPropElement)
  then begin
    //otherwise find without index
    if not aElementNode.FindChildById(xPropNameIndex, {%H-}xPropElement) then
      Exit;
  end;

  xNewValue := aValue;
  if ReadObjectPropertyValue(aType, xPropElement, xNewValue) then
    SetPropertyValue(aInstance, aMember, xNewValue);
end;

function TXMLRTTIDeserializer.ReadObjectPropertyValue(const aType: TRttiType;
  const aElementValueNode: PXMLNode; var ioValue: TValue): Boolean;
begin
  ReadObjectPropertyValue(aType, aElementValueNode, ioValue, Result);
end;

procedure TXMLRTTIDeserializer.RegisterClass(const aClass: TClass);
begin
  fCreateClasses.Add(aClass.ClassName, aClass);
end;

procedure TXMLRTTIDeserializer.ReadObjectPropertyValue(
  aType: TRttiType;
  const aElementValueNode: PXMLNode;
  var ioValue: TValue;
  var outValueDataChanged: Boolean);
var
  xStrValue: OWideString;
  xOrdValue: Integer;
  xFloatValue: Extended;
  xNewValue: TValue;
  xItemType: TRttiType;
  xItemNode: PXMLNode;
  xArrayLength: ONativeInt;
  I: Integer;
begin
  aType := fContext.GetRealObjectType(ioValue, aType);

  outValueDataChanged := False;
  case aType.TypeKind of
    tkClass: ReadObjectProperties(ioValue.AsObject, aType, aElementValueNode);
    tkInterface: ReadObjectProperties(Pointer(ioValue.AsInterface), aType, aElementValueNode);
    tkRecord: begin
      ReadObjectProperties(ioValue.GetReferenceToRawData, aType, aElementValueNode);
      outValueDataChanged := True;
    end;
    tkArray, tkDynArray:
    begin
      if aType is TRttiDynamicArrayType then
      begin
        xItemType := TRttiDynamicArrayType(aType).ElementType;
        xArrayLength := aElementValueNode.ChildCount;
        DynArraySetLength(PPointer(ioValue.GetReferenceToRawData)^, ioValue.TypeInfo, 1, @xArrayLength);
      end else
      if aType is TRttiArrayType then
      begin
        xItemType := TRttiArrayType(aType).ElementType;
        xArrayLength := TRttiArrayType(aType).TotalElementCount;
      end else
        raise EXMLRTTISerializer.Create(OXmlLng_RTTIInternalError);

      I := 0;
      xItemNode := aElementValueNode.FirstChild;
      while Assigned(xItemNode) do
      begin
        if I >= xArrayLength then
          Break;

        if aType.TypeKind = tkArray then
          xNewValue := ioValue.GetArrayElement(I)
        else
          xNewValue := CreateNewValue(xItemType, xItemNode.NodeName);

        ReadObjectPropertyValue(xItemType, xItemNode, xNewValue);//add whatever the result is!
        ioValue.SetArrayElement(I, xNewValue);//add whatever the result is!

        Inc(I);
        xItemNode := xItemNode.NextSibling;
      end;
      outValueDataChanged := True;
    end;
  else
    xStrValue := aElementValueNode.Text;

    case aType.TypeKind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
      begin
        case aType.TypeKind of
          tkInteger: xOrdValue := StrToInt(xStrValue);
          tkChar, tkWChar: xOrdValue := Integer(xStrValue[1]);
          tkEnumeration: xOrdValue := GetEnumValue(aType.Handle, xStrValue);
          tkSet: xOrdValue := StringToSet(aType.Handle, xStrValue);
        else
          xOrdValue := 0;
        end;

        TValue.Make(xOrdValue, aType.Handle, ioValue);
        outValueDataChanged := True;
      end;
      tkString, tkLString, tkUString, tkWString:
      begin
        ioValue := TValue.From<string>(xStrValue);
        outValueDataChanged := True;
      end;
      tkFloat:
      begin
        if (aType = System.TypeInfo(TDateTime)) then
          xFloatValue := ISOStrToDateTime(xStrValue)
        else if (aType = System.TypeInfo(TTime)) then
          xFloatValue := ISOStrToTime(xStrValue)
        else if (aType = System.TypeInfo(TDate)) then
          xFloatValue := ISOStrToDate(xStrValue)
        else
          xFloatValue := ISOStrToFloat(xStrValue);
        ioValue := TValue.From(xFloatValue);
        outValueDataChanged := True;
      end;
    end;
  end;
end;

procedure TXMLRTTIDeserializer.ReleaseDocument;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

procedure TXMLRTTIDeserializer.SetPropertyValue(const aInstance: Pointer;
  const aMember: TRttiMember; const aNewValue: TValue);
begin
  if aMember is TRttiField then
    TRttiField(aMember).SetValue(aInstance, aNewValue)
  else
  if aMember is TRttiProperty then
    TRttiProperty(aMember).SetValue(aInstance, aNewValue)
end;

{ TValueHelper }

function TValueHelper.AsSet: Integer;
begin
  Result := Self.FData.FAsSLong;//get private field hook
end;

{ TRttiContextHelper }

function TRttiContextHelper.GetRealObjectType(const aValue: TValue;
  const aDefType: TRttiType): TRttiType;
var
  xObject: TObject;
begin
  Result := aDefType;
  if aValue.IsObject and (aDefType.TypeKind = tkClass) then
  begin
    xObject := aValue.AsObject;
    if Assigned(xObject) then
      Result := Self.GetType(xObject.ClassType)
  end;
end;

function TRttiContextHelper.GetRealObjectType<T>(
  const aObject: T): TRttiType;
var
  xObject: TObject;
begin
  Result := Self.GetType(TypeInfo(T));//TypeInfo
  if (Result.TypeKind = tkClass) then
  begin
    xObject := PObject(@aObject)^;
    if Assigned(xObject) then
      Result := Self.GetType(xObject.ClassType);
  end;
end;

end.
