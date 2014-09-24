unit OXmlUnitTests;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 25}
      {$ZEROBASEDSTRINGS OFF}
      {$LEGACYIFEND ON}
    {$IFEND}
    {$IF CompilerVersion >= 21}
      {$DEFINE USE_RTTI}
    {$IFEND}
    {$IF CompilerVersion < 20}
      {$DEFINE USE_CONTROLS}
    {$IFEND}
    {$DEFINE USE_DATEUTILS}
  {$ELSE}
    //Delphi 5
    {$DEFINE USE_CONTROLS}
  {$ENDIF}
{$ELSE}
  {$DEFINE USE_DATEUTILS}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFDEF USE_DATEUTILS}DateUtils,{$ENDIF}
  {$IFDEF USE_CONTROLS}Controls,{$ENDIF}

  OWideSupp, OXmlUtils, OEncoding,
  OTextReadWrite, OXmlReadWrite,
  OXmlPDOM, OXmlCDOM, OHashedStrings, OXmlSAX, OXmlSeq,
  OXmlSerialize
  {$IFDEF USE_RTTI}, OXmlRTTISerialize, Generics.Collections{$ENDIF}

  {$IFDEF NEXTGEN}, System.IOUtils{$ENDIF}
  ;

const
  cTestCount = 50;

type
  TObjFunc = function(): Boolean of object;

  TOXmlUnitTest = class(TObject)
  private
    fStrList: TStrings;
    fPassNameIfFalse: TStringList;
    fPassedCount: Integer;

    function GetAllTestCount: Integer;
    procedure ExecuteFunction(const aFunction: TObjFunc; const aFunctionName: String);
  private
    //OTextReadWrite.pas
    function Test_TOTextReader_InitBuffer: Boolean;
  private
    //OXmlReadWrite.pas
    function Test_TXMLReader_FinishOpenElementClose_NodeName_Empty: Boolean;
    function Test_TXMLReader_InvalidDocument1: Boolean;
  private
    //OXmlPDOM.pas
    function Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
    function Test_OXmlPDOM_TXMLNode_Clone: Boolean;
    function Test_OXmlPDOM_TXMLNode_Normalize: Boolean;
    function Test_OXmlPDOM_TXMLNode_GetElementsByTagNameNS_FindAttributeNS: Boolean;
    function Test_OXmlPDOM_TXMLDocument_InvalidDocument1: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
    function Test_OXmlPDOM_TXMLDocument_AttributeIndex: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WrongDocument1: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WrongDocument2: Boolean;
    function Test_OXmlPDOM_TXMLDocument_WrongDocument3: Boolean;
    function Test_OXmlPDOM_TXMLDocument_NameSpaces1: Boolean;
    function Test_OXmlPDOM_TXMLDocument_NameSpaces2: Boolean;
    function Test_OXmlPDOM_TXMLDocument_HeaderWithSpaces: Boolean;
    function Test_OXmlPDOM_DoctypeEntityTest1: Boolean;
    function Test_OXmlPDOM_EntityTest1: Boolean;
    function Test_OXmlPDOM_ExternalDTD: Boolean;
    function Test_OXmlPDOM_RussianANSI: Boolean;
    function Test_OXmlPDOM_ChildCount: Boolean;
    function Test_OXmlPDOM_Id: Boolean;
    function Test_OXmlPDOM_NextNodeInTree: Boolean;
    function Test_OXmlPDOM_OASIS: Boolean;
  private
    //OXmlCDOM.pas
    function Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
    function Test_OXmlCDOM_TXMLNode_Clone: Boolean;
    function Test_OXmlCDOM_TXMLNode_Normalize: Boolean;
    function Test_OXmlCDOM_TXMLDocument_InvalidDocument1: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
    function Test_OXmlCDOM_TXMLDocument_AttributeIndex: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WrongDocument1: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WrongDocument2: Boolean;
    function Test_OXmlCDOM_TXMLDocument_WrongDocument3: Boolean;
    function Test_OXmlCDOM_TXMLDocument_NameSpaces1: Boolean;
    function Test_OXmlCDOM_TXMLDocument_NameSpaces2: Boolean;
    function Test_OXmlCDOM_DoctypeEntityTest1: Boolean;
    function Test_OXmlCDOM_EntityTest1: Boolean;
    function Test_OXmlCDOM_ExternalDTD: Boolean;
    function Test_OXmlCDOM_ChildCount: Boolean;
    function Test_OXmlCDOM_NextNodeInTree: Boolean;
    function Test_OXmlCDOM_OASIS: Boolean;
  private
    //OWideSupp.pas
    function Test_TOTextBuffer: Boolean;
  private
    //OHashedStrings.pas
    function Test_TOHashedStrings_Grow: Boolean;
  private
    //OXmlSAX.pas
    procedure Test_TSAXParser_HashIndex_SAXStartElement({%H-}aSaxParser: TSAXParser;
      const {%H-}aName: OWideString; const aAttributes: TSAXAttributes);
    function Test_TSAXParser_HashIndex: Boolean;
  private
    //OXmlSeq.pas
    function Test_TXMLSeqParser_Test1: Boolean;
  private
    //OXmlXPath.pas
    function Test_OXmlXPath_Test1: Boolean;
  private
    //oasis tests
    function Test_OASIS(const aIsPDOM: Boolean): Boolean;
  private
    //OXmlSerialize.pas
    function Text_OXmlSerializer_Test1(const aUseIndex: Boolean): Boolean;
    function Text_OXmlSerializer_Test1True: Boolean;
    function Text_OXmlSerializer_Test1False: Boolean;

    //OXmlRTTISerialize.pas
    function Text_OXmlRTTISerializer_Test1(const {%H-}aUseIndex: Boolean): Boolean;
    function Text_OXmlRTTISerializer_Test1True: Boolean;
    function Text_OXmlRTTISerializer_Test1False: Boolean;
  public
    procedure OXmlTestAll(const aStrList: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  //custom classes for serializer tests
  TText_OXmlSerializer_Test1_Enum = (enOne, enTwo, enThree);
  TText_OXmlSerializer_Test1_Set = set of TText_OXmlSerializer_Test1_Enum;

  TText_OXmlSerializer_Test1_Class2 = class(TPersistent)
  private
    fMyInt: Integer;
  public
    constructor Create; overload;
    constructor Create(const aMyInt: Integer); overload;

    function SameAs(const aComp: TText_OXmlSerializer_Test1_Class2): Boolean; virtual;
  published
    property MyInt: Integer read fMyInt write fMyInt;
  end;
  TText_OXmlSerializer_Test1_Class2A = class(TText_OXmlSerializer_Test1_Class2)
  public
    MyIntVar: Integer;
  public
    constructor Create(const aMyInt, aMyIntVar: Integer); overload;

    function SameAs(const aComp: TText_OXmlSerializer_Test1_Class2): Boolean; override;
  end;

  MyWideString = {$IFNDEF NEXTGEN}WideString{$ELSE}string{$ENDIF};

  TText_OXmlSerializer_Test1_Class = class(TPersistent)
  private
    fMyInt: Integer;
    fMyEnum: TText_OXmlSerializer_Test1_Enum;
    fMySet: TText_OXmlSerializer_Test1_Set;
    fMyDate: TDate;
    fMyDateTime: TDateTime;
    fMyTime: TTime;
    fMyFloat: Double;
    fMyString: String;
    fMyWideString: MyWideString;
    fMyClass: TText_OXmlSerializer_Test1_Class2;
  public
    constructor Create;
    destructor Destroy; override;

    function SameAs(aCompare: TText_OXmlSerializer_Test1_Class): Boolean;
  published
    property MyInt: Integer read fMyInt write fMyInt;
    property MyEnum: TText_OXmlSerializer_Test1_Enum read fMyEnum write fMyEnum;
    property MySet: TText_OXmlSerializer_Test1_Set read fMySet write fMySet;
    property MyDate: TDate read fMyDate write fMyDate;
    property MyDateTime: TDateTime read fMyDateTime write fMyDateTime;
    property MyTime: TTime read fMyTime write fMyTime;
    property MyFloat: Double read fMyFloat write fMyFloat;
    property MyString: String read fMyString write fMyString;
    property MyWideString: MyWideString read fMyWideString write fMyWideString;
    property MyClass: TText_OXmlSerializer_Test1_Class2 read fMyClass;
  end;

  {$IFDEF USE_RTTI}
  TText_OXmlRTTISerializer_Test1_Record = record
    MyInt: Integer;
    MyString: string;
    MyArray: array[1..2] of String;
    MyDynArray: array of String;
    MyDynArrayDate: array of TDateTime;
  end;
  PText_OXmlRTTISerializer_Test1_Record = ^TText_OXmlRTTISerializer_Test1_Record;

  TText_OXmlRTTISerializer_Test1_Class = class(TPersistent)
  private
    fMyInt: Integer;
    fMyEnum: TText_OXmlSerializer_Test1_Enum;
    fMySet: TText_OXmlSerializer_Test1_Set;
    fMyDate: TDate;
    fMyDateTime: TDateTime;
    fMyTime: TTime;
    fMyFloat: Double;
    fMyString: string;
    fMyWideString: MyWideString;
    fMyClass: TText_OXmlSerializer_Test1_Class2;
    fMyRecord: TText_OXmlRTTISerializer_Test1_Record;
    fMyStrList: TList<string>;
    fMyObjList: TObjectList<TText_OXmlSerializer_Test1_Class2>;
  public
    constructor Create;
    destructor Destroy; override;

    function SameAs(aCompare: TText_OXmlRTTISerializer_Test1_Class): Boolean;
  public
    property MyInt: Integer read fMyInt write fMyInt default 0;
    property MyEnum: TText_OXmlSerializer_Test1_Enum read fMyEnum write fMyEnum default enOne;
    property MySet: TText_OXmlSerializer_Test1_Set read fMySet write fMySet;
    property MyDate: TDate read fMyDate write fMyDate;
    property MyDateTime: TDateTime read fMyDateTime write fMyDateTime;
    property MyTime: TTime read fMyTime write fMyTime;
    property MyFloat: Double read fMyFloat write fMyFloat;
    property MyString: string read fMyString write fMyString;
    property MyWideString: MyWideString read fMyWideString write fMyWideString;
    property MyClass: TText_OXmlSerializer_Test1_Class2 read fMyClass;
    property MyRecord: TText_OXmlRTTISerializer_Test1_Record read fMyRecord write fMyRecord;
    property MyStrList: TList<string> read fMyStrList;
    property MyObjList: TObjectList<TText_OXmlSerializer_Test1_Class2> read fMyObjList;
  end;
  {$ENDIF}

implementation

{$IFDEF VER130}
//Delphi 5 compatibility
type
  PByte = ^Byte;
{$ENDIF}

{ TOXmlUnitTest }

constructor TOXmlUnitTest.Create;
begin
  inherited Create;

  fPassNameIfFalse := TStringList.Create;
end;

destructor TOXmlUnitTest.Destroy;
begin
  fPassNameIfFalse.Free;

  inherited;
end;

procedure TOXmlUnitTest.ExecuteFunction(const aFunction: TObjFunc;
  const aFunctionName: String);
begin
  if not aFunction() then
    fPassNameIfFalse.Add(aFunctionName)
  else
    Inc(fPassedCount);
end;

function TOXmlUnitTest.GetAllTestCount: Integer;
begin
  Result := fPassNameIfFalse.Count + fPassedCount;
end;

procedure TOXmlUnitTest.OXmlTestAll(const aStrList: TStrings);
var
  I: Integer;
begin
  //because this tests are supposed to run in D7 and Lazarus too,
  //we cannot use RTTI to call all test functions automatically
  // -> call here all functions manually

  fStrList := aStrList;
  aStrList.Clear;

  ExecuteFunction(Test_OXmlPDOM_OASIS, 'Test_OASIS_PDOM');
  ExecuteFunction(Test_OXmlCDOM_OASIS, 'Test_OASIS_CDOM');
  ExecuteFunction(Test_TOTextReader_InitBuffer, 'Test_TOTextReader_InitBuffer');
  ExecuteFunction(Test_TXMLReader_FinishOpenElementClose_NodeName_Empty, 'Test_TXMLReader_FinishOpenElementClose_NodeName_Empty');
  ExecuteFunction(Test_TXMLReader_InvalidDocument1, 'Test_TXMLReader_InvalidDocument1');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute, 'Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_Clone, 'Test_OXmlPDOM_TXMLNode_Clone');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_Normalize, 'Test_OXmlPDOM_TXMLNode_Normalize');
  ExecuteFunction(Test_OXmlPDOM_TXMLNode_GetElementsByTagNameNS_FindAttributeNS, 'Test_OXmlPDOM_TXMLNode_GetElementsByTagNameNS_FindAttributeNS');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_InvalidDocument1, 'Test_OXmlPDOM_TXMLDocument_InvalidDocument1');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling, 'Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_AttributeIndex, 'Test_OXmlPDOM_TXMLDocument_AttributeIndex');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WrongDocument1, 'Test_OXmlPDOM_TXMLDocument_WrongDocument1');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WrongDocument2, 'Test_OXmlPDOM_TXMLDocument_WrongDocument2');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_WrongDocument3, 'Test_OXmlPDOM_TXMLDocument_WrongDocument3');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_NameSpaces1, 'Test_OXmlPDOM_TXMLDocument_NameSpaces1');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_NameSpaces2, 'Test_OXmlPDOM_TXMLDocument_NameSpaces2');
  ExecuteFunction(Test_OXmlPDOM_TXMLDocument_HeaderWithSpaces, 'Test_OXmlPDOM_TXMLDocument_HeaderWithSpaces');
  ExecuteFunction(Test_OXmlPDOM_DoctypeEntityTest1, 'Test_OXmlPDOM_DoctypeEntityTest1');
  ExecuteFunction(Test_OXmlPDOM_EntityTest1, 'Test_OXmlPDOM_EntityTest1');
  ExecuteFunction(Test_OXmlPDOM_ExternalDTD, 'Test_OXmlPDOM_ExternalDTD');
  ExecuteFunction(Test_OXmlPDOM_RussianANSI, 'Test_OXmlPDOM_RussianANSI');
  ExecuteFunction(Test_OXmlPDOM_ChildCount, 'Test_OXmlPDOM_ChildCount');
  ExecuteFunction(Test_OXmlPDOM_Id, 'Test_OXmlPDOM_Id');
  ExecuteFunction(Test_OXmlPDOM_NextNodeInTree, 'Test_OXmlPDOM_NextNodeInTree');
  ExecuteFunction(Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute, 'Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute');
  ExecuteFunction(Test_OXmlCDOM_TXMLNode_Clone, 'Test_OXmlCDOM_TXMLNode_Clone');
  ExecuteFunction(Test_OXmlCDOM_TXMLNode_Normalize, 'Test_OXmlCDOM_TXMLNode_Normalize');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_InvalidDocument1, 'Test_OXmlCDOM_TXMLDocument_InvalidDocument1');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling, 'Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_AttributeIndex, 'Test_OXmlCDOM_TXMLDocument_AttributeIndex');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WrongDocument1, 'Test_OXmlCDOM_TXMLDocument_WrongDocument1');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WrongDocument2, 'Test_OXmlCDOM_TXMLDocument_WrongDocument2');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_WrongDocument3, 'Test_OXmlCDOM_TXMLDocument_WrongDocument3');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_NameSpaces1, 'Test_OXmlCDOM_TXMLDocument_NameSpaces1');
  ExecuteFunction(Test_OXmlCDOM_TXMLDocument_NameSpaces2, 'Test_OXmlCDOM_TXMLDocument_NameSpaces2');
  ExecuteFunction(Test_OXmlCDOM_DoctypeEntityTest1, 'Test_OXmlCDOM_DoctypeEntityTest1');
  ExecuteFunction(Test_OXmlCDOM_EntityTest1, 'Test_OXmlCDOM_EntityTest1');
  ExecuteFunction(Test_OXmlCDOM_ExternalDTD, 'Test_OXmlCDOM_ExternalDTD');
  ExecuteFunction(Test_OXmlCDOM_ChildCount, 'Test_OXmlCDOM_ChildCount');
  ExecuteFunction(Test_OXmlCDOM_NextNodeInTree, 'Test_OXmlCDOM_NextNodeInTree');
  ExecuteFunction(Test_TOTextBuffer, 'Test_TOTextBuffer');
  ExecuteFunction(Test_TOHashedStrings_Grow, 'Test_TOHashedStrings_Grow');
  ExecuteFunction(Test_TSAXParser_HashIndex, 'Test_TSAXParser_HashIndex');
  ExecuteFunction(Test_TXMLSeqParser_Test1, 'Test_TXMLSeqParser_Test1');
  ExecuteFunction(Test_OXmlXPath_Test1, 'Test_OXmlXPath_Test1');
  ExecuteFunction(Text_OXmlSerializer_Test1True, 'Text_OXmlSerializer_Test1True');
  ExecuteFunction(Text_OXmlSerializer_Test1False, 'Text_OXmlSerializer_Test1False');
  ExecuteFunction(Text_OXmlRTTISerializer_Test1True, 'Text_OXmlRTTISerializer_Test1True');
  ExecuteFunction(Text_OXmlRTTISerializer_Test1False, 'Text_OXmlRTTISerializer_Test1False');

  if fPassNameIfFalse.Count = 0 then
    aStrList.Add(Format('OXml: all tests from %d passed.', [GetAllTestCount]))
  else
  begin
    aStrList.Add('');
    aStrList.Add(Format('ERROR OXml: %d from %d test(s) not passed:', [fPassNameIfFalse.Count, GetAllTestCount]));

    for I := 0 to fPassNameIfFalse.Count-1 do
      aStrList.Add(fPassNameIfFalse[I]);
  end;

  if (GetAllTestCount <> cTestCount) then
  begin
    aStrList.Add('');
    aStrList.Add('WARNING OXml: test count is invalid.');
    aStrList.Add(Format('tests runned: %d, tests expected: %d',
      [GetAllTestCount, cTestCount]));
  end;

end;

function TOXmlUnitTest.Test_OXmlXPath_Test1: Boolean;
const
  cXML: OWideString =
    //'  '+sLineBreak+'  '+
    '<?xml version="1.0" encoding="utf-8" ?>'+
    '<root description="test xml">'+
      '<boss name="Max Muster">'+
        '<person name="boss person"/>'+
        '<person name="boss person 2">'+
          '<person name="boss person/2.1"/>'+
          '<dog name="boss dog 2.2" type="fight" />'+
        '</person>'+
      '</boss>'+
      '<!-- comment -->'+
      '<person name="Paul Caster">this text is in person tag</person>'+
      '<![CDATA[some test info]]>'+
      '<?pi processing instruction ?>'+
    '</root>';

var
  xXml: OXmlPDOM.IXMLDocument;

  function _TestXPathElements(const aStartNode: OXmlPDOM.PXMLNode; const aXPath, aResult: OWideString): Boolean;
  var
    xList: OXmlPDOM.IXMLNodeList;
    xElement: OXmlPDOM.PXMLNode;
    xStr: OWideString;
    I: Integer;
  begin
    if aStartNode.SelectNodes(aXPath, {%H-}xList) then begin
      xStr := '';
      for I := 0 to xList.Count-1 do begin
        xElement := xList[I];

        if xStr <> '' then
        if xStr <> '' then
          xStr := xStr+sLineBreak;
        case xElement.NodeType of
          ntElement: xStr := xStr+xElement.NodeName+'='+xElement.Attributes['name'];
          ntAttribute: xStr := xStr+xElement.ParentNode.NodeName+':'+xElement.NodeName+'='+xElement.NodeValue;
          ntText, ntCData: xStr := xStr+xElement.NodeValue;
        end;
      end;

      Result := (xStr = aResult);
    end else begin
      Result := (aResult = '');//nothing selected
    end;
  end;
begin
  xXml := OXmlPDOM.CreateXMLDoc;
  xXml.LoadFromXML(cXML);

  Result := True;

  Result := Result and _TestXPathElements(xXml.DocumentElement, '.', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root|../root', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root/.', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root/boss/..', 'root=');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '../root/person', 'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '..//person[@name="boss person/2.1"]', 'person=boss person/2.1');
  Result := Result and _TestXPathElements(xXml.DocumentElement, '//person[@name="boss person/2.1"]', 'person=boss person/2.1');
  Result := Result and _TestXPathElements(xXml.Node, '//person[@name]', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, '//root//person/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
  Result := Result and _TestXPathElements(xXml.Node, '//person/../../boss', 'boss=Max Muster');
  Result := Result and _TestXPathElements(xXml.Node, '//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root//boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
  Result := Result and _TestXPathElements(xXml.Node, 'root//*', 'boss=Max Muster'+sLineBreak+'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root/*', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, '/root/boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
  Result := Result and _TestXPathElements(xXml.Node, 'root/boss', 'boss=Max Muster');
  Result := Result and _TestXPathElements(xXml.Node, 'root/person|root/boss', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root', 'root=');
  Result := Result and _TestXPathElements(xXml.Node, 'root/boss/person[2]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
  Result := Result and _TestXPathElements(xXml.Node, 'root/person[1]', 'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root/person[last()]', 'person=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, '/root/*[last()-1]/person[last()]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
  Result := Result and _TestXPathElements(xXml.Node, '//text()', 'this text is in person tag'+sLineBreak+'some test info');
  Result := Result and _TestXPathElements(xXml.Node, 'root/node()', 'root:description=test xml'+sLineBreak+'boss=Max Muster'+sLineBreak+'person=Paul Caster'+sLineBreak+'some test info');


  Result := Result and _TestXPathElements(xXml.Node, 'root//@*', 'root:description=test xml'+sLineBreak+'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'dog:type=fight'+sLineBreak+'person:name=Paul Caster');
  Result := Result and _TestXPathElements(xXml.Node, 'root//@name', 'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'person:name=Paul Caster');
end;

function TOXmlUnitTest.Test_TOHashedStrings_Grow: Boolean;
var
  xHS: TOHashedStrings;
  I: Integer;
begin
  xHS := TOHashedStrings.Create;
  try
    for I := 1 to 35 do
      xHS.Add(IntToStr(I));

    //36 is the limit when GrowBuckets is called and new hashes are generated
    xHS.Add('x');
    //x must be found in created list by a new hash!
    xHS.Add('x');

    Result := xHS.Count = 36;
  finally
    xHS.Free;
  end;
end;

function TOXmlUnitTest.Test_TOTextBuffer: Boolean;
var
  xC: OWideString;
  xBuf: TOTextBuffer;
  I, L: Integer;
begin
  xBuf := TOTextBuffer.Create;
  try
    for L := 1 to 2 do begin
      for I := 0 to 10*1000 - 1 do
        xBuf.WriteChar(OWideChar(IntToStr(I mod 10)[1]));

      Result := xBuf.UsedLength = (10*1000);
      if not Result then Exit;

      for I := 0 to xBuf.UsedLength-1 do begin
        xBuf.GetBuffer({%H-}xC, I+1, 1);
        Result := xC = IntToStr(I mod 10);
        if not Result then
          Exit;
      end;

      xBuf.Clear(True);
    end;
  finally
    xBuf.Free;
  end;
end;

function TOXmlUnitTest.Test_TOTextReader_InitBuffer: Boolean;
const
  inText: string = 'xmlTest';
var
  xReader: TOTextReader;
  xBuffer: TBytes;
  I: Integer;
  xC: OWideChar;
begin
  SetLength(xBuffer, Length(inText));
  for I := 1 to Length(inText) do
    xBuffer[I-1] := Byte(inText[I]);

  xReader := TOTextReader.Create;
  try
    xReader.InitBuffer(xBuffer);

    for I := 1 to Length(inText) do
    begin
      Result := xReader.ReadNextChar({%H-}xC) and (xC = OWideChar(inText[I]));
      if not Result then
        Exit;
    end;

    Result := not xReader.ReadNextChar(xC) and xReader.EOF;
  finally
    xReader.Free;
  end;
end;

function TOXmlUnitTest.Test_TSAXParser_HashIndex: Boolean;
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
  xSAXParser: TSAXParser;
  I: Integer;
  xAttr: OWideString;
begin
  xStream := nil;
  xWriter := nil;
  xSAXParser := nil;
  try
    xStream := TMemoryStream.Create;
    xWriter := TXMLWriter.Create(xStream);
    xWriter.OpenElement('root', stFinish);

    xWriter.OpenElement('ten');//under the hash index limit
    for I := 1 to 10 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('ten');

    xWriter.OpenElement('thousand');//above the hash index limit
    for I := 1 to 1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('thousand');

    xWriter.OpenElement('tenthousand');//above the hash index limit
    for I := 1 to 10*1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('tenthousand');

    xWriter.CloseElement('root');
    xWriter.Free;
    xWriter := nil;

    xStream.Position := 0;

    xSAXParser := TSAXParser.Create;
    xSAXParser.OnStartElement := Test_TSAXParser_HashIndex_SAXStartElement;
    xSAXParser.ParseStream(xStream);

  finally
    xWriter.Free;
    xStream.Free;
    xSAXParser.Free;
  end;

  Result := True;//always true -> check for assertions in Test_TSAXParser_HashIndex_SAXStartElement
end;

procedure TOXmlUnitTest.Test_TSAXParser_HashIndex_SAXStartElement(
  aSaxParser: TSAXParser; const aName: OWideString;
  const aAttributes: TSAXAttributes);
var
  I: Integer;
  xAttrName, xAttrValue: OWideString;
begin
  for I := 1 to aAttributes.Count do
  begin
    xAttrName := 'a'+IntToStr(I);
    aAttributes.Find(xAttrName, {%H-}xAttrValue);
    Assert(xAttrName = xAttrValue);
  end;
end;

function TOXmlUnitTest.Test_OXmlPDOM_ChildCount: Boolean;
const
  inXML: OWideString =
    '<?xml version="1.0" encoding="windows-1250"?>'+
    '<ROOT attribute="1">'+
    '  <CHILD1></CHILD1>'+
    '  <CHILD2></CHILD2>'+
    '  <CHILD3></CHILD3>'+
    '  <CHILD4></CHILD4>'+
    '</ROOT>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xRoot: OXmlPDOM.PXMLNode;
  {%H-}xDummy: String;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.LoadFromXML(inXML);
  xRoot := xXML.DocumentElement;
  xDummy := xRoot.AttributeNodes[0].NodeName;
  Result := xRoot.ChildCount = 4;
  if not Result then Exit;

  xXML.LoadFromXML(inXML);
  xRoot := xXML.DocumentElement;
  Result := xRoot.ChildCount = 4;
end;

function TOXmlUnitTest.Test_OXmlPDOM_DoctypeEntityTest1: Boolean;
const
  inXML: OWideString =
    '<!DOCTYPE elem'+sLineBreak+
    '['+sLineBreak+
    '<!ELEMENT elem (#PCDATA|elem)*>'+sLineBreak+
    '<!ENTITY ent "<elem>CharData</elem>">'+sLineBreak+
    '<!ENTITY ent2 "&ent; &gt;">'+sLineBreak+
    ']>'+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
  outXML: OWideString =
    '<!DOCTYPE elem'+sLineBreak+
    '['+sLineBreak+
    '<!ELEMENT elem (#PCDATA|elem)*>'+sLineBreak+
    '<!ENTITY ent "<elem>CharData</elem>">'+sLineBreak+
    '<!ENTITY ent2 "&ent; &gt;">'+sLineBreak+
    ']>'+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData '+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<?pi?>&lt;elem&gt;CharData&lt;/elem&gt;"'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xEntityValue: OWideString{$IFDEF FPC} = ''{$ENDIF};
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := xXML.XML = outXML;
  if not Result then Exit;

  Result := xXML.ReaderSettings.EntityList.Find('ent', xEntityValue) and (xEntityValue = '<elem>CharData</elem>');
  if not Result then Exit;

  Result := xXML.ReaderSettings.EntityList.Find('ent2', xEntityValue) and (xEntityValue = '<elem>CharData</elem> >');
  if not Result then Exit;

end;

function TOXmlUnitTest.Test_OXmlPDOM_EntityTest1: Boolean;
const
  inXML: Array [0..11] of OWideString = (
    ('<xml> & </xml>'),
    ('<xml> &a </xml>'),
    ('<xml> &a% </xml>'),
    ('<xml> &% </xml>'),
    ('<xml> &unknown; </xml>'),
    ('<xml> &#a </xml>'),
    ('<xml> &#xa </xml>'),
    ('<xml> &#32 </xml>'),
    ('<xml> &#x20 </xml>'),
    ('<xml> &#32323232323232323232; </xml>'),
    ('<xml> &#xFF2020202020202020; </xml>'),
    ('')
    );
  outXML: Array [0..11] of OWideString = (
    ('<xml> &amp; </xml>'),
    ('<xml> &amp;a </xml>'),
    ('<xml> &amp;a% </xml>'),
    ('<xml> &amp;% </xml>'),
    ('<xml> &amp;unknown; </xml>'),
    ('<xml> &amp;#a </xml>'),
    ('<xml> &amp;#xa </xml>'),
    ('<xml> &amp;#32 </xml>'),
    ('<xml> &amp;#x20 </xml>'),
    ('<xml> &amp;#32323232323232323232; </xml>'),
    ('<xml> &amp;#xFF2020202020202020; </xml>'),
    ('')
    );
var
  I: Integer;
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.ReaderSettings.StrictXML := False;
  xXML.ReaderSettings.ExpandEntities := False;

  for I := Low(inXML) to High(outXML) do
  begin
    xXML.LoadFromXML(inXML[I]);

    Result := (xXML.XML = outXML[I]);
    if not Result then
      Exit;
  end;
end;

function TOXmlUnitTest.Test_OXmlPDOM_ExternalDTD: Boolean;
const
  inDTD: OWideString =
    '<!ELEMENT elem (#PCDATA|elem)*>'+sLineBreak+
    '<!ENTITY ent "<elem>CharData</elem>">'+sLineBreak+
    '<!ENTITY ent2 "&ent; '+sLineBreak+' &gt;">'+sLineBreak+
    '';
  inXML: OWideString =
    '<elem>'+sLineBreak+
    '<?pi?>&ent2;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
  outXML: OWideString =
    '<elem>'+sLineBreak+
    '<?pi?>&lt;elem&gt;CharData&lt;/elem&gt; '+sLineBreak+' &gt;"'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xEntityValue: OWideString{$IFDEF FPC} = ''{$ENDIF};
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.ReaderSettings.LoadDTDFromString(inDTD);
  xXML.LoadFromXML(inXML);

  Result := xXML.XML = outXML;
  if not Result then Exit;

  Result := xXML.ReaderSettings.EntityList.Find('ent', xEntityValue) and (xEntityValue = '<elem>CharData</elem>');
  if not Result then Exit;

  Result := xXML.ReaderSettings.EntityList.Find('ent2', xEntityValue) and (xEntityValue = '<elem>CharData</elem> '+sLineBreak+' >');
  if not Result then Exit;
end;

function TOXmlUnitTest.Test_OXmlPDOM_Id: Boolean;
var
  I: Integer;
  xXML: OXmlPDOM.IXMLDocument;
  xRoot, xNode: PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc('root');
  xRoot := xXML.DocumentElement;

  for I := 2 to 10*1000 do
  begin
    xNode := xRoot.AddChild('c');
    Result := (xNode.Id = XMLNodeId(I)) and (xXML.GetNodeById(I) = xNode);

    if not Result then
      Exit;
  end;
end;

function TOXmlUnitTest.Test_OXmlPDOM_NextNodeInTree: Boolean;
var
  I: Integer;

  function NextId: String;
  begin
    Result := IntToStr(I);
    Inc(I);
  end;
var
  xXML: OXmlPDOM.IXMLDocument;
  xNode: PXMLNode;
begin
  I := 1;
  xXML := OXmlPDOM.CreateXMLDoc(NextId);
  xNode := xXML.DocumentElement;
  begin
    xNode.AddChild(NextId);
    xNode := xNode.AddChild(NextId);
    begin
      xNode.AddChild(NextId);
      xNode.AddChild(NextId);
      xNode := xNode.AddChild(NextId);
      begin
        xNode.AddChild(NextId);
        xNode.AddChild(NextId);

        xNode := xNode.ParentNode;
      end;
      xNode := xNode.ParentNode;
    end;
    xNode.AddChild(NextId);
    xNode := xNode.AddChild(NextId);
    begin
      xNode := xNode.AddChild(NextId);
      begin
        xNode.AddChild(NextId);
        xNode := xNode.ParentNode;
      end;
      xNode.AddChild(NextId);
      xNode := xNode.ParentNode;
    end;
    xNode.AddChild(NextId);
  end;

  Result := True;
  I := 1;
  xNode := xXML.DocumentElement;
  while Assigned(xNode) do
  begin
    Result := (xNode.NodeName = IntToStr(I));
    Inc(I);
    if not Result then
      Exit;

    xNode := xNode.NextNodeInTree;
  end;

  xNode := xXML.DocumentElement.LastChild;
  I := StrToInt(xNode.NodeName);
  while Assigned(xNode) and (I > 0) do
  begin
    Result := (xNode.NodeName = IntToStr(I));
    Dec(I);
    if not Result then
      Exit;

    xNode := xNode.PreviousNodeInTree;
  end;
end;

function TOXmlUnitTest.Test_OXmlPDOM_OASIS: Boolean;
begin
  Result := Test_OASIS(True);
end;

function TOXmlUnitTest.Test_OXmlPDOM_RussianANSI: Boolean;
  function _GetBytes(Chars: PChar; CharCount: Integer;
    Bytes: PByteArray; ByteCount: Integer): Integer;
  var
    I: Integer;
    C: {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF};
  begin
    Result := ByteCount;
    if Result > CharCount then
      Result := CharCount;

    if Result > 0 then
    begin
      C := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(Chars);
      for I := 0 to Result-1 do
      begin
        Bytes[I] := (C^ and $FF);
        Inc(C);
      end;
    end;
  end;
const
  inXML: String = //MUST BE ORealWideString because of ARC Delphi / FPC
    '<?xml version="1.0" encoding="windows-1251"?>'+
    '<channel>'+
    {$IFDEF FPC}
    '<display-name>'+#$CF#$E5#$F0#$E2#$FB#$E9+'</display-name>'+
    {$ELSE}
    '<display-name>'+#$00CF#$00E5#$00F0#$00E2#$00FB#$00E9+'</display-name>'+
    {$ENDIF}
    '</channel>';
  outXML: String = //MUST BE ORealWideString because of ARC Delphi / FPC
    '<?xml version="1.0" encoding="utf-8"?>'+
    '<channel>'+
    {$IFDEF FPC}
    '<display-name>'+#$D0#$9F#$D0#$B5#$D1#$80#$D0#$B2#$D1#$8B#$D0#$B9+'</display-name>'+//UTF-8
    {$ELSE}
    '<display-name>'+#$00D0#$009F#$00D0#$00B5#$00D1#$0080#$00D0#$00B2#$00D1#$008B#$00D0#$00B9+'</display-name>'+//UTF-8
    {$ENDIF}
    '</channel>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xInBuffer, xOutBuffer1, xOutBuffer2: TBytes;
begin
  SetLength(xInBuffer, Length(inXML));
  _GetBytes(PChar(inXML), Length(inXML), @xInBuffer[0], Length(xInBuffer));

  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.LoadFromBuffer(xInBuffer[0], Length(xInBuffer));
  xXML.Encoding := 'utf-8';
  xXML.WriterSettings.WriteBOM := False;
  xXML.SaveToBuffer({%H-}xOutBuffer1);

  SetLength(xOutBuffer2, Length(outXML));
  _GetBytes(PChar(outXML), Length(outXML), @xOutBuffer2[0], Length(xOutBuffer2));

  Result :=
    (Length(xOutBuffer1) = Length(xOutBuffer2)) and
    (Length(xOutBuffer1) > 0) and
    CompareMem(@xOutBuffer1[0], @xOutBuffer2[0], Length(xOutBuffer1));
end;

function TOXmlUnitTest.Test_TXMLReader_FinishOpenElementClose_NodeName_Empty: Boolean;
var
  xReader: TXMLReader;
  xReaderToken: PXMLReaderToken;
  xResult: OWideString;
begin
  xReader := TXMLReader.Create;
  try
    xReader.InitXML('<root attribute="1" />');

    xResult := '';
    while xReader.ReadNextToken({%H-}xReaderToken) do
    begin
      xResult := xResult + Format('%d:%s:%s;', [Ord(xReaderToken.TokenType), xReaderToken.TokenName, xReaderToken.TokenValue]);
    end;

    Result := xResult = '4:root:;5:attribute:1;7:root:;';
  finally
    xReader.Free;
  end;
end;

function TOXmlUnitTest.Test_TXMLReader_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
var
  xXMLReader: TXMLReader;
  xToken: PXMLReaderToken;

  procedure CheckNextToken(aTokenType: TXMLReaderTokenType; const aTokenName, aTokenValue: OWideString);
  begin
    Result := Result and xXMLReader.ReadNextToken(xToken) and
      ((xToken.TokenType = aTokenType) and (xToken.TokenName = aTokenName) and (xToken.TokenValue = aTokenValue));
  end;
begin
  Result := True;

  xXMLReader := TXMLReader.Create;
  try
    xXMLReader.ReaderSettings.StrictXML := False;

    xXMLReader.InitXML(inXML);

    CheckNextToken(rtOpenElement, 'root', '');
    CheckNextToken(rtFinishOpenElement, 'root', '');
    CheckNextToken(rtOpenElement, 'b', '');
    CheckNextToken(rtFinishOpenElement, 'b', '');
    CheckNextToken(rtText, '', 'TEXT');
    CheckNextToken(rtCloseElement, 'b', '');
    CheckNextToken(rtOpenElement, 'p', '');
    CheckNextToken(rtFinishOpenElement, 'p', '');
    CheckNextToken(rtOpenElement, 't', '');
    CheckNextToken(rtFinishOpenElement, 't', '');
    CheckNextToken(rtOpenElement, 'aaa', '');
    CheckNextToken(rtFinishOpenElementClose, 'aaa', '');
    CheckNextToken(rtCloseElement, 't', '');
    CheckNextToken(rtCloseElement, 'p', '');
    CheckNextToken(rtCloseElement, 'root', '');

  finally
    xXMLReader.Free;
  end;
end;

function TOXmlUnitTest.Test_TXMLSeqParser_Test1: Boolean;
const
  inXml: OWideString =
    '<?xml version="1.0" encoding="UTF-8"?>'+sLineBreak+
    '<teryt>'+sLineBreak+
    '  <catalog name="ULIC">'+sLineBreak+
    '    <row name="row1">'+sLineBreak+
    '      <col name="WOJ">04</col>'+sLineBreak+
    '      <col name="POW">10</col>'+sLineBreak+
    '    </row>'+sLineBreak+
    '    <row name="row2">'+sLineBreak+
    '      <col name="ABC">09</col>'+sLineBreak+
    '      <col name="CDE">11</col>'+sLineBreak+
    '    </row>'+sLineBreak+
    '    <row name="row3">'+sLineBreak+
    '      <col name="REW">00</col>'+sLineBreak+
    '      <col name="OLD">99</col>'+sLineBreak+
    '    </row>'+sLineBreak+
    '  </catalog>'+sLineBreak+
    '</teryt>'+sLineBreak;
  outStr: OWideString = 'WOJ:04;POW:10;ABC:09;CDE:11;REW:00;OLD:99;';
var
  xXMLSeq: TXMLSeqParser;
  xNode: OXmlPDOM.PXMLNode;
  xColNode: OXmlPDOM.PXMLNode;
  xName, xValue:String;
  xOpened: Boolean;
  xStr: OWideString;
begin
  Result := False;

  xXMLSeq := TXMLSeqParser.Create;
  try
    xXMLSeq.InitXML(inXml);
    xXMLSeq.WhiteSpaceHandling := wsTrim;

    if not(xXMLSeq.GoToPath('/teryt/catalog')) then
      Exit;

    if not((xXMLSeq.ReadNextChildElementHeader({%H-}xNode, {%H-}xOpened)) and xOpened) then
      Exit;

    xStr := '';
    while xXMLSeq.ReadNextChildNode(xNode) do
    begin
      if(xNode.NodeType = ntElement) and (xNode.NodeName = 'row') then
      begin
        xColNode := nil;
        while xNode.GetNextChild(xColNode) do
        begin
          xName := xColNode.GetAttribute('name');
          xValue := xColNode.Text;

          xStr := xStr + xName+':'+xValue+';'
        end;
      end;
    end;

    Result := (xStr = outStr);
  finally
    xXMLSeq.Free;
  end;
end;

function TOXmlUnitTest.Text_OXmlRTTISerializer_Test1(
  const aUseIndex: Boolean): Boolean;
{$IFDEF USE_RTTI}
var
  xStream: TStream;
  xSerializer: TXMLRTTISerializer;
  xDeserializer: TXMLRTTIDeserializer;
  I: Integer;
  xObjectIn, xObjectOut: array[0..1] of TText_OXmlRTTISerializer_Test1_Class;
  xClassName: String;
  xRec: TText_OXmlRTTISerializer_Test1_Record;
begin
  Result := False;

  xStream := TMemoryStream.Create;
  xSerializer := TXMLRTTISerializer.Create;
  xDeserializer := TXMLRTTIDeserializer.Create;
  for I := Low(xObjectIn) to High(xObjectIn) do
  begin
    xObjectIn[I] := TText_OXmlRTTISerializer_Test1_Class.Create;
    xObjectOut[I] := nil;
  end;
  try
    xSerializer.WriterSettings.IndentType := itIndent;
    xSerializer.WriterSettings.WriteBOM := False;
    xSerializer.InitStream(xStream);

    for I := Low(xObjectIn) to High(xObjectIn) do
    begin
      xObjectIn[I].MyInt := I;
      xObjectIn[I].MyEnum := enTwo;
      xObjectIn[I].MySet := [enOne, enThree];
      xObjectIn[I].MyDate := Trunc(Now) + I;//get date only
      xObjectIn[I].MyDateTime := RecodeMilliSecond(Now, 0);//clear milliseconds
      xObjectIn[I].MyTime := Frac(xObjectIn[I].MyDateTime);//get time only
      xObjectIn[I].MyFloat := 3.14;
      xObjectIn[I].MyString := 'Kluug.net';
      xObjectIn[I].MyWideString := 'Ond'#$0159'ej';//utf-16: Ondrej
      xObjectIn[I].MyClass.MyInt := I + 10;
      xRec.MyInt := I + 11;
      xRec.MyString := 'hello';
      SetLength(xRec.MyDynArray, 2);
      xRec.MyDynArray[0] := 'zero';
      xRec.MyDynArray[1] := 'one';
      xRec.MyArray[1] := 'one';
      xRec.MyArray[2] := 'two';
      SetLength(xRec.MyDynArrayDate, 2);
      xRec.MyDynArrayDate[0] := Now;
      xRec.MyDynArrayDate[1] := Now-1;
      xObjectIn[I].MyRecord := xRec;
      xObjectIn[I].MyStrList.Add('first');
      xObjectIn[I].MyStrList.Add('second');
      xObjectIn[I].MyObjList.Add(TText_OXmlSerializer_Test1_Class2A.Create(1, 5));
      xObjectIn[I].MyObjList.Add(TText_OXmlSerializer_Test1_Class2.Create(2));
      xObjectIn[I].MyObjList.Add(TText_OXmlSerializer_Test1_Class2.Create(3));

      xSerializer.WriteObject(xObjectIn[I]);
    end;

    xSerializer.ReleaseDocument;

    xStream.Position := 0;

    xDeserializer.InitStream(xStream);
    xDeserializer.UseIndex := aUseIndex;
    xDeserializer.RegisterClass(TText_OXmlSerializer_Test1_Class2);
    xDeserializer.RegisterClass(TText_OXmlSerializer_Test1_Class2A);

    I := 0;
    while xDeserializer.ReadObjectInfo({%H-}xClassName) do
    begin
      if xClassName = TText_OXmlRTTISerializer_Test1_Class.ClassName then
      begin
        xObjectOut[I] := TText_OXmlRTTISerializer_Test1_Class.Create;

        xDeserializer.ReadObject(xObjectOut[I]);

        Inc(I);
      end else
        raise Exception.Create('Text_OXmlRTTISerializer_Test1_CreateObject: class "'+xClassName+'" is unknown.');
    end;

    for I := Low(xObjectIn) to High(xObjectIn) do
    begin
      Result :=
        Assigned(xObjectOut[I]) and
        xObjectIn[I].SameAs(xObjectOut[I]);

      if not Result then
        Exit;
    end;
  finally
    xSerializer.Free;
    xDeserializer.Free;
    xStream.Free;
    for I := Low(xObjectIn) to High(xObjectIn) do
    begin
      xObjectIn[I].Free;
      xObjectOut[I].Free;
    end;
  end;
{$ELSE}
begin
  //FPC + Delphi 2009 -- no enhanced RTTI!
  Result := True;
{$ENDIF}
end;

function TOXmlUnitTest.Text_OXmlRTTISerializer_Test1False: Boolean;
begin
  Result := Text_OXmlRTTISerializer_Test1(False);
end;

function TOXmlUnitTest.Text_OXmlRTTISerializer_Test1True: Boolean;
begin
  Result := Text_OXmlRTTISerializer_Test1(True);
end;

function TOXmlUnitTest.Text_OXmlSerializer_Test1(
  const aUseIndex: Boolean): Boolean;
var
  xStream: TStream;
  xSerializer: TXMLSerializer;
  xDeserializer: TXMLDeserializer;
  I: Integer;
  xObjectIn, xObjectOut: array[0..1] of TText_OXmlSerializer_Test1_Class;
  xClassName: String;
begin
  Result := False;

  xStream := TMemoryStream.Create;
  xSerializer := TXMLSerializer.Create;
  xDeserializer := TXMLDeserializer.Create;
  for I := Low(xObjectIn) to High(xObjectIn) do
  begin
    xObjectIn[I] := TText_OXmlSerializer_Test1_Class.Create;
    xObjectOut[I] := nil;
  end;
  try
    xSerializer.WriterSettings.IndentType := itIndent;
    xSerializer.InitStream(xStream);

    for I := Low(xObjectIn) to High(xObjectIn) do
    begin
      xObjectIn[I].MyInt := I;
      xObjectIn[I].MyEnum := enTwo;
      xObjectIn[I].MySet := [enOne, enThree];
      xObjectIn[I].MyDate := Trunc(Now) + I;//get date only
      {$IFDEF USE_DATEUTILS}
      xObjectIn[I].MyDateTime := RecodeMilliSecond(Now, 0);//clear milliseconds
      {$ELSE}
      xObjectIn[I].MyDateTime := Now;
      {$ENDIF}
      xObjectIn[I].MyTime := Frac(xObjectIn[I].MyDateTime);//get time only
      xObjectIn[I].MyFloat := 3.14;
      xObjectIn[I].MyString := 'Kluug.net';
      xObjectIn[I].MyWideString := 'Ond'#$0159'ej';//utf-16: Ondrej
      xObjectIn[I].MyClass.MyInt := I + 10;

      xSerializer.WriteObject(xObjectIn[I]);
    end;

    xSerializer.ReleaseDocument;

    xStream.Position := 0;

    xDeserializer.InitStream(xStream);
    xDeserializer.UseIndex := aUseIndex;

    I := 0;
    while xDeserializer.ReadObjectInfo({%H-}xClassName) do
    begin
      if xClassName = TText_OXmlSerializer_Test1_Class.ClassName then
      begin
        xObjectOut[I] := TText_OXmlSerializer_Test1_Class.Create;

        xDeserializer.ReadObject(xObjectOut[I]);

        Inc(I);
      end else
        raise Exception.Create('Text_OXmlSerializer_Test1_CreateObject: class "'+xClassName+'" is unknown.');
    end;

    for I := Low(xObjectIn) to High(xObjectIn) do
    begin
      Result :=
        Assigned(xObjectOut[I]) and
        xObjectIn[I].SameAs(xObjectOut[I]);

      if not Result then
        Exit;
    end;
  finally
    xSerializer.Free;
    xDeserializer.Free;
    xStream.Free;
    for I := Low(xObjectIn) to High(xObjectIn) do
    begin
      xObjectIn[I].Free;
      xObjectOut[I].Free;
    end;
  end;
end;

function TOXmlUnitTest.Text_OXmlSerializer_Test1False: Boolean;
begin
  Result := Text_OXmlSerializer_Test1(False);
end;

function TOXmlUnitTest.Text_OXmlSerializer_Test1True: Boolean;
begin
  Result := Text_OXmlSerializer_Test1(True);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_AttributeIndex: Boolean;
  procedure _TestNode(const bNode: OXmlPDOM.PXMLNode);
  var
    I: Integer;
    xAttr: OWideString;
  begin
    for I := 1 to bNode.AttributeCount do
    begin
      xAttr := 'a'+IntToStr(I);
      Result := (bNode.GetAttribute(xAttr) = xAttr);
      if not Result then
        Exit;
    end;
  end;
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
  xXML: OXmlPDOM.IXMLDocument;
  I: Integer;
  xAttr: OWideString;
  xNode: OXmlPDOM.PXMLNode;
begin
  xStream := nil;
  xWriter := nil;
  try
    xStream := TMemoryStream.Create;
    xWriter := TXMLWriter.Create(xStream);
    xWriter.OpenElement('root', stFinish);

    xWriter.OpenElement('ten');//under the hash index limit
    for I := 1 to 10 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('ten');

    xWriter.OpenElement('thousand');//above the hash index limit
    for I := 1 to 1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('thousand');

    xWriter.OpenElement('tenthousand');//above the hash index limit
    for I := 1 to 10*1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('tenthousand');

    xWriter.CloseElement('root');
    xWriter.Free;
    xWriter := nil;

    xStream.Position := 0;

    xXML := OXmlPDOM.CreateXMLDoc;
    xXML.LoadFromStream(xStream);

    xNode := xXML.Node.SelectNode('root/ten');
    Result := xNode.AttributeCount = 10;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/thousand');
    Result := xNode.AttributeCount = 1000;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/tenthousand');
    Result := xNode.AttributeCount = 10*1000;
    if not Result then Exit;
    _TestNode(xNode);

  finally
    xWriter.Free;
    xStream.Free;
  end;

  Result := True;//always true -> check for assertions in Test_TSAXParser_HashIndex_SAXStartElement
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_HeaderWithSpaces: Boolean;
const
  inXML: OWideString = '<?xml version="1.0" encoding = "UTF-8"?><HeaderFooterSettings version = "8.0">test</HeaderFooterSettings>';
  outXML: OWideString = '<?xml version="1.0" encoding="UTF-8"?><HeaderFooterSettings version="8.0">test</HeaderFooterSettings>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
  outXML: OWideString = '<root><b>TEXT</b><p><t><aaa/></t></p></root>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.ReaderSettings.StrictXML := False;
  xXML.WriterSettings.StrictXML := False;

  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_NameSpaces1: Boolean;
const
  inXml: OWideString =
    '<h:table xmlns:h="http://www.w3.org/TR/html4/">'+
    '<h:tr>'+
    '<h:td desc:comment="simple test" xmlns:desc="ns-desc">Apples</h:td>'+
    '</h:tr>'+
    '</h:table>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = inXml);
  if not Result then Exit;

  Result := xXML.DocumentElement.NameSpaceURI = 'http://www.w3.org/TR/html4/';
  if not Result then Exit;

  Result := xXML.Node.SelectNode('//h:tr').NameSpaceURI = 'http://www.w3.org/TR/html4/';
  if not Result then Exit;

  Result := xXML.Node.SelectNode('//h:td/@desc:comment').NameSpaceURI = 'ns-desc';
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_NameSpaces2: Boolean;
const
  outXML = '<x:root xmlns:x="my-ns"><x:text f:begin="bgn" f:hello="txt" xmlns:f="my-ns"><e:hello xmlns:e="extra-ns"/></x:text></x:root>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xRoot, xNode1, xNode2, xAttr: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xRoot := xXML.Node.AppendChild(xXML.CreateElementNS('my-ns', 'x:root'));
  xNode1 := xXML.CreateElementNS('my-ns', 'x:text');
  xRoot.AppendChild(xNode1);
  Result := (xNode1.namespaceURI = 'my-ns');
  if not Result then Exit;

  xAttr := xXML.CreateAttribute('f:begin', 'bgn');
  xNode1.SetAttributeNode(xAttr);
  xAttr := xXML.CreateAttributeNS('my-ns', 'f:hello', 'txt');
  xNode1.SetAttributeNode(xAttr);
  Result := xAttr.namespaceURI = 'my-ns';
  if not Result then Exit;
  xNode2 := xNode1.AppendChild(xXML.CreateElementNS('extra-ns', 'e:hello'));
  Result := xNode2.namespaceURI = 'extra-ns';
  if not Result then Exit;

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
const
  inXML: OWideString =  '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default"> default <p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p>  </text>  </root>';
  outXML: OWideString = '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default">default<p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p></text>  </root>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.WhiteSpaceHandling := wsAutoTag;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WrongDocument1: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>'#0'</T1> {Chr(0)}'+sLineBreak+
    '</Test>'+sLineBreak;
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 7) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = #0);
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WrongDocument2: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>0</T1> {Chr(0)}';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 21) and
    (xXML.ParseError.ErrorCode = HIERARCHY_REQUEST_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = '0');
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLDocument_WrongDocument3: Boolean;
const
  inXML: OWideString =
    '<Test> /]]> </Test>';
var
  xXML: OXmlPDOM.IXMLDocument;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 1) and
    (xXML.ParseError.LinePos = 11) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test').Text = ' /]]> ');
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_Clone: Boolean;
const
  inXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone></root>';
  outXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone><clone attr="value"/><clone attr="value"><n>text</n><m/></clone></root>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xCloneNode: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc;
  xXML.LoadFromXML(inXML);
  xCloneNode := xXML.DocumentElement.SelectNode('clone');
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(False));
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(True));

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_GetElementsByTagNameNS_FindAttributeNS: Boolean;
const
  inXml: OWideString =
    '<h:table xmlns:h="http://www.w3.org/TR/html4/" xmlns:x="http://www.w3.org/TR/html4/">'+
    '<h:tr h:id="tr0" />'+
    '<h:tr x:id="tr1" />'+
    '<x:tr h:id="tr2" />'+
    '</h:table>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xNodeList: OXmlPDOM.IXMLNodeList;
  xAttrValue: OWideString;
  I: Integer;
begin
  xXML := OXmlPDOM.CreateXMLDoc;

  xXML.LoadFromXML(inXML);

  xXML.DocumentElement.GetElementsByTagNameNS('http://www.w3.org/TR/html4/', 'tr', {%H-}xNodeList);
  Result := xNodeList.Count = 3;
  if not Result then Exit;

  for I := 0 to xNodeList.Count-1 do
  begin
    Result := xNodeList[I].FindAttributeNS('http://www.w3.org/TR/html4/', 'id', {%H-}xAttrValue);
    if not Result then Exit;
    Result := xAttrValue = 'tr'+IntToStr(I);
    if not Result then Exit;
  end;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_Normalize: Boolean;
const
  outXML: OWideString = '<root><test/>my  text<b>hello<clone/></b></root>';
var
  xXML: OXmlPDOM.IXMLDocument;
  xDocElement, xNodeB: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc('root');
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xDocElement := xXML.DocumentElement;
  xDocElement.AddText(sLineBreak+'   '+sLineBreak+#9);
  xDocElement.AddChild('test');
  xDocElement.AddText(#9'my  text '+sLineBreak);
  xDocElement.AddText(sLineBreak);
  xNodeB := xDocElement.AddChild('b');
  xNodeB.AddText('  ');
  xNodeB.AddText('hello');
  xNodeB.AddText(sLineBreak);
  xNodeB.AddText('  ');
  xNodeB.AddChild('clone');
  xNodeB.AddText('  ');

  xDocElement.Normalize;

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlPDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
var
  xXML: OXmlPDOM.IXMLDocument;
  xAttribute: OXmlPDOM.PXMLNode;
begin
  xXML := OXmlPDOM.CreateXMLDoc('root', False);

  xAttribute := xXML.DocumentElement.SelectNodeCreate('@attr');
  xAttribute.NodeValue := 'value';

  Result := (xXML.XML = '<root attr="value"/>');
end;

function TOXmlUnitTest.Test_OASIS(const aIsPDOM: Boolean): Boolean;
var
  xOASISTestPassedCount: Integer;
  xOASISTestOmittedCount: Integer;

  function _GetDOMName: String;
  begin
    if aIsPDOM then
      Result := 'PDOM'
    else
      Result := 'CDOM';
  end;

  function _GetOutFileName(const bFileName: String): String;
  begin
    Result := ExtractFilePath(bFileName)+'outOXml'+PathDelim+ExtractFileName(bFileName);
  end;

  procedure _FileSaveToBuffer(const {%H-}bFileName: String;
    bXml: OXmlReadWrite.ICustomXMLDocument; var bBuffer: TEncodingBuffer; bEncoding: TEncoding;
    {%H-}bSaveForCompare: Boolean);
  var
    xStream: TMemoryStream;
    xWriter: TXMLWriter;
  begin
    xStream := TMemoryStream.Create;
    try
      xWriter := TXMLWriter.Create;
      try
        xWriter.InitStream(xStream);
        xWriter.WriterSettings.Assign(bXml.WriterSettings);
        xWriter.Encoding := bEncoding;

        bXml.SaveToWriter(xWriter);
      finally
        xWriter.Free;
      end;

      SetLength(bBuffer, xStream.Size);
      if xStream.Size > 0 then begin
        xStream.Seek(0, soFromBeginning);
        xStream.ReadBuffer(bBuffer[TEncodingBuffer_FirstElement], xStream.Size);
      end;

      // uncomment to write current (correct) output to check it back in the future
      {if bSaveForCompare then
      begin
        xStream.Seek(0, soFromBeginning);
        xStream.SaveToFile(_GetOutFileName(bFileName));
      end;{}
    finally
      xStream.Free;
    end;
  end;

  function _FileRunTest(const bFileName: String; const bCompareWithOriginal, bExpandRoot: Boolean): Boolean;
  var
    xXml: OXmlReadWrite.ICustomXMLDocument;
    xOriginalFileBuffer, xResavedFileBuffer: TEncodingBuffer;
    xOriginalEncoding: TEncoding;
    xFS: TFileStream;
  begin
    xFS := TFileStream.Create(bFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(xOriginalFileBuffer, xFS.Size);
      xFS.ReadBuffer(xOriginalFileBuffer[TEncodingBuffer_FirstElement], xFS.Size);
    finally
      xFS.Free;
    end;

    if Length(xOriginalFileBuffer) = 0 then
    begin
      Result := True;
      Exit;
    end;

    if aIsPDOM then
      xXml := OXmlPDOM.CreateXMLDoc
    else
      xXml := OXmlCDOM.CreateXMLDoc;

    xXml.ReaderSettings.ErrorHandling := ehRaiseAndEat;
    xXml.ReaderSettings.ExpandEntities := False;
    xXml.ReaderSettings.BreakReading := brNone;
    xXml.WhiteSpaceHandling := wsPreserveAll;
    xXml.WriterSettings.UseGreaterThanEntity := False;
    xXml.LoadFromBuffer(xOriginalFileBuffer[TEncodingBuffer_FirstElement], Length(xOriginalFileBuffer));

    Result := not Assigned(xXml.ParseError);
    if not Result then Exit;

    if TEncoding.GetEncodingFromBOM(xOriginalFileBuffer, {%H-}xOriginalEncoding, TEncoding.UTF8) > 0 then
      //bom found
      xXml.WriterSettings.WriteBOM := True
    else
      xXml.WriterSettings.WriteBOM := False;
    xXml.WriterSettings.LineBreak := lbCRLF;

    //compare files

    //the xmltest uses expanded root element "<doc></doc>" a lot, fake it
    if bExpandRoot then
    begin
      if aIsPDOM then
        OXmlPDOM.IXMLDocument(xXml).DocumentElement.AddText('x').NodeValue := ''
      else
        OXmlCDOM.IXMLDocument(xXml).DocumentElement.AddText('x').NodeValue := ''
    end;
    _FileSaveToBuffer({%H-}bFileName, xXml, {%H-}xResavedFileBuffer, xOriginalEncoding, not bCompareWithOriginal);

    if not bCompareWithOriginal then
    begin
      if not FileExists(_GetOutFileName(bFileName)) then
        Exit;

      xFS := TFileStream.Create(_GetOutFileName(bFileName), fmOpenRead or fmShareDenyNone);
      try
        SetLength(xOriginalFileBuffer, xFS.Size);
        xFS.ReadBuffer(xOriginalFileBuffer[TEncodingBuffer_FirstElement], xFS.Size);
      finally
        xFS.Free;
      end;
    end;

    Result := Length(xOriginalFileBuffer) = Length(xResavedFileBuffer);
    if not Result then Exit;

    Result := CompareMem(@xOriginalFileBuffer[TEncodingBuffer_FirstElement], @xResavedFileBuffer[TEncodingBuffer_FirstElement], Length(xResavedFileBuffer));
  end;

  function _DirRunTests(bDirID: Integer; bDirectory, bFilter: String; bExpandRoot: Boolean): Boolean;
  var
    xSearchRes: TSearchRec;
    xCompareFiles: Boolean;
  begin
    bDirectory := StringReplace(bDirectory, '\', PathDelim, [rfReplaceAll]);

    Result := True;

    if FindFirst(bDirectory+bFilter, faAnyFile, xSearchRes) = 0 then
    try
      repeat
        if//specific XML features not supported by OXml
          ((bDirID = 0) and (xSearchRes.Name = 'p66pass1.xml')) or//entity with invalid unicode data  <<< doable?
          ((bDirID = 0) and (xSearchRes.Name = 'p73pass1.xml')) or//externally-defined entity
          ((bDirID = 0) and (xSearchRes.Name = 'p74pass1.xml')) or//externally-defined entity
          ((bDirID = 0) and (xSearchRes.Name = 'p75pass1.xml')) or//externally-defined entity
          ((bDirID = 0) and (xSearchRes.Name = 'p76pass1.xml')) or//externally-defined entity

          ((bDirID = 1) and (xSearchRes.Name = '064.xml')) or//crazy unicode entity     <<< doable?
          ((bDirID = 1) and (xSearchRes.Name = '082.xml')) or//externally-defined entity
          ((bDirID = 1) and (xSearchRes.Name = '083.xml')) or//externally-defined entity
          ((bDirID = 1) and (xSearchRes.Name = '089.xml')) or//entity with invalid unicode data
          ((bDirID = 1) and (xSearchRes.Name = '091.xml')) or//externally-defined entity
          ((bDirID = 1) and (xSearchRes.Name = '097.xml')) or//externally-defined entity
          ((bDirID = 1) and (xSearchRes.Name = '100.xml')) or//externally-defined entity
          ((bDirID = 1) and (xSearchRes.Name = '114.xml')) or//CDATA in entity not supported
          ((bDirID = 1) and (xSearchRes.Name = '115.xml')) or//one entity defined before the other in DTD

          {$IFDEF FPC}
          //Lazarus cannot validate unicode element names because it reads the document in UTF-8 instead of UTF-16
          ((bDirID = 0) and (xSearchRes.Name = 'p04pass1.xml')) or
          ((bDirID = 1) and (xSearchRes.Name = '051.xml')) or
          ((bDirID = 1) and (xSearchRes.Name = '063.xml')) or
          {$ENDIF}
          False

        then
        begin
          Inc(xOASISTestOmittedCount);
          Continue;
        end;

        //There are some files that produce different output by OXml design.
        //The files below are tested on read but the output can't be compared to the original file,
        //which is absolutely OK.
        xCompareFiles := not(
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p10')) or//entities in attribute name is always expanded
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p24')) or//single quotes in attributes are converted to double quotes by OXml
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p25')) or//whitespace in attributes is removed by OXml
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p32')) or//whitespace in attributes is removed by OXml
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p40')) or//whitespace in attributes is removed by OXml
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p41')) or//whitespace in attributes is removed by OXml
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p42')) or//whitespace in attributes is removed by OXml
          ((bDirID = 0) and (Copy(xSearchRes.Name, 1, 3) = 'p44')) or//whitespace in attributes is removed by OXml

          ((bDirID = 1) and (xSearchRes.Name = '002.xml')) or//whitespace in attributes is removed by OXml
          ((bDirID = 1) and (xSearchRes.Name = '003.xml')) or//whitespace in attributes is removed by OXml
          ((bDirID = 1) and (xSearchRes.Name = '005.xml')) or//whitespace in attributes is removed by OXml
          ((bDirID = 1) and (xSearchRes.Name = '006.xml')) or//single quotes
          ((bDirID = 1) and (xSearchRes.Name = '010.xml')) or//whitespace in attributes is removed by OXml
          ((bDirID = 1) and (xSearchRes.Name = '025.xml')) or//"<foo></foo>" converted to "<foo/>"
          ((bDirID = 1) and (xSearchRes.Name = '026.xml')) or//"<foo></foo>" converted to "<foo/>"
          ((bDirID = 1) and (xSearchRes.Name = '027.xml')) or//"<foo></foo>" converted to "<foo/>"
          ((bDirID = 1) and (xSearchRes.Name = '029.xml')) or//single quotes
          ((bDirID = 1) and (xSearchRes.Name = '030.xml')) or//whitespace in attributes is removed by OXml
          ((bDirID = 1) and (xSearchRes.Name = '031.xml')) or//single quotes
          ((bDirID = 1) and (xSearchRes.Name = '032.xml')) or//single quotes
          ((bDirID = 1) and (xSearchRes.Name = '033.xml')) or//single quotes
          ((bDirID = 1) and (xSearchRes.Name = '034.xml')) or//<doc/>
          ((bDirID = 1) and (xSearchRes.Name = '035.xml')) or//<doc/>
          ((bDirID = 1) and (xSearchRes.Name = '040.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '041.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '054.xml')) or//whitespace in attributes is removed by OXml
          ((bDirID = 1) and (xSearchRes.Name = '066.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '093.xml')) or//#10 converted to #10#13
          ((bDirID = 1) and (xSearchRes.Name = '102.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '105.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '106.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '107.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '108.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '110.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '111.xml')) or//entities in attribute name are always expanded
          ((bDirID = 1) and (xSearchRes.Name = '112.xml')) or//<a></a> => <a/>
          False);

        Result := _FileRunTest(bDirectory+xSearchRes.Name, xCompareFiles, bExpandRoot);
        if not Result then
        begin
          fStrList.Add('OASIS '+_GetDOMName+' Hint: test not passed: '+xSearchRes.Name);
          fStrList.Add('');
          Exit;
        end else
        begin
          Inc(xOASISTestPassedCount);
        end;
      until FindNext(xSearchRes) <> 0;

    finally
      FindClose(xSearchRes);
    end;
  end;

var
  xDir: String;
begin
  {$IFDEF NEXTGEN}
  xDir := TPath.Combine(TPath.GetDocumentsPath, 'oasis'+PathDelim);
  {$ELSE}
  xDir := ExtractFilePath(ParamStr(0)) + '..'+PathDelim+'..'+PathDelim+'oasis'+PathDelim+'xmlconf'+PathDelim+'';
  {$ENDIF}

  {$IFNDEF VER130}
  if not DirectoryExists(xDir) then
  begin
    fStrList.Add('OASIS '+_GetDOMName+' Hint: test directory not found.');
    fStrList.Add('');
    fStrList.Add('If you want to run OASIS tests, download the OASIS test package from');
    fStrList.Add('https://www.oasis-open.org/committees/xml-conformance/suite-v1se/xmlconf-20010315.htm');
    fStrList.Add('and unzip it into OXml\oasis directory.');
    fStrList.Add('');
    fStrList.Add('');
    Result := True;
    Exit;
  end;
  {$ENDIF}

  xOASISTestPassedCount := 0;
  xOASISTestOmittedCount := 0;
  Result := _DirRunTests(0, xDir + 'oasis'+PathDelim+'', '*pass*.xml', False);
  if not Result then Exit;
  Result := _DirRunTests(1, xDir + 'xmltest'+PathDelim+'valid'+PathDelim+'sa'+PathDelim+'', '*.xml', True);
  if not Result then Exit;

  if Result and (xOASISTestPassedCount > 0) then
  begin
    fStrList.Add(
      Format('OASIS '+_GetDOMName+': all tests from %d passed (%d omitted on purpose).',
        [xOASISTestPassedCount, xOASISTestOmittedCount]));
    fStrList.Add('');
  end;
end;

function TOXmlUnitTest.Test_OXmlCDOM_ChildCount: Boolean;
const
  inXML: OWideString =
    '<?xml version="1.0" encoding="windows-1250"?>'+
    '<ROOT attribute="1">'+
    '  <CHILD1></CHILD1>'+
    '  <CHILD2></CHILD2>'+
    '  <CHILD3></CHILD3>'+
    '  <CHILD4></CHILD4>'+
    '</ROOT>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xRoot: OXmlCDOM.TXMLNode;
  {%H-}xDummy: String;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.LoadFromXML(inXML);
  xRoot := xXML.DocumentElement;
  xDummy := xRoot.AttributeNodes[0].NodeName;
  Result := xRoot.ChildCount = 4;
  if not Result then Exit;

  xXML.LoadFromXML(inXML);
  xRoot := xXML.DocumentElement;
  Result := xRoot.ChildCount = 4;
end;

function TOXmlUnitTest.Test_OXmlCDOM_DoctypeEntityTest1: Boolean;
const
  inXML: OWideString =
    '<!DOCTYPE elem'+sLineBreak+
    '['+sLineBreak+
    '<!ELEMENT elem (#PCDATA|elem)*>'+sLineBreak+
    '<!ENTITY ent "<elem>CharData</elem>">'+sLineBreak+
    ']>'+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
  outXML: OWideString =
    '<!DOCTYPE elem'+sLineBreak+
    '['+sLineBreak+
    '<!ELEMENT elem (#PCDATA|elem)*>'+sLineBreak+
    '<!ENTITY ent "<elem>CharData</elem>">'+sLineBreak+
    ']>'+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData '+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<![CDATA['+sLineBreak+
    '<elem>'+sLineBreak+
    'CharData&#32;'+sLineBreak+
    '<!--comment-->'+sLineBreak+
    '<?pi?>&ent;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>'+sLineBreak+
    ']]>'+sLineBreak+
    '<?pi?>&lt;elem&gt;CharData&lt;/elem&gt;"'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlCDOM_EntityTest1: Boolean;
const
  inXML: Array [0..11] of OWideString = (
    ('<xml> & </xml>'),
    ('<xml> &a </xml>'),
    ('<xml> &a% </xml>'),
    ('<xml> &% </xml>'),
    ('<xml> &unknown; </xml>'),
    ('<xml> &#a </xml>'),
    ('<xml> &#xa </xml>'),
    ('<xml> &#32 </xml>'),
    ('<xml> &#x20 </xml>'),
    ('<xml> &#32323232323232323232; </xml>'),
    ('<xml> &#xFF2020202020202020; </xml>'),
    ('')
    );
  outXML: Array [0..11] of OWideString = (
    ('<xml> &amp; </xml>'),
    ('<xml> &amp;a </xml>'),
    ('<xml> &amp;a% </xml>'),
    ('<xml> &amp;% </xml>'),
    ('<xml> &amp;unknown; </xml>'),
    ('<xml> &amp;#a </xml>'),
    ('<xml> &amp;#xa </xml>'),
    ('<xml> &amp;#32 </xml>'),
    ('<xml> &amp;#x20 </xml>'),
    ('<xml> &amp;#32323232323232323232; </xml>'),
    ('<xml> &amp;#xFF2020202020202020; </xml>'),
    ('')
    );
var
  I: Integer;
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.ReaderSettings.StrictXML := False;
  xXML.ReaderSettings.ExpandEntities := False;

  for I := Low(inXML) to High(outXML) do
  begin
    xXML.LoadFromXML(inXML[I]);

    Result := (xXML.XML = outXML[I]);
    if not Result then
      Exit;
  end;
end;

function TOXmlUnitTest.Test_OXmlCDOM_ExternalDTD: Boolean;
const
  inDTD: OWideString =
    '<!ELEMENT elem (#PCDATA|elem)*>'+sLineBreak+
    '<!ENTITY ent "<elem>CharData</elem>">'+sLineBreak+
    '<!ENTITY ent2 "&ent; '+sLineBreak+' &gt;">'+sLineBreak+
    '';
  inXML: OWideString =
    '<elem>'+sLineBreak+
    '<?pi?>&ent2;&quot;'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
  outXML: OWideString =
    '<elem>'+sLineBreak+
    '<?pi?>&lt;elem&gt;CharData&lt;/elem&gt; '+sLineBreak+' &gt;"'+sLineBreak+
    'CharData'+sLineBreak+
    '</elem>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xEntityValue: OWideString{$IFDEF FPC} = ''{$ENDIF};
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.ReaderSettings.LoadDTDFromString(inDTD);
  xXML.LoadFromXML(inXML);

  Result := xXML.XML = outXML;
  if not Result then Exit;

  Result := xXML.ReaderSettings.EntityList.Find('ent', xEntityValue) and (xEntityValue = '<elem>CharData</elem>');
  if not Result then Exit;

  Result := xXML.ReaderSettings.EntityList.Find('ent2', xEntityValue) and (xEntityValue = '<elem>CharData</elem> '+sLineBreak+' >');
  if not Result then Exit;
end;

function TOXmlUnitTest.Test_OXmlCDOM_NextNodeInTree: Boolean;
var
  I: Integer;

  function NextId: String;
  begin
    Result := IntToStr(I);
    Inc(I);
  end;
var
  xXML: OXmlCDOM.IXMLDocument;
  xNode: TXMLNode;
begin
  I := 1;
  xXML := OXmlCDOM.CreateXMLDoc(NextId);
  xNode := xXML.DocumentElement;
  begin
    xNode.AddChild(NextId);
    xNode := xNode.AddChild(NextId);
    begin
      xNode.AddChild(NextId);
      xNode.AddChild(NextId);
      xNode := xNode.AddChild(NextId);
      begin
        xNode.AddChild(NextId);
        xNode.AddChild(NextId);

        xNode := xNode.ParentNode;
      end;
      xNode := xNode.ParentNode;
    end;
    xNode.AddChild(NextId);
    xNode := xNode.AddChild(NextId);
    begin
      xNode := xNode.AddChild(NextId);
      begin
        xNode.AddChild(NextId);
        xNode := xNode.ParentNode;
      end;
      xNode.AddChild(NextId);
      xNode := xNode.ParentNode;
    end;
    xNode.AddChild(NextId);
  end;

  Result := True;
  I := 1;
  xNode := xXML.DocumentElement;
  while Assigned(xNode) do
  begin
    Result := (xNode.NodeName = IntToStr(I));
    Inc(I);
    if not Result then
      Exit;

    xNode := xNode.NextNodeInTree;
  end;

  xNode := xXML.DocumentElement.LastChild;
  I := StrToInt(xNode.NodeName);
  while Assigned(xNode) and (I > 0) do
  begin
    Result := (xNode.NodeName = IntToStr(I));
    Dec(I);
    if not Result then
      Exit;

    xNode := xNode.PreviousNodeInTree;
  end;
end;

function TOXmlUnitTest.Test_OXmlCDOM_OASIS: Boolean;
begin
  Result := Test_OASIS(False);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_AttributeIndex: Boolean;
  procedure _TestNode(const bNode: OXmlCDOM.TXMLNode);
  var
    I: Integer;
    xAttr: OWideString;
  begin
    for I := 1 to bNode.AttributeCount do
    begin
      xAttr := 'a'+IntToStr(I);
      Result := (bNode.GetAttribute(xAttr) = xAttr);
      if not Result then
        Exit;
    end;
  end;
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
  xXML: OXmlCDOM.IXMLDocument;
  I: Integer;
  xAttr: OWideString;
  xNode: OXmlCDOM.TXMLNode;
begin
  xStream := nil;
  xWriter := nil;
  try
    xStream := TMemoryStream.Create;
    xWriter := TXMLWriter.Create(xStream);
    xWriter.OpenElement('root', stFinish);

    xWriter.OpenElement('ten');//under the hash index limit
    for I := 1 to 10 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('ten');

    xWriter.OpenElement('thousand');//above the hash index limit
    for I := 1 to 1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('thousand');

    xWriter.OpenElement('tenthousand');//above the hash index limit
    for I := 1 to 10*1000 do
    begin
      xAttr := 'a'+IntToStr(I);
      xWriter.Attribute(xAttr, xAttr);
    end;
    xWriter.FinishOpenElementClose('tenthousand');

    xWriter.CloseElement('root');
    xWriter.Free;
    xWriter := nil;

    xStream.Position := 0;

    xXML := OXmlCDOM.CreateXMLDoc;
    xXML.LoadFromStream(xStream);

    xNode := xXML.Node.SelectNode('root/ten');
    Result := xNode.AttributeCount = 10;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/thousand');
    Result := xNode.AttributeCount = 1000;
    if not Result then Exit;
    _TestNode(xNode);

    xNode := xXML.Node.SelectNode('root/tenthousand');
    Result := xNode.AttributeCount = 10*1000;
    if not Result then Exit;
    _TestNode(xNode);

  finally
    xWriter.Free;
    xStream.Free;
  end;

  Result := True;//always true -> check for assertions in Test_TSAXParser_HashIndex_SAXStartElement
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
  outXML: OWideString = '<root><b>TEXT</b><p><t><aaa/></t></p></root>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.ReaderSettings.StrictXML := False;
  xXML.WriterSettings.StrictXML := False;

  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_NameSpaces1: Boolean;
const
  inXml: OWideString =
    '<h:table xmlns:h="http://www.w3.org/TR/html4/">'+
    '<h:tr>'+
    '<h:td desc:comment="simple test" xmlns:desc="ns-desc">Apples</h:td>'+
    '</h:tr>'+
    '</h:table>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = inXml);
  if not Result then Exit;

  Result := xXML.DocumentElement.NameSpaceURI = 'http://www.w3.org/TR/html4/';
  if not Result then Exit;

  Result := xXML.Node.SelectNode('//h:tr').NameSpaceURI = 'http://www.w3.org/TR/html4/';
  if not Result then Exit;

  Result := xXML.Node.SelectNode('//h:td/@desc:comment').NameSpaceURI = 'ns-desc';
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_NameSpaces2: Boolean;
const
  outXML = '<x:root xmlns:x="my-ns"><x:text f:begin="bgn" f:hello="txt" xmlns:f="my-ns"><e:hello xmlns:e="extra-ns"/></x:text></x:root>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xRoot, xNode1, xNode2, xAttr: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xRoot := xXML.Node.AppendChild(xXML.CreateElementNS('my-ns', 'x:root'));
  xNode1 := xXML.CreateElementNS('my-ns', 'x:text');
  xRoot.AppendChild(xNode1);
  Result := (xNode1.namespaceURI = 'my-ns');
  if not Result then Exit;

  xAttr := xXML.CreateAttribute('f:begin', 'bgn');
  xNode1.SetAttributeNode(xAttr);
  xAttr := xXML.CreateAttributeNS('my-ns', 'f:hello', 'txt');
  xNode1.SetAttributeNode(xAttr);
  Result := xAttr.namespaceURI = 'my-ns';
  if not Result then Exit;
  xNode2 := xNode1.AppendChild(xXML.CreateElementNS('extra-ns', 'e:hello'));
  Result := xNode2.namespaceURI = 'extra-ns';
  if not Result then Exit;

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WhiteSpaceHandling: Boolean;
const
  inXML: OWideString =  '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default"> default <p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p>  </text>  </root>';
  outXML: OWideString = '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default">default<p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p></text>  </root>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.WhiteSpaceHandling := wsAutoTag;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WrongDocument1: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>'#0'</T1> {Chr(0)}'+sLineBreak+
    '</Test>'+sLineBreak;
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 7) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = #0);
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WrongDocument2: Boolean;
const
  inXML: OWideString =
    '<Test>'+sLineBreak+
    '  <T1>0</T1> {Chr(0)}';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 2) and
    (xXML.ParseError.LinePos = 21) and
    (xXML.ParseError.ErrorCode = HIERARCHY_REQUEST_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test/T1').Text = '0');
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLDocument_WrongDocument3: Boolean;
const
  inXML: OWideString =
    '<Test> /]]> </Test>';
var
  xXML: OXmlCDOM.IXMLDocument;
begin
  xXML := OXmlCDOM.CreateXMLDoc;

  xXML.ReaderSettings.ErrorHandling := ehSilent;

  Result :=
    not xXML.LoadFromXML(inXML);
  Result := Result and
    (xXML.ParseError.Line = 1) and
    (xXML.ParseError.LinePos = 11) and
    (xXML.ParseError.ErrorCode = INVALID_CHARACTER_ERR);

  if not Result then
    Exit;

  //now check XML read in not strict mode
  xXML.ReaderSettings.StrictXML := False;
  xXML.WhiteSpaceHandling := wsPreserveAll;
  Result :=
    xXML.LoadFromXML(inXML);

  Result := Result and
    (xXML.Node.SelectNode('/Test').Text = ' /]]> ');
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLNode_Clone: Boolean;
const
  inXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone></root>';
  outXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone><clone attr="value"/><clone attr="value"><n>text</n><m/></clone></root>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xCloneNode: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc;
  xXML.LoadFromXML(inXML);
  xCloneNode := xXML.DocumentElement.SelectNode('clone');
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(False));
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(True));

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLNode_Normalize: Boolean;
const
  outXML: OWideString = '<root><test/>my  text<b>hello<clone/></b></root>';
var
  xXML: OXmlCDOM.IXMLDocument;
  xDocElement, xNodeB: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc('root');
  xXML.WhiteSpaceHandling := wsPreserveAll;
  xDocElement := xXML.DocumentElement;
  xDocElement.AddText(sLineBreak+'   '+sLineBreak+#9);
  xDocElement.AddChild('test');
  xDocElement.AddText(#9'my  text '+sLineBreak);
  xDocElement.AddText(sLineBreak);
  xNodeB := xDocElement.AddChild('b');
  xNodeB.AddText('  ');
  xNodeB.AddText('hello');
  xNodeB.AddText(sLineBreak);
  xNodeB.AddText('  ');
  xNodeB.AddChild('clone');
  xNodeB.AddText('  ');

  xDocElement.Normalize;

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_OXmlCDOM_TXMLNode_SelectNodeCreate_Attribute: Boolean;
var
  xXML: OXmlCDOM.IXMLDocument;
  xAttribute: OXmlCDOM.TXMLNode;
begin
  xXML := OXmlCDOM.CreateXMLDoc('root', False);

  xAttribute := xXML.DocumentElement.SelectNodeCreate('@attr');
  xAttribute.NodeValue := 'value';

  Result := (xXML.XML = '<root attr="value"/>');
end;

{$IFDEF USE_RTTI}
{ TText_OXmlRTTISerializer_Test1_Class }

constructor TText_OXmlRTTISerializer_Test1_Class.Create;
begin
  fMyClass := TText_OXmlSerializer_Test1_Class2.Create;
  fMyStrList := TList<string>.Create;
  fMyObjList := TObjectList<TText_OXmlSerializer_Test1_Class2>.Create;
end;

destructor TText_OXmlRTTISerializer_Test1_Class.Destroy;
begin
  fMyClass.Free;
  fMyStrList.Free;
  fMyObjList.Free;

  inherited;
end;

function TText_OXmlRTTISerializer_Test1_Class.SameAs(
  aCompare: TText_OXmlRTTISerializer_Test1_Class): Boolean;
var
  I: Integer;
begin
  Result :=
    (MyInt = aCompare.MyInt) and
    (MyEnum = aCompare.MyEnum) and
    (MySet = aCompare.MySet) and
    SameDate(MyDate, aCompare.MyDate) and
    SameDateTime(MyDateTime, aCompare.MyDateTime) and
    SameTime(MyTime, aCompare.MyTime) and
    (MyFloat = aCompare.MyFloat) and
    (MyString = aCompare.MyString) and
    (MyWideString = aCompare.MyWideString) and
    (MyClass.MyInt = aCompare.MyClass.MyInt) and
    (MyRecord.MyInt = aCompare.MyRecord.MyInt) and
    (MyRecord.MyString = aCompare.MyRecord.MyString) and
    (Length(MyRecord.MyArray) = Length(aCompare.MyRecord.MyArray)) and
    (Length(MyRecord.MyDynArray) = Length(aCompare.MyRecord.MyDynArray)) and
    (Length(MyRecord.MyDynArrayDate) = Length(aCompare.MyRecord.MyDynArrayDate)) and
    (MyStrList.Count = aCompare.MyStrList.Count) and
    (MyObjList.Count = aCompare.MyObjList.Count);

  if Result then
  begin
    for I := Low(MyRecord.MyArray) to High(MyRecord.MyArray) do
    if not (MyRecord.MyArray[I] = aCompare.MyRecord.MyArray[I]) then
    begin
      Result := False;
      Exit;
    end;
    for I := Low(MyRecord.MyDynArray) to High(MyRecord.MyDynArray) do
    if not (MyRecord.MyDynArray[I] = aCompare.MyRecord.MyDynArray[I]) then
    begin
      Result := False;
      Exit;
    end;
    for I := Low(MyRecord.MyDynArrayDate) to High(MyRecord.MyDynArrayDate) do
    if not SameDateTime(MyRecord.MyDynArrayDate[I], aCompare.MyRecord.MyDynArrayDate[I]) then
    begin
      Result := False;
      Exit;
    end;
    for I := 0 to MyStrList.Count-1 do
    if not (MyStrList[I] = aCompare.MyStrList[I]) then
    begin
      Result := False;
      Exit;
    end;
    for I := 0 to MyObjList.Count-1 do
    if not (MyObjList[I]).SameAs(aCompare.MyObjList[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;
{$ENDIF}

{ TText_OXmlSerializer_Test1_Class2 }

constructor TText_OXmlSerializer_Test1_Class2.Create;
begin
  inherited Create;
end;

constructor TText_OXmlSerializer_Test1_Class2.Create(const aMyInt: Integer);
begin
  inherited Create;

  fMyInt := aMyInt;
end;

function TText_OXmlSerializer_Test1_Class2.SameAs(
  const aComp: TText_OXmlSerializer_Test1_Class2): Boolean;
begin
  Result := fMyInt = aComp.fMyInt;
end;

{ TText_OXmlSerializer_Test1_Class }

constructor TText_OXmlSerializer_Test1_Class.Create;
begin
  fMyClass := TText_OXmlSerializer_Test1_Class2.Create;

  inherited Create;
end;

destructor TText_OXmlSerializer_Test1_Class.Destroy;
begin
  fMyClass.Free;

  inherited;
end;

function TText_OXmlSerializer_Test1_Class.SameAs(
  aCompare: TText_OXmlSerializer_Test1_Class): Boolean;
begin
  Result :=
    (MyInt = aCompare.MyInt) and
    (MyEnum = aCompare.MyEnum) and
    (MySet = aCompare.MySet) and
    {$IFDEF USE_DATEUTILS}
    SameDate(MyDate, aCompare.MyDate) and
    SameDateTime(MyDateTime, aCompare.MyDateTime) and
    SameTime(MyTime, aCompare.MyTime) and
    {$ENDIF}
    (MyFloat = aCompare.MyFloat) and
    (MyString = aCompare.MyString) and
    (MyWideString = aCompare.MyWideString) and
    (MyClass.MyInt = aCompare.MyClass.MyInt);
end;

{ TText_OXmlSerializer_Test1_Class2A }

constructor TText_OXmlSerializer_Test1_Class2A.Create(const aMyInt,
  aMyIntVar: Integer);
begin
  inherited Create(aMyInt);

  MyIntVar := aMyIntVar;
end;

function TText_OXmlSerializer_Test1_Class2A.SameAs(
  const aComp: TText_OXmlSerializer_Test1_Class2): Boolean;
var
  xComp: TText_OXmlSerializer_Test1_Class2A;
begin
  Result := aComp is TText_OXmlSerializer_Test1_Class2A;
  if Result then
  begin
    xComp := TText_OXmlSerializer_Test1_Class2A(aComp);
    Result :=
      inherited SameAs(aComp) and
      (MyIntVar = xComp.MyIntVar);
  end;
end;

end.

