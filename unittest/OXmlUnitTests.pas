unit OXmlUnitTests;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion >= 25}
    {$ZEROBASEDSTRINGS OFF}
  {$IFEND}
{$ENDIF}

interface

uses Classes, SysUtils, OWideSupp, OXmlUtils, OXmlReadWrite, OXmlPDOM,
  OHashedStrings;

type
  TObjFunc = function(): Boolean of object;

  TOXmlUnitTest = class(TObject)
  private const
    cTestCount = 9;
  private
    fPassNameIfFalse: TStringList;
    fPassedCount: Integer;

    function GetAllTestCount: Integer;
    procedure FunctionPassed(const aFunction: TObjFunc; const aFunctionName: String);
  private
    //OXmlReadWrite.pas
    function Test_TXMLReader_FinishOpenElementClose_NodeName_Empty: Boolean;
    function Test_TXMLReader_InvalidDocument1: Boolean;
  private
    //OXmlPDOM.pas
    function Test_TXMLNode_SelectNodeCreate_Attribute: Boolean;
    function Test_TXMLNode_Clone: Boolean;
    function Test_TXMLNode_Normalize: Boolean;
    function Test_TXMLDocument_InvalidDocument1: Boolean;
    function Test_TXMLDocument_WhiteSpaceHandling: Boolean;
  private
    //OWideSupp.pas
    function Test_TOTextBuffer: Boolean;
  private
    //OHashedStrings
    function Test_TOHashedStrings_Grow: Boolean;
  public
    procedure OXmlTestAll(const aStrList: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation


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

procedure TOXmlUnitTest.FunctionPassed(const aFunction: TObjFunc;
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

  FunctionPassed(Test_TXMLReader_FinishOpenElementClose_NodeName_Empty, 'Test_TXMLReader_FinishOpenElementClose_NodeName_Empty');
  FunctionPassed(Test_TXMLReader_InvalidDocument1, 'Test_TXMLReader_InvalidDocument1');
  FunctionPassed(Test_TXMLNode_SelectNodeCreate_Attribute, 'Test_TXMLNode_SelectNodeCreate_Attribute');
  FunctionPassed(Test_TXMLNode_Clone, 'Test_TXMLNode_Clone');
  FunctionPassed(Test_TXMLNode_Normalize, 'Test_TXMLNode_Normalize');
  FunctionPassed(Test_TXMLDocument_InvalidDocument1, 'Test_TXMLDocument_InvalidDocument1');
  FunctionPassed(Test_TXMLDocument_WhiteSpaceHandling, 'Test_TXMLDocument_WhiteSpaceHandling');
  FunctionPassed(Test_TOTextBuffer, 'Test_TOTextBuffer');
  FunctionPassed(Test_TOHashedStrings_Grow, 'Test_TOHashedStrings_Grow');

  aStrList.Clear;

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
    aStrList.Add('ERROR OXmlUnitTest: test count is invalid.');
    aStrList.Add(Format('tests runned: %d, tests expected: %d',
      [GetAllTestCount, cTestCount]));
  end;

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

function TOXmlUnitTest.Test_TXMLDocument_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
  outXML: OWideString = '<root><b>TEXT</b><p><t><aaa/></t></p></root>';
var
  xXML: IXMLDocument;
begin
  xXML := CreateXMLDoc;
  xXML.ReaderSettings.StrictXML := False;
  xXML.WriterSettings.StrictXML := False;

  xXML.WhiteSpaceHandling := wsPreserveAll;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_TXMLDocument_WhiteSpaceHandling: Boolean;
const
  inXML: OWideString =  '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default"> default <p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p>  </text>  </root>';
  outXML: OWideString = '<root xml:space="preserve">'+sLineBreak+'<text xml:space="default">default<p xml:space="preserve"> text <b> hello <br/> </b>  my text'+sLineBreak+'</p></text>  </root>';
var
  xXML: IXMLDocument;
begin
  xXML := CreateXMLDoc;

  xXML.WhiteSpaceHandling := wsAutoTag;
  xXML.LoadFromXML(inXML);

  Result := (xXML.XML = outXML);
end;

function TOXmlUnitTest.Test_TXMLNode_Clone: Boolean;
const
  inXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone></root>';
  outXML: OWideString = '<root><clone attr="value"><n>text</n><m/></clone><clone attr="value"/><clone attr="value"><n>text</n><m/></clone></root>';
var
  xXML: IXMLDocument;
  xCloneNode: PXMLNode;
begin
  xXML := CreateXMLDoc;
  xXML.LoadFromXML(inXML);
  xCloneNode := xXML.DocumentElement.SelectNode('clone');
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(False));
  xXML.DocumentElement.AppendChild(xCloneNode.CloneNode(True));

  Result := xXML.XML = outXML;
end;

function TOXmlUnitTest.Test_TXMLNode_Normalize: Boolean;
const
  outXML: OWideString = '<root><test/>my  text<b>hello<clone/></b></root>';
var
  xXML: IXMLDocument;
  xDocElement, xNodeB: PXMLNode;
begin
  xXML := CreateXMLDoc('root');
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

function TOXmlUnitTest.Test_TXMLNode_SelectNodeCreate_Attribute: Boolean;
var
  xXML: IXMLDocument;
  xAttribute: PXMLNode;
begin
  xXML := CreateXMLDoc('root', False);

  xAttribute := xXML.DocumentElement.SelectNodeCreate('@attr');
  xAttribute.NodeValue := 'value';

  Result := (xXML.XML = '<root attr="value"/>');
end;

end.

