unit OXmlUnitTests;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

{$IFNDEF FPC}
  {$IF CompilerVersion >= 25}
    {$ZEROBASEDSTRINGS OFF}
  {$IFEND}
{$ENDIF}

interface

uses Classes, SysUtils, OWideSupp, OXmlUtils, OXmlReadWrite, OXmlPDOM;

type
  TObjFunc = function(): Boolean of object;

  TOXmlUnitTest = class(TObject)
  private
    fPassNameIfFalse: TStringList;

    procedure FunctionPassed(const aFunction: TObjFunc; const aFunctionName: String);
  private
    //OXmlReadWrite.pas
    function Test_TOXmlReader_FinishOpenElementClose_NodeName_Empty: Boolean;
    function Test_TOXmlReader_InvalidDocument1: Boolean;
  private
    //OXmlPDOM.pas
    function Test_TXMLNode_SelectNodeCreate_Attribute: Boolean;
    function Test_TXMLDocument_InvalidDocument1: Boolean;
  private
    //OWideSupp.pas
    function Test_TOTextBuffer: Boolean;
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
    fPassNameIfFalse.Add(aFunctionName);
end;

procedure TOXmlUnitTest.OXmlTestAll(const aStrList: TStrings);
var
  I: Integer;
begin
  //because this tests are supposed to run in D7 and Lazarus too,
  //we cannot use RTTI to call all test functions automatically
  // -> call here all functions manually

  FunctionPassed(Test_TOXmlReader_FinishOpenElementClose_NodeName_Empty, 'Test_TOXmlReader_FinishOpenElementClose_NodeName_Empty');
  FunctionPassed(Test_TOXmlReader_InvalidDocument1, 'Test_TOXmlReader_InvalidDocument1');
  FunctionPassed(Test_TXMLNode_SelectNodeCreate_Attribute, 'Test_TXMLNode_SelectNodeCreate_Attribute');
  FunctionPassed(Test_TXMLDocument_InvalidDocument1, 'Test_TXMLDocument_InvalidDocument1');
  FunctionPassed(Test_TOTextBuffer, 'Test_TOTextBuffer');


  if fPassNameIfFalse.Count = 0 then
    aStrList.Text := 'OXml: all tests passed'
  else
  begin
    aStrList.Clear;
    aStrList.Add('');
    aStrList.Add(Format('ERROR OXml: %d test(s) not passed:', [fPassNameIfFalse.Count]));

    for I := 0 to fPassNameIfFalse.Count-1 do
      aStrList.Add(fPassNameIfFalse[I]);
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
        xBuf.GetBuffer(xC, I+1, 1);
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

function TOXmlUnitTest.Test_TOXmlReader_FinishOpenElementClose_NodeName_Empty: Boolean;
var
  xReader: TXMLReader;
  xResult: OWideString;
begin
  xReader := TXMLReader.Create;
  try
    xReader.ReaderSettings.NodePathHandling := npFull;
    xReader.InitXML('<root attribute="1" />');

    xResult := '';
    while xReader.ReadNextToken do
    begin
      xResult := xResult + Format('%d:%s:%s;', [Ord(xReader.ReaderToken.TokenType), xReader.ReaderToken.TokenName, xReader.ReaderToken.TokenValue]);
    end;

    Result := xResult = '4:root:;5:attribute:1;7:root:;';
  finally
    xReader.Free;
  end;
end;

function TOXmlUnitTest.Test_TOXmlReader_InvalidDocument1: Boolean;
const
  inXML: OWideString = '<root><b>TEXT</i><p><t><aaa/></p></root>';
var
  xXMLReader: TXMLReader;
  xToken: TXMLReaderToken;

  procedure CheckNextToken(aTokenType: TXMLReaderTokenType; const aTokenName, aTokenValue: OWideString);
  begin
    Result := Result and xXMLReader.ReadNextToken and
      ((xToken.TokenType = aTokenType) and (xToken.TokenName = aTokenName) and (xToken.TokenValue = aTokenValue));
  end;
begin
  Result := True;

  xXMLReader := TXMLReader.Create;
  try
    xToken := xXMLReader.ReaderToken;

    xXMLReader.ReaderSettings.StrictXML := False;
    xXMLReader.ReaderSettings.NodePathHandling := npFull;

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

