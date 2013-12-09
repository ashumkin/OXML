unit OXmlUtils;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlUtils.pas

  Collection of types and methods for XML.
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
  SysUtils, Classes, OWideSupp, OEncoding
  {$IFDEF O_GENERICS}
    , Generics.Collections
  {$ELSE}
    , OHashedStrings
  {$ENDIF}
  ;

type
  TXmlNodeType = (ntDOMDocument, ntDocType, ntXMLDeclaration, ntElement,
    ntAttribute, ntText, ntCData, ntComment, ntProcessingInstruction);

  EXmlDOMException = class(Exception);

  TXmlOutputFormat = (ofNone, ofFlat, ofIndent);
  TXmlWhiteSpaceHandling = (wsTrim, wsPreserveAll, wsPreserveInTextOnly, wsAutoTag);
  TXmlBreakReading = (brNone, brAfterDocumentNode);

  {$IFDEF O_GENERICS}
  TOXmlReaderEntityList = TDictionary<OWideString,OWideString>;
  {$ELSE}
  TOXmlReaderEntityList = TOHashedStringDictionary;
  {$ENDIF}

  //virtual MS above some custom buffer (may be string, array of byte etc.)
  //  MUST BE TCustomMemoryStream -> SO THAT THE MEMORY POINTER WOULD NOT GET DESTROYED IN .Destroy!!!
  TVirtualMemoryStream = class(TCustomMemoryStream)
  public
    procedure SetPointer(aPtr: Pointer; const aSize: Longint); reintroduce;//public
    function Write(const {%H-}Buffer; {%H-}Count: Longint): Longint; override;
  end;

  IXMLCustomDocument = interface
    ['{C0ADA341-6E9C-41CD-9E67-E4F92222FBDA}']

  //protected
    function GetLoading: Boolean;
    procedure SetLoading(const aLoading: Boolean);
    function GetCodePage: Word;
    procedure SetCodePage(const aCodePage: Word);
    function GetVersion: OWideString;
    procedure SetVersion(const aVersion: OWideString);
    function GetEncoding: OWideString;
    procedure SetEncoding(const aEncoding: OWideString);
    function GetStandAlone: OWideString;
    procedure SetStandAlone(const aStandAlone: OWideString);
    function GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    procedure SetWhiteSpaceHandling(const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
    function GetStrictXML: Boolean;
    procedure SetStrictXML(const aStrictXML: Boolean);
    function GetBreakReading: TXmlBreakReading;
    procedure SetBreakReading(const aBreakReading: TXmlBreakReading);

    property Loading: Boolean read GetLoading write SetLoading;
  //public
    //clear the whole document
    procedure Clear;

    //load document from file in encoding specified by the document
    function LoadFromFile(const aFileName: String): Boolean;
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //loads XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFNDEF NEXTGEN}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_DELPHI_2009_UP}
    //load document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    //save document to file in encoding specified by the document
    procedure SaveToFile(const aFileName: String; const aOutputFormat: TXmlOutputFormat = ofNone);
    //save document to stream in encoding specified by the document
    procedure SaveToStream(const aStream: TStream; const aOutputFormat: TXmlOutputFormat = ofNone); overload;
    //save document to stream and enforce encoding
    procedure SaveToStream(const aStream: TStream; const aOutputFormat: TXmlOutputFormat;
      const aForceEncoding: TEncoding; const aWriteBOM: Boolean); overload;
    //returns XML as string
    procedure SaveToXML(var aXML: OWideString; const aOutputFormat: TXmlOutputFormat);
    {$IFNDEF NEXTGEN}
    procedure SaveToXML_UTF8(var aXML: ORawByteString; const aOutputFormat: TXmlOutputFormat);
    {$ENDIF}

    {$IFDEF O_DELPHI_2009_UP}
    //returns XML as a buffer in encoding specified by the document
    procedure SaveToBuffer(var aBuffer: TBytes; const aOutputFormat: TXmlOutputFormat); overload;
    //returns XML as a buffer and enforce a custom encoding
    procedure SaveToBuffer(var aBuffer: TBytes; const aOutputFormat: TXmlOutputFormat;
      const aForceEncoding: TEncoding; const aWriteBOM: Boolean); overload;
    {$ENDIF}

  //public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML(const aOutputFormat: TXmlOutputFormat = ofNone): OWideString;
    {$IFNDEF NEXTGEN}
    function XML_UTF8(const aOutputFormat: TXmlOutputFormat = ofNone): ORawByteString;
    {$ENDIF}

  //public

    //document whitespace handling
    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;

    //StrictXML: document must be valid XML
    // READING:
    //   = true: raise Exceptions when document is not valid
    //   = false: try to fix and go over document errors.
    //WRITING:
    //   = true: element names & values checking
    //   = false: no element names & values checking
    property StrictXML: Boolean read GetStrictXML write SetStrictXML;
    //decide if you want to read the document after the root element has been closed
    property BreakReading: TXmlBreakReading read GetBreakReading write SetBreakReading;

    //document encoding (as integer identifier) - from <?xml encoding="???"?>
    property CodePage: Word read GetCodePage write SetCodePage;
    //document encoding (as string alias) - from <?xml encoding="???"?>
    property Encoding: OWideString read GetEncoding write SetEncoding;
    //document standalone - from <?xml standalone="???"?>
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    //document version - from <?xml version="???"?>
    property Version: OWideString read GetVersion write SetVersion;
  end;

function OXmlIsNameStartChar(const aChar: OWideChar): Boolean;
function OXmlIsNameChar(const aChar: OWideChar): Boolean;
function OXmlIsWhiteSpaceChar(const aChar: OWideChar): Boolean;
function OXmlIsDecimalChar(const aChar: OWideChar): Boolean;
function OXmlIsHexadecimalChar(const aChar: OWideChar): Boolean;
function OXmlIsSignChar(const aChar: OWideChar): Boolean;
function OXmlIsBreakChar(const aChar: OWideChar): Boolean;

function OXmlNeedsPreserveAttribute(const aText: OWideString): Boolean;
function OXmlIsWhiteSpace(const aText: OWideString): Boolean;
function OXmlIsNumber(const aText: OWideString): Boolean;

function OXmlValidCData(const aText: OWideString): Boolean;
function OXmlValidComment(const aText: OWideString): Boolean;
function OXmlValidPIContent(const aText: OWideString): Boolean;

function OXmlValidName(const aText: OWideString): Boolean;

function OXmlPreserveToStr(const aPreserveWhiteSpace: Boolean): OWideString;
function OXmlStrToPreserve(const aStr: OWideString): Boolean;

implementation

resourcestring
  OXmlLng_CannotWriteToVirtualMemoryStream = 'You cannot write to a TVirtualMemoryStream.';


function OXmlStrToPreserve(const aStr: OWideString): Boolean;
begin
  Result := (aStr <> '') and ((aStr[1] = 'p') or (aStr[1] = 'P'));//preserve = true
end;

function OXmlPreserveToStr(const aPreserveWhiteSpace: Boolean): OWideString;
begin
  if aPreserveWhiteSpace then
    Result := 'preserve'
  else
    Result := 'default';
end;

function OXmlValidName(const aText: OWideString): Boolean;
var I: Integer;
begin
  if aText = '' then begin
    Result := False;
    Exit;
  end;

  Result := OXmlIsNameStartChar(aText[1]);
  if not Result then
    Exit;

  for I := 2 to Length(aText) do begin
    Result := OXmlIsNameChar(aText[I]);
    if not Result then
      Exit;
  end;
end;

function OXmlNeedsPreserveAttribute(const aText: OWideString): Boolean;
var
  I, xLength: Integer;
  xThisCharWhiteSpace, xLastCharWhiteSpace: Boolean;
begin
  if aText = '' then begin
    Result := False;
    Exit;
  end;

  xLength := Length(aText);

  Result := OXmlIsWhiteSpaceChar(aText[1]) or OXmlIsWhiteSpaceChar(aText[xLength]);
  if Result then
    Exit;

  xLastCharWhiteSpace := False;
  I := 2;//we can omit first and last characters (already checked)!
  while I < xLength do begin//we can omit first and last characters (already checked)!
    if (aText[I] = #13) and (aText[I+1] = #10) then
      Inc(I);//step over #13#10
    xThisCharWhiteSpace := OXmlIsWhiteSpaceChar(aText[I]);
    if xThisCharWhiteSpace and xLastCharWhiteSpace then begin
      Result := True;
      Exit;
    end;
    xLastCharWhiteSpace := xThisCharWhiteSpace;
    Inc(I);
  end;
end;

function OXmlIsWhiteSpace(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aText) do
  if not OXmlIsWhiteSpaceChar(aText[I]) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function OXmlIsNumber(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aText) do
  if not (
    OXmlIsDecimalChar(aText[I]) or//'0'..'1'
    ((I = 1) and OXmlIsSignChar(aText[I])))//sign
  then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function OXmlValidCData(const aText: OWideString): Boolean;
begin
  Result := (Pos(']]>', aText) = 0);
end;

function OXmlValidComment(const aText: OWideString): Boolean;
var
  xL: Integer;
begin
  xL := Length(aText);
  Result := (xL = 0) or ((Pos('--', aText) = 0) and (aText[xL] <> '-'));
end;

function OXmlValidPIContent(const aText: OWideString): Boolean;
var
  xL: Integer;
begin
  xL := Length(aText);
  Result := (xL = 0) or (Pos('?>', aText) = 0);
end;

function OXmlIsDecimalChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    Ord('0')..Ord('9'): Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsHexadecimalChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    Ord('a')..Ord('f'),
    Ord('A')..Ord('F'),
    Ord('0')..Ord('9'): Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsSignChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    Ord('-'), Ord('+'): Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsBreakChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $00..$20,//0..space
    Ord('"'),
    Ord(''''),
    Ord('/'),
    Ord('?'),
    Ord('<'),
    Ord('='),
    Ord('>'): Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsNameStartChar(const aChar: OWideChar): Boolean;
begin
{$IF DEFINED(FPC)}
  //UTF-8 characters!
  case Ord(aChar) of
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord(':'),
    Ord('_'),
    $C0..$D6,
    $D8..$F6,
    $F8..$FF: Result := True;
  else
    Result := False;
  end;
{$ELSE}
  case Ord(aChar) of
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord(':'),
    Ord('_'),
    $C0..$D6,
    $D8..$F6,
    $F8..$FF,
    $100..$2FF,
    $370..$37D,
    $37F..$1FFF,
    $200C..$200D,
    $2070..$218F,
    $2C00..$2FEF,
    $3001..$D7FF,
    $F900..$FDCF,
    $FDF0..$FFFD: Result := True;
  else
    Result := False;
  end;
{$IFEND}
end;

function OXmlIsWhiteSpaceChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $09, $0A, $0D, $20: Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsNameChar(const aChar: OWideChar): Boolean;
begin
{$IF DEFINED(FPC)}
  //UTF-8 characters!
  case Ord(aChar) of
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord('0')..Ord('9'),
    Ord(':'),
    Ord('_'),
    Ord('-'),
    Ord('.'),
    $B7,
    $C0..$D6,
    $D8..$F6,
    $F8..$FF: Result := True;
  else
    Result := False;
  end;
{$ELSE}
  case Ord(aChar) of
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord('0')..Ord('9'),
    Ord(':'),
    Ord('_'),
    Ord('-'),
    Ord('.'),
    $B7,
    $C0..$D6,
    $D8..$F6,
    $F8..$FF,
    $100..$2FF,
    $370..$37D,
    $37F..$1FFF,
    $200C..$200D,
    $2070..$218F,
    $2C00..$2FEF,
    $3001..$D7FF,
    $F900..$FDCF,
    $FDF0..$FFFD,
    $0300..$036F,
    $203F..$2040: Result := True;
  else
    Result := False;
  end;
{$IFEND}
end;

{ TVirtualMemoryStream }

procedure TVirtualMemoryStream.SetPointer(aPtr: Pointer; const aSize: Integer);
begin
  inherited SetPointer(aPtr, aSize);
end;

function TVirtualMemoryStream.{%H-}Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create(OXmlLng_CannotWriteToVirtualMemoryStream);
end;

end.
