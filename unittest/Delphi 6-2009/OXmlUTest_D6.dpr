program OXmlUTest_D6;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  StrUtils,
  OXmlUnitTests in '..\OXmlUnitTests.pas',
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas',
  OXmlCDOM in '..\..\units\OXmlCDOM.pas',
  OXmlSerialize in '..\..\units\OXmlSerialize.pas',
  OXmlCSeq in '..\..\units\OXmlCSeq.pas',
  OXmlPSeq in '..\..\units\OXmlPSeq.pas',
  OJsonUtils in '..\..\units\OJsonUtils.pas',
  OJsonUtf8ReadWrite in '..\..\units\OJsonUtf8ReadWrite.pas',
  OJsonReadWrite in '..\..\units\OJsonReadWrite.pas';

var
  xTest: TOXmlUnitTest;
  xStrL: TStringList;
  I: Integer;
begin
  xTest := TOXmlUnitTest.Create;
  xStrL := TStringList.Create;
  try
    xTest.OXmlTestAll(xStrL);
    for I := 0 to xStrL.Count-1 do
    begin
      Writeln(xStrL[I]);
      if AnsiStartsText('ERROR', xStrL[i])
          or AnsiStartsText('WARNING', xStrL[i]) then
        ExitCode := 1;
    end;
  finally
    xStrL.Free;
    xTest.Free;
  end;

  if ParamStr(1) = '-i' then
  begin
    Writeln;
    Write('Press ENTER to close.');
    Readln;
  end;
end.
