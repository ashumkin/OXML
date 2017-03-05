program OXmlUTest_Laz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, StrUtils,
  OXmlUnitTests, OXmlXPath, OBufferedStreams, ODictionary,
  OEncoding, OHashedStrings, OTextReadWrite, OWideSupp, OXmlLng, OXmlReadWrite,
  OXmlSAX, OXmlUtils, OXmlPDOM, OXmlCDOM, OXmlSerialize, OJsonUtf8ReadWrite,
  OJsonUtils;

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

  Writeln;
  {$IFDEF MSWINDOWS}
  Write('Press enter to close.');
  Readln;
  {$ENDIF}
end.