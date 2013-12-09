program OXmlTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uXmlTest, OBufferedStreams, ODictionary, OEncoding, OHashedStrings,
  OTextReadWrite, OWideSupp, OXmlLng, OXmlReadWrite, OXmlSAX,
  OXmlSeq, OXmlUtils, OXmlXPath, OXmlIntfDOM, OXmlPDOM, NativeXml;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

