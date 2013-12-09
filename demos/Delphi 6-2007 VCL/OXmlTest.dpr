program OXmlTest;

uses
  Forms,
  OXmlTestUnit in 'OXmlTestUnit.pas' {Form1},
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas',
  OXmlSeq in '..\..\units\OXmlSeq.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas',
  NativeXml in '..\..\..\NativeXML\units\NativeXml.pas',
  OXmlIntfDOM in '..\..\units\OXmlIntfDOM.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
