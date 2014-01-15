program OXmlTest;

uses
  Vcl.Forms,
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
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OmniXML in '..\..\units\OmniXML\OmniXML.pas',
  OmniXML_Dictionary in '..\..\units\OmniXML\OmniXML_Dictionary.pas',
  OmniXML_LookupTables in '..\..\units\OmniXML\OmniXML_LookupTables.pas',
  OmniXML_MSXML in '..\..\units\OmniXML\OmniXML_MSXML.pas',
  OmniXML_Types in '..\..\units\OmniXML\OmniXML_Types.pas',
  OmniXMLConf in '..\..\units\OmniXML\OmniXMLConf.pas',
  OmniXMLDatabase in '..\..\units\OmniXML\OmniXMLDatabase.pas',
  OmniXMLPersistent in '..\..\units\OmniXML\OmniXMLPersistent.pas',
  OmniXMLProperties in '..\..\units\OmniXML\OmniXMLProperties.pas',
  OmniXMLUtils in '..\..\units\OmniXML\OmniXMLUtils.pas',
  OmniXMLXPath in '..\..\units\OmniXML\OmniXMLXPath.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
