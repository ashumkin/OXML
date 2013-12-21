// JCL_DEBUG_EXPERT_GENERATEJDBG ON
program FileList;

uses
  Forms,
  main in 'main.pas' {fMain},
  OBufferedStreams in '..\..\units\OBufferedStreams.pas',
  ODictionary in '..\..\units\ODictionary.pas',
  OEncoding in '..\..\units\OEncoding.pas',
  OHashedStrings in '..\..\units\OHashedStrings.pas',
  OTextReadWrite in '..\..\units\OTextReadWrite.pas',
  OWideSupp in '..\..\units\OWideSupp.pas',
  OXmlIntfDOM in '..\..\units\OXmlIntfDOM.pas',
  OXmlLng in '..\..\units\OXmlLng.pas',
  OXmlPDOM in '..\..\units\OXmlPDOM.pas',
  OXmlReadWrite in '..\..\units\OXmlReadWrite.pas',
  OXmlSAX in '..\..\units\OXmlSAX.pas',
  OXmlSeq in '..\..\units\OXmlSeq.pas',
  OXmlUtils in '..\..\units\OXmlUtils.pas',
  OXmlXPath in '..\..\units\OXmlXPath.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

