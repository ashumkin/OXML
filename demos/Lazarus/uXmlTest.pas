unit uXmlTest;

{$mode delphi}{$H+}

{.$DEFINE USE_DELPHIXML}//define/undefine to compare OXml with Delphi XML
{.$DEFINE USE_MSXML}//define/undefine to compare OXml with MS XML
{.$DEFINE USE_OMNIXML}//define/undefine to compare OXml with OmniXML
{.$DEFINE USE_NATIVEXML}//define/undefine to compare OXml with NativeXML
{.$DEFINE USE_VERYSIMPLE}//define/undefine to compare OXml with VerySimpleXML
{.$DEFINE USE_SIMPLEXML}//define/undefine to compare OXml with SimpleXML: http://www.audio-data.de/simplexml.html

{$IFDEF FPC}
  {$DEFINE USE_GENERICS}
{$ELSE}
  {$IF CompilerVersion >= 20}//D2009
    {$DEFINE USE_GENERICS}
  {$IFEND}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFDEF USE_DELPHIXML}
  XMLIntf, XMLDoc,
  {$ENDIF}
  {$IFDEF USE_MSXML}
  msxmldom, msxml,
  {$ENDIF}
  {$IFDEF USE_OMNIXML}
  OmniXML,
  {$ENDIF}
  {$IFDEF USE_NATIVEXML}
  NativeXml,
  {$ENDIF}
  {$IFDEF USE_VERYSIMPLE}
  Xml.VerySimple,
  {$ENDIF}
  OXmlReadWrite, OXmlUtils, OEncoding, Windows, OXmlPDOM, OXmlSAX,
  OWideSupp, OTextReadWrite, OXmlSeq;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnDOMTest: TButton;
    BtnEncodingTest: TButton;
    BtnInterfaceCreate: TButton;
    BtnIterateTest: TButton;
    BtnResaveWithDOM: TButton;
    BtnSequentialTest: TButton;
    BtnTestReadInvalid: TButton;
    BtnTestSAX: TButton;
    BtnTestWriteInvalid: TButton;
    BtnTestXPath: TButton;
    BtnXmlDirectWrite: TButton;
    LblTimeInfo: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure BtnDOMTestClick(Sender: TObject);
    procedure BtnIterateTestClick(Sender: TObject);
    procedure BtnSequentialTestClick(Sender: TObject);
    procedure BtnTestReadInvalidClick(Sender: TObject);
    procedure BtnTestWriteInvalidClick(Sender: TObject);
    procedure BtnXmlDirectWriteClick(Sender: TObject);
    procedure BtnResaveWithDOMClick(Sender: TObject);
    procedure BtnTestXPathClick(Sender: TObject);
    procedure BtnTestSAXClick(Sender: TObject);
    procedure BtnInterfaceCreateClick(Sender: TObject);
    procedure BtnEncodingTestClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    DocDir: String;

    procedure SAXStartDocument(Sender: TSAXParser);
    procedure SAXEndDocument(Sender: TSAXParser);
    procedure SAXCharacters(Sender: TSAXParser; const aText: OWideString);
    procedure SAXComment(Sender: TSAXParser; const aText: OWideString);
    procedure SAXProcessingInstruction(Sender: TSAXParser; const aTarget, aContent: OWideString);
    procedure SAXStartElement(Sender: TSAXParser; const aName: OWideString;
      const aAttributes: TSAXAttributes);
    procedure SAXEndElement(Sender: TSAXParser; const aName: OWideString);
  protected
    procedure DoCreate; override;
  end;

function SAXEscapeString(const aString: OWideString): OWideString;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.BtnResaveWithDOMClick(Sender: TObject);
  {$IFDEF USE_DELPHIXML}
  procedure TestDelphiXmlDOM;
  var
    xXml: XMLIntf.IXMLDocument;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := XMLDoc.TXMLDocument.Create(nil);
    xXml.LoadFromFile(DocDir+'sheet1.xml');
    xXml.Active := True;
    xT2 := GetTickCount;
    xXml.SaveToFile(DocDir+'sheet1-resave.xml');

    xXml := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'DELPHI XML DOM'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      '(The very good results from this test are a fact that no Delphi nodes'+sLineBreak+
      'are created from the MS XML DOM. '+sLineBreak+
      'Basically, MS XML DOM is very fast but the Delphi implementation is'+sLineBreak+
      'slow (and the Delphi implementation is not used in this demo.)'+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}
  {$IFDEF USE_MSXML}
  procedure TestMSXmlDOM;
  var
    xXml: msxml.IXMLDOMDocument;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := msxmldom.CreateDOMDocument;
    xXml.load(DocDir+'sheet1.xml');
    xT2 := GetTickCount;
    xXml.save(DocDir+'sheet1-resave.xml');

    xXml := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'MS XML DOM'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  {$IFDEF USE_OMNIXML}
  procedure TestOmniXmlDOM;
  var
    xXml: OmniXml.IXMLDocument;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := OmniXml.CreateXMLDoc;
    //xXml.WhiteSpaceHandling := OmniXML.wsPreserveAll;//enable/disable according to OmniXML mod
    xXml.PreserveWhiteSpace := True;//enable/disable according to OmniXML mod
    xXml.Load(DocDir+'sheet1.xml');
    xT2 := GetTickCount;
    xXml.Save(DocDir+'sheet1-resave.xml');

    xXml := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OmniXML DOM'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;

  procedure TestOmniXmlDOM_MS;
  var
    xXml: OmniXml.IXMLDocument;
    xT1, xT2: Cardinal;
    xMS: TMemoryStream;
  begin
    xT1 := GetTickCount;
    xXml := OmniXml.CreateXMLDoc;
    //xXml.WhiteSpaceHandling := OmniXML.wsPreserveAll;//enable/disable according to OmniXML mod
    xXml.PreserveWhiteSpace := True;//enable/disable according to OmniXML mod
    xXml.Load(DocDir+'sheet1.xml');
    xT2 := GetTickCount;
    xMS := TMemoryStream.Create;
    try
      xXml.SaveToStream(xMS);
      xMS.Position := 0;
      xMS.SaveToFile(DocDir+'sheet1-resave.xml');
    finally
      xMS.Free;
    end;

    xXml := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OmniXML DOM (TMemoryStream)'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  {$IFDEF USE_NATIVEXML}
  procedure TestNativeXmlDOM;
  var
    xXml: NativeXml.TNativeXml;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := NativeXml.TNativeXml.Create(nil);
    try
      xXml.XmlFormat := NativeXml.xfPreserve;
      xXml.LoadFromFile(DocDir+'sheet1.xml');
      xT2 := GetTickCount;
      xXml.SaveToFile(DocDir+'sheet1-resave.xml');

      FreeAndNil(xXml);

      Memo1.Lines.Text :=
        Memo1.Lines.Text+sLineBreak+
        'NativeXml DOM'+sLineBreak+
        'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
        'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
        'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
        sLineBreak+sLineBreak;
    finally
      xXml.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF USE_VERYSIMPLE}
  procedure TestVerySimpleXmlDOM;
  var
    xXml: Xml.VerySimple.TXmlVerySimple;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := Xml.VerySimple.TXmlVerySimple.Create;
    try
      xXml.LoadFromFile(DocDir+'sheet1.xml');
      xT2 := GetTickCount;
      xXml.SaveToFile(DocDir+'sheet1-resave.xml');

      FreeAndNil(xXml);

      Memo1.Lines.Text :=
        Memo1.Lines.Text+sLineBreak+
        'VerySimpleXML DOM'+sLineBreak+
        'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
        'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
        'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
        sLineBreak+sLineBreak;
    finally
      xXml.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF USE_SIMPLEXML}
  procedure TestSimpleXmlDOM;
  var
    xXml: SimpleXML.IXmlDocument;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := SimpleXML.CreateXmlDocument;
    xXml.PreserveWhiteSpace := True;
    xXml.Load(DocDir+'sheet1.xml');
    xT2 := GetTickCount;
    xXml.Save(DocDir+'sheet1-resave.xml');

    xXml := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'SimpleXML DOM'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  procedure TestOXmlPDOM;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xT1, xT2: Cardinal;
  begin
    xT1 := GetTickCount;
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.WhiteSpaceHandling := wsPreserveAll;
    xXml.LoadFromFile(DocDir+'sheet1.xml');
    xT2 := GetTickCount;
    xXml.SaveToFile(DocDir+'sheet1-resave.xml');

    xXml := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OXml record DOM (default)'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;

  procedure TestDirect;
    procedure _DoNothing(const {%H-}aStr1, {%H-}aStr2: OWideString);
    begin
    end;
  var
    xXmlReader: TXMLReader;
    xXmlWriter: TXMLWriter;
    xT1, xT2: Cardinal;
    xE: TXMLReaderToken;
  begin
    xXmlReader := nil;
    xXmlWriter := nil;
    try
      xXmlReader := TXMLReader.Create;
      xXmlWriter := TXMLWriter.Create;

      xXmlReader.InitFile(DocDir+'sheet1.xml');
      xXmlReader.ReaderSettings.NodePathHandling := npNo;

      xXmlWriter.InitFile(DocDir+'sheet1-resave.xml');
      xXmlWriter.Encoding := TEncoding.UTF8;
      xXmlWriter.WriterSettings.WriteBOM := False;

      xE := xXmlReader.ReaderToken;

      xT1 := GetTickCount;
      //simulate reading
      while xXmlReader.ReadNextToken do begin
        _DoNothing(xE.TokenName, xE.TokenValue);
      end;
      xT2 := GetTickCount;

      //read+write
      xXmlReader.InitFile(DocDir+'sheet1.xml');
      xXmlWriter.XmlDeclaration(True);
      while xXmlReader.ReadNextToken do begin
        case xXmlReader.ReaderToken.TokenType of
          rtAttribute: xXmlWriter.Attribute(xE.TokenName, xE.TokenValue);
          rtOpenElement: xXmlWriter.OpenElement(xE.TokenName);
          rtFinishOpenElement: xXmlWriter.FinishOpenElement;
          rtFinishOpenElementClose: xXmlWriter.FinishOpenElementClose;
          rtCloseElement: xXmlWriter.CloseElement(xE.TokenName);
          rtText: xXmlWriter.Text(xE.TokenValue);
          rtCData: xXmlWriter.CData(xE.TokenValue);
          rtComment: xXmlWriter.Comment(xE.TokenValue);
          rtProcessingInstruction: xXmlWriter.ProcessingInstruction(xE.TokenName, xE.TokenValue);
        end;
      end;
    finally
      xXmlReader.Free;
      xXmlWriter.Free;
    end;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OXml direct reader/writer'+sLineBreak+
      'Whole: '+FloatToStr((GetTickCount-xT2) / 1000)+sLineBreak+
      'Read: '+FloatToStr((xT2-xT1) / 1000)+sLineBreak+
      'Write: '+FloatToStr(((GetTickCount-xT2)-(xT2-xT1)) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  procedure TestSAX;
  var
    xSAX: TSAXParser;
    xT1: Cardinal;
  begin
    xT1 := GetTickCount;

    xSAX := TSAXParser.Create;
    try
      xSAX.ReaderSettings.NodePathHandling := npLastPath;//better performance than npFull

      xSAX.ParseFile(DocDir+'sheet1.xml');
    finally
      xSAX.Free;
    end;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OXml SAX'+sLineBreak+
      'Read: '+FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;

  procedure MatchTestFiles;
  var
    xReader1, xReader2: TOTextReader;
    xC1, xC2: OWideChar;
    I: Integer;
  begin
    xC1 := #0;
    xC2 := #0;
    xReader1 := TOTextReader.Create;
    xReader2 := TOTextReader.Create;
    try
      xReader1.InitFile(DocDir+'sheet1.xml');
      xReader2.InitFile(DocDir+'sheet1-resave.xml');

      //start comparing after PI
      while (xC1 <> '>') do
        if not xReader1.ReadNextChar(xC1) then
          Break;
      while xC2 <> '>' do
        if not xReader2.ReadNextChar(xC2) then
          Break;
      xReader1.ReadNextChar(xC1);
      xReader2.ReadNextChar(xC2);
      while OXmlIsWhiteSpaceChar(xC1) do
        if not xReader1.ReadNextChar(xC1) then
          Break;
      while OXmlIsWhiteSpaceChar(xC2) do
        if not xReader2.ReadNextChar(xC2) then
          Break;

      while True do begin
        xReader1.ReadNextChar(xC1);
        xReader2.ReadNextChar(xC2);
        if xC1 <> xC2 then begin
          //get some information
          xReader1.ClearCustomBuffer;
          xReader2.ClearCustomBuffer;
          xReader1.WritePreviousCharToBuffer;
          xReader2.WritePreviousCharToBuffer;
          for I := 0 to 19 do begin
            xReader1.ReadNextChar(xC1);
            xReader2.ReadNextChar(xC2);
            xReader1.WritePreviousCharToBuffer;
            xReader2.WritePreviousCharToBuffer;
          end;

          raise Exception.Create('Files do not match:'+sLineBreak+
            'Reader1 = '+xReader1.GetCustomBuffer+sLineBreak+
            'Reader2 = '+xReader2.GetCustomBuffer+sLineBreak);
        end;

        if (xC1 = #0) or (xC2 = #0) then
          Break;
      end;
    finally
      xReader1.Free;
      xReader2.Free;
    end;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  {$IFDEF USE_DELPHIXML}
  TestDelphiXmlDOM;
  {$ENDIF}

  {$IFDEF USE_MSXML}
  TestMSXmlDOM;
  {$ENDIF}

  {$IFDEF USE_OMNIXML}
  TestOmniXmlDOM;
  MatchTestFiles;//comment/uncomment to check if files match

  //TestOmniXmlDOM_MS;
  {$ENDIF}

  {$IFDEF USE_NATIVEXML}
  TestNativeXmlDOM;
  MatchTestFiles;//comment/uncomment to check if files match
  {$ENDIF}

  {$IFDEF USE_VERYSIMPLE}
  TestVerySimpleXmlDOM;
  MatchTestFiles;//comment/uncomment to check if files match
  {$ENDIF}

  {$IFDEF USE_SIMPLEXML}
  TestSimpleXmlDOM;
  //MatchTestFiles;//comment/uncomment to check if files match
  {$ENDIF}

  TestOXmlPDOM;
  MatchTestFiles;//comment/uncomment to check if files match

  TestSAX;

  TestDirect;
  MatchTestFiles;//comment/uncomment to check if files match

end;

procedure TForm1.BtnSequentialTestClick(Sender: TObject);
  procedure TestSeq(const aXML: OWideString; aMemo: TMemo);
  var
    xSeqParser: TXMLSeqParser;
    xNode, xAttr: OXmlPDOM.PXMLNode;
    xItemsElementIsOpen: Boolean;
    xName, xColor, xText: OWideString;
  begin
    xSeqParser := TXMLSeqParser.Create;
    try
      xSeqParser.InitXML(aXML);

      if not xSeqParser.GoToPath('/root/items:test') then
        raise Exception.Create('Wrong XML document.');

      if not xSeqParser.ReadNextChildElementHeader({%H-}xNode, {%H-}xItemsElementIsOpen) then
        raise Exception.Create('Wrong XML document.');

      aMemo.Lines.Add(xNode.XML);

      if xItemsElementIsOpen then begin
        while xSeqParser.ReadNextChildNode(xNode) do
        begin
          if (xNode.NodeType = ntElement) and
             (xNode.NodeName = 'item')
          then begin
            if xNode.FindAttribute('color', {%H-}xAttr) then
              xColor := xAttr.NodeValue
            else
              xColor := '[default]';
            xName := xNode.Attributes['name'];
            xText := xNode.Text;

            aMemo.Lines.Add(xNode.XML);
            aMemo.Lines.Add('  -> '+xName+':'+xColor+':'+xText);
          end;
        end;
      end;

      while xSeqParser.ReadNextChildElementHeaderClose(xNode) do
      begin
        if (xNode.NodeType = ntElement) and
           (xNode.NodeName = 'info')
        then
          aMemo.Lines.Add(xNode.XML);
      end;
    finally
      xSeqParser.Free;
    end;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  TestSeq(
    '<root>'+
      '<items:test defaultcolor="red">'+
        '<item name="car" color="blue" />'+
        '<skip>Skip this element</skip>'+
        '<item name="bike">bike has <b>default</b> color!</item>'+
        '<item name="tree" color="green" />'+
      '</items:test>'+
      '<info text="Yes, I want to know it!" />'+
      'skip texts'+
      '<info text="Show me!" />'+
    '</root>',
  Memo1);

  TestSeq(
    '<root>'+
      '<items:test defaultcolor="red" />'+
      'skip texts'+
      '<info text="Yes, I want to know it!">'+
        'skip this information'+
        '<skip>all elements in info tag are not read</skip>'+
      '</info>'+
      '<info text="Show me!" />'+
    '</root>',
  Memo2);

end;

function SAXEscapeString(const aString: OWideString): OWideString;
begin
  Result := aString;
  Result := OStringReplace(Result, sLineBreak, '\n', [rfReplaceAll]);
  Result := OStringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

procedure TForm1.BtnTestWriteInvalidClick(Sender: TObject);

  procedure TestOXmlPDOM;
  var
    xXML: OXmlPDOM.IXMLDocument;
    xRoot: OXmlPDOM.PXMLNode;
  begin
    xXML := OXmlPDOM.CreateXMLDoc('root');
    xXML.WriterSettings.StrictXML := False;//set to true/false - allow/disallow invalid document
    xXML.ReaderSettings.StrictXML := False;//set to true/false - allow/disallow invalid document

    //comment/uncomment to test validity
    xRoot := xXML.DocumentElement;
    xRoot.Attributes['0name'] := 'test';//invalid attribute name
    xRoot.AddChild('0name');//invalid name
    xRoot.AddComment('te--st');//invalid comment (a comment cannot contain "--" string
    xRoot.AddCDATASection('te]]>st');//invalid cdata (a cdata section cannot contain "]]>" string

    Memo2.Lines.Text := xXML.XML;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  TestOXmlPDOM;
end;

procedure TForm1.BtnTestReadInvalidClick(Sender: TObject);
const
  cXML: OWideString =
    '<root>'+sLineBreak+
    '  <item < AttributeWithoutValue attr2 = value2 attr3  =  "value3"  ? />'+sLineBreak+
    '  <![aaa'+sLineBreak+
    '  kolo >'+sLineBreak+
    '  <test />>'+sLineBreak+
    '  <!-x'+sLineBreak+
    '  <!'+sLineBreak+
    '  <!rr'+sLineBreak+
    '  <!DOC'+
    '  <<<'+sLineBreak+
    ' &nbsp ; '+sLineBreak+
    '  <lo.ko> <para> <b> TEXT </i> </lo.ko>'+
    '  <? = aaa ?>'+
    '  <?= aaa ?>'+
    '  <?aaa ?>'+
    '  <??>'+
    '</root>';

  procedure TestOXmlPDOM;
  var
    xXml: OXmlPDOM.IXMLDocument;
  begin
    xXML := OXmlPDOM.CreateXMLDoc;
    xXML.ReaderSettings.StrictXML := False;//set to true/false - allow/disallow invalid document
    xXML.WriterSettings.StrictXML := False;//set to true/false - allow/disallow invalid document

    xXML.WhiteSpaceHandling := wsPreserveAll;
    xXML.LoadFromXML(cXML);

    Memo2.Lines.Add('OXml record based DOM:');
    Memo2.Lines.Text := Memo2.Lines.Text + xXML.XML;
  end;
begin
  Memo1.Lines.Text :=
    'Original invalid XML:'+sLineBreak+sLineBreak+
    cXML;
  Memo2.Lines.Text :=
    'XML as it is read and understood by OXml:'+sLineBreak+sLineBreak;

  TestOXmlPDOM;
end;

procedure TForm1.BtnTestSAXClick(Sender: TObject);
var
  xSAX: TSAXParser;
const
  cXML: OWideString =
    '<?xml version="1.0"?>'+sLineBreak+
    '<seminararbeit>'+sLineBreak+
    ' <titel>DOM, SAX und SOAP</titel>'+sLineBreak+
    ' <inhalt>'+sLineBreak+
    '  <kapitel value="1">Einleitung</kapitel>'+sLineBreak+
    '  <kapitel value="2" attr="val">Hauptteil</kapitel>'+sLineBreak+
    '  <kapitel value="3">Fazit</kapitel>'+sLineBreak+
    '  <kapitel value="4" />'+sLineBreak+
    ' </inhalt>'+sLineBreak+
    ' <!-- comment -->'+sLineBreak+
    ' <![CDATA[ cdata ]]>'+sLineBreak+
    ' <?php echo "custom processing instruction" ?>'+sLineBreak+
    '</seminararbeit>'+sLineBreak;
begin
  Memo1.Lines.Text := 'Events:'+sLineBreak+sLineBreak;
  Memo2.Lines.Text := 'Anonymous methods:'+sLineBreak+sLineBreak;

  Memo1.Lines.BeginUpdate;
  Memo2.Lines.BeginUpdate;
  xSAX := TSAXParser.Create;
  try
    //old-fashioned events
    xSAX.OnStartDocument := SAXStartDocument;
    xSAX.OnEndDocument := SAXEndDocument;
    xSAX.OnCharacters := SAXCharacters;
    xSAX.OnComment := SAXComment;
    xSAX.OnProcessingInstruction := SAXProcessingInstruction;
    xSAX.OnStartElement := SAXStartElement;
    xSAX.OnEndElement := SAXEndElement;

    {$IFNDEF FPC}{$IF CompilerVersion >= 20}
    //anonymous methods
    xSAX.StartDocumentProc := (
      procedure(aSaxParser: TSAXParser)
      begin
        Memo2.Lines.Add('startDocument()');
      end);

    xSAX.EndDocumentProc := (
      procedure(aSaxParser: TSAXParser)
      begin
        Memo2.Lines.Add('endDocument()');
      end);

    xSAX.CharactersProc := (
      procedure(aSaxParser: TSAXParser; const aText: OWideString)
      begin
        Memo2.Lines.Add('characters("'+SAXEscapeString(aText)+'")');
      end);

    xSAX.CommentProc := (
      procedure(aSaxParser: TSAXParser; const aText: OWideString)
      begin
        Memo2.Lines.Add('comment("'+SAXEscapeString(aText)+'")');
      end);

    xSAX.ProcessingInstructionProc := (
      procedure(aSaxParser: TSAXParser; const aTarget, aContent: OWideString)
      begin
        Memo2.Lines.Add('processingInstruction("'+SAXEscapeString(aTarget)+'", "'+SAXEscapeString(aContent)+'")');
      end);

    xSAX.StartElementProc := (
      procedure(aSaxParser: TSAXParser; const aName: OWideString;
        const aAttributes: TSAXAttributes)
      var
        xAttrStr: OWideString;
        xAttr: TSAXAttribute;
      begin
        xAttrStr := '';
        for xAttr in aAttributes do begin
          if xAttrStr <> '' then
            xAttrStr := xAttrStr + ', ';
          xAttrStr := xAttrStr + SAXEscapeString(xAttr.AttrName)+'="'+SAXEscapeString(xAttr.AttrValue)+'"';
        end;
        xAttrStr := '['+xAttrStr+']';

        Memo2.Lines.Add('startElement("'+SAXEscapeString(aName)+'", '+xAttrStr+')');
      end);

    xSAX.EndElementProc := (
      procedure(aSaxParser: TSAXParser; const aName: OWideString)
      begin
        Memo2.Lines.Add('endElement("'+SAXEscapeString(aName)+'")');
      end);
    {$IFEND}{$ENDIF}

    xSAX.ParseXML(cXML);
  finally
    xSAX.Free;

    Memo1.Lines.EndUpdate;
    Memo2.Lines.EndUpdate;
  end;
end;

procedure TForm1.BtnTestXPathClick(Sender: TObject);
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

  {$IFDEF USE_OMNIXML}
  procedure TestOmniXML;
  var
    xXml: OmniXML.IXMLDocument;

    procedure _TestXPathElements(const aStartNode: OmniXML.IXMLNode; const aXPath, aResult: OWideString);
    var
      xList: OmniXML.IXMLNodeList;
      xElement: OmniXML.IXMLNode;
      xAttr: OmniXML.IXMLNode;
      xStr: OWideString;
      I: Integer;
    begin
      aStartNode.SelectNodes(aXPath, {%H-}xList);
      if xList.Length > 0 then begin
        xStr := '';
        for I := 0 to xList.Length-1 do begin
          xElement := xList.Item[I];

          if xStr <> '' then
            xStr := xStr+sLineBreak;
          case xElement.NodeType of
            ELEMENT_NODE: begin
              xStr := xStr+xElement.NodeName+'=';
              xAttr := xElement.Attributes.GetNamedItem('name');
              if Assigned(xAttr) then
                xStr := xStr+xAttr.NodeValue;
            end;
            ATTRIBUTE_NODE: xStr := xStr+xElement.ParentNode.NodeName+':'+xElement.NodeName+'='+xElement.NodeValue;
            TEXT_NODE, CDATA_SECTION_NODE: xStr := xStr+xElement.NodeValue;
          end;
        end;

        if xStr <> aResult then begin
          Memo1.Lines.Text := (
            'XPath test failed: '+aXPath+sLineBreak+
              xStr+sLineBreak+
              '-----'+sLineBreak+
              aResult);
          raise Exception.Create(Form1.Memo1.Lines.Text);
        end;
      end else begin
        raise Exception.Create('XPath test failed (nothing selected): '+aXPath);
      end;
    end;
  begin
    xXml := OmniXML.CreateXMLDoc;

    xXml.LoadXML(cXML);

    _TestXPathElements(xXml.DocumentElement, '.', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root/.', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root/boss/..', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root/person', 'person=Paul Caster');
    //not supported by OmniXML:_TestXPathElements(xXml.DocumentElement, '..//person[@name="boss person/2.1"]', 'person=boss person/2.1');
    //not supported by OmniXML:_TestXPathElements(xXml.DocumentElement, '//person[@name="boss person/2.1"]', 'person=boss person/2.1');
    //not supported by OmniXML:_TestXPathElements(xXml, '//person[@name]', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml, '//root//person/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
    //OmniXML: ERROR _TestXPathElements(xXml, '//person/../../boss', 'boss=Max Muster');
    _TestXPathElements(xXml, '//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml, 'root//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml, 'root//boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
    _TestXPathElements(xXml, 'root//*', 'boss=Max Muster'+sLineBreak+'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml, 'root/*', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml, '/root/boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
    _TestXPathElements(xXml, 'root/boss', 'boss=Max Muster');
    //not supported by OmniXML:_TestXPathElements(xXml, 'root/person|root/boss', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml, 'root', 'root=');
    _TestXPathElements(xXml, 'root/boss/person[2]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
    _TestXPathElements(xXml, 'root/person[1]', 'person=Paul Caster');
    //not supported by OmniXML:_TestXPathElements(xXml, 'root/person[last()]', 'person=Paul Caster');
    //not supported by OmniXML:_TestXPathElements(xXml, '/root/*[last()-1]/person[last()]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
    //not supported by OmniXML:_TestXPathElements(xXml, '//text()', 'this text is in person tag'+sLineBreak+'some test info');
    //not supported by OmniXML:_TestXPathElements(xXml, 'root/node()', 'root:description=test xml'+sLineBreak+'boss=Max Muster'+sLineBreak+'person=Paul Caster'+sLineBreak+'some test info');

    //not supported by OmniXML:_TestXPathElements(xXml, 'root//@*', 'root:description=test xml'+sLineBreak+'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'dog:type=fight'+sLineBreak+'person:name=Paul Caster');
    //not supported by OmniXML:_TestXPathElements(xXml, 'root//@name', 'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'person:name=Paul Caster');

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OmniXML DOM: All XPath tests succeeded.';
  end;
  {$ENDIF}

  procedure TestOXmlPDOM;
  var
    xXml: OXmlPDOM.IXMLDocument;

    procedure _TestXPathElements(const aStartNode: OXmlPDOM.PXMLNode; const aXPath, aResult: OWideString);
    var
      xList: OXmlPDOM.IXMLNodeList;
      xElement: OXmlPDOM.PXMLNode;
      xStr: OWideString;
      {$IFNDEF USE_FORIN}
      I: Integer;
      {$ENDIF}
    begin
      if aStartNode.SelectNodes(aXPath, {%H-}xList) then begin
        xStr := '';
        {$IFDEF USE_FORIN}
        for xElement in xList do begin
        {$ELSE}
        for I := 0 to xList.Count-1 do begin
          xElement := xList[I];
        {$ENDIF}
          if xStr <> '' then
          if xStr <> '' then
            xStr := xStr+sLineBreak;
          case xElement.NodeType of
            ntElement: xStr := xStr+xElement.NodeName+'='+xElement.Attributes['name'];
            ntAttribute: xStr := xStr+xElement.ParentNode.NodeName+':'+xElement.NodeName+'='+xElement.NodeValue;
            ntText, ntCData: xStr := xStr+xElement.NodeValue;
          end;
        end;

        if xStr <> aResult then begin
          Memo1.Lines.Text := (
            'XPath test failed: '+aXPath+sLineBreak+
              xStr+sLineBreak+
              '-----'+sLineBreak+
              aResult);
          raise Exception.Create(Memo1.Lines.Text);
        end;
      end else begin
        raise Exception.Create('XPath test failed (nothing selected): '+aXPath);
      end;
    end;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml := TXMLDocument.Create(nil);
    xXml.LoadFromXML(cXML);

    _TestXPathElements(xXml.DocumentElement, '.', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root/.', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root/boss/..', 'root=');
    _TestXPathElements(xXml.DocumentElement, '../root/person', 'person=Paul Caster');
    _TestXPathElements(xXml.DocumentElement, '..//person[@name="boss person/2.1"]', 'person=boss person/2.1');
    _TestXPathElements(xXml.DocumentElement, '//person[@name="boss person/2.1"]', 'person=boss person/2.1');
    _TestXPathElements(xXml.Node, '//person[@name]', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml.Node, '//root//person/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
    _TestXPathElements(xXml.Node, '//person/../../boss', 'boss=Max Muster');
    _TestXPathElements(xXml.Node, '//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml.Node, 'root//person', 'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml.Node, 'root//boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
    _TestXPathElements(xXml.Node, 'root//*', 'boss=Max Muster'+sLineBreak+'person=boss person'+sLineBreak+'person=boss person 2'+sLineBreak+'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml.Node, 'root/*', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml.Node, '/root/boss/person', 'person=boss person'+sLineBreak+'person=boss person 2');
    _TestXPathElements(xXml.Node, 'root/boss', 'boss=Max Muster');
    _TestXPathElements(xXml.Node, 'root/person|root/boss', 'boss=Max Muster'+sLineBreak+'person=Paul Caster');
    _TestXPathElements(xXml.Node, 'root', 'root=');
    _TestXPathElements(xXml.Node, 'root/boss/person[2]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
    _TestXPathElements(xXml.Node, 'root/person[1]', 'person=Paul Caster');
    _TestXPathElements(xXml.Node, 'root/person[last()]', 'person=Paul Caster');
    _TestXPathElements(xXml.Node, '/root/*[last()-1]/person[last()]/*', 'person=boss person/2.1'+sLineBreak+'dog=boss dog 2.2');
    _TestXPathElements(xXml.Node, '//text()', 'this text is in person tag'+sLineBreak+'some test info');
    _TestXPathElements(xXml.Node, 'root/node()', 'root:description=test xml'+sLineBreak+'boss=Max Muster'+sLineBreak+'person=Paul Caster'+sLineBreak+'some test info');


    _TestXPathElements(xXml.Node, 'root//@*', 'root:description=test xml'+sLineBreak+'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'dog:type=fight'+sLineBreak+'person:name=Paul Caster');
    _TestXPathElements(xXml.Node, 'root//@name', 'boss:name=Max Muster'+sLineBreak+'person:name=boss person'+sLineBreak+'person:name=boss person 2'+sLineBreak+'person:name=boss person/2.1'+sLineBreak+'dog:name=boss dog 2.2'+sLineBreak+'person:name=Paul Caster');

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OXml record DOM: All XPath tests succeeded.';
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  {$IFDEF USE_OMNIXML}
  TestOmniXML;
  {$ENDIF}

  TestOXmlPDOM;
end;

procedure TForm1.BtnIterateTestClick(Sender: TObject);
const
  cXML: OWideString =
    '<root attr1="z" attr2="o" attr3="3x" attr4="y4">'+
      '<element1>Hello</element1>'+
      '<element2>Bye</element2>'+
      '<element3/>'+
    '</root>';

  {$IFDEF USE_FORIN}
  procedure TestOXmlPDOMForIn;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    for xNode in xXML.DocumentElement.ChildNodes do
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);

    Memo2.Lines.Add('');
  end;
  {$ENDIF}

  procedure TestOXmlPDOMForTo;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
    I: Integer;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    for I := 0 to xRoot.ChildNodes.Count-1 do begin
      xNode := xRoot.ChildNodes[I];
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);
    end;

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMForDownTo;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
    I: Integer;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    for I := xRoot.ChildNodes.Count-1 downto 0 do begin
      xNode := xRoot.ChildNodes[I];
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);
    end;

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMNextChild;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    xNode := nil;
    while xRoot.GetNextChild(xNode) do
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMPreviousChild;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    xNode := nil;
    while xRoot.GetPreviousChild(xNode) do
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMNextSibling;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xNode := xXML.DocumentElement.FirstChild;
    while Assigned(xNode) do begin
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);
      xNode := xNode.NextSibling;
    end;

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMPreviousSibling;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xNode := xXML.DocumentElement.LastChild;
    while Assigned(xNode) do begin
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.Text);
      xNode := xNode.PreviousSibling;
    end;

    Memo2.Lines.Add('');
  end;

  {$IFDEF USE_FORIN}
  procedure TestOXmlPDOMAttributesForIn;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    for xNode in xXML.DocumentElement.AttributeNodes do
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.NodeValue);

    Memo2.Lines.Add('');
  end;
  {$ENDIF}

  procedure TestOXmlPDOMAttributesForTo;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
    I: Integer;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    for I := 0 to xRoot.AttributeNodes.Count-1 do begin
      xNode := xRoot.AttributeNodes[I];
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.NodeValue);
    end;

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMAttributesForDownTo;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
    I: Integer;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    for I := xRoot.AttributeNodes.Count-1 downto 0 do begin
      xNode := xRoot.AttributeNodes[I];
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.NodeValue);
    end;

    Memo2.Lines.Add('');
  end;

  procedure TestOXmlPDOMNextAttribute;
  var
    xXml: OXmlPDOM.IXMLDocument;
    xRoot, xNode: OXmlPDOM.PXMLNode;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.LoadFromXML(cXML);

    xRoot := xXML.DocumentElement;
    xNode := nil;
    while xRoot.GetNextAttribute(xNode) do
      Memo2.Lines.Add(xNode.NodeName+': '+xNode.NodeValue);

    Memo2.Lines.Add('');
  end;

begin
  Memo1.Lines.BeginUpdate;
  Memo2.Lines.BeginUpdate;
  try
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;

    {$IFDEF USE_FORIN}
    TestOXmlPDOMForIn;
    {$ENDIF}
    TestOXmlPDOMForTo;
    TestOXmlPDOMForDownTo;
    TestOXmlPDOMNextChild;
    TestOXmlPDOMNextSibling;
    TestOXmlPDOMPreviousChild;
    TestOXmlPDOMPreviousSibling;
    {$IFDEF USE_FORIN}
    TestOXmlPDOMAttributesForIn;
    {$ENDIF}
    TestOXmlPDOMAttributesForTo;
    TestOXmlPDOMAttributesForDownTo;
    TestOXmlPDOMNextAttribute;

  finally
    Memo1.Lines.EndUpdate;
    Memo2.Lines.EndUpdate;
  end;
end;

procedure TForm1.BtnDOMTestClick(Sender: TObject);
  procedure TestOXmlPDOM;
  var
    xXML: OXmlPDOM.IXMLDocument;
    xRoot: OXmlPDOM.PXMLNode;
    xChild1, xChild2, xChild3, xReplacedChild: OXmlPDOM.PXMLNode;
    xAttribute: OXmlPDOM.PXMLNode;
  begin
    xXML := OXmlPDOM.CreateXMLDoc('root');

    xRoot := xXML.DocumentElement;

    xChild1 := xXML.CreateElement('test');
    xRoot.AppendChild(xChild1);

    xAttribute := xXML.CreateAttribute('attr', 'value');
    xChild1.SetAttributeNode(xAttribute);

    xChild2 := xXML.CreateElement('child2');
    xRoot.InsertBefore(xChild2, xChild1);

    xChild3 := xXML.CreateElement('node3');
    xReplacedChild := xRoot.ReplaceChild(xChild3, xChild2);
    if Assigned(xReplacedChild) then//the replaced child doesn't get destroyed automatically
      xReplacedChild.DeleteSelf;//free the old child

    Memo2.Lines.Text := xXML.XML;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  TestOXmlPDOM;
end;

procedure TForm1.BtnEncodingTestClick(Sender: TObject);
  procedure TestOXmlPDOM;
  var
    xXML: OXmlPDOM.IXMLDocument;
  begin
    xXML := OXmlPDOM.CreateXMLDoc;

    xXML.LoadFromFile(DocDir+'koi8-r.xml');
    xXML.DocumentElement.NodeName := 'rootnode';
    xXML.DocumentElement.SelectNode('load').LoadFromXML('some <i>text</i> with <b>tags</b>');
    xXML.CodePage := CP_WIN_1251;
    xXML.SaveToFile(DocDir+'1251.xml');

    xXML.WriterSettings.IndentType := itIndent;
    Memo2.Lines.Text := xXML.XML;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  TestOXmlPDOM;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Memo1.SetBounds(0, LblTimeInfo.BoundsRect.Bottom + 5, ClientWidth div 2, ClientHeight-LblTimeInfo.BoundsRect.Bottom - 5);
  Memo2.SetBounds(Memo1.BoundsRect.Right, Memo1.BoundsRect.Top, ClientWidth-Memo1.BoundsRect.Right, Memo1.Height);
end;

procedure TForm1.BtnInterfaceCreateClick(Sender: TObject);
  {$IFDEF USE_DELPHIXML}
  procedure DelphiXmlTest;
  var
    I: Integer;
    xT1: Cardinal;
    xXML: XmlIntf.IXMLDocument;
    xRootNode, xNode: XmlIntf.IXMLNode;
  begin
    xT1 := GetTickCount;

    xXML := XMLDoc.TXMLDocument.Create(nil);
    xXML.Active := True;
    xXML.DocumentElement := xXML.CreateElement('root', '');
    xRootNode := xXML.DocumentElement;


    for I := 1 to 10*1000 do begin
      xNode := xRootNode.AddChild('text');
      xNode.AddChild('A'+IntToStr(I)).AddChild('noname').AddChild('some').AddChild('p').Text := 'afg';
      xNode.SetAttribute('attr1', 'A'+IntToStr(I));
      xNode.SetAttribute('attr2', 'const');
      xNode.SetAttribute('attr3', 'const');
    end;

    xRootNode := nil;
    xNode := nil;
    xXML := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'DELPHI XML DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      'IMPORTANT: The Delphi XML performance is so horrible'+sLineBreak+
      'that it''s not possible to create the nodes within a reasonable'+sLineBreak+
      'time limit. Therefore only 1/10 of the nodes are created!'+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}
  {$IFDEF USE_MSXML}
  procedure MSXmlTest;
  var
    I: Integer;
    xT1: Cardinal;
    xXML: msxml.IXMLDOMDocument;
    xRootNode, xNode, xNewChild, xFirstNode: msxml.IXMLDOMNode;
    xNewAttr: msxml.IXMLDOMAttribute;
  begin
    xT1 := GetTickCount;

    xXML := msxmldom.CreateDOMDocument;
    xRootNode := xXML.createElement('root');
    xXML.appendChild(xRootNode);
    for I := 1 to 100*1000 do begin
      xNewChild := xXML.CreateElement('text');
      xNode := xRootNode.AppendChild(xNewChild);
      xFirstNode := xNode;

      xNewChild := xXML.CreateElement('A'+IntToStr(I));
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateElement('noname');
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateElement('some');
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateElement('p');
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateTextNode('afg');
      xNode := xNode.AppendChild(xNewChild);

      xNewAttr := xXML.CreateAttribute('attr1');
      xNewAttr.Value := 'A'+IntToStr(I);
      xFirstNode.Attributes.setNamedItem(xNewAttr);
      xNewAttr := xXML.CreateAttribute('attr2');
      xNewAttr.Value := 'const';
      xFirstNode.Attributes.setNamedItem(xNewAttr);
      xNewAttr := xXML.CreateAttribute('attr3');
      xNewAttr.Value := 'const';
      xFirstNode.Attributes.setNamedItem(xNewAttr);
    end;

    xRootNode := nil;
    xNode := nil;
    xXML := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'MS XML DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  {$IFDEF USE_OMNIXML}
  procedure OmniXmlTest;
  var
    I: Integer;
    xT1: Cardinal;
    xXML: OmniXml.IXMLDocument;
    xRootNode, xNode, xNewChild, xFirstNode: OmniXml.IXMLNode;
    xNewAttr: OmniXml.IXMLAttr;
  begin
    xT1 := GetTickCount;

    xXML := OmniXml.CreateXMLDoc;
    xXML.DocumentElement := xXML.CreateElement('root');
    xRootNode := xXML.DocumentElement;
    for I := 1 to 100*1000 do begin
      xNewChild := xXML.CreateElement('text');
      xNode := xRootNode.AppendChild(xNewChild);
      xFirstNode := xNode;

      xNewChild := xXML.CreateElement('A'+IntToStr(I));
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateElement('noname');
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateElement('some');
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateElement('p');
      xNode := xNode.AppendChild(xNewChild);

      xNewChild := xXML.CreateTextNode('afg');
      xNode := xNode.AppendChild(xNewChild);

      xNewAttr := xXML.CreateAttribute('attr1');
      xNewAttr.Value := 'A'+IntToStr(I);
      xFirstNode.Attributes.Add(xNewAttr);
      xNewAttr := xXML.CreateAttribute('attr2');
      xNewAttr.Value := 'const';
      xFirstNode.Attributes.Add(xNewAttr);
      xNewAttr := xXML.CreateAttribute('attr3');
      xNewAttr.Value := 'const';
      xFirstNode.Attributes.Add(xNewAttr);
    end;

    xRootNode := nil;
    xNode := nil;
    xXML := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OmniXML DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  {$IFDEF USE_NATIVEXML}
  procedure NativeXmlTest;
  var
    I: Integer;
    xT1: Cardinal;
    xXML: NativeXml.TNativeXml;
    xRootNode, xNode, xNodeN: NativeXml.TXmlNode;
  begin
    xT1 := GetTickCount;

    xXML := NativeXml.TNativeXml.CreateEx(nil, False, False, True, 'root');
    try
      xRootNode := xXML.Root;
      for I := 1 to 100*1000 do begin
        xNode := xXML.NodeNew('text');
        xRootNode.NodeAdd(xNode);

        xNode.AttributeAdd('attr1', UTF8Encode('A'+IntToStr(I)));
        xNode.AttributeAdd('attr2', 'const');
        xNode.AttributeAdd('attr3', 'const');

        xNodeN := xXML.NodeNew(UTF8Encode('A'+IntToStr(I)));
        xNode.NodeAdd(xNodeN);
        xNode := xNodeN;

        xNodeN := xXML.NodeNew('noname');
        xNode.NodeAdd(xNodeN);
        xNode := xNodeN;

        xNodeN := xXML.NodeNew('some');
        xNode.NodeAdd(xNodeN);
        xNode := xNodeN;

        xNodeN := xXML.NodeNew('p');
        xNode.NodeAdd(xNodeN);
        xNode := xNodeN;

        xNodeN := xXML.NodeNewTextType('', 'afg', xeCharData);
        xNode.NodeAdd(xNodeN);
      end;
    finally
      xXML.Free;
    end;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'NativeXml DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  {$IFDEF USE_VERYSIMPLE}
  procedure VerySimpleXmlTest;
  var
    xXML: Xml.VerySimple.TXmlVerySimple;
    I: Integer;
    xT1: Cardinal;
    xRootNode, xNode: Xml.VerySimple.TXMLNode;
  begin
    xT1 := GetTickCount;

    xXML := Xml.VerySimple.TXmlVerySimple.Create;
    try
      xXML.Root.NodeName := 'root';
      xRootNode := xXML.Root;
      for I := 1 to 100*1000 do begin
        xNode := xRootNode.AddChild('text');
        xNode.AddChild('A'+IntToStr(I)).AddChild('noname').AddChild('some').AddChild('p').Text := 'afg';
        xNode.SetAttribute('attr1', 'A'+IntToStr(I));
        xNode.SetAttribute('attr2', 'const');
        xNode.SetAttribute('attr3', 'const');
      end;

      xRootNode := nil;
      xNode := nil;
    finally
      xXML.Free;
    end;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'VerySimpleXML DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  {$IFDEF USE_SIMPLEXML}
  procedure SimpleXmlTest;
  var
    xXML: SimpleXML.IXmlDocument;
    I: Integer;
    xT1: Cardinal;
    xRootNode, xNode, xNewChild, xFirstNode: SimpleXML.IXMLNode;
  begin
    xT1 := GetTickCount;

    xXML := SimpleXML.CreateXmlDocument('root', '1.0', 'utf-8');

    xRootNode := xXML.DocumentElement;
    for I := 1 to 100*1000 do begin
      xNode := xXML.CreateElement('text');
      xRootNode.AppendChild(xNode);
      xFirstNode := xNode;

      xNewChild := xXML.CreateElement('A'+IntToStr(I));
      xNode.AppendChild(xNewChild);
      xNode := xNewChild;

      xNewChild := xXML.CreateElement('noname');
      xNode.AppendChild(xNewChild);
      xNode := xNewChild;

      xNewChild := xXML.CreateElement('some');
      xNode.AppendChild(xNewChild);
      xNode := xNewChild;

      xNewChild := xXML.CreateElement('p');
      xNode.AppendChild(xNewChild);
      xNode := xNewChild;

      xNewChild := xXML.CreateText('afg');
      xNode.AppendChild(xNewChild);
      xNode := xNewChild;

      xFirstNode.SetAttr('attr1', 'A'+IntToStr(I));
      xFirstNode.SetAttr('attr2', 'const');
      xFirstNode.SetAttr('attr3', 'const');
    end;

    xRootNode := nil;
    xNode := nil;
    xFirstNode := nil;
    xXML := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'SimpleXML DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
  {$ENDIF}

  procedure OXmlPDOMTest;
  var
    xXML: OXmlPDOM.IXMLDocument;
    I: Integer;
    xT1: Cardinal;
    xRootNode, xNode: OXmlPDOM.PXMLNode;
  begin
    xT1 := GetTickCount;

    xXML := OXmlPDOM.CreateXMLDoc('root');
    xRootNode := xXML.DocumentElement;
    for I := 1 to 100*1000 do begin
      xNode := xRootNode.AddChild('text');
      xNode.AddChild('A'+IntToStr(I)).AddChild('noname').AddChild('some').AddChild('p').AddText('afg');
      xNode.AddAttribute('attr1', 'A'+IntToStr(I));
      xNode.AddAttribute('attr2', 'const');
      xNode.AddAttribute('attr3', 'const');
    end;

    xXML := nil;

    Memo1.Lines.Text :=
      Memo1.Lines.Text+sLineBreak+
      'OXml record DOM'+sLineBreak+
      FloatToStr((GetTickCount-xT1) / 1000)+sLineBreak+
      sLineBreak+sLineBreak;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  {$IFDEF USE_DELPHIXML}
  DelphiXmlTest;
  {$ENDIF}
  {$IFDEF USE_MSXML}
  MSXmlTest;
  {$ENDIF}
  {$IFDEF USE_OMNIXML}
  OmniXmlTest;
  {$ENDIF}
  {$IFDEF USE_NATIVEXML}
  NativeXmlTest;
  {$ENDIF}
  {$IFDEF USE_VERYSIMPLE}
  VerySimpleXmlTest;
  {$ENDIF}
  {$IFDEF USE_SIMPLEXML}
  SimpleXmlTest;
  {$ENDIF}
  OXmlPDOMTest;
end;

procedure TForm1.BtnXmlDirectWriteClick(Sender: TObject);
  procedure WriteDocument;
  var
    xS: TStream;
    xXmlWriter: TXMLWriter;
    xRootElement, xPersonElement: TXMLWriterElement;
  begin
    xS := TFileStream.Create(DocDir+'simple.xml', fmCreate);
    xXmlWriter := TXMLWriter.Create;
    try
      xXmlWriter.InitStream(xS);
      xXmlWriter.Encoding := TEncoding.UTF8;
      xXmlWriter.WriterSettings.IndentType := itNone;

      xXmlWriter.DocType('root PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"');
      xXmlWriter.XmlDeclaration(True);

      xRootElement := xXmlWriter.OpenElementR('root');
      xRootElement.Attribute('description', 'test xml');

      xPersonElement := xRootElement.OpenElementR('boss');
      xPersonElement.Attribute('name', '?Max Muster');
      xPersonElement.CloseElement;

      xRootElement.Comment('this is some text in comment');

      xPersonElement := xRootElement.OpenElementR('person');
      xPersonElement.Attribute('name', '/Paul Caster');
      xPersonElement.Text('/this text is in person tag');
      xPersonElement.CloseElement;

      xRootElement.Text('some test info');

      xRootElement.CData('!this is some text in <CDATA> section.');
      xRootElement.ProcessingInstruction('target', '((custom processing instruction.))');

      xRootElement.CloseElement;
    finally
      xXmlWriter.Free;
      xS.Free;
    end;
  end;

  procedure TestOXmlPDOM;
  var
    xXml: OXmlPDOM.IXMLDocument;
  begin
    xXml := OXmlPDOM.CreateXMLDoc;
    xXml.WhiteSpaceHandling := wsPreserveAll;
    xXml.LoadFromFile(DocDir+'simple.xml');
    xXML.WriterSettings.IndentType := itIndent;
    Memo2.Lines.Text :=
      xXml.XML+sLineBreak+sLineBreak+
      '------'+sLineBreak+
      xXml.DocumentElement.Text;
  end;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  //WRITE DOCUMENT
  WriteDocument;

  //READ DOCUMENT AND FORMAT IT NICELY
  TestOXmlPDOM;
end;

procedure TForm1.DoCreate;
begin
  inherited;

  DocDir := ExtractFilePath(Application.ExeName)+'..'+PathDelim+'doc'+PathDelim;

  {$IFNDEF FPC}{$IF CompilerVersion >= 18}//Delphi 2006 UP
  ReportMemoryLeaksOnShutdown := True;
  {$IFEND}{$ENDIF}
end;

procedure TForm1.SAXCharacters(Sender: TSAXParser; const aText: OWideString);
begin
  Memo1.Lines.Add('characters("'+SAXEscapeString(aText)+'")');
end;

procedure TForm1.SAXComment(Sender: TSAXParser; const aText: OWideString);
begin
  Memo1.Lines.Add('comment("'+SAXEscapeString(aText)+'")');
end;

procedure TForm1.SAXEndDocument(Sender: TSAXParser);
begin
  Memo1.Lines.Add('endDocument()');
end;

procedure TForm1.SAXEndElement(Sender: TSAXParser; const aName: OWideString);
begin
  Memo1.Lines.Add('endElement("'+SAXEscapeString(aName)+'")');
end;

procedure TForm1.SAXProcessingInstruction(Sender: TSAXParser; const aTarget,
  aContent: OWideString);
begin
  Memo1.Lines.Add('processingInstruction("'+SAXEscapeString(aTarget)+'", "'+SAXEscapeString(aContent)+'")');
end;

procedure TForm1.SAXStartDocument(Sender: TSAXParser);
begin
  Memo1.Lines.Add('startDocument()');
end;

procedure TForm1.SAXStartElement(Sender: TSAXParser; const aName: OWideString;
  const aAttributes: TSAXAttributes);
var
  xAttrStr: OWideString;
  xAttr: TSAXAttribute;
  {$IFNDEF USE_FORIN}
  I: Integer;
  {$ENDIF}
begin
  xAttrStr := '';
  {$IFDEF USE_FORIN}
  for xAttr in aAttributes do
  begin
  {$ELSE}
  for I := 0 to aAttributes.Count-1 do
  begin
    xAttr := aAttributes.Attributes[I];
  {$ENDIF}
    if xAttrStr <> '' then
      xAttrStr := xAttrStr + ', ';
    xAttrStr := xAttrStr + SAXEscapeString(xAttr.AttrName)+'="'+SAXEscapeString(xAttr.AttrValue)+'"';
  end;
  xAttrStr := '['+xAttrStr+']';

  Memo1.Lines.Add('startElement("'+SAXEscapeString(aName)+'", '+xAttrStr+')');
end;

end.

