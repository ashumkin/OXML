unit OXmlPDOM;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlPDOM.pas

  XML DOM record/pointer implementation

  Simplified W3C DOM (Core) Level 1 specification:
    http://www.w3.org/TR/REC-DOM-Level-1/level-one-core.html
  - OXmlPDOM uses record-based nodes instead of interfaces for maximum
    performance.

  Very close to MSXML/OmniXML implementations but much faster.

  Uses a never-reallocated node buffer for very fast creation of nodes.

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
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes,
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    System.Generics.Collections,
    {$ELSE}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}

  OWideSupp, OXmlReadWrite, OEncoding, OHashedStrings, OXmlUtils, OXmlXPath,
  ODictionary;

type

  TXMLDocument = class;
  XMLNodeId = ONativeUInt;
  PXMLNode = ^TXMLNode;
  //IXMLNode = PXMLNode;//Can be disabled/enabled so that you don't have to change IXMLNode to PXMLNode if you switch from MS XML / OmniXML

  IXMLNodeList = interface;
  TXMLChildNodeList = class;

  TXMLNode = packed {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fNodeType: TXMLNodeType;
    fNodeNameId: OHashedStringsIndex;
    fNodeValueId: OHashedStringsIndex;
    fParentNode: PXMLNode;

    fFirstCChild: Array[TXMLChildType] of PXMLNode;
    fLastCChild: Array[TXMLChildType] of PXMLNode;
    fNextSibling: PXMLNode;
    fPreviousSibling: PXMLNode;

    fPreserveWhiteSpace: TXMLPreserveWhiteSpace;

    fOwnerDocument: TXMLDocument;
  private
    //methods to work with child/attribute nodes
    procedure Append(const aNew: PXMLNode; const aChildType: TXMLChildType);
    procedure Insert(const aNew, aBeforeNode: PXMLNode; const aChildType: TXMLChildType);
    procedure Remove(const aOld: PXMLNode; const aChildType: TXMLChildType);

    function GetNextCChild(var ioChildEnum: PXMLNode; const aChildType: TXMLChildType): Boolean;
    function GetPreviousCChild(var ioChildEnum: PXMLNode; const aChildType: TXMLChildType): Boolean;
    function GetFirstCChild(const aChildType: TXMLChildType): PXMLNode;
    function GetLastCChild(const aChildType: TXMLChildType): PXMLNode;
    function GetCChildFromBegin(const aIndex: Integer; const aChildType: TXMLChildType): PXMLNode;
    function GetCChildFromEnd(const aIndex: Integer; const aChildType: TXMLChildType): PXMLNode;
    procedure DeleteCChildren(const aDestroyList: Boolean; const aChildType: TXMLChildType);
  private
    function GetNodeName: OWideString;
    function GetNodeValue: OWideString;
    procedure SetNodeValue(const aValue: OWideString);
    function GetDoPreserveWhiteSpace: Boolean;
    function GetText: OWideString;
    procedure SetText(const aText: OWideString);
    function _AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
    procedure _SetAttribute(const aAttrName, aAttrValue: OWideString);

    function AddCustomChild(const aType: TXmlNodeType; const aName, aValue: OWideString): PXMLNode;
    function InsertCustomChild(const aType: TXmlNodeType; const aName, aValue: OWideString;
      const aBeforeNode: PXMLNode): PXMLNode;

    function GetAttributeNodes: TXMLChildNodeList;
    function GetChildNodes: TXMLChildNodeList;
    function GetAttributeCount: Integer;
    function GetChildCount: Integer;
    function TryGetChildNodes(var outList: TXMLChildNodeList; const aChildType: TXMLChildType): Boolean;
    function GetIsTextElement: Boolean;
    //assign only basic properties: PreserveWhiteSpace
    procedure AssignProperties(const aFromNode: PXMLNode);
  private
    //methods for direct reading/writing
    procedure WriteChildrenXML(const aWriter: TXMLWriter);
    procedure WriteAttributesXML(const aWriter: TXMLWriter);
  public
    procedure Init(const aNodeType: TXmlNodeType;
      const aNodeNameId, aNodeValueId: OHashedStringsIndex;
      const aOwnerDocument: TXMLDocument);
  public
    //create and append an element child
    function AddChild(const aElementName: OWideString): PXMLNode;
    //create and add an attribute (if attr does not exist, it will be appended to the end
    //  if attr exists, its value will be replaced but the attr won't be moved to the end)
    //  return attribute node
    function AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
    //create and append an XML declaration child
    function AddXMLDeclaration: PXMLNode;
    //create and append a text child
    function AddText(const aText: OWideString): PXMLNode;
    //create and append a CData child
    function AddCDATASection(const aText: OWideString): PXMLNode;
    //create and append a comment child
    function AddComment(const aText: OWideString): PXMLNode;
    //create and append a DOCTYPE child
    function AddDocType(const aDocTypeRawText: OWideString): PXMLNode;
    //create and append a processing instruction child
    function AddProcessingInstruction(const aTarget, aContent: OWideString): PXMLNode;

    //create and insert an element child
    function InsertChild(const aElementName: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    //create and insert an attribute
    //  return attribute node
    function InsertAttribute(const aAttrName, aAttrValue: OWideString; const aBeforeAttribute: PXMLNode): PXMLNode; overload;
    function InsertAttribute(const aAttrName, aAttrValue: OWideString; const aBeforeAttributeName: OWideString): PXMLNode; overload;
    //etc.
    function InsertXMLDeclaration(const aBeforeNode: PXMLNode): PXMLNode;
    function InsertText(const aText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertCDATASection(const aText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertComment(const aText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertDocType(const aDocTypeRawText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertProcessingInstruction(const aTarget, aContent: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
  public
    function HasChildNodes: Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
    function HasAttributes: Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
    function HasAttribute(const aName: OWideString): Boolean;
    function FindAttribute(const aName: OWideString; var outAttr: PXMLNode): Boolean; overload;
    function FindAttribute(const aName: OWideString; var outValue: OWideString): Boolean; overload;
    //get attribute
    function GetAttribute(const aName: OWideString): OWideString;
    //get attribute, if attr does not exist, return aDefaultValue
    function GetAttributeDef(const aName, aDefaultValue: OWideString): OWideString;
    //set attribute and return self
    function SetAttribute(const aName, aValue: OWideString): PXMLNode;

    //atribute nodes
    property AttributeNodes: TXMLChildNodeList read GetAttributeNodes;
    property AttributeCount: Integer read GetAttributeCount;
    //element children
    property ChildNodes: TXMLChildNodeList read GetChildNodes;
    property ChildCount: Integer read GetChildCount;

    //iterate through all children from first to last (get first for ioChildEnum=nil)
    function GetNextChild(var ioChildEnum: PXMLNode): Boolean;
    //iterate through all attributes from first to last (get first for aAttributeEnum=nil)
    function GetNextAttribute(var ioAttrEnum: PXMLNode): Boolean;
    //iterate through all children from last to first (get last for ioChildEnum=nil)
    function GetPreviousChild(var ioChildEnum: PXMLNode): Boolean;
    //iterate through all attributes from last to first (get last for ioAttrEnum=nil)
    function GetPreviousAttribute(var ioAttrEnum: PXMLNode): Boolean;

    procedure DeleteAttribute(const aName: OWideString); overload;
    procedure DeleteAttribute(const aAttr: PXMLNode); overload;
    procedure DeleteAttributes(const aDestroyList: Boolean = True);
    procedure DeleteChild(const aChild: PXMLNode);
    procedure DeleteChildren(const aDestroyList: Boolean = True);
    procedure DeleteSelf;
    procedure RemoveSelfFromParent;

    //insert a node before another
    //  Inserts the node aNewChild before the existing child node aRefChild.
    //  If aRefChild is nil, insert aNewChild at the end of the list of children.
    //  If the aNewChild is already in the tree, it is first removed.
    function InsertBefore(const aNewChild, aRefChild: PXMLNode): PXMLNode;
    //replace a child
    //  Replaces the child node oldChild with aNewChild in the list of children, and returns the aOldChild node.
    //  If the aNewChild is already in the tree, it is first removed.
    //  The removed child is not destroyed in any case!
    function ReplaceChild(const aNewChild, aOldChild: PXMLNode): PXMLNode;
    //remove a child
    //  Removes the child node indicated by aOldChild from the list of children, and returns it.
    //  The removed child is not destroyed in any case!
    function RemoveChild(const aOldChild: PXMLNode): PXMLNode;
    //append a child
    //  Adds the node aNewChild to the end of the list of children of this node.
    //  If the aNewChild is already in the tree, it is first removed.
    function AppendChild(const aNewChild: PXMLNode): PXMLNode;
    //get attribute node by name
    function GetAttributeNode(const aAttrName: OWideString): PXMLNode;
    //set attribute
    //  if the aAttr replaces an existing attribute with the same name, the previously existing Attr node is returned, otherwise nil is returned.
    function SetAttributeNode(const aAttr: PXMLNode): PXMLNode;
    //remove an attribute
    //  Removes the attribute node indicated by aOldAttribute from the list of children, and returns it.
    //  The removed attribute is not destroyed in any case!
    function RemoveAttribute(const aOldAttribute: PXMLNode): PXMLNode;
    //clone node
    function CloneNode(const aDeep: Boolean): PXMLNode;
    //consolidate adjacent text nodes and remove any empty text nodes
    procedure Normalize;
  public
    //select the first node by XPath, if not found return false (and outNode=nil)
    function SelectNode(const aXPath: OWideString; var outNode: PXMLNode): Boolean; overload;
    //select the first node by XPath, if not found return nil
    function SelectNode(const aXPath: OWideString): PXMLNode; overload;
    //select the first node by XPath, if not found return a fake "dummy" node (name="", value="")
    function SelectNodeDummy(const aXPath: OWideString): PXMLNode;
    //select the first node (element/attribute) by name (not XPath!!!),
    //  if not found the element is created, appended to current node and returned
    //  - use "NodeName" to find node, use "@AttrName" to find attribute
    function SelectNodeCreate(const aNodeName: OWideString): PXMLNode;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return a aNodeList with no items (count = 0)
    function SelectNodes(const aXPath: OWideString;
      var outNodeList: IXMLNodeList;
      const aMaxNodeCount: Integer = 0): Boolean; overload;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return a list with no items (count = 0)
    function SelectNodes(const aXPath: OWideString;
      const aMaxNodeCount: Integer = 0): IXMLNodeList; overload;
  public
    //load document with custom reader
    //  outReaderToken -> the token enumerator
    //  aBreakWhenRefNodeReached -> break reading when reference node is reached (used in sequential parser)
    //return false if the XML document is invalid
    function LoadFromReader(const aReader: TXMLReader;
      var outReaderToken: PXMLReaderToken;
      const aBreakWhenRefNodeReached: PXMLNode = nil): Boolean;
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromFile(const aFileName: String; const aForceEncoding: TEncoding = nil): Boolean;
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //loads XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFDEF O_RAWBYTESTRING}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_GENERICBYTES}
    //load document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    //save document with custom writer
    procedure SaveToWriter(const aWriter: TXMLWriter);
    //save document to file in encoding specified by the document
    procedure SaveToFile(const aFileName: String);
    //save document to stream in encoding specified by the document
    procedure SaveToStream(const aStream: TStream);
    //returns XML as string
    procedure SaveToXML(var outXML: OWideString); overload;
    procedure SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType); overload;
    {$IFDEF O_RAWBYTESTRING}
    procedure SaveToXML_UTF8(var outXML: ORawByteString); overload;
    procedure SaveToXML_UTF8(var outXML: ORawByteString; const aIndentType: TXMLIndentType); overload;
    {$ENDIF}

    {$IFDEF O_GENERICBYTES}
    //returns XML as a buffer in encoding specified by the document
    procedure SaveToBuffer(var outBuffer: TBytes);
    {$ENDIF}
  public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML: OWideString; overload;
    function XML(const aIndentType: TXMLIndentType): OWideString; overload;
    {$IFDEF O_RAWBYTESTRING}
    function XML_UTF8: ORawByteString; overload;
    function XML_UTF8(const aIndentType: TXMLIndentType): ORawByteString; overload;
    {$ENDIF}
  public
    property NodeType: TXmlNodeType read fNodeType;
    property NodeName: OWideString read GetNodeName;
    property NodeValue: OWideString read GetNodeValue write SetNodeValue;
    property Text: OWideString read GetText write SetText;
    property PreserveWhiteSpace: TXMLPreserveWhiteSpace read fPreserveWhiteSpace write fPreserveWhiteSpace;

    property ParentNode: PXMLNode read fParentNode;
    property OwnerDocument: TXMLDocument read fOwnerDocument;

    property FirstChild: PXMLNode index ctChild read GetFirstCChild;
    property LastChild: PXMLNode index ctChild read GetLastCChild;
    property ChildFromBegin[const aIndex: Integer]: PXMLNode index ctChild read GetCChildFromBegin;
    property ChildFromEnd[const aIndex: Integer]: PXMLNode index ctChild read GetCChildFromEnd;
    property FirstAttribute: PXMLNode index ctAttribute read GetFirstCChild;
    property LastAttribute: PXMLNode index ctAttribute read GetLastCChild;
    property AttributeFromBegin[const aIndex: Integer]: PXMLNode index ctAttribute read GetCChildFromBegin;
    property AttributeFromEnd[const aIndex: Integer]: PXMLNode index ctAttribute read GetCChildFromEnd;

    property IsTextElement: Boolean read GetIsTextElement;

    property NextSibling: PXMLNode read fNextSibling;
    property PreviousSibling: PXMLNode read fPreviousSibling;

    //Although you may use the Attributes[] list, it's faster when you use GetAttribute and SetAttribute directly!
    property Attributes[const aName: OWideString]: OWideString read GetAttribute write _SetAttribute;
  end;
  TXMLNodeArray = Array of TXMLNode;
  PXMLNodeArray = ^TXMLNodeArray;

  IXMLDocument = interface
    ['{490301A3-C95B-4E03-B09D-99E4682BC3FE}']

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
    function GetWriterSettings: TXMLWriterSettings;
    function GetReaderSettings: TXMLReaderSettings;

    property Loading: Boolean read GetLoading write SetLoading;

  //protected
    function GetDummyNode: PXMLNode;
    function GetDummyResNodeList: IXMLNodeList;
    function GetDocumentNode: PXMLNode;
    function GetDocumentElement: PXMLNode;
    procedure SetDocumentElement(const aDocumentElement: PXMLNode);

    property DummyNode: PXMLNode read GetDummyNode;
    property DummyResNodeList: IXMLNodeList read GetDummyResNodeList;

  //public
    //clear the whole document
    procedure Clear(const aFullClear: Boolean = True);

    //load document with custom reader
    //  return false if the XML document is invalid
    function LoadFromReader(const aReader: TXMLReader; var outReaderToken: PXMLReaderToken): Boolean;
    //load document from file in encoding specified by the document
    function LoadFromFile(const aFileName: String; const aForceEncoding: TEncoding = nil): Boolean;
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //loads XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFDEF O_RAWBYTESTRING}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_GENERICBYTES}
    //load document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    //save document with custom writer
    procedure SaveToWriter(const aWriter: TXMLWriter);
    //save document to file in encoding specified by the document
    procedure SaveToFile(const aFileName: String);
    //save document to stream in encoding specified by the document
    procedure SaveToStream(const aStream: TStream);
    //returns XML as string
    procedure SaveToXML(var outXML: OWideString); overload;
    procedure SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType); overload;
    {$IFDEF O_RAWBYTESTRING}
    procedure SaveToXML_UTF8(var outXML: ORawByteString); overload;
    procedure SaveToXML_UTF8(var outXML: ORawByteString; const aIndentType: TXMLIndentType); overload;
    {$ENDIF}

    {$IFDEF O_GENERICBYTES}
    //returns XML as a buffer in encoding specified by the document
    procedure SaveToBuffer(var outBuffer: TBytes);
    {$ENDIF}

  //public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML: OWideString; overload;
    function XML(const aIndentType: TXMLIndentType): OWideString; overload;
    {$IFDEF O_RAWBYTESTRING}
    function XML_UTF8: ORawByteString; overload;
    function XML_UTF8(const aIndentType: TXMLIndentType): ORawByteString; overload;
    {$ENDIF}

  //public

    //document whitespace handling
    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;

    //document encoding (as integer identifier) - from <?xml encoding="???"?>
    property CodePage: Word read GetCodePage write SetCodePage;
    //document encoding (as string alias) - from <?xml encoding="???"?>
    property Encoding: OWideString read GetEncoding write SetEncoding;
    //document standalone - from <?xml standalone="???"?>
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    //document version - from <?xml version="???"?>
    property Version: OWideString read GetVersion write SetVersion;

    //XML writer settings
    property WriterSettings: TXMLWriterSettings read GetWriterSettings;
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //ParseError has information about the error that occured when parsing a document
    function GetParseError: IXMLParseError;
    property ParseError: IXMLParseError read GetParseError;
  //public
    //attribute aName="aValue"
    function CreateAttribute(const aName: OWideString; const aValue: OWideString = ''): PXMLNode;
    //element <aNodeName />
    function CreateElement(const aNodeName: OWideString): PXMLNode;
    //xml declaration <?xml ?>
    function CreateXMLDeclaration: PXMLNode;
    //text
    function CreateTextNode(const aText: OWideString): PXMLNode;
    //cdata <![CDATA[aText]]>
    function CreateCDATASection(const aData: OWideString): PXMLNode;
    //comment <!--aText-->
    function CreateComment(const aText: OWideString): PXMLNode;
    //doctype <!DOCTYPE aDocTypeRawText>
    function CreateDocType(const aDocTypeRawText: OWideString): PXMLNode;
    //custom PI <?aTarget aContent?>
    function CreateProcessingInstruction(const aTarget, aContent: OWideString): PXMLNode;

    //create and append an element child to Self.Node (document node)
    function AddChild(const aElementName: OWideString): PXMLNode;

  //public

    //returns the very document node (parent of the DocumentElement)
    property Node: PXMLNode read GetDocumentNode;
    //returns the root node (first element in the document)
    property DocumentElement: PXMLNode read GetDocumentElement write SetDocumentElement;
  end;

  TXMLDocument = class(TInterfacedObject, IXMLDocument)
  private
    fLoading: Boolean;
    fURL: String;

    fDictionary: TOHashedStrings;

    {$IFDEF O_GENERICS}
    fNodes: TList<PXMLNodeArray>;//Memory blocks of 1024 elements. Do not reallocate its memory!
    fFreeNodes: TDictionary<PXMLNode,Boolean>;
    fTempChildNodes: Array[TXMLChildType] of TObjectDictionary<PXMLNode,TXMLChildNodeList>;
    {$ELSE}
    fNodes: TList;
    fFreeNodes: TODictionary;
    fTempChildNodes: Array[TXMLChildType] of TODictionary;
    {$ENDIF}

    fNextNodeId: XMLNodeId;//next NodeId to use
    fNodesLength: XMLNodeId;//= "Count(fNodes)*1024" - count of allocated nodes
    fBlankDocumentNode: PXMLNode;//the blank document element
    fDummyNode: PXMLNode;
    fDummyNodeList: IXMLNodeList;
    fWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    fWriterSettings: TXMLWriterSettings;
    fReaderSettings: TXMLReaderSettings;
    fParseError: IXMLParseError;

    function FindXMLDeclarationNode(var outXMLDeclarationNode: PXMLNode): Boolean;
    function GetXMLDeclarationAttribute(const aAttributeName: OWideString): OWideString;
    procedure SetXMLDeclarationAttribute(const aAttributeName, aAttributeValue: OWideString);
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
    function GetLoading: Boolean;
    procedure SetLoading(const aLoading: Boolean);
    function GetDummyNode: PXMLNode;
    function GetDummyResNodeList: IXMLNodeList;
    function GetDocumentNode: PXMLNode;//absolute root element (= empty document)
    function GetDocumentElement: PXMLNode;//first element in document (=root)
    procedure SetDocumentElement(const aDocumentElement: PXMLNode);
    function GetWriterSettings: TXMLWriterSettings;
    function GetReaderSettings: TXMLReaderSettings;
    function GetParseError: IXMLParseError;
  protected
    procedure FreeNode(const aNode: PXMLNode); virtual;
    procedure ClearNodes(const aFullClear: Boolean); virtual;

    procedure CreateNode(const aNodeType: TXmlNodeType;
      const aNodeName, aNodeValue: OWideString; var outNode: PXMLNode); overload;
    procedure CreateNode(const aNodeType: TXmlNodeType;
      const aNodeNameId, aNodeValueId: OHashedStringsIndex; var outNode: PXMLNode); overload;
    function GetNode(const aNodeId: XMLNodeId): PXMLNode;
    function GetString(const aStringId: OHashedStringsIndex): OWideString;
    function SetString(const aString: OWideString): OHashedStringsIndex;

    procedure Grow;

    function GetCreateTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType): TXMLChildNodeList;
    function TryGetTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType; var outList: TXMLChildNodeList): Boolean;
    procedure DestroyTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType);
    procedure ClearTempChildNodeLists(const aChildType: TXMLChildType);

    property Loading: Boolean read GetLoading write SetLoading;
    property DummyNode: PXMLNode read GetDummyNode;
    property DummyResNodeList: IXMLNodeList read GetDummyResNodeList;

    procedure DoCreate; virtual;
  public
    function CreateAttribute(const aName, aValue: OWideString): PXMLNode;
    function CreateElement(const aNodeName: OWideString): PXMLNode;
    function CreateXMLDeclaration: PXMLNode;
    function CreateTextNode(const aText: OWideString): PXMLNode;
    function CreateCDATASection(const aData: OWideString): PXMLNode;
    function CreateComment(const aText: OWideString): PXMLNode;
    function CreateDocType(const aDocTypeRawText: OWideString): PXMLNode;
    function CreateProcessingInstruction(const aTarget, aContent: OWideString): PXMLNode;

    function AddChild(const aElementName: OWideString): PXMLNode;
  public
    constructor Create({%H-}aDummy: TObject); overload;//aDummy to ge ignored - Delphi XML compatibility
    constructor Create(const aRootNodeName: OWideString = ''; const aAddUTF8Declaration: Boolean = False); overload;
    destructor Destroy; override;
  public
    procedure Clear(const aFullClear: Boolean = True);

  public
    function LoadFromReader(const aReader: TXMLReader; var outReaderToken: PXMLReaderToken): Boolean;
    function LoadFromFile(const aFileName: String; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFDEF O_RAWBYTESTRING}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_GENERICBYTES}
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    procedure SaveToWriter(const aWriter: TXMLWriter);
    procedure SaveToFile(const aFileName: String);
    procedure SaveToStream(const aStream: TStream);

    {$IFDEF O_GENERICBYTES}
    procedure SaveToBuffer(var outBuffer: TBytes);
    {$ENDIF}
    procedure SaveToXML(var outXML: OWideString); overload;
    procedure SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType); overload;
    {$IFDEF O_RAWBYTESTRING}
    procedure SaveToXML_UTF8(var outXML: ORawByteString); overload;
    procedure SaveToXML_UTF8(var outXML: ORawByteString; const aIndentType: TXMLIndentType); overload;
    {$ENDIF}
  public
    function XML: OWideString; overload;
    function XML(const aIndentType: TXMLIndentType): OWideString; overload;
    {$IFDEF O_RAWBYTESTRING}
    function XML_UTF8: ORawByteString; overload;
    function XML_UTF8(const aIndentType: TXMLIndentType): ORawByteString; overload;
    {$ENDIF}
  public
    property Node: PXMLNode read fBlankDocumentNode;//GetDocumentNode; performance
    property DocumentElement: PXMLNode read GetDocumentElement write SetDocumentElement;

    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read fWhiteSpaceHandling write fWhiteSpaceHandling;//Get/Set; performance

    property CodePage: Word read GetCodePage write SetCodePage;
    property Encoding: OWideString read GetEncoding write SetEncoding;
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    property Version: OWideString read GetVersion write SetVersion;

    property WriterSettings: TXMLWriterSettings read fWriterSettings;
    property ReaderSettings: TXMLReaderSettings read fReaderSettings;

    property ParseError: IXMLParseError read GetParseError;
  end;

  TXMLResNodeListEnumerator = class;
  IXMLNodeList = interface
    ['{9FD530D4-B35E-467E-916A-07B5E3D83AC6}']

    //protected
    function GetCount: Integer;
    procedure ExtNodeAppended;
    procedure ExtNodeInserted;
    procedure ExtNodeRemoved;

    //public
    function Add(const aNode: PXMLNode): Integer;
    function AddNode(const aNode: PXMLNode): PXMLNode;
    procedure Clear;
    procedure Delete(const aNode: PXMLNode); overload;
    procedure Delete(const aName: OWideString); overload;
    procedure Delete(const aIndex: Integer); overload;
    function Get(const aIndex: Integer): PXMLNode;

    function FindNode(const aName: OWideString): PXMLNode;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var outNode: PXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Remove(const aNode: PXMLNode): Integer;

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var ioNode: PXMLNode): Boolean;
    function GetPrevious(var ioNode: PXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLResNodeListEnumerator;
    {$ENDIF}

    property Nodes[const aIndex: Integer]: PXMLNode read Get; default;
    property Count: Integer read GetCount;
  end;

  TXMLChildNodeListEnumerator = class(TObject)
  private
    fList: TXMLChildNodeList;
    fCurrent: PXMLNode;
  public
    constructor Create(aList: TXMLChildNodeList);
    function GetCurrent: PXMLNode;
    function MoveNext: Boolean;
  public
    property Current: PXMLNode read GetCurrent;
  end;

  TXMLChildNodeList = class(TObject)
  private
    fParent: PXMLNode;
    fChildType: TXMLChildType;

    fLastGetNodeIndex: Integer;
    fLastGetNode: PXMLNode;
    fTempCount: Integer;
  protected
    function GetCount: Integer;

    procedure ClearTempVariables;

    procedure ExtNodeAppended;
    procedure ExtNodeInserted;
    procedure ExtNodeRemoved;
  public
    constructor Create(const aParent: PXMLNode; const aChildType: TXMLChildType);
  public
    function Add(const aNode: PXMLNode): Integer;
    function AddNode(const aNode: PXMLNode): PXMLNode;
    procedure Clear;
    procedure Delete(const aNode: PXMLNode); overload;//important: the node gets automatically destroyed in all delete procedures!
    procedure Delete(const aName: OWideString); overload;
    procedure Delete(const aIndex: Integer); overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Get(const aIndex: Integer): PXMLNode;

    function FindNode(const aName: OWideString): PXMLNode;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var outNode: PXMLNode): Integer; overload;
    function Remove(const aNode: PXMLNode): Integer; overload;//important: node gets automatically destroyed
    function Remove(const aName: OWideString): Integer; overload;//important: node gets automatically destroyed
    function Remove(const aName: OWideString; var outNode: PXMLNode): Integer; overload;//important: node DOES NOT GET automatically destroyed

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var ioNode: PXMLNode): Boolean;
    function GetPrevious(var ioNode: PXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLChildNodeListEnumerator;
    {$ENDIF}

    property Nodes[const aIndex: Integer]: PXMLNode read Get; default;
    property Count: Integer read GetCount;
  end;

  TXMLResNodeListEnumerator = class(TObject)
  private
    fList: IXMLNodeList;
    fIndex: Integer;
  public
    constructor Create(aList: IXMLNodeList);
    function GetCurrent: PXMLNode;
    function MoveNext: Boolean;
  public
    property Current: PXMLNode read GetCurrent;
  end;

  TXMLResNodeList = class(TInterfacedObject, IXMLNodeList)
  private
    {$IFDEF O_GENERICS}
    fList: TList<PXMLNode>;
    {$ELSE}
    fList: TList;
    {$ENDIF}
    fIteratorCurrent: Integer;//for fast Next & Prev

    function GetPrevNext(var ioNodeEnum: PXMLNode; const aInc: Integer): Boolean;
  protected
    function GetCount: Integer;
    procedure ExtNodeAppended;
    procedure ExtNodeInserted;
    procedure ExtNodeRemoved;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(const aNode: PXMLNode): Integer;
    function AddNode(const aNode: PXMLNode): PXMLNode;
    procedure Clear;
    procedure Delete(const aNode: PXMLNode); overload;
    procedure Delete(const aName: OWideString); overload;
    procedure Delete(const aIndex: Integer); overload;
    function Get(const aIndex: Integer): PXMLNode;

    function FindNode(const aName: OWideString): PXMLNode;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var outNode: PXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Remove(const aNode: PXMLNode): Integer;

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var ioNodeEnum: PXMLNode): Boolean;
    function GetPrevious(var ioNodeEnum: PXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLResNodeListEnumerator;
    {$ENDIF}

    property Nodes[const aIndex: Integer]: PXMLNode read Get; default;
    property Count: Integer read GetCount;
  end;

  TXMLXPathDOMAdapter = class(TXMLXPathAdapter)
  private
    fResNodeList: IXMLNodeList;
  public
    procedure BuildIdTree(const aStartWithNode: TXMLXPathNode; const aLevelsDeep: Integer;
      const aIdTree: TXMLXPathIdTree); override;
    function CreateResNodeList: TXMLXPathNodeList; override;
    procedure AddNodeToResList(const aNode: TXMLXPathNode); override;
    function GetNodeName(const aNode: TXMLXPathNode): OWideString; override;
    function GetNodeValue(const aNode: TXMLXPathNode): OWideString; override;
    function GetNodeType(const aNode: TXMLXPathNode): TXmlNodeType; override;
    procedure GetNodeInfo(const aNode: TXMLXPathNode; var outNodeInfo: TXMLXPathNodeInfo); override;
    function NodeHasAttributes(const aNode: TXMLXPathNode): Boolean; override;
    function NodeFindAttribute(const aNode: TXMLXPathNode; const aAttrName: OWideString): TXMLXPathNode; override;
    procedure GetNodeAttributes(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); override;
    function GetNodeParent(const aNode: TXMLXPathNode): TXMLXPathNode; override;
    function GetNodeDOMDocument(const aNode: TXMLXPathNode): TXMLXPathNode; override;
    function NodeHasChildNodes(const aNode: TXMLXPathNode): Boolean; override;
    procedure GetNodeChildren(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); override;
  end;


function CreateXMLDoc: IXMLDocument; overload;
function CreateXMLDoc(const aRootNodeName: OWideString): IXMLDocument; overload;
function CreateXMLDoc(const aRootNodeName: OWideString; const aAddUTF8Declaration: Boolean): IXMLDocument; overload;

implementation

uses OXmlLng;

function CreateXMLDoc: IXMLDocument;
begin
  Result := TXMLDocument.Create;
end;

function CreateXMLDoc(const aRootNodeName: OWideString): IXMLDocument;
begin
  Result := TXMLDocument.Create(aRootNodeName);
end;

function CreateXMLDoc(const aRootNodeName: OWideString; const aAddUTF8Declaration: Boolean): IXMLDocument;
begin
  Result := TXMLDocument.Create(aRootNodeName, aAddUTF8Declaration);
end;

{ TXMLNode }

function TXMLNode.AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
begin
  Result := _AddAttribute(aAttrName, aAttrValue);
end;

function TXMLNode.AddCDATASection(const aText: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntCData, '', aText);
end;

function TXMLNode.AddComment(const aText: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntComment, '', aText);
end;

function TXMLNode.AddChild(const aElementName: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntElement, aElementName, '');
end;

function TXMLNode.AddCustomChild(const aType: TXmlNodeType; const aName,
  aValue: OWideString): PXMLNode;
begin
  fOwnerDocument.CreateNode(aType, aName, aValue, {%H-}Result);
  Append(Result, ctChild);
end;

function TXMLNode.AddXMLDeclaration: PXMLNode;
begin
  Result := AddCustomChild(ntXMLDeclaration, 'xml', '');
end;

function TXMLNode.AddDocType(const aDocTypeRawText: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntDocType, '', aDocTypeRawText);
end;

function TXMLNode.AddProcessingInstruction(const aTarget,
  aContent: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntProcessingInstruction, aTarget, aContent);
end;

function TXMLNode.AddText(const aText: OWideString): PXMLNode;
var
  xText: OWideString;
begin
  if OwnerDocument.Loading then begin
    //document is reading XML
    if (OwnerDocument.WhiteSpaceHandling = wsPreserveInTextOnly) and OXmlIsWhiteSpace(aText) then begin
      xText := '';
    end else if
      (OwnerDocument.WhiteSpaceHandling = wsTrim) or
      ((OwnerDocument.WhiteSpaceHandling = wsAutoTag) and not GetDoPreserveWhiteSpace)
    then begin
      xText := Trim(aText);
    end else begin
      xText := aText;
    end;
  end else begin
    //programatically creating document
    if (OwnerDocument.WhiteSpaceHandling = wsPreserveInTextOnly) and OXmlIsWhiteSpace(aText) then begin
      xText := '';
    end else if (OwnerDocument.WhiteSpaceHandling = wsTrim) then begin
      xText := Trim(aText);
    end else begin
      xText := aText;

      if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and
        OXmlNeedsPreserveAttribute(xText) and
        (not Self.GetDoPreserveWhiteSpace)
      then
        Self.fPreserveWhiteSpace := pwPreserve;
    end;
  end;

  if xText <> '' then
    Result := AddCustomChild(ntText, '', xText)
  else
    Result := nil;
end;

procedure TXMLNode.Append(const aNew: PXMLNode; const aChildType: TXMLChildType);
var
  xLastChild: PXMLNode;
  xList: TXMLChildNodeList;
begin
  if TryGetChildNodes({%H-}xList, aChildType) then
    xList.ExtNodeAppended;

  if Assigned(fFirstCChild[aChildType]) then begin
    //append to the end

    xLastChild := fLastCChild[aChildType];
    //set new as next sibling of last child
    xLastChild.fNextSibling := aNew;
    //set last id to new
    fLastCChild[aChildType] := aNew;
    //set prev sibling of new child to last
    aNew.fPreviousSibling := xLastChild;
  end else begin
    //no children

    fFirstCChild[aChildType] := aNew;
    fLastCChild[aChildType] := aNew;
  end;
  aNew.fParentNode := @Self;
end;

function TXMLNode.AppendChild(const aNewChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  if Assigned(aNewChild.fParentNode) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  Append(aNewChild, ctChild);
  Result := aNewChild;
end;

procedure TXMLNode.AssignProperties(const aFromNode: PXMLNode);
begin
  Self.fPreserveWhiteSpace := aFromNode.fPreserveWhiteSpace;
end;

function TXMLNode.CloneNode(const aDeep: Boolean): PXMLNode;
var
  xIter, xNewNode: PXMLNode;
begin
  fOwnerDocument.CreateNode(fNodeType, fNodeNameId, fNodeValueId, {%H-}Result);
  Result.AssignProperties(@Self);

  xIter := Self.FirstAttribute;
  while Assigned(xIter) do
  begin
    fOwnerDocument.CreateNode(xIter.fNodeType, xIter.fNodeNameId,
      xIter.fNodeValueId, {%H-}xNewNode);
    xNewNode.AssignProperties(xIter);

    Result.Append(xNewNode, ctAttribute);
    xIter := xIter.NextSibling;
  end;

  if aDeep then
  begin
    xIter := Self.FirstChild;
    while Assigned(xIter) do
    begin
      xNewNode := xIter.CloneNode(aDeep);
      Result.Append(xNewNode, ctChild);
      xIter := xIter.NextSibling;
    end;
  end;
end;

procedure TXMLNode.DeleteAttribute(const aAttr: PXMLNode);
begin
  aAttr.DeleteSelf;
end;

procedure TXMLNode.DeleteAttributes(const aDestroyList: Boolean);
begin
  DeleteCChildren(aDestroyList, ctAttribute);
end;

procedure TXMLNode.DeleteChildren;
begin
  DeleteCChildren(aDestroyList, ctChild);
end;

procedure TXMLNode.DeleteSelf;
begin
  RemoveSelfFromParent;

  OwnerDocument.FreeNode(@Self);
end;

procedure TXMLNode.DeleteAttribute(const aName: OWideString);
var
  xAttr: PXMLNode;
begin
  if not FindAttribute(aName, {%H-}xAttr) then
    Exit;

  DeleteAttribute(xAttr);
end;

procedure TXMLNode.DeleteCChildren(const aDestroyList: Boolean;
  const aChildType: TXMLChildType);
var
  xChild, xNextChild: PXMLNode;
begin
  xChild := fFirstCChild[aChildType];
  while Assigned(xChild) do
  begin
    xNextChild := xChild.fNextSibling;
    OwnerDocument.FreeNode(xChild);
    xChild := xNextChild;
  end;

  if aDestroyList then
    OwnerDocument.DestroyTempChildNodeList(@Self, aChildType);

  fFirstCChild[aChildType] := nil;
  fLastCChild[aChildType] := nil;
end;

procedure TXMLNode.DeleteChild(const aChild: PXMLNode);
begin
  aChild.DeleteSelf;
end;

function TXMLNode.FindAttribute(const aName: OWideString;
  var outAttr: PXMLNode): Boolean;
var
  xNameId: OHashedStringsIndex;
  xAttr: PXMLNode;
begin
  if not HasAttributes then
  begin
    Result := False;
    Exit;
  end;

  xNameId := fOwnerDocument.fDictionary.IndexOf(aName);
  if xNameId < 0 then
  begin
    Result := False;
    Exit;
  end;

  xAttr := fFirstCChild[ctAttribute];
  while Assigned(xAttr) do
  begin
    if xAttr.fNodeNameId = xNameId then
    begin
      outAttr := xAttr;
      Result := True;
      Exit;
    end;
    xAttr := xAttr.fNextSibling;
  end;
  Result := False;
  outAttr := nil;
end;

function TXMLNode.FindAttribute(const aName: OWideString;
  var outValue: OWideString): Boolean;
var
  xAttr: PXMLNode;
begin
  Result := FindAttribute(aName, {%H-}xAttr);
  if Result then
    outValue := xAttr.NodeValue
  else
    outValue := '';
end;

function TXMLNode.GetAttribute(const aName: OWideString): OWideString;
begin
  Result := GetAttributeDef(aName, '');
end;

function TXMLNode.GetAttributeCount: Integer;
var
  xList: TXMLChildNodeList;
  xAttr: PXMLNode;
begin
  if TryGetChildNodes({%H-}xList, ctAttribute) then begin
    Result := xList.Count;
    Exit;
  end;

  Result := 0;
  xAttr := fFirstCChild[ctAttribute];
  while Assigned(xAttr) do
  begin
    Inc(Result);
    xAttr := xAttr.NextSibling;
  end;
end;

function TXMLNode.GetAttributeDef(const aName,
  aDefaultValue: OWideString): OWideString;
begin
  if not FindAttribute(aName, {%H-}Result) then
    Result := aDefaultValue;
end;

function TXMLNode.GetAttributeNode(const aAttrName: OWideString): PXMLNode;
begin
  if not FindAttribute(aAttrName, {%H-}Result) then
    Result := nil;
end;

function TXMLNode.GetAttributeNodes: TXMLChildNodeList;
begin
  case fNodeType of
    ntXMLDeclaration, ntElement:
      Result := OwnerDocument.GetCreateTempChildNodeList(@Self, ctAttribute);
  else
    Result := nil;
  end;
end;

function TXMLNode.GetChildCount: Integer;
var
  xList: TXMLChildNodeList;
  xChild: PXMLNode;
begin
  if TryGetChildNodes({%H-}xList, ctAttribute) then begin
    Result := xList.Count;
    Exit;
  end;

  Result := 0;
  xChild := fFirstCChild[ctChild];
  while Assigned(xChild) do
  begin
    Inc(Result);
    xChild := xChild.fNextSibling;
  end;
end;

function TXMLNode.GetCChildFromBegin(const aIndex: Integer;
  const aChildType: TXMLChildType): PXMLNode;
var
  I: Integer;
begin
  if aIndex < 0 then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  //search forwards through all nodes
  I := -1;
  while (I < aIndex) and GetNextCChild({%H-}Result, aChildType) do
    Inc(I);

  if (I <> aIndex) or not Assigned(Result) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);
end;

function TXMLNode.GetCChildFromEnd(const aIndex: Integer;
  const aChildType: TXMLChildType): PXMLNode;
var
  I: Integer;
begin
  if aIndex < 0 then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  //search forwards through all nodes
  I := -1;
  while (I < aIndex) and GetPreviousCChild({%H-}Result, aChildType) do
    Inc(I);

  if (I <> aIndex) or not Assigned(Result) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);
end;

function TXMLNode.GetChildNodes: TXMLChildNodeList;
begin
  Result := OwnerDocument.GetCreateTempChildNodeList(@Self, ctChild);
end;

function TXMLNode.GetDoPreserveWhiteSpace: Boolean;
begin
  case fPreserveWhiteSpace of
    pwPreserve: Result := True;
    pwDefault: Result := False;
  else
    //pwInherit
    if Assigned(fParentNode) then
      Result := fParentNode.GetDoPreserveWhiteSpace
    else
      Result := False;
  end;
end;

function TXMLNode.TryGetChildNodes(var outList: TXMLChildNodeList;
  const aChildType: TXMLChildType): Boolean;
begin
  Result := OwnerDocument.TryGetTempChildNodeList(@Self, aChildType, outList);
end;

function TXMLNode.GetFirstCChild(const aChildType: TXMLChildType): PXMLNode;
begin
  Result := fFirstCChild[aChildType];
end;

function TXMLNode.GetIsTextElement: Boolean;
begin
  Result := (NodeType = ntElement) and
    Assigned(fFirstCChild[ctChild]) and
    (fFirstCChild[ctChild] = fLastCChild[ctChild]) and
    (fFirstCChild[ctChild].NodeType = ntText);
end;

function TXMLNode.GetLastCChild(const aChildType: TXMLChildType): PXMLNode;
begin
  Result := fLastCChild[aChildType];
end;

function TXMLNode.GetNodeName: OWideString;
begin
  Result := fOwnerDocument.GetString(fNodeNameId);
end;

function TXMLNode.GetNextAttribute(var ioAttrEnum: PXMLNode): Boolean;
begin
  Result := GetNextCChild(ioAttrEnum, ctAttribute);
end;

function TXMLNode.GetNextCChild(var ioChildEnum: PXMLNode;
  const aChildType: TXMLChildType): Boolean;
begin
  if Assigned(ioChildEnum) then
    ioChildEnum := ioChildEnum.fNextSibling
  else
    ioChildEnum := GetFirstCChild(aChildType);

  Result := Assigned(ioChildEnum);
end;

function TXMLNode.GetNextChild(var ioChildEnum: PXMLNode): Boolean;
begin
  Result := GetNextCChild(ioChildEnum, ctChild);
end;

function TXMLNode.GetPreviousAttribute(var ioAttrEnum: PXMLNode): Boolean;
begin
  Result := GetPreviousCChild(ioAttrEnum, ctAttribute);
end;

function TXMLNode.GetPreviousCChild(var ioChildEnum: PXMLNode;
  const aChildType: TXMLChildType): Boolean;
begin
  if Assigned(ioChildEnum) then
    ioChildEnum := ioChildEnum.fPreviousSibling
  else
    ioChildEnum := fLastCChild[aChildType];

  Result := Assigned(ioChildEnum);
end;

function TXMLNode.GetPreviousChild(var ioChildEnum: PXMLNode): Boolean;
begin
  Result := GetPreviousCChild(ioChildEnum, ctChild);
end;

function TXMLNode.GetText: OWideString;
var
  xChild: PXMLNode;
begin
  Result := '';
  case NodeType of
    ntText, ntCData: Result := NodeValue;
    ntDocument, ntElement:
    begin
      xChild := fFirstCChild[ctChild];
      while Assigned(xChild) do
      begin
        Result := Result + xChild.Text;
        xChild := xChild.fNextSibling;
      end;
    end;
  end
end;

function TXMLNode.XML: OWideString;
begin
  SaveToXML({%H-}Result);
end;

function TXMLNode.XML(const aIndentType: TXMLIndentType): OWideString;
begin
  SaveToXML({%H-}Result, aIndentType);
end;

{$IFDEF O_RAWBYTESTRING}
function TXMLNode.XML_UTF8: ORawByteString;
begin
  SaveToXML_UTF8({%H-}Result);
end;

function TXMLNode.XML_UTF8(const aIndentType: TXMLIndentType): ORawByteString;
begin
  SaveToXML_UTF8({%H-}Result, aIndentType);
end;
{$ENDIF}

function TXMLNode._AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and SameText(aAttrName, 'xml:space') then begin
    Self.fPreserveWhiteSpace := OXmlStrToPreserve(aAttrValue);
  end else begin
    if FindAttribute(aAttrName, Result) then begin
      Result.SetNodeValue(aAttrValue);
    end else begin
      fOwnerDocument.CreateNode(ntAttribute, aAttrName, aAttrValue, Result);
      Append(Result, ctAttribute);
    end;
  end;
end;

procedure TXMLNode._SetAttribute(const aAttrName, aAttrValue: OWideString);
begin
  _AddAttribute(aAttrName, aAttrValue);
end;

function TXMLNode.GetNodeValue: OWideString;
  procedure _GetXMLDeclarationData;
  begin
    Self.SaveToXML(Result);
    Result := Copy(Result, 7, Length(Result)-8);//extract content from "<?xml content?>"
  end;

begin
  case fNodeType of
    ntXMLDeclaration: _GetXMLDeclarationData;
  else
    Result := fOwnerDocument.GetString(fNodeValueId);
  end;
end;

function TXMLNode.HasAttribute(const aName: OWideString): Boolean;
var
  x: PXMLNode;
begin
  Result := FindAttribute(aName, {%H-}x);
end;

function TXMLNode.HasAttributes: Boolean;
begin
  Result := Assigned(fFirstCChild[ctAttribute]);
end;

function TXMLNode.HasChildNodes: Boolean;
begin
  Result := Assigned(fFirstCChild[ctChild]);
end;

procedure TXMLNode.Init(const aNodeType: TXmlNodeType;
  const aNodeNameId, aNodeValueId: OHashedStringsIndex;
  const aOwnerDocument: TXMLDocument);
begin
  fOwnerDocument := aOwnerDocument;
  fNodeType := aNodeType;
  fNodeNameId := aNodeNameId;
  fNodeValueId := aNodeValueId;

  fParentNode := nil;
  fFirstCChild[ctChild] := nil;
  fLastCChild[ctChild] := nil;
  fFirstCChild[ctAttribute] := nil;
  fLastCChild[ctAttribute] := nil;
  fNextSibling := nil;
  fPreviousSibling := nil;
  fPreserveWhiteSpace := pwInherit;
end;

procedure TXMLNode.Insert(const aNew, aBeforeNode: PXMLNode;
  const aChildType: TXMLChildType);
var
  xAfterNode: PXMLNode;
  xList: TXMLChildNodeList;
begin
  if aBeforeNode.fParentNode <> @Self then
    raise EXmlDOMException.Create(OXmlLng_NodeToInsertNotAChild);

  if TryGetChildNodes({%H-}xList, aChildType) then
    xList.ExtNodeInserted;

  xAfterNode := aBeforeNode.PreviousSibling;
  if Assigned(xAfterNode) then begin
    xAfterNode.fNextSibling := aNew;
    aNew.fPreviousSibling := xAfterNode;
  end else begin
    aNew.fPreviousSibling := nil;
  end;

  if fFirstCChild[aChildType] = aBeforeNode then
    fFirstCChild[aChildType] := aNew;
  aBeforeNode.fPreviousSibling := aNew;
  aNew.fNextSibling := aBeforeNode;

  aNew.fParentNode := @Self;
end;

function TXMLNode.InsertAttribute(const aAttrName, aAttrValue: OWideString;
  const aBeforeAttribute: PXMLNode): PXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and SameText(aAttrName, 'xml:space') then begin
    Self.fPreserveWhiteSpace := OXmlStrToPreserve(aAttrValue);
  end else begin
    DeleteAttribute(aAttrName);

    fOwnerDocument.CreateNode(ntAttribute, aAttrName, aAttrValue, Result);

    Insert(Result, aBeforeAttribute, ctAttribute);
  end;
end;

function TXMLNode.InsertAttribute(const aAttrName, aAttrValue: OWideString;
  const aBeforeAttributeName: OWideString): PXMLNode;
var
  xBeforeAttr: PXMLNode;
begin
  if FindAttribute(aBeforeAttributeName, {%H-}xBeforeAttr) then
    Result := InsertAttribute(aAttrName, aAttrValue, xBeforeAttr)
  else
    Result := AddAttribute(aAttrName, aAttrValue);
end;

function TXMLNode.InsertBefore(const aNewChild, aRefChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  if (aNewChild = aRefChild) then
    raise EXmlDOMException.Create(OXmlLng_InsertEqualNodes);

  if Assigned(aNewChild.fParentNode) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  Result := aNewChild;
  if Assigned(aRefChild) then
    Insert(Result, aRefChild, ctChild)
  else
    Append(Result, ctChild);
end;

function TXMLNode.InsertCDATASection(const aText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntCData, '', aText, aBeforeNode);
end;

function TXMLNode.InsertCustomChild(const aType: TXmlNodeType; const aName,
  aValue: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
begin
  fOwnerDocument.CreateNode(aType, aName, aValue, {%H-}Result);
  Insert(Result, aBeforeNode, ctChild);
end;

function TXMLNode.InsertComment(const aText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntComment, '', aText, aBeforeNode);
end;

function TXMLNode.InsertChild(const aElementName: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntElement, aElementName, '', aBeforeNode);
end;

function TXMLNode.InsertXMLDeclaration(const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntXMLDeclaration, '', '', aBeforeNode);
end;

{$IFDEF O_GENERICBYTES}
function TXMLNode.LoadFromBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
var
  xLength: Integer;
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xLength := Length(aBuffer);
    if xLength > 0 then
      xStream.SetPointer(@aBuffer[0], xLength);
    Result := LoadFromStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

function TXMLNode.LoadFromFile(const aFileName: String;
  const aForceEncoding: TEncoding): Boolean;
var
  xFS: TFileStream;
begin
  fOwnerDocument.fURL := aFileName;
  xFS := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(xFS, aForceEncoding);
  finally
    xFS.Free;
  end;
end;

function TXMLNode.LoadFromReader(const aReader: TXMLReader;
  var outReaderToken: PXMLReaderToken; const aBreakWhenRefNodeReached: PXMLNode): Boolean;
var
  xLastNode: PXMLNode;
  xOmitEmptyTextInTheBeginning: Boolean;
begin
  fOwnerDocument.fParseError := nil;
  Result := True;
  xOmitEmptyTextInTheBeginning := True;

  if not (NodeType in [ntDocument, ntElement]) then
    raise EXmlDOMException.Create(OXmlLng_NodeMustBeDOMDocumentOrElement);

  DeleteChildren(True);

  //aReader.ReaderSettings.NodePathHandling := npNo; !!! MUST NOT BE HERE due to OXmlSeq.pas -> MUST BE IN LoadFromStream !!!
  fOwnerDocument.Loading := True;
  try
    xLastNode := @Self;
    while aReader.ReadNextToken({%H-}outReaderToken) do begin
      case outReaderToken.TokenType of
        rtText:
          if not (xOmitEmptyTextInTheBeginning and OXmlIsWhiteSpace(outReaderToken.TokenValue))
          then//omit empty text before root node
            xLastNode.AddText(outReaderToken.TokenValue);
        rtCData: xLastNode.AddCDATASection(outReaderToken.TokenValue);
        rtComment: xLastNode.AddComment(outReaderToken.TokenValue);
        rtDocType: xLastNode.AddDocType(outReaderToken.TokenValue);
        rtProcessingInstruction: xLastNode.AddProcessingInstruction(outReaderToken.TokenName, outReaderToken.TokenValue);
        rtOpenElement:
        begin
          xOmitEmptyTextInTheBeginning := False;
          xLastNode := xLastNode.AddChild(outReaderToken.TokenName);
        end;
        rtOpenXMLDeclaration:
          xLastNode := xLastNode.AddXMLDeclaration;
        rtXMLDeclarationAttribute, rtAttribute:
          xLastNode._AddAttribute(outReaderToken.TokenName, outReaderToken.TokenValue);
        rtFinishOpenElementClose, rtCloseElement, rtFinishXMLDeclarationClose:
          xLastNode := xLastNode.ParentNode;
      end;

      if Assigned(aBreakWhenRefNodeReached) and
        not (outReaderToken.TokenType in [rtOpenElement, rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtAttribute])
      then begin
        if Assigned(xLastNode) then
        begin
          if Assigned(aBreakWhenRefNodeReached) and (
            (xLastNode = aBreakWhenRefNodeReached)) then
          begin
            Break;
          end;
        end else begin
          Break;//This is not an error -> it may happen in the sequential reader (error would be raised already in TXMLReader!)
        end;
      end;
    end;

    if Assigned(aReader.ParseError) then
    begin
      fOwnerDocument.fParseError := aReader.ParseError;

      Result := False;

      Self.DeleteChildren;
    end;
  finally
    fOwnerDocument.Loading := False;
    fOwnerDocument.fURL := '';
  end;
end;

function TXMLNode.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
var
  xReader: TXMLReader;
  xReaderToken: PXMLReaderToken;
begin
  xReader := TXMLReader.Create;
  try
    xReader.InitStream(aStream, aForceEncoding);
    xReader.ReaderSettings.Assign(OwnerDocument.fReaderSettings);
    Result := LoadFromReader(xReader, {%H-}xReaderToken);
  finally
    xReader.Free;
  end;
end;

function TXMLNode.LoadFromXML(const aXML: OWideString): Boolean;
var
  xLength: Integer;
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xLength := Length(aXML);
    if xLength > 0 then
      xStream.SetPointer(@aXML[1], xLength * SizeOf(OWideChar));
    Result := LoadFromStream(xStream, TEncoding.OWideStringEncoding);
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.Normalize;
var
  xText: OWideString;
  xPrevChild, xChild: PXMLNode;
begin
  xPrevChild := nil;
  xChild := nil;
  while GetNextChild(xChild) do
  begin
    case xChild.NodeType of
      ntText:
      begin
        xText := Trim(xChild.Text);
        if xText = '' then
        begin
          xChild.DeleteSelf;
          Continue;//do not actualize xPrevChild
        end;

        if Assigned(xPrevChild) and (xPrevChild.NodeType = ntText) then
        begin
          xPrevChild.Text := xPrevChild.Text + xText;
          xChild.DeleteSelf;
          Continue;//do not actualize xPrevChild
        end;

        xChild.Text := xText;
      end;
      ntDocument, ntElement:
        xChild.Normalize;
    end;

    xPrevChild := xChild;
  end;
end;

{$IFDEF O_RAWBYTESTRING}
function TXMLNode.LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
var
  xLength: Integer;
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xLength := Length(aXML);
    if xLength > 0 then
      xStream.SetPointer(@aXML[1], xLength);
    Result := LoadFromStream(xStream, TEncoding.UTF8);
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

procedure TXMLNode.Remove(const aOld: PXMLNode; const aChildType: TXMLChildType);
var
  xPrev, xNext: PXMLNode;
  xList: TXMLChildNodeList;
begin
  if aOld.fParentNode <> @Self then
    raise EXmlDOMException.Create(OXmlLng_NodeToDeleteNotAChild);

  if TryGetChildNodes({%H-}xList, aChildType) then
    xList.ExtNodeRemoved;

  if fFirstCChild[aChildType] = aOld then
    fFirstCChild[aChildType] := aOld.fNextSibling;
  if fLastCChild[aChildType] = aOld then
    fLastCChild[aChildType] := aOld.fPreviousSibling;

  xPrev := aOld.PreviousSibling;
  xNext := aOld.NextSibling;
  if Assigned(xPrev) then begin
    if Assigned(xNext) then
      xPrev.fNextSibling := xNext
    else
      xPrev.fNextSibling := nil;
  end;
  if Assigned(xNext) then begin
    if Assigned(xPrev) then
      xNext.fPreviousSibling := xPrev
    else
      xNext.fPreviousSibling := nil;
  end;

  aOld.fParentNode := nil;
end;

function TXMLNode.RemoveAttribute(const aOldAttribute: PXMLNode): PXMLNode;
begin
  Remove(aOldAttribute, ctAttribute);
  Result := aOldAttribute;
end;

function TXMLNode.RemoveChild(const aOldChild: PXMLNode): PXMLNode;
begin
  Remove(aOldChild, ctChild);
  Result := aOldChild;
end;

procedure TXMLNode.RemoveSelfFromParent;
begin
  if Assigned(fParentNode) then
  begin
    if fNodeType = ntAttribute then
      fParentNode.Remove(@Self, ctAttribute)
    else
      fParentNode.Remove(@Self, ctChild);

    fParentNode := nil;
  end;
end;

function TXMLNode.ReplaceChild(const aNewChild, aOldChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  Result := aOldChild;

  if Assigned(aNewChild.fParentNode) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  aOldChild.ParentNode.InsertBefore(aNewChild, aOldChild);
  aOldChild.ParentNode.RemoveChild(aOldChild);
end;

function TXMLNode.InsertDocType(const aDocTypeRawText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntDocType, '', aDocTypeRawText, aBeforeNode);
end;

function TXMLNode.InsertProcessingInstruction(const aTarget,
  aContent: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntComment, aTarget, aContent, aBeforeNode);
end;

function TXMLNode.InsertText(const aText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntElement, '', aText, aBeforeNode);
end;

function TXMLNode.SelectNode(const aXPath: OWideString;
  var outNode: PXMLNode): Boolean;
var
  xNodeList: IXMLNodeList;
  xChildType: TXMLChildType;
  xNodeName: OWideString;
  xNodeNameId: OHashedStringsIndex;
begin
  if aXPath = '' then
    raise EXmlDOMException.Create(OXmlLng_XPathCannotBeEmpty);

  if XPathIsSimpleNode(aXPath, {%H-}xNodeName, {%H-}xChildType) then begin
    //speed optimization without true XPath support
    xNodeNameId := fOwnerDocument.fDictionary.IndexOf(xNodeName);
    if xNodeNameId >= 0 then
    begin
      outNode := fFirstCChild[xChildType];
      while Assigned(outNode) do
      begin
        if outNode.fNodeNameId = xNodeNameId then begin
          Result := True;
          Exit;
        end;
        outNode := outNode.fNextSibling;
      end;
    end;
    outNode := nil;
    Result := False;
  end else begin
    xNodeList := nil;

    Result := SelectNodes(aXPath, xNodeList, 1);
    if Result and (xNodeList.Count > 0) then
      outNode := xNodeList[0]
    else
      outNode := nil;
  end;
end;

procedure TXMLNode.SaveToFile(const aFileName: String);
var
  xFS: TFileStream;
begin
  xFS := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(xFS);
  finally
    xFS.Free;
  end;
end;

procedure TXMLNode.SaveToStream(const aStream: TStream);
var
  xWriter: TXMLWriter;
begin
  xWriter := TXMLWriter.Create;
  try
    xWriter.InitStream(aStream);
    xWriter.WriterSettings.Assign(OwnerDocument.fWriterSettings);
    xWriter.Encoding := GetCreateCodePage(OwnerDocument.CodePage);

    SaveToWriter(xWriter);
  finally
    xWriter.Free;
  end;
end;

procedure TXMLNode.SaveToXML(var outXML: OWideString);
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
begin
  xStream := TMemoryStream.Create;
  try
    xWriter := TXMLWriter.Create;
    try
      xWriter.InitStream(xStream);
      xWriter.WriterSettings.Assign(OwnerDocument.fWriterSettings);
      xWriter.Encoding := TEncoding.OWideStringEncoding;
      xWriter.WriterSettings.WriteBOM := False;

      SaveToWriter(xWriter);
    finally
      xWriter.Free;
    end;

    SetLength(outXML, xStream.Size div SizeOf(OWideChar));
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outXML[1], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType);
var
  xIndentType: TXMLIndentType;
begin
  xIndentType := fOwnerDocument.fWriterSettings.IndentType;
  fOwnerDocument.fWriterSettings.IndentType := aIndentType;
  try
    SaveToXML(outXML);
  finally
    fOwnerDocument.fWriterSettings.IndentType := xIndentType;
  end;
end;

{$IFDEF O_RAWBYTESTRING}
procedure TXMLNode.SaveToXML_UTF8(var outXML: ORawByteString);
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
begin
  xStream := TMemoryStream.Create;
  try
    xWriter := TXMLWriter.Create;
    try
      xWriter.InitStream(xStream);
      xWriter.WriterSettings.Assign(OwnerDocument.fWriterSettings);
      xWriter.Encoding := TEncoding.UTF8;
      xWriter.WriterSettings.WriteBOM := False;

      SaveToWriter(xWriter);
    finally
      xWriter.Free;
    end;

    SetLength(outXML, xStream.Size);
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outXML[1], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.SaveToXML_UTF8(var outXML: ORawByteString; const aIndentType: TXMLIndentType);
var
  xIndentType: TXMLIndentType;
begin
  xIndentType := fOwnerDocument.fWriterSettings.IndentType;
  fOwnerDocument.fWriterSettings.IndentType := aIndentType;
  try
    SaveToXML_UTF8(outXML);
  finally
    fOwnerDocument.fWriterSettings.IndentType := xIndentType;
  end;
end;
{$ENDIF}

function TXMLNode.SelectNode(const aXPath: OWideString): PXMLNode;
begin
  SelectNode(aXPath, {%H-}Result);
end;

function TXMLNode.SelectNodeCreate(const aNodeName: OWideString): PXMLNode;
begin
  if aNodeName = '' then
    raise EXmlDOMException.Create(OXmlLng_NodeNameCannotBeEmpty);

  if not SelectNode(aNodeName, {%H-}Result) then
  begin
    if aNodeName[1] = '@' then
      Result := AddAttribute(Copy(aNodeName, 2, High(Integer)), '')
    else
      Result := AddChild(aNodeName);
  end;
end;

function TXMLNode.SelectNodeDummy(const aXPath: OWideString): PXMLNode;
begin
  if not SelectNode(aXPath, {%H-}Result) then
    Result := OwnerDocument.DummyNode;
end;

function TXMLNode.SelectNodes(const aXPath: OWideString;
  const aMaxNodeCount: Integer): IXMLNodeList;
begin
  SelectNodes(aXPath, {%H-}Result, aMaxNodeCount);
end;

function TXMLNode.SelectNodes(const aXPath: OWideString;
  var outNodeList: IXMLNodeList; const aMaxNodeCount: Integer): Boolean;
var
  xXPaths: TXMLXPathList;
  xCustomList: TXMLXPathNodeList;
begin
  xXPaths := TXMLXPathList.Create;
  try
    xXPaths.LoadFromString(aXPath);

    xCustomList := nil;//must be here -> list will be created in SelectNodes
    Result := xXPaths.SelectNodes(@Self, xCustomList, TXMLXPathDOMAdapter, aMaxNodeCount);
    if Result then
      outNodeList := (IInterface(xCustomList) as IXMLNodeList)
    else
      outNodeList := OwnerDocument.DummyResNodeList;
  finally
    xXPaths.Free;
  end;
end;

function TXMLNode.SetAttribute(const aName, aValue: OWideString): PXMLNode;
begin
  AddAttribute(aName, aValue);
  Result := @Self;
end;

function TXMLNode.SetAttributeNode(const aAttr: PXMLNode): PXMLNode;
begin
  if aAttr.OwnerDocument <> Self.OwnerDocument then
    raise EXmlDOMException.Create(OXmlLng_AppendFromDifferentDocument);

  if Assigned(aAttr.fParentNode) then
    raise EXmlDOMException.Create(OXmlLng_ParentNodeMustBeNil);

  if FindAttribute(aAttr.NodeName, {%H-}Result) then
    Remove(Result, ctAttribute)
  else
    Result := nil;

  Append(aAttr, ctAttribute);
end;

procedure TXMLNode.SetNodeValue(const aValue: OWideString);
  procedure _SetTextAndTrim;
  begin
    if OwnerDocument.fWhiteSpaceHandling = wsTrim then
      fNodeValueId := fOwnerDocument.SetString(Trim(aValue))
    else
      fNodeValueId := fOwnerDocument.SetString(aValue);
  end;
  procedure _SetXMLDeclarationData;
  var
    xXML: IXMLDocument;
    xAttr: PXMLNode;
  begin
    Self.DeleteAttributes(False);
    if aValue <> '' then
    begin
      xXML := CreateXMLDoc;
      xXML.LoadFromXML('<x '+aValue+'/>');
      xAttr := nil;
      while xXML.Node.FirstChild.GetNextAttribute(xAttr) do
        Self.SetAttribute(xAttr.NodeName, xAttr.NodeValue);
    end;
  end;
begin
  case fNodeType of
    ntText: _SetTextAndTrim;
    ntXMLDeclaration: _SetXMLDeclarationData;
  else
    fNodeValueId := fOwnerDocument.SetString(aValue);
  end;
end;

procedure TXMLNode.SetText(const aText: OWideString);
begin
  case NodeType of
    ntText, ntCData: SetNodeValue(aText);
    ntDocument, ntElement:
    begin
      DeleteChildren(True);
      AddText(aText);
    end;
  else
    raise EXmlDOMException.Create(OXmlLng_CannotSetText);
  end;
end;

procedure TXMLNode.WriteChildrenXML(
  const aWriter: TXMLWriter);
var
  xChild: PXMLNode;
begin
  xChild := fFirstCChild[ctChild];
  while Assigned(xChild) do
  begin
    xChild.SaveToWriter(aWriter);
    xChild := xChild.fNextSibling;
  end;
end;

procedure TXMLNode.WriteAttributesXML(const aWriter: TXMLWriter);
var
  xAttr: PXMLNode;
begin
  xAttr := fFirstCChild[ctAttribute];
  while Assigned(xAttr) do
  begin
    aWriter.Attribute(xAttr.NodeName, xAttr.NodeValue);
    xAttr := xAttr.fNextSibling;
  end;

  if (fOwnerDocument.WhiteSpaceHandling = wsAutoTag) and
     (fNodeType = ntElement) and
     (Self.fPreserveWhiteSpace <> pwInherit) and
     (Self.fPreserveWhiteSpace <> ParentNode.fPreserveWhiteSpace)
  then
    aWriter.Attribute('xml:space', OXmlPreserveToStr(Self.fPreserveWhiteSpace));
end;

procedure TXMLNode.SaveToWriter(const aWriter: TXMLWriter);
var
  xDict: TOHashedStrings;
begin
  xDict := fOwnerDocument.fDictionary;
  case fNodeType of
    ntDocument: WriteChildrenXML(aWriter);
    ntElement: begin
      aWriter.OpenElement(xDict.GetItem(fNodeNameId).Text);
      WriteAttributesXML(aWriter);
      if HasChildNodes then begin
        aWriter.FinishOpenElement;
        WriteChildrenXML(aWriter);
        aWriter.CloseElement(xDict.GetItem(fNodeNameId).Text,
          (aWriter.WriterSettings.IndentType <> itNone) and//speed optimization
          not (//IsTextElement
            (fFirstCChild[ctChild] = fLastCChild[ctChild]) and
            (fFirstCChild[ctChild].NodeType = ntText)));
      end else begin
        aWriter.FinishOpenElementClose;
      end;
    end;
    ntXMLDeclaration: begin
      aWriter.OpenXMLDeclaration;
      WriteAttributesXML(aWriter);
      aWriter.FinishOpenXMLDeclaration;
    end;
    ntAttribute: aWriter.Attribute(
      xDict.GetItem(fNodeNameId).Text,
      xDict.GetItem(fNodeValueId).Text);
    ntText: begin
      aWriter.Text(xDict.GetItem(fNodeValueId).Text,
        //= not ParentNode.IsTextElement
        Assigned(fNextSibling) or Assigned(fPreviousSibling));
    end;
    ntCData: begin
      aWriter.CData(xDict.GetItem(fNodeValueId).Text);
    end;
    ntComment: begin
      aWriter.Comment(xDict.GetItem(fNodeValueId).Text);
    end;
    ntDocType: begin
      aWriter.DocType(xDict.GetItem(fNodeValueId).Text);
    end;
    ntProcessingInstruction: begin
      aWriter.ProcessingInstruction(
        xDict.GetItem(fNodeNameId).Text,
        xDict.GetItem(fNodeValueId).Text);
    end;
  end;
end;

{$IFDEF O_GENERICBYTES}
procedure TXMLNode.SaveToBuffer(var outBuffer: TBytes);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream);

    SetLength(outBuffer, xStream.Size);
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outBuffer[0], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

{ TXMLDocument }

constructor TXMLDocument.Create(const aRootNodeName: OWideString;
  const aAddUTF8Declaration: Boolean);
var
  xDec: PXMLNode;
begin
  inherited Create;

  DoCreate;

  if aAddUTF8Declaration then begin
    xDec := fBlankDocumentNode.AddXMLDeclaration;
    xDec.AddAttribute('version', '1.0');
    xDec.AddAttribute('encoding', 'utf-8');
    xDec.AddAttribute('standalone', 'yes');
  end;

  if aRootNodeName <> '' then
    fBlankDocumentNode.AddChild(aRootNodeName);
end;

function TXMLDocument.CreateAttribute(const aName,
  aValue: OWideString): PXMLNode;
begin
  CreateNode(ntAttribute, aName, aValue, {%H-}Result);
end;

function TXMLDocument.CreateCDATASection(const aData: OWideString): PXMLNode;
begin
  CreateNode(ntCData, '', aData, {%H-}Result);
end;

function TXMLDocument.CreateComment(const aText: OWideString): PXMLNode;
begin
  CreateNode(ntComment, '', aText, {%H-}Result);
end;

function TXMLDocument.CreateDocType(
  const aDocTypeRawText: OWideString): PXMLNode;
begin
  CreateNode(ntDocType, '', aDocTypeRawText, {%H-}Result);
end;

function TXMLDocument.CreateElement(const aNodeName: OWideString): PXMLNode;
begin
  CreateNode(ntElement, aNodeName, '', {%H-}Result);
end;

procedure TXMLDocument.CreateNode(const aNodeType: TXmlNodeType;
  const aNodeNameId, aNodeValueId: OHashedStringsIndex; var outNode: PXMLNode);
{$IFDEF O_GENERICS}
var
  xEnum: TPair<PXMLNode,Boolean>;
{$ENDIF}
begin
  if fFreeNodes.Count = 0 then begin
    //use new id
    if fNextNodeId >= fNodesLength then
      Grow;
    outNode := GetNode(fNextNodeId);
    Inc(fNextNodeId);
  end else begin
    //use last free id - from the end to be sure no memory must be moved
    {$IFDEF O_GENERICS}
    for xEnum in fFreeNodes do//trick to get the first key in fFreeNodes
    begin
      outNode := xEnum.Key;
      fFreeNodes.Remove(outNode);
      Break;
    end;
    {$ELSE}
    outNode := {%H-}PXMLNode(fFreeNodes[fFreeNodes.Count-1]);
    fFreeNodes.Delete(fFreeNodes.Count-1);
    {$ENDIF}

  end;
  outNode.Init(aNodeType, aNodeNameId, aNodeValueId, Self);
end;

constructor TXMLDocument.Create(aDummy: TObject);
begin
  inherited Create;

  DoCreate;
end;

procedure TXMLDocument.CreateNode(const aNodeType: TXmlNodeType;
  const aNodeName, aNodeValue: OWideString; var outNode: PXMLNode);
begin
  CreateNode(aNodeType, fDictionary.Add(aNodeName), fDictionary.Add(aNodeValue),
    outNode);
end;

function TXMLDocument.CreateProcessingInstruction(const aTarget,
  aContent: OWideString): PXMLNode;
begin
  CreateNode(ntProcessingInstruction, aTarget, aContent, {%H-}Result);
end;

function TXMLDocument.GetCreateTempChildNodeList(
  const aParentNode: PXMLNode; const aChildType: TXMLChildType): TXMLChildNodeList;
begin
  if not TryGetTempChildNodeList(aParentNode, aChildType, {%H-}Result) then begin
    Result := TXMLChildNodeList.Create(aParentNode, aChildType);
    {$IFDEF O_GENERICS}
    fTempChildNodes[aChildType].Add(aParentNode, Result);
    {$ELSE}
    fTempChildNodes[aChildType].AddObject({%H-}ONativeInt(aParentNode), Result);
    {$ENDIF}
  end;
end;

function TXMLDocument.CreateTextNode(const aText: OWideString): PXMLNode;
begin
  CreateNode(ntText, '', aText, {%H-}Result);
end;

function TXMLDocument.CreateXMLDeclaration: PXMLNode;
begin
  CreateNode(ntXMLDeclaration, 'xml', '', {%H-}Result);
end;

function TXMLDocument.AddChild(const aElementName: OWideString): PXMLNode;
begin
  Result := Node.AddChild(aElementName);
end;

procedure TXMLDocument.Clear(const aFullClear: Boolean);
begin
  ClearNodes(aFullClear);
  CreateNode(ntDocument, '', '', fBlankDocumentNode);
end;

procedure TXMLDocument.ClearNodes(const aFullClear: Boolean);
var
  I: Integer;
  C: TXMLChildType;
begin
  fDictionary.Clear(aFullClear);

  fFreeNodes.Clear;
  fNextNodeId := 0;

  if aFullClear then
  begin
    //clear nodes list
    fNodesLength := 0;
    for I := 0 to fNodes.Count-1 do
      Dispose(PXMLNodeArray(fNodes[I]));
    fNodes.Clear;
  end;

  //clear temp nodes
  fBlankDocumentNode := nil;
  fDummyNode := nil;

  //clear temp nodes list
  for C := Low(C) to High(C) do
    ClearTempChildNodeLists(C);
end;

procedure TXMLDocument.ClearTempChildNodeLists(const aChildType: TXMLChildType);
begin
  fTempChildNodes[aChildType].Clear;
end;

destructor TXMLDocument.Destroy;
var
  C: TXMLChildType;
begin
  ClearNodes(True);
  fDictionary.Free;
  fNodes.Free;
  fFreeNodes.Free;
  for C := Low(C) to High(C) do
    fTempChildNodes[C].Free;

  fWriterSettings.Free;
  fReaderSettings.Free;

  inherited;
end;

procedure TXMLDocument.DestroyTempChildNodeList(const aParentNode: PXMLNode;
  const aChildType: TXMLChildType);
begin
  if fTempChildNodes[aChildType].Count > 0 then
    fTempChildNodes[aChildType].Remove({$IFNDEF O_GENERICS}{%H-}ONativeInt{$ENDIF}(aParentNode));
end;

procedure TXMLDocument.DoCreate;
var
  C: TXMLChildType;
begin
  fWriterSettings := TXMLWriterSettings.Create;
  fReaderSettings := TXMLReaderSettings.Create;

  fDictionary := TOHashedStrings.Create;

  {$IFDEF O_GENERICS}
  fNodes := TList<PXMLNodeArray>.Create;
  fFreeNodes := TDictionary<PXMLNode,Boolean>.Create;
  for C := Low(C) to High(C) do
    fTempChildNodes[C] := TObjectDictionary<PXMLNode,TXMLChildNodeList>.Create([doOwnsValues]);
  {$ELSE}
  fNodes := TList.Create;
  fFreeNodes := TODictionary.Create;
  for C := Low(C) to High(C) do
    fTempChildNodes[C] := TODictionary.Create(dupIgnore, soAscending, True);
  {$ENDIF}

  fWhiteSpaceHandling := wsPreserveInTextOnly;

  Clear;
end;

function TXMLDocument.FindXMLDeclarationNode(
  var outXMLDeclarationNode: PXMLNode): Boolean;
var
  xChild: PXMLNode;
begin
  if fBlankDocumentNode.HasChildNodes then begin
    xChild := nil;
    while fBlankDocumentNode.GetNextChild(xChild) do
    if (xChild.NodeType = ntXMLDeclaration)
    then begin
      outXMLDeclarationNode := xChild;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
  outXMLDeclarationNode := nil;
end;

procedure TXMLDocument.FreeNode(const aNode: PXMLNode);
begin
  aNode.DeleteAttributes(True);
  aNode.DeleteChildren(True);
  aNode.fNodeNameId := -1;
  aNode.fNodeValueId := -1;
  aNode.fParentNode := nil;

  //do not set fNextSibling and fPrevSibling to nil, it's not necessary and also e.g. GetNextChild() has to work also after the node has been deleted!!!

  {$IFDEF O_GENERICS}
  fFreeNodes.Add(aNode, True);
  {$ELSE}
  fFreeNodes.Add({%H-}ONativeInt(aNode));
  {$ENDIF}
end;

function TXMLDocument.GetCodePage: Word;
var
  xEncodingAlias: OWideString;
begin
  xEncodingAlias := Encoding;

  if (xEncodingAlias <> '') then
    Result := AliasToCodePage(xEncodingAlias)
  else
    Result := 0;

  if Result = 0 then
    Result := CP_UTF8;
end;

function TXMLDocument.GetXMLDeclarationAttribute(
  const aAttributeName: OWideString): OWideString;
var
  xDecNode: PXMLNode;
begin
  if FindXMLDeclarationNode({%H-}xDecNode) then
    Result := xDecNode.GetAttribute(aAttributeName)
  else
    Result := '';
end;

function TXMLDocument.GetWriterSettings: TXMLWriterSettings;
begin
  Result := fWriterSettings;
end;

function TXMLDocument.GetDocumentElement: PXMLNode;
var
  xChild: PXMLNode;
begin
  xChild := nil;
  while fBlankDocumentNode.GetNextChild(xChild) do
  if xChild.NodeType = ntElement then begin
    Result := xChild;
    Exit;
  end;
  Result := nil;
end;

function TXMLDocument.GetDocumentNode: PXMLNode;
begin
  Result := fBlankDocumentNode;
end;

function TXMLDocument.GetEncoding: OWideString;
begin
  Result := GetXMLDeclarationAttribute('encoding');
end;

function TXMLDocument.GetLoading: Boolean;
begin
  Result := fLoading;
end;

function TXMLDocument.GetNode(const aNodeId: XMLNodeId): PXMLNode;
begin
  Result := @(PXMLNodeArray(fNodes[aNodeId shr 10])^)[aNodeId and 1023]//= [aNode div 1024][aNode mod 1024]
end;

function TXMLDocument.GetParseError: IXMLParseError;
begin
  Result := fParseError;
end;

function TXMLDocument.GetDummyNode: PXMLNode;
begin
  if not Assigned(fDummyNode) then begin
    CreateNode(ntElement, '', '', fDummyNode);
    fDummyNode.fParentNode := fBlankDocumentNode;
  end;
  Result := fDummyNode;
end;

function TXMLDocument.GetDummyResNodeList: IXMLNodeList;
begin
  if not Assigned(fDummyNodeList) then
    fDummyNodeList := TXMLResNodeList.Create
  else
    fDummyNodeList.Clear;

  Result := fDummyNodeList;
end;

function TXMLDocument.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fReaderSettings;
end;

function TXMLDocument.GetStandAlone: OWideString;
begin
  Result := GetXMLDeclarationAttribute('standalone');
end;

function TXMLDocument.GetString(const aStringId: OHashedStringsIndex): OWideString;
begin
  Result := fDictionary.Get(aStringId);
end;

function TXMLDocument.TryGetTempChildNodeList(const aParentNode: PXMLNode;
  const aChildType: TXMLChildType; var outList: TXMLChildNodeList): Boolean;
{$IFNDEF O_GENERICS}
var
  xIndex: Integer;
{$ENDIF}
begin
  if fTempChildNodes[aChildType].Count = 0 then begin
    Result := False;
    outList := nil;
  end else begin
    {$IFDEF O_GENERICS}
    Result := fTempChildNodes[aChildType].TryGetValue(aParentNode, outList);
    {$ELSE}
    Result := fTempChildNodes[aChildType].Find({%H-}ONativeInt(aParentNode), {%H-}xIndex);
    if Result then
      outList := TXMLChildNodeList(fTempChildNodes[aChildType].Objects[xIndex])
    else
      outList := nil;
    {$ENDIF}
  end;
end;

function TXMLDocument.GetVersion: OWideString;
begin
  Result := GetXMLDeclarationAttribute('version');
end;

function TXMLDocument.GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
begin
  Result := fWhiteSpaceHandling;
end;

function TXMLDocument.XML: OWideString;
begin
  Result := Node.XML;
end;

function TXMLDocument.XML(const aIndentType: TXMLIndentType): OWideString;
begin
  Result := Node.XML(aIndentType);
end;

{$IFDEF O_RAWBYTESTRING}
function TXMLDocument.XML_UTF8: ORawByteString;
begin
  Result := Node.XML_UTF8;
end;

function TXMLDocument.XML_UTF8(const aIndentType: TXMLIndentType): ORawByteString;
begin
  Result := Node.XML_UTF8(aIndentType);
end;
{$ENDIF}

{$IFDEF O_GENERICBYTES}
procedure TXMLDocument.SaveToBuffer(var outBuffer: TBytes);
begin
  Node.SaveToBuffer(outBuffer);
end;
{$ENDIF}

procedure TXMLDocument.Grow;
var
  xNewArray: PXMLNodeArray;
begin
  fNodesLength := fNodesLength+1024;

  New(xNewArray);
  SetLength(xNewArray^, 1024);
  fNodes.Add(xNewArray);
end;

{$IFDEF O_GENERICBYTES}
function TXMLDocument.LoadFromBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromBuffer(aBuffer, aForceEncoding);
end;
{$ENDIF}

function TXMLDocument.LoadFromFile(const aFileName: String;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromFile(aFileName, aForceEncoding);
end;

function TXMLDocument.LoadFromReader(const aReader: TXMLReader; var outReaderToken: PXMLReaderToken): Boolean;
begin
  Result := Node.LoadFromReader(aReader, outReaderToken);
end;

function TXMLDocument.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromStream(aStream, aForceEncoding);
end;

function TXMLDocument.LoadFromXML(const aXML: OWideString): Boolean;
begin
  Clear;
  Result := Node.LoadFromXML(aXML);
end;

{$IFDEF O_RAWBYTESTRING}
function TXMLDocument.LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
begin
  Clear;
  Result := Node.LoadFromXML_UTF8(aXML);
end;
{$ENDIF}

procedure TXMLDocument.SaveToFile(const aFileName: String);
begin
  Node.SaveToFile(aFileName);
end;

procedure TXMLDocument.SaveToXML(var outXML: OWideString);
begin
  Node.SaveToXML(outXML);
end;

procedure TXMLDocument.SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType);
begin
  Node.SaveToXML(outXML, aIndentType);
end;

{$IFDEF O_RAWBYTESTRING}
procedure TXMLDocument.SaveToXML_UTF8(var outXML: ORawByteString);
begin
  Node.SaveToXML_UTF8(outXML);
end;

procedure TXMLDocument.SaveToXML_UTF8(var outXML: ORawByteString; const aIndentType: TXMLIndentType);
begin
  Node.SaveToXML_UTF8(outXML, aIndentType);
end;
{$ENDIF}

procedure TXMLDocument.SaveToStream(const aStream: TStream);
begin
  Node.SaveToStream(aStream);
end;

procedure TXMLDocument.SaveToWriter(const aWriter: TXMLWriter);
begin
  Node.SaveToWriter(aWriter);
end;

procedure TXMLDocument.SetCodePage(const aCodePage: Word);
begin
  Encoding := CodePageToAlias(aCodePage);
end;

procedure TXMLDocument.SetDocumentElement(const aDocumentElement: PXMLNode);
var
  xChild: PXMLNode;
begin
  xChild := nil;
  while fBlankDocumentNode.GetNextChild(xChild) do
  if xChild.NodeType = ntElement then begin
    xChild.DeleteSelf;
  end;

  fBlankDocumentNode.AppendChild(aDocumentElement);
end;

procedure TXMLDocument.SetXMLDeclarationAttribute(const aAttributeName,
  aAttributeValue: OWideString);
var
  xDecNode: PXMLNode;
begin
  if not FindXMLDeclarationNode({%H-}xDecNode) then begin
    if fBlankDocumentNode.HasChildNodes then
      xDecNode := fBlankDocumentNode.InsertXMLDeclaration(fBlankDocumentNode.FirstChild)
    else
      xDecNode := fBlankDocumentNode.AddXMLDeclaration;
  end;

  xDecNode.AddAttribute(aAttributeName, aAttributeValue);
end;

procedure TXMLDocument.SetEncoding(const aEncoding: OWideString);
begin
  SetXMLDeclarationAttribute('encoding', aEncoding);
end;

procedure TXMLDocument.SetLoading(const aLoading: Boolean);
begin
  fLoading := aLoading;
end;

procedure TXMLDocument.SetStandAlone(const aStandAlone: OWideString);
begin
  SetXMLDeclarationAttribute('standalone', aStandAlone);
end;

function TXMLDocument.SetString(const aString: OWideString): OHashedStringsIndex;
begin
  Result := fDictionary.Add(aString);
end;

procedure TXMLDocument.SetVersion(const aVersion: OWideString);
begin
  SetXMLDeclarationAttribute('version', aVersion);
end;

procedure TXMLDocument.SetWhiteSpaceHandling(
  const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
begin
  fWhiteSpaceHandling := aWhiteSpaceHandling;
end;

{ TXMLResNodeListEnumerator }

constructor TXMLResNodeListEnumerator.Create(aList: IXMLNodeList);
begin
  inherited Create;

  fList := aList;
  fIndex := -1;
end;

function TXMLResNodeListEnumerator.GetCurrent: PXMLNode;
begin
  Result := fList[fIndex];
end;

function TXMLResNodeListEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex < fList.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ TXMLResNodeList }

function TXMLResNodeList.Add(const aNode: PXMLNode): Integer;
begin
  Result := fList.Add(aNode);
end;

function TXMLResNodeList.AddNode(const aNode: PXMLNode): PXMLNode;
begin
  Add(aNode);
  Result := aNode;
end;

procedure TXMLResNodeList.Clear;
begin
  fList.Clear;
end;

constructor TXMLResNodeList.Create;
begin
  inherited Create;

  {$IFDEF O_GENERICS}
  fList := TList<PXMLNode>.Create;
  {$ELSE}
  fList := TList.Create;
  {$ENDIF}
end;

procedure TXMLResNodeList.Delete(const aIndex: Integer);
begin
  if (aIndex >= 0) and  (aIndex < fList.Count) then begin
    fList.Delete(aIndex);
  end;
end;

procedure TXMLResNodeList.Delete(const aNode: PXMLNode);
var
  I: Integer;
begin
  I := IndexOf(aNode);
  if I >= 0 then
    Delete(I)
end;

destructor TXMLResNodeList.Destroy;
begin
  fList.Free;

  inherited;
end;

procedure TXMLResNodeList.ExtNodeAppended;
begin
  //do nothing
end;

procedure TXMLResNodeList.ExtNodeRemoved;
begin
  //do nothing
end;

procedure TXMLResNodeList.ExtNodeInserted;
begin
  //do nothing
end;

procedure TXMLResNodeList.Delete(const aName: OWideString);
var
  I: Integer;
begin
  I := IndexOf(aName);
  if I >= 0 then
    Delete(I)
end;

function TXMLResNodeList.FindNode(const aName: OWideString): PXMLNode;
begin
  if IndexOf(aName, {%H-}Result) < 0 then
    Result := nil;
end;

function TXMLResNodeList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TXMLResNodeList.GetFirst: PXMLNode;
begin
  if Count > 0 then
    Result := Nodes[0]
  else
    Result := nil;
end;

function TXMLResNodeList.GetLast: PXMLNode;
begin
  if Count > 0 then
    Result := Nodes[Count-1]
  else
    Result := nil;
end;

{$IFDEF O_ENUMERATORS}
function TXMLResNodeList.GetEnumerator: TXMLResNodeListEnumerator;
begin
  Result := TXMLResNodeListEnumerator.Create(Self);
end;
{$ENDIF}

function TXMLResNodeList.GetNext(var ioNodeEnum: PXMLNode): Boolean;
begin
  Result := GetPrevNext(ioNodeEnum, +1);
end;

function TXMLResNodeList.Get(const aIndex: Integer): PXMLNode;
begin
  {$IFDEF O_GENERICS}
  Result := fList.Items[aIndex];
  {$ELSE}
  Result := PXMLNode(fList.Items[aIndex]);
  {$ENDIF}
end;

function TXMLResNodeList.GetPrevious(var ioNodeEnum: PXMLNode): Boolean;
begin
  Result := GetPrevNext(ioNodeEnum, -1);
end;

function TXMLResNodeList.GetPrevNext(var ioNodeEnum: PXMLNode;
  const aInc: Integer): Boolean;
var
  xCount: Integer;
begin
  //same code as TSAXAttributes.GetPrevNext
  Result := False;

  xCount := Count;
  if xCount = 0 then
  begin
    ioNodeEnum := nil;
    Exit;
  end;

  if Assigned(ioNodeEnum) then begin
    //get prev/next
    if not(
       (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount) and
       (Nodes[fIteratorCurrent] = ioNodeEnum))
    then//ioNodeEnum is NOT the last iterator -> we have to find it
      fIteratorCurrent := IndexOf(ioNodeEnum);

    if (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount)
    then begin
      fIteratorCurrent := fIteratorCurrent + aInc;
      Result := (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount);
      if Result then
        ioNodeEnum := Nodes[fIteratorCurrent]
      else
        ioNodeEnum := nil;
    end;
  end else begin
    //return first or last element (list must not be not empty)
    if aInc > 0 then
      fIteratorCurrent := 0
    else
      fIteratorCurrent := xCount-1;
    ioNodeEnum := Nodes[fIteratorCurrent];
    Result := True;
  end;
end;

function TXMLResNodeList.IndexOf(const aName: OWideString): Integer;
var x: PXMLNode;
begin
  Result := IndexOf(aName, {%H-}x);
end;

function TXMLResNodeList.IndexOf(const aName: OWideString;
  var outNode: PXMLNode): Integer;
var
  xNameId: OHashedStringsIndex;
begin
  if Count = 0 then begin
    Result := -1;
    outNode := nil;
    Exit;
  end;

  xNameId := Nodes[0].fOwnerDocument.fDictionary.IndexOf(aName);
  if xNameId < 0 then begin
    Result := -1;
    outNode := nil;
    Exit;
  end;

  for Result := 0 to Count-1 do
  if (Nodes[Result].fNodeNameId = xNameId) then begin
    outNode := Nodes[Result];
    Exit;
  end;

  Result := -1;
  outNode := nil;
end;

function TXMLResNodeList.IndexOf(const aNode: PXMLNode): Integer;
begin
  Result := fList.IndexOf(aNode);
end;

procedure TXMLResNodeList.Insert(const aIndex: Integer; const aNode: PXMLNode);
begin
  fList.Insert(aIndex, aNode);
end;

function TXMLResNodeList.Remove(const aNode: PXMLNode): Integer;
begin
  Result := fList.Remove(aNode);
end;

{ TXMLXPathDOMAdapter }

procedure TXMLXPathDOMAdapter.AddNodeToResList(const aNode: TXMLXPathNode);
begin
  fResNodeList.Add(aNode);
end;

procedure TXMLXPathDOMAdapter.BuildIdTree(const aStartWithNode: TXMLXPathNode;
  const aLevelsDeep: Integer; const aIdTree: TXMLXPathIdTree);
var
  xId: XMLNodeId;

  procedure _ScanNode(const bNode: PXMLNode; const bLevelsDeepLeft: Integer);
  var
    xChild: PXMLNode;
  begin
    {$IFDEF O_GENERICS}
    aIdTree.Add(TXMLXPathNode(bNode), xId);
    {$ELSE}
    aIdTree.AddPointer({%H-}ONativeInt(TXMLXPathNode(bNode)), {%H-}Pointer(xId));
    {$ENDIF}
    Inc(xId);

    if bLevelsDeepLeft < 0 then
      Exit;

    if bNode.HasAttributes then begin
      xChild := nil;
      while bNode.GetNextAttribute(xChild) do begin
        {$IFDEF O_GENERICS}
        aIdTree.Add(TXMLXPathNode(xChild), xId);
        {$ELSE}
        aIdTree.AddPointer({%H-}ONativeInt(TXMLXPathNode(xChild)), {%H-}Pointer(xId));
        {$ENDIF}
        Inc(xId);
      end;
    end;

    if bNode.HasChildNodes then begin
      xChild := nil;
      while bNode.GetNextChild(xChild) do
      if xChild.NodeType in [ntElement, ntText, ntCData] then begin
        _ScanNode(xChild, bLevelsDeepLeft-1);
      end;
    end;
  end;
begin
  aIdTree.Clear;
  xId := 0;

  _ScanNode(PXMLNode(aStartWithNode), aLevelsDeep);
end;

function TXMLXPathDOMAdapter.CreateResNodeList: TXMLXPathNodeList;
begin
  if not Assigned(fResNodeList) then
    fResNodeList := TXMLResNodeList.Create;
  Result := TXMLXPathNodeList(fResNodeList as IXMLNodeList);
end;

procedure TXMLXPathDOMAdapter.GetNodeAttributes(
  const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList);
var
  xAttr: PXMLNode;
begin
  aList.Clear;
  xAttr := nil;
  while PXMLNode(aParentNode).GetNextAttribute(xAttr) do
    aList.Add(xAttr);
end;

procedure TXMLXPathDOMAdapter.GetNodeChildren(const aParentNode: TXMLXPathNode;
  const aList: TXMLXPathResNodeList);
var
  xChild: PXMLNode;
begin
  aList.Clear;
  xChild := nil;
  while PXMLNode(aParentNode).GetNextChild(xChild) do
    aList.Add(xChild);
end;

function TXMLXPathDOMAdapter.GetNodeDOMDocument(
  const aNode: TXMLXPathNode): TXMLXPathNode;
begin
  Result := PXMLNode(aNode).OwnerDocument.fBlankDocumentNode;
end;

procedure TXMLXPathDOMAdapter.GetNodeInfo(const aNode: TXMLXPathNode;
  var outNodeInfo: TXMLXPathNodeInfo);
var
  xNode: PXMLNode;
begin
  xNode := PXMLNode(aNode);
  outNodeInfo.NodeName := xNode.NodeName;
  outNodeInfo.NodeValue := xNode.NodeValue;
  outNodeInfo.NodeType := xNode.NodeType;
end;

function TXMLXPathDOMAdapter.GetNodeName(
  const aNode: TXMLXPathNode): OWideString;
begin
  Result := PXMLNode(aNode).NodeName;
end;

function TXMLXPathDOMAdapter.GetNodeParent(
  const aNode: TXMLXPathNode): TXMLXPathNode;
begin
  Result := PXMLNode(aNode).ParentNode;
end;

function TXMLXPathDOMAdapter.GetNodeType(
  const aNode: TXMLXPathNode): TXmlNodeType;
begin
  Result := PXMLNode(aNode).NodeType;
end;

function TXMLXPathDOMAdapter.GetNodeValue(
  const aNode: TXMLXPathNode): OWideString;
begin
  Result := PXMLNode(aNode).NodeValue;
end;

function TXMLXPathDOMAdapter.NodeFindAttribute(const aNode: TXMLXPathNode;
  const aAttrName: OWideString): TXMLXPathNode;
var
  xAttr: PXMLNode;
begin
  if PXMLNode(aNode).FindAttribute(aAttrName, {%H-}xAttr) then
    Result := xAttr
  else
    Result := nil;
end;

function TXMLXPathDOMAdapter.NodeHasAttributes(
  const aNode: TXMLXPathNode): Boolean;
begin
  Result := PXMLNode(aNode).HasAttributes;
end;

function TXMLXPathDOMAdapter.NodeHasChildNodes(
  const aNode: TXMLXPathNode): Boolean;
begin
  Result := PXMLNode(aNode).HasChildNodes;
end;

{ TXMLChildNodeList }

procedure TXMLChildNodeList.Delete(const aIndex: Integer);
var
  xNode: PXMLNode;
begin
  xNode := Nodes[aIndex];
  if Assigned(xNode) then
    Delete(xNode);
end;

procedure TXMLChildNodeList.ExtNodeAppended;
begin
  Inc(fTempCount);
end;

procedure TXMLChildNodeList.ExtNodeRemoved;
begin
  Dec(fTempCount);
  ClearTempVariables;
end;

procedure TXMLChildNodeList.ExtNodeInserted;
begin
  Inc(fTempCount);
  ClearTempVariables;
end;

procedure TXMLChildNodeList.Delete(const aNode: PXMLNode);
begin
  aNode.DeleteSelf;
end;

procedure TXMLChildNodeList.Delete(const aName: OWideString);
var
  xNode: PXMLNode;
begin
  xNode := FindNode(aName);
  if Assigned(xNode) then
    Delete(xNode);
end;

function TXMLChildNodeList.FindNode(const aName: OWideString): PXMLNode;
begin
  if IndexOf(aName, {%H-}Result) < 0 then
    Result := nil;
end;

function TXMLChildNodeList.GetCount: Integer;
var
  xIter: PXMLNode;
begin
  if fTempCount >= 0 then begin
    Result := fTempCount;
  end else begin
    Result := 0;
    xIter := nil;
    while GetNext(xIter) do
      Inc(Result);
    fTempCount := Result;
  end;
end;

function TXMLChildNodeList.Add(const aNode: PXMLNode): Integer;
begin
  fParent.Append(aNode, fChildType);
  Result := Count-1;
end;

function TXMLChildNodeList.AddNode(const aNode: PXMLNode): PXMLNode;
begin
  fParent.Append(aNode, fChildType);
  Result := aNode;
end;

procedure TXMLChildNodeList.Clear;
begin
  if fChildType = ctAttribute then
    fParent.DeleteAttributes(False)
  else
    fParent.DeleteChildren(False);
end;

procedure TXMLChildNodeList.ClearTempVariables;
begin
  fLastGetNodeIndex := -1;
  fLastGetNode := nil;
  //do not clear fTempCount here
end;

constructor TXMLChildNodeList.Create(const aParent: PXMLNode; const aChildType: TXMLChildType);
begin
  inherited Create;

  fParent := aParent;
  fLastGetNodeIndex := -1;
  fChildType := aChildType;

  fTempCount := -1;
  GetCount;//load fTempCount -> must be here
end;

{$IFDEF O_ENUMERATORS}
function TXMLChildNodeList.GetEnumerator: TXMLChildNodeListEnumerator;
begin
  Result := TXMLChildNodeListEnumerator.Create(Self);
end;
{$ENDIF}

function TXMLChildNodeList.GetFirst: PXMLNode;
begin
  Result := fParent.fFirstCChild[fChildType];
end;

function TXMLChildNodeList.GetLast: PXMLNode;
begin
  Result := fParent.fLastCChild[fChildType];
end;

function TXMLChildNodeList.GetNext(var ioNode: PXMLNode): Boolean;
begin
  if ioNode = nil then
    ioNode := GetFirst
  else
    ioNode := ioNode.fNextSibling;
  Result := Assigned(ioNode);
end;

function TXMLChildNodeList.Get(const aIndex: Integer): PXMLNode;
var
  I: Integer;
begin
  if aIndex < 0 then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := nil;
  if (fLastGetNodeIndex >= 0) and Assigned(fLastGetNode) and
     not ((aIndex < (fLastGetNodeIndex-aIndex)) or (aIndex < (aIndex-fLastGetNodeIndex))) and //performance -> search from the start if it needs less cycles
     not (((Count-aIndex) < (fLastGetNodeIndex-aIndex)) or ((Count-aIndex) < (aIndex-fLastGetNodeIndex)))//performance -> search from the end if it needs less cycles
  then begin
    if (aIndex = fLastGetNodeIndex) then begin
      //The same node
      Result := fLastGetNode;
    end else begin
      //you cannot run this code for (aIndex = fLastGetNodeIndex)!!!
      //find node as a relative sibling from fLastGetNode
      I := fLastGetNodeIndex;
      Result := fLastGetNode;
      while (I <> aIndex) and Assigned(Result) do begin
        if aIndex > fLastGetNodeIndex then begin
          //Next in list
          Result := Result.NextSibling;
          Inc(I);
        end else begin
          //Previous in list
          Result := Result.PreviousSibling;
          Dec(I);
        end;
      end;
    end;
  end;

  if not Assigned(Result) then begin
    if aIndex < Count div 2 then begin
      //search forwards through all nodes
      I := -1;
      while (I < aIndex) and GetNext(Result) do
        Inc(I);
    end else begin
      //search backwards through all nodes
      I := Count;
      while (I > aIndex) and GetPrevious(Result) do
        Dec(I);
    end;
    if I <> aIndex then
      raise EListError.Create(OXmlLng_ListIndexOutOfRange);
  end;

  if not Assigned(Result) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  fLastGetNode := Result;
  fLastGetNodeIndex := aIndex;
end;

function TXMLChildNodeList.GetPrevious(var ioNode: PXMLNode): Boolean;
begin
  if ioNode = nil then
    ioNode := GetLast
  else
    ioNode := ioNode.fPreviousSibling;
  Result := Assigned(ioNode);
end;

function TXMLChildNodeList.IndexOf(const aNode: PXMLNode): Integer;
var
  xIter: PXMLNode;
begin
  Result := -1;
  xIter := nil;
  while (aNode <> xIter) and GetNext(xIter) do
    Inc(Result);

  if (aNode <> xIter) then
    Result := -1;
end;

function TXMLChildNodeList.IndexOf(const aName: OWideString): Integer;
var
  x: PXMLNode;
begin
  Result := IndexOf(aName, {%H-}x);
end;

function TXMLChildNodeList.IndexOf(const aName: OWideString;
  var outNode: PXMLNode): Integer;
var
  xNameId: OHashedStringsIndex;
begin
  Result := -1;
  outNode := nil;//must be here

  xNameId := fParent.fOwnerDocument.fDictionary.IndexOf(aName);
  if xNameId < 0 then
    Exit;

  //outNode was set to nil at the start of the function
  while GetNext(outNode) do
  begin
    Inc(Result);
    if xNameId = outNode.fNodeNameId then
      Exit;
  end;

  Result := -1;
  outNode := nil;
end;

procedure TXMLChildNodeList.Insert(const aIndex: Integer;
  const aNode: PXMLNode);
var
  xNode: PXMLNode;
begin
  xNode := Nodes[aIndex];
  fParent.Insert(aNode, xNode, fChildType);
end;

function TXMLChildNodeList.Remove(const aName: OWideString;
  var outNode: PXMLNode): Integer;
begin
  Result := IndexOf(aName, outNode);
  if Result >= 0 then
    outNode.RemoveSelfFromParent;
end;

function TXMLChildNodeList.Remove(const aName: OWideString): Integer;
var
  xNode: PXMLNode;
begin
  Result := Remove(aName, {%H-}xNode);
  if Result >= 0 then
    xNode.DeleteSelf;
end;

function TXMLChildNodeList.Remove(const aNode: PXMLNode): Integer;
begin
  Result := IndexOf(aNode);
  if Result >= 0 then
    aNode.DeleteSelf;
end;

{ TXMLChildNodeListEnumerator }

constructor TXMLChildNodeListEnumerator.Create(aList: TXMLChildNodeList);
begin
  inherited Create;

  fList := aList;
  fCurrent := nil;
end;

function TXMLChildNodeListEnumerator.GetCurrent: PXMLNode;
begin
  Result := fCurrent;
end;

function TXMLChildNodeListEnumerator.MoveNext: Boolean;
begin
  if Assigned(fCurrent) then
    fCurrent := fCurrent.NextSibling
  else
    fCurrent := fList.GetFirst;

  Result := Assigned(fCurrent);
end;

end.
