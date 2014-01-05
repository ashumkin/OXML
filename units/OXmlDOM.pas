unit OXmlDOM;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlDOM.pas

  XML DOM record/pointer implementation

  Simplified W3C DOM (Core) Level 1 specification:
    http://www.w3.org/TR/REC-DOM-Level-1/level-one-core.html
  - OXmlDOM uses record-based nodes instead of interfaces for maximum
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
  SysUtils, Classes, OWideSupp, OXmlReadWrite, OEncoding,
  OHashedStrings, OXmlUtils, OXmlXPath
  {$IFDEF O_GENERICS}, Generics.Collections{$ELSE}, ODictionary{$ENDIF}
  ;

type

  TXMLDocument = class;
  XMLNodeId = Cardinal;//Can be switched to ONativeUInt with some memory and performance penalty in 64bit
  PXMLNode = ^TXMLNode;
  //IXMLNode = PXMLNode;//Can be disabled/enabled so that you don't have to change IXMLNode to PXMLNode if you switch from MS XML / OmniXML

  IXMLNodeList = interface;
  TXMLChildNodeList = class;

  TXMLNode = packed {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fId: XMLNodeId;
    fNodeType: TXmlNodeType;
    fNodeNameId: OHashedStringsIndex;
    fNodeValueId: OHashedStringsIndex;
    fParentNodeId: XMLNodeId;

    fFirstChildId: Array[TXMLChildType] of XMLNodeId;
    fLastChildId: Array[TXMLChildType] of XMLNodeId;
    fNextSiblingId: XMLNodeId;
    fPreviousSiblingId: XMLNodeId;

    fPreserveWhiteSpace: Boolean;

    fOwnerDocument: TXMLDocument;
  private
    //methods to work with child/attribute nodes
    procedure Append(const aNew: PXMLNode; const aChildType: TXMLChildType);
    procedure Insert(const aNew, aBeforeNode: PXMLNode; const aChildType: TXMLChildType);
    procedure Remove(const aOld: PXMLNode; const aChildType: TXMLChildType);
    procedure Delete(const aOld: PXMLNode; const aChildType: TXMLChildType);

    function GetNextCChild(var aChildEnum: PXMLNode; const aChildType: TXMLChildType): Boolean;
    function GetPreviousCChild(var aChildEnum: PXMLNode; const aChildType: TXMLChildType): Boolean;
    function GetFirstCChild(const aChildType: TXMLChildType): PXMLNode;
    function GetLastChild(const aChildType: TXMLChildType): PXMLNode;
    function GetChildFromBegin(const aIndex: Integer; const aChildType: TXMLChildType): PXMLNode;
    function GetChildFromEnd(const aIndex: Integer; const aChildType: TXMLChildType): PXMLNode;

    function GetNextSibling: PXMLNode;
    function GetPreviousSibling: PXMLNode;

    function GetParentNode: PXMLNode;
  private
    function GetNodeName: OWideString;
    function GetNodeValue: OWideString;
    procedure SetNodeName(const aName: OWideString);
    procedure SetNodeValue(const aValue: OWideString);
    function GetText: OWideString;
    procedure SetText(const aText: OWideString);
    procedure _SetAttribute(const aName, aValue: OWideString);

    function AddCustomChild(const aType: TXmlNodeType; const aName, aValue: OWideString): PXMLNode;
    function InsertCustomChild(const aType: TXmlNodeType; const aName, aValue: OWideString;
      const aBeforeNode: PXMLNode): PXMLNode;

    function GetAttributeNodes: TXMLChildNodeList;
    function GetChildNodes: TXMLChildNodeList;
    function GetAttributeCount: Integer;
    function GetChildCount: Integer;
    function TryGetChildNodes(var aList: TXMLChildNodeList; const aChildType: TXMLChildType): Boolean;
    function GetIsTextElement: Boolean;
  private
    //methods for direct reading/writing
    procedure WriteChildrenXML(const aWriter: TOXMLWriter);
    procedure WriteAttributesXML(const aWriter: TOXMLWriter);
  public
    procedure Init(const aId: XMLNodeId; const aNodeType: TXmlNodeType;
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
    function HasChildNodes: Boolean;
    function HasAttributes: Boolean;
    function HasAttribute(const aName: OWideString): Boolean;
    function FindAttribute(const aName: OWideString; var aAttr: PXMLNode): Boolean; overload;
    function FindAttribute(const aName: OWideString; var aValue: OWideString): Boolean; overload;
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

    //iterate through all children from first to last (get first for aChildEnum=nil)
    function GetNextChild(var aChildEnum: PXMLNode): Boolean;
    //iterate through all attributes from first to last (get first for aAttributeEnum=nil)
    function GetNextAttribute(var aAttributeEnum: PXMLNode): Boolean;
    //iterate through all children from last to first (get last for aChildEnum=nil)
    function GetPreviousChild(var aChildEnum: PXMLNode): Boolean;
    //iterate through all attributes from last to first (get last for aAttributeEnum=nil)
    function GetPreviousAttribute(var aAttributeEnum: PXMLNode): Boolean;

    procedure DeleteAttribute(const aName: OWideString); overload;
    procedure DeleteAttribute(const aAttr: PXMLNode); overload;
    procedure DeleteAttributes(const aDestroyList: Boolean = True);
    procedure DeleteChild(const aChild: PXMLNode);
    procedure DeleteChildren(const aDestroyList: Boolean = True);
    procedure DeleteSelf;
    //clear: delete all attributes and child nodes, set name and value to empty string
    procedure Clear;

    //insert a node before another
    //  Inserts the node aNewChild before the existing child node aRefChild.
    //  If aRefChild is null, insert aNewChild at the end of the list of children.
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
  public
    //select the first node by XPath, if not found return false (and aNode=nil)
    function SelectNode(const aXPath: OWideString; var aNode: PXMLNode): Boolean; overload;
    //select the first node by XPath, if not found return nil
    function SelectNode(const aXPath: OWideString): PXMLNode; overload;
    //select the first node by XPath, if not found return a fake "null" node (name="", value="")
    function SelectNodeNull(const aXPath: OWideString): PXMLNode;
    //select the first child element by name (not XPath!!!),
    //  if not found the element is created, appended to current node and returned
    function SelectNodeCreate(const aNodeName: OWideString): PXMLNode;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return a aNodeList with no items (count = 0)
    function SelectNodes(const aXPath: OWideString;
      var aNodeList: IXMLNodeList;
      const aMaxNodeCount: Integer = 0): Boolean; overload;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return a list with no items (count = 0)
    function SelectNodes(const aXPath: OWideString;
      const aMaxNodeCount: Integer = 0): IXMLNodeList; overload;
  public
    //load document with custom reader
    function LoadFromReader(const aReader: TOXmlReader): Boolean;
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

    //save document with custom writer
    procedure SaveToWriter(const aWriter: TOXMLWriter);
    //save document to file in encoding specified by the document
    procedure SaveToFile(const aFileName: String);
    //save document to stream in encoding specified by the document
    procedure SaveToStream(const aStream: TStream); overload;
    //returns XML as string
    procedure SaveToXML(var aXML: OWideString);
    {$IFNDEF NEXTGEN}
    procedure SaveToXML_UTF8(var aXML: ORawByteString);
    {$ENDIF}

    {$IFDEF O_DELPHI_2009_UP}
    //returns XML as a buffer in encoding specified by the document
    procedure SaveToBuffer(var aBuffer: TBytes); overload;
    {$ENDIF}
  public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML: OWideString;
    {$IFNDEF NEXTGEN}
    function XML_UTF8: ORawByteString;
    {$ENDIF}
  public
    property Id: XMLNodeId read fId;
    property NodeType: TXmlNodeType read fNodeType;
    property NodeName: OWideString read GetNodeName write SetNodeName;
    property NodeValue: OWideString read GetNodeValue write SetNodeValue;
    property Text: OWideString read GetText write SetText;
    property PreserveWhiteSpace: Boolean read fPreserveWhiteSpace write fPreserveWhiteSpace;

    property ParentNode: PXMLNode read GetParentNode;
    property OwnerDocument: TXMLDocument read fOwnerDocument;

    property FirstChild: PXMLNode index ctChild read GetFirstCChild;
    property LastChild: PXMLNode index ctChild read GetLastChild;
    property ChildFromBegin[const aIndex: Integer]: PXMLNode index ctChild read GetChildFromBegin;
    property ChildFromEnd[const aIndex: Integer]: PXMLNode index ctChild read GetChildFromEnd;
    property FirstAttribute: PXMLNode index ctAttribute read GetFirstCChild;
    property LastAttribute: PXMLNode index ctAttribute read GetLastChild;
    property AttributeFromBegin[const aIndex: Integer]: PXMLNode index ctAttribute read GetChildFromBegin;
    property AttributeFromEnd[const aIndex: Integer]: PXMLNode index ctAttribute read GetChildFromEnd;

    property IsTextElement: Boolean read GetIsTextElement;

    property NextSibling: PXMLNode read GetNextSibling;
    property PreviousSibling: PXMLNode read GetPreviousSibling;

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
    function GetWriterSettings: TOXmlWriterSettings;
    function GetReaderSettings: TOXmlReaderSettings;

    property Loading: Boolean read GetLoading write SetLoading;

  //protected
    function GetNullNode: PXMLNode;
    function GetNullResNodeList: IXMLNodeList;
    function GetDocumentNode: PXMLNode;
    function GetDocumentElement: PXMLNode;
    procedure SetDocumentElement(const aDocumentElement: PXMLNode);

    property NullNode: PXMLNode read GetNullNode;
    property NullResNodeList: IXMLNodeList read GetNullResNodeList;

  //public
    //clear the whole document
    procedure Clear;

    //load document with custom reader
    function LoadFromReader(const aReader: TOXmlReader): Boolean;
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

    //save document with custom writer
    procedure SaveToWriter(const aWriter: TOXMLWriter);
    //save document to file in encoding specified by the document
    procedure SaveToFile(const aFileName: String);
    //save document to stream in encoding specified by the document
    procedure SaveToStream(const aStream: TStream); overload;
    //returns XML as string
    procedure SaveToXML(var aXML: OWideString);
    {$IFNDEF NEXTGEN}
    procedure SaveToXML_UTF8(var aXML: ORawByteString);
    {$ENDIF}

    {$IFDEF O_DELPHI_2009_UP}
    //returns XML as a buffer in encoding specified by the document
    procedure SaveToBuffer(var aBuffer: TBytes); overload;
    {$ENDIF}

  //public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML: OWideString;
    {$IFNDEF NEXTGEN}
    function XML_UTF8: ORawByteString;
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
    property WriterSettings: TOXmlWriterSettings read GetWriterSettings;
    //XML reader settings
    property ReaderSettings: TOXmlReaderSettings read GetReaderSettings;

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

  { TXMLDocument }

  TXMLDocument = class(TInterfacedObject, IXMLDocument)
  private
    fLoading: Boolean;
    fDictionary: TOHashedStrings;
    {$IFDEF O_GENERICS}
    fNodes: TList<PXMLNodeArray>;//Memory blocks of 1024 elements. Do not reallocate its memory!
    fFreeIds: TList<XMLNodeId>;
    fTempChildNodes: Array[TXMLChildType] of TObjectDictionary<XMLNodeId,TXMLChildNodeList>;
    {$ELSE}
    fNodes: TList;
    fFreeIds: TList;
    fTempChildNodes: Array[TXMLChildType] of TODictionary;
    {$ENDIF}
    fLastNodeId: XMLNodeId;//highest used NodeId
    fNodesLength: XMLNodeId;//= "Count(fNodes)*1024" - count of allocated nodes
    fBlankDocumentNode: PXMLNode;//the blank document element
    fNullNode: PXMLNode;
    fNullNodeList: IXMLNodeList;
    fWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    fWriterSettings: TOXmlWriterSettings;
    fReaderSettings: TOXmlReaderSettings;

    function FindXMLDeclarationNode(var aXMLDeclarationNode: PXMLNode): Boolean;
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
    function GetNullNode: PXMLNode;
    function GetNullResNodeList: IXMLNodeList;
    function GetDocumentNode: PXMLNode;//absolute root element (= empty document)
    function GetDocumentElement: PXMLNode;//first element in document (=root)
    procedure SetDocumentElement(const aDocumentElement: PXMLNode);
    function GetWriterSettings: TOXmlWriterSettings;
    function GetReaderSettings: TOXmlReaderSettings;

    procedure ClearNodes(const aDisposeNodes: Boolean);
  protected
    procedure FreeNode(const aNode: PXMLNode);
    function CreateNode(const aNodeType: TXmlNodeType; var aNode: PXMLNode): XMLNodeId;
    function GetNode(const aNodeId: XMLNodeId): PXMLNode;
    function GetString(const aStringId: OHashedStringsIndex): OWideString;
    function SetString(const aString: OWideString): OHashedStringsIndex;

    procedure Grow;

    function GetCreateTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType): TXMLChildNodeList;
    function TryGetTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType; var aList: TXMLChildNodeList): Boolean;
    procedure DestroyTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType);
    procedure ClearTempChildNodeLists(const aChildType: TXMLChildType);

    property Loading: Boolean read GetLoading write SetLoading;
    property NullNode: PXMLNode read GetNullNode;
    property NullResNodeList: IXMLNodeList read GetNullResNodeList;

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
    procedure Clear;

  public
    function LoadFromReader(const aReader: TOXmlReader): Boolean;
    function LoadFromFile(const aFileName: String): Boolean;
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFNDEF NEXTGEN}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_DELPHI_2009_UP}
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    procedure SaveToWriter(const aWriter: TOXMLWriter);
    procedure SaveToFile(const aFileName: String);
    procedure SaveToStream(const aStream: TStream);

    {$IFDEF O_DELPHI_2009_UP}
    procedure SaveToBuffer(var aBuffer: TBytes);
    {$ENDIF}
    procedure SaveToXML(var aXML: OWideString);
    {$IFNDEF NEXTGEN}
    procedure SaveToXML_UTF8(var aXML: ORawByteString);
    {$ENDIF}
  public
    function XML: OWideString;
    {$IFNDEF NEXTGEN}
    function XML_UTF8: ORawByteString;
    {$ENDIF}
  public
    property Node: PXMLNode read GetDocumentNode;
    property DocumentElement: PXMLNode read GetDocumentElement write SetDocumentElement;

    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;

    property CodePage: Word read GetCodePage write SetCodePage;
    property Encoding: OWideString read GetEncoding write SetEncoding;
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    property Version: OWideString read GetVersion write SetVersion;
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

    function FindNode(const aName: OWideString): PXMLNode; overload;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var aNode: PXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Remove(const aNode: PXMLNode): Integer;

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var aNode: PXMLNode): Boolean;
    function GetPrevious(var aNode: PXMLNode): Boolean;

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

    function FindNode(const aName: OWideString): PXMLNode; overload;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var aNode: PXMLNode): Integer; overload;
    function Remove(const aNode: PXMLNode): Integer;//important: the node gets automatically destroyed!

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var aNode: PXMLNode): Boolean;
    function GetPrevious(var aNode: PXMLNode): Boolean;

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

    function GetPrevNext(var aNodeEnum: PXMLNode; const aInc: Integer): Boolean;
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

    function FindNode(const aName: OWideString): PXMLNode; overload;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var aNode: PXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Remove(const aNode: PXMLNode): Integer;

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var aNodeEnum: PXMLNode): Boolean;
    function GetPrevious(var aNodeEnum: PXMLNode): Boolean;

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
    procedure GetNodeInfo(const aNode: TXMLXPathNode; var aNodeInfo: TXMLXPathNodeInfo); override;
    function NodeHasAttributes(const aNode: TXMLXPathNode): Boolean; override;
    function NodeFindAttribute(const aNode: TXMLXPathNode; const aAttrName: OWideString): TXMLXPathNode; overload; override;
    procedure GetNodeAttributes(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); override;
    function GetNodeParent(const aNode: TXMLXPathNode): TXMLXPathNode; override;
    function GetNodeDOMDocument(const aNode: TXMLXPathNode): TXMLXPathNode; override;
    function NodeHasChildNodes(const aNode: TXMLXPathNode): Boolean; override;
    procedure GetNodeChildren(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); override;
  end;


function CreateXMLDoc: IXMLDocument; overload;
function CreateXMLDoc(const aRootNodeName: OWideString): IXMLDocument; overload;
function CreateXMLDoc(const aRootNodeName: OWideString; const aAddUTF8Declaration: Boolean): IXMLDocument; overload;

function XMLNodeIdAssigned(const aId: XMLNodeId): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}

const
  XMLNODEID_UNASSIGNED = High(XMLNodeId);

implementation

uses OXmlLng;

function XMLNodeIdAssigned(const aId: XMLNodeId): Boolean;
begin
  Result := aId <> XMLNODEID_UNASSIGNED;
end;

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

function TXMLNode.AddAttribute(const aAttrName,
  aAttrValue: OWideString): PXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and SameText(aAttrName, 'xml:space') then begin
    Self.fPreserveWhiteSpace := OXmlStrToPreserve(aAttrValue);
  end else begin
    if FindAttribute(aAttrName, Result) then begin
      Result.NodeValue := aAttrValue;
    end else begin
      fOwnerDocument.CreateNode(ntAttribute, Result);
      Append(Result, ctAttribute);
      Result.NodeName := aAttrName;
      Result.NodeValue := aAttrValue;
    end;
  end;
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
  fOwnerDocument.CreateNode(aType, {%H-}Result);
  Append(Result, ctChild);
  Result.NodeName := aName;
  Result.NodeValue := aValue;
end;

function TXMLNode.AddXMLDeclaration: PXMLNode;
begin
  Result := AddCustomChild(ntXMLDeclaration, '', '');
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
      ((OwnerDocument.WhiteSpaceHandling = wsAutoTag) and not fPreserveWhiteSpace)
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
        (not Self.fPreserveWhiteSpace) and
        OXmlNeedsPreserveAttribute(xText)
      then
        Self.fPreserveWhiteSpace := True;
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

  if XMLNodeIdAssigned(fFirstChildId[aChildType]) then begin
    //append to the end

    xLastChild := OwnerDocument.GetNode(fLastChildId[aChildType]);
    //set new as next sibling of last child
    xLastChild.fNextSiblingId := aNew.Id;
    //set last id to new
    fLastChildId[aChildType] := aNew.Id;
    //set prev sibling of new child to last
    aNew.fPreviousSiblingId := xLastChild.Id;
  end else begin
    //no children

    fFirstChildId[aChildType] := aNew.Id;
    fLastChildId[aChildType] := aNew.Id;
  end;
  aNew.fParentNodeId := Self.Id;
  aNew.fPreserveWhiteSpace := Self.fPreserveWhiteSpace;
end;

function TXMLNode.AppendChild(const aNewChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  if XMLNodeIdAssigned(aNewChild.fParentNodeId) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  Append(aNewChild, ctChild);
  Result := aNewChild;
end;

procedure TXMLNode.Clear;
begin
  DeleteAttributes(True);
  DeleteChildren(True);
  fNodeNameId := -1;
  fNodeValueId := -1;
end;

procedure TXMLNode.DeleteAttribute(const aAttr: PXMLNode);
begin
  Delete(aAttr, ctAttribute);
end;

procedure TXMLNode.DeleteAttributes(const aDestroyList: Boolean);
var
  xAttribute: PXMLNode;
begin
  xAttribute := nil;
  while GetNextAttribute(xAttribute) do
    OwnerDocument.FreeNode(xAttribute);

  if aDestroyList then
    OwnerDocument.DestroyTempChildNodeList(@Self, ctAttribute);

  fFirstChildId[ctAttribute] := XMLNODEID_UNASSIGNED;
  fLastChildId[ctAttribute] := XMLNODEID_UNASSIGNED;
end;

procedure TXMLNode.DeleteChildren;
var
  xChild: PXMLNode;
begin
  xChild := nil;
  while GetNextChild(xChild) do begin
    OwnerDocument.FreeNode(xChild);
  end;

  if aDestroyList then
    OwnerDocument.DestroyTempChildNodeList(@Self, ctChild);

  fFirstChildId[ctChild] := XMLNODEID_UNASSIGNED;
  fLastChildId[ctChild] := XMLNODEID_UNASSIGNED;
end;

procedure TXMLNode.DeleteSelf;
var
  xP: PXMLNode;
begin
  xP := ParentNode;
  if Assigned(xP) then begin
    if NodeType = ntAttribute then
      xP.DeleteAttribute(@Self)
    else
      xP.DeleteChild(@Self)
  end else begin
    OwnerDocument.FreeNode(@Self);
  end;
end;

procedure TXMLNode.Delete(const aOld: PXMLNode; const aChildType: TXMLChildType);
begin
  Remove(aOld, aChildType);

  OwnerDocument.FreeNode(aOld);
end;

procedure TXMLNode.DeleteAttribute(const aName: OWideString);
var
  xAttr: PXMLNode;
begin
  if not FindAttribute(aName, {%H-}xAttr) then
    Exit;

  DeleteAttribute(xAttr);
end;

procedure TXMLNode.DeleteChild(const aChild: PXMLNode);
begin
  aChild.DeleteChildren(True);

  Delete(aChild, ctChild);
end;

function TXMLNode.FindAttribute(const aName: OWideString;
  var aAttr: PXMLNode): Boolean;
var
  xAttr: PXMLNode;
  xNameId: OHashedStringsIndex;
begin
  if not HasAttributes then begin
    Result := False;
    Exit;
  end;

  xNameId := OwnerDocument.fDictionary.IndexOf(aName);
  if xNameId < 0 then begin
    Result := False;
    Exit;
  end;

  xAttr := nil;
  while GetNextAttribute(xAttr) do
  if xAttr.fNodeNameId = xNameId then begin
    aAttr := xAttr;
    Result := True;
    Exit;
  end;
  Result := False;
  aAttr := nil;
end;

function TXMLNode.FindAttribute(const aName: OWideString;
  var aValue: OWideString): Boolean;
var
  xAttr: PXMLNode;
begin
  Result := FindAttribute(aName, {%H-}xAttr);
  if Result then
    aValue := xAttr.NodeValue
  else
    aValue := '';
end;

function TXMLNode.GetAttribute(const aName: OWideString): OWideString;
begin
  Result := GetAttributeDef(aName, '');
end;

function TXMLNode.GetAttributeCount: Integer;
var
  xList: TXMLChildNodeList;
  xIter: PXMLNode;
begin
  if TryGetChildNodes({%H-}xList, ctAttribute) then begin
    Result := xList.Count;
    Exit;
  end;

  Result := 0;
  xIter := nil;
  while GetNextAttribute(xIter) do
    Inc(Result);
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
  Result := OwnerDocument.GetCreateTempChildNodeList(@Self, ctAttribute);
end;

function TXMLNode.GetChildCount: Integer;
var
  xList: TXMLChildNodeList;
  xIter: PXMLNode;
begin
  if TryGetChildNodes({%H-}xList, ctAttribute) then begin
    Result := xList.Count;
    Exit;
  end;

  Result := 0;
  xIter := nil;
  while GetNextChild(xIter) do
    Inc(Result);
end;

function TXMLNode.GetChildFromBegin(const aIndex: Integer;
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

function TXMLNode.GetChildFromEnd(const aIndex: Integer;
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

function TXMLNode.TryGetChildNodes(var aList: TXMLChildNodeList;
  const aChildType: TXMLChildType): Boolean;
begin
  Result := OwnerDocument.TryGetTempChildNodeList(@Self, aChildType, aList);
end;

function TXMLNode.GetFirstCChild(const aChildType: TXMLChildType): PXMLNode;
begin
  Result := fOwnerDocument.GetNode(fFirstChildId[aChildType]);
end;

function TXMLNode.GetIsTextElement: Boolean;
begin
  Result := (NodeType = ntElement) and
    XMLNodeIdAssigned(fFirstChildId[ctChild]) and
    (fFirstChildId[ctChild] = fLastChildId[ctChild]) and
    (FirstChild.NodeType = ntText);
end;

function TXMLNode.GetLastChild(const aChildType: TXMLChildType): PXMLNode;
begin
  Result := fOwnerDocument.GetNode(fLastChildId[aChildType]);
end;

function TXMLNode.GetNodeName: OWideString;
begin
  Result := fOwnerDocument.GetString(fNodeNameId);
end;

function TXMLNode.GetNextAttribute(var aAttributeEnum: PXMLNode): Boolean;
begin
  Result := GetNextCChild(aAttributeEnum, ctAttribute);
end;

function TXMLNode.GetNextCChild(var aChildEnum: PXMLNode;
  const aChildType: TXMLChildType): Boolean;
begin
  if Assigned(aChildEnum) then
    aChildEnum := aChildEnum.NextSibling
  else
    aChildEnum := GetFirstCChild(aChildType);

  Result := Assigned(aChildEnum);
end;

function TXMLNode.GetNextChild(var aChildEnum: PXMLNode): Boolean;
begin
  Result := GetNextCChild(aChildEnum, ctChild);
end;

function TXMLNode.GetNextSibling: PXMLNode;
begin
  Result := fOwnerDocument.GetNode(fNextSiblingId);
end;

function TXMLNode.GetParentNode: PXMLNode;
begin
  Result := fOwnerDocument.GetNode(fParentNodeId);
end;

function TXMLNode.GetPreviousAttribute(var aAttributeEnum: PXMLNode): Boolean;
begin
  Result := GetPreviousCChild(aAttributeEnum, ctAttribute);
end;

function TXMLNode.GetPreviousCChild(var aChildEnum: PXMLNode;
  const aChildType: TXMLChildType): Boolean;
begin
  if Assigned(aChildEnum) then
    aChildEnum := aChildEnum.PreviousSibling
  else
    aChildEnum := GetLastChild(aChildType);

  Result := Assigned(aChildEnum);
end;

function TXMLNode.GetPreviousChild(var aChildEnum: PXMLNode): Boolean;
begin
  Result := GetPreviousCChild(aChildEnum, ctChild);
end;

function TXMLNode.GetPreviousSibling: PXMLNode;
begin
  Result := fOwnerDocument.GetNode(fPreviousSiblingId);
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
      xChild := nil;
      while GetNextChild(xChild) do
        Result := Result + xChild.Text;
    end;
  end
end;

function TXMLNode.XML: OWideString;
begin
  SaveToXML({%H-}Result);
end;

{$IFNDEF NEXTGEN}
function TXMLNode.XML_UTF8: ORawByteString;
begin
  SaveToXML_UTF8({%H-}Result);
end;
{$ENDIF}

procedure TXMLNode._SetAttribute(const aName, aValue: OWideString);
begin
  AddAttribute(aName, aValue);
end;

function TXMLNode.GetNodeValue: OWideString;
begin
  Result := fOwnerDocument.GetString(fNodeValueId);
end;

function TXMLNode.HasAttribute(const aName: OWideString): Boolean;
var
  x: PXMLNode;
begin
  Result := FindAttribute(aName, {%H-}x);
end;

function TXMLNode.HasAttributes: Boolean;
begin
  Result := XMLNodeIdAssigned(fFirstChildId[ctAttribute]);
end;

function TXMLNode.HasChildNodes: Boolean;
begin
  Result := XMLNodeIdAssigned(fFirstChildId[ctChild]);
end;

procedure TXMLNode.Init(const aId: XMLNodeId; const aNodeType: TXmlNodeType;
  const aOwnerDocument: TXMLDocument);
begin
  fId := aId;
  fNodeType := aNodeType;
  fNodeNameId := -1;
  fNodeValueId := -1;
  fParentNodeId := XMLNODEID_UNASSIGNED;
  fFirstChildId[ctChild] := XMLNODEID_UNASSIGNED;
  fLastChildId[ctChild] := XMLNODEID_UNASSIGNED;
  fFirstChildId[ctAttribute] := XMLNODEID_UNASSIGNED;
  fLastChildId[ctAttribute] := XMLNODEID_UNASSIGNED;
  fNextSiblingId := XMLNODEID_UNASSIGNED;
  fPreviousSiblingId := XMLNODEID_UNASSIGNED;
  fOwnerDocument := aOwnerDocument;
  fPreserveWhiteSpace := False;
end;

procedure TXMLNode.Insert(const aNew, aBeforeNode: PXMLNode; const aChildType: TXMLChildType);
var
  xAfterNode: PXMLNode;
  xList: TXMLChildNodeList;
begin
  if aBeforeNode.fParentNodeId <> Self.Id then
    raise EXmlDOMException.Create(OXmlLng_NodeToInsertNotAChild);

  if TryGetChildNodes({%H-}xList, aChildType) then
    xList.ExtNodeInserted;

  xAfterNode := aBeforeNode.PreviousSibling;
  if Assigned(xAfterNode) then begin
    xAfterNode.fNextSiblingId := aNew.Id;
    aNew.fPreviousSiblingId := xAfterNode.Id;
  end else begin
    aNew.fPreviousSiblingId := XMLNODEID_UNASSIGNED;
  end;

  if fFirstChildId[aChildType] = aBeforeNode.Id then
    fFirstChildId[aChildType] := aNew.Id;
  aBeforeNode.fPreviousSiblingId := aNew.Id;
  aNew.fNextSiblingId := aBeforeNode.Id;

  aNew.fParentNodeId := Self.Id;
  aNew.fPreserveWhiteSpace := Self.fPreserveWhiteSpace;
end;

function TXMLNode.InsertAttribute(const aAttrName, aAttrValue: OWideString;
  const aBeforeAttribute: PXMLNode): PXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and SameText(aAttrName, 'xml:space') then begin
    Self.fPreserveWhiteSpace := OXmlStrToPreserve(aAttrValue);
  end else begin
    DeleteAttribute(aAttrName);

    fOwnerDocument.CreateNode(ntAttribute, Result);

    Insert(Result, aBeforeAttribute, ctAttribute);
    Result.NodeName := aAttrName;
    Result.NodeValue := aAttrValue;
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

  if XMLNodeIdAssigned(aNewChild.fParentNodeId) then
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
  fOwnerDocument.CreateNode(aType, {%H-}Result);
  Insert(Result, aBeforeNode, ctChild);
  Result.NodeName := aName;
  Result.NodeValue := aValue;
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

{$IFDEF O_DELPHI_2009_UP}
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

function TXMLNode.LoadFromFile(const aFileName: String): Boolean;
var
  xFS: TFileStream;
begin
  xFS := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(xFS);
  finally
    xFS.Free;
  end;
end;

function TXMLNode.LoadFromReader(const aReader: TOXmlReader): Boolean;
var
  xReaderNode: TOXmlReaderNode;
  xLastNode, xLastParentNode: PXMLNode;
  xOmitEmptyTextInTheBeginning: Boolean;
begin
  {$IFNDEF FPC}
  Result := False;//to avoid false warnings in Delphi
  {$ENDIF}

  xOmitEmptyTextInTheBeginning := True;

  if not (NodeType in [ntDocument, ntElement]) then
    raise EXmlDOMException.Create(OXmlLng_NodeMustBeDOMDocumentOrElement);

  DeleteChildren(True);

  fOwnerDocument.Loading := True;
  try
    xReaderNode.NodeType := etDocumentStart;
    xReaderNode.NodeName := '';
    xReaderNode.NodeValue := '';

    xLastNode := @Self;
    while aReader.ReadNextNode(xReaderNode) do begin
      case xReaderNode.NodeType of
        etOpenXMLDeclaration: xLastNode := xLastNode.AddXMLDeclaration;
        etXMLDeclarationAttribute, etAttribute: xLastNode.Attributes[xReaderNode.NodeName] := xReaderNode.NodeValue;
        etXMLDeclarationFinishClose, etFinishOpenElementClose, etCloseElement: begin
          xLastParentNode := xLastNode.ParentNode;

          if
            not Assigned(xLastParentNode) or//we reached the very top of the DOM
            ( //break reading after the root element (when reading into a DOM Document)
              (NodeType = ntDocument) and
              (aReader.BreakReading = brAfterDocumentElement) and
              (xLastNode.NodeType = ntElement) and
              (xLastParentNode = fOwnerDocument.fBlankDocumentNode)
            ) or ( //break reading after this element has been closed (when reading into an element)
              (NodeType = ntElement) and
              (xLastNode = @Self)
            )
          then
            Exit;

          xLastNode := xLastParentNode;
        end;
        etOpenElement: begin
          xOmitEmptyTextInTheBeginning := False;
          xLastNode := xLastNode.AddChild(xReaderNode.NodeName);
        end;
        etText:
          if not (xOmitEmptyTextInTheBeginning and OXmlIsWhiteSpace(xReaderNode.NodeValue))
          then//omit empty text before root node
            xLastNode.AddText(xReaderNode.NodeValue);
        etCData: xLastNode.AddCDATASection(xReaderNode.NodeValue);
        etComment: xLastNode.AddComment(xReaderNode.NodeValue);
        etDocType: xLastNode.AddDocType(xReaderNode.NodeValue);
        etProcessingInstruction: xLastNode.AddProcessingInstruction(xReaderNode.NodeName, xReaderNode.NodeValue);
      end;
    end;
  finally
    fOwnerDocument.Loading := False;
    Result := HasAttributes or HasChildNodes;
  end;
end;

function TXMLNode.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
var
  xReader: TOXmlReader;
begin
  xReader := TOXmlReader.Create(aStream, aForceEncoding);
  try
    xReader.Assign(OwnerDocument.fReaderSettings);
    Result := LoadFromReader(xReader);
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

{$IFNDEF NEXTGEN}
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
  if aOld.fParentNodeId <> Self.Id then
    raise EXmlDOMException.Create(OXmlLng_NodeToDeleteNotAChild);

  if TryGetChildNodes({%H-}xList, aChildType) then
    xList.ExtNodeRemoved;

  if fFirstChildId[aChildType] = aOld.Id then
    fFirstChildId[aChildType] := aOld.fNextSiblingId;
  if fLastChildId[aChildType] = aOld.Id then
    fLastChildId[aChildType] := aOld.fPreviousSiblingId;

  xPrev := aOld.PreviousSibling;
  xNext := aOld.NextSibling;
  if Assigned(xPrev) then begin
    if Assigned(xNext) then
      xPrev.fNextSiblingId := xNext.Id
    else
      xPrev.fNextSiblingId := XMLNODEID_UNASSIGNED;
  end;
  if Assigned(xNext) then begin
    if Assigned(xPrev) then
      xNext.fPreviousSiblingId := xPrev.Id
    else
      xNext.fPreviousSiblingId := XMLNODEID_UNASSIGNED;
  end;

  aOld.fParentNodeId := XMLNODEID_UNASSIGNED;
end;

function TXMLNode.RemoveChild(const aOldChild: PXMLNode): PXMLNode;
begin
  Remove(aOldChild, ctChild);
  Result := aOldChild;
end;

function TXMLNode.ReplaceChild(const aNewChild, aOldChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  Result := aOldChild;

  if XMLNodeIdAssigned(aNewChild.fParentNodeId) then
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
  var aNode: PXMLNode): Boolean;
var
  xNodeList: IXMLNodeList;
  xChildType: TXMLChildType;
  xNodeName: OWideString;
begin
  if XPathIsSimpleNode(aXPath, {%H-}xNodeName, {%H-}xChildType) then begin
    //speed optimization without true XPath support
    aNode := nil;
    while GetNextCChild(aNode, xChildType) do
    if aNode.NodeName = xNodeName then begin
      Result := True;
      Exit;
    end;
    aNode := nil;
    Result := False;
  end else begin
    xNodeList := nil;

    Result := SelectNodes(aXPath, xNodeList, 1);
    if Result and (xNodeList.Count > 0) then
      aNode := xNodeList[0]
    else
      aNode := nil;
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
  xWriter: TOXMLWriter;
begin
  xWriter := TOXMLWriter.Create(aStream);
  try
    xWriter.Assign(OwnerDocument.fWriterSettings);
    xWriter.Encoding := GetCreateCodePage(OwnerDocument.CodePage);

    SaveToWriter(xWriter);
  finally
    xWriter.Free;
  end;
end;

procedure TXMLNode.SaveToXML(var aXML: OWideString);
var
  xStream: TMemoryStream;
  xWriter: TOXMLWriter;
begin
  xStream := TMemoryStream.Create;
  try
    xWriter := TOXMLWriter.Create(xStream);
    try
      xWriter.Assign(OwnerDocument.fWriterSettings);
      xWriter.Encoding := TEncoding.OWideStringEncoding;
      xWriter.WriteBOM := False;

      SaveToWriter(xWriter);
    finally
      xWriter.Free;
    end;

    SetLength(aXML, xStream.Size div SizeOf(OWideChar));
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(aXML[1], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

{$IFNDEF NEXTGEN}
procedure TXMLNode.SaveToXML_UTF8(var aXML: ORawByteString);
var
  xStream: TMemoryStream;
  xWriter: TOXMLWriter;
begin
  xStream := TMemoryStream.Create;
  try
    xWriter := TOXMLWriter.Create(xStream);
    try
      xWriter.Assign(OwnerDocument.fWriterSettings);
      xWriter.Encoding := TEncoding.UTF8;
      xWriter.WriteBOM := False;

      SaveToWriter(xWriter);
    finally
      xWriter.Free;
    end;

    SetLength(aXML, xStream.Size);
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(aXML[1], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

function TXMLNode.SelectNode(const aXPath: OWideString): PXMLNode;
begin
  SelectNode(aXPath, {%H-}Result);
end;

function TXMLNode.SelectNodeCreate(const aNodeName: OWideString): PXMLNode;
begin
  if not SelectNode(aNodeName, {%H-}Result) then
    Result := AddChild(aNodeName);
end;

function TXMLNode.SelectNodeNull(const aXPath: OWideString): PXMLNode;
begin
  if not SelectNode(aXPath, {%H-}Result) then
    Result := OwnerDocument.NullNode;
end;

function TXMLNode.SelectNodes(const aXPath: OWideString;
  const aMaxNodeCount: Integer): IXMLNodeList;
begin
  SelectNodes(aXPath, {%H-}Result, aMaxNodeCount);
end;

function TXMLNode.SelectNodes(const aXPath: OWideString;
  var aNodeList: IXMLNodeList; const aMaxNodeCount: Integer): Boolean;
var
  xXPaths: TXMLXPathList;
  xCustomList: TXMLXPathNodeList;
begin
  aNodeList := nil;

  xXPaths := TXMLXPathList.Create;
  try
    xXPaths.LoadFromString(aXPath);

    xCustomList := nil;//must be here -> list will be created in SelectNodes
    Result := xXPaths.SelectNodes(@Self, xCustomList, TXMLXPathDOMAdapter, aMaxNodeCount);
    if Result then
      aNodeList := (IInterface(xCustomList) as IXMLNodeList)
    else
      aNodeList := OwnerDocument.NullResNodeList;
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

  if XMLNodeIdAssigned(aAttr.fParentNodeId) then
    raise EXmlDOMException.Create(OXmlLng_ParentNodeMustBeNil);

  if FindAttribute(aAttr.NodeName, {%H-}Result) then
    Remove(Result, ctAttribute)
  else
    Result := nil;

  Append(aAttr, ctAttribute);
end;

procedure TXMLNode.SetNodeName(const aName: OWideString);
begin
  fNodeNameId := fOwnerDocument.SetString(aName);
end;

procedure TXMLNode.SetNodeValue(const aValue: OWideString);
begin
  if (NodeType = ntText) and (OwnerDocument.WhiteSpaceHandling = wsTrim) then
    fNodeValueId := fOwnerDocument.SetString(Trim(aValue))
  else
    fNodeValueId := fOwnerDocument.SetString(aValue);
end;

procedure TXMLNode.SetText(const aText: OWideString);
begin
  case NodeType of
    ntText, ntCData: NodeValue := aText;
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
  const aWriter: TOXMLWriter);
var
  xChild: PXMLNode;
begin
  xChild := nil;
  while GetNextChild(xChild) do
    xChild.SaveToWriter(aWriter);
end;

procedure TXMLNode.WriteAttributesXML(
  const aWriter: TOXMLWriter);
var
  xAttr: PXMLNode;
begin
  xAttr := nil;
  while GetNextAttribute(xAttr) do
    aWriter.Attribute(xAttr.NodeName, xAttr.NodeValue);

  if (fOwnerDocument.WhiteSpaceHandling = wsAutoTag) and
     (fNodeType = ntElement) and
     (ParentNode.fPreserveWhiteSpace <> Self.fPreserveWhiteSpace)
  then
    aWriter.Attribute('xml:space', OXmlPreserveToStr(Self.fPreserveWhiteSpace));
end;

procedure TXMLNode.SaveToWriter(const aWriter: TOXMLWriter);
begin
  case fNodeType of
    ntDocument: WriteChildrenXML(aWriter);
    ntElement: begin
      aWriter.OpenElement(NodeName);
      WriteAttributesXML(aWriter);
      if HasChildNodes then begin
        aWriter.FinishOpenElement;
        WriteChildrenXML(aWriter);
        aWriter.CloseElement(NodeName,
          (aWriter.IndentType <> itNone) and//speed optimization
          not (//IsTextElement
            (fFirstChildId[ctChild] = fLastChildId[ctChild]) and
            (FirstChild.NodeType = ntText)));
      end else begin
        aWriter.FinishOpenElementClose;
      end;
    end;
    ntXMLDeclaration: begin
      aWriter.OpenXMLDeclaration;
      WriteAttributesXML(aWriter);
      aWriter.FinishOpenXMLDeclaration;
    end;
    ntAttribute: aWriter.Attribute(NodeName, NodeValue);
    ntText: begin
      aWriter.Text(NodeValue,
        //not ParentNode.IsTextElement
        XMLNodeIdAssigned(fNextSiblingId) or XMLNodeIdAssigned(fPreviousSiblingId));
    end;
    ntCData: begin
      aWriter.CData(NodeValue);
    end;
    ntComment: begin
      aWriter.Comment(NodeValue);
    end;
    ntDocType: begin
      aWriter.DocType(NodeValue);
    end;
    ntProcessingInstruction: begin
      aWriter.ProcessingInstruction(NodeName, NodeValue);
    end;
  end;
end;

{$IFDEF O_DELPHI_2009_UP}
procedure TXMLNode.SaveToBuffer(var aBuffer: TBytes);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream);

    SetLength(aBuffer, xStream.Size);
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(aBuffer[0], xStream.Size);
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
    xDec.Attributes['version'] := '1.0';
    xDec.Attributes['encoding'] := 'utf-8';
    xDec.Attributes['standalone'] := 'yes';
  end;

  if aRootNodeName <> '' then
    fBlankDocumentNode.AddChild(aRootNodeName);
end;

function TXMLDocument.CreateAttribute(const aName,
  aValue: OWideString): PXMLNode;
begin
  CreateNode(ntAttribute, {%H-}Result);
  Result.NodeName := aName;
  Result.NodeValue := aValue;
end;

function TXMLDocument.CreateCDATASection(const aData: OWideString): PXMLNode;
begin
  CreateNode(ntCData, {%H-}Result);
  Result.NodeValue := aData;
end;

function TXMLDocument.CreateComment(const aText: OWideString): PXMLNode;
begin
  CreateNode(ntComment, {%H-}Result);
  Result.NodeValue := aText;
end;

function TXMLDocument.CreateDocType(
  const aDocTypeRawText: OWideString): PXMLNode;
begin
  CreateNode(ntDocType, {%H-}Result);
  Result.NodeValue := aDocTypeRawText;
end;

function TXMLDocument.CreateElement(const aNodeName: OWideString): PXMLNode;
begin
  CreateNode(ntElement, {%H-}Result);
  Result.NodeName := aNodeName;
end;

constructor TXMLDocument.Create(aDummy: TObject);
begin
  inherited Create;

  DoCreate;
end;

function TXMLDocument.CreateNode(const aNodeType: TXmlNodeType; var aNode: PXMLNode): XMLNodeId;
begin
  if fFreeIds.Count = 0 then begin
    //use new id
    if XMLNodeIdAssigned(fLastNodeId) then
      Inc(fLastNodeId)
    else
      fLastNodeId := 0;
    if fLastNodeId >= fNodesLength then
      Grow;
    Result := fLastNodeId;
  end else begin
    //use last free id - from the end to be sure no memory must be moved
    Result := {%H-}XMLNodeId(fFreeIds[fFreeIds.Count-1]);
    fFreeIds.Delete(fFreeIds.Count-1);
  end;

  aNode := GetNode(Result);
  aNode.Init(Result, aNodeType, Self);
end;

function TXMLDocument.CreateProcessingInstruction(const aTarget,
  aContent: OWideString): PXMLNode;
begin
  CreateNode(ntProcessingInstruction, {%H-}Result);
  Result.NodeName := aTarget;
  Result.NodeValue := aContent;
end;

function TXMLDocument.GetCreateTempChildNodeList(
  const aParentNode: PXMLNode; const aChildType: TXMLChildType): TXMLChildNodeList;
begin
  if not TryGetTempChildNodeList(aParentNode, aChildType, {%H-}Result) then begin
    Result := TXMLChildNodeList.Create(aParentNode, aChildType);
    {$IFDEF O_GENERICS}
    fTempChildNodes[aChildType].Add(aParentNode.fId, Result);
    {$ELSE}
    fTempChildNodes[aChildType].AddObject(aParentNode.fId, Result);
    {$ENDIF}
  end;
end;

function TXMLDocument.CreateTextNode(const aText: OWideString): PXMLNode;
begin
  CreateNode(ntText, {%H-}Result);
  Result.NodeValue := aText;
end;

function TXMLDocument.CreateXMLDeclaration: PXMLNode;
begin
  CreateNode(ntXMLDeclaration, {%H-}Result);
end;

function TXMLDocument.AddChild(const aElementName: OWideString): PXMLNode;
begin
  Result := Node.AddChild(aElementName);
end;

procedure TXMLDocument.Clear;
begin
  ClearNodes(False);
  CreateNode(ntDocument, fBlankDocumentNode);
end;

procedure TXMLDocument.ClearNodes(const aDisposeNodes: Boolean);
var
  I: Integer;
  C: TXMLChildType;
begin
  fDictionary.Clear;

  fFreeIds.Clear;
  fLastNodeId := XMLNODEID_UNASSIGNED;

  //clear nodes list
  if aDisposeNodes then begin
    fNodesLength := 0;
    for I := 0 to fNodes.Count-1 do
      Dispose(PXMLNodeArray(fNodes[I]));
    fNodes.Clear;
  end;

  //clear temp nodes
  fBlankDocumentNode := nil;
  fNullNode := nil;

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
  fFreeIds.Free;
  for C := Low(C) to High(C) do
    fTempChildNodes[C].Free;

  fWriterSettings.Free;
  fReaderSettings.Free;

  inherited;
end;

procedure TXMLDocument.DestroyTempChildNodeList(const aParentNode: PXMLNode;
  const aChildType: TXMLChildType);
begin
  fTempChildNodes[aChildType].Remove(aParentNode.fId);
end;

procedure TXMLDocument.DoCreate;
var
  C: TXMLChildType;
begin
  fWriterSettings := TOXmlWriterSettings.Create;
  fReaderSettings := TOXmlReaderSettings.Create;

  fDictionary := TOHashedStrings.Create;
  {$IFDEF O_GENERICS}
  fNodes := TList<PXMLNodeArray>.Create;
  fFreeIds := TList<XMLNodeId>.Create;
  for C := Low(C) to High(C) do
    fTempChildNodes[C] := TObjectDictionary<XMLNodeId,TXMLChildNodeList>.Create([doOwnsValues]);
  {$ELSE}
  fNodes := TList.Create;
  fFreeIds := TList.Create;
  for C := Low(C) to High(C) do
    fTempChildNodes[C] := TODictionary.Create(dupIgnore, soAscending, True);
  {$ENDIF}
  fWhiteSpaceHandling := wsPreserveInTextOnly;

  Clear;
end;

function TXMLDocument.FindXMLDeclarationNode(
  var aXMLDeclarationNode: PXMLNode): Boolean;
var
  xChild: PXMLNode;
begin
  if fBlankDocumentNode.HasChildNodes then begin
    xChild := nil;
    while fBlankDocumentNode.GetNextChild(xChild) do
    if (xChild.NodeType = ntXMLDeclaration)
    then begin
      aXMLDeclarationNode := xChild;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
  aXMLDeclarationNode := nil;
end;

procedure TXMLDocument.FreeNode(const aNode: PXMLNode);
begin
  if not XMLNodeIdAssigned(aNode.fId) then
    Exit;

  aNode.Clear;

  {$IFDEF O_GENERICS}
  fFreeIds.Add(aNode.fId);
  {$ELSE}
  fFreeIds.Add({%H-}Pointer(aNode.fId));
  {$ENDIF}

  aNode.fId := XMLNODEID_UNASSIGNED;
  //do not set fNextSiblingId and fPrevSiblingId to -1 so that GetNextChild() in e.g. DeleteChildren worked!!!
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
    Result := xDecNode.Attributes[aAttributeName]
  else
    Result := '';
end;

function TXMLDocument.GetWriterSettings: TOXmlWriterSettings;
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
  if XMLNodeIdAssigned(aNodeId) and (aNodeId <= fLastNodeId) then
    Result := @(PXMLNodeArray(fNodes[aNodeId shr 10])^)[aNodeId and 1023]//= [aNode div 1024][aNode mod 1024]
  else
    Result := nil;
end;

function TXMLDocument.GetNullNode: PXMLNode;
begin
  if not Assigned(fNullNode) then begin
    CreateNode(ntElement, fNullNode);
    fNullNode.fParentNodeId := fBlankDocumentNode.fId;
  end;
  Result := fNullNode;
end;

function TXMLDocument.GetNullResNodeList: IXMLNodeList;
begin
  if not Assigned(fNullNodeList) then
    fNullNodeList := TXMLResNodeList.Create
  else
    fNullNodeList.Clear;

  Result := fNullNodeList;
end;

function TXMLDocument.GetReaderSettings: TOXmlReaderSettings;
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
  const aChildType: TXMLChildType; var aList: TXMLChildNodeList): Boolean;
{$IFNDEF O_GENERICS}
var
  xIndex: Integer;
{$ENDIF}
begin
  {$IFDEF O_GENERICS}
  Result := fTempChildNodes[aChildType].TryGetValue(aParentNode.fId, aList);
  {$ELSE}
  Result := fTempChildNodes[aChildType].Find(aParentNode.fId, {%H-}xIndex);
  if Result then
    aList := TXMLChildNodeList(fTempChildNodes[aChildType].Objects[xIndex])
  else
    aList := nil;
  {$ENDIF}
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

{$IFNDEF NEXTGEN}
function TXMLDocument.XML_UTF8: ORawByteString;
begin
  Result := Node.XML_UTF8;
end;
{$ENDIF}

{$IFDEF O_DELPHI_2009_UP}
procedure TXMLDocument.SaveToBuffer(var aBuffer: TBytes);
begin
  Node.SaveToBuffer(aBuffer);
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

{$IFDEF O_DELPHI_2009_UP}
function TXMLDocument.LoadFromBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromBuffer(aBuffer, aForceEncoding);
end;
{$ENDIF}

function TXMLDocument.LoadFromFile(const aFileName: String): Boolean;
begin
  Clear;
  Result := Node.LoadFromFile(aFileName);
end;

function TXMLDocument.LoadFromReader(const aReader: TOXmlReader): Boolean;
begin
  Result := Node.LoadFromReader(aReader);
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

{$IFNDEF NEXTGEN}
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

procedure TXMLDocument.SaveToXML(var aXML: OWideString);
begin
  Node.SaveToXML(aXML);
end;

{$IFNDEF NEXTGEN}
procedure TXMLDocument.SaveToXML_UTF8(var aXML: ORawByteString);
begin
  Node.SaveToXML_UTF8(aXML);
end;
{$ENDIF}

procedure TXMLDocument.SaveToStream(const aStream: TStream);
begin
  Node.SaveToStream(aStream);
end;

procedure TXMLDocument.SaveToWriter(const aWriter: TOXMLWriter);
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

  xDecNode.Attributes[aAttributeName] := aAttributeValue;
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

function TXMLResNodeList.GetNext(var aNodeEnum: PXMLNode): Boolean;
begin
  Result := GetPrevNext(aNodeEnum, +1);
end;

function TXMLResNodeList.Get(const aIndex: Integer): PXMLNode;
begin
  {$IFDEF O_GENERICS}
  Result := fList.Items[aIndex];
  {$ELSE}
  Result := PXMLNode(fList.Items[aIndex]);
  {$ENDIF}
end;

function TXMLResNodeList.GetPrevious(var aNodeEnum: PXMLNode): Boolean;
begin
  Result := GetPrevNext(aNodeEnum, -1);
end;

function TXMLResNodeList.GetPrevNext(var aNodeEnum: PXMLNode;
  const aInc: Integer): Boolean;
begin
  Result := False;
  if Count = 0 then
    Exit;

  if Assigned(aNodeEnum) then begin
    //get prev/next
    if not(
       (0 <= fIteratorCurrent) and (fIteratorCurrent < Count) and
       (Nodes[fIteratorCurrent] = aNodeEnum))
    then//aNodeEnum is NOT the last iterator -> we have to find it
      fIteratorCurrent := IndexOf(aNodeEnum);

    if (0 <= fIteratorCurrent) and (fIteratorCurrent < Count)
    then begin
      fIteratorCurrent := fIteratorCurrent + aInc;
      Result := (0 <= fIteratorCurrent) and (fIteratorCurrent < Count);
      if Result then
        aNodeEnum := Nodes[fIteratorCurrent]
      else
        aNodeEnum := nil;
    end;
  end else if Count > 0 then begin
    //return first or last element (list must not be not empty)
    if aInc > 0 then
      fIteratorCurrent := 0
    else
      fIteratorCurrent := Count-1;
    aNodeEnum := Nodes[fIteratorCurrent];
    Result := True;
  end;
end;

function TXMLResNodeList.IndexOf(const aName: OWideString): Integer;
var x: PXMLNode;
begin
  Result := IndexOf(aName, {%H-}x);
end;

function TXMLResNodeList.IndexOf(const aName: OWideString;
  var aNode: PXMLNode): Integer;
var
  xNameId: OHashedStringsIndex;
begin
  if Count = 0 then begin
    Result := -1;
    aNode := nil;
    Exit;
  end;

  xNameId := Nodes[0].OwnerDocument.fDictionary.IndexOf(aName);
  if xNameId < 0 then begin
    Result := -1;
    aNode := nil;
    Exit;
  end;

  for Result := 0 to Count-1 do
  if (Nodes[Result].fNodeNameId = xNameId) then begin
    aNode := Nodes[Result];
    Exit;
  end;

  Result := -1;
  aNode := nil;
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
  var aNodeInfo: TXMLXPathNodeInfo);
var
  xNode: PXMLNode;
begin
  xNode := PXMLNode(aNode);
  aNodeInfo.NodeName := xNode.NodeName;
  aNodeInfo.NodeValue := xNode.NodeValue;
  aNodeInfo.NodeType := xNode.NodeType;
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
  fParent.Delete(aNode, fChildType);
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
  Result := fParent.OwnerDocument.GetNode(fParent.fFirstChildId[fChildType]);
end;

function TXMLChildNodeList.GetLast: PXMLNode;
begin
  Result := fParent.OwnerDocument.GetNode(fParent.fLastChildId[fChildType]);
end;

function TXMLChildNodeList.GetNext(var aNode: PXMLNode): Boolean;
begin
  if aNode = nil then
    aNode := GetFirst
  else
    aNode := aNode.GetNextSibling;
  Result := Assigned(aNode);
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

function TXMLChildNodeList.GetPrevious(var aNode: PXMLNode): Boolean;
begin
  if aNode = nil then
    aNode := GetLast
  else
    aNode := aNode.GetPreviousSibling;
  Result := Assigned(aNode);
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
  var aNode: PXMLNode): Integer;
var
  xNameId: OHashedStringsIndex;
begin
  Result := -1;
  aNode := nil;//must be here

  xNameId := fParent.OwnerDocument.fDictionary.IndexOf(aName);
  if xNameId < 0 then
    Exit;

  //aNode was set to nil at the start of the function
  while GetNext(aNode) do begin
    Inc(Result);
    if xNameId = aNode.fNodeNameId then
      Exit;
  end;

  Result := -1;
  aNode := nil;
end;

procedure TXMLChildNodeList.Insert(const aIndex: Integer;
  const aNode: PXMLNode);
var
  xNode: PXMLNode;
begin
  xNode := Nodes[aIndex];
  fParent.Insert(aNode, xNode, fChildType);
end;

function TXMLChildNodeList.Remove(const aNode: PXMLNode): Integer;
begin
  Result := IndexOf(aNode);
  Delete(aNode);
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
