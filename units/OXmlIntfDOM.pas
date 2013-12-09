unit OXmlIntfDOM;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlIntfDOM.pas

  XML DOM interface implementation

  !!! Deprecated, only for testing purposes, please use OXmlPDOM.pas !!!

  Simplified W3C DOM (Core) Level 1 specification:
    http://www.w3.org/TR/REC-DOM-Level-1/level-one-core.html

  Very close to MS XML and OmniXML interface DOM. OXmlIntfDOM should be faster
  than both of them.

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
  {$IFDEF O_GENERICS}, Generics.Collections, Generics.Defaults{$ENDIF}
  ;

type
  IXMLNode = interface;
  IXMLDocument = interface;
  IXMLNodeList = interface;
  IXMLAttrList = interface;

  IXMLNode = interface
    ['{FE2408FD-05CD-41E5-9DEF-28C18DB6FFAC}']

    //protected
    function GetDictionary: TOHashedStrings;

    function GetAttribute(const aAttrName: OWideString): OWideString;
    function GetAttributeDef(const aName, aDefaultValue: OWideString): OWideString;
    procedure SetAttribute(const aAttrName, aAttrValue: OWideString);
    function GetAttributeNodes: IXMLAttrList;
    function GetChildNodes: IXMLNodeList;
    function GetHasChildNodes: Boolean;
    function GetHasAttributes: Boolean;
    function GetAttributeCount: Integer;
    function GetChildCount: Integer;

    function GetNodeName: OWideString;
    function GetNodeNameId: OHashedStringsIndex;
    function GetNodeValue: OWideString;
    function GetNodeType: TXmlNodeType;
    procedure SetNodeName(const aName: OWideString);
    procedure SetNodeValue(const aValue: OWideString);
    function GetText: OWideString;
    procedure SetText(const aText: OWideString);
    function GetPreserveWhiteSpace: Boolean;
    procedure SetPreserveWhiteSpace(const aPreserveWhiteSpace: Boolean);

    function GetOwnerDocument: IXMLDocument;
    function GetParentNode: IXMLNode;
    procedure SetParentNode(const aParentNode: IXMLNode);

    procedure WriteXML(const aOutputWriter: TOXMLWriterIndentation);

    property NameId: OHashedStringsIndex read GetNodeNameId;
    property Dictionary: TOHashedStrings read GetDictionary;

    // public
    procedure DeleteChildren;
    procedure DeleteAttributes;
    //clear: delete all attributes and child nodes, set name and value to empty string
    procedure Clear;

    //insert a node before another
    //  Inserts the node aNewChild before the existing child node aRefChild.
    //  If aRefChild is null, insert aNewChild at the end of the list of children.
    //  If the aNewChild is already in the tree, it is first removed.
    function InsertBefore(const aNewChild, aRefChild: IXMLNode): IXMLNode;
    //replace a child
    //  Replaces the child node oldChild with aNewChild in the list of children, and returns the aOldChild node.
    //  If the aNewChild is already in the tree, it is first removed.
    function ReplaceChild(const aNewChild, aOldChild: IXMLNode): IXMLNode;
    //remove a child
    //  Removes the child node indicated by aOldChild from the list of children, and returns it.
    function RemoveChild(const aOldChild: IXMLNode): IXMLNode;
    //append a child
    //  Adds the node aNewChild to the end of the list of children of this node.
    //  If the aNewChild is already in the tree, it is first removed.
    function AppendChild(const aNewChild: IXMLNode): IXMLNode;
    //get attribute node by name
    function GetAttributeNode(const aAttrName: OWideString): IXMLNode;
    //set attribute
    //  if the aAttr replaces an existing attribute with the same name, the previously existing Attr node is returned, otherwise nil is returned.
    function SetAttributeNode(const aAttr: IXMLNode): IXMLNode;

    //add/insert a child of some type to current element
    function AddChild(const aNodeName: OWideString; Index: Integer = -1): IXMLNode;
    function AddAttribute(const aName, aValue: OWideString): IXMLNode;
    function AddXMLDeclaration: IXMLNode;
    function AddText(const aText: OWideString): IXMLNode;
    function AddCDATASection(const aText: OWideString): IXMLNode;
    function AddComment(const aText: OWideString): IXMLNode;
    function AddDocType(const aDocTypeRawText: OWideString): IXMLNode;
    function AddProcessingInstruction(const aTarget, aContent: OWideString): IXMLNode;

    function InsertElement(const aNodeName: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertAttribute(const aName, aValue: OWideString; const aBeforeAttributeName: OWideString): IXMLNode;
    function InsertXMLDeclaration(const aBeforeNode: IXMLNode): IXMLNode;
    function InsertText(const aText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertCDATASection(const aText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertComment(const aText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertDocType(const aDocTypeRawText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertProcessingInstruction(const aTarget, aContent: OWideString; const aBeforeNode: IXMLNode): IXMLNode;

    //delete all child nodes by name
    procedure RemoveChildren(const aNodeName: OWideString);

    //returns true if element has some child nodes
    property HasChildNodes: Boolean read GetHasChildNodes;
    //returns true if element has some attributes
    property HasAttributes: Boolean read GetHasAttributes;
    //returns true if attribute with the name aAttrName exists
    function HasAttribute(const aAttrName: OWideString): Boolean;

    function GetFirstChild: IXMLNode;
    function GetLastChild: IXMLNode;
    function GetFirstAttribute: IXMLNode;
    function GetLastAttribute: IXMLNode;
    function GetNextSibling: IXMLNode;
    function GetPreviousSibling: IXMLNode;

    //iterate through all children from first to last (get first for aChildEnum=nil)
    function GetNextChild(var aChildEnum: IXMLNode): Boolean;
    //iterate through all attributes from first to last (get first for aAttributeEnum=nil)
    function GetNextAttribute(var aAttributeEnum: IXMLNode): Boolean;
    //iterate through all children from last to first (get last for aChildEnum=nil)
    function GetPreviousChild(var aChildEnum: IXMLNode): Boolean;
    //iterate through all attributes from last to first (get last for aAttributeEnum=nil)
    function GetPreviousAttribute(var aAttributeEnum: IXMLNode): Boolean;

    //select the first node by XPath, if not found return false (and aNode=nil)
    function SelectNode(const aXPath: OWideString; var aNode: IXMLNode): Boolean; overload;
    //select the first node by XPath, if not found return nil
    function SelectNode(const aXPath: OWideString): IXMLNode; overload;
    //select the first node by XPath, if not found return a "null" node (no name, no value)
    function SelectNodeNull(const aXPath: OWideString): IXMLNode;
    //select the first child element by name (not XPath!!!),
    //  if not found the element is created, appended to current node and returned
    function SelectNodeCreate(const aNodeName: OWideString): IXMLNode;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return false and aNodeList=nil
    function SelectNodes(const aXPath: OWideString;
      var aNodeList: IXMLNodeList;
      const aMaxNodeCount: Integer = 0): Boolean; overload;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return an empty IXMLNodeList (but not nil)
    function SelectNodes(const aXPath: OWideString;
      const aMaxNodeCount: Integer = 0): IXMLNodeList; overload;

  //public
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
    //get attribute value from name, if not found return empty string ('')
    property Attributes[const aAttrName: OWideString]: OWideString read GetAttribute write SetAttribute;
    //atribute nodes - may be undefined for some node types
    property AttributeNodes: IXMLAttrList read GetAttributeNodes;
    property AttributeCount: Integer read GetAttributeCount;
    //element children - may be undefined for some node types
    property ChildNodes: IXMLNodeList read GetChildNodes;
    property ChildCount: Integer read GetChildCount;

    //parent element
    property ParentNode: IXMLNode read GetParentNode;
    //owner document
    property OwnerDocument: IXMLDocument read GetOwnerDocument;

    property FirstChild: IXMLNode read GetFirstChild;
    property LastChild: IXMLNode read GetLastChild;
    property FirstAttribute: IXMLNode read GetFirstAttribute;
    property LastAttribute: IXMLNode read GetLastAttribute;
    property NextSibling: IXMLNode read GetNextSibling;
    property PreviousSibling: IXMLNode read GetPreviousSibling;

    property PreserveWhiteSpace: Boolean read GetPreserveWhiteSpace write SetPreserveWhiteSpace;
    //node text - get all child (and grandchild...) text/cdata element texts
    property Text: OWideString read GetText write SetText;
    //node name - may be undefined for some node types
    property NodeName: OWideString read GetNodeName write SetNodeName;
    //node value - may be undefined for some node types
    property NodeValue: OWideString read GetNodeValue write SetNodeValue;
    //node type
    property NodeType: TXmlNodeType read GetNodeType;
  end;

  IXMLDocument = interface(IXMLCustomDocument)
    ['{9B4371A0-839B-4580-A54D-0E71AA6B4A9A}']

  //protected
    function GetNullNode: IXMLNode;
    function GetDOMDocument: IXMLNode;
    function GetDocumentNode: IXMLNode;
    procedure SetDocumentNode(const aDocumentNode: IXMLNode);

    property NullNode: IXMLNode read GetNullNode;

  //public
    function CreateAttribute(const aName: OWideString; const aValue: OWideString = ''): IXMLNode;
    function CreateElement(const aNodeName: OWideString): IXMLNode;
    function CreateXMLDeclaration: IXMLNode;
    function CreateTextNode(const aText: OWideString): IXMLNode;
    function CreateCDATASection(const aData: OWideString): IXMLNode;
    function CreateComment(const aText: OWideString): IXMLNode;
    function CreateDocType(const aDocTypeRawText: OWideString): IXMLNode;
    function CreateProcessingInstruction(const aTarget, aContent: OWideString): IXMLNode;

  //public

    //returns the very document node (parent of the DocumentNode)
    property DOMDocument: IXMLNode read GetDOMDocument;
    //returns the root node (first element in the document)
    property DocumentNode: IXMLNode read GetDocumentNode write SetDocumentNode;
  end;

  TXMLNodeListEnumerator = class(TObject)
  private
    fIndex: Integer;
    fList: IXMLNodeList;
  public
    constructor Create(aList: IXMLNodeList);
    function GetCurrent: IXMLNode;
    function MoveNext: Boolean;
  public
    property Current: IXMLNode read GetCurrent;
  end;

  IXMLNodeList = interface
    ['{E6D8B5B5-A4CD-4F40-A4F4-1F61DB436EA0}']

    //protected
    function GetCount: Integer;
    function GetNode(const Index: Integer): IXMLNode;

    //public
    function Add(const aNode: IXMLNode): Integer;
    function AddNode(const aNode: IXMLNode): IXMLNode;
    procedure Clear;
    procedure Delete(const aNode: IXMLNode); overload;
    function Delete(const aName: OWideString): IXMLNode; overload;
    procedure Delete(const Index: Integer); overload;

    function FindNode(const aName: OWideString): IXMLNode; overload;

    function IndexOf(const aNode: IXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var aNode: IXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: IXMLNode);
    function Remove(const aNode: IXMLNode): Integer;

    function GetFirst: IXMLNode;
    function GetLast: IXMLNode;
    function GetNext(var aNodeEnum: IXMLNode): Boolean;
    function GetPrevious(var aNodeEnum: IXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLNodeListEnumerator;
    {$ENDIF}

    property Nodes[const Index: Integer]: IXMLNode read GetNode; default;
    property Count: Integer read GetCount;
  end;

  IXMLAttrList = interface(IXMLNodeList)
    ['{CFB58897-FB86-4549-A9BD-264616B69876}']

    // public
    function GetValue(const aName: OWideString): OWideString;
    procedure SetValue(const aName, aValue: OWideString);

    property Values[const aName: OWideString]: OWideString read GetValue write SetValue;
  end;

  TXMLDocument = class;

  TXMLNode = class(TInterfacedObject, IXMLNode)
  private
    {$IFDEF NEXTGEN}
    [weak] fOwnerDocument: TXMLDocument;//AVOID CIRCULAR REFERENCES
    [weak] fParentNode: IXMLNode;//AVOID CIRCULAR REFERENCES
    {$ELSE}
    fOwnerDocument: TXMLDocument;//AVOID CIRCULAR REFERENCES
    fParentNode: IXMLNode;//AVOID CIRCULAR REFERENCES
    {$ENDIF}
  protected
    function GetDictionary: TOHashedStrings;
    procedure WriteXML(const aOutputWriter: TOXMLWriterIndentation); virtual; abstract;

    function GetText: OWideString; virtual;
    procedure SetText(const aText: OWideString); virtual;
    function GetNodeNameId: OHashedStringsIndex; virtual;
    function GetNodeValueId: OHashedStringsIndex; virtual;
    function GetPreserveWhiteSpace: Boolean; virtual;
    procedure SetNodeNameId(const {%H-}aNodeNameId: OHashedStringsIndex); virtual;
    procedure SetNodeValueId(const {%H-}aNodeValueId: OHashedStringsIndex); virtual;
    procedure SetPreserveWhiteSpace(const {%H-}aPreserveWhiteSpace: Boolean); virtual;
    function GetHasAttributes: Boolean; virtual;
    function GetHasChildNodes: Boolean; virtual;

    function GetAttributeNodes: IXMLAttrList; virtual;
    function GetChildNodes: IXMLNodeList; virtual;
    function GetAttributeCount: Integer; virtual;
    function GetChildCount: Integer; virtual;

    property Dictionary: TOHashedStrings read GetDictionary;
  protected
    function GetOwnerDocument: IXMLDocument;
    function GetParentNode: IXMLNode;
    procedure SetParentNode(const aParentNode: IXMLNode);
  protected
    function GetAttribute(const aAttrName: OWideString): OWideString;
    function GetAttributeDef(const aName, aDefaultValue: OWideString): OWideString;
    procedure SetAttribute(const aAttrName, aAttrValue: OWideString);

    function GetNodeName: OWideString; virtual;
    function GetNodeValue: OWideString; virtual;
    procedure SetNodeName(const aName: OWideString);
    procedure SetNodeValue(const aValue: OWideString);
    function GetNodeType: TXmlNodeType; virtual; abstract;

    procedure WriteChildrenXML(const aOutputWriter: TOXMLWriterIndentation);
    procedure WriteAttributesXML(const aOutputWriter: TOXMLWriterIndentation);
  public
    constructor Create(const aOwnerDocument: TXMLDocument; const aName, aValue: OWideString);
    destructor Destroy; override;
  public
    procedure DeleteChildren; virtual;
    procedure DeleteAttributes; virtual;
    procedure Clear;

    function InsertBefore(const aNewChild, aRefChild: IXMLNode): IXMLNode;
    function ReplaceChild(const aNewChild, aOldChild: IXMLNode): IXMLNode;
    function RemoveChild(const aOldChild: IXMLNode): IXMLNode;
    function AppendChild(const aNewChild: IXMLNode): IXMLNode;
    function GetAttributeNode(const aAttrName: OWideString): IXMLNode;
    function SetAttributeNode(const aAttr: IXMLNode): IXMLNode;

    function AddChild(const aNodeName: OWideString; Index: Integer = -1): IXMLNode;
    function AddAttribute(const aName, aValue: OWideString): IXMLNode;
    function AddText(const aText: OWideString): IXMLNode;
    function AddCDATASection(const aText: OWideString): IXMLNode;
    function AddComment(const aText: OWideString): IXMLNode;
    function AddDocType(const aDocTypeRawText: OWideString): IXMLNode;
    function AddProcessingInstruction(const aTarget, aContent: OWideString): IXMLNode;
    function AddXMLDeclaration: IXMLNode;

    function InsertElement(const aNodeName: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertAttribute(const aName, aValue: OWideString; const aBeforeAttributeName: OWideString): IXMLNode;
    function InsertXMLDeclaration(const aBeforeNode: IXMLNode): IXMLNode;
    function InsertText(const aText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertCDATASection(const aText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertComment(const aText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertDocType(const aDocTypeRawText: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
    function InsertProcessingInstruction(const aTarget, aContent: OWideString; const aBeforeNode: IXMLNode): IXMLNode;

    procedure RemoveChildren(const aNodeName: OWideString);

    function GetFirstChild: IXMLNode;
    function GetLastChild: IXMLNode;
    function GetFirstAttribute: IXMLNode;
    function GetLastAttribute: IXMLNode;
    function GetNextSibling: IXMLNode;
    function GetPreviousSibling: IXMLNode;
    function GetNextChild(var aChildEnum: IXMLNode): Boolean;
    function GetNextAttribute(var aAttributeEnum: IXMLNode): Boolean;
    function GetPreviousChild(var aChildEnum: IXMLNode): Boolean;
    function GetPreviousAttribute(var aAttributeEnum: IXMLNode): Boolean;

    property HasChildNodes: Boolean read GetHasChildNodes;
    property HasAttributes: Boolean read GetHasAttributes;
    function HasAttribute(const aAttrName: OWideString): Boolean;

    function SelectNode(const aXPath: OWideString; var aNode: IXMLNode): Boolean; overload;
    function SelectNode(const aXPath: OWideString): IXMLNode; overload;
    function SelectNodeNull(const aXPath: OWideString): IXMLNode;
    function SelectNodeCreate(const aNodeName: OWideString): IXMLNode;
    function SelectNodes(const aXPath: OWideString;
      var aNodeList: IXMLNodeList;
      const aMaxNodeCount: Integer = 0): Boolean; overload;
    function SelectNodes(const aXPath: OWideString;
      const aMaxNodeCount: Integer = 0): IXMLNodeList; overload;

  public
    function LoadFromFile(const aFileName: String): Boolean;
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFNDEF NEXTGEN}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_DELPHI_2009_UP}
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    procedure SaveToFile(const aFileName: String; const aOutputFormat: TXmlOutputFormat = ofNone);
    procedure SaveToStream(const aStream: TStream; const aOutputFormat: TXmlOutputFormat = ofNone); overload;
    procedure SaveToStream(const aStream: TStream; const aOutputFormat: TXmlOutputFormat;
      const aForceEncoding: TEncoding; const aWriteBOM: Boolean); overload;
    procedure SaveToXML(var aXML: OWideString; const aOutputFormat: TXmlOutputFormat);
    {$IFNDEF NEXTGEN}
    procedure SaveToXML_UTF8(var aXML: ORawByteString; const aOutputFormat: TXmlOutputFormat);
    {$ENDIF}

    {$IFDEF O_DELPHI_2009_UP}
    procedure SaveToBuffer(var aBuffer: TBytes; const aOutputFormat: TXmlOutputFormat); overload;
    procedure SaveToBuffer(var aBuffer: TBytes; const aOutputFormat: TXmlOutputFormat;
      const aForceEncoding: TEncoding; const aWriteBOM: Boolean); overload;
    {$ENDIF}

    function XML(const aOutputFormat: TXmlOutputFormat = ofNone): OWideString;
    {$IFNDEF NEXTGEN}
    function XML_UTF8(const aOutputFormat: TXmlOutputFormat = ofNone): ORawByteString;
    {$ENDIF}
  public
    property ParentNode: IXMLNode read GetParentNode;
    property OwnerDocument: IXMLDocument read GetOwnerDocument;

    property Attributes[const aAttrName: OWideString]: OWideString read GetAttribute write SetAttribute;
    property AttributeNodes: IXMLAttrList read GetAttributeNodes;
    property AttributeCount: Integer read GetAttributeCount;
    property ChildNodes: IXMLNodeList read GetChildNodes;
    property ChildCount: Integer read GetChildCount;

    property FirstChild: IXMLNode read GetFirstChild;
    property LastChild: IXMLNode read GetLastChild;
    property FirstAttribute: IXMLNode read GetFirstAttribute;
    property LastAttribute: IXMLNode read GetLastAttribute;
    property NextSibling: IXMLNode read GetNextSibling;
    property PreviousSibling: IXMLNode read GetPreviousSibling;

    property NodeName: OWideString read GetNodeName write SetNodeName;
    property NodeValue: OWideString read GetNodeValue write SetNodeValue;
    property NodeType: TXmlNodeType read GetNodeType;
    property Text: OWideString read GetText write SetText;
    property PreserveWhiteSpace: Boolean read GetPreserveWhiteSpace write SetPreserveWhiteSpace;
  end;

  TXMLAttribute = class(TXMLNode)
  private
    fNodeNameId: OHashedStringsIndex;
    fNodeValueId: OHashedStringsIndex;
  protected
    function GetNodeNameId: OHashedStringsIndex; override;
    procedure SetNodeNameId(const aNodeNameId: OHashedStringsIndex); override;
    function GetNodeValueId: OHashedStringsIndex; override;
    procedure SetNodeValueId(const aNodeValueId: OHashedStringsIndex); override;

    function GetNodeType: TXmlNodeType; override;
    procedure WriteXML(const aOutputWriter: TOXMLWriterIndentation); override;
  end;

  TXMLElement = class(TXMLNode)
  private
    fNodeNameId: OHashedStringsIndex;
    fPreserveWhiteSpace: Boolean;

    fAttributeNodes: IXMLAttrList;
    fChildNodes: IXMLNodeList;
  protected
    function GetNodeNameId: OHashedStringsIndex; override;
    procedure SetNodeNameId(const aNodeNameId: OHashedStringsIndex); override;
    function GetPreserveWhiteSpace: Boolean; override;
    procedure SetPreserveWhiteSpace(const aPreserveWhiteSpace: Boolean); override;
    function GetHasAttributes: Boolean; override;
    function GetHasChildNodes: Boolean; override;

    function GetNodeType: TXmlNodeType; override;
    procedure WriteXML(const aOutputWriter: TOXMLWriterIndentation); override;

    function GetAttributeNodes: IXMLAttrList; override;
    function GetChildNodes: IXMLNodeList; override;
    function GetAttributeCount: Integer; override;
    function GetChildCount: Integer; override;
  public
    procedure DeleteChildren; override;
    procedure DeleteAttributes; override;
  public
    constructor Create(const aOwnerDocument: TXMLDocument; const aNodeName: OWideString);
  end;

  TXMLDeclaration = class(TXMLElement)
  protected
    function GetNodeType: TXmlNodeType; override;
    procedure WriteXML(const aOutputWriter: TOXMLWriterIndentation); override;
  public
    constructor Create(const aOwnerDocument: TXMLDocument);
  end;

  TXMLCharacterData = class(TXMLNode)
  private
    fNodeValueId: OHashedStringsIndex;
  protected
    function GetNodeValueId: OHashedStringsIndex; override;
    procedure SetNodeValueId(const aNodeValueId: OHashedStringsIndex); override;

    function GetText: OWideString; override;
    procedure SetText(const aText: OWideString); override;
  public
    constructor Create(const aOwnerDocument: TXMLDocument; const aCharacterData: OWideString);
  end;

  TXMLText = class(TXMLCharacterData)
  protected
    function GetNodeType: TXmlNodeType; override;
    procedure WriteXML(const aOutputWriter: TOXmlWriterIndentation); override;
    procedure SetText(const aText: OWideString); override;
  end;

  TXMLCDATASection = class(TXMLCharacterData)
  protected
    procedure WriteXML(const aOutputWriter: TOXmlWriterIndentation); override;
    function GetNodeType: TXmlNodeType; override;
  end;

  TXMLComment = class(TXMLCharacterData)
  protected
    procedure WriteXML(const aOutputWriter: TOXmlWriterIndentation); override;
    function GetNodeType: TXmlNodeType; override;
    function GetText: OWideString; override;
    procedure SetText(const {%H-}aText: OWideString); override;
  end;

  TXMLDocType = class(TXMLCharacterData)
  protected
    procedure WriteXML(const aOutputWriter: TOXmlWriterIndentation); override;
    function GetNodeType: TXmlNodeType; override;
    function GetText: OWideString; override;
    procedure SetText(const {%H-}aText: OWideString); override;
  end;

  TXMLProcessingInstruction = class(TXMLAttribute)
  protected
    procedure WriteXML(const aOutputWriter: TOXmlWriterIndentation); override;
    function GetNodeType: TXmlNodeType; override;
    function GetText: OWideString; override;
    procedure SetText(const {%H-}aText: OWideString); override;
  public
    constructor Create(const aOwnerDocument: TXMLDocument; const aTarget, aContent: OWideString);
  end;

  TXMLDOMDocument = class(TXMLElement)
  protected
    function GetNodeName: OWideString; override;
    procedure WriteXML(const aOutputWriter: TOXMLWriterIndentation); override;
    function GetNodeType: TXmlNodeType; override;
  end;

  TXMLDocument = class(TInterfacedObject, IXMLDocument)
  private
    fDictionary: TOHashedStrings;
    fLoading: Boolean;
    fDOMDocument: IXMLNode;
    fNullNode: IXMLNode;
    fWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    fStrictXML: Boolean;
    fBreakReading: TXmlBreakReading;
  protected
    function GetLoading: Boolean;
    procedure SetLoading(const aLoading: Boolean);
    function GetDOMDocument: IXMLNode;
    function GetDocumentNode: IXMLNode;
    procedure SetDocumentNode(const aDocumentNode: IXMLNode);

    function FindXMLDeclarationNode(var aXMLDeclarationNode: IXMLNode): Boolean;
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
    function GetStrictXML: Boolean;
    procedure SetStrictXML(const aStrictXML: Boolean);
    function GetBreakReading: TXmlBreakReading;
    procedure SetBreakReading(const aBreakReading: TXmlBreakReading);

    function GetNullNode: IXMLNode;

    function GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    procedure SetWhiteSpaceHandling(const Value: TXmlWhiteSpaceHandling);
  protected
    property NullNode: IXMLNode read GetNullNode;
    property Loading: Boolean read GetLoading write SetLoading;

    procedure DoCreate; virtual;
  public
    constructor Create({%H-}aParent: TObject); overload;//aParent to ge ignored - MSXML compatibility
    constructor Create(const aRootNodeName: OWideString = ''; const aAddUTF8Declaration: Boolean = False); overload;
    destructor Destroy; override;
  public
    function CreateAttribute(const aName: OWideString; const aValue: OWideString = ''): IXMLNode;
    function CreateElement(const aNodeName: OWideString): IXMLNode;
    function CreateXMLDeclaration: IXMLNode;
    function CreateTextNode(const aText: OWideString): IXMLNode;
    function CreateCDATASection(const aData: OWideString): IXMLNode;
    function CreateComment(const aText: OWideString): IXMLNode;
    function CreateDocType(const aDocTypeRawText: OWideString): IXMLNode;
    function CreateProcessingInstruction(const aTarget, aContent: OWideString): IXMLNode;
  public
    procedure Clear;

    function LoadFromFile(const aFileName: String): Boolean;
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromXML(const aXML: OWideString): Boolean;
    {$IFNDEF NEXTGEN}
    function LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
    {$ENDIF}
    {$IFDEF O_DELPHI_2009_UP}
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
    {$ENDIF}

    procedure SaveToFile(const aFileName: String; const aOutputFormat: TXmlOutputFormat = ofNone);
    procedure SaveToStream(const aStream: TStream; const aOutputFormat: TXmlOutputFormat = ofNone); overload;
    procedure SaveToStream(const aStream: TStream; const aOutputFormat: TXmlOutputFormat;
      const aForceEncoding: TEncoding; const aWriteBOM: Boolean); overload;
    procedure SaveToXML(var aXML: OWideString; const aOutputFormat: TXmlOutputFormat);
    {$IFNDEF NEXTGEN}
    procedure SaveToXML_UTF8(var aXML: ORawByteString; const aOutputFormat: TXmlOutputFormat);
    {$ENDIF}
    {$IFDEF O_DELPHI_2009_UP}
    procedure SaveToBuffer(var aBuffer: TBytes; const aOutputFormat: TXmlOutputFormat); overload;
    procedure SaveToBuffer(var aBuffer: TBytes; const aOutputFormat: TXmlOutputFormat;
      const aForceEncoding: TEncoding; const aWriteBOM: Boolean); overload;
    {$ENDIF}

    function XML(const aOutputFormat: TXmlOutputFormat = ofNone): OWideString;
    {$IFNDEF NEXTGEN}
    function XML_UTF8(const aOutputFormat: TXmlOutputFormat = ofNone): ORawByteString;
    {$ENDIF}
  public
    property DOMDocument: IXMLNode read GetDOMDocument;
    property DocumentNode: IXMLNode read GetDocumentNode write SetDocumentNode;
    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;
    property StrictXML: Boolean read GetStrictXML write SetStrictXML;
    property BreakReading: TXmlBreakReading read GetBreakReading write SetBreakReading;

    property CodePage: Word read GetCodePage write SetCodePage;
    property Encoding: OWideString read GetEncoding write SetEncoding;
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    property Version: OWideString read GetVersion write SetVersion;
  end;

  TXMLNodeList = class(TInterfacedObject, IXMLNodeList)//do not use it manually, use TXMLRefNodeList instead!
  private
    {$IFDEF O_GENERICS}
    fList: TList<IXMLNode>;
    {$ELSE}
    fList: TList;
    {$ENDIF}

    {$IFDEF NEXTGEN}
    [weak] fParentNode: IXMLNode;//AVOID CIRCULAR REFERENCES
    {$ELSE}
    fParentNode: IXMLNode;//AVOID CIRCULAR REFERENCES
    {$ENDIF}
    fIteratorCurrent: Integer;//for fast Next & Prev

    function GetParentNode: IXMLNode;
    procedure SetParentNode(const aParentNode: IXMLNode);

    function GetPrevNext(var aNodeEnum: IXMLNode; const aInc: Integer): Boolean;
  protected
    function GetCount: Integer;

    property ParentNode: IXMLNode read GetParentNode;
  public
    function GetNode(const Index: Integer): IXMLNode;
  public
    constructor Create(const aParentNode: IXMLNode);
    destructor Destroy; override;
  public
    function Add(const aNode: IXMLNode): Integer;
    function AddNode(const aNode: IXMLNode): IXMLNode;
    function IndexOf(const aNode: IXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var aNode: IXMLNode): Integer; overload;
    procedure Insert(const Index: Integer; const aNode: IXMLNode);
    procedure Delete(const aNode: IXMLNode); overload;
    function Delete(const aName: OWideString): IXMLNode; overload;
    procedure Delete(const aIndex: Integer); overload;
    function Remove(const aNode: IXMLNode): Integer;
    procedure Clear;
    function FindNode(const aName: OWideString): IXMLNode;

    function GetFirst: IXMLNode;
    function GetLast: IXMLNode;
    function GetNext(var aNodeEnum: IXMLNode): Boolean;
    function GetPrevious(var aNodeEnum: IXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLNodeListEnumerator;
    {$ENDIF}
  public
    property Nodes[const Index: Integer]: IXMLNode read GetNode; default;
    property Count: Integer read GetCount;
  end;

  TXMLAttributeList = class(TXMLNodeList, IXMLAttrList)//do not use it manually, use TXMLRefAttributeList
  public
    function GetValue(const aName: OWideString): OWideString;
    procedure SetValue(const aName, aValue: OWideString);

    property Values[const aName: OWideString]: OWideString read GetValue write SetValue;
  end;

  TXMLRefNodeList = class(TXMLNodeList)
  public
    constructor Create;
  end;

  TXMLRefAttributeList = class(TXMLAttributeList)
  public
    constructor Create;
  end;

  TXMLXPathIntfDOMAdapter = class(TXMLXPathAdapter)
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

implementation

uses OXmlLng;

function CreateXMLDoc: IXMLDocument; overload;
begin
  Result := TXMLDocument.Create;
end;

function CreateXMLDoc(const aRootNodeName: OWideString): IXMLDocument; overload;
begin
  Result := CreateXMLDoc(aRootNodeName, False);
end;

function CreateXMLDoc(const aRootNodeName: OWideString; const aAddUTF8Declaration: Boolean): IXMLDocument; overload;
var
  xDec: IXMLNode;
begin
  Result := TXMLDocument.Create;
  if aAddUTF8Declaration then begin
    xDec := Result.DOMDocument.AddXMLDeclaration;
    xDec.Attributes['version'] := '1.0';
    xDec.Attributes['encoding'] := 'utf-8';
    xDec.Attributes['standalone'] := 'yes';
  end;

  if aRootNodeName <> '' then
    Result.DOMDocument.AddChild(aRootNodeName);
end;

{ TXMLNodeListEnumerator }

constructor TXMLNodeListEnumerator.Create(aList: IXMLNodeList);
begin
  inherited Create;

  fIndex := -1;
  fList := aList;
end;

function TXMLNodeListEnumerator.GetCurrent: IXMLNode;
begin
  Result := fList[fIndex];
end;

function TXMLNodeListEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex < fList.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ TXMLNode }

procedure TXMLNode.Clear;
begin
  DeleteChildren;
  DeleteAttributes;
  SetNodeNameId(-1);
  SetNodeValueId(-1);
end;

constructor TXMLNode.Create(const aOwnerDocument: TXMLDocument; const aName, aValue: OWideString);
begin
  inherited Create;

  fOwnerDocument := aOwnerDocument;

  if Assigned(Dictionary) then begin
    SetNodeNameId(Dictionary.Add(aName));
    SetNodeValueId(Dictionary.Add(aValue));
  end else begin
    SetNodeNameId(-1);
    SetNodeValueId(-1);
  end;
end;

procedure TXMLNode.DeleteAttributes;
begin
  if HasAttributes then
    AttributeNodes.Clear;
end;

procedure TXMLNode.DeleteChildren;
begin
  if HasChildNodes then
    ChildNodes.Clear;
end;

destructor TXMLNode.Destroy;
begin
  SetParentNode(nil);//MUST BE HERE because of FPC on Linux/Unix -> access violation otherwise

  inherited;
end;

function TXMLNode.GetDictionary: TOHashedStrings;
begin
  Result := fOwnerDocument.fDictionary;
end;

function TXMLNode.GetFirstAttribute: IXMLNode;
begin
  if HasAttributes then
    Result := AttributeNodes.GetFirst
  else
    Result := nil;
end;

function TXMLNode.GetFirstChild: IXMLNode;
begin
  if HasChildNodes then
    Result := ChildNodes.GetFirst
  else
    Result := nil;
end;

function TXMLNode.GetOwnerDocument: IXMLDocument;
begin
  Result := fOwnerDocument;
end;

function TXMLNode.GetParentNode: IXMLNode;
begin
  Result := fParentNode;
end;

procedure TXMLNode.SetAttribute(const aAttrName, aAttrValue: OWideString);
begin
  AttributeNodes.Values[aAttrName] := aAttrValue;
end;

function TXMLNode.SetAttributeNode(const aAttr: IXMLNode): IXMLNode;
var
  xOldAttrIndex: Integer;
begin
  if aAttr.OwnerDocument <> Self.OwnerDocument then
    raise EXmlDOMException.Create(OXmlLng_AppendFromDifferentDocument);

  if Assigned(aAttr.ParentNode) then
    raise EXmlDOMException.Create(OXmlLng_ParentNodeMustBeNil);

  xOldAttrIndex := AttributeNodes.IndexOf(aAttr.NodeName, {%H-}Result);
  if xOldAttrIndex >= 0 then
    AttributeNodes.Delete(xOldAttrIndex)
  else
    Result := nil;

  AttributeNodes.Add(aAttr);
end;

procedure TXMLNode.SetParentNode(const aParentNode: IXMLNode);
begin
  {$IFDEF NEXTGEN}
  fParentNode := aParentNode;//AVOID CIRCULAR REFERENCES: fParentNode set as WEAK reference
  {$ELSE}
  PPointer(@fParentNode)^ := Pointer(aParentNode);//AVOID CIRCULAR REFERENCES
  {$ENDIF}
end;

function TXMLNode.AddAttribute(const aName, aValue: OWideString): IXMLNode;
begin
  Attributes[aName] := aValue;
  Result := AttributeNodes.FindNode(aName);
end;

function TXMLNode.AddCDATASection(
  const aText: OWideString): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateCDATASection(aText);
  Self.AppendChild(Result);
end;

function TXMLNode.AddComment(const aText: OWideString): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateComment(aText);
  Self.AppendChild(Result);
end;

function TXMLNode.AddChild(const aNodeName: OWideString; Index: Integer = -1): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateElement(aNodeName);
  if (Index = -1) or (Index >= ChildNodes.Count) then
    Self.AppendChild(Result)
  else
    Self.InsertBefore(Result, Self.ChildNodes[Index]);
  Result.PreserveWhiteSpace := Self.PreserveWhiteSpace;
end;

function TXMLNode.AddXMLDeclaration: IXMLNode;
begin
  Result := Self.OwnerDocument.CreateXMLDeclaration;
  Self.AppendChild(Result);
end;

function TXMLNode.AddDocType(const aDocTypeRawText: OWideString): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateDocType(aDocTypeRawText);
  Self.AppendChild(Result);
end;

function TXMLNode.AddProcessingInstruction(const aTarget,
  aContent: OWideString): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateProcessingInstruction(aTarget, aContent);
  Self.AppendChild(Result);
end;

function TXMLNode.AddText(const aText: OWideString): IXMLNode;
var
  xText: OWideString;
begin
  if OwnerDocument.Loading then begin
    //document is reading XML
    if (OwnerDocument.WhiteSpaceHandling = wsPreserveInTextOnly) and OXmlIsWhiteSpace(aText) then begin
      xText := '';
    end else if
      (OwnerDocument.WhiteSpaceHandling = wsTrim) or
      ((OwnerDocument.WhiteSpaceHandling = wsAutoTag) and not PreserveWhiteSpace)
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
        (not Self.PreserveWhiteSpace) and
        OXmlNeedsPreserveAttribute(xText)
      then
        Self.PreserveWhiteSpace := True;
    end;
  end;

  if xText <> '' then begin
    Result := Self.OwnerDocument.CreateTextNode(xText);
    Self.AppendChild(Result);
  end else begin
    Result := nil;
  end;
end;

function TXMLNode.AppendChild(const aNewChild: IXMLNode): IXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_AppendFromDifferentDocument);

  //remove from last parent
  if Assigned(aNewChild.ParentNode) then
    aNewChild.ParentNode.ChildNodes.Remove(aNewChild);

  Result := ChildNodes.AddNode(aNewChild);
end;

function TXMLNode.GetAttribute(const aAttrName: OWideString): OWideString;
begin
  Result := GetAttributeDef(aAttrName, '');
end;

function TXMLNode.GetAttributeCount: Integer;
begin
  Result := 0;
end;

function TXMLNode.GetAttributeDef(const aName,
  aDefaultValue: OWideString): OWideString;
var xA: IXMLNode;
begin
  if not HasAttributes then begin
    Result := aDefaultValue;
    Exit;
  end;

  xA := AttributeNodes.FindNode(aName);
  if Assigned(xA) then
    Result := xA.NodeValue
  else
    Result := aDefaultValue;
end;

function TXMLNode.GetAttributeNode(const aAttrName: OWideString): IXMLNode;
begin
  if HasAttributes then
    Result := AttributeNodes.FindNode(aAttrName)
  else
    Result := nil;
end;

function TXMLNode.GetAttributeNodes: IXMLAttrList;
begin
  Result := nil;
end;

function TXMLNode.GetChildCount: Integer;
begin
  Result := 0;
end;

function TXMLNode.GetChildNodes: IXMLNodeList;
begin
  Result := nil;
end;

function TXMLNode.GetNextAttribute(var aAttributeEnum: IXMLNode): Boolean;
begin
  if HasAttributes then begin
    Result := AttributeNodes.GetNext(aAttributeEnum)
  end else begin
    Result := False;
    aAttributeEnum := nil;
  end;
end;

function TXMLNode.GetNextChild(var aChildEnum: IXMLNode): Boolean;
begin
  if HasChildNodes then begin
    Result := ChildNodes.GetNext(aChildEnum)
  end else begin
    Result := False;
    aChildEnum := nil;
  end;
end;

function TXMLNode.GetNextSibling: IXMLNode;
var
  xIter: IXMLNode;
begin
  Result := nil;
  xIter := Self;

  if not Assigned(fParentNode) then
    Exit;

  if (NodeType = ntAttribute) then begin
    if (fParentNode.HasAttributes) and
       (fParentNode.AttributeNodes.GetNext(xIter))
    then
      Result := xIter;
  end else begin
    if (fParentNode.HasChildNodes) and
       (fParentNode.ChildNodes.GetNext(xIter))
    then
      Result := xIter;
  end;
end;

function TXMLNode.GetNodeName: OWideString;
var
  xNodeNameId: Integer;
begin
  xNodeNameId := GetNodeNameId;
  if xNodeNameId >= 0 then
    Result := Dictionary.Get(xNodeNameId)
  else
    Result := '';
end;

function TXMLNode.GetNodeNameId: OHashedStringsIndex;
begin
  Result := -1;
end;

function TXMLNode.GetNodeValue: OWideString;
var
  xNodeValueId: Integer;
begin
  xNodeValueId := GetNodeValueId;
  if xNodeValueId >= 0 then
    Result := Dictionary.Get(xNodeValueId)
  else
    Result := '';
end;

function TXMLNode.GetNodeValueId: OHashedStringsIndex;
begin
  Result := -1;
end;

procedure TXMLNode.SetNodeName(const aName: OWideString);
begin
  SetNodeNameId(Dictionary.Add(aName));
end;

procedure TXMLNode.SetNodeNameId(const aNodeNameId: OHashedStringsIndex);
begin
  //nothing
end;

procedure TXMLNode.SetNodeValue(const aValue: OWideString);
begin
  SetNodeValueId(Dictionary.Add(aValue));
end;

procedure TXMLNode.SetNodeValueId(const aNodeValueId: OHashedStringsIndex);
begin
  //nothing
end;

function TXMLNode.GetPreserveWhiteSpace: Boolean;
begin
  Result := False;
end;

function TXMLNode.GetPreviousAttribute(var aAttributeEnum: IXMLNode): Boolean;
begin
  if HasAttributes then begin
    Result := AttributeNodes.GetPrevious(aAttributeEnum)
  end else begin
    Result := False;
    aAttributeEnum := nil;
  end;
end;

function TXMLNode.GetPreviousChild(var aChildEnum: IXMLNode): Boolean;
begin
  if HasChildNodes then begin
    Result := ChildNodes.GetPrevious(aChildEnum)
  end else begin
    Result := False;
    aChildEnum := nil;
  end;
end;

function TXMLNode.GetPreviousSibling: IXMLNode;
var
  xIter: IXMLNode;
begin
  Result := nil;
  xIter := Self;

  if not Assigned(fParentNode) then
    Exit;

  if (NodeType = ntAttribute) then begin
    if (fParentNode.HasAttributes) and
       (fParentNode.AttributeNodes.GetPrevious(xIter))
    then
      Result := xIter;
  end else begin
    if (fParentNode.HasChildNodes) and
       (fParentNode.ChildNodes.GetPrevious(xIter))
    then
      Result := xIter;
  end;
end;

function TXMLNode.GetText: OWideString;
var
  I: Integer;
begin
  Result := '';

  if HasChildNodes then
  for I := 0 to ChildNodes.Count-1 do
    Result := Result + ChildNodes.Nodes[I].Text;
end;

function TXMLNode.XML(const aOutputFormat: TXmlOutputFormat): OWideString;
begin
  SaveToXML({%H-}Result, aOutputFormat);
end;

{$IFNDEF NEXTGEN}
function TXMLNode.XML_UTF8(
  const aOutputFormat: TXmlOutputFormat): ORawByteString;
begin
  SaveToXML_UTF8({%H-}Result, aOutputFormat);
end;
{$ENDIF}

{$IFDEF O_DELPHI_2009_UP}
procedure TXMLNode.SaveToBuffer(var aBuffer: TBytes;
  const aOutputFormat: TXmlOutputFormat);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream, aOutputFormat);

    SetLength(aBuffer, xStream.Size);
    if xStream.Size > 0 then begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(aBuffer[0], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.SaveToBuffer(var aBuffer: TBytes;
  const aOutputFormat: TXmlOutputFormat; const aForceEncoding: TEncoding;
  const aWriteBOM: Boolean);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream, aOutputFormat, aForceEncoding, aWriteBOM);

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

function TXMLNode.HasAttribute(const aAttrName: OWideString): Boolean;
begin
  Result := HasAttributes and Assigned(AttributeNodes.FindNode(aAttrName));
end;

function TXMLNode.GetHasAttributes: Boolean;
begin
  Result := False;
end;

function TXMLNode.GetHasChildNodes: Boolean;
begin
  Result := False;
end;

function TXMLNode.GetLastAttribute: IXMLNode;
begin
  if HasAttributes then
    Result := AttributeNodes.GetLast
  else
    Result := nil;
end;

function TXMLNode.GetLastChild: IXMLNode;
begin
  if HasChildNodes then
    Result := ChildNodes.GetLast
  else
    Result := nil;
end;

function TXMLNode.InsertAttribute(const aName, aValue,
  aBeforeAttributeName: OWideString): IXMLNode;
var
  xBeforeAttr: IXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and SameText(aName, 'xml:space') then begin
    ParentNode.PreserveWhiteSpace := OXmlStrToPreserve(aValue);
  end else begin
    if HasAttributes then
      AttributeNodes.Delete(aName);

    Result := fOwnerDocument.CreateAttribute(aName, aValue);

    xBeforeAttr := AttributeNodes.FindNode(aBeforeAttributeName);
    if Assigned(xBeforeAttr) then
      InsertBefore(Result, xBeforeAttr)
    else
      AppendChild(Result);
  end;
end;

function TXMLNode.InsertBefore(const aNewChild,
  aRefChild: IXMLNode): IXMLNode;
var
  xIndex: Integer;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  if (aNewChild = aRefChild) then
    raise EXmlDOMException.Create(OXmlLng_InsertEqualNodes);

  if Assigned(aRefChild) then begin
    xIndex := ChildNodes.IndexOf(aRefChild);
    if xIndex < 0 then
      raise EXmlDOMException.Create(OXmlLng_NodeToInsertNotAChild);

    ChildNodes.Insert(xIndex, aNewChild);
    aNewChild.SetParentNode(Self);
    Result := aNewChild;
  end else begin
    Result := AppendChild(aNewChild);
  end;
end;

function TXMLNode.InsertCDATASection(const aText: OWideString;
  const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateCDATASection(aText);
  Self.InsertBefore(Result, aBeforeNode);
end;

function TXMLNode.InsertElement(const aNodeName: OWideString;
  const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateElement(aNodeName);
  Self.InsertBefore(Result, aBeforeNode);
  Result.PreserveWhiteSpace := Self.PreserveWhiteSpace;
end;

function TXMLNode.InsertComment(const aText: OWideString;
  const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateComment(aText);
  Self.InsertBefore(Result, aBeforeNode);
end;

function TXMLNode.InsertXMLDeclaration(const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateXMLDeclaration;
  Self.InsertBefore(Result, aBeforeNode);
  Result.PreserveWhiteSpace := Self.PreserveWhiteSpace;
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

function TXMLNode.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
var
  xReader: TOXmlReader;
  xReaderNode: TOXmlReaderNode;
  xDataRead: Boolean;
  xLastNode: IXMLNode;
begin
  if not (NodeType in [ntDOMDocument, ntElement]) then
    raise EXmlDOMException.Create(OXmlLng_NodeMustBeDOMDocumentOrElement);

  xReaderNode.NodeType := etDocumentStart;
  xReaderNode.NodeName := '';
  xReaderNode.NodeValue := '';

  DeleteChildren;

  OwnerDocument.Loading := True;
  xReader := TOXmlReader.Create(aStream, aForceEncoding);
  try
    xReader.StrictXML := OwnerDocument.StrictXML;
    xReader.BreakReading := OwnerDocument.BreakReading;
    xLastNode := Self;
    xDataRead := ((Self as IXMLNode) <> OwnerDocument.DOMDocument);//omit empty text before root node -> only for DOMDocument
    while xReader.ReadNextNode(xReaderNode) do begin
      case xReaderNode.NodeType of
        etOpenXMLDeclaration: xLastNode := xLastNode.AddXMLDeclaration;
        etXMLDeclarationAttribute, etAttribute: xLastNode.Attributes[xReaderNode.NodeName] := xReaderNode.NodeValue;
        etXMLDeclarationFinishClose, etFinishOpenElementClose, etCloseElement: xLastNode := xLastNode.ParentNode;
        etOpenElement: begin
          xDataRead := True;
          xLastNode := xLastNode.AddChild(xReaderNode.NodeName);
        end;
        etText:
          if xDataRead or not OXmlIsWhiteSpace(xReaderNode.NodeValue) then//omit empty text before root node
            xLastNode.AddText(xReaderNode.NodeValue);
        etCData: xLastNode.AddCDATASection(xReaderNode.NodeValue);
        etComment: xLastNode.AddComment(xReaderNode.NodeValue);
        etDocType: xLastNode.AddDocType(xReaderNode.NodeValue);
        etProcessingInstruction: xLastNode.AddProcessingInstruction(xReaderNode.NodeName, xReaderNode.NodeValue);
      end;
    end;
  finally
    xReader.Free;
    OwnerDocument.Loading := False;
  end;

  Result := True;
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

function TXMLNode.InsertDocType(const aDocTypeRawText: OWideString;
  const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateDocType(aDocTypeRawText);
  Self.InsertBefore(Result, aBeforeNode);
end;

function TXMLNode.InsertProcessingInstruction(const aTarget,
  aContent: OWideString; const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateProcessingInstruction(aTarget, aContent);
  Self.InsertBefore(Result, aBeforeNode);
end;

function TXMLNode.InsertText(const aText: OWideString;
  const aBeforeNode: IXMLNode): IXMLNode;
begin
  Result := Self.OwnerDocument.CreateTextNode(aText);
  Self.InsertBefore(Result, aBeforeNode);
end;

function TXMLNode.RemoveChild(const aOldChild: IXMLNode): IXMLNode;
var
  xIndex: Integer;
begin
  if HasChildNodes then
    xIndex := ChildNodes.IndexOf(aOldChild)
  else
    xIndex := -1;

  if xIndex > -1 then begin
    Result := aOldChild;
    ChildNodes.Remove(aOldChild);
  end else
    raise EXmlDOMException.Create(OXmlLng_ChildNotFound);
end;

procedure TXMLNode.RemoveChildren(const aNodeName: OWideString);
var
  I: Integer;
begin
  if not HasChildNodes then
    Exit;

  for I := Self.ChildNodes.Count-1 downto 0 do
  if (Self.ChildNodes[I].NodeName = aNodeName) then begin
    Self.ChildNodes.Delete(I);
  end;
end;

function TXMLNode.ReplaceChild(const aNewChild,
  aOldChild: IXMLNode): IXMLNode;
var
  xIndex: Integer;
begin
  if HasChildNodes then
    xIndex := ChildNodes.IndexOf(aOldChild)
  else
    xIndex := -1;

  if xIndex > -1 then
  begin
    Result := aOldChild;
    ChildNodes.Insert(xIndex, aNewChild);
    ChildNodes.Remove(aOldChild);
  end
  else
    raise EXmlDOMException.Create(OXmlLng_ChildNotFound);
end;

procedure TXMLNode.SaveToFile(const aFileName: String;
  const aOutputFormat: TXmlOutputFormat);
var
  xFS: TFileStream;
begin
  xFS := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(xFS, aOutputFormat);
  finally
    xFS.Free;
  end;
end;

procedure TXMLNode.SaveToStream(const aStream: TStream;
  const aOutputFormat: TXmlOutputFormat; const aForceEncoding: TEncoding;
  const aWriteBOM: Boolean);
var
  xWriter: TOXmlWriterIndentation;
begin
  if not Assigned(aForceEncoding) then
    raise EXmlDOMException.Create(OXmlLng_ForceEncodingNil);

  xWriter := TOXmlWriterIndentation.Create(aStream);
  try
    xWriter.StrictXML := OwnerDocument.StrictXML;
    xWriter.Encoding := aForceEncoding;

    xWriter.OutputFormat := aOutputFormat;
    xWriter.WriteBOM := aWriteBOM;

    WriteXML(xWriter);
  finally
    xWriter.Free;
  end;
end;

procedure TXMLNode.SaveToXML(var aXML: OWideString;
  const aOutputFormat: TXmlOutputFormat);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream, aOutputFormat, TEncoding.OWideStringEncoding, False);

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
procedure TXMLNode.SaveToXML_UTF8(var aXML: ORawByteString;
  const aOutputFormat: TXmlOutputFormat);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream, aOutputFormat, TEncoding.UTF8, False);

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

procedure TXMLNode.SaveToStream(const aStream: TStream;
  const aOutputFormat: TXmlOutputFormat);
var
  xEncoding: TEncoding;
  xWriteBOM: Boolean;
begin
  xEncoding := GetCreateCodePage(OwnerDocument.CodePage);
  xWriteBOM := True;

  SaveToStream(aStream, aOutputFormat, xEncoding, xWriteBOM);
end;

function TXMLNode.SelectNode(
  const aXPath: OWideString): IXMLNode;
begin
  if not SelectNode(aXPath, {%H-}Result) then
    Result := nil;
end;

function TXMLNode.SelectNode(const aXPath: OWideString;
  var aNode: IXMLNode): Boolean;
var
  xNodeList: IXMLNodeList;
begin
  xNodeList := nil;

  Result := SelectNodes(aXPath, xNodeList, 1);
  if Result and (xNodeList.Count > 0) then
    aNode := xNodeList[0]
  else
    aNode := nil;
end;

function TXMLNode.SelectNodeCreate(
  const aNodeName: OWideString): IXMLNode;
begin
  if not SelectNode(aNodeName, {%H-}Result) then
    Result := AddChild(aNodeName);
end;

function TXMLNode.SelectNodeNull(
  const aXPath: OWideString): IXMLNode;
begin
  if not SelectNode(aXPath, {%H-}Result) then
    Result := OwnerDocument.NullNode;
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
    Result := xXPaths.SelectNodes(TXMLXPathNode(IXmlNode(Self)), xCustomList, TXMLXPathIntfDOMAdapter, aMaxNodeCount);
    if Result then
      aNodeList := (IInterface(xCustomList) as IXMLNodeList)
    else
      aNodeList := nil;
  finally
    xXPaths.Free;
  end;
end;

function TXMLNode.SelectNodes(
  const aXPath: OWideString;
  const aMaxNodeCount: Integer): IXMLNodeList;
begin
  if not SelectNodes(aXPath, {%H-}Result, aMaxNodeCount) then
    Result := nil;
end;

procedure TXMLNode.SetPreserveWhiteSpace(
  const aPreserveWhiteSpace: Boolean);
begin
  //nothing
end;

procedure TXMLNode.SetText(const aText: OWideString);
begin
  if HasChildNodes then
    ChildNodes.Clear;

  AddText(aText);
end;

procedure TXMLNode.WriteChildrenXML(const aOutputWriter: TOXmlWriterIndentation);
var I: Integer;
begin
  if not HasChildNodes then
    Exit;

  for I := 0 to ChildNodes.Count-1 do
    ChildNodes[I].WriteXML(aOutputWriter);
end;

procedure TXMLNode.WriteAttributesXML(
  const aOutputWriter: TOXmlWriterIndentation);
var
  I: Integer;
  xAttr: IXMLNode;
begin
  if HasAttributes then
  for I := 0 to AttributeNodes.Count-1 do begin
    xAttr := AttributeNodes[I];
    aOutputWriter.Attribute(xAttr.NodeName, xAttr.NodeValue);
  end;

  if (fOwnerDocument.WhiteSpaceHandling = wsAutoTag) and
     (NodeType = ntElement) and
     (fParentNode.PreserveWhiteSpace <> Self.PreserveWhiteSpace)
  then
    aOutputWriter.Attribute('xml:space', OXmlPreserveToStr(Self.PreserveWhiteSpace));
end;

{ TXMLNodeList }

function TXMLNodeList.Add(const aNode: IXMLNode): Integer;
begin
  {$IFDEF O_GENERICS}
  Result := fList.Add(aNode);
  {$ELSE}
  Result := fList.Add(Pointer(aNode));
  aNode._AddRef;
  {$ENDIF}

  if Assigned(fParentNode) then
    aNode.SetParentNode(ParentNode);
end;

function TXMLNodeList.AddNode(const aNode: IXMLNode): IXMLNode;
begin
  Add(aNode);
  Result := aNode;
end;

procedure TXMLNodeList.Clear;
{$IFNDEF O_GENERICS}
var I: Integer;
{$ENDIF}
begin
  {$IFNDEF O_GENERICS}
  for I := 0 to fList.Count-1 do
    Self[I]._Release;
  {$ENDIF}
  fList.Clear;
end;

constructor TXMLNodeList.Create(const aParentNode: IXMLNode);
begin
  inherited Create;

  SetParentNode(aParentNode);

  {$IFDEF O_GENERICS}
  fList := TList<IXMLNode>.Create;
  {$ELSE}
  fList := TList.Create;
  {$ENDIF}
end;

procedure TXMLNodeList.Delete(const aIndex: Integer);
begin
  if (aIndex >= 0) and  (aIndex < fList.Count) then begin
    {$IFNDEF O_GENERICS}
    Self[aIndex]._Release;
    {$ENDIF}
    fList.Delete(aIndex);
  end;
end;

procedure TXMLNodeList.Delete(const aNode: IXMLNode);
begin
  Remove(aNode);
end;

destructor TXMLNodeList.Destroy;
begin
  SetParentNode(nil);//MUST BE HERE because of FPC on Linux/Unix -> access violation otherwise

  Clear;
  fList.Free;

  inherited;
end;

function TXMLNodeList.GetNext(var aNodeEnum: IXMLNode): Boolean;
begin
  Result := GetPrevNext(aNodeEnum, +1);
end;

function TXMLNodeList.GetNode(const Index: Integer): IXMLNode;
begin
  {$IFDEF O_GENERICS}
  Result := fList.Items[Index];
  {$ELSE}
  Result := IInterface(fList.Items[Index]) as IXMLNode;
  {$ENDIF}
end;

function TXMLNodeList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TXMLNodeList.GetFirst: IXMLNode;
begin
  if Count > 0 then
    Result := Nodes[0];
end;

function TXMLNodeList.GetLast: IXMLNode;
begin
  if Count > 0 then
    Result := Nodes[Count-1];
end;

function TXMLNodeList.GetParentNode: IXMLNode;
begin
  Result := fParentNode;
end;

function TXMLNodeList.GetPrevious(var aNodeEnum: IXMLNode): Boolean;
begin
  Result := GetPrevNext(aNodeEnum, -1);
end;

function TXMLNodeList.GetPrevNext(var aNodeEnum: IXMLNode;
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

function TXMLNodeList.IndexOf(const aNode: IXMLNode): Integer;
begin
  {$IFDEF O_GENERICS}
  Result := fList.IndexOf(aNode);
  {$ELSE}
  Result := fList.IndexOf(Pointer(aNode));
  {$ENDIF}
end;

procedure TXMLNodeList.Insert(const Index: Integer; const aNode: IXMLNode);
begin
  {$IFDEF O_GENERICS}
  fList.Insert(Index, aNode);
  {$ELSE}
  fList.Insert(Index, Pointer(aNode));
  aNode._AddRef;
  {$ENDIF}
end;

function TXMLNodeList.Remove(const aNode: IXMLNode): Integer;
begin
  {$IFDEF O_GENERICS}
  Result := fList.Remove(aNode);
  {$ELSE}
  Result := fList.Remove(Pointer(aNode));
  if Result >= 0 then
    aNode._Release;
  {$ENDIF}
end;

procedure TXMLNodeList.SetParentNode(const aParentNode: IXMLNode);
begin
  {$IFDEF NEXTGEN}
  fParentNode := aParentNode;//AVOID CIRCULAR REFERENCES: fParentNode set as WEAK reference
  {$ELSE}
  PPointer(@fParentNode)^ := Pointer(aParentNode);//AVOID CIRCULAR REFERENCES
  {$ENDIF}
end;

{ TXMLNodeList }

{$IFDEF O_ENUMERATORS}
function TXMLNodeList.GetEnumerator: TXMLNodeListEnumerator;
begin
  Result := TXMLNodeListEnumerator.Create(Self);
end;
{$ENDIF}

function TXMLNodeList.IndexOf(const aName: OWideString): Integer;
var x: IXMLNode;
begin
  Result := IndexOf(aName, {%H-}x);
end;

function TXMLNodeList.IndexOf(const aName: OWideString; var aNode: IXMLNode): Integer;
var
  xNameId: OHashedStringsIndex;
begin
  if Count = 0 then begin
    Result := -1;
    aNode := nil;
    Exit;
  end;

  xNameId := ParentNode.Dictionary.IndexOf(aName);
  if xNameId < 0 then begin
    Result := -1;
    aNode := nil;
    Exit;
  end;

  for Result := 0 to Count-1 do
  if (Nodes[Result].NameId = xNameId) then begin
    aNode := Nodes[Result];
    Exit;
  end;

  Result := -1;
  aNode := nil;
end;

function TXMLNodeList.FindNode(const aName: OWideString): IXMLNode;
begin
  if IndexOf(aName, {%H-}Result) < 0 then
    Result := nil;
end;

function TXMLNodeList.Delete(const aName: OWideString): IXMLNode;
var I: Integer;
begin
  I := IndexOf(aName, {%H-}Result);
  if I >= 0 then
    Delete(I)
  else
    Result := nil;
end;

{ TXMLAttributeList }

function TXMLAttributeList.GetValue(const aName: OWideString): OWideString;
var xA: IXMLNode;
begin
  xA := FindNode(aName);
  if Assigned(xA) then
    Result := xA.NodeValue
  else
    Result := '';
end;

procedure TXMLAttributeList.SetValue(const aName, aValue: OWideString);
var xA: IXMLNode;
  xP: IXMLNode;
begin
  xP := ParentNode;
  if not Assigned(xP) then
    Exit;

  if (xP.OwnerDocument.WhiteSpaceHandling = wsAutoTag) and (aName = 'xml:space') then begin
    xP.PreserveWhiteSpace := OXmlStrToPreserve(aValue);
  end else begin
    xA := FindNode(aName);
    if not Assigned(xA) then begin
      xA := xP.OwnerDocument.CreateAttribute(aName, aValue);
      Add(xA);
    end else begin
      xA.NodeValue := aValue;
    end;
  end;
end;

{ TXMLDocument }

procedure TXMLDocument.Clear;
begin
  fDOMDocument.Clear;
end;

constructor TXMLDocument.Create(aParent: TObject);
begin
  inherited Create;

  DoCreate;
end;

function TXMLDocument.FindXMLDeclarationNode(var aXMLDeclarationNode: IXMLNode): Boolean;
var
  I: Integer;
begin
  if fDOMDocument.HasChildNodes then
  for I := 0 to fDOMDocument.ChildNodes.Count-1 do
  if (fDOMDocument.ChildNodes[I].NodeType = ntXMLDeclaration)
  then begin
    aXMLDeclarationNode := fDOMDocument.ChildNodes[I];
    Result := True;
    Exit;
  end;

  Result := False;
  aXMLDeclarationNode := nil;
end;

constructor TXMLDocument.Create(const aRootNodeName: OWideString;
  const aAddUTF8Declaration: Boolean);
var
  xDec: IXMLNode;
begin
  inherited Create;

  DoCreate;

  if aAddUTF8Declaration then begin
    xDec := fDOMDocument.AddXMLDeclaration;
    xDec.Attributes['version'] := '1.0';
    xDec.Attributes['encoding'] := 'utf-8';
    xDec.Attributes['standalone'] := 'yes';
  end;

  if aRootNodeName <> '' then
    fDOMDocument.AddChild(aRootNodeName);
end;

function TXMLDocument.CreateAttribute(const aName, aValue: OWideString): IXMLNode;
begin
  Result := TXMLAttribute.Create(Self, aName, aValue);
end;

function TXMLDocument.CreateCDATASection(
  const aData: OWideString): IXMLNode;
begin
  Result := TXMLCDATASection.Create(Self, aData);
end;

function TXMLDocument.CreateComment(const aText: OWideString): IXMLNode;
begin
  Result := TXMLComment.Create(Self, aText);
end;

function TXMLDocument.CreateElement(const aNodeName: OWideString): IXMLNode;
begin
  Result := TXMLElement.Create(Self, aNodeName);
end;

function TXMLDocument.CreateProcessingInstruction(const aTarget,
  aContent: OWideString): IXMLNode;
begin
  Result := TXMLProcessingInstruction.Create(Self, aTarget, aContent);
end;

function TXMLDocument.CreateXMLDeclaration: IXMLNode;
begin
  Result := TXMLDeclaration.Create(Self);
end;

function TXMLDocument.CreateDocType(
  const aDocTypeRawText: OWideString): IXMLNode;
begin
  Result := TXMLDocType.Create(Self, aDocTypeRawText);
end;

function TXMLDocument.CreateTextNode(const aText: OWideString): IXMLNode;
begin
  Result := TXMLText.Create(Self, aText);
end;

destructor TXMLDocument.Destroy;
begin
  fDictionary.Free;
  fNullNode := nil;

  inherited;
end;

procedure TXMLDocument.DoCreate;
begin
  fDictionary := TOHashedStrings.Create;

  fWhiteSpaceHandling := wsPreserveInTextOnly;
  fStrictXML := True;
  fBreakReading := brAfterDocumentNode;

  fDOMDocument := TXMLDOMDocument.Create(Self, '');
end;

function TXMLDocument.GetBreakReading: TXmlBreakReading;
begin
  Result := fBreakReading;
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
  xDecNode: IXMLNode;
begin
  if FindXMLDeclarationNode({%H-}xDecNode) then
    Result := xDecNode.Attributes[aAttributeName]
  else
    Result := '';
end;

function TXMLDocument.GetDocumentNode: IXMLNode;
var
  I: Integer;
begin
  if fDOMDocument.HasChildNodes then begin
    for I := 0 to fDOMDocument.ChildNodes.Count-1 do
    if fDOMDocument.ChildNodes.Nodes[I].NodeType = ntElement then begin
      Result := fDOMDocument.ChildNodes.Nodes[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TXMLDocument.GetDOMDocument: IXMLNode;
begin
  Result := fDOMDocument;
end;

function TXMLDocument.GetEncoding: OWideString;
begin
  Result := GetXMLDeclarationAttribute('encoding');
end;

function TXMLDocument.GetLoading: Boolean;
begin
  Result := fLoading;
end;

function TXMLDocument.GetNullNode: IXMLNode;
begin
  if not Assigned(fNullNode) then
    fNullNode := CreateElement('');
  Result := fNullNode;
end;

function TXMLDocument.GetStandAlone: OWideString;
begin
  Result := GetXMLDeclarationAttribute('standalone');
end;

function TXMLDocument.GetStrictXML: Boolean;
begin
  Result := fStrictXML;
end;

function TXMLDocument.GetVersion: OWideString;
begin
  Result := GetXMLDeclarationAttribute('version');
end;

function TXMLDocument.GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
begin
  Result := fWhiteSpaceHandling;
end;

function TXMLDocument.XML(const aOutputFormat: TXmlOutputFormat): OWideString;
begin
  Result := DOMDocument.XML(aOutputFormat);
end;

{$IFNDEF NEXTGEN}
function TXMLDocument.XML_UTF8(
  const aOutputFormat: TXmlOutputFormat): ORawByteString;
begin
  Result := DOMDocument.XML_UTF8(aOutputFormat);
end;
{$ENDIF}

{$IFDEF O_DELPHI_2009_UP}
procedure TXMLDocument.SaveToBuffer(var aBuffer: TBytes;
  const aOutputFormat: TXmlOutputFormat);
begin
  DOMDocument.SaveToBuffer(aBuffer, aOutputFormat);
end;

procedure TXMLDocument.SaveToBuffer(var aBuffer: TBytes;
  const aOutputFormat: TXmlOutputFormat; const aForceEncoding: TEncoding;
  const aWriteBOM: Boolean);
begin
  DOMDocument.SaveToBuffer(aBuffer, aOutputFormat, aForceEncoding, aWriteBOM);
end;
{$ENDIF}

function TXMLDocument.LoadFromFile(const aFileName: String): Boolean;
begin
  Clear;
  Result := DOMDocument.LoadFromFile(aFileName);
end;

function TXMLDocument.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := DOMDocument.LoadFromStream(aStream, aForceEncoding);
end;

function TXMLDocument.LoadFromXML(const aXML: OWideString): Boolean;
begin
  Clear;
  Result := DOMDocument.LoadFromXML(aXML);
end;

{$IFNDEF NEXTGEN}
function TXMLDocument.LoadFromXML_UTF8(const aXML: ORawByteString): Boolean;
begin
  Clear;
  Result := DOMDocument.LoadFromXML_UTF8(aXML);
end;
{$ENDIF}

{$IFDEF O_DELPHI_2009_UP}
function TXMLDocument.LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean;
begin
  Clear;
  Result := DOMDocument.LoadFromBuffer(aBuffer, aForceEncoding);
end;
{$ENDIF}

procedure TXMLDocument.SaveToFile(const aFileName: String;
  const aOutputFormat: TXmlOutputFormat);
begin
  DOMDocument.SaveToFile(aFileName, aOutputFormat);
end;

procedure TXMLDocument.SaveToStream(const aStream: TStream;
  const aOutputFormat: TXmlOutputFormat);
begin
  DOMDocument.SaveToStream(aStream, aOutputFormat);
end;

procedure TXMLDocument.SaveToStream(const aStream: TStream;
  const aOutputFormat: TXmlOutputFormat;
  const aForceEncoding: TEncoding;
  const aWriteBOM: Boolean);
begin
  DOMDocument.SaveToStream(aStream, aOutputFormat, aForceEncoding, aWriteBOM);
end;

procedure TXMLDocument.SaveToXML(var aXML: OWideString;
  const aOutputFormat: TXmlOutputFormat);
begin
  DOMDocument.SaveToXML(aXML, aOutputFormat);
end;

{$IFNDEF NEXTGEN}
procedure TXMLDocument.SaveToXML_UTF8(var aXML: ORawByteString;
  const aOutputFormat: TXmlOutputFormat);
begin
  DOMDocument.SaveToXML_UTF8(aXML, aOutputFormat);
end;
{$ENDIF}

procedure TXMLDocument.SetBreakReading(const aBreakReading: TXmlBreakReading);
begin
  fBreakReading := aBreakReading;
end;

procedure TXMLDocument.SetCodePage(const aCodePage: Word);
begin
  Encoding := CodePageToAlias(aCodePage);
end;

procedure TXMLDocument.SetDocumentNode(const aDocumentNode: IXMLNode);
var
  I: Integer;
begin
  if fDOMDocument.HasChildNodes then begin
    for I := fDOMDocument.ChildNodes.Count-1 downto 0 do
    if fDOMDocument.ChildNodes.Nodes[I].NodeType = ntElement then begin
      fDOMDocument.ChildNodes.Delete(I);
    end;
  end;

  fDOMDocument.AppendChild(aDocumentNode);
end;

procedure TXMLDocument.SetXMLDeclarationAttribute(const aAttributeName,
  aAttributeValue: OWideString);
var
  xDecNode: IXMLNode;
begin
  if not FindXMLDeclarationNode({%H-}xDecNode) then begin
    if fDOMDocument.HasChildNodes then
      xDecNode := fDOMDocument.InsertXMLDeclaration(fDOMDocument.ChildNodes[0])
    else
      xDecNode := fDOMDocument.AddXMLDeclaration;
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

procedure TXMLDocument.SetStrictXML(const aStrictXML: Boolean);
begin
  fStrictXML := aStrictXML;
end;

procedure TXMLDocument.SetVersion(const aVersion: OWideString);
begin
  SetXMLDeclarationAttribute('version', aVersion);
end;

procedure TXMLDocument.SetWhiteSpaceHandling(
  const Value: TXmlWhiteSpaceHandling);
begin
  fWhiteSpaceHandling := Value;
end;

{ TXMLAttribute }

function TXMLAttribute.GetNodeType: TXmlNodeType;
begin
  Result := ntAttribute;
end;

procedure TXMLAttribute.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Attribute(NodeName, NodeValue);
end;

function TXMLAttribute.GetNodeNameId: OHashedStringsIndex;
begin
  Result := fNodeNameId;
end;

procedure TXMLAttribute.SetNodeValueId(const aNodeValueId: OHashedStringsIndex);
begin
  fNodeValueId := aNodeValueId;
end;

function TXMLAttribute.GetNodeValueId: OHashedStringsIndex;
begin
  Result := fNodeValueId;
end;

procedure TXMLAttribute.SetNodeNameId(const aNodeNameId: OHashedStringsIndex);
begin
  fNodeNameId := aNodeNameId;
end;

{ TXMLCharacterData }

constructor TXMLCharacterData.Create(const aOwnerDocument: TXMLDocument;
  const aCharacterData: OWideString);
begin
  inherited Create(aOwnerDocument, '', aCharacterData);
end;

function TXMLCharacterData.GetNodeValueId: OHashedStringsIndex;
begin
  Result := fNodeValueId;
end;

function TXMLCharacterData.GetText: OWideString;
begin
  Result := NodeValue;
end;

procedure TXMLCharacterData.SetNodeValueId(
  const aNodeValueId: OHashedStringsIndex);
begin
  fNodeValueId := aNodeValueId;
end;

procedure TXMLCharacterData.SetText(const aText: OWideString);
begin
  NodeValue := aText;
end;

{ TXMLDeclaration }

constructor TXMLDeclaration.Create(const aOwnerDocument: TXMLDocument);
begin
  inherited Create(aOwnerDocument, '');
end;

function TXMLDeclaration.GetNodeType: TXmlNodeType;
begin
  Result := ntXMLDeclaration;
end;

procedure TXMLDeclaration.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Indent;
  aOutputWriter.OpenXMLDeclaration;
  WriteAttributesXML(aOutputWriter);
  aOutputWriter.FinishOpenXMLDeclaration;
end;

{ TXMLCDATASection }

function TXMLCDATASection.GetNodeType: TXmlNodeType;
begin
  Result := ntCData;
end;

procedure TXMLCDATASection.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Indent;
  aOutputWriter.CData(Text);
end;

{ TXMLText }

function TXMLText.GetNodeType: TXmlNodeType;
begin
  Result := ntText;
end;

procedure TXMLText.SetText(const aText: OWideString);
begin
  if OwnerDocument.WhiteSpaceHandling = wsTrim then
    inherited SetText(Trim(aText))
  else
    inherited SetText(aText);
end;

procedure TXMLText.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  if (aOutputWriter.OutputFormat <> ofNone) and//speed optimization
    not(fParentNode.HasChildNodes and (fParentNode.ChildNodes.Count = 1))
  then//indent if the text is not the only child
    aOutputWriter.Indent;
  aOutputWriter.Text(Text);
end;

{ TXMLComment }

function TXMLComment.GetNodeType: TXmlNodeType;
begin
  Result := ntComment;
end;

function TXMLComment.GetText: OWideString;
begin
  Result := '';//IMPORTANT
end;

procedure TXMLComment.SetText(const aText: OWideString);
begin
  raise EXmlDOMException.Create(OXmlLng_CannotSetText);
end;

procedure TXMLComment.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Indent;
  aOutputWriter.Comment(NodeValue);
end;

{ TXMLElement }

constructor TXMLElement.Create(const aOwnerDocument: TXMLDocument;
  const aNodeName: OWideString);
begin
  inherited Create(aOwnerDocument, aNodeName, '');
end;

procedure TXMLElement.DeleteAttributes;
begin
  inherited;

  if Assigned(fAttributeNodes) then
    fAttributeNodes := nil;
end;

procedure TXMLElement.DeleteChildren;
begin
  inherited;

  if Assigned(fChildNodes) then
    fChildNodes := nil;
end;

function TXMLElement.GetAttributeCount: Integer;
begin
  if Assigned(fAttributeNodes) then
    Result := fAttributeNodes.Count
  else
    Result := 0;
end;

function TXMLElement.GetAttributeNodes: IXMLAttrList;
begin
  if not Assigned(fAttributeNodes) then
    fAttributeNodes := TXMLAttributeList.Create(Self);
  Result := fAttributeNodes;
end;

function TXMLElement.GetChildCount: Integer;
begin
  if Assigned(fChildNodes) then
    Result := fChildNodes.Count
  else
    Result := 0;
end;

function TXMLElement.GetChildNodes: IXMLNodeList;
begin
  if not Assigned(fChildNodes) then
    fChildNodes := TXMLNodeList.Create(Self);
  Result := fChildNodes;
end;

function TXMLElement.GetHasAttributes: Boolean;
begin
  Result := Assigned(fAttributeNodes) and (fAttributeNodes.Count > 0);
end;

function TXMLElement.GetHasChildNodes: Boolean;
begin
  Result := Assigned(fChildNodes) and (fChildNodes.Count > 0);
end;

function TXMLElement.GetNodeNameId: OHashedStringsIndex;
begin
  Result := fNodeNameId;
end;

function TXMLElement.GetPreserveWhiteSpace: Boolean;
begin
  Result := fPreserveWhiteSpace;
end;

procedure TXMLElement.SetNodeNameId(
  const aNodeNameId: OHashedStringsIndex);
begin
  fNodeNameId := aNodeNameId;
end;

function TXMLElement.GetNodeType: TXmlNodeType;
begin
  Result := ntElement;
end;

procedure TXMLElement.SetPreserveWhiteSpace(
  const aPreserveWhiteSpace: Boolean);
begin
  fPreserveWhiteSpace := aPreserveWhiteSpace;
end;

procedure TXMLElement.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Indent;
  aOutputWriter.OpenElement(NodeName);
  aOutputWriter.IncIndentLevel;
  WriteAttributesXML(aOutputWriter);
  if HasChildNodes then begin
    aOutputWriter.FinishOpenElement;
    WriteChildrenXML(aOutputWriter);
    aOutputWriter.DecIndentLevel;
    if (aOutputWriter.OutputFormat <> ofNone) and//speed optimization
      not(
       (fChildNodes.Count = 1) and
       (fChildNodes[0].NodeType = ntText))
    then//indent if the text is not the only child
      aOutputWriter.Indent;
    aOutputWriter.CloseElement(NodeName);
  end else begin
    aOutputWriter.DecIndentLevel;
    aOutputWriter.FinishOpenElementClose;
  end;
end;

{ TXMLRefNodeList }

constructor TXMLRefNodeList.Create;
begin
  inherited Create(nil);
end;

{ TXMLRefAttributeList }

constructor TXMLRefAttributeList.Create;
begin
  inherited Create(nil);
end;

{ TXMLProcessingInstruction }

constructor TXMLProcessingInstruction.Create(const aOwnerDocument: TXMLDocument;
  const aTarget, aContent: OWideString);
begin
  inherited Create(aOwnerDocument, aTarget, aContent);
end;

function TXMLProcessingInstruction.GetNodeType: TXmlNodeType;
begin
  Result := ntProcessingInstruction;
end;

function TXMLProcessingInstruction.GetText: OWideString;
begin
  Result := '';//IMPORTANT
end;

procedure TXMLProcessingInstruction.SetText(const aText: OWideString);
begin
  raise EXmlDOMException.Create(OXmlLng_CannotSetText);
end;

procedure TXMLProcessingInstruction.WriteXML(
  const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Indent;
  aOutputWriter.ProcessingInstruction(NodeName, NodeValue);
end;

{ TXMLDocType }

function TXMLDocType.GetNodeType: TXmlNodeType;
begin
  Result := ntDocType;
end;

function TXMLDocType.GetText: OWideString;
begin
  Result := '';//IMPORTANT
end;

procedure TXMLDocType.SetText(const aText: OWideString);
begin
  raise EXmlDOMException.Create(OXmlLng_CannotSetText);
end;

procedure TXMLDocType.WriteXML(const aOutputWriter: TOXmlWriterIndentation);
begin
  aOutputWriter.Indent;
  aOutputWriter.DocType(NodeValue);
end;

{ TXMLDOMDocument }

function TXMLDOMDocument.GetNodeName: OWideString;
begin
  Result := '#document';
end;

function TXMLDOMDocument.GetNodeType: TXmlNodeType;
begin
  Result := ntDOMDocument;
end;

procedure TXMLDOMDocument.WriteXML(const aOutputWriter: TOXMLWriterIndentation);
begin
  WriteChildrenXML(aOutputWriter);
end;

{ TXMLXPathIntfDOMAdapter }

procedure TXMLXPathIntfDOMAdapter.AddNodeToResList(const aNode: TXMLXPathNode);
begin
  (IInterface(fResNodeList) as IXMLNodeList).Add(IInterface(aNode) as IXMLNode);
end;

procedure TXMLXPathIntfDOMAdapter.BuildIdTree(const aStartWithNode: TXMLXPathNode;
  const aLevelsDeep: Integer; const aIdTree: TXMLXPathIdTree);
var
  xId: XMLXPathId;

  procedure _ScanNode(const bNode: IXMLNode; const bLevelsDeepLeft: Integer);
  var
    I: Integer;
  begin
    {$IFDEF O_GENERICS}
    aIdTree.Add(TXMLXPathNode(bNode), xId);
    {$ELSE}
    aIdTree.AddPointer({%H-}ONativeInt(TXMLXPathNode(bNode)), {%H-}Pointer(xId));
    {$ENDIF}
    Inc(xId);

    if bLevelsDeepLeft < 0 then
      Exit;

    if bNode.HasAttributes then
    for I := 0 to bNode.AttributeNodes.Count-1 do begin
      {$IFDEF O_GENERICS}
      aIdTree.Add(TXMLXPathNode(bNode.AttributeNodes[I]), xId);
      {$ELSE}
      aIdTree.AddPointer({%H-}ONativeInt(TXMLXPathNode(bNode.AttributeNodes[I])), {%H-}Pointer(xId));
      {$ENDIF}
      Inc(xId);
    end;

    if bNode.HasChildNodes then
    for I := 0 to bNode.ChildNodes.Count-1 do
    if bNode.ChildNodes[I].NodeType in [ntElement, ntText, ntCData] then begin
      _ScanNode(bNode.ChildNodes[I], bLevelsDeepLeft-1);
    end;
  end;
begin
  aIdTree.Clear;
  xId := 0;

  _ScanNode(IInterface(aStartWithNode) as IXMLNode, aLevelsDeep);
end;

function TXMLXPathIntfDOMAdapter.CreateResNodeList: TXMLXPathNodeList;
begin
  if not Assigned(fResNodeList) then
    fResNodeList := TXMLRefNodeList.Create;
  Result := TXMLXPathNodeList(fResNodeList as IXMLNodeList);
end;

procedure TXMLXPathIntfDOMAdapter.GetNodeAttributes(
  const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList);
var
  I: Integer;
  xParentNode: IXMLNode;
begin
  aList.Clear;

  xParentNode := (IInterface(aParentNode) as IXMLNode);
  if not xParentNode.HasAttributes then
    Exit;

  for I := 0 to xParentNode.AttributeNodes.Count-1 do
    aList.Add(TXMLXPathNode(xParentNode.AttributeNodes[I]));
end;

procedure TXMLXPathIntfDOMAdapter.GetNodeChildren(const aParentNode: TXMLXPathNode;
  const aList: TXMLXPathResNodeList);
var
  I: Integer;
  xParentNode: IXMLNode;
begin
  aList.Clear;

  xParentNode := (IInterface(aParentNode) as IXMLNode);
  if not xParentNode.HasChildNodes then
    Exit;

  for I := 0 to xParentNode.ChildNodes.Count-1 do
    aList.Add(TXMLXPathNode(xParentNode.ChildNodes[I]));
end;

function TXMLXPathIntfDOMAdapter.GetNodeDOMDocument(
  const aNode: TXMLXPathNode): TXMLXPathNode;
begin
  Result := TXMLXPathNode((IInterface(aNode) as IXMLNode).OwnerDocument.DOMDocument as IXMLNode);
end;

procedure TXMLXPathIntfDOMAdapter.GetNodeInfo(const aNode: TXMLXPathNode;
  var aNodeInfo: TXMLXPathNodeInfo);
var
  xNode: IXMLNode;
begin
  xNode := (IInterface(aNode) as IXMLNode);
  aNodeInfo.NodeName := xNode.NodeName;
  aNodeInfo.NodeValue := xNode.NodeValue;
  aNodeInfo.NodeType := xNode.NodeType;
end;

function TXMLXPathIntfDOMAdapter.GetNodeName(
  const aNode: TXMLXPathNode): OWideString;
begin
  Result := (IInterface(aNode) as IXMLNode).NodeName;
end;

function TXMLXPathIntfDOMAdapter.GetNodeParent(
  const aNode: TXMLXPathNode): TXMLXPathNode;
begin
  Result := TXMLXPathNode((IInterface(aNode) as IXMLNode).ParentNode);
end;

function TXMLXPathIntfDOMAdapter.GetNodeType(
  const aNode: TXMLXPathNode): TXmlNodeType;
begin
  Result := (IInterface(aNode) as IXMLNode).NodeType;
end;

function TXMLXPathIntfDOMAdapter.GetNodeValue(
  const aNode: TXMLXPathNode): OWideString;
begin
  Result := (IInterface(aNode) as IXMLNode).NodeValue;
end;

function TXMLXPathIntfDOMAdapter.NodeFindAttribute(const aNode: TXMLXPathNode;
  const aAttrName: OWideString): TXMLXPathNode;
begin
  Result := TXMLXPathNode((IInterface(aNode) as IXMLNode).AttributeNodes.FindNode(aAttrName));
end;

function TXMLXPathIntfDOMAdapter.NodeHasAttributes(
  const aNode: TXMLXPathNode): Boolean;
begin
  Result := (IInterface(aNode) as IXMLNode).HasAttributes;
end;

function TXMLXPathIntfDOMAdapter.NodeHasChildNodes(
  const aNode: TXMLXPathNode): Boolean;
begin
  Result := (IInterface(aNode) as IXMLNode).HasChildNodes;
end;

end.

