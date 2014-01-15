unit OXmlXPath;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlXPath.pas

  Abstract XPath library.
    - every DOM can be supported through a TXMLXPathAdapter descendant.
    - only for internal use. should not be used manually.

  Supported XPath syntax elements:
    nodename    - select all child nodes of the node
    .           - select current node
    ..          - select parent node
    /           - select root node
    //          - select through all child nodes (if used at the beginning select through all nodes in document)
    @attr       - select attributes with the name "attr"
    node()      - select all node types: elements, attributes, text and cdata
    text()      - select text nodes (including cdata)
    node[@attr] - select nodes with "attr" attribute
    n[@a="v"]   - select nodes "n" with attribute "a" of the value "v"
    n[1]        - select the first node
    n[last()]   - select the last node
    |           - OR operator on XPaths

  Supported XPath examples (and possible combinations of them):
    /topnode                 - select the "topnode" element of the document
    ./*                      - select all children of current node
    //*                      - select all elements (children, grandchildren etc.) within the document
    .//*                     - select all elements (children, grandchildren etc.) within the current node
    ./example                - select all children of current node with the name "example"
    ../element               - select all children of parent node with the name "example"
    example[@big]            - select all examples with defined "big" attribute
    example[@big="true"]     - select all examples with "big" attribute of "true"
    example[1]               - select the first example child
    example[last()-1]        - select the second from the end example child
    ../example|../book       - select all children of parent node (siblings) with the name "example" or "book"

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
  SysUtils, Classes, OWideSupp, OXmlUtils
  {$IFDEF O_GENERICS}
  , Generics.Collections
  {$ELSE}
  , Contnrs, ODictionary
  {$ENDIF}
  ;

type
  //xntNode = "xntElement, xntAttribute, xntText"
  TXMLXPathNodeType = (xntNode, xntElement, xntAttribute, xntText,
    xntParentElement, xntRootElement, xntCurrentElement, xntAllLevelsElement);

  TXMLXPathNode = Pointer;
  TXMLXPathNodeList = Pointer;
  XMLXPathId = ONativeUInt;
  {$IFDEF O_GENERICS}
  TXMLXPathResNodeList = TList<TXMLXPathNode>;
  TXMLXPathResNodeDictionary = TDictionary<XMLXPathId,TXMLXPathNode>;
  {$ELSE}
  TXMLXPathResNodeList = TList;
  TXMLXPathResNodeDictionary = TODictionary;
  {$ENDIF}
  TXMLXPathCheckedParent = {$IF DEFINED (O_EXTRECORDS) OR DEFINED(O_GENERICS)}record{$ELSE}object{$IFEND}
    ParentNodeId: XMLXPathId;
    SelectorLevel: Integer;
  {$IFNDEF O_GENERICS}
  public
    function FootPrint: String;
  {$ENDIF}
  end;
  {$IFDEF O_GENERICS}
  TXMLXPathCheckedParents = TDictionary<TXMLXPathCheckedParent,Boolean>;
  TXMLXPathIdTree = TDictionary<TXMLXPathNode,XMLXPathId>;
  {$ELSE}
  TXMLXPathCheckedParents = TStringList;
  TXMLXPathIdTree = TODictionary;
  {$ENDIF}
  TXMLXPathNodeInfo = record
    NodeName: OWideString;
    NodeValue: OWideString;
    NodeType: TXmlNodeType;
  end;

  TXMLXPathAdapter = class(TObject)
  public
    procedure BuildIdTree(const aReferenceWithNode: TXMLXPathNode; const aLevelsDeep: Integer;
      const aIdTree: TXMLXPathIdTree); virtual; abstract;
    function CreateResNodeList: TXMLXPathNodeList; virtual; abstract;
    procedure AddNodeToResList(const aNode: TXMLXPathNode); virtual; abstract;
    function GetNodeName(const aNode: TXMLXPathNode): OWideString; virtual; abstract;
    function GetNodeValue(const aNode: TXMLXPathNode): OWideString; virtual; abstract;
    function GetNodeType(const aNode: TXMLXPathNode): TXmlNodeType; virtual; abstract;
    procedure GetNodeInfo(const aNode: TXMLXPathNode; var outNodeInfo: TXMLXPathNodeInfo); virtual; abstract;
    function NodeHasAttributes(const aNode: TXMLXPathNode): Boolean; virtual; abstract;
    function NodeFindAttribute(const aNode: TXMLXPathNode; const aAttrName: OWideString): TXMLXPathNode; virtual; abstract;
    procedure GetNodeAttributes(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); virtual; abstract;
    function GetNodeParent(const aNode: TXMLXPathNode): TXMLXPathNode; virtual; abstract;
    function GetNodeDOMDocument(const aNode: TXMLXPathNode): TXMLXPathNode; virtual; abstract;
    function NodeHasChildNodes(const aNode: TXMLXPathNode): Boolean; virtual; abstract;
    procedure GetNodeChildren(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); virtual; abstract;
  end;
  TXMLXPathAdapterClass = class of TXMLXPathAdapter;

  TXMLXPathSelector = class(TObject)
  private
    fLevel: Integer;
    fNodeType: TXMLXPathNodeType;

    fCheckElementName: Boolean;
    fElementName: OWideString;

    fCheckAttributeName: Boolean;
    fAttributeName: OWideString;
    fCheckAttributeValue: Boolean;
    fAttributeValue: OWideString;

    fCheckIndexFromStart: Boolean;
    fCheckIndexFromEnd: Boolean;
    fIndexInParent: Integer;

    fNext: TXMLXPathSelector;

    function GetLast: TXMLXPathSelector;
  protected
    procedure SelectElements(
      const aParentNode: Pointer;
      const aCheckedParents: TXMLXPathCheckedParents;
      const aAddedNodes: TXMLXPathResNodeDictionary;
      const aIdTree: TXMLXPathIdTree;
      const aAdapter: TXMLXPathAdapter
      );
    procedure SelectAttributes(
      const aParentNode: TXMLXPathNode;
      const aAddedNodes: TXMLXPathResNodeDictionary;
      const aIdTree: TXMLXPathIdTree;
      const aAdapter: TXMLXPathAdapter);

    procedure SelectNodes(
      const aParentNode: TXMLXPathNode;
      const aCheckedParents: TXMLXPathCheckedParents;
      const aAddedNodes: TXMLXPathResNodeDictionary;
      const aIdTree: TXMLXPathIdTree;
      const aAdapter: TXMLXPathAdapter);
  public
    destructor Destroy; override;
  public
    procedure LoadFromString(aString: OWideString; aIsFirst: Boolean);
    function CreateNext: TXMLXPathSelector;

    function MatchesNode(const aNode: TXMLXPathNode;
      const aNodeInfo: TXMLXPathNodeInfo;
      const aAdapter: TXMLXPathAdapter): Boolean;
    function MatchesNodeIndex(const {%H-}aNode: TXMLXPathNode;
      const aNodeIndexInParent, aNodeCountInParent: Integer): Boolean;
  public
    property NodeType: TXMLXPathNodeType read fNodeType;

    property CheckElementName: Boolean read fCheckElementName write fCheckElementName;
    property ElementName: OWideString read fElementName write fElementName;

    property CheckAttributeName: Boolean read fCheckAttributeName write fCheckAttributeName;
    property AttributeName: OWideString read fAttributeName write fAttributeName;
    property CheckAttributeValue: Boolean read fCheckAttributeValue write fCheckAttributeValue;
    property AttributeValue: OWideString read fAttributeValue write fAttributeValue;

    property CheckIndexFromStart: Boolean read fCheckIndexFromStart write fCheckIndexFromStart;
    property CheckIndexFromEnd: Boolean read fCheckIndexFromEnd write fCheckIndexFromEnd;
    property IndexInParent: Integer read fIndexInParent write fIndexInParent;

    property Level: Integer read fLevel;
    property Next: TXMLXPathSelector read fNext;
    property Last: TXMLXPathSelector read GetLast;
  end;

  TXMLXPath = class(TObject)
  private
    fFirst: TXMLXPathSelector;
  private
    function GetFirst: TXMLXPathSelector;
    function GetLast: TXMLXPathSelector;

    procedure GetNodeRangeToBuildIdTree(
      const aReferenceLevel: Integer;
      var outStartLevel, outEndLevel: Integer);
  public
    destructor Destroy; override;
  public
    property First: TXMLXPathSelector read GetFirst;
    property Last: TXMLXPathSelector read GetLast;
  public
    procedure LoadFromString(const aString: OWideString);
  end;

  TXMLXPathList = class(TObject)
  private
    fAdapter: TXMLXPathAdapter;

    {$IFDEF O_GENERICS}
    fList: TObjectList<TXMLXPath>;
    {$ELSE}
    fList: TObjectList;
    {$ENDIF}
  private
    function GetI(const Index: Integer): TXMLXPath;
    function GetCount: Integer;

    procedure GetNodeToBuildIdTreeFrom(const aReferenceNode: TXMLXPathNode;
      var outStartNode: TXMLXPathNode; var outLevelsDeep: Integer);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Items[const Index: Integer]: TXMLXPath read GetI; default;
    property Count: Integer read GetCount;
  public
    procedure LoadFromString(const aString: OWideString);

    function SelectNodes(
      const aParentNode: TXMLXPathNode;
      var outList: TXMLXPathNodeList;
      const aAdapterClass: TXMLXPathAdapterClass;
      const aMaxNodeCount: Integer): Boolean;
  end;

  EXmlXPathInvalidString = class(Exception);

function XPathIsSimpleNode(const aXPath: OWideString;
  var outNodeName: OWideString; var outChildType: TXmlChildType): Boolean;

implementation

uses OXmlLng;

function XPathIsSimpleNode(const aXPath: OWideString;
  var outNodeName: OWideString; var outChildType: TXmlChildType): Boolean;
var
  I, xL: Integer;
begin
  outNodeName := '';
  xL := Length(aXPath);
  if xL = 0 then begin
    Result := True;
    Exit;
  end;

  if aXPath[1] = '@' then begin
    outChildType := ctAttribute;
    I := 2;
  end else begin
    outChildType := ctChild;
    I := 1;
  end;

  for I := I to Length(aXPath) do
  case aXPath[I] of
    '/', '[', '.':
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
  if outChildType = ctAttribute then
    outNodeName := Copy(aXPath, 2, xL-1)
  else
    outNodeName := aXPath;
end;

{ TXMLXPath }

destructor TXMLXPath.Destroy;
begin
  fFirst.Free;

  inherited;
end;

function TXMLXPath.GetFirst: TXMLXPathSelector;
begin
  Result := fFirst;
end;

function TXMLXPath.GetLast: TXMLXPathSelector;
begin
  if Assigned(fFirst) then
    Result := fFirst.Last
  else
    Result := nil;
end;

procedure TXMLXPath.GetNodeRangeToBuildIdTree(
  const aReferenceLevel: Integer;
  var outStartLevel, outEndLevel: Integer);
var
  xItem: TXMLXPathSelector;
  xCurrentLevel: Integer;
begin
  //gets the deepest level we need to build the id tree for

  xCurrentLevel := aReferenceLevel;
  outStartLevel := aReferenceLevel;
  outEndLevel := aReferenceLevel;
  xItem := fFirst;
  while Assigned(xItem) do begin
    case xItem.NodeType of
      xntRootElement: begin
        //root -> go to start
        outStartLevel := 0;
        outEndLevel := 0;
        xCurrentLevel := 0;
      end;
      xntParentElement: begin
        //parent node selector -> decrease level
        Dec(xCurrentLevel);
        if xCurrentLevel < outStartLevel then
          outStartLevel := xCurrentLevel;
      end;
      xntCurrentElement: begin
        //current node selector -> don't do anything
      end;
      xntAllLevelsElement: begin
        //search through all nodes
        outEndLevel := High(outEndLevel);
        Exit;
      end;
    else
      //normal level -> increase level
      if Assigned(xItem.fNext) then begin//only for not-last element
        Inc(xCurrentLevel);
        if xCurrentLevel > outEndLevel then
          outEndLevel := xCurrentLevel;
      end;
    end;

    xItem := xItem.Next;
  end;
end;

procedure TXMLXPath.LoadFromString(const aString: OWideString);
var
  xStrL: TOWideStringList;
  I: Integer;
  xNewSelector: TXMLXPathSelector;
begin
  fFirst.Free;
  fFirst := nil;
  xNewSelector := nil;

  if aString = '' then
    Exit;

  xStrL := TOWideStringList.Create;
  try
    OExplode(aString, '/', xStrL, True);

    for I := 0 to xStrL.Count-1 do begin
      if I = 0 then begin
        fFirst := TXMLXPathSelector.Create;
        xNewSelector := fFirst;
      end else begin
        xNewSelector := xNewSelector.CreateNext;
      end;

      xNewSelector.LoadFromString(xStrL[I], (I = 0));
    end;
  finally
    xStrL.Free;
  end;
end;

{ TXMLXPathList }

constructor TXMLXPathList.Create;
begin
  {$IFDEF O_GENERICS}
  fList := TObjectList<TXMLXPath>.Create(True);
  {$ELSE}
  fList := TObjectList.Create(True);
  {$ENDIF}
end;

destructor TXMLXPathList.Destroy;
begin
  fList.Free;
  fAdapter.Free;

  inherited;
end;

function TXMLXPathList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TXMLXPathList.GetI(const Index: Integer): TXMLXPath;
begin
  Result := TXMLXPath(fList[Index]);
end;

procedure TXMLXPathList.GetNodeToBuildIdTreeFrom(const aReferenceNode: TXMLXPathNode;
  var outStartNode: TXMLXPathNode; var outLevelsDeep: Integer);
var
  I, xReferenceNodeLevel, xStartLevel, xEndLevel, xStartLevelI, xEndLevelI: Integer;
  xNode, xDocumentElement: TXMLXPathNode;
begin
  //first find the level of current node
  xReferenceNodeLevel := 0;
  xNode := aReferenceNode;
  xDocumentElement := fAdapter.GetNodeDOMDocument(xNode);
  while xNode <> xDocumentElement do begin
    Inc(xReferenceNodeLevel);
    xNode := fAdapter.GetNodeParent(xNode);
  end;
  xStartLevel := xReferenceNodeLevel;
  xEndLevel := xReferenceNodeLevel;
  xStartLevelI := xReferenceNodeLevel;
  xEndLevelI := xReferenceNodeLevel;

  //now search through all paths to find the lowest and highest level
  for I := 0 to Count-1 do begin
    Items[I].GetNodeRangeToBuildIdTree(xReferenceNodeLevel, xStartLevelI, xEndLevelI);
    if xStartLevelI < xStartLevel then
      xStartLevel := xStartLevelI;
    if xEndLevelI > xEndLevel then
      xEndLevel := xEndLevelI;
  end;

  if xStartLevel = 0 then begin
    outStartNode := xDocumentElement;
  end else begin
    outStartNode := aReferenceNode;
    for I := xReferenceNodeLevel-1 downto xStartLevel do begin
      if Assigned(outStartNode) then
        outStartNode := fAdapter.GetNodeParent(outStartNode)
      else
        Break;
    end;
  end;

  if not Assigned(outStartNode) then
    outStartNode := aReferenceNode;

  if xStartLevel < 0 then
    xStartLevel := 0;

  if xEndLevel > xStartLevel then
    outLevelsDeep := xEndLevel-xStartLevel
  else
    outLevelsDeep := 0;
end;

procedure TXMLXPathList.LoadFromString(const aString: OWideString);
var
  xStrL: TOWideStringList;
  I: Integer;
  xNewXPath: TXMLXPath;
begin
  if aString = '' then
    raise EXmlXPathInvalidString.Create(OXmlLng_XPathCannotBeEmpty);

  fList.Clear;

  xStrL := TOWideStringList.Create;
  try
    OExplode(aString, '|', xStrL, True);

    for I := 0 to xStrL.Count-1 do begin
      xNewXPath := TXMLXPath.Create;
      fList.Add(xNewXPath);
      xNewXPath.LoadFromString(xStrL[I]);
    end;
  finally
    xStrL.Free;
  end;
end;

function TXMLXPathList.SelectNodes(
  const aParentNode: TXMLXPathNode;
  var outList: TXMLXPathNodeList;
  const aAdapterClass: TXMLXPathAdapterClass;
  const aMaxNodeCount: Integer): Boolean;
var
  I: Integer;
  xCheckedParents: TXMLXPathCheckedParents;
  xAddedElements: TXMLXPathResNodeDictionary;
  {$IFDEF O_GENERICS}
  xSortedElements: TList<XMLXPathId>;
  xElementId: XMLXPathId;
  {$ENDIF}
  xIdTree: TXMLXPathIdTree;
  xBuildIdTreeNode: TXMLXPathNode;
  xBuildIdTreeLevelsDeep: Integer;
  xResNodeCount: Integer;
begin
  fAdapter.Free;
  fAdapter := aAdapterClass.Create;
  xCheckedParents := TXMLXPathCheckedParents.Create;
  xAddedElements := TXMLXPathResNodeDictionary.Create;
  xIdTree := TXMLXPathIdTree.Create;
  {$IFNDEF O_GENERICS}
  xCheckedParents.Sorted := True;
  xCheckedParents.Duplicates := dupIgnore;
  {$ENDIF}
  try
    GetNodeToBuildIdTreeFrom(aParentNode, {%H-}xBuildIdTreeNode, {%H-}xBuildIdTreeLevelsDeep);
    fAdapter.BuildIdTree(xBuildIdTreeNode, xBuildIdTreeLevelsDeep, xIdTree);

    for I := 0 to Count-1 do
    if Assigned(Items[I].First) then begin
      xCheckedParents.Clear;//check always whole tree for all paths
      Items[I].First.SelectNodes(aParentNode,
        xCheckedParents, xAddedElements, xIdTree, fAdapter);
    end;

    Result := xAddedElements.Count > 0;
    if Result then begin
      outList := fAdapter.CreateResNodeList;

      xResNodeCount := xAddedElements.Count-1;
      if (aMaxNodeCount > 0) and (aMaxNodeCount < xResNodeCount) then
        xResNodeCount := aMaxNodeCount;

      {$IFDEF O_GENERICS}
      xSortedElements := TList<XMLXPathId>.Create(xAddedElements.Keys);
      try
        xSortedElements.Sort;
        for I := 0 to xResNodeCount do begin
          xElementId := xSortedElements[I];
          fAdapter.AddNodeToResList(xAddedElements.Items[xElementId]);
        end;
      finally
        xSortedElements.Free;
      end;
      {$ELSE}
      for I := 0 to xResNodeCount do begin
        fAdapter.AddNodeToResList(TXMLXPathNode(xAddedElements.Pointers[I]));
      end;
      {$ENDIF}
    end else begin
      outList := nil;
    end;
  finally
    xCheckedParents.Free;
    xAddedElements.Free;
    xIdTree.Free;
  end;
end;

{ TXMLXPathSelector }

function TXMLXPathSelector.CreateNext: TXMLXPathSelector;
begin
  if not Assigned(fNext) then begin
    fNext := TXMLXPathSelector.Create;
    fNext.fLevel := Self.fLevel+1;
  end;
  Result := fNext;
end;

destructor TXMLXPathSelector.Destroy;
begin
  fNext.Free;

  inherited;
end;

function TXMLXPathSelector.GetLast: TXMLXPathSelector;
begin
  if Assigned(fNext) then
    Result := fNext.Last
  else
    Result := Self;
end;

procedure TXMLXPathSelector.LoadFromString(aString: OWideString; aIsFirst: Boolean);
var
  xPredicatePos: Integer;
  xPredicate: OWideString;
begin
  aString := Trim(aString);

  if (aString <> '') and (aString[1] = '@') then begin
    //attribute "@attr"
    fNodeType := xntAttribute;
    AttributeName := Copy(aString, 2, High(Integer));
    CheckAttributeName := AttributeName <> '*';
  end else if SameText(aString, 'node()') then begin
    //any node
    fNodeType := xntNode;
  end else if SameText(aString, 'text()') then begin
    //text node
    fNodeType := xntText;
  end else if (aString = '') then begin
    //root node if first
    if aIsFirst then
      fNodeType := xntRootElement
    else
      fNodeType := xntAllLevelsElement;
  end else if (aString = '..') then begin
    fNodeType := xntParentElement;
  end else if (aString = '.') then begin
    fNodeType := xntCurrentElement;
  end else begin
    fNodeType := xntElement;
    //search predicate: element[attr=value]
    xPredicatePos := Pos('[', aString);
    if xPredicatePos = 0 then begin
      //no predicate
      ElementName := Trim(aString);
    end else if (aString[Length(aString)] = ']') then begin
      //predicate
      ElementName := Trim(Copy(aString, 1, xPredicatePos-1));
      xPredicate := Trim(Copy(aString, xPredicatePos+1, Length(aString)-xPredicatePos-1));

      if xPredicate = '' then begin
        //empty predicate
      end else if OXmlIsNumber(xPredicate) then begin
        //number predicate "book[1]" -> elements are 1-based!
        CheckIndexFromStart := True;
        IndexInParent := StrToInt(xPredicate)-1;//1-based!
      end else if Pos('last()', xPredicate) = 1 then begin
        //number predicate with last() "book[last()-1]"
        Delete(xPredicate, 1, 6);//delete "last()"
        xPredicate := Trim(xPredicate);
        if xPredicate = '' then begin
          CheckIndexFromEnd := True;
          IndexInParent := 0;
        end else if OXmlIsNumber(xPredicate) then begin
          CheckIndexFromEnd := True;
          IndexInParent := StrToInt(xPredicate);
        end else begin
          raise EXmlXPathInvalidString.CreateFmt(OXmlLng_XPathPredicateNotSupported, [xPredicate, aString])
        end;
      end else if xPredicate[1] = '@' then begin
        //search for '='
        xPredicatePos := Pos('=', xPredicate);
        if xPredicatePos = 0 then begin
          //no attr value
          AttributeName := Copy(xPredicate, 2, High(Integer));
        end else begin
          //attr value
          AttributeName := Copy(xPredicate, 2, xPredicatePos-2);
          xPredicate := Trim(Copy(xPredicate, xPredicatePos+1, Length(xPredicate)-xPredicatePos));
          if (xPredicate <> '') then begin
            if (xPredicate[1] = '"') or (xPredicate[1] = '''') then begin
              if xPredicate[Length(xPredicate)] <> xPredicate[1] then
                raise EXmlXPathInvalidString.CreateFmt(OXmlLng_XPathPredicateNotValid, [xPredicate, aString]);

              xPredicate := Copy(xPredicate, 2, Length(xPredicate)-2);
            end;
            AttributeValue := xPredicate;
          end;
        end;
      end else begin
        //unknown predicate
        raise EXmlXPathInvalidString.CreateFmt(OXmlLng_XPathPredicateNotSupported, [xPredicate, aString])
      end;

      CheckAttributeName := (AttributeName <> '');
      CheckAttributeValue := (AttributeValue <> '');
    end else begin
      //unknown XPath
      raise EXmlXPathInvalidString.CreateFmt(OXmlLng_XPathNotSupported, [aString])
    end;
    CheckElementName := (ElementName <> '*');
  end;
end;

function TXMLXPathSelector.MatchesNode(const aNode: TXMLXPathNode;
  const aNodeInfo: TXMLXPathNodeInfo;
  const aAdapter: TXMLXPathAdapter): Boolean;
var
  xAttr: TXMLXPathNode;
begin
  Result := False;

  if
    ((NodeType = xntAllLevelsElement) and (aNodeInfo.NodeType = ntElement)) or
    ((NodeType = xntNode) and (aNodeInfo.NodeType in [ntElement, ntAttribute, ntText, ntCData]))
  then begin
    Result := True;
    Exit;
  end;

  if (NodeType = xntElement) and (aNodeInfo.NodeType = ntElement)
  then begin
    //check element name
    Result :=
      (not CheckElementName or (ElementName = aNodeInfo.NodeName));

    if Result and CheckAttributeName then begin
      //check attributes
      Result := aAdapter.NodeHasAttributes(aNode);

      if Result then begin
        //find attribute by name
        xAttr := aAdapter.NodeFindAttribute(aNode, AttributeName);
        Result := Assigned(xAttr);

        if Result then begin
          //check attribute value
          Result := (not CheckAttributeValue or (aAdapter.GetNodeValue(xAttr) = AttributeValue));
        end;
      end;
    end;

    if Result then
      Exit;//do not check for attribute
  end;

  if (NodeType = xntAttribute) and (aNodeInfo.NodeType = ntAttribute)
  then begin
    //check attribute name and value
    Result :=
      (not CheckAttributeName or (AttributeName = aNodeInfo.NodeName)) and
      (not CheckAttributeValue or (AttributeValue = aNodeInfo.NodeValue));

    if Result then
      Exit;//do not check for texts
  end;

  if (NodeType = xntText) and (aNodeInfo.NodeType in [ntText, ntCData])
  then begin
    Result := True;
  end;
end;

function TXMLXPathSelector.MatchesNodeIndex(const aNode: TXMLXPathNode;
  const aNodeIndexInParent, aNodeCountInParent: Integer): Boolean;
begin
  Result := True;
  if ((CheckIndexFromStart or CheckIndexFromEnd))
  then begin
    if CheckIndexFromStart then begin
      //check index from start
      Result := (IndexInParent = aNodeIndexInParent);
    end;

    if Result and CheckIndexFromEnd then begin
      //check index from end
      Result := (aNodeCountInParent+IndexInParent) = aNodeIndexInParent;//IndexInParent is negative
    end;
  end;
end;

procedure TXMLXPathSelector.SelectAttributes(
  const aParentNode: TXMLXPathNode;
  const aAddedNodes: TXMLXPathResNodeDictionary;
  const aIdTree: TXMLXPathIdTree;
  const aAdapter: TXMLXPathAdapter);

  procedure _AddAttribute(const bAttr: TXMLXPathNode);
  begin
    if not CheckAttributeValue or (AttributeValue = aAdapter.GetNodeValue(bAttr)) then begin
      {$IFDEF O_GENERICS}
      aAddedNodes.Add(aIdTree.Items[bAttr], bAttr);
      {$ELSE}
      aAddedNodes.AddPointer({%H-}ONativeInt(aIdTree.PointerOfKey[{%H-}ONativeInt(bAttr)]), bAttr);
      {$ENDIF}
    end;
  end;
var
  xAttr: TXMLXPathNode;
  xList: TXMLXPathResNodeList;
  I: Integer;
begin
  if not aAdapter.NodeHasAttributes(aParentNode) then
    Exit;

  if not CheckAttributeName then begin
    xList := TXMLXPathResNodeList.Create;
    try
      aAdapter.GetNodeAttributes(aParentNode, xList);
      for I := 0 to xList.Count-1 do begin
        _AddAttribute(xList[I]);
      end;
    finally
      xList.Free;
    end;
  end else begin
    xAttr := aAdapter.NodeFindAttribute(aParentNode, AttributeName);
    if Assigned(xAttr) then begin
      _AddAttribute(xAttr);
    end;
  end;
end;

procedure TXMLXPathSelector.SelectElements(
  const aParentNode: TXMLXPathNode;
  const aCheckedParents: TXMLXPathCheckedParents;
  const aAddedNodes: TXMLXPathResNodeDictionary;
  const aIdTree: TXMLXPathIdTree;
  const aAdapter: TXMLXPathAdapter);

  procedure _SelectWithNode(const bNode: TXMLXPathNode; bSelector: TXMLXPathSelector = nil);
  begin
    if not Assigned(bSelector) then
      bSelector := Next;

    if Assigned(bSelector) then begin
      bSelector.SelectNodes(bNode, aCheckedParents, aAddedNodes, aIdTree, aAdapter);
    end else begin
      //last selector -> add to list!
      {$IFDEF O_GENERICS}
      aAddedNodes.Add(aIdTree.Items[bNode], bNode);
      {$ELSE}
      aAddedNodes.AddPointer({%H-}ONativeInt(aIdTree.PointerOfKey[{%H-}ONativeInt(bNode)]), bNode);
      {$ENDIF}
    end;
  end;
var
  xChildNode: TXMLXPathNode;
  xChildNodeInfo: TXMLXPathNodeInfo;
  xList: TXMLXPathResNodeList;
  I: Integer;
begin
  case NodeType of
    xntParentElement: begin
      _SelectWithNode(aAdapter.GetNodeParent(aParentNode));
      Exit;
    end;
    xntRootElement: begin
      _SelectWithNode(aAdapter.GetNodeDOMDocument(aParentNode));
      Exit;
    end;
    xntCurrentElement: begin
      _SelectWithNode(aParentNode);
      Exit;
    end;
    xntAllLevelsElement: begin
      _SelectWithNode(aParentNode);
      //do not exit -> go through all child nodes
    end;
  end;

  //check for child elements
  if not aAdapter.NodeHasChildNodes(aParentNode) then
    exit;

  xList := TXMLXPathResNodeList.Create;
  try
    aAdapter.GetNodeChildren(aParentNode, xList);

    //first check for validity
    for I := xList.Count-1 downto 0 do begin
      xChildNode := xList[I];
      aAdapter.GetNodeInfo(xChildNode, {%H-}xChildNodeInfo);
      if not MatchesNode(xChildNode, xChildNodeInfo, aAdapter) then
        xList.Delete(I);
    end;

    //now check for positions
    for I := 0 to xList.Count-1 do begin
      xChildNode := xList[I];
      if MatchesNodeIndex(xChildNode, I, xList.Count-1)
      then begin
        if NodeType = xntAllLevelsElement then begin
          //all elements selector -> check through all levels, also self
          _SelectWithNode(xChildNode, Self);
        end;

        _SelectWithNode(xChildNode);
      end;
    end;
  finally
    xList.Free;
  end;
end;

procedure TXMLXPathSelector.SelectNodes(
  const aParentNode: TXMLXPathNode;
  const aCheckedParents: TXMLXPathCheckedParents;
  const aAddedNodes: TXMLXPathResNodeDictionary;
  const aIdTree: TXMLXPathIdTree;
  const aAdapter: TXMLXPathAdapter);
var
  xParent: TXMLXPathCheckedParent;
begin
  xParent.SelectorLevel := Self.Level;
  {$IFDEF O_GENERICS}
  xParent.ParentNodeId := aIdTree.Items[aParentNode];
  if aCheckedParents.ContainsKey(xParent) then
    Exit;
  aCheckedParents.Add(xParent, True);
  {$ELSE}
  xParent.ParentNodeId := {%H-}ONativeInt(aIdTree.PointerOfKey[{%H-}ONativeInt(aParentNode)]);
  if aCheckedParents.IndexOf(xParent.FootPrint) >= 0 then
    Exit;
  aCheckedParents.Add(xParent.FootPrint);
  {$ENDIF}

  if NodeType in [xntAttribute, xntNode] then
    SelectAttributes(aParentNode, aAddedNodes, aIdTree, aAdapter);

  if NodeType <> xntAttribute then
    SelectElements(aParentNode,
      aCheckedParents, aAddedNodes, aIdTree, aAdapter);
end;

{ TXMLXPathCheckedParent }

{$IFNDEF O_GENERICS}
function TXMLXPathCheckedParent.FootPrint: String;
begin
  Result := IntToHex(ParentNodeId, 8)+':'+IntToHex(SelectorLevel, 2);
end;
{$ENDIF}

end.
