unit OXmlLng;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OXmlLng.pas

  Language definitions for OXml library.

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

resourcestring
  OXmlLng_InvalidCData = '"%s" is not a valid CData text.';
  OXmlLng_InvalidComment = '"%s" is not a valid comment text.';
  OXmlLng_InvalidPITarget = '"%s" is not a valid processing instruction target.';
  OXmlLng_InvalidPIContent = '"%s" is not a valid processing instruction content.';
  OXmlLng_InvalidAttributeName = '"%s" is not a valid attribute name.';
  OXmlLng_InvalidElementName = '"%s" is not a valid element name.';
  OXmlLng_InvalidCharacterInText = 'The character "%s" cannot occur in text.';
  OXmlLng_InvalidStringInText = 'The string "%s" cannot occur in text.';
  OXmlLng_InvalidCharacterInElement = 'The character "%s" cannot occur in element header.';
  OXmlLng_EqualSignMustFollowAttribute = 'Equal sign must follow the attribute "%s".';
  OXmlLng_AttributeValueMustBeEnclosed = '"%s" attribute value must be enclosed in quotes.';
  OXmlLng_TooManyElementsClosed = 'Too many elements closed.';
  OXmlLng_WrongElementClosed = 'Trying to close wrong element. Close="%s", open element="%s".';
  OXmlLng_InvalidEntity = '"%s" is not a valid entity.';
  OXmlLng_ReadingAt = 'Reading at:'+sLineBreak+'%s';

  OXmlLng_XPathPredicateNotSupported = 'XPath predicate "%s" is not supported.'+sLineBreak+'XPath: %s';
  OXmlLng_XPathPredicateNotValid = 'XPath predicate "%s" is not valid.'+sLineBreak+'XPath: %s';
  OXmlLng_XPathNotSupported = 'XPath is not supported.'+sLineBreak+'XPath: %s';

  OXmlLng_AppendFromDifferentDocument = 'You can''t append a node from a different XML document.';
  OXmlLng_InsertFromDifferentDocument = 'You can''t insert a node from a different XML document.';
  OXmlLng_InsertEqualNodes = 'Node to insert and reference node can''t be equal.';
  OXmlLng_ParentNodeCantBeNil = 'Parent node can''t be nil.';
  OXmlLng_ParentNodeMustBeNil = 'Parent node must be nil.';
  OXmlLng_NodeToDeleteNotAChild = 'You can''t delete a node that is not a child of current node.';
  OXmlLng_NodeToInsertNotAChild = 'You can''t insert node before a node that is not a child of current node.';
  OXmlLng_NodeMustBeDOMDocumentOrElement = 'Node must be a DOMDocument or an element.';
  OXmlLng_ForceEncodingNil = 'aForceEncoding cannot be nil.';
  OXmlLng_CannotSetText = 'You can''t set the text property of this node. Use NodeValue instead.';
  OXmlLng_ChildNotFound = 'Child not found.';
  OXmlLng_ListIndexOutOfRange = 'List index out of range.';
  OXmlLng_FeatureNotSupported = 'This feature is not supported.';
  OXmlLng_CannotWriteAttributesWhenFinished = 'You can''t add an attribute %s="%s" when the element header has been finished.';

implementation

end.
