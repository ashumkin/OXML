unit OHashedStrings;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    MPL 1.1 / GPLv2 / LGPLv2 / FPC modified LGPLv2
    Please see the /license.txt file for more information.

}

{
  OHashedStrings.pas

  TOHashedStrings
    - hashed unsorted string list
    - always keeps original order of strings
    - every string is unique in the list
    - fast IndexOf() function
    - an object can be assiciated with every string

  TOHashedStringDictionary
    - a TDictionary<String,String> replacement for FPC and D6-2007
    - always keeps original order of keys
    - every key is unique in the list
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
  Classes, SysUtils, OWideSupp
  {$IFDEF FPC}, contnrs{$ELSE}, IniFiles{$ENDIF}
  {$IFDEF O_GENERICS}, Generics.Collections{$ENDIF}
  ;

type

  OHashedStringsIndex = Integer;
  TOHashedStrings = class(TPersistent)
  private
    {$IFDEF FPC}
    fHashTable: TFPHashList;
    {$ELSE}
    fHashTable: TStringHash;
    {$ENDIF}
    fTextList: TStringList;
    fMaxItemsBeforeGrow: OHashedStringsIndex;
    fLastHashI: Integer;

    procedure Grow;
    procedure RefreshHashTable;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function IndexOf(const aText: OWideString): OHashedStringsIndex;
    function IndexOfFast(const aText: OFastString): OHashedStringsIndex;
    function Add(const aText: OWideString): OHashedStringsIndex; overload;
    function Add(const aText: OWideString; var aNewEntry: Boolean): OHashedStringsIndex; overload;
    function Get(const aIndex: OHashedStringsIndex): OWideString;
    procedure SetObject(const aIndex: OHashedStringsIndex; const aObject: TObject);
    function GetObject(const aIndex: OHashedStringsIndex): TObject;
    {$IFNDEF NEXTGEN}
    procedure SetPObject(const aIndex: OHashedStringsIndex; const aObject: Pointer);
    function GetPObject(const aIndex: OHashedStringsIndex): Pointer;
    {$ENDIF}
    function Count: OHashedStringsIndex;
    procedure Clear;
    procedure Delete(const aIndex: OHashedStringsIndex);//Warning: the indexes are changed after delete!!!
  end;

  TOHashedStringDictionaryEnum = class;
  {$IFDEF O_GENERICS}
  TOHashedStringDictionaryEnumPair = TPair<OWideString,OWideString>;
  {$ELSE}
  TOHashedStringDictionaryEnumPair = record
    Key: OWideString;
    Value: OWideString;
  end;
  {$ENDIF}

  TOHashedStringDictionary = class(TPersistent)
  private
    fKeys: TOHashedStrings;
    fValues: TOWideStringList;
    function GetKey(const aIndex: OHashedStringsIndex): OWideString;
    function GetValue(const aIndex: OHashedStringsIndex): OWideString;
    procedure SetValue(const aIndex: OHashedStringsIndex; aValue: OWideString);
    function GetValueOfKey(const aKey: OWideString): OWideString;
    procedure SetValueOfKey(const aKey, aValue: OWideString);
    function GetPair(const aIndex: OHashedStringsIndex): TOHashedStringDictionaryEnumPair;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function IndexOf(const aKey: OWideString): OHashedStringsIndex;
    function IndexOfFast(const aKey: OFastString): OHashedStringsIndex;
    function Add(const aKey, aValue: OWideString): OHashedStringsIndex;
    function TryGetValue(const aKey: OWideString; var aValue: OWideString): Boolean;
    function Count: OHashedStringsIndex;
    procedure Clear;
    procedure Delete(const aIndex: OHashedStringsIndex);
    procedure Remove(const aKey: OWideString);

    property Keys[const aIndex: OHashedStringsIndex]: OWideString read GetKey;
    property Values[const aIndex: OHashedStringsIndex]: OWideString read GetValue write SetValue;
    property Items[const aKey: OWideString]: OWideString read GetValueOfKey write SetValueOfKey; default;
    property Pairs[const aIndex: OHashedStringsIndex]: TOHashedStringDictionaryEnumPair read GetPair;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TOHashedStringDictionaryEnum;
    {$ENDIF}
  end;

  TOHashedStringDictionaryEnum = class(TObject)
  private
    fIndex: OHashedStringsIndex;
    fDictionary: TOHashedStringDictionary;
  public
    constructor Create(aDictionary: TOHashedStringDictionary);
    function GetCurrent: TOHashedStringDictionaryEnumPair;
    function MoveNext: Boolean;
  public
    property Current: TOHashedStringDictionaryEnumPair read GetCurrent;
  end;

function OHashedStringsIndexAssigned(const aId: OHashedStringsIndex): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}

const
  OHASHEDSTRINGSINDEX_UNASSIGNED = -1;

implementation

function OHashedStringsIndexAssigned(const aId: OHashedStringsIndex): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}
begin
  Result := aId <> OHASHEDSTRINGSINDEX_UNASSIGNED;
end;

{ TOHashedStrings }

function TOHashedStrings.Add(const aText: OWideString;
  var aNewEntry: Boolean): OHashedStringsIndex;
var
  xStorageText: OFastString;
  {$IFDEF FPC}
  xValue: ONativeInt;
  {$ELSE}
  xValue: OHashedStringsIndex;
  {$ENDIF}
begin
  xStorageText := OWideToFast(aText);
  xValue := IndexOfFast(xStorageText);

  aNewEntry := (xValue < 0);
  if aNewEntry then begin
    xValue := fTextList.Add(xStorageText);

    if fTextList.Count < fMaxItemsBeforeGrow then begin
      fHashTable.Add(xStorageText, {$IFDEF FPC}{%H-}Pointer(xValue){$ELSE}xValue{$ENDIF});
    end else begin
      Grow;
    end;
  end;
  Result := xValue;
end;

procedure TOHashedStrings.AssignTo(Dest: TPersistent);
var
  xDest: TOHashedStrings;
begin
  if Dest is TOHashedStrings then begin
    xDest := TOHashedStrings(Dest);

    xDest.Clear;
    xDest.fTextList.Assign(Self.fTextList);
    xDest.fLastHashI := Self.fLastHashI;
    xDest.RefreshHashTable;
  end else
    inherited;
end;

procedure TOHashedStrings.Clear;
begin
  fHashTable.Clear;
  fTextList.Clear;
end;

function TOHashedStrings.Count: OHashedStringsIndex;
begin
  Result := fTextList.Count;
end;

constructor TOHashedStrings.Create;
begin
  inherited Create;

  fTextList := TStringList.Create;
  RefreshHashTable;
end;

procedure TOHashedStrings.Delete(const aIndex: OHashedStringsIndex);
begin
  fTextList.Delete(aIndex);

  RefreshHashTable;
end;

destructor TOHashedStrings.Destroy;
begin
  fHashTable.Free;
  fTextList.Free;

  inherited;
end;

function TOHashedStrings.Add(const aText: OWideString): OHashedStringsIndex;
var
  x: Boolean;
begin
  Result := Add(aText, {%H-}x);
end;

function TOHashedStrings.Get(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := OFastToWide(fTextList[aIndex]);
end;

function TOHashedStrings.GetObject(const aIndex: OHashedStringsIndex): TObject;
begin
  Result := fTextList.Objects[aIndex];
end;

procedure TOHashedStrings.Grow;
begin
  Inc(fLastHashI);

  RefreshHashTable;
end;

function TOHashedStrings.IndexOf(const aText: OWideString): OHashedStringsIndex;
begin
  Result := IndexOfFast(OWideToFast(aText));
end;

function TOHashedStrings.IndexOfFast(
  const aText: OFastString): OHashedStringsIndex;
{$IFDEF FPC}
var
  xValue: ONativeInt;
{$ENDIF}
begin
  {$IFDEF FPC}
  xValue := fHashTable.FindIndexOf(aText);
  if xValue >= 0 then
    Result := {%H-}ONativeInt(fHashTable.Items[xValue])
  else
    Result := -1;
  {$ELSE}
  Result := fHashTable.ValueOf(aText);
  {$ENDIF}
end;

procedure TOHashedStrings.RefreshHashTable;
var
  I: OHashedStringsIndex;
  xTableSize: LongWord;
const
  cHashTable: Array[0..27] of LongWord =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );
begin
  if Assigned(fHashTable) then
    fHashTable.Free;

  xTableSize := cHashTable[fLastHashI];

  {$IFDEF FPC}
  fHashTable := TFPHashList.Create;
  fHashTable.Capacity := xTableSize;
  {$ELSE}
  fHashTable := TStringHash.Create(xTableSize);
  {$ENDIF}
  fMaxItemsBeforeGrow := (xTableSize * 2) div 3;

  for I := 0 to fTextList.Count-1 do
    fHashTable.Add(fTextList[I], {$IFDEF FPC}{%H-}Pointer(I){$ELSE}I{$ENDIF});
end;

procedure TOHashedStrings.SetObject(const aIndex: OHashedStringsIndex; const aObject: TObject);
begin
  fTextList.Objects[aIndex] := aObject;
end;

{$IFNDEF NEXTGEN}
function TOHashedStrings.GetPObject(const aIndex: OHashedStringsIndex): Pointer;
begin
  Result := Pointer(fTextList.Objects[aIndex]);//unsave but fine
end;

procedure TOHashedStrings.SetPObject(const aIndex: OHashedStringsIndex; const aObject: Pointer);
begin
  fTextList.Objects[aIndex] := TObject(aObject);//unsave but fine!
end;
{$ENDIF}

{ TOHashedStringDictionary }

{$IFDEF O_ENUMERATORS}
function TOHashedStringDictionary.GetEnumerator: TOHashedStringDictionaryEnum;
begin
  Result := TOHashedStringDictionaryEnum.Create(Self);
end;
{$ENDIF}

function TOHashedStringDictionary.GetKey(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fKeys.Get(aIndex);
end;

function TOHashedStringDictionary.GetPair(
  const aIndex: OHashedStringsIndex): TOHashedStringDictionaryEnumPair;
begin
  Result.Key := Keys[aIndex];
  Result.Value := Values[aIndex];
end;

function TOHashedStringDictionary.GetValue(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fValues[aIndex];
end;

function TOHashedStringDictionary.GetValueOfKey(
  const aKey: OWideString): OWideString;
begin
  Result := fValues[fKeys.IndexOf(aKey)];
end;

function TOHashedStringDictionary.IndexOf(
  const aKey: OWideString): OHashedStringsIndex;
begin
  Result := fKeys.IndexOf(aKey);
end;

function TOHashedStringDictionary.IndexOfFast(
  const aKey: OFastString): OHashedStringsIndex;
begin
  Result := fKeys.IndexOfFast(aKey);
end;

procedure TOHashedStringDictionary.Remove(const aKey: OWideString);
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := IndexOf(aKey);
  Delete(xIndex);
end;

procedure TOHashedStringDictionary.SetValue(const aIndex: OHashedStringsIndex; aValue: OWideString
  );
begin
  fValues[aIndex] := aValue;
end;

procedure TOHashedStringDictionary.SetValueOfKey(const aKey,
  aValue: OWideString);
begin
  fValues[fKeys.IndexOf(aKey)] := aValue;
end;

constructor TOHashedStringDictionary.Create;
begin
  inherited Create;

  fKeys := TOHashedStrings.Create;
  fValues := TOWideStringList.Create;
end;

procedure TOHashedStringDictionary.Delete(const aIndex: OHashedStringsIndex);
begin
  fKeys.Delete(aIndex);
  fValues.Delete(aIndex);
end;

destructor TOHashedStringDictionary.Destroy;
begin
  fKeys.Free;
  fValues.Free;

  inherited Destroy;
end;

function TOHashedStringDictionary.Add(const aKey, aValue: OWideString
  ): OHashedStringsIndex;
var
  xNew: Boolean;
begin
  Result := fKeys.Add(aKey, {%H-}xNew);
  if xNew then
    fValues.Add(aValue)
  else
    fValues[Result] := aValue;
end;

function TOHashedStringDictionary.TryGetValue(const aKey: OWideString;
  var aValue: OWideString): Boolean;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := fKeys.IndexOf(aKey);
  Result := (xIndex >= 0);
  if Result then
    aValue := fValues[xIndex];
end;

function TOHashedStringDictionary.Count: OHashedStringsIndex;
begin
  Result := fKeys.Count;
end;

procedure TOHashedStringDictionary.AssignTo(Dest: TPersistent);
var
  xDest: TOHashedStringDictionary;
begin
  if Dest is TOHashedStringDictionary then begin
    xDest := TOHashedStringDictionary(Dest);

    xDest.fKeys.Assign(Self.fKeys);
    xDest.fValues.Assign(Self.fValues);
  end else
    inherited;
end;

procedure TOHashedStringDictionary.Clear;
begin
  fValues.Clear;
  fKeys.Clear;
end;

{ TOHashedStringDictionaryEnum }

constructor TOHashedStringDictionaryEnum.Create(
  aDictionary: TOHashedStringDictionary);
begin
  inherited Create;

  fIndex := -1;
  fDictionary := aDictionary;
end;

function TOHashedStringDictionaryEnum.GetCurrent: TOHashedStringDictionaryEnumPair;
begin
  Result := fDictionary.Pairs[fIndex];
end;

function TOHashedStringDictionaryEnum.MoveNext: Boolean;
begin
  Result := (fIndex < fDictionary.Count - 1);
  if Result then
    Inc(fIndex);
end;

end.

