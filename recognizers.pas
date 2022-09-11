unit Recognizers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, CharOrNewLineReaders;

type

  TCharOrNewLineArray = array of TCharOrNewLine;

  TRecognition = (rNegative, rPartial, rPositive);

  TTokenRecognizer = class
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; virtual; abstract;
  end;

  TTokenRecognizers = array of TTokenRecognizer;

  TCharPredicate = function(Ch: Char): Boolean;

  TPredicateRecognizer = class(TTokenRecognizer)
  private
    FPredicate: TCharPredicate;
  public
    constructor Create(Predicate: TCharPredicate);
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

  TLeadingPredicateRecognizer = class(TTokenRecognizer)
  private
    FLeadingPredicate: TCharPredicate;
    FRemainingPredicate: TCharPredicate;
  public
    constructor Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate);
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

  TAnyRecognizer = class(TTokenRecognizer)
  public
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

  TNewLineRecognizer = class(TTokenRecognizer)
  public
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

  TSingleCharRecognizer = class(TTokenRecognizer)
  private
    FAllowedChars: String;
  public
    constructor Create(AllowedChars: String);
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

  TStringRecognizer = class(TTokenRecognizer)
  private
    FValue: String;
  public
    constructor Create(Value: String);
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

  TKeywordRecognizer = class(TTokenRecognizer)
  private
    FKeywords: TStringList;
  public
    constructor Create(Keywords: array of String);
    destructor Destroy; override;
    function Recognize(Buffer: TCharOrNewLineArray): TRecognition; override;
  end;

function IsDigit(Ch: Char): Boolean;
function IsLetter(Ch: Char): Boolean;
function IsWhiteSpace(Ch: Char): Boolean;

implementation

function IsDigit(Ch: Char): Boolean;
begin
  Result := (Ch >= '0') and (Ch <= '9');
end;

function IsLetter(Ch: Char): Boolean;
begin
  Result := ((Ch >= 'a') and (Ch <= 'z')) or ((Ch >= 'A') and (Ch <= 'Z'));
end;

function IsWhiteSpace(Ch: Char): Boolean;
begin
  Result := (Ch = ' ') or (Ch = #9);
end;

(* Predicate *)

constructor TPredicateRecognizer.Create(Predicate: TCharPredicate);
begin
  FPredicate := Predicate;
end;

function TPredicateRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
var
  i: Integer;
begin
  i := Low(Buffer);
  while (i <= High(Buffer)) and (Buffer[i].Kind = ckChar) and (FPredicate(Buffer[i].Ch)) do
    Inc(i);
  if i > High(Buffer) then
    Result := rPositive
  else
    Result := rNegative
end;

(* LeadingPredicate *)

constructor TLeadingPredicateRecognizer.Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate);
begin
  FLeadingPredicate := LeadingPredicate;
  FRemainingPredicate := RemainingPredicate;
end;

function TLeadingPredicateRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
var
  i: Integer;
begin
  i := Low(Buffer);
  while (i <= High(Buffer)) and (Buffer[i].Kind = ckChar) and (
    ((i = 0) and FLeadingPredicate(Buffer[i].Ch))
    or
    ((i > 0) and FRemainingPredicate(Buffer[i].Ch))
  ) do
    Inc(i);
  if i > High(Buffer) then
    Result := rPositive
  else
    Result := rNegative
end;

(* Any *)

function TAnyRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
begin
  // recognize any *single* "LineRead"
  if Length(Buffer) = 1 then
    Result := rPositive
  else
    Result := rNegative
end;

(* New Line *)
function TNewLineRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
begin
  if (Length(Buffer) = 1) and (Buffer[0].Kind = ckEol) then
    Result := rPositive
  else
    Result := rNegative
end;

(* Single Char *)

constructor TSingleCharRecognizer.Create(AllowedChars: String);
begin
  FAllowedChars := AllowedChars;
end;

function TSingleCharRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
begin
  if (Length(Buffer) = 1) and (Buffer[0].Kind = ckChar) then
  begin
    if Pos(Buffer[0].Ch, FAllowedChars) > 0 then
      Result := rPositive
    else
      Result := rNegative
  end
  else
    Result := rNegative;
end;

(* Specific String *)

constructor TStringRecognizer.Create(Value: String);
begin
  if Length(Value) <= 0 then
    raise Exception.Create('Empty string');
  FValue := Value;
end;

function TStringRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
var
  i: Integer;
begin
  i := Low(Buffer);
  while (i <= High(Buffer)) and (i < Length(FValue)) and (Buffer[i].Kind = ckChar) and (Buffer[i].Ch = FValue.Chars[i]) do
    Inc(i);
  if i > High(Buffer) then
  begin
    if i = Length(FValue) then
      Result := rPositive
    else
      Result := rPartial
  end
  else
    Result := rNegative;
end;

(* Keywords *)

constructor TKeywordRecognizer.Create(Keywords: array of String);
var
  Keyword: String;
begin
  FKeywords := TStringList.Create;
  FKeywords.Sorted := True;
  for Keyword in Keywords do
    FKeywords.Add(Keyword);
end;

destructor TKeywordRecognizer.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

function TKeywordRecognizer.Recognize(Buffer: TCharOrNewLineArray): TRecognition;
var
  Keyword: String;
  Index: Integer;
begin
  Index := Low(Buffer);
  Keyword := '';
  while (Index <= High(Buffer)) and (Buffer[Index].Kind = ckChar) do
  begin
    Keyword := Keyword + Buffer[Index].Ch;
    Inc(Index);
  end;
  if (Index > High(Buffer)) and (Keyword <> '') then
    if FKeywords.Find(Keyword, Index) then
      Result := rPositive
    else
    begin
      // search one by one
      Index := 0;
      while (Index < FKeywords.Count) and (not FKeywords[Index].StartsWith(Keyword)) do
        Inc(Index);
      if Index < FKeywords.Count then
        Result := rPartial
      else
        Result := rNegative
    end
  else
    Result := rNegative
end;

end.
