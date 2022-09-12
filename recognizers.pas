unit Recognizers;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type

  TRecognition = (rNegative, rPartial, rPositive);

  TTokenRecognizer = class
    function Recognize(Buffer: String): TRecognition; virtual; abstract;
  end;

  TTokenRecognizers = array of TTokenRecognizer;

  TCharPredicate = function(Ch: Char): Boolean;

  TPredicateRecognizer = class(TTokenRecognizer)
  private
    FPredicate: TCharPredicate;
  public
    constructor Create(Predicate: TCharPredicate);
    function Recognize(Buffer: String): TRecognition; override;
  end;

  TLeadingPredicateRecognizer = class(TTokenRecognizer)
  private
    FLeadingPredicate: TCharPredicate;
    FRemainingPredicate: TCharPredicate;
    FPartialLength: Word;
  public
    constructor Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate); overload;
    constructor Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate; PartialLength: Word); overload;
    function Recognize(Buffer: String): TRecognition; override;
  end;

  TAnyRecognizer = class(TTokenRecognizer)
  public
    function Recognize(Buffer: String): TRecognition; override;
  end;

  TNewLineRecognizer = class(TTokenRecognizer)
  public
    function Recognize(Buffer: String): TRecognition; override;
  end;

  TSingleCharRecognizer = class(TTokenRecognizer)
  private
    FAllowedChars: String;
  public
    constructor Create(AllowedChars: String);
    function Recognize(Buffer: String): TRecognition; override;
  end;

  TStringRecognizer = class(TTokenRecognizer)
  private
    FValue: String;
  public
    constructor Create(Value: String);
    function Recognize(Buffer: String): TRecognition; override;
  end;

  TKeywordCaseSensitivity = (csSensitive, csInsensitive);
  TKeywordRecognizer = class(TTokenRecognizer)
  private
    FKeywords: TStringList;
    FCaseSensitivity: TKeywordCaseSensitivity;
  public
    constructor Create(Keywords: array of String; CaseSensitivity: TKeywordCaseSensitivity);
    destructor Destroy; override;
    function Recognize(Buffer: String): TRecognition; override;
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

function PositiveOrNegative(Flag: Boolean): TRecognition;
begin
  if Flag then
    Result := rPositive
  else
    Result := rNegative
end;

(* Predicate *)

constructor TPredicateRecognizer.Create(Predicate: TCharPredicate);
begin
  FPredicate := Predicate;
end;

function TPredicateRecognizer.Recognize(Buffer: String): TRecognition;
var
  i: Integer;
begin
  i := 1;
  while (i <= Length(Buffer)) and FPredicate(Buffer[i]) do
    Inc(i);
  Result := PositiveOrNegative(i > Length(Buffer));
end;

(* LeadingPredicate *)

constructor TLeadingPredicateRecognizer.Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate);
begin
  FLeadingPredicate := LeadingPredicate;
  FRemainingPredicate := RemainingPredicate;
  FPartialLength := 0;
end;

constructor TLeadingPredicateRecognizer.Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate; PartialLength: Word);
begin
  FLeadingPredicate := LeadingPredicate;
  FRemainingPredicate := RemainingPredicate;
  FPartialLength := PartialLength;
end;

function TLeadingPredicateRecognizer.Recognize(Buffer: String): TRecognition;
var
  i: Integer;
begin
  i := 1;
  while (i <= Length(Buffer)) and (
    ((i = 1) and FLeadingPredicate(Buffer[i]))
    or
    ((i > 1) and FRemainingPredicate(Buffer[i]))
  ) do
    Inc(i);
  if i > Length(Buffer) then
  begin
    if Length(Buffer) > FPartialLength then
      Result := rPositive
    else
      Result := rPartial
  end
  else
    Result := rNegative
end;

(* Any *)

function TAnyRecognizer.Recognize(Buffer: String): TRecognition;
begin
  // recognize any *single* character, usefull as fallback
  Result := PositiveOrNegative(Length(Buffer) = 1);
end;

(* New Line *)
function TNewLineRecognizer.Recognize(Buffer: String): TRecognition;
begin
  Result := PositiveOrNegative((
    ( (Length(Buffer) = 1) and (Buffer[1] in [#10, #13]) )
    or
    ( Buffer = #13#10 )
  ));
end;

(* Single Char *)

constructor TSingleCharRecognizer.Create(AllowedChars: String);
begin
  FAllowedChars := AllowedChars;
end;

function TSingleCharRecognizer.Recognize(Buffer: String): TRecognition;
begin
  Result := PositiveOrNegative( (Length(Buffer) = 1) and (Pos(Buffer[1], FAllowedChars) > 0) );
end;

(* Specific String *)

constructor TStringRecognizer.Create(Value: String);
begin
  if Length(Value) <= 0 then
    raise Exception.Create('Empty string');
  FValue := Value;
end;

function TStringRecognizer.Recognize(Buffer: String): TRecognition;
var
  i: Integer;
begin
  i := 1;
  while (i <= Length(Buffer)) and (i <= Length(FValue)) and (Buffer[i] = FValue[i]) do
    Inc(i);
  if i > Length(Buffer) then
  begin
    if i > Length(FValue) then
      Result := rPositive
    else
      Result := rPartial
  end
  else
    Result := rNegative;
end;

(* Keywords *)

constructor TKeywordRecognizer.Create(Keywords: array of String; CaseSensitivity: TKeywordCaseSensitivity);
var
  Keyword: String;
begin
  FCaseSensitivity := CaseSensitivity;
  FKeywords := TStringList.Create;
  FKeywords.Sorted := True;
  for Keyword in Keywords do
    if FCaseSensitivity = csSensitive then
      FKeywords.Add(Keyword)
    else
      FKeywords.Add(UpCase(Keyword))
end;

destructor TKeywordRecognizer.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

function TKeywordRecognizer.Recognize(Buffer: String): TRecognition;
var
  Index: Integer;
  Needle: String;
begin
  if FCaseSensitivity = csSensitive then
    Needle := Buffer
  else
    Needle := UpCase(Buffer);
  if FKeywords.Find(Needle, Index) then
    Result := rPositive
  else
  begin
    // search one by one
    Index := 0;
    while (Index < FKeywords.Count) and (not FKeywords[Index].StartsWith(Needle)) do
      Inc(Index);
    if Index < FKeywords.Count then
      Result := rPartial
    else
      Result := rNegative
  end
end;

end.
