unit Recognizers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, CharOrNewLineReaders;

type

  TCharOrNewLineArray = array of TCharOrNewLine;

  TTokenRecognizer = class
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; virtual; abstract;
  end;

  TTokenRecognizers = array of TTokenRecognizer;

  TCharPredicate = function(Ch: Char): Boolean;

  TPredicateRecognizer = class(TTokenRecognizer)
  private
    FPredicate: TCharPredicate;
  public
    constructor Create(Predicate: TCharPredicate);
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TLeadingPredicateRecognizer = class(TTokenRecognizer)
  private
    FLeadingPredicate: TCharPredicate;
    FRemainingPredicate: TCharPredicate;
  public
    constructor Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate);
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TAnyRecognizer = class(TTokenRecognizer)
  public
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TNewLineRecognizer = class(TTokenRecognizer)
  public
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TSingleCharRecognizer = class(TTokenRecognizer)
  private
    FAllowedChars: String;
  public
    constructor Create(AllowedChars: String);
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TStringRecognizer = class(TTokenRecognizer)
  private
    FValue: String;
  public
    constructor Create(Value: String);
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TKeywordRecognizer = class(TTokenRecognizer)
  private
    FKeywords: TStringList;
  public
    constructor Create(Keywords: array of String);
    destructor Destroy; override;
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
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

function TPredicateRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
var
  i: Integer;
begin
  i := Low(Buffer);
  while (i <= High(Buffer)) and (Buffer[i].Kind = ckChar) and (FPredicate(Buffer[i].Ch)) do
    Inc(i);
  Result := i > High(Buffer);
end;

(* LeadingPredicate *)

constructor TLeadingPredicateRecognizer.Create(LeadingPredicate: TCharPredicate; RemainingPredicate: TCharPredicate);
begin
  FLeadingPredicate := LeadingPredicate;
  FRemainingPredicate := RemainingPredicate;
end;

function TLeadingPredicateRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
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
  Result := i > High(Buffer);
end;

(* Any *)

function TAnyRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
begin
  // recognize any single "LineRead"
  Result := Length(Buffer) = 1;
end;

(* New Line *)
function TNewLineRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
begin
  Result := (Length(Buffer) = 1) and (Buffer[0].Kind = ckEol);
end;

(* Single Char *)

constructor TSingleCharRecognizer.Create(AllowedChars: String);
begin
  FAllowedChars := AllowedChars;
end;

function TSingleCharRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
begin
  if (Length(Buffer) = 1) and (Buffer[0].Kind = ckChar) then
    Result := Pos(Buffer[0].Ch, FAllowedChars) > 0
  else
    Result := False;
end;

(* Specific String *)

constructor TStringRecognizer.Create(Value: String);
begin
  FValue := Value;
end;

function TStringRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
var
  i: Integer;
begin
  if (Length(Buffer) = Length(FValue)) and (Length(Buffer) > 0) then
  begin
    i := Low(Buffer);
    while (i <= High(Buffer)) and (Buffer[i].Kind = ckChar) and (Buffer[i].Ch = FValue[i]) do
      Inc(i);
    Result := i > High(Buffer);
  end
  else
    Result := False;
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

function TKeywordRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
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
    Result := FKeywords.Find(Keyword, Index)
  else
    Result := False
end;

end.
