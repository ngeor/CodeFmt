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

  TNeedleRecognizer = class(TTokenRecognizer)
  private
    FHay: String;
  public
    constructor Create(Hay: String);
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
  Result := (Ch = ' ') or (Ch = '\t');
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

(* Hay *)

constructor TNeedleRecognizer.Create(Hay: String);
begin
  FHay := Hay;
end;

function TNeedleRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
begin
  if (Length(Buffer) = 1) and (Buffer[0].Kind = ckChar) then
    Result := Pos(Buffer[0].Ch, FHay) > 0
  else
    Result := False;
end;

end.
