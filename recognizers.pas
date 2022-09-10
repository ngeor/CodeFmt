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
    destructor Destroy; override;
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

  TAnyRecognizer = class(TTokenRecognizer)
  private
  public
    constructor Create;
    destructor Destroy; override;
    function Recognize(Buffer: TCharOrNewLineArray): Boolean; override;
  end;

function IsDigit(Ch: Char): Boolean;
function IsLetter(Ch: Char): Boolean;

implementation

function IsDigit(Ch: Char): Boolean;
begin
  Result := (Ch >= '0') and (Ch <= '9');
end;

function IsLetter(Ch: Char): Boolean;
begin
  Result := ((Ch >= 'a') and (Ch <= 'z')) or ((Ch >= 'A') and (Ch <= 'Z'));
end;

constructor TPredicateRecognizer.Create(Predicate: TCharPredicate);
begin
  FPredicate := Predicate;
end;

destructor TPredicateRecognizer.Destroy;
begin
  inherited Destroy;
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

constructor TAnyRecognizer.Create;
begin
end;

destructor TAnyRecognizer.Destroy;
begin
  inherited Destroy;
end;

function TAnyRecognizer.Recognize(Buffer: TCharOrNewLineArray): Boolean;
begin
  // recognize any single "LineRead"
  Result := Length(Buffer) = 1;
end;


end.
