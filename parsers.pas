unit Parsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Tokenizers, ParseResult;

type
  { Base class for all parsers }
  TParser<T> = class
  public
    function Parse(Source: TUndoTokenizer): TParseResult<T>; virtual; abstract;
    procedure Undo(Source: TUndoTokenizer; Data: T); virtual; abstract;
    function OrElse(Next: TParser<T>): TParser<T>;
  end;

  { A parser that filters the result of another parser }
  TFilterParser<T> = class(TParser<T>)
  private
    FParser: TParser<T>;
  protected
    function Filter(Data: T): Boolean; virtual; abstract;
  public
    constructor Create(Parser: TParser<T>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<T>; override;
    procedure Undo(Source: TUndoTokenizer; Data: T); override;
  end;

  { A parser that calls another parser while it succeeds and collects all results. This parser always succeeds (i.e. can return empty list). }
  TManyParser<T, U> = class(TParser<U>)
  private
    FParser: TParser<T>;
  protected
    function CreateSeed: U; virtual; abstract;
    procedure Collect(Aggregate: U; Data: T); virtual; abstract;
  public
    constructor Create(Parser: TParser<T>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<U>; override;
  end;

  { Base class for parsers that transform their result into a different type. Undo must be provided by the subclass. }
  TMapParser<T, U> = class(TParser<U>)
  private
    FParser: TParser<T>;
  protected
    function Map(Data: T): U; virtual; abstract;
  public
    constructor Create(Parser: TParser<T>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<U>; override;
  end;

function OrElse<T>(Left: TParser<T>; Right: TParser<T>): TParser<T>;

implementation

uses BinaryParsers; // not circular reference as long as it's in the implementation

function TParser<T>.OrElse(Next: TParser<T>): TParser<T>;
begin
  Result := TOrParser<T>.Create(Self, Next);
end;

function OrElse<T>(Left: TParser<T>; Right: TParser<T>): TParser<T>;
begin
  if Assigned(Left) then
    if Assigned(Right) then
      Result := Left.OrElse(Right)
    else
      Result := Left
  else
    Result := Right
end;

(* Filter *)

constructor TFilterParser<T>.Create(Parser: TParser<T>);
begin
  FParser := Parser;
end;

destructor TFilterParser<T>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TFilterParser<T>.Parse(Source: TUndoTokenizer): TParseResult<T>;
var
  Next: TParseResult<T>;
begin
  Next := FParser.Parse(Source);
  if Next.Success then
  begin
    if Filter(Next.Data) then
      Result := Next
    else
    begin
      Undo(Source, Next.Data);
      Result := FailedParseResult<T>();
    end
  end
  else
    Result := Next
end;

procedure TFilterParser<T>.Undo(Source: TUndoTokenizer; Data: T);
begin
  FParser.Undo(Source, Data);
end;

(* Many *)

constructor TManyParser<T, U>.Create(Parser: TParser<T>);
begin
  FParser := Parser;
end;

destructor TManyParser<T, U>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TManyParser<T, U>.Parse(Source: TUndoTokenizer): TParseResult<U>;
var
  Next: TParseResult<T>;
  Data: U;
begin
  Data := CreateSeed;
  repeat
    Next := FParser.Parse(Source);
    if Next.Success then
      Collect(Data, Next.Data);
  until not Next.Success;
  Result := SuccessParseResult<U>(Data);
end;

(* Map *)

constructor TMapParser<T, U>.Create(Parser: TParser<T>);
begin
  FParser := Parser;
end;

destructor TMapParser<T, U>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TMapParser<T, U>.Parse(Source: TUndoTokenizer): TParseResult<U>;
var
  Next: TParseResult<T>;
begin
  Next := FParser.Parse(Source);
  if Next.Success then
    Result := SuccessParseResult<U>(Map(Next.Data))
  else
    Result := FailedParseResult<U>()
end;

end.
