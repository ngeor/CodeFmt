unit Parsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Tokenizers, Types;

type
  { The result of the parsing operation }
  TParseResult<T> = record
    Success: Boolean;
    Data: T;
  end;

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

  (* And *)

  TPair<L, R> = record
    Left: L;
    Right: R;
  end;

  TAndParser<L, R> = class(TParser<TPair<L, R>>)
  private
    FLeft: TParser<L>;
    FRight: TParser<R>;
  public
    constructor Create(Left: TParser<L>; Right: TParser<R>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TPair<L, R>>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TPair<L, R>); override;
  end;

function Seq<L, R>(Left: TParser<L>; Right: TParser<R>): TParser<TPair<L, R>>; overload;
function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;

type
  (* Map *)
  TMapper<T, U> = function(x : T): U;
  TFunctionMapParser<T, U> = class(TMapParser<T, U>)
  private
    FMapper: TMapper<T, U>;
  protected
    function Map(Data: T): U; override;
  public
    constructor Create(Parser: TParser<T>; Mapper: TMapper<T, U>);
    procedure Undo(Source: TUndoTokenizer; Data: U); override;
  end;

type
  (* Or *)
  TOrParser<T> = class(TParser<T>)
  private
    FLeft: TParser<T>;
    FRight: TParser<T>;
  public
    constructor Create(Left: TParser<T>; Right: TParser<T>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<T>; override;
    procedure Undo(Source: TUndoTokenizer; Data: T); override;
  end;

type
  TAnyTokenParser = class(TParser<TToken>)
  public
    function Parse(Source: TUndoTokenizer): TParseResult<TToken>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TToken); override;
  end;

type
  TMapTokenToTokenListParser = class(TMapParser<TToken, TTokenLinkedList>)
  protected
    function Map(Token: TToken): TTokenLinkedList; override;
  public
    procedure Undo(Source: TUndoTokenizer; Data: TTokenLinkedList); override;
  end;

  TManyTokensParser = class(TManyParser<TToken, TTokenLinkedList>)
  protected
    function CreateSeed: TTokenLinkedList; override;
    procedure Collect(Aggregate: TTokenLinkedList; Data: TToken); override;
  public
    procedure Undo(Source: TUndoTokenizer; Data: TTokenLinkedList); override;
  end;


implementation

function TParser<T>.OrElse(Next: TParser<T>): TParser<T>;
begin
  Result := TOrParser<T>.Create(Self, Next);
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
      Result.Success := False;
      Undo(Source, Next.Data);
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
begin
  Result.Success := True;
  Result.Data := CreateSeed;
  repeat
    Next := FParser.Parse(Source);
    if Next.Success then
      Collect(Result.Data, Next.Data);
  until not Next.Success;
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
  Result.Success := Next.Success;
  if Next.Success then
    Result.Data := Map(Next.Data);
end;

(* And *)

constructor TAndParser<L, R>.Create(Left: TParser<L>; Right: TParser<R>);
begin
  FLeft := Left;
  FRight := Right;
end;

destructor TAndParser<L, R>.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited Destroy;
end;

function TAndParser<L, R>.Parse(Source: TUndoTokenizer): TParseResult<TPair<L, R>>;
var
  Left: TParseResult<L>;
  Right: TParseResult<R>;
begin
  Left := FLeft.Parse(Source);
  if Left.Success then
  begin
    Right := FRight.Parse(Source);
    if Right.Success then
    begin
      Result.Success := True;
      Result.Data.Left := Left.Data;
      Result.Data.Right := Right.Data;
    end
    else
    begin
      FLeft.Undo(Source, Left.Data);
      Result.Success := False;
    end
  end
  else
  begin
    Result.Success := False;
  end
end;

procedure TAndParser<L, R>.Undo(Source: TUndoTokenizer; Data: TPair<L, R>);
begin
  FRight.Undo(Source, Data.Right);
  FLeft.Undo(Source, Data.Left);
end;

function Seq<L, R>(Left: TParser<L>; Right: TParser<R>): TParser<TPair<L, R>>; overload;
begin
  Result := TAndParser<L, R>.Create(Left, Right);
end;

type
  TListPair = TPair<TTokenLinkedList, TTokenLinkedList>;

function MergeLists(Pair: TListPair): TTokenLinkedList;
begin
  if Pair.Right.IsEmpty then
  begin
    Pair.Right.Free;
    Result := Pair.Left;
  end
  else
  begin
    Pair.Right.Append(Pair.Left);
    Pair.Left.Free;
    Result := Pair.Right;
  end
end;

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
begin
  // TODO this is the only usage of TFunctionMapParser<T, U> try to remove it
  Result := TFunctionMapParser<TListPair, TTokenLinkedList>.Create(
    TAndParser<TTokenLinkedList, TTokenLinkedList>.Create(Left, Right),
    MergeLists
  );
end;

function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
begin
  Result := Seq(TMapTokenToTokenListParser.Create(Left), Right);
end;

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;
begin
  Result := Seq(Left, TMapTokenToTokenListParser.Create(Right));
end;

(* FunctionMap *)

constructor TFunctionMapParser<T, U>.Create(Parser: TParser<T>; Mapper: TMapper<T, U>);
begin
  inherited Create(Parser);
  FMapper := Mapper;
end;

function TFunctionMapParser<T, U>.Map(Data: T): U;
begin
  Result := FMapper(Data);
end;

procedure TFunctionMapParser<T, U>.Undo(Source: TUndoTokenizer; Data: U);
begin
  raise Exception.Create('not possible');
end;

(* Or *)

constructor TOrParser<T>.Create(Left: TParser<T>; Right: TParser<T>);
begin
  FLeft := Left;
  FRight := Right;
end;

destructor TOrParser<T>.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited Destroy;
end;

function TOrParser<T>.Parse(Source: TUndoTokenizer): TParseResult<T>;
begin
  Result := FLeft.Parse(Source);
  if not Result.Success then
    Result := FRight.Parse(Source);
end;

procedure TOrParser<T>.Undo(Source: TUndoTokenizer; Data: T);
begin
  FLeft.Undo(Source, Data); // TODO: remember which parser was actually used?
end;

(* AnyToken *)

function TAnyTokenParser.Parse(Source: TUndoTokenizer): TParseResult<TToken>;
begin
  Result.Data := Source.Read;
  Result.Success := Result.Data.Kind >= 0;
end;

procedure TAnyTokenParser.Undo(Source: TUndoTokenizer; Data: TToken);
begin
  Source.Undo(Data);
end;

(* MapTokenToTokenList *)

function TMapTokenToTokenListParser.Map(Token: TToken): TTokenLinkedList;
begin
  Result := TTokenLinkedList.Create;
  Result.Push(Token);
end;

procedure TMapTokenToTokenListParser.Undo(Source: TUndoTokenizer; Data: TTokenLinkedList);
begin
  while not Data.IsEmpty do
    FParser.Undo(Source, Data.Pop);
  Data.Free;
end;

(* ManyTokens *)

function TManyTokensParser.CreateSeed: TTokenLinkedList;
begin
  Result := TTokenLinkedList.Create;
end;

procedure TManyTokensParser.Collect(Aggregate: TTokenLinkedList; Data: TToken);
begin
  Aggregate.Push(Data);
end;

procedure TManyTokensParser.Undo(Source: TUndoTokenizer; Data: TTokenLinkedList);
begin
  Source.Undo(Data);
end;

end.
