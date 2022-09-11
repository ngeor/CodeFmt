unit Parsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Tokenizers;

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

  TBinaryParser<L, R, T> = class(TParser<T>)
  private
    FLeft: TParser<L>;
    FRight: TParser<R>;
  public
    constructor Create(Left: TParser<L>; Right: TParser<R>);
    destructor Destroy; override;
  end;

  TAbstractAndParser<L, R, T> = class(TBinaryParser<L, R, T>)
  protected
    function Combine(Left: L; Right: R): T; virtual; abstract;
  public
    function Parse(Source: TUndoTokenizer): TParseResult<T>; override;
  end;

  (* And *)

  TPair<L, R> = record
    Left: L;
    Right: R;
  end;

  TAndParser<L, R> = class(TAbstractAndParser<L, R, TPair<L, R>>)
  protected
    function Combine(Left: L; Right: R): TPair<L, R>; override;
  public
    procedure Undo(Source: TUndoTokenizer; Data: TPair<L, R>); override;
  end;

function Seq<L, R>(Left: TParser<L>; Right: TParser<R>): TParser<TPair<L, R>>; overload;
function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;

type
  (* Or *)
  TOrParser<T> = class(TBinaryParser<T, T, T>)
  public
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

  TAndTokenListsParser = class(TAbstractAndParser<TTokenLinkedList, TTokenLinkedList, TTokenLinkedList>)
  protected
    function Combine(Left: TTokenLinkedList; Right: TTokenLinkedList): TTokenLinkedList; override;
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

(* Binary *)

constructor TBinaryParser<L, R, T>.Create(Left: TParser<L>; Right: TParser<R>);
begin
  FLeft := Left;
  FRight := Right;
end;

destructor TBinaryParser<L, R, T>.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited Destroy;
end;

(* AbstractAnd *)

function TAbstractAndParser<L, R, T>.Parse(Source: TUndoTokenizer): TParseResult<T>;
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
      Result.Data := Combine(Left.Data, Right.Data);
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

(* And *)

function TAndParser<L, R>.Combine(Left: L; Right: R): TPair<L, R>;
begin
  Result.Left := Left;
  Result.Right := Right;
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

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
begin
  Result := TAndTokenListsParser.Create(Left, Right);
end;

function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
begin
  Result := Seq(TMapTokenToTokenListParser.Create(Left), Right);
end;

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;
begin
  Result := Seq(Left, TMapTokenToTokenListParser.Create(Right));
end;

(* Or *)

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

(* AndTokenLists *)

function TAndTokenListsParser.Combine(Left: TTokenLinkedList; Right: TTokenLinkedList): TTokenLinkedList;
begin
  if Right.IsEmpty then
  begin
    Right.Free;
    Result := Left;
  end
  else
  begin
    Right.Append(Left);
    Left.Free;
    Result := Right;
  end
end;

procedure TAndTokenListsParser.Undo(Source: TUndoTokenizer; Data: TTokenLinkedList);
begin
  Source.Undo(Data);
end;

end.
