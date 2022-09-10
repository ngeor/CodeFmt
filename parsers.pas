unit Parsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Tokenizers, Types;

type
  TParseResult<T> = record
    Success: Boolean;
    Data: T;
  end;

  TParser<T> = class
  public
    function Parse(Source: TUndoTokenizer): TParseResult<T>; virtual; abstract;
    procedure Undo(Source: TUndoTokenizer; Data: T); virtual; abstract;
  end;

  (* TokenKind *)

  TTokenKindParser = class(TParser<TToken>)
  private
    FTokenKinds: TWordDynArray;
    function Matches(TokenKind: Word): Boolean;
  public
    constructor Create(TokenKind: Word); overload;
    constructor Create(TokenKinds: TWordDynArray); overload;
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TToken>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TToken); override;
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
type
  (* Map *)
  TMapper<T, U> = function(x : T): U;
  TMapParser<T, U> = class(TParser<U>)
  private
    FParser: TParser<T>;
    FMapper: TMapper<T, U>;
  public
    constructor Create(Parser: TParser<T>; Mapper: TMapper<T, U>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<U>; override;
    procedure Undo(Source: TUndoTokenizer; Data: U); override;
  end;

function Map<T, U>(Parser: TParser<T>; Mapper: TMapper<T, U>): TParser<U>;

type
  (* Many *)
  TManyParser<T: record> = class(TParser<TList>)
  private
    FParser: TParser<T>;
  public
    constructor Create(Parser: TParser<T>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TList>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TList); override;
  end;

type
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
  TMapTokenToTokenListParser = class(TParser<TTokenLinkedList>)
  private
    FParser: TParser<TToken>;
  public
    constructor Create(Parser: TParser<TToken>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TTokenLinkedList>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TTokenLinkedList); override;
  end;

  TManyTokensParser = class(TParser<TTokenLinkedList>)
  private
    FParser: TParser<TToken>;
  public
    constructor Create(Parser: TParser<TToken>);
    destructor Destroy; override;
    function Parse(Source: TUndoTokenizer): TParseResult<TTokenLinkedList>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TTokenLinkedList); override;
  end;


implementation

(* TokenKind *)

constructor TTokenKindParser.Create(TokenKind: Word);
begin
  FTokenKinds := [TokenKind];
end;

constructor TTokenKindParser.Create(TokenKinds: TWordDynArray);
begin
  FTokenKinds := TokenKinds;
end;

destructor TTokenKindParser.Destroy;
begin
  inherited Destroy;
end;

function TTokenKindParser.Parse(Source: TUndoTokenizer): TParseResult<TToken>;
var
  Token: TToken;
begin
  Token := Source.Read;
  if Matches(Token.Kind) then
  begin
    Result.Success := True;
    Result.Data := Token;
  end
  else
  begin
    Source.Undo(Token);
    Result.Success := False;
  end
end;

procedure TTokenKindParser.Undo(Source: TUndoTokenizer; Data: TToken);
begin
  Source.Undo(Data);
end;

function TTokenKindParser.Matches(TokenKind: Word): Boolean;
var
  i: Integer;
begin
  i := Low(FTokenKinds);
  while (i <= High(FTokenKinds)) and (FTokenKinds[i] <> TokenKind) do
    Inc(i);
  Result := i <= High(FTokenKinds);
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
  Result := TMapParser<TListPair, TTokenLinkedList>.Create(
    TAndParser<TTokenLinkedList, TTokenLinkedList>.Create(Left, Right),
    MergeLists
  );
end;

(* Map *)

constructor TMapParser<T, U>.Create(Parser: TParser<T>; Mapper: TMapper<T, U>);
begin
  FParser := Parser;
  FMapper := Mapper;
end;

destructor TMapParser<T, U>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TMapParser<T, U>.Parse(Source: TUndoTokenizer): TParseResult<U>;
var
  temp: TParseResult<T>;
begin
  temp := FParser.Parse(Source);
  if temp.Success then
  begin
    Result.Success := True;
    Result.Data := FMapper(temp.Data);
  end
  else
    Result.Success := False;
end;

procedure TMapParser<T, U>.Undo(Source: TUndoTokenizer; Data: U);
begin
  raise Exception.Create('not possible');
end;

function Map<T, U>(Parser: TParser<T>; Mapper: TMapper<T, U>): TParser<U>;
begin
  Result := TMapParser<T, U>.Create(Parser, Mapper);
end;

(* Many *)

constructor TManyParser<T: record>.Create(Parser: TParser<T>);
begin
  FParser := Parser;
end;

destructor TManyParser<T: record>.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TManyParser<T: record>.Parse(Source: TUndoTokenizer): TParseResult<TList>;
var
  List: TList;
  Item: TParseResult<T>;
  P: ^T;
begin
  List := TList.Create;
  repeat
    Item := FParser.Parse(Source);
    if Item.Success then
    begin
      New(P);
      P^ := Item.Data;
      List.Add(P);
    end;
  until not Item.Success;
  if List.Count > 0 then
  begin
    Result.Success := True;
    Result.Data := List;
  end
  else
  begin
    Result.Success := False;
    Result.Data := nil;
    List.Free;
  end
end;

procedure TManyParser<T: record>.Undo(Source: TUndoTokenizer; Data: TList);
var
  i: Integer;
  P: ^T;
begin
  i := Data.Count - 1;
  while i >= 0 do
  begin
    P := Data[i];
    FParser.Undo(Source, P^);
    Dispose(P);
    Dec(i);
  end;
  Data.Free;
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

constructor TMapTokenToTokenListParser.Create(Parser: TParser<TToken>);
begin
  FParser := Parser;
end;

destructor TMapTokenToTokenListParser.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TMapTokenToTokenListParser.Parse(Source: TUndoTokenizer): TParseResult<TTokenLinkedList>;
var
  Temp: TParseResult<TToken>;
begin
  Temp := FParser.Parse(Source);
  if Temp.Success then
  begin
    Result.Success := True;
    Result.Data := TTokenLinkedList.Create;
    Result.Data.Push(Temp.Data);
  end
  else
    Result.Success := False
end;

procedure TMapTokenToTokenListParser.Undo(Source: TUndoTokenizer; Data: TTokenLinkedList);
begin
  while not Data.IsEmpty do
    FParser.Undo(Source, Data.Pop);
  Data.Free;
end;

constructor TManyTokensParser.Create(Parser: TParser<TToken>);
begin
  FParser := Parser;
end;

destructor TManyTokensParser.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TManyTokensParser.Parse(Source: TUndoTokenizer): TParseResult<TTokenLinkedList>;
var
  Temp: TParseResult<TToken>;
begin
  Result.Success := True;
  Result.Data := TTokenLinkedList.Create;
  repeat
    Temp := FParser.Parse(Source);
    if Temp.Success then
    begin
      Result.Data.Push(Temp.Data);
    end;
  until not Temp.Success;
end;

procedure TManyTokensParser.Undo(Source: TUndoTokenizer; Data: TTokenLinkedList);
begin
  while not Data.IsEmpty do
    FParser.Undo(Source, Data.Pop);
  Data.Free;
end;

end.
