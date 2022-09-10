unit Parsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Tokenizers;

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
    FTokenKind: Word;
  public
    constructor Create(TokenKind: Word);
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

function Seq<L, R>(Left: TParser<L>; Right: TParser<R>): TParser<TPair<L, R>>;

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

implementation

(* TokenKind *)

constructor TTokenKindParser.Create(TokenKind: Word);
begin
  FTokenKind := TokenKind;
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
  if Token.Kind = FTokenKind then
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

function Seq<L, R>(Left: TParser<L>; Right: TParser<R>): TParser<TPair<L, R>>;
begin
  Result := TAndParser<L, R>.Create(Left, Right);
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

end.
