unit TokenParsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Types, Parsers, ParseResult, BinaryParsers, Tokenizers;

type
  TAnyTokenParser = class(TParser<TToken>)
  public
    function Parse(Source: TUndoTokenizer): TParseResult<TToken>; override;
    procedure Undo(Source: TUndoTokenizer; Data: TToken); override;
  end;

type
  TTokenTypeFilterParser = class(TFilterParser<TToken>)
  private
    FTokenTypes: TByteDynArray;
  protected
    function Filter(Data: TToken): Boolean; override;
  public
    constructor Create(Parser: TParser<TToken>; TokenType: Byte); overload;
    constructor Create(Parser: TParser<TToken>; TokenTypes: TByteDynArray); overload;
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

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;

function FilterTokenType(TokenType: Byte): TParser<TToken>; overload;
function FilterTokenTypes(TokenTypes: TByteDynArray): TParser<TToken>; overload;

function MapTokenToList(Parser: TParser<TToken>): TParser<TTokenLinkedList>; overload;

function ManyTokens(Parser: TParser<TToken>): TParser<TTokenLinkedList>; overload;

implementation

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
begin
  Result := TAndTokenListsParser.Create(Left, Right);
end;

function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
begin
  Result := Seq(MapTokenToList(Left), Right);
end;

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;
begin
  Result := Seq(Left, MapTokenToList(Right));
end;

function FilterTokenType(TokenType: Byte): TParser<TToken>; overload;
begin
  Result := TTokenTypeFilterParser.Create(TAnyTokenParser.Create, TokenType);
end;

function FilterTokenTypes(TokenTypes: TByteDynArray): TParser<TToken>; overload;
begin
  Result := TTokenTypeFilterParser.Create(TAnyTokenParser.Create, TokenTypes);
end;

function MapTokenToList(Parser: TParser<TToken>): TParser<TTokenLinkedList>; overload;
begin
  Result := TMapTokenToTokenListParser.Create(Parser);
end;

function ManyTokens(Parser: TParser<TToken>): TParser<TTokenLinkedList>; overload;
begin
  Result := TManyTokensParser.Create(Parser);
end;

(* AnyToken *)

function TAnyTokenParser.Parse(Source: TUndoTokenizer): TParseResult<TToken>;
var
  Next: TToken;
begin
  Next := Source.Read;
  if Next.Kind >= 0 then
    Result := SuccessParseResult<TToken>(Next)
  else
    Result := FailedParseResult<TToken>()
end;

procedure TAnyTokenParser.Undo(Source: TUndoTokenizer; Data: TToken);
begin
  Source.Undo(Data);
end;

(* TokenType Filter *)

constructor TTokenTypeFilterParser.Create(Parser: TParser<TToken>; TokenType: Byte);
begin
  inherited Create(Parser);
  FTokenTypes := [TokenType];
end;

constructor TTokenTypeFilterParser.Create(Parser: TParser<TToken>; TokenTypes: TByteDynArray);
begin
  inherited Create(Parser);
  FTokenTypes := TokenTypes;
end;

function TTokenTypeFilterParser.Filter(Data: TToken): Boolean;
var
  i: Integer;
begin
  i := Low(FTokenTypes);
  while (i <= High(FTokenTypes)) and (Data.Kind <> FTokenTypes[i]) do
    Inc(i);
  Result := i <= High(FTokenTypes);
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
