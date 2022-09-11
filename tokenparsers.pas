unit TokenParsers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Parsers, ParseResult, BinaryParsers, Tokenizers;

function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TToken>; Right: TParser<TTokenLinkedList>): TParser<TTokenLinkedList>; overload;
function Seq(Left: TParser<TTokenLinkedList>; Right: TParser<TToken>): TParser<TTokenLinkedList>; overload;

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
