unit BinaryParsers;

{$mode Delphi}

interface

uses
  Parsers, ParseResult, Tokenizers;

type
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

type
  (* Or *)
  TOrParser<T> = class(TBinaryParser<T, T, T>)
  public
    function Parse(Source: TUndoTokenizer): TParseResult<T>; override;
    procedure Undo(Source: TUndoTokenizer; Data: T); override;
  end;

implementation

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
      Result := SuccessParseResult<T>(Combine(Left.Data, Right.Data))
    else
    begin
      FLeft.Undo(Source, Left.Data);
      Result := FailedParseResult<T>();
    end
  end
  else
    Result := FailedParseResult<T>()
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

end.
