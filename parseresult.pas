unit ParseResult;

{$mode Delphi}

interface

type
  { The result of the parsing operation }
  TParseResult<T> = record
    Success: Boolean;
    Data: T;
  end;

function SuccessParseResult<T>(Data: T): TParseResult<T>;
function FailedParseResult<T>(): TParseResult<T>;

implementation

function SuccessParseResult<T>(Data: T): TParseResult<T>;
begin
  Result.Success := True;
  Result.Data := Data;
end;

function FailedParseResult<T>(): TParseResult<T>;
begin
  Result.Success := False;
end;

end.
