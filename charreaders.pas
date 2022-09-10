unit CharReaders;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type
  TOptChar = record
    HasValue: Boolean;
    Value: Char;
  end;

  TReader = class
  public
    function Read: TOptChar; virtual; abstract;
  end;

  TStreamReader = class(TReader)
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read: TOptChar; override;
  end;

  TPushBackReader = class(TReader)
  private
    FReader: TReader;
    FBuffer: TOptChar;
  public
    constructor Create(Reader: TReader);
    destructor Destroy; override;
    function Read: TOptChar; override;
    procedure UnRead(Ch: Char);
  end;

implementation

constructor TStreamReader.Create(Stream: TStream);
begin
  FStream := Stream;
end;

destructor TStreamReader.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TStreamReader.Read: TOptChar;
var
  Buffer: array [0..1] of Char;
  BytesRead: Integer;
begin
  BytesRead := FStream.Read(Buffer, 1);
  if BytesRead = 1 then
  begin
    Result.HasValue := True;
    Result.Value := Buffer[0];
  end
  else
    Result.HasValue := False;
end;

constructor TPushBackReader.Create(Reader: TReader);
begin
  FReader := Reader;
  FBuffer.HasValue := False;
end;

destructor TPushBackReader.Destroy;
begin
  FReader.Free;
  inherited Destroy;
end;

function TPushBackReader.Read: TOptChar;
begin
  if FBuffer.HasValue then
  begin
    Result := FBuffer;
    FBuffer.HasValue := False;
  end
  else
    Result := FReader.Read;
end;

procedure TPushBackReader.UnRead(Ch: Char);
begin
  if FBuffer.HasValue then
    raise Exception.Create('Buffer overflow')
  else
  begin
    FBuffer.HasValue := True;
    FBuffer.Value := Ch;
  end;
end;

end.
