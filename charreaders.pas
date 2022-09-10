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

  TCharReader = class
  public
    function Read: TOptChar; virtual; abstract;
  end;

  TStreamCharReader = class(TCharReader)
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read: TOptChar; override;
  end;

  TPushBackCharReader = class(TCharReader)
  private
    FReader: TCharReader;
    FBuffer: TOptChar;
  public
    constructor Create(Reader: TCharReader);
    destructor Destroy; override;
    function Read: TOptChar; override;
    procedure UnRead(Ch: Char);
  end;

function CreatePushBackCharReader(Stream: TStream): TPushBackCharReader;

implementation

constructor TStreamCharReader.Create(Stream: TStream);
begin
  FStream := Stream;
end;

destructor TStreamCharReader.Destroy;
begin
  // FStream.Free;
  inherited Destroy;
end;

function TStreamCharReader.Read: TOptChar;
var
  Buffer: array [0..1] of Char;
  BytesRead: Integer;
begin
  // TODO: use buffering instead of reading just one character
  BytesRead := FStream.Read(Buffer, 1);
  if BytesRead = 1 then
  begin
    Result.HasValue := True;
    Result.Value := Buffer[0];
  end
  else
    Result.HasValue := False;
end;

constructor TPushBackCharReader.Create(Reader: TCharReader);
begin
  FReader := Reader;
  FBuffer.HasValue := False;
end;

destructor TPushBackCharReader.Destroy;
begin
  FReader.Free;
  inherited Destroy;
end;

function TPushBackCharReader.Read: TOptChar;
begin
  if FBuffer.HasValue then
  begin
    Result := FBuffer;
    FBuffer.HasValue := False;
  end
  else
    Result := FReader.Read;
end;

procedure TPushBackCharReader.UnRead(Ch: Char);
begin
  if FBuffer.HasValue then
    raise Exception.Create('Buffer overflow')
  else
  begin
    FBuffer.HasValue := True;
    FBuffer.Value := Ch;
  end;
end;

function CreatePushBackCharReader(Stream: TStream): TPushBackCharReader;
begin
  Result := TPushBackCharReader.Create(TStreamCharReader.Create(Stream));
end;

end.
