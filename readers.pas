unit Readers;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type
  TOptional<T> = record
    HasValue: Boolean;
    Value: T;
  end;

  TOptChar = TOptional<Char>;

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

  TLineEnding = (leCR, leLF, leCRLF);

  TLineRead = record
    case HasValue: Boolean of
      False: ();
      True: (
        case IsEol: Boolean of
          False: (Value: Char);
          True: (Kind: TLineEnding));
  end;

  TLineReads = array of TLineRead;

  TCharPredicate = function(Ch: Char): Boolean;

  TLineReader = class
  private
    FPushBackReader: TPushBackReader;
  public
    constructor Create(PushBackReader: TPushBackReader);
    destructor Destroy; override;
    function Read: TLineRead;
  end;

  type
    TPushBackLineReader = class
    private
      FLineReader: TLineReader;
      FPrevious: TLineRead;
    public
      constructor Create(LineReader: TLineReader);
      destructor Destroy; override;
      function Read: TLineRead;
      procedure UnRead(LineRead: TLineRead);
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

constructor TLineReader.Create(PushBackReader: TPushBackReader);
begin
  FPushBackReader := PushBackReader;
end;

destructor TLineReader.Destroy;
begin
  FPushBackReader.Free;
  inherited Destroy;
end;

function TLineReader.Read: TLineRead;
var
  First, Second: TOptChar;
begin
  First := FPushBackReader.Read;
  if First.HasValue then
  begin
    Result.HasValue := true;
    if First.Value = '\r' then
    begin
      Result.IsEol := True;
      Second := FPushBackReader.Read;
      if Second.HasValue then
      begin
        if Second.Value = '\n' then
        begin
          Result.Kind := leCRLF;
        end
        else
        begin
          FPushBackReader.UnRead(Second.Value);
          Result.Kind := leCR;
        end
      end
      else
      begin
        Result.IsEol := True;
        Result.Kind := leCR;
      end
    end
    else if First.Value = '\n' then
    begin
      Result.IsEol := True;
      Result.Kind := leLF;
    end
    else
    begin
      Result.IsEol := False;
      Result.Value := First.Value;
    end
  end
  else
    Result.HasValue := False
end;

constructor TPushBackLineReader.Create(LineReader: TLineReader);
begin
  FLineReader := LineReader;
  FPrevious.HasValue := False;
end;

destructor TPushBackLineReader.Destroy;
begin
  FLineReader.Free;
  inherited Destroy;
end;

function TPushBackLineReader.Read: TLineRead;
begin
  if FPrevious.HasValue then
  begin
    Result := FPrevious;
    FPrevious.HasValue := False;
  end
  else
    Result := FLineReader.Read;
end;

procedure TPushBackLineReader.UnRead(LineRead: TLineRead);
begin
  if FPrevious.HasValue then
    raise Exception.Create('Buffer overflow')
  else if not LineRead.HasValue then
    raise Exception.Create('Invalid argument')
  else
    FPrevious := LineRead;
end;

end.