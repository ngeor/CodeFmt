unit CharOrNewLineReaders;

{$mode Delphi}

interface

uses
  Classes, SysUtils, CharReaders;

type
  TNewLineKind = (nlCR, nlLF, nlCRLF);

  TCharOrNewLine = record
    case HasValue: Boolean of
      False: ();
      True: (
        case IsEol: Boolean of
          False: (Value: Char);
          True: (NewLineKind: TNewLineKind));
  end;

  TCharOrNewLineArray = array of TCharOrNewLine;

  TCharOrNewLineReader = class
  private
    FPushBackReader: TPushBackCharReader;
  public
    constructor Create(PushBackReader: TPushBackCharReader);
    destructor Destroy; override;
    function Read: TCharOrNewLine;
  end;

  type
    TPushBackCharOrNewLineReader = class
    private
      FLineReader: TCharOrNewLineReader;
      FPrevious: TCharOrNewLine;
    public
      constructor Create(LineReader: TCharOrNewLineReader);
      destructor Destroy; override;
      function Read: TCharOrNewLine;
      procedure UnRead(LineRead: TCharOrNewLine);
    end;

implementation

constructor TCharOrNewLineReader.Create(PushBackReader: TPushBackCharReader);
begin
  FPushBackReader := PushBackReader;
end;

destructor TCharOrNewLineReader.Destroy;
begin
  FPushBackReader.Free;
  inherited Destroy;
end;

function TCharOrNewLineReader.Read: TCharOrNewLine;
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
          Result.NewLineKind := nlCRLF;
        end
        else
        begin
          FPushBackReader.UnRead(Second.Value);
          Result.NewLineKind := nlCR;
        end
      end
      else
      begin
        Result.IsEol := True;
        Result.NewLineKind := nlCR;
      end
    end
    else if First.Value = '\n' then
    begin
      Result.IsEol := True;
      Result.NewLineKind := nlLF;
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

constructor TPushBackCharOrNewLineReader.Create(LineReader: TCharOrNewLineReader);
begin
  FLineReader := LineReader;
  FPrevious.HasValue := False;
end;

destructor TPushBackCharOrNewLineReader.Destroy;
begin
  FLineReader.Free;
  inherited Destroy;
end;

function TPushBackCharOrNewLineReader.Read: TCharOrNewLine;
begin
  if FPrevious.HasValue then
  begin
    Result := FPrevious;
    FPrevious.HasValue := False;
  end
  else
    Result := FLineReader.Read;
end;

procedure TPushBackCharOrNewLineReader.UnRead(LineRead: TCharOrNewLine);
begin
  if FPrevious.HasValue then
    raise Exception.Create('Buffer overflow')
  else if not LineRead.HasValue then
    raise Exception.Create('Invalid argument')
  else
    FPrevious := LineRead;
end;

end.
