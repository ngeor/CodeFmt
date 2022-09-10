unit CharOrNewLineReaders;

{$mode Delphi}

interface

uses
  Classes, SysUtils, CharReaders;

type
  TNewLineKind = (nlCR, nlLF, nlCRLF);

  TCharOrNewLineKind = (ckEof, ckEol, ckChar);

  TCharOrNewLine = record
    case Kind : TCharOrNewLineKind of
      ckEof: ();
      ckEol: (NewLineKind: TNewLineKind);
      ckChar: (Ch: Char);
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
    if First.Value = '\r' then
    begin
      Result.Kind := ckEol;
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
        Result.NewLineKind := nlCR;
      end
    end
    else if First.Value = '\n' then
    begin
      Result.Kind := ckEol;
      Result.NewLineKind := nlLF;
    end
    else
    begin
      Result.Kind := ckChar;
      Result.Ch := First.Value;
    end
  end
  else
    Result.Kind := ckEof;
end;

constructor TPushBackCharOrNewLineReader.Create(LineReader: TCharOrNewLineReader);
begin
  FLineReader := LineReader;
  FPrevious.Kind := ckEof;
end;

destructor TPushBackCharOrNewLineReader.Destroy;
begin
  FLineReader.Free;
  inherited Destroy;
end;

function TPushBackCharOrNewLineReader.Read: TCharOrNewLine;
begin
  if FPrevious.Kind <> ckEof then
  begin
    Result := FPrevious;
    FPrevious.Kind := ckEof;
  end
  else
    Result := FLineReader.Read;
end;

procedure TPushBackCharOrNewLineReader.UnRead(LineRead: TCharOrNewLine);
begin
  if FPrevious.Kind <> ckEof then
    raise Exception.Create('Buffer overflow')
  else if LineRead.Kind = ckEof then
    raise Exception.Create('Invalid argument')
  else
    FPrevious := LineRead;
end;

end.
