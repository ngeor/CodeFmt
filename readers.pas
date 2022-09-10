unit Readers;

{$mode Delphi}

interface

uses
  Classes, SysUtils, CharReaders;

type
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
