unit TokenTypes;

{$mode delphi}

interface

type
  { A least-common-denominator token type that is understood by the RTF/HTML formatters }
  THigherTokenType = (
    htAssembler,
    htComment,
    htCRLF,
    htDirective,
    htIdentifier,
    htKeyWord,
    htNumber,
    htPreProcessor,
    htSpace,
    htString,
    htSymbol,
    htUnknown
  );

  TFmt = record
    Text: String;
    Kind: THigherTokenType;
  end;

function CreateFmt(Text: String; Kind: THigherTokenType): TFmt;

implementation

function CreateFmt(Text: String; Kind: THigherTokenType): TFmt;
begin
  Result.Text := Text;
  Result.Kind := Kind;
end;

end.
