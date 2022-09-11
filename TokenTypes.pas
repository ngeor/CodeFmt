unit TokenTypes;

{$mode delphi}

interface

type
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

implementation

end.
