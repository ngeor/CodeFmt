unit TokenTypes;

{$mode delphi}

interface

type
  TTokenType = (
    ttAssembler,
    ttComment,
    ttCRLF,
    ttDirective,
    ttIdentifier,
    ttKeyWord,
    ttNumber,
    ttPreProcessor,
    ttSpace,
    ttString,
    ttSymbol,
    ttUnknown
  );

implementation

end.
