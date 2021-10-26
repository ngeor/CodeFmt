
Unit TokenTypes;

{$mode delphi}

Interface

Type 
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

Implementation

End.
