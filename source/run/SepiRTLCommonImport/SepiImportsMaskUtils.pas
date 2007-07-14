{*
  Importe l'unité MaskUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMaskUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, MaskUtils;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'MaskUtils', []);

  // Constants
  TSepiConstant.Create(Result, 'mDirReverse', mDirReverse);
  TSepiConstant.Create(Result, 'mDirUpperCase', mDirUpperCase);
  TSepiConstant.Create(Result, 'mDirLowerCase', mDirLowerCase);
  TSepiConstant.Create(Result, 'mDirLiteral', mDirLiteral);
  TSepiConstant.Create(Result, 'mMskAlpha', mMskAlpha);
  TSepiConstant.Create(Result, 'mMskAlphaOpt', mMskAlphaOpt);
  TSepiConstant.Create(Result, 'mMskAlphaNum', mMskAlphaNum);
  TSepiConstant.Create(Result, 'mMskAlphaNumOpt', mMskAlphaNumOpt);
  TSepiConstant.Create(Result, 'mMskAscii', mMskAscii);
  TSepiConstant.Create(Result, 'mMskAsciiOpt', mMskAsciiOpt);
  TSepiConstant.Create(Result, 'mMskNumeric', mMskNumeric);
  TSepiConstant.Create(Result, 'mMskNumericOpt', mMskNumericOpt);
  TSepiConstant.Create(Result, 'mMskNumSymOpt', mMskNumSymOpt);
  TSepiConstant.Create(Result, 'mMskTimeSeparator', mMskTimeSeparator);
  TSepiConstant.Create(Result, 'mMskDateSeparator', mMskDateSeparator);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMaskCharType));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMaskDirectives));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TMaskedText));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TEditMask));

  // Routines
  TSepiMetaMethod.Create(Result, 'FormatMaskText', @FormatMaskText,
    'function(const EditMask: string; const Value: string): string');
  TSepiMetaMethod.Create(Result, 'MaskGetMaskSave', @MaskGetMaskSave,
    'function(const EditMask: string): Boolean');
  TSepiMetaMethod.Create(Result, 'MaskGetMaskBlank', @MaskGetMaskBlank,
    'function(const EditMask: string): Char');
  TSepiMetaMethod.Create(Result, 'MaskGetFldSeparator', @MaskGetFldSeparator,
    'function(const EditMask: string): Integer');
  TSepiMetaMethod.Create(Result, 'PadInputLiterals', @PadInputLiterals,
    'function(const EditMask: String; const Value: string; Blank: Char): string');
  TSepiMetaMethod.Create(Result, 'MaskOffsetToOffset', @MaskOffsetToOffset,
    'function(const EditMask: String; MaskOffset: Integer): Integer');
  TSepiMetaMethod.Create(Result, 'MaskOffsetToWideOffset', @MaskOffsetToWideOffset,
    'function(const EditMask: String; MaskOffset: Integer): Integer');
  TSepiMetaMethod.Create(Result, 'IsLiteralChar', @IsLiteralChar,
    'function(const EditMask: string; Offset: Integer): Boolean');
  TSepiMetaMethod.Create(Result, 'MaskGetCharType', @MaskGetCharType,
    'function(const EditMask: string; MaskOffset: Integer): TMaskCharType');
  TSepiMetaMethod.Create(Result, 'MaskGetCurrentDirectives', @MaskGetCurrentDirectives,
    'function(const EditMask: string; MaskOffset: Integer): TMaskDirectives');
  TSepiMetaMethod.Create(Result, 'MaskIntlLiteralToChar', @MaskIntlLiteralToChar,
    'function(IChar: Char): Char');
  TSepiMetaMethod.Create(Result, 'OffsetToMaskOffset', @OffsetToMaskOffset,
    'function(const EditMask: string; Offset: Integer): Integer');
  TSepiMetaMethod.Create(Result, 'MaskDoFormatText', @MaskDoFormatText,
    'function(const EditMask: string; const Value: string; Blank: Char): string');

  // Global variables
  TSepiVariable.Create(Result, 'DefaultBlank',
     DefaultBlank, TypeInfo(Char));
  TSepiVariable.Create(Result, 'MaskFieldSeparator',
     MaskFieldSeparator, TypeInfo(Char));
  TSepiVariable.Create(Result, 'MaskNoSave',
     MaskNoSave, TypeInfo(Char));

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('MaskUtils', ImportUnit);
end.

