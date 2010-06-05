Around line 61 in Delphi 2009
----------
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  // CG: Define old enum for compression level
  TCompressionLevel = (clNone = Integer(zcNone), clFastest, clDefault, clMax);
----------
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);
  TCompressionLevel = type Byte; const
  clNone = TCompressionLevel(zcNone); clFastest = TCompressionLevel(zcFastest);
  clDefault = TCompressionLevel(zcDefault); clMax = TCompressionLevel(zcMax); type
----------

