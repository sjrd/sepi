
// FPC
{$IFDEF FPC}
 {$MODE delphi}
 {$ASMMODE INTEL}
 {$DEFINE HasInline}          // Compiler can Inline functions

// Not FPC (Delphi...)
{$ELSE}
  {$IFDEF WIN32}
   {$DEFINE MSWINDOWS}
  {$ENDIF}

  // TODO : Check 64/32 platform ?
  {$IF not Declared(PtrUInt)}
  type
    PtrUInt = Cardinal;
  {$IFEND}
  {$IF not Declared(PtrInt)}
  type
    PtrInt = Integer;
    PPtrInt = ^PtrInt;
  {$IFEND}

  {$IFDEF MSWINDOWS}

    //# VER220 - Delphi XE
    //# VER210 - Delphi 2010
    //# VER200 - Delphi 2009
    //# VER185 - Delphi 2007
    //# VER180 - Delphi 2007
    //# VER180 - Delphi 2006
    //# VER170 - Delphi 2005
    //# VER160 - Delphi 8
    //# VER150 - Delphi 7
    //# VER140 - Delphi 6
    //# VER130 - Delphi 5
    //# VER120 - Delphi 4
    //# VER100 - Delphi 3
    //# VER90  - Delphi 2
    //# VER80  - Delphi 1

    {$IFDEF VER220}
      {$DEFINE DelphiXE}
      {$DEFINE Delphi2010}
      {$DEFINE Delphi2009}
      {.$DEFINE HasInline}         // Compiler can Inline functions
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF VER220}

    {$IFDEF VER210}
      {$DEFINE Delphi2010}
      {$DEFINE Delphi2009}
      {.$DEFINE HasInline}         // Compiler can Inline functions
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF VER210}

    {$IFDEF VER200}
      {$DEFINE Delphi2009}
      {.$DEFINE HasInline}         // Compiler can Inline functions
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF VER200}

    {$IFDEF VER185}
      {$DEFINE Delphi2007}
      {$DEFINE Delphi2006}
      {.$DEFINE HasInline}         // Compiler can Inline functions
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF VER185}

    {$IFDEF VER180}
      {$DEFINE Delphi2007}
      {$DEFINE Delphi2006}
      {.$DEFINE HasInline}         // Compiler can Inline functions
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF ver180}

    {$IFDEF VER170}
      {$DEFINE Delphi8}
      {$DEFINE Delphi7}
      {$DEFINE Delphi6}
      {.$DEFINE HasInline}         // Compiler can Inline functions
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF ver170}

    {$IFDEF VER160}
      {$DEFINE Delphi8}
      {$DEFINE Delphi7}
      {$DEFINE Delphi6}
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF}

    {$IFDEF VER150}
      {$DEFINE Delphi7}
      {$DEFINE Delphi6}
      {.$DEFINE HasInt64Variant}   // Support for Int64 as Variant
    {$ENDIF}

    {$IFDEF VER140}
      {$DEFINE Delphi6}
      {.$DEFINE HasInt64Variant}  // Support for Int64 as Variant
    {$ENDIF}

    {$IFDEF VER130}
      {$DEFINE Delphi5}
      {$DEFINE Delphi4}
    {$ENDIF}

    {$IFDEF VER120}
      {$DEFINE Delphi4}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


// TODO - CHECK COMPILER DIRECTIVES

// GENERAL COMPILER DIRECTIVES
{$H+}   {+ Use ANSI LongStrings }
{$T+}   {+ Typed @ operator }

// DEBUGGING VERSION
{$IFDEF DEBUG_SEPI}
 {$Q+}  {+ Overflow-Checking }
 {$R+}  {+ Range-Checking }
 {$O-}  {- Optimisation }
 {$D+}  {+ Debug info }

// RELEASE VERSION
{$ELSE}
 {.$B-} {- Complete Boolean Evaluation }
 {$Q-}  {- Overflow-Checking }
 {$R-}  {- Range-Checking }
 {.$V-} {- Var-String Checking }
 {.$T-} {- Typed @ operator }
 {$X+}  {+ Extended syntax }
 {$P+}  {+ Open string params }
 {$O+}  {+ Optimisation }
{$ENDIF}

{$IFDEF MSWINDOWS}
 {.$J+} {- Writeable structured consts }
{$ENDIF}
