{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  Sébastien Doeraene
All Rights Reserved

This file is part of Sepi.

Sepi is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Sepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Sepi.  If not, see <http://www.gnu.org/licenses/>.

Linking this library statically or dynamically with other modules is making a
combined work based on this library.  Thus, the terms and conditions of the GNU
General Public License cover the whole combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules, and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms and
conditions of the license of that module.  An independent module is a module
which is not derived from or based on this library.  If you modify this
library, you may extend this exception to your version of the library, but you
are not obligated to do so.  If you do not wish to do so, delete this exception
statement from your version.
-------------------------------------------------------------------------------}

{*
  Types et constantes définissant les OpCodes de Sepi
  @author sjrd
  @version 1.0
*}
unit SepiOpCodes;

interface

uses
  SysUtils, SepiReflectionCore, SepiMembers;

resourcestring
  SInvalidOpCode = 'OpCode invalide';

type
  {*
    Déclenchée si l'interpréteur rencontre un OpCode invalide
    @author sjrd
    @version 1.0
  *}
  ESepiInvalidOpCode = class(Exception);

  /// Type d'un OpCode
  TSepiOpCode = type Byte;

  {*
    Type d'adressage
    L'octet est décomposé en deux partie : $HL. Les 4 bits de poids faible (L)
    sont une valeur de type TSepiMemorySpace. Les 4 bits de poids fort (H)
    indiquent le nombre d'opérations de type Address à appliquer successivement
    sur l'adresse.
  *}
  TSepiMemoryRef = type Byte;

  {*
    Espace d'adressage
    Les deux valeurs msConstant et msTrueConst représentent, comme leur nom
    l'indique, des constantes. Certaines opérations n'acceptent pas les
    constantes en paramètre, par exemple la destination d'un MOV ne peut pas
    être une constante. De même, msVariable indique une TSepiVariable, qui peut
    également être une constante (adressée) : la plupart des opérations
    n'acceptant pas les constantes n'acceptent pas non plus une TSepiVariable
    constante.
    La valeur msZero est parfois acceptée par des instructions n'acceptant pas
    de constantes, bien que 0 soit manifestement une constante. C'est le cas
    par exemple de l'affectation de chaîne, qui n'accepte que la constante
    nulle (qui vaut la chaîne vide '').
    La valeur msZero en tant que valeur de retour d'un CALL est interprétée
    plutôt en tant que msNoResult, autrement dit cela demande que le résultat
    ne soit pas stocké. C'est également le cas de l'argument ExceptObject d'un
    TRYE (try-except).
    - msZero : 0, nil, '', etc. selon le type de variable
    - msConstant : la variable est une constante, stockée juste derrière
    - msLocalsBase : variable locale, sans offset
    - msLocalsByte : variable locale, offset d'un octet
    - msLocalsWord : variable locale, offset de deux octets
    - msParamsBase : paramètre, sans offset
    - msParamsByte : paramètre, offset d'un octet
    - msParamsWord : paramètre, offset de deux octets
    - msTrueConst : référence à une TSepiConstant
    - msVariable : référence à une TSepiVariable
  *}
  TSepiMemorySpace = (
    msZero, msConstant, msLocalsBase, msLocalsByte, msLocalsWord, msParamsBase,
    msParamsByte, msParamsWord, msTrueConst, msVariable
  );

{$IF Byte(High(TSepiMemorySpace)) >= 16}
  {$MESSAGE ERROR 'TSepiMemorySpace mustn''t have more than 16 values'}
{$IFEND}

const
  msNil = msZero;          /// Constante nil
  msNoResult = msZero;     /// Ne pas garder le résultat
  msResult = msLocalsBase; /// Variable résultat
  msSelf = msParamsBase;   /// Paramètre Self

type
  {*
    Combinaison d'un déréférencement et d'une opération sur une adresse
    Utilisez les routines AddressDerefAndOpEncode et AddressDerefAndOpDecode
    pour travailler sur des valeurs de type TSepiAddressDerefAndOp.
    S'il est présent, le déréférencement est toujours appliqué avant
    l'opération. Cela correspond à l'accès indicé d'un tableau (ou chaîne de
    caractères).
  *}
  TSepiAddressDerefAndOp = type Byte;

  {*
    Type d'opération à appliquer à une adresse
    Les "+ const" sont utiles pour les offsets de champs, comme dans les record
    ou les objets, ou encore dans les VMT.
    Les "+ mem" sont utiles pour les index de tableaux de bytes, ou pour les
    index de caractère dans les chaînes.
    Les "+ const*mem" sont utiles pour les index de tableaux dont les éléments
    font plus d'un octet.
    - aoNone : aucune opération
    - aoPlusConstShortint : ajoute un Shortint constant
    - aoPlusConstSmallint : ajoute un Smallint constant
    - aoPlusConstLongint : ajoute un Longint constant
    - aoPlusMemShortint : ajoute un Shortint mémoire
    - aoPlusMemSmallint : ajoute un Smallint mémoire
    - aoPlusMemLongint : ajoute un Longint mémoire
    - aoPlusConstTimesMemShortint : ajoute un Byte constant multiplié par un
      Shortint mémoire
    - aoPlusConstTimesMemSmallint : ajoute un Byte constant multiplié par un
      Smallint mémoire
    - aoPlusConstTimesMemLongint : ajoute un Byte constant multiplié par un
      Longint mémoire
  *}
  TSepiAddressOperation = (
    aoNone, aoPlusConstShortint, aoPlusConstSmallint, aoPlusConstLongint,
    aoPlusMemShortint, aoPlusMemSmallint, aoPlusMemLongint,
    aoPlusConstTimesMemShortint, aoPlusConstTimesMemSmallint,
    aoPlusConstTimesMemLongint, aoPlusConstTimesMemByte,
    aoPlusConstTimesMemWord, aoPlusConstTimesMemLongWord,
    aoPlusLongConstTimesMemShortint, aoPlusLongConstTimesMemSmallint,
    aoPlusLongConstTimesMemLongint, aoPlusLongConstTimesMemByte,
    aoPlusLongConstTimesMemWord, aoPlusLongConstTimesMemLongWord
  );

  {*
    Dérérencement d'adresse
    Les doubles déréférencements sont utilisés essentiellement lorsqu'on accède
    à un champ de la VMT d'un objet. Mais ils peuvent avoir d'autres
    utilisations.
    - adNone : pas de déréférencement
    - adSimple : déréférencement simple (Ptr^)
    - adDouble : déréférencement double (Ptr^^)
  *}
  TSepiAddressDereference = (adNone, adSimple, adDouble);

  {*
    Types de données de base que gère l'interpréteur Sepi
  *}
  TSepiBaseType = (
    btBoolean, btByte, btWord, btDWord, btShortint, btSmallint, btLongint,
    btInt64, btSingle, btDouble, btExtended, btComp, btCurrency, btAnsiChar,
    btWideChar, btAnsiStr, btWideStr, btUnicodeStr, btVariant
  );

  {*
    Ensemble de types de données de base
  *}
  TSepiBaseTypes = set of TSepiBaseType;

const
  btIntegers =
    [btByte, btWord, btDWord, btShortint, btSmallint, btLongint, btInt64];
  btFloats = [btSingle, btDouble, btExtended, btComp, btCurrency];
  btNumbers = btIntegers + btFloats;
  btChars = [btAnsiChar, btWideChar];
  btStrings = [btAnsiStr, btWideStr, btUnicodeStr];
  btCharsAndStrings = btChars + btStrings;

type
  /// Configuration d'un appel basique
  TSepiCallSettings = type Byte;

  /// Taille d'un paramètre à passer à une procédure
  TSepiParamSize = type Byte;

  {*
    Option de lecture d'adresse
    Si aoZeroAsNil est spécifié, aoAcceptZero n'a aucun effet.
    Si la taille des constantes est négative ou nulle, aoAcceptConstInCode n'a
    aucun effet.
    - aoZeroAsNil : mappe un msZero en nil au retour
    - aoAcceptZero : accepte msZero
    - aoAcceptTrueConst : accepte les vraies constantes
    - aoAcceptAddressedConst : accepte les constantes adressées
    - aoAcceptConstInCode : accepte les constantes dans le code
  *}
  TSepiAddressOption = (
    aoZeroAsNil, aoAcceptZero, aoAcceptTrueConst, aoAcceptAddressedConst,
    aoAcceptConstInCode
  );

  /// Options de lecture d'adresse
  TSepiAddressOptions = set of TSepiAddressOption;

const
  // Accepte tous les types de constantes
  aoAcceptAllConsts = [
    aoAcceptZero, aoAcceptTrueConst, aoAcceptAddressedConst,
    aoAcceptConstInCode
  ];

  /// Accepte tous les types de constantes hors code
  aoAcceptNonCodeConsts = [
    aoAcceptZero, aoAcceptTrueConst, aoAcceptAddressedConst
  ];

  /// Taille des constantes en fonction des types de base
  BaseTypeConstSizes: array[TSepiBaseType] of Integer = (
    1, 1, 2, 4, 1, 2, 4, 8, 4, 8, 10, 8, 8, 1, 2, 0, 0, 0, 0
  );

  /// Le paramètre est passé par adresse
  psByAddress = TSepiParamSize(0);

const
  MemPlaceMask = $0F;   /// Masque d'espace mémoire
  MemOpCountMask = $F0; /// Masque du nombre d'opérations
  MemOpCountShift = 4;  /// Offset du nombre d'opérations

  AddrOperationMask = $3F; /// Masque de l'opération sur adresse
  AddrDerefMask = $C0;     /// Masque de déréférencement
  AddrDerefShift = 6;      /// Offset du déréférencement

  SettingsCallingConvMask = $07;    /// Masque de la convention d'appel
  SettingsRegUsageMask = $18;       /// Masque de l'usage des registres
  SettingsRegUsageShift = 3;        /// Offset de l'usage des registres
  SettingsResultBehaviorMask = $E0; /// Masque du comportement du résultat
  SettingsResultBehaviorShift = 5;  /// Offset du comportement du résultat

const
  (*
    Mem := TSepiMemoryRef + Value [+ Operations]
    Value :=
      Zero        -> Nothing
      Constant    -> Constant of the relevant type
      LocalsBase  -> Local vars, no offset
      LocalsByte  -> Byte offset in local vars
      LocalsWord  -> Word offset in local vars
      ParamsBase  -> Parameters, no offset
      ParamsByte  -> Byte offset in parameters
      ParamsWord  -> Word offset in parameters
      GlobalConst -> Constant reference
      GlobalVar   -> Variable reference
    Params := Byte-Count + (Byte or Word)-Size + (Param){Count} + Result
      (Size is counted by 4 bytes ; it is Byte-wide if Count <= (255 div 3))
      Parameters must be ordered in growing SepiStackOffset.
    Param := Param-Size + Mem
    Result := Mem (zero as nil)
    Class := Mem(4) where a constant is a TSepiClass reference
    Type := TSepiBaseType
  *)

  // No category
  ocNope     = TSepiOpCode($00); /// NOP
  ocExtended = TSepiOpCode($01); /// Instruction étendue (non utilisé)

  // Flow control (destinations are relative to end of instruction)
  ocJump        = TSepiOpCode($02); /// JUMP Dest
  ocJumpIfTrue  = TSepiOpCode($03); /// JIT  Dest, Test
  ocJumpIfFalse = TSepiOpCode($04); /// JIF  Dest, Test

  // Calls
  ocAddressCall = TSepiOpCode($05); /// CALL CallSettings (ORS)? Address Params
  ocStaticCall  = TSepiOpCode($06); /// CALL Method-Ref Params
  ocDynamicCall = TSepiOpCode($07); /// CALL Method-Ref Self Params

  // Memory moves
  ocLoadAddress    = TSepiOpCode($10); /// LEA   Dest, Src
  ocMoveByte       = TSepiOpCode($11); /// MOVB  Dest, Src
  ocMoveWord       = TSepiOpCode($12); /// MOVW  Dest, Src
  ocMoveDWord      = TSepiOpCode($13); /// MOVD  Dest, Src
  ocMoveQWord      = TSepiOpCode($14); /// MOVQ  Dest, Src
  ocMoveExt        = TSepiOpCode($15); /// MOVE  Dest, Src
  ocMoveAnsiStr    = TSepiOpCode($16); /// MOVAS Dest, Src
  ocMoveWideStr    = TSepiOpCode($17); /// MOVWS Dest, Src
  ocMoveUnicodeStr = TSepiOpCode($18); /// MOVUS Dest, Src
  ocMoveVariant    = TSepiOpCode($19); /// MOVV  Dest, Src
  ocMoveIntf       = TSepiOpCode($1A); /// MOVI  Dest, Src
  ocMoveSome       = TSepiOpCode($1B); /// MOVS  Byte-Count, Dest, Src
  ocMoveMany       = TSepiOpCode($1C); /// MOVM  Word-Count, Dest, Src
  ocMoveOther      = TSepiOpCode($1D); /// MOVO  Type-Ref, Dest, Src
  ocConvert        = TSepiOpCode($1E); /// CVRT  Type, Type, [CP,] Mem, Mem

  // Self dest unary operations
  ocSelfInc = TSepiOpCode($20); /// INC Type, Var
  ocSelfDec = TSepiOpCode($21); /// DEC Type, Var
  ocSelfNot = TSepiOpCode($22); /// NOT Type, Var
  ocSelfNeg = TSepiOpCode($23); /// NEG Type, Var

  // Self dest binary operations
  ocSelfAdd      = TSepiOpCode($24); /// ADD Type, Var, Value
  ocSelfSubtract = TSepiOpCode($25); /// SUB Type, Var, Value
  ocSelfMultiply = TSepiOpCode($26); /// MUL Type, Var, Value
  ocSelfDivide   = TSepiOpCode($27); /// DIV Type, Var, Value
  ocSelfIntDiv   = TSepiOpCode($28); /// IDV Type, Var, Value
  ocSelfModulus  = TSepiOpCode($29); /// MOD Type, Var, Value
  ocSelfShl      = TSepiOpCode($2A); /// SHL Type, Var, Value
  ocSelfShr      = TSepiOpCode($2B); /// SHR Type, Var, Value
  ocSelfAnd      = TSepiOpCode($2C); /// AND Type, Var, Value
  ocSelfOr       = TSepiOpCode($2D); /// OR  Type, Var, Value
  ocSelfXor      = TSepiOpCode($2E); /// XOR Type, Var, Value

  // Other dest unary operations
  ocOtherInc = TSepiOpCode($30); /// INC Type, Dest, Value
  ocOtherDec = TSepiOpCode($31); /// DEC Type, Dest, Value
  ocOtherNot = TSepiOpCode($32); /// NOT Type, Dest, Value
  ocOtherNeg = TSepiOpCode($33); /// NEG Type, Dest, Value

  // Other dest binary operations
  ocOtherAdd      = TSepiOpCode($34); /// ADD Type, Dest, Left, Right
  ocOtherSubtract = TSepiOpCode($35); /// SUB Type, Dest, Left, Right
  ocOtherMultiply = TSepiOpCode($36); /// MUL Type, Dest, Left, Right
  ocOtherDivide   = TSepiOpCode($37); /// DIV Type, Dest, Left, Right
  ocOtherIntDiv   = TSepiOpCode($38); /// IDV Type, Dest, Left, Right
  ocOtherModulus  = TSepiOpCode($39); /// MOD Type, Dest, Left, Right
  ocOtherShl      = TSepiOpCode($3A); /// SHL Type, Dest, Left, Right
  ocOtherShr      = TSepiOpCode($3B); /// SHR Type, Dest, Left, Right
  ocOtherAnd      = TSepiOpCode($3C); /// AND Type, Dest, Left, Right
  ocOtherOr       = TSepiOpCode($3D); /// OR  Type, Dest, Left, Right
  ocOtherXor      = TSepiOpCode($3E); /// XOR Type, Dest, Left, Right

  // Comparisons
  ocCompEquals    = TSepiOpCode($40); /// EQ  Type, Dest, Left, Right
  ocCompNotEquals = TSepiOpCode($41); /// NEQ Type, Dest, Left, Right
  ocCompLower     = TSepiOpCode($42); /// LT  Type, Dest, Left, Right
  ocCompGreater   = TSepiOpCode($43); /// GT  Type, Dest, Left, Right
  ocCompLowerEq   = TSepiOpCode($44); /// LE  Type, Dest, Left, Right
  ocCompGreaterEq = TSepiOpCode($45); /// GE  Type, Dest, Left, Right

  // Compile time objects which must be read at runtime in Sepi
  ocGetTypeInfo    = TSepiOpCode($50); /// GTI Dest, Type-Ref
  ocGetDelphiClass = TSepiOpCode($51); /// GDC Dest, Class-Ref
  ocGetMethodCode  = TSepiOpCode($52); /// GMC Dest, Method-Ref

  // is and as operators
  ocIsClass = TSepiOpCode($53); /// IS Dest, Object, Class
  ocAsClass = TSepiOpCode($54); /// AS Object, Class

  // Exception handling
  ocRaise      = TSepiOpCode($60); /// RAISE ExceptObject
  ocReraise    = TSepiOpCode($61); /// RERS
  ocTryExcept  = TSepiOpCode($62); /// TRYE TrySize, ExceptSize, [ExceptObject]
  ocTryFinally = TSepiOpCode($63); /// TRYF TrySize, FinallySize
  /// ON ExceptObject, Byte-Count, (Class-Ref, Dest){Count}
  ocMultiOn    = TSepiOpCode($64);

  // Set operations
  ocSetInclude        = TSepiOpCode($70); /// SINC Dest, Elem
  ocSetExclude        = TSepiOpCode($71); /// SEXC Dest, Elem
  ocSetIn             = TSepiOpCode($72); /// SIN  Dest, Set, Elem
  ocSetElem           = TSepiOpCode($73); /// SELE SetSize, Dest, Elem
  ocSetRange          = TSepiOpCode($74); /// SRNG SetSize, Dest, Lo, Hi
  ocSetUnionRange     = TSepiOpCode($75); /// SUR  SetSize, Dest, Lo, Hi
  ocSetEquals         = TSepiOpCode($76); /// SEQ  SetSize, Dest, Set1, Set2
  ocSetNotEquals      = TSepiOpCode($77); /// SNE  SetSize, Dest, Set1, Set2
  ocSetContained      = TSepiOpCode($78); /// SLE  SetSize, Dest, Set1, Set2
  ocSetSelfIntersect  = TSepiOpCode($79); /// SINT SetSize, Dest, Src
  ocSetSelfUnion      = TSepiOpCode($7A); /// SADD SetSize, Dest, Src
  ocSetSelfSubtract   = TSepiOpCode($7B); /// SSUB SetSize, Dest, Src
  ocSetOtherIntersect = TSepiOpCode($7C); /// SINT SetSize, Dest, Left, Right
  ocSetOtherUnion     = TSepiOpCode($7D); /// SADD SetSize, Dest, Left, Right
  ocSetOtherSubtract  = TSepiOpCode($7E); /// SSUB SetSize, Dest, Left, Right
  ocSetExpand         = TSepiOpCode($7F); /// SEXP Dest, Src, Lo, Hi

  // Standard Delphi functions
  ocAnsiStrLength       = TSepiOpCode($80); /// ASL Dest, Value
  ocWideStrLength       = TSepiOpCode($81); /// WSL Dest, Value
  ocUnicodeStrLength    = TSepiOpCode($82); /// USL Dest, Value
  ocDynArrayLength      = TSepiOpCode($83); /// DAL Dest, Value
  ocDynArrayHigh        = TSepiOpCode($84); /// DAH Dest, Value
  ocAnsiStrSetLength    = TSepiOpCode($85); /// ASSL Var, Len
  ocWideStrSetLength    = TSepiOpCode($86); /// WSSL Var, Len
  ocUnicodeStrSetLength = TSepiOpCode($87); /// USSL Var, Len
  ocDynArraySetLength   = TSepiOpCode($88); /// DASL Type-Ref, Var,DimCount,Dims
  ocAnsiStrCopy         = TSepiOpCode($89); /// ASCP Dest, Src, Index, Count
  ocWideStrCopy         = TSepiOpCode($8A); /// WSCP Dest, Src, Index, Count
  ocUnicodeStrCopy      = TSepiOpCode($8B); /// USCP Dest, Src, Index, Count
  ocDynArrayCopy        = TSepiOpCode($8C); /// DACP Type-Ref, Dest, Src
  ocDynArrayCopyRange   = TSepiOpCode($8D); /// DACP Type-Ref, Dest, Src,Idx,Cnt

  // Routine reference instructions
  /// RRFMR MethodRefType, Dest, Source
  ocRoutineRefFromMethodRef = TSepiOpCode($A0);

function MemoryRefEncode(MemorySpace: TSepiMemorySpace;
  OpCount: Integer): TSepiMemoryRef;
procedure MemoryRefDecode(MemoryRef: TSepiMemoryRef;
  out MemorySpace: TSepiMemorySpace; out OpCount: Integer);

function AddressDerefAndOpEncode(Dereference: TSepiAddressDereference;
  Operation: TSepiAddressOperation): TSepiAddressDerefAndOp;
procedure AddressDerefAndOpDecode(AddressDerefAndOp: TSepiAddressDerefAndOp;
  out Dereference: TSepiAddressDereference;
  out Operation: TSepiAddressOperation);

function CallSettingsEncode(CallingConvention: TCallingConvention;
  RegUsage: Byte; ResultBehavior: TSepiTypeResultBehavior): TSepiCallSettings;
procedure CallSettingsDecode(Settings: TSepiCallSettings;
  out CallingConvention: TCallingConvention; out RegUsage: Byte;
  out ResultBehavior: TSepiTypeResultBehavior);

procedure RaiseInvalidOpCode;

implementation

{*
  Encode une référence mémoire
  @param Place     Espace mémoire
  @param OpCount   Nombre d'opérations
  @return Référence mémoire correspondante
*}
function MemoryRefEncode(MemorySpace: TSepiMemorySpace;
  OpCount: Integer): TSepiMemoryRef;
begin
  Result := Byte(MemorySpace) or (Byte(OpCount) shl MemOpCountShift);
end;

{*
  Décode une référence mémoire
  @param MemoryRef   Référence mémoire à décoder
  @param Place       En sortie : Espace mémoire
  @param OpCount     En sortie : Nombre d'opérations
*}
procedure MemoryRefDecode(MemoryRef: TSepiMemoryRef;
  out MemorySpace: TSepiMemorySpace; out OpCount: Integer);
begin
  MemorySpace := TSepiMemorySpace(MemoryRef and MemPlaceMask);
  OpCount := (MemoryRef and MemOpCountMask) shr MemOpCountShift;
end;

{*
  Encode un déréférencement + opération sur adresse
  @param Dereference   Déréférencement
  @param Operation     Opération
  @return Combinaison du déréférencement et de l'opération
*}
function AddressDerefAndOpEncode(Dereference: TSepiAddressDereference;
  Operation: TSepiAddressOperation): TSepiAddressDerefAndOp;
begin
  Result := Byte(Operation) or (Byte(Dereference) shl AddrDerefShift);
end;

{*
  Décode un déréférencement + opération sur adresse
  @param AddressDerefAndOp   Combinaison à décoder
  @param Dereference         En sortie : Déréférencement
  @param Operation           En sortie : Opération
*}
procedure AddressDerefAndOpDecode(AddressDerefAndOp: TSepiAddressDerefAndOp;
  out Dereference: TSepiAddressDereference;
  out Operation: TSepiAddressOperation);
begin
  Dereference := TSepiAddressDereference(
    (AddressDerefAndOp and AddrDerefMask) shr AddrDerefShift);
  Operation := TSepiAddressOperation(AddressDerefAndOp and AddrOperationMask);
end;

{*
  Encode une configuration d'appel
  @param CallingConvention   Convention d'appel
  @param RegUsage            Usage des registres
  @param ResultBehavior      Comportement du résultat
  @return Configuration d'appel correspondante
*}
function CallSettingsEncode(CallingConvention: TCallingConvention;
  RegUsage: Byte; ResultBehavior: TSepiTypeResultBehavior): TSepiCallSettings;
begin
  Result := Byte(CallingConvention) or
    (RegUsage shl SettingsRegUsageShift) or
    (Byte(ResultBehavior) shl SettingsResultBehaviorShift);
end;

{*
  Décode une configuration d'appel
  @param Settings            Configuration à décoder
  @param CallingConvention   En sortie : Convention d'appel
  @param RegUsage            En sortie : Usage des registres
  @param ResultBehavior      En sortie : Comportement du résultat
*}
procedure CallSettingsDecode(Settings: TSepiCallSettings;
  out CallingConvention: TCallingConvention; out RegUsage: Byte;
  out ResultBehavior: TSepiTypeResultBehavior);
begin
  CallingConvention := TCallingConvention(Settings and SettingsCallingConvMask);
  RegUsage := (Settings and SettingsRegUsageMask) shr SettingsRegUsageShift;
  ResultBehavior := TSepiTypeResultBehavior(
    (Settings and SettingsResultBehaviorMask) shr SettingsResultBehaviorShift);
end;

{*
  Déclenche une exception OpCode invalide
*}
procedure RaiseInvalidOpCode;

  function ReturnAddress: Pointer;
  asm
        MOV     EAX,[ESP+4]
  end;

begin
  raise ESepiInvalidOpCode.CreateRes(@SInvalidOpCode) at ReturnAddress;
end;

end.

