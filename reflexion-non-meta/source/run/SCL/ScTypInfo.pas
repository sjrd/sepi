{*
  �tend les informations et les routines propos�es par l'unit� TypInfo
  @author sjrd
  @version 1.0
*}
unit ScTypInfo;

interface

uses
  TypInfo;

const
  /// Types qui requi�rent une initialisation
  { tkArray and tkRecord are listed here, though static arrays and record don't
    always need initialization, because thes types have got RTTI if and only if
    the particular type needs initialization. }
  NeedInitTypeKinds = [
    tkLString, tkWString, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray
  ];

type
  /// Pointeur vers TRecordField
  PRecordField = ^TRecordField;

  {*
    Champ d'un record (qui requiert une initialisation)
    @author sjrd
    @version 1.0
  *}
  TRecordField = packed record
    TypeInfo : PPTypeInfo; /// RTTI du type du champ
    Offset : integer;      /// Offset du champ dans le record
  end;

  /// Pointeur vers TRecordTypeData
  PRecordTypeData = ^TRecordTypeData;

  {*
    Donn�es d'un type record
    @author sjrd
    @version 1.0
  *}
  TRecordTypeData = packed record
    Size : integer;                       /// Taille du type
    FieldCount : integer;                 /// Nombre de champs
    Fields : array[0..0] of TRecordField; /// Champs (0..FieldCount-1)
  end;

  /// Pointeur vers TArrayTypeData
  PArrayTypeData = ^TArrayTypeData;

  {*
    Donn�es d'un type tableau statique
    @author sjrd
    @version 1.0
  *}
  TArrayTypeData = packed record
    Size : integer;      /// Taille du type
    Count : integer;     /// Nombre d'�l�ments (lin�aris�s)
    ElType : PPTypeInfo; /// RTTI du type des �l�ments
  end;

function TypeSize(TypeInfo : PTypeInfo) : integer;

procedure CopyData(const Source; var Dest; Size : integer;
  TypeInfo : PTypeInfo); overload;
procedure CopyData(const Source; var Dest; TypeInfo : PTypeInfo); overload;

implementation

uses
  ScCompilerMagic;

{*
  D�termine la taille d'un type � partir de ses RTTI
  Le seul cas dans lequel TypeSize est incapable de d�terminer la taille du
  type (et donc renvoie -1), est si TypeInfo.Kind = tkUnknown, ce qui ne
  devrait jamais arriver.
  @param TypeInfo   RTTI du type
  @return Taille du type, ou -1 si c'est impossible � d�terminer
*}
function TypeSize(TypeInfo : PTypeInfo) : integer;
const
  TypeKindToSize : array[TTypeKind] of integer = (
    -1, 0, 1, 0, 0, 0, 0, 4, 8, 2, 4, 4, 16, 0, 0, 4, 8, 4
  );
  OrdTypeToSize : array[TOrdType] of integer = (1, 1, 2, 2, 4, 4);
  FloatTypeToSize : array[TFloatType] of integer = (4, 8, 10, 8, 8);
var TypeData : PTypeData;
begin
  Result := TypeKindToSize[TypeInfo.Kind];

  if Result = 0 then
  begin
    TypeData := GetTypeData(TypeInfo);
    case TypeInfo.Kind of
      tkInteger,
      tkEnumeration : Result := OrdTypeToSize[TypeData.OrdType];
      tkFloat       : Result := FloatTypeToSize[TypeData.FloatType];
      tkString      : Result := TypeData.MaxLength+1;
      tkArray       : Result := PArrayTypeData (TypeData).Size;
      tkRecord      : Result := PRecordTypeData(TypeData).Size;

      { Though tkSet has also the OrdType field, it isn't always reliable,
        since it can be out of range for large sets. }
      tkSet :
      begin
        with GetTypeData(TypeData.CompType^)^ do
          Result := (MaxValue - MinValue) div 8 + 1;
        if Result = 3 then Result := 4;
      end;
    end;
  end;
end;

{*
  Copie une variable
  Le param�tre TypeInfo n'est requis que si le type de donn�es requiert une
  initialisation. Mais ce n'est pas une erreur de le renseigner m�me dans les
  autres cas.
  @param Source     Variable source
  @param Dest       Variable destination
  @param Size       Taille du type de la variable
  @param TypeInfo   RTTI du type de la variable (si requiert une initialisation)
*}
procedure CopyData(const Source; var Dest; Size : integer;
  TypeInfo : PTypeInfo);
begin
  if Assigned(TypeInfo) and (TypeInfo.Kind in NeedInitTypeKinds) then
    CopyData(Source, Dest, TypeInfo)
  else
    Move(Source, Dest, Size);
end;

{*
  Copie une variable dont le type poss�de des RTTI
  Utilisez cette variante de CopyData si vous ne connaissez pas la taille du
  type de donn�es. En revanche, vous devez fournir des RTTI non-nulles du type.
  @param Source     Variable source
  @param Dest       Variable destination
  @param TypeInfo   RTTI du type de la variable
*}
procedure CopyData(const Source; var Dest; TypeInfo : PTypeInfo);
begin
  case TypeInfo.Kind of
    tkLString : AnsiString(Dest) := AnsiString(Source);
    tkWString : WideString(Dest) := WideString(Source);
    tkVariant : Variant(Dest) := Variant(Source);
    tkArray :
      with PArrayTypeData(GetTypeData(TypeInfo))^ do
        CopyArray(@Dest, @Source, ElType^, Count);
    tkRecord : CopyRecord(@Dest, @Source, TypeInfo);
    tkInterface : IInterface(Dest) := IInterface(Source);
    tkDynArray : DynArrayCopy(Pointer(Source), TypeInfo, Pointer(Dest));
    else Move(Source, Dest, TypeSize(TypeInfo));
  end;
end;

end.

