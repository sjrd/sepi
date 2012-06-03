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
  Utilitaires de gestion d'un programme en console
  @author sjrd
  @version 1.0
*}
unit ScConsoleUtils;
{$i ..\..\source\Sepi.inc}
interface

uses
  Windows, SysUtils, Classes, Contnrs, IniFiles, TypInfo,
  ScUtils, ScConsts
  {$IFNDEF FPC}
  , StrUtils, ScStrUtils
  {$ENDIF};

type
  {*
    Erreur liée à a la ligne de commande
  *}
  ECommandLineException = class(Exception);

  {*
    Erreur de parsing de la ligne de commande
  *}
  ECommandLineParsingException = class(ECommandLineException);

  {*
    Multiplicité d'une option de ligne de commande
    - omSingle : L'option ne peut pas être présente plus d'une fois ;
    - omMultiple : L'option peut être présente plusieurs fois ;
    - omLeftMost : Conserve uniquement la première occurrence ;
    - omRightMost : Conserve uniquement la dernière occurrence.
  *}
  TCommandLineOptionMultiplicity = (
    omSingle, omMultiple, omLeftMost, omRightMost
  );

  {*
    Option de ligne de commande
    @author sjrd
    @version 1.0
  *}
  TCommandLineOption = class(TObject)
  private
    FName: string;             /// Nom canonique de l'option (--name)
    FChars: TSysCharSet;       /// Caractères acceptés en option courte (-c)
    FMultiplicity: TCommandLineOptionMultiplicity; /// Multiplicité
    FRequired: Boolean;        /// Indique l'option comme requise
    FHasDefaultValue: Boolean; /// Indique si l'option a une valeur par défaut

    FPresent: Boolean; /// Indique si l'option est présente

    FVariable: Pointer;    /// Pointeur sur la variable où stocker le résultat

    {$IFDEF FPC} // and maybe specific to windows too...
    FPresentVar: PWinBool;
    {$ELSE}
    FPresentVar: PBoolean; /// Pointeur sur le booléen où indiquer si présente
    {$ENDIF}

    FRightMostArgument: string; /// Argument sauvegardé pour omRightMost
  protected
    {*
      Analyse interne
      Cette méthode est appelée par Parse après avoir vérifié la multiplicité
      et modifié l'état de présence. Pour omRightMost, c'est Finit qui
      appellera cette méthode.
      @param Argument   Argument (ou chaîne vide si ne prend pas d'argument)
    *}
    procedure InternalParse(const Argument: string); virtual; abstract;

    procedure UseDefaultValue; virtual;

    function GetNeedArgument: Boolean; virtual;

    property Variable: Pointer read FVariable;
  public
    constructor Create(const AName: string; const AChars: TSysCharSet;
      AVariable: Pointer; AMultiplicity: TCommandLineOptionMultiplicity;
      ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False);

    procedure Init; virtual;
    procedure Parse(const Argument: string); virtual;
    procedure Finit; virtual;

    property Name: string read FName;
    property Chars: TSysCharSet read FChars;
    property Multiplicity: TCommandLineOptionMultiplicity read FMultiplicity;
    property Required: Boolean read FRequired;
    property HasDefaultValue: Boolean read FHasDefaultValue;

    property Present: Boolean read FPresent;

    property NeedArgument: Boolean read GetNeedArgument;
  end;

  {*
    Alias d'une option en ligne de commande
    @author sjrd
    @version 1.0
  *}
  TOptionAlias = class(TCommandLineOption)
  private
    FOption: TCommandLineOption; /// Option dont c'est un alias
    FArgument: string;           /// Argument pour l'option
  protected
    procedure InternalParse(const Argument: string); override;

    function GetNeedArgument: Boolean; override;
  public
    constructor Create(const AName: string; const AChars: TSysCharSet;
      AOption: TCommandLineOption; const AArgument: string = '');

    property Option: TCommandLineOption read FOption;
    property Argument: string read FArgument;
  end;

  {*
    Commutateur en ligne de commande
    @author sjrd
    @version 1.0
  *}
  TCommandLineSwitch = class(TCommandLineOption)
  protected
    procedure InternalParse(const Argument: string); override;

    function GetNeedArgument: Boolean; override;
  public
    constructor Create(const AName: string; const AChars: TSysCharSet;
    {$IFDEF FPC}  // and maybe specific to windows too...
    AVariable: PWinBool);
    {$ELSE}
    AVariable: PBoolean);
    {$ENDIF}
  end;

  {*
    Option de ligne de commande entière
    @author sjrd
    @version 1.0
  *}
  TIntegerOption = class(TCommandLineOption)
  private
    FOrdType: TOrdType; /// Type d'ordinal de l'entier stocké
    FMinValue: Int64;   /// Valeur minimale acceptée
    FMaxValue: Int64;   /// Valeur maximale acceptée
  protected
    function ParseValue(const Argument: string): Int64; virtual;
    procedure ValidateValue(Value: Int64); virtual;
    procedure StoreValue(Value: Int64); virtual;

    procedure InternalParse(const Argument: string); override;
  public
    constructor Create(const AName: string; const AChars: TSysCharSet;
      AVariable: Pointer; AOrdType: TOrdType; AMinValue, AMaxValue: Int64;
      ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False); overload;
    constructor Create(const AName: string; const AChars: TSysCharSet;
      AVariable: Pointer; AMinValue, AMaxValue: Int64;
      ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False); overload;
    constructor CreateInt64(const AName: string; const AChars: TSysCharSet;
      AVariable: Pointer; AMinValue, AMaxValue: Int64;
      ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False);

    constructor CreateTypeInfo(const AName: string; const AChars: TSysCharSet;
      AVariable: Pointer; ATypeInfo: PTypeInfo; ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False);

    property OrdType: TOrdType read FOrdType;
    property MinValue: Int64 read FMinValue;
    property MaxValue: Int64 read FMaxValue;
  end;

  {*
    Option de ligne de commande chaîne
    @author sjrd
    @version 1.0
  *}
  TStringOption = class(TCommandLineOption)
  protected
    function ParseValue(const Argument: string): string; virtual;
    procedure ValidateValue(const Value: string); virtual;
    procedure StoreValue(const Value: string); virtual;

    procedure InternalParse(const Argument: string); override;
  public
    constructor Create(const AName: string; const AChars: TSysCharSet;
      AVariable: PString; ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False); overload;
  end;

  {*
    Option de ligne de commande d'un type énuméré
    @author sjrd
    @version 1.0
  *}
  TEnumOption = class(TCommandLineOption)
  private
    FEnumTypeInfo: PTypeInfo; /// RTTI du type énuméré
    FPrefix: string;          /// Préfixe à appliquer devant l'argument
  protected
    procedure InternalParse(const Argument: string); override;
  public
    constructor Create(const AName: string; const AChars: TSysCharSet;
      AVariable: Pointer; AEnumTypeInfo: PTypeInfo; const APrefix: string = '';
      ARequired: Boolean = True;
      {$IFDEF FPC}  // and maybe specific to windows too...
      APresentVar: PWinBool = nil;
      {$ELSE}
      APresentVar: PBoolean = nil;
      {$ENDIF}
      AHasDefaultValue: Boolean = False); overload;

    property EnumTypeInfo: PTypeInfo read FEnumTypeInfo;
    property Prefix: string read FPrefix;
  end;

  {*
    Classe de gestion de la ligne de commande
    @author sjrd
    @version 1.0
  *}
  TCommandLine = class(TObject)
  private
    FSwitchChars: TSysCharSet; /// Caractères marquant une option
    FCaseSensitive: Boolean;   /// True si les recherches sont case-sensitive
    FParameters: TStrings;     /// Paramètres stockés

    FOptions: TObjectList; /// Liste des options recensées
    FNameTable: TStrings;  /// Table des noms des options
    /// Table des versions courtes des options
    FCharsTable: array[Char] of TCommandLineOption;

    function GetCount: Integer;
    function GetParameters(Index: Integer): string;

    function GetOptionCount: Integer;
    function GetOptions(Index: Integer): TCommandLineOption;
  public
    constructor Create(const ASwitchChars: TSysCharSet;
      ACaseSensitive: Boolean = True); overload;
    constructor Create(ACaseSensitive: Boolean = True); overload;
    destructor Destroy; override;

    function IndexOf(const Str: string): Integer;

    procedure AddOption(Option: TCommandLineOption);

    function FindOptionByName(const Name: string): TCommandLineOption;
    function FindOptionByChar(OptChar: Char): TCommandLineOption;

    procedure Parse(UnparsedParams: TStrings = nil);

    property SwitchChars: TSysCharSet read FSwitchChars;
    property CaseSensitive: Boolean read FCaseSensitive;

    property Count: Integer read GetCount;
    property Parameters[Index: Integer]: string read GetParameters; default;

    property OptionCount: Integer read GetOptionCount;
    property Options[Index: Integer]: TCommandLineOption read GetOptions;
  end;

function FindCmdLineOption(const Option: string; const Chars: TSysCharSet;
  IgnoreCase: Boolean = True): Integer; overload;
function FindCmdLineOption(const Option: string;
  IgnoreCase: Boolean = True): Integer; overload;

implementation

{*
  Trouve la position d'une option en ligne de commande
  @param Option       Option recherchée
  @param Chars        Caractères identifiant une option
  @param IgnoreCase   Ignorer la casse (défaut = True)
  @return Position de l'option, ou 0 si non trouvée
*}
function FindCmdLineOption(const Option: string; const Chars: TSysCharSet;
  IgnoreCase: Boolean = True): Integer;
var
  Param: string;
begin
  Result := 1;
  while Result <= ParamCount do
  begin
    Param := ParamStr(Result);

    if (Chars = []) or CharInSet(Param[1], Chars) then
    begin
      if IgnoreCase then
      begin
        if AnsiSameText(Copy(Param, 2, MaxInt), Option) then
          Exit;
      end else
      begin
        if AnsiSameStr(Copy(Param, 2, MaxInt), Option) then
          Exit;
      end;
    end;
  end;

  Result := 0;
end;

{*
  Trouve la position d'une option en ligne de commande
  @param Option       Option recherchée
  @param IgnoreCase   Ignorer la casse (défaut = True)
  @return Position de l'option, ou 0 si non trouvée
*}
function FindCmdLineOption(const Option: string;
  IgnoreCase: Boolean = True): Integer;
begin
  Result := FindCmdLineOption(Option, SwitchChars, IgnoreCase);
end;

{--------------------------}
{ TCommandLineOption class }
{--------------------------}

{*
  Crée l'option de ligne de commande
  @param AName         Nom canonique de l'option
  @param AVariable     Variable où stocker le résultat
  @param ARequired     Indique si l'option est requise
  @param APresentVar   Variable où stocker la présence de l'option
*}
constructor TCommandLineOption.Create(const AName: string;
  const AChars: TSysCharSet; AVariable: Pointer;
  AMultiplicity: TCommandLineOptionMultiplicity; ARequired: Boolean = True;
  {$IFDEF FPC}  // and maybe specific to windows too...
  APresentVar: PWinBool = nil;
  {$ELSE}
  APresentVar: PBoolean = nil;
  {$ENDIF}
  AHasDefaultValue: Boolean = False);
begin
  inherited Create;

  FName := AName;
  FChars := AChars;
  FMultiplicity := AMultiplicity;
  FRequired := ARequired;
  FHasDefaultValue := AHasDefaultValue;

  FVariable := AVariable;
  FPresentVar := APresentVar;
end;

{*
  Utilise la valeur par défaut
  L'implémentation par défaut dans TCommandLineOption ne fait rien, ce qui
  suffit pour toutes les options qui utilisent comme valeur par défaut la
  valeur déjà présente dans Variable^.
*}
procedure TCommandLineOption.UseDefaultValue;
begin
end;

{*
  Indique si l'option requiert un argument
  @return True si l'option requiert un argument, False si elle n'en prend pas
*}
function TCommandLineOption.GetNeedArgument: Boolean;
begin
  Result := True;
end;

{*
  Initialise l'option au démarrage de l'analyse
*}
procedure TCommandLineOption.Init;
begin
  FPresent := False;
  if Assigned(FPresentVar) then
    FPresentVar^ := False;
end;

{*
  Analye l'argument de l'option
  @param Argument   Argument de l'option (chaîne vide si pas d'argument)
*}
procedure TCommandLineOption.Parse(const Argument: string);
begin
  // Checks
  if Present then
  begin
    case Multiplicity of
      omSingle:
        raise ECommandLineParsingException.CreateFmt(
          SSingleOptionFoundMultipleTimes, [Name]);
      omLeftMost:
        Exit;
    end;
  end else
  begin
    FPresent := True;
    if Assigned(FPresentVar) then
      FPresentVar^ := True;
  end;

  // Handle option
  if Multiplicity = omRightMost then
    FRightMostArgument := Argument
  else
    InternalParse(Argument);
end;

{*
  Finalise l'option à la fin de l'analyse
*}
procedure TCommandLineOption.Finit;
begin
  if (not Present) and HasDefaultValue then
  begin
    FPresent := True;
    if Assigned(FPresentVar) then
      FPresentVar^ := True;
    UseDefaultValue;
  end;

  if Required and (not Present) then
    raise ECommandLineParsingException.CreateFmt(
      SRequiredOptionNotFound, [Name]);

  if Present and (Multiplicity = omRightMost) then
    InternalParse(FRightMostArgument);
end;

{--------------------}
{ TOptionAlias class }
{--------------------}

{*
  Crée un alias d'une option
  @param AName       Nom de l'alias
  @param AChars      Caractères de version courte de l'alias
  @param AOption     Option dont c'est un alias
  @param AArgument   Argument à transmettre à l'option
*}
constructor TOptionAlias.Create(const AName: string; const AChars: TSysCharSet;
  AOption: TCommandLineOption; const AArgument: string = '');
begin
  inherited Create(AName, AChars, nil, omMultiple, False);

  FOption := AOption;

  if Option.NeedArgument then
    FArgument := AArgument;
end;

{*
  [@inheritDoc]
*}
procedure TOptionAlias.InternalParse(const Argument: string);
begin
  if FArgument = '' then
    Option.Parse(Argument)
  else
    Option.Parse(FArgument);
end;

{*
  [@inheritDoc]
*}
function TOptionAlias.GetNeedArgument: Boolean;
begin
  Result := Option.NeedArgument and (Argument = '');
end;

{--------------------------}
{ TCommandLineSwitch class }
{--------------------------}

{*
  Crée le switch
  @param AName       Nom canonique du switch
  @param AChars      Caractères de la version courte du switch
  @param AVariable   Pointeur sur la variable booléenne où stocker le résultat
*}
constructor TCommandLineSwitch.Create(const AName: string; const AChars: TSysCharSet;
    {$IFDEF FPC}  // and maybe specific to windows too...
    AVariable: PWinBool);
    {$ELSE}
    AVariable: PBoolean);
    {$ENDIF}
begin
  inherited Create(AName, AChars, nil, omSingle, False, AVariable);
end;

{*
  [@inheritDoc]
*}
function TCommandLineSwitch.GetNeedArgument: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TCommandLineSwitch.InternalParse(const Argument: string);
begin
end;

{----------------------}
{ TIntegerOption class }
{----------------------}

const
  otInt64 = TOrdType(-1);

{*
  Crée l'option
  @param AName         Nom canonique
  @param AChars        Caractères de la version courte
  @param AVariable     Pointeur sur la variable entière où stocker le résultat
  @param AOrdType      Type de la variable entière
  @param AMinValue     Valeur minimale acceptée
  @param AMaxValue     Valeur maximale acceptée
  @param ARequired     Indique si l'option est requise
  @param APresentVar   Pointeur sur une variable booléenne de présence
  @param AHasDefaultValue   Indique si cette option a une valeur par défaut
*}
constructor TIntegerOption.Create(const AName: string;
  const AChars: TSysCharSet; AVariable: Pointer; AOrdType: TOrdType;
  AMinValue, AMaxValue: Int64; ARequired: Boolean = True;
  {$IFDEF FPC}  // and maybe specific to windows too...
  APresentVar: PWinBool = nil;
  {$ELSE}
  APresentVar: PBoolean = nil;
  {$ENDIF}
  AHasDefaultValue: Boolean = False);
begin
  inherited Create(AName, AChars, AVariable, omSingle, ARequired, APresentVar,
    AHasDefaultValue);

  FOrdType := AOrdType;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
end;

{*
  Crée l'option avec le type Integer
  @param AName         Nom canonique
  @param AChars        Caractères de la version courte
  @param AVariable     Pointeur sur la variable entière où stocker le résultat
  @param AMinValue     Valeur minimale acceptée
  @param AMaxValue     Valeur maximale acceptée
  @param ARequired     Indique si l'option est requise
  @param APresentVar   Pointeur sur une variable booléenne de présence
  @param AHasDefaultValue   Indique si cette option a une valeur par défaut
*}
constructor TIntegerOption.Create(const AName: string;
  const AChars: TSysCharSet; AVariable: Pointer; AMinValue, AMaxValue: Int64;
  ARequired: Boolean = True;
  {$IFDEF FPC}  // and maybe specific to windows too...
  APresentVar: PWinBool = nil;
  {$ELSE}
  APresentVar: PBoolean = nil;
  {$ENDIF}
  AHasDefaultValue: Boolean = False);
var
  AOrdType: TOrdType;
begin
  AOrdType := GetTypeData(TypeInfo(Integer)).OrdType;

  Create(AName, AChars, AVariable, AOrdType, AMinValue, AMaxValue, ARequired,
    APresentVar, AHasDefaultValue);
end;

{*
  Crée l'option avec le type Int64
  @param AName         Nom canonique
  @param AChars        Caractères de la version courte
  @param AVariable     Pointeur sur la variable entière où stocker le résultat
  @param AMinValue     Valeur minimale acceptée
  @param AMaxValue     Valeur maximale acceptée
  @param ARequired     Indique si l'option est requise
  @param APresentVar   Pointeur sur une variable booléenne de présence
  @param AHasDefaultValue   Indique si cette option a une valeur par défaut
*}
constructor TIntegerOption.CreateInt64(const AName: string;
  const AChars: TSysCharSet; AVariable: Pointer; AMinValue, AMaxValue: Int64;
  ARequired: Boolean = True;
  {$IFDEF FPC}  // and maybe specific to windows too...
  APresentVar: PWinBool = nil;
  {$ELSE}
  APresentVar: PBoolean = nil;
  {$ENDIF}
  AHasDefaultValue: Boolean = False);
begin
  Create(AName, AChars, AVariable, otInt64, AMinValue, AMaxValue, ARequired,
    APresentVar, AHasDefaultValue);
end;

{*
  Crée l'option avec un type défini par ses RTTI
  @param AName         Nom canonique
  @param AChars        Caractères de la version courte
  @param AVariable     Pointeur sur la variable entière où stocker le résultat
  @param ATypeInfo     RTTI du type de l'option
  @param ARequired     Indique si l'option est requise
  @param APresentVar   Pointeur sur une variable booléenne de présence
  @param AHasDefaultValue   Indique si cette option a une valeur par défaut
*}
constructor TIntegerOption.CreateTypeInfo(const AName: string;
  const AChars: TSysCharSet; AVariable: Pointer; ATypeInfo: PTypeInfo;
  ARequired: Boolean = True;
  {$IFDEF FPC}  // and maybe specific to windows too...
  APresentVar: PWinBool = nil;
  {$ELSE}
  APresentVar: PBoolean = nil;
  {$ENDIF}
  AHasDefaultValue: Boolean = False);
var
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(ATypeInfo);

  case ATypeInfo.Kind of
    tkInteger:
    begin
      Create(AName, AChars, AVariable, TypeData.OrdType,
        TypeData.MinValue, TypeData.MaxValue, ARequired,
        APresentVar, AHasDefaultValue);
    end;
    tkInt64:
    begin
      CreateInt64(AName, AChars, AVariable,
        TypeData.MinInt64Value, TypeData.MaxInt64Value, ARequired,
        APresentVar, AHasDefaultValue);
    end;
  else
    raise ECommandLineException.Create(SInvalidTypeInfo);
  end;
end;

{*
  Analyse l'argument sous forme d'entier
  @param Argument   Argument à analyser
  @return Valeur entière correspondante
  @raise ECommandLineParsingException L'argument n'est pas un entier
*}
function TIntegerOption.ParseValue(const Argument: string): Int64;
begin
  try
    Result := StrToInt64(Argument);
  except
    on Error: EConvertError do
      raise ECommandLineParsingException.Create(Error.Message);
  end;
end;

{*
  Valide une valeur
  @param Value   Valeur à valider
  @raise ECommandLineParsingException La valeur n'est pas valide
*}
procedure TIntegerOption.ValidateValue(Value: Int64);
begin
  if (Value < MinValue) or (Value > MaxValue) then
    raise ECommandLineParsingException.CreateFmt(SOptionValueOutOfRange,
      [Name, MinValue, MaxValue]);
end;

{*
  Stocke la valeur
  @param Value   Valeur à stocker
*}
procedure TIntegerOption.StoreValue(Value: Int64);
begin
  case OrdType of
    otSByte: Shortint(Variable^) := Value;
    otUByte: Byte    (Variable^) := Value;
    otSWord: Smallint(Variable^) := Value;
    otUWord: Word    (Variable^) := Value;
    otSLong: Longint (Variable^) := Value;
    otULong: LongWord(Variable^) := Value;
  else
    Int64(Variable^) := Value;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TIntegerOption.InternalParse(const Argument: string);
var
  Value: Int64;
begin
  Value := ParseValue(Argument);
  ValidateValue(Value);
  StoreValue(Value);
end;

{---------------------}
{ TStringOption class }
{---------------------}

{*
  Crée l'option
  @param AName         Nom canonique
  @param AChars        Caractères de la version courte
  @param AVariable     Pointeur sur la variable entière où stocker le résultat
  @param ARequired     Indique si l'option est requise
  @param APresentVar   Pointeur sur une variable booléenne de présence
  @param AHasDefaultValue   Indique si cette option a une valeur par défaut
*}
constructor TStringOption.Create(const AName: string;
  const AChars: TSysCharSet; AVariable: PString; ARequired: Boolean = True;
        {$IFDEF FPC}  // and maybe specific to windows too...
        APresentVar: PWinBool = nil;
        {$ELSE}
        APresentVar: PBoolean = nil;
        {$ENDIF}
        AHasDefaultValue: Boolean = False);
begin
  inherited Create(AName, AChars, AVariable, omSingle, ARequired, APresentVar,
    AHasDefaultValue);
end;

{*
  Analyse l'argument sous forme de chaîne de caractère
  @param Argument   Argument à analyser
  @return Valeur chaîne correspondante
  @raise ECommandLineParsingException L'argument n'est pas une chaine
*}
function TStringOption.ParseValue(const Argument: string): string;
begin
  Result := Argument;
end;

{*
  Valide une valeur
  @param Value   Valeur à valider
  @raise ECommandLineParsingException La valeur n'est pas valide
*}
procedure TStringOption.ValidateValue(const Value: string);
begin
end;

{*
  Stocke la valeur
  @param Value   Valeur à stocker
*}
procedure TStringOption.StoreValue(const Value: string);
begin
  string(Variable^) := Value;
end;

{*
  [@inheritDoc]
*}
procedure TStringOption.InternalParse(const Argument: string);
var
  Value: string;
begin
  Value := ParseValue(Argument);
  ValidateValue(Value);
  StoreValue(Value);
end;

{-------------------}
{ TEnumOption class }
{-------------------}

{*
  Crée l'option
  @param AName           Nom canonique
  @param AChars          Caractères de la version courte
  @param AVariable       Pointeur sur la variable enum où stocker le résultat
  @param AEnumTypeInfo   RTTI du type énuméré
  @param APrefix         Préfixe à appliquer devant l'argument
  @param ARequired       Indique si l'option est requise
  @param APresentVar     Pointeur sur une variable booléenne de présence
  @param AHasDefaultValue   Indique si cette option a une valeur par défaut
*}
constructor TEnumOption.Create(const AName: string; const AChars: TSysCharSet;
  AVariable: Pointer; AEnumTypeInfo: PTypeInfo; const APrefix: string;
  ARequired: Boolean;
  {$IFDEF FPC}  // and maybe specific to windows too...
  APresentVar: PWinBool = nil;
  {$ELSE}
  APresentVar: PBoolean = nil;
  {$ENDIF}
  AHasDefaultValue: Boolean = False);
begin
  inherited Create(AName, AChars, AVariable, omSingle, ARequired, APresentVar,
    AHasDefaultValue);

  FEnumTypeInfo := AEnumTypeInfo;
  FPrefix := APrefix;

  if EnumTypeInfo.Kind <> tkEnumeration then
    raise ECommandLineException.Create(SInvalidTypeInfo);
end;

{*
  [@inheritDoc]
*}
procedure TEnumOption.InternalParse(const Argument: string);
var
  IntValue: Integer;
begin
  IntValue := TypInfo.GetEnumValue(EnumTypeInfo, Prefix+Argument);
  if IntValue < 0 then
    raise ECommandLineParsingException.CreateFmt(SInvalidValue,
      [Argument, Name]);

  case GetTypeData(EnumTypeInfo).OrdType of
    otUByte: PByte(Variable)^ := IntValue;
    otUWord: PWord(Variable)^ := IntValue;
    otULong: PLongWord(Variable)^ := IntValue;
  end;
end;

{--------------------}
{ TCommandLine class }
{--------------------}

{*
  Crée un gestionnaire de ligne de commande
  @param ASwitchChars     Caractères marquant les options
  @param ACaseSensitive   True pour effectuer des comparaisons case-sensitive
*}
constructor TCommandLine.Create(const ASwitchChars: TSysCharSet;
  ACaseSensitive: Boolean = True);
var
  I: Integer;
begin
  inherited Create;

  FSwitchChars := ASwitchChars;
  FCaseSensitive := ACaseSensitive;

  FParameters := TStringList.Create;
  TStringList(FParameters).CaseSensitive := CaseSensitive;

  FOptions := TObjectList.Create;

  FNameTable := THashedStringList.Create;
  TStringList(FNameTable).CaseSensitive := CaseSensitive;

  for I := 1 to ParamCount do
    FParameters.Add(ParamStr(I));
end;

{*
  Crée un gestionnaire de ligne de commande
  Les caractères marquant les options sont définis d'après SysUtils.SwitchChars.
  @param ACaseSensitive   True pour effectuer des comparaisons case-sensitive
*}
constructor TCommandLine.Create(ACaseSensitive: Boolean = True);
begin
  Create(SysUtils.SwitchChars, ACaseSensitive);
end;

{*
  [@inheritDoc]
*}
destructor TCommandLine.Destroy;
begin
  FNameTable.Free;
  FOptions.Free;
  FParameters.Free;

  inherited;
end;

{*
  Nombre de paramètres en ligne de commande
  @return Nombre de paramètres en ligne de commande
*}
function TCommandLine.GetCount: Integer;
begin
  Result := FParameters.Count;
end;

{*
  Tableau zero-based des paramètres en ligne de commande
  @param Index   Index d'un paramètre (0 pour le premier, pas comme ParamStr)
  @return Paramètres à cet index
*}
function TCommandLine.GetParameters(Index: Integer): string;
begin
  Result := FParameters[Index];
end;

{*
  Nombre d'options recensées
  @return Nombre d'options recensées
*}
function TCommandLine.GetOptionCount: Integer;
begin
  Result := FOptions.Count;
end;

{*
  Tableau zero-based des options recensées
  @param Index   Index d'une option
  @return Option à cet index
*}
function TCommandLine.GetOptions(Index: Integer): TCommandLineOption;
begin
  Result := TCommandLineOption(FOptions[Index]);
end;

{*
  Cherche un paramètre
  @param Str   Paramètre recherché
  @return Position de ce paramètre, ou -1 si non trouvé
*}
function TCommandLine.IndexOf(const Str: string): Integer;
begin
  Result := FParameters.IndexOf(Str);
end;

{*
  Recense une nouvelle option
  @param Option à recenser
*}
procedure TCommandLine.AddOption(Option: TCommandLineOption);
const
  CaseDiff = Ord('a') - Ord('A');
var
  C: Char;
begin
  if FNameTable.IndexOf(Option.Name) >= 0 then
    raise ECommandLineException.CreateFmt(SDuplicateLongOption, [Option.Name]);

  for C in Option.Chars do
    if FCharsTable[C] <> nil then
      raise ECommandLineException.CreateFmt(SDuplicateShortOption, [C]);

  FOptions.Add(Option);

  FNameTable.AddObject(Option.Name, Option);

  for C in Option.Chars do
  begin
    FCharsTable[C] := Option;

    if not CaseSensitive then
    begin
      if CharInSet(C, ['a'..'z']) then
        FCharsTable[Chr(Ord(C) - CaseDiff)] := Option
      else if CharInSet(C, ['A'..'Z']) then
        FCharsTable[Chr(Ord(C) + CaseDiff)] := Option;
    end;
  end;
end;

{*
  Trouve une option depuis sont nom
  @param Name   Nom de l'option recherchée
  @return Option correspondante
  @raise ECommandLineParsingException Option inconnue
*}
function TCommandLine.FindOptionByName(const Name: string): TCommandLineOption;
var
  Index: Integer;
begin
  Index := FNameTable.IndexOf(Name);
  if Index < 0 then
    raise ECommandLineParsingException.CreateFmt(
      SUnknownLongOption, [Name]);

  Result := TCommandLineOption(FNameTable.Objects[Index]);
end;

{*
  Trouve une option depuis sa version courte
  @param Name   Version courte de l'option recherchée
  @return Option correspondante
  @raise ECommandLineParsingException Option inconnue
*}
function TCommandLine.FindOptionByChar(OptChar: Char): TCommandLineOption;
begin
  Result := FCharsTable[OptChar];
  if Result = nil then
    raise ECommandLineParsingException.CreateFmt(
      SUnknownShortOption, [OptChar]);
end;

{*
  Analyse la ligne de commande
  @param UnparsedParams   List dans laquelle stocker les paramètres non analysés
*}
procedure TCommandLine.Parse(UnparsedParams: TStrings = nil);
var
  I, Current, Position: Integer;
  Param, Argument: string;
  Option: TCommandLineOption;
begin
  // Initialization
  for I := 0 to OptionCount-1 do
    Options[I].Init;

  // Parsing
  Current := 0;
  while Current < Count do
  begin
    Param := Parameters[Current];
    Inc(Current);

    if (Length(Param) < 2) or (not CharInSet(Param[1], SwitchChars)) then
    begin
      if UnparsedParams = nil then
        raise ECommandLineParsingException.Create(SNonOptionParamForbidden);
      UnparsedParams.Add(Param);
    end else
    begin
      if Param[2] = Param[1] then
      begin
        // Long option --name --name=value {--name value}
        Position := Pos('=', Param);

        // Find option
        if Position = 0 then
          Option := FindOptionByName(Copy(Param, 3, MaxInt))
        else
          Option := FindOptionByName(Copy(Param, 3, Position-3));

        // Read argument
        if Option.NeedArgument then
        begin
          if Position = 0 then
          begin
            if Current >= Count then
              raise ECommandLineParsingException.CreateFmt(
                SArgumentRequired, [Option.Name]);

            Argument := Parameters[Current];
            Inc(Current);
          end else
          begin
            Argument := Copy(Param, Position+1, MaxInt);
          end;
        end else
        begin
          if Position > 0 then
            raise ECommandLineParsingException.CreateFmt(
              SArgumentForbidden, [Option.Name]);

          Argument := '';
        end;

        // Parse option
        Option.Parse(Argument);
      end else
      begin
        // Short option -c -cde -cValue -cdeValue {-c Value} {-cde Value}
        Position := 2;
        repeat
          // Find option
          Option := FindOptionByChar(Param[Position]);
          Inc(Position);

          // Read argument
          if not Option.NeedArgument then
            Argument := ''
          else
          begin
            if Position <= Length(Param) then
              Argument := Copy(Param, Position, MaxInt)
            else
            begin
              if Current >= Count then
                raise ECommandLineParsingException.CreateFmt(
                  SArgumentRequired, [Option.Name]);

              Argument := Parameters[Current];
              Inc(Current);
            end;
          end;

          // Parse option
          Option.Parse(Argument);
        until (Position > Length(Param)) or Option.NeedArgument;
      end;
    end;
  end;

  // Finalization
  for I := 0 to OptionCount-1 do
    Options[I].Finit;
end;

end.

