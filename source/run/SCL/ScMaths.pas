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
  Définit des classes et routines de mathématiques avancées
  @author sjrd
  @version 1.0
*}
unit ScMaths;

interface

uses
  Classes, SysUtils, Contnrs, Math, VarCmplx, ScUtils, ScStrUtils, ScLists,
  ScConsts;

const
  MonomsChars = ['0'..'9', '­', 'x', 'e', 'E', ',', '.'];
  BinaryOperators = ['+', '-', '*', '/', '%', '^', 'r', 'o', '|'];
  UnaryOperators = ['­', 'a', 's', 'c', 't', 'S', 'C', 'T',
    '²', '\', '!', 'v', 'w'];
  VarsListsCount = 2;

type
  TComplex = type Variant;
  TComplexList = class;
  TPolynom = class;
  EEvalError = class;
  EEvalErrorClass = class of EEvalError;

{$REGION 'EMathsError et descendants'}

  EMathsError = class(Exception)
    // Toute erreur déclenchée dans cette unité
  public
    constructor CreateMaths;
  end;

  EDivisionError = class(EMathsError)
    // Erreur de division
  public
    Dividend, Divisor: TPolynom;
    constructor CreateDivision(ADividend, ADivisor: TPolynom);
    destructor Destroy; override;
  end;

  ERootError = class(EMathsError)
    // Erreur de racine
  public
    Degree, Radican: TPolynom;
    constructor CreateRoot(ADegree, ARadican: TPolynom);
    destructor Destroy; override;
  end;

  EDegreeError = class(EMathsError)
    // Erreur de degré
  public
    Value: TPolynom;
    constructor CreateDegree(AValue: TPolynom);
    destructor Destroy; override;
  end;

  ENotDegreeZeroError = class(EDegreeError)
    // Erreur : polynome n'est pas du degré 0
  public
    constructor CreateNotDegreeZero(AValue: TPolynom);
  end;

  EIntegerError = class(EMathsError)
    // Erreur d'entier
  public
    Value: Extended;
    constructor CreateInteger(AValue: Extended);
  end;

  ENotIntegerError = class(EIntegerError)
    // Erreur : Extended n'est pas un entier
  public
    constructor CreateNotInteger(AValue: Extended);
  end;

  ENaturalError = class(EMathsError)
    // Erreur de naturel
  public
    Value: Int64;
    constructor CreateNatural(AValue: Int64);
  end;

  ENotNaturalError = class(ENaturalError)
    // Erreur : Entier n'est pas naturel
  public
    constructor CreateNotNatural(AValue: Int64);
  end;

  EEvalError = class(EMathsError)
    // Erreur provoquée par Eval
  public
    Expression: string;
    constructor CreateEval(AExpression: string; AMessage: string);
      overload;
    constructor CreateEval(AExpression: string; Format: string;
      Args: array of const); overload;
    function ClassType: EEvalErrorClass;
  end;

{$REGION 'EEvalError et descendants'}

  EWrongExpressionError = class(EEvalError)
    // Mauvaise expression
  public
    constructor CreateWrongExpression;
  end;

  EWrongCharacterError = class(EEvalError)
    // Mauvais caractère
  public
    Character: Char;
    constructor CreateWrongCharacter(ACharacter: Char);
  end;

  EOperationError = class(EEvalError)
    // Erreur d'opération
  public
    Operation: string;
    constructor CreateOperation(AOperation: string);
  end;

  EOpNotExistsError = class(EOperationError)
    // L'opération spécifiée n'existe pas
  public
    constructor CreateOpNotExists(AOperation: string);
    constructor CreateOpIsNotBinary(AOperation: string);
    constructor CreateOpIsNotUnary(AOperation: string);
  end;

  EOpRequestsError = class(EOperationError);
  // Erreur : Opérandes incorrect

  EOpRequestsDegreeZeroError = class(EOpRequestsError)
    // Erreur : L'opération requiert un polynome de degré 0
  public
    Value: TPolynom;
    constructor CreateOpRequestsDegreeZero(AOperation: string;
      AValue: TPolynom);
    destructor Destroy; override;
  end;

  EOpRequestsIntegerError = class(EOpRequestsError)
    // Erreur : L'opération requiert un nombre entier
  public
    Value: Extended;
    constructor CreateOpRequestsInteger(AOperation: string;
      AValue: Extended);
  end;

  EOpRequestsNaturalError = class(EOpRequestsError)
    // Erreur : L'opération requiert un nombre naturel
  public
    Value: Int64;
    constructor CreateOpRequestsNatural(AOperation: string;
      AValue: Int64);
  end;

  EOpRequestsCorrectIndexError = class(EOpRequestsError)
    // Erreur : L'opération requiert un index de liste correct
  public
    Index: Integer;
    constructor CreateOpRequestsCorrectIndex(AOperation: string;
      AIndex: Integer);
  end;

  EDivisionOpError = class(EOperationError)
    // Erreur dans l'opération de division
  public
    Dividend, Divisor: TPolynom;
    constructor CreateDivisionOp(ADividend, ADivisor: TPolynom);
    destructor Destroy; override;
  end;

  ERootOpError = class(EOperationError)
    // Erreur dans l'opération de racine
  public
    Degree, Radican: TPolynom;
    constructor CreateRootOp(ADegree, ARadican: TPolynom);
    destructor Destroy; override;
  end;

  EBracketsError = class(EEvalError)
    // Erreur de parenthèses
  public
    constructor CreateBrackets;
  end;

  ETooManyBracketsError = class(EBracketsError)
    // Erreur : Trop de parenthèses
  public
    HowTooMany: Integer;
    WhichBracket: Char;
    constructor CreateTooManyBrackets(AHowTooMany: Integer;
      AWhichBracket: Char);
  end;

{$ENDREGION}

{$REGION 'EEquationError et descendants'}

  EEquationError = class(EMathsError);
  // Erreur d'équation

  ECannotSolveEquationError = class(EEquationError);
  // Erreur : ne peut résoudre l'équation

  EDegreeTooHighError = class(ECannotSolveEquationError);
  // Erreur : Degré de l'équation trop élevé
  EDegreeTooLowError = class(ECannotSolveEquationError);
  // Erreur : Degré de l'équation trop bas

{$ENDREGION}

{$ENDREGION}

  TTestIsAbortedProc = procedure of object;
  TScalarProdFunc = function(Left, Right: TPolynom): Extended;

  TComplexList = class(TScList)
    // Liste de nombres complexes
  private
    function GetItems(Index: Integer): TComplex;
    procedure SetItems(Index: Integer; New: TComplex);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function IsAssignClass(ScListClass: TScListClass): Boolean; override;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;

    procedure Write(New: TComplex);
    function Read: TComplex;
    function Add(New: TComplex): Integer;
    function Insert(Index: Integer; New: TComplex): Integer;
    function Delete(Index: Integer): TComplex;

    property Items[Index: Integer]: TComplex read GetItems write SetItems;
      default;
  end;

  TPolynom = class
    // Polynome à 1 variable
  private
    FCoefficients: TExtendedList;
    FMinDegree: Integer;
    function GetMinDegree: Integer;
    function GetMaxDegree: Integer;
    function GetNegDegree: Integer;
    function GetPosDegree: Integer;
    function GetIsZero: Boolean;
    function GetIsExtended: Boolean;
    function GetIsInteger: Boolean;
    function GetIsNatural: Boolean;
    function GetCoefficients(Exponent: Integer): Extended;
    function GetAsExtended: Extended;
    function GetAsInteger: Int64;
    function GetAsNatural: Int64;
    procedure SetCoefficients(Exponent: Integer; New: Extended);
    procedure SetAsExtended(New: Extended);
    procedure SetAsInteger(New: Int64);
  public
    constructor Create;
    constructor CreateAssign(Source: TPolynom);
    constructor CreateFromExtended(Exponent: Integer; Coefficient: Extended);
    constructor CreateFromStream(Stream: TStream);
    destructor Destroy; override;
    procedure SetToZero;
    procedure Assign(Source: TPolynom);
    function ToString: string; {$IF RTLVersion >= 20.0} override; {$IFEND}
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure Add(Source: TPolynom);
    procedure Sub(Source: TPolynom);
    procedure Multiply(Source: TPolynom);
    procedure Divide(Source: TPolynom);
    procedure Modulo(Source: TPolynom);
    procedure Power(Source: TPolynom);
    procedure Root(Source: TPolynom);
    procedure Insert(Source: TPolynom);
    procedure ScalarProd(Source: TPolynom);
    procedure Oppose;
    procedure Reverse;
    procedure Abs;
    procedure Sinus;
    procedure Cosinus;
    procedure Tangent;
    procedure ArcSinus;
    procedure ArcCosinus;
    procedure ArcTangent;
    procedure Factorial;
    class function Addition(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function Substraction(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function Multiplication(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function Division(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function ModuloOf(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function PowerOf(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function RootOf(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function InsertOf(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function ScalarProdOf(Val1, Val2: TPolynom;
      ReleaseVals: Boolean = False): TPolynom;
    class function OpposedOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function ReversedOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function AbsOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function SinusOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function CosinusOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function TangentOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function ArcSinusOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function ArcCosinusOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function ArcTangentOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    class function FactorialOf(Value: TPolynom;
      ReleaseValue: Boolean = False): TPolynom;
    property MinDegree: Integer read GetMinDegree;
    property MaxDegree: Integer read GetMaxDegree;
    property NegDegree: Integer read GetNegDegree;
    property PosDegree: Integer read GetPosDegree;
    property IsZero: Boolean read GetIsZero;
    property IsExtended: Boolean read GetIsExtended;
    property IsInteger: Boolean read GetIsInteger;
    property IsNatural: Boolean read GetIsNatural;
    property Coefficients[Exponent: Integer]: Extended
      read GetCoefficients write SetCoefficients; default;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsNatural: Int64 read GetAsNatural write SetAsInteger;
  end;

  TPolynomList = class
    // Liste de polynomes
  private
    FPolynoms: TObjectList;
    function GetCount: Integer;
    function GetOwnsObjects: Boolean;
    function GetPolynoms(Index: Integer): TPolynom;
    procedure SetOwnsObjects(New: Boolean);
    procedure SetPolynoms(Index: Integer; New: TPolynom);
  public
    constructor Create(AOwnsObjects: Boolean = False);
    constructor CreateFromStream(Stream: TStream;
      AOwnsObjects: Boolean = False);
    constructor CreateFromFile(FileName: string;
      AOwnsObjects: Boolean = False);
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure Clear;
    function Add(New: TPolynom): Integer;
    function Insert(Index: Integer; New: TPolynom): Integer;
    procedure Delete(Index: Integer);
    function Extract(Polynom: TPolynom): TPolynom;
    property Count: Integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    property Polynoms[Index: Integer]: TPolynom
      read GetPolynoms write SetPolynoms; default;
  end;

function IsInteger(X: Extended): Boolean;
// Renvoie True si X est un Int64
procedure VerifyIsInteger(X: Extended);
// Déclenche une exception ENotInteger si X n'est pas un Int64
function AsInteger(X: Extended): Int64;
// Si X est un Int64, renvoie l'Int64 correspondant ;
// Sinon déclenche une exception ENotInteger

function IsNatural(X: Int64): Boolean;
// Renvoie True si X est un naturel
procedure VerifyIsNatural(X: Int64);
// Déclenche une exception ENotNatural si X n'est pas un naturel
function AsNatural(X: Int64): Int64;
// Si X est un naturel, renvoie X ;
// Sinon déclenche une exception ENotNatural

function Factorial(X: Int64): Int64;
// Renvoie X!

function IntPower(Base, Exponent: LongWord): LongWord;
// Renvoie Base^Exponent

function DefaultScalarProd(Left, Right: TPolynom): Extended;
// Produit scalaire canonique

function Eval(Expression: string;
  TestIsAborted: TTestIsAbortedProc = nil): TPolynom;
// Evalue l'expression mathématique Expression

const
  MinValueEver = 1e-16;

var
  VarsLists: array[0..VarsListsCount-1] of TPolynomList;
  ScalarProdFunc: TScalarProdFunc;

implementation

{$REGION 'EMathsError et descendants'}

{----------------------------------------------}
{ Constructeurs et destructeurs des exceptions }
{----------------------------------------------}

constructor EMathsError.CreateMaths;
begin
  Create(sScErrorMaths);
end;

constructor EDivisionError.CreateDivision(ADividend, ADivisor: TPolynom);
begin
  if ADivisor.IsZero then
    CreateFmt(sScErrorDivByZero, [ADividend.ToString])
  else
    CreateFmt(sScErrorDivision, [ADividend.ToString, ADivisor.ToString]);
  Dividend := TPolynom.CreateAssign(ADividend);
  Divisor := TPolynom.CreateAssign(ADivisor);
end;

destructor EDivisionError.Destroy;
begin
  Dividend.Free;
  inherited Destroy;
end;

constructor ERootError.CreateRoot(ADegree, ARadican: TPolynom);
begin
  CreateFmt(sScErrorRoot, [ADegree.ToString, ARadican.ToString]);
  Degree := TPolynom.CreateAssign(ADegree);
  Radican := TPolynom.CreateAssign(ARadican);
end;

destructor ERootError.Destroy;
begin
  Degree.Free;
  Radican.Free;
  inherited Destroy;
end;

constructor EDegreeError.CreateDegree(AValue: TPolynom);
begin
  CreateFmt(sScErrorDegree, [AValue.ToString]);
  Value := TPolynom.CreateAssign(AValue);
end;

destructor EDegreeError.Destroy;
begin
  Value.Free;
  inherited Destroy;
end;

constructor ENotDegreeZeroError.CreateNotDegreeZero(AValue: TPolynom);
begin
  CreateFmt(sScErrorNotDegreeZero, [AValue.ToString]);
  Value := TPolynom.CreateAssign(AValue);
end;

constructor EIntegerError.CreateInteger(AValue: Extended);
begin
  CreateFmt(sScErrorInteger, [AValue]);
  Value := AValue;
end;

constructor ENotIntegerError.CreateNotInteger(AValue: Extended);
begin
  CreateFmt(sScErrorNotInteger, [AValue]);
  Value := AValue;
end;

constructor ENaturalError.CreateNatural(AValue: Int64);
begin
  CreateFmt(sScErrorNatural, [AValue]);
  Value := AValue;
end;

constructor ENotNaturalError.CreateNotNatural(AValue: Int64);
begin
  CreateFmt(sScErrorNotNatural, [AValue]);
  Value := AValue;
end;

{$REGION 'EEvalError et descendants'}

constructor EEvalError.CreateEval(AExpression: string; AMessage: string);
begin
  Create(Format(sScErrorEval, [AExpression])+AMessage);
  Expression := AExpression;
end;

constructor EEvalError.CreateEval(AExpression: string;
  Format: string; Args: array of const);
begin
  Create(SysUtils.Format(sScErrorEval, [AExpression]) +
    SysUtils.Format(Format, Args));
end;

function EEvalError.ClassType: EEvalErrorClass;
begin
  Result := EEvalErrorClass(inherited ClassType);
end;

constructor EWrongExpressionError.CreateWrongExpression;
begin
  Create(sScErrorWrongExpression);
end;

constructor EWrongCharacterError.CreateWrongCharacter(ACharacter: Char);
begin
  CreateFmt(sScErrorWrongCharacter, [ACharacter]);
  Character := ACharacter;
end;

constructor EOperationError.CreateOperation(AOperation: string);
begin
  CreateFmt(sScErrorOperation, [AOperation]);
  Operation := AOperation;
end;

constructor EOpNotExistsError.CreateOpNotExists(AOperation: string);
begin
  CreateFmt(sScErrorOpNotExists, [AOperation]);
  Operation := AOperation;
end;

constructor EOpNotExistsError.CreateOpIsNotBinary(AOperation: string);
begin
  CreateFmt(sScErrorOpIsNotBinary, [AOperation]);
  Operation := AOperation;
end;

constructor EOpNotExistsError.CreateOpIsNotUnary(AOperation: string);
begin
  CreateFmt(sScErrorOpIsNotUnary, [AOperation]);
  Operation := AOperation;
end;

constructor EOpRequestsDegreeZeroError.CreateOpRequestsDegreeZero(
  AOperation: string; AValue: TPolynom);
begin
  CreateFmt(sScErrorOpRequestsDegreeZero,
    [AOperation, AValue.ToString, AValue.MinDegree, AValue.MaxDegree]);
  Operation := AOperation;
  Value := TPolynom.CreateAssign(AValue);
end;

destructor EOpRequestsDegreeZeroError.Destroy;
begin
  Value.Free;
  inherited Destroy;
end;

constructor EOpRequestsIntegerError.CreateOpRequestsInteger(
  AOperation: string;
  AValue: Extended);
begin
  CreateFmt(sScErrorOpRequestsInteger, [AOperation, AValue]);
  Value := AValue;
end;

constructor EOpRequestsNaturalError.CreateOpRequestsNatural(
  AOperation: string;
  AValue: Int64);
begin
  CreateFmt(sScErrorOpRequestsNatural, [AOperation, AValue]);
  Value := AValue;
end;

constructor EOpRequestsCorrectIndexError.CreateOpRequestsCorrectIndex(
  AOperation: string; AIndex: Integer);
begin
  CreateFmt(sScErrorOpRequestsCorrectIndex, [AOperation, AIndex]);
  Index := AIndex;
end;

constructor EDivisionOpError.CreateDivisionOp(ADividend, ADivisor: TPolynom);
begin
  if ADivisor.IsZero then
    CreateFmt(sScErrorDivByZero, [ADividend.ToString])
  else
    CreateFmt(sScErrorDivision, [ADividend.ToString, ADivisor.ToString]);
  Dividend := TPolynom.CreateAssign(ADividend);
  Divisor := TPolynom.CreateAssign(ADivisor);
end;

destructor EDivisionOpError.Destroy;
begin
  Dividend.Free;
  Divisor.Free;
  inherited Destroy;
end;

constructor ERootOpError.CreateRootOp(ADegree, ARadican: TPolynom);
begin
  CreateFmt(sScErrorRoot, [ADegree.ToString, ARadican.ToString]);
  Degree := TPolynom.CreateAssign(ADegree);
  Radican := TPolynom.CreateAssign(ARadican);
end;

destructor ERootOpError.Destroy;
begin
  Degree.Free;
  Radican.Free;
  inherited Destroy;
end;

constructor EBracketsError.CreateBrackets;
begin
  Create(sScErrorBrackets);
end;

constructor ETooManyBracketsError.CreateTooManyBrackets(AHowTooMany: Integer;
  AWhichBracket: Char);
begin
  CreateFmt(sScErrorTooManyBrackets, [AHowTooMany, AWhichBracket]);
  HowTooMany := AHowTooMany;
  WhichBracket := AWhichBracket;
end;

{$ENDREGION}

{$ENDREGION}

{$REGION 'Procédures et fonctions globales'}

{----------------------------------}
{ Procédures et fonctions globales }
{----------------------------------}

function IsInteger(X: Extended): Boolean;
begin
  Result := X = Trunc(X);
end;

procedure VerifyIsInteger(X: Extended);
begin
  if not IsInteger(X) then
    raise ENotIntegerError.CreateNotInteger(X);
end;

function AsInteger(X: Extended): Int64;
begin
  VerifyIsInteger(X);
  Result := Trunc(X);
end;

function IsNatural(X: Int64): Boolean;
begin
  Result := X >= 0;
end;

procedure VerifyIsNatural(X: Int64);
begin
  if not IsNatural(X) then
    raise ENotNaturalError.CreateNotNatural(X);
end;

function AsNatural(X: Int64): Int64;
begin
  VerifyIsNatural(X);
  Result := X;
end;

function Factorial(X: Int64): Int64;
begin
  VerifyIsNatural(X);
  if X <= 1 then
    Result := 1
  else
    Result := X*Factorial(X-1);
end;

function IntPower(Base, Exponent: LongWord): LongWord;
begin
  Result := 1;
  while Exponent > 0 do
  begin
    Result := Result*Base;
    Dec(Exponent);
  end;
end;

function DefaultScalarProd(Left, Right: TPolynom): Extended;
var
  FromDeg, ToDeg, Deg: Integer;
begin
  Result := 0.0;
  FromDeg := Max(Left.MinDegree, Right.MinDegree);
  ToDeg := Min(Left.MaxDegree, Right.MaxDegree);

  for Deg := FromDeg to ToDeg do
    Result := Result + Left[Deg]*Right[Deg];
end;

{$ENDREGION}

{$REGION 'Classe TComplexList'}

{---------------------}
{ Classe TComplexList }
{---------------------}

type
  TComplexRec = record
    RealPart, ImaginaryPart: Double;
  end;

constructor TComplexList.Create;
begin
  inherited Create(16);
end;

function TComplexList.GetItems(Index: Integer): TComplex;
var
  ComplexRec: TComplexRec;
begin
  _GetItems(Index, ComplexRec);
  Result := VarComplexCreate(ComplexRec.RealPart, ComplexRec.ImaginaryPart);
end;

procedure TComplexList.SetItems(Index: Integer; New: TComplex);
var
  ComplexRec: TComplexRec;
begin
  ComplexRec.RealPart := New.Real;
  ComplexRec.ImaginaryPart := New.Imaginary;
  _SetItems(Index, ComplexRec);
end;

procedure TComplexList.AssignTo(Dest: TPersistent);
var
  DestStrings: TStrings;
begin
  if Dest is TStrings then
  begin
    DestStrings := TStrings(Dest);
    DestStrings.Clear;
    Reset;
    while HasMoreValue do
      DestStrings.Append(Read);
  end else
    inherited;
end;

function TComplexList.IsAssignClass(ScListClass: TScListClass): Boolean;
begin
  if ScListClass.InheritsFrom(TComplexList) then
    Result := True
  else
    Result := inherited IsAssignClass(ScListClass);
end;

procedure TComplexList.Assign(Source: TPersistent);
var
  SourceStrings: TStrings;
  I: Integer;
begin
  if Source is TStrings then
  begin
    SourceStrings := TStrings(Source);
    Clear;
    for I := 0 to SourceStrings.Count-1 do
      Add(VarComplexCreate(SourceStrings[I]));
  end else
    inherited;
end;

procedure TComplexList.Write(New: TComplex);
var
  ComplexRec: TComplexRec;
begin
  ComplexRec.RealPart := New.Real;
  ComplexRec.ImaginaryPart := New.Imaginary;
  _Write(ComplexRec);
end;

function TComplexList.Read: TComplex;
var
  ComplexRec: TComplexRec;
begin
  _Read(ComplexRec);
  Result := VarComplexCreate(ComplexRec.RealPart, ComplexRec.ImaginaryPart);
end;

function TComplexList.Add(New: TComplex): Integer;
var
  ComplexRec: TComplexRec;
begin
  ComplexRec.RealPart := New.Real;
  ComplexRec.ImaginaryPart := New.Imaginary;
  Result := _Add(ComplexRec);
end;

function TComplexList.Insert(Index: Integer; New: TComplex): Integer;
var
  ComplexRec: TComplexRec;
begin
  ComplexRec.RealPart := New.Real;
  ComplexRec.ImaginaryPart := New.Imaginary;
  Result := _Insert(Index, ComplexRec);
end;

function TComplexList.Delete(Index: Integer): TComplex;
var
  ComplexRec: TComplexRec;
begin
  _Delete(Index, ComplexRec);
  Result := VarComplexCreate(ComplexRec.RealPart, ComplexRec.ImaginaryPart);
end;

{$ENDREGION}

{$REGION 'Classe TPolynom'}

{-----------------}
{ Classe TPolynom }
{-----------------}

{$REGION 'Constructeurs et destructeur'}

constructor TPolynom.Create;
begin
  inherited Create;
  FCoefficients := TExtendedList.Create;
  FMinDegree := 0;
end;

constructor TPolynom.CreateAssign(Source: TPolynom);
begin
  Create;
  Assign(Source);
end;

constructor TPolynom.CreateFromExtended(Exponent: Integer;
  Coefficient: Extended);
begin
  Create;
  Coefficients[Exponent] := Coefficient;
end;

constructor TPolynom.CreateFromStream(Stream: TStream);
begin
  Create;
  LoadFromStream(Stream);
end;

destructor TPolynom.Destroy;
begin
  FCoefficients.Free;
  inherited Destroy;
end;

{$ENDREGION}

function TPolynom.GetMinDegree: Integer;
begin
  Result := FMinDegree;
end;

function TPolynom.GetMaxDegree: Integer;
begin
  Result := FMinDegree+FCoefficients.Count-1;
end;

function TPolynom.GetNegDegree: Integer;
begin
  Result := Min(MinDegree, 0);
end;

function TPolynom.GetPosDegree: Integer;
begin
  Result := Max(MaxDegree, 0);
end;

function TPolynom.GetIsZero: Boolean;
begin
  Result := FCoefficients.Count = 0;
end;

function TPolynom.GetIsExtended: Boolean;
begin
  Result := (FCoefficients.Count <= 1) and (MinDegree = 0);
end;

function TPolynom.GetIsInteger: Boolean;
begin
  Result := IsExtended and ScMaths.IsInteger(AsExtended);
end;

function TPolynom.GetIsNatural: Boolean;
begin
  Result := IsInteger and ScMaths.IsNatural(AsInteger);
end;

function TPolynom.GetCoefficients(Exponent: Integer): Extended;
begin
  if IsZero or (Exponent > MaxDegree) or (Exponent < MinDegree) then
    Result := 0
  else
    Result := FCoefficients[Exponent-MinDegree];
end;

function TPolynom.GetAsExtended: Extended;
begin
  if not IsExtended then
    raise ENotDegreeZeroError.CreateNotDegreeZero(Self);
  if IsZero then
    Result := 0
  else
    Result := FCoefficients[0];
end;

function TPolynom.GetAsInteger: Int64;
begin
  Result := ScMaths.AsInteger(AsExtended);
end;

function TPolynom.GetAsNatural: Int64;
begin
  Result := ScMaths.AsNatural(AsInteger);
end;

procedure TPolynom.SetCoefficients(Exponent: Integer; New: Extended);
begin
  if (New < MinValueEver) and (New > -MinValueEver) then
    New := 0.0;

  // Si New a déjà la valeur de cet exposant, on termine
  if New = Coefficients[Exponent] then
    Exit;
  if New = 0 then
  begin
    // Si New = 0 et que New <> Coefficients[Exponent], c'est que
    // Coefficients[Exponent] <> 0 et donc FCoefficients[Exponent-MinDegree]
    // existe
    FCoefficients[Exponent-MinDegree] := 0;
    // On supprime tous les 0 au début de FCoefficients
    while (FCoefficients.Count > 0) and (FCoefficients[0] = 0) do
    begin
      FCoefficients.Delete(0);
      Inc(FMinDegree);
    end;
    // On supprime tous les 0 à la fin de FCoefficients
    while (FCoefficients.Count > 0) and
      (FCoefficients[FCoefficients.Count-1] = 0) do
      FCoefficients.Delete(FCoefficients.Count-1);
    // Si FCoefficients est vide, on remet FMinDegree à 0
    if FCoefficients.Count = 0 then
      FMinDegree := 0;
  end else
  begin
    if FCoefficients.Count = 0 then
    begin
      // Si FCoefficients = 0, il suffit d'ajouter l'élément et de placer
      // MinDegree à Exponent
      FCoefficients.Add(New);
      FMinDegree := Exponent;
    end else
    begin
      // On ajoute des 0 éventuels au début de FCoefficients
      while Exponent < MinDegree do
      begin
        FCoefficients.Insert(0, 0);
        Dec(FMinDegree);
      end;
      // On ajoute des 0 éventuels à la fin de FCoefficients
      while Exponent > MaxDegree do
        FCoefficients.Add(0);
      // On défini la nouvelle valeur
      FCoefficients[Exponent-MinDegree] := New;
    end;
  end;
end;

procedure TPolynom.SetAsExtended(New: Extended);
begin
  if (New < MinValueEver) and (New > -MinValueEver) then
    FCoefficients.Count := 0
  else
  begin
    FCoefficients.Clear;
    FCoefficients.Add(New);
    FMinDegree := 0;
  end;
end;

procedure TPolynom.SetAsInteger(New: Int64);
begin
  AsExtended := New;
end;

procedure TPolynom.SetToZero;
begin
  FCoefficients.Clear;
  FMinDegree := 0;
end;

procedure TPolynom.Assign(Source: TPolynom);
begin
  FCoefficients.Assign(Source.FCoefficients);
  FMinDegree := Source.FMinDegree;
end;

function TPolynom.ToString: string;
var
  I: Integer;
begin
  if IsZero then
    Result := '0'
  else
  begin
    Result := '';
    for I := MaxDegree downto MinDegree do
    begin
      if Coefficients[I] <> 0 then
      begin
        if (I = 0) or (Coefficients[I] <> 1) then
        begin
          // Si le degré est 0 ou si le coefficient n'est pas 1,
          // il faut d'abord indiquer le coefficient
          if Coefficients[I] < 0 then
            Result := Result+FloatToStr(Coefficients[I])
          else
            Result := Result + '+' + FloatToStr(Coefficients[I]);
        end else
        begin
          // Sinon il suffit d'ajouter un +
          if Coefficients[I] >= 0 then
            Result := Result + '+';
          case I of
            0: ;                      // Rien à ajouter (pas de x)
            1: Result := Result+'x';  // Coefficient 1, donc x
            2: Result := Result+'x²'; // Coefficient 2, donc x²
            3: Result := Result+'x³'; // Coefficient 3, donc x³
          else
            // Dans les autres cas : (x^i)
            Result := Result+
              IIF(Coefficients[I] = 1, '', '(')+
              'x^'+IntToStr(I)+
              IIF(Coefficients[I] = 1, '', ')');
          end;
        end;
      end;
    end;

    // Si le premier caractère de la chaîne est un +, on le supprime, il ne
    // sert à rien. Remarquez que la chaîne a toujours au moins un caractère,
    // car le contenu de la boucle est toujours exécuté au moins une fois.
    if Result[1] = '+' then
      Delete(Result, 1, 1);
  end;
end;

procedure TPolynom.LoadFromStream(Stream: TStream);
begin
  SetToZero;
  Stream.Read(FMinDegree, SizeOf(Integer));
  FCoefficients.LoadFromStream(Stream);
end;

procedure TPolynom.SaveToStream(Stream: TStream);
begin
  Stream.Write(FMinDegree, SizeOf(Integer));
  FCoefficients.SaveToStream(Stream);
end;

procedure TPolynom.Add(Source: TPolynom);
var
  I: Integer;
begin
  for I := Source.MinDegree to Source.MaxDegree do
    Coefficients[I] := Coefficients[I]+Source[I];
end;

procedure TPolynom.Sub(Source: TPolynom);
var
  I: Integer;
begin
  for I := Source.MinDegree to Source.MaxDegree do
    Coefficients[I] := Coefficients[I]-Source[I];
end;

procedure TPolynom.Multiply(Source: TPolynom);
var
  I, J: Integer;
  Resultat: TPolynom;
begin
  Resultat := TPolynom.Create;
  for I := MinDegree to MaxDegree do
    for J := Source.MinDegree to Source.MaxDegree do
      Resultat[I+J] := Resultat[I+J] + Coefficients[I]*Source[J];
  Assign(Resultat);
  Resultat.Free;
end;

procedure TPolynom.Divide(Source: TPolynom);
var
  Temp, Reste, Resultat: TPolynom;
  Result: Extended;
  BeginTime, MaxTime: TDateTime;
  DegreReste, DegreSource: Integer;
begin
  // On vérifie que la source n'est pas 0
  if Source.IsZero then
    raise EDivisionError.CreateDivision(Self, Source);
  // Si dividende = 0 -> on termine tout de suite
  if IsZero then
    Exit;

  // On récupère le temps actuel
  BeginTime := Now;
  // On définit le temps maximal
  MaxTime := EncodeTime(0, 0, 15, 0);

  Temp := TPolynom.Create;
  Reste := TPolynom.CreateAssign(Self);
  Resultat := TPolynom.Create;

  try
    DegreSource := Source.MaxDegree;
    while not Reste.IsZero do
    begin
      // On vérifie qu'on a pas dépassé le temps imparti
      if Now-BeginTime > MaxTime then
        raise EDivisionError.CreateDivision(Self, Source);

      // On applique le principe de division de polynomes
      DegreReste := Reste.MaxDegree;
      Result := Reste[DegreReste]/Source[DegreSource];
      Resultat[DegreReste-DegreSource] := Result;
      Temp.SetToZero;
      Temp[DegreReste-DegreSource] := Result;
      Temp.Multiply(Source);
      Reste.Sub(Temp);
    end;
    Assign(Resultat);
  finally
    Temp.Free;
    Reste.Free;
    Resultat.Free;
  end;
end;

procedure TPolynom.Modulo(Source: TPolynom);
begin
  AsInteger := AsInteger mod Source.AsInteger;
end;

procedure TPolynom.Power(Source: TPolynom);
var
  Exposant: Integer;
  NegExp: Boolean;
  Temp: TPolynom;
begin
  if IsExtended then
    AsExtended := Math.Power(AsExtended, Source.AsExtended)
  else
  begin
    // On récupère l'exposant
    Exposant := Source.AsInteger;

    // On prend la val abs de l'exposant mais on conserve
    // un boolean indiquant si uk était négatif
    NegExp := Exposant < 0;
    if NegExp then
      Exposant := -Exposant;

    Temp := TPolynom.CreateAssign(Self);
    try
      SetToZero;
      Coefficients[0] := 1;
      while Exposant > 0 do
      begin
        Multiply(Temp);
        Dec(Exposant);
      end;
      if NegExp then
        Reverse;
    finally
      Temp.Free;
    end;
  end;
end;

procedure TPolynom.Root(Source: TPolynom);
begin
  try
    AsExtended := Math.Power(AsExtended, 1/Source.AsExtended);
  except
    on Error: EInvalidOp do
      raise ERootError.CreateRoot(Source, Self);
    on Error: EZeroDivide do
      raise ERootError.CreateRoot(Source, Self);
  end;
end;

procedure TPolynom.Insert(Source: TPolynom);
var
  Deg: Integer;
  Coeff: Extended;
  Result, Temp, Scalar: TPolynom;
begin
  Result := TPolynom.Create;
  try
    Temp := TPolynom.Create;
    try
      Scalar := TPolynom.Create;
      try
        for Deg := MinDegree to MaxDegree do
        begin
          Coeff := Coefficients[Deg];
          if Coeff = 0 then
            Continue;
          Temp.Assign(Source);
          Scalar.AsInteger := Deg;
          Temp.Power(Scalar);
          Scalar.AsExtended := Coeff;
          Temp.Multiply(Scalar);
          Result.Add(Temp);
        end;
      finally
        Scalar.Free;
      end;
    finally
      Temp.Free;
    end;

    Assign(Result);
  finally
    Result.Free;
  end;
end;

procedure TPolynom.ScalarProd(Source: TPolynom);
begin
  AsExtended := ScalarProdFunc(Self, Source);
end;

procedure TPolynom.Oppose;
var
  I: Integer;
begin
  for I := 0 to FCoefficients.Count-1 do
    FCoefficients[I] := -FCoefficients[I];
end;

procedure TPolynom.Reverse;
var
  Resultat: TPolynom;
begin
  Resultat := TPolynom.CreateFromExtended(0, 1);
  try
    Resultat.Divide(Self);
    Assign(Resultat);
  finally
    Resultat.Free;
  end;
end;

procedure TPolynom.Abs;
begin
  AsExtended := System.Abs(AsExtended);
end;

procedure TPolynom.Sinus;
begin
  AsExtended := Sin(AsExtended);
end;

procedure TPolynom.Cosinus;
begin
  AsExtended := Cos(AsExtended);
end;

procedure TPolynom.Tangent;
begin
  AsExtended := Tan(AsExtended);
end;

procedure TPolynom.ArcSinus;
begin
  AsExtended := Sinh(AsExtended);
end;

procedure TPolynom.ArcCosinus;
begin
  AsExtended := Cosh(AsExtended);
end;

procedure TPolynom.ArcTangent;
begin
  AsExtended := Tanh(AsExtended);
end;

procedure TPolynom.Factorial;
begin
  AsNatural := ScMaths.Factorial(AsNatural);
end;

class function TPolynom.Addition(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Add(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.Substraction(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Sub(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.Multiplication(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Multiply(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.Division(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Divide(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.ModuloOf(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Modulo(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.PowerOf(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Power(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.RootOf(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Root(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.InsertOf(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.Insert(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.ScalarProdOf(Val1, Val2: TPolynom;
  ReleaseVals: Boolean = False): TPolynom;
begin
  Result := CreateAssign(Val1);
  Result.ScalarProd(Val2);
  if not ReleaseVals then
    Exit;
  Val1.Free;
  Val2.Free;
end;

class function TPolynom.OpposedOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Oppose;
end;

class function TPolynom.ReversedOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Reverse;
end;

class function TPolynom.AbsOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Abs;
end;

class function TPolynom.SinusOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Sinus;
end;

class function TPolynom.CosinusOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Cosinus;
end;

class function TPolynom.TangentOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Tangent;
end;

class function TPolynom.ArcSinusOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.ArcSinus;
end;

class function TPolynom.ArcCosinusOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.ArcCosinus;
end;

class function TPolynom.ArcTangentOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.ArcTangent;
end;

class function TPolynom.FactorialOf(Value: TPolynom;
  ReleaseValue: Boolean = False): TPolynom;
begin
  if ReleaseValue then
    Result := Value
  else
    Result := CreateAssign(Value);
  Result.Factorial;
end;

{$ENDREGION}

{$REGION 'Classe TPolynomList'}

{---------------------}
{ Classe TPolynomList }
{---------------------}

constructor TPolynomList.Create(AOwnsObjects: Boolean = False);
begin
  inherited Create;
  FPolynoms := TObjectList.Create(AOwnsObjects);
end;

constructor TPolynomList.CreateFromStream(Stream: TStream;
  AOwnsObjects: Boolean = False);
begin
  Create(AOwnsObjects);
  LoadFromStream(Stream);
end;

constructor TPolynomList.CreateFromFile(FileName: string;
  AOwnsObjects: Boolean = False);
begin
  Create(AOwnsObjects);
  LoadFromFile(FileName);
end;

destructor TPolynomList.Destroy;
begin
  FPolynoms.Free;
  inherited Destroy;
end;

function TPolynomList.GetCount: Integer;
begin
  Result := FPolynoms.Count;
end;

function TPolynomList.GetOwnsObjects: Boolean;
begin
  Result := FPolynoms.OwnsObjects;
end;

function TPolynomList.GetPolynoms(Index: Integer): TPolynom;
begin
  Result := TPolynom(FPolynoms[Index]);
end;

procedure TPolynomList.SetOwnsObjects(New: Boolean);
begin
  FPolynoms.OwnsObjects := New;
end;

procedure TPolynomList.SetPolynoms(Index: Integer; New: TPolynom);
begin
  FPolynoms[Index] := New;
end;

procedure TPolynomList.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  Clear;
  Stream.Read(I, SizeOf(Integer));
  while I > 0 do
  begin
    Add(TPolynom.CreateFromStream(Stream));
    Dec(I);
  end;
end;

procedure TPolynomList.LoadFromFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TPolynomList.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  I := Count;
  Stream.Write(I, SizeOf(Integer));
  for I := 0 to Count-1 do
    Polynoms[I].SaveToStream(Stream);
end;

procedure TPolynomList.SaveToFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TPolynomList.Clear;
begin
  FPolynoms.Clear;
end;

function TPolynomList.Add(New: TPolynom): Integer;
begin
  Result := FPolynoms.Add(New);
end;

function TPolynomList.Insert(Index: Integer; New: TPolynom): Integer;
begin
  FPolynoms.Insert(Index, New);
  Result := Index;
end;

procedure TPolynomList.Delete(Index: Integer);
begin
  FPolynoms.Delete(Index);
end;

function TPolynomList.Extract(Polynom: TPolynom): TPolynom;
begin
  Result := TPolynom(Self.FPolynoms.Extract(Polynom));
end;

{$ENDREGION}

{$REGION 'Fonction Eval et sous-fonctions'}

{--------------------------------------}
{ Fonctions pour Eval et fonction Eval }
{--------------------------------------}

const
  EvalChars = MonomsChars+BinaryOperators+UnaryOperators+[' ', '(', ')'];

function ExecBinaire(Operation: Char;
  Operande1, Operande2: TPolynom): TPolynom;
begin
  try
    case Operation of
      '+': Result := TPolynom.Addition(Operande1, Operande2, True);
      '-': Result := TPolynom.Substraction(Operande1, Operande2, True);
      '*': Result := TPolynom.Multiplication(Operande1, Operande2, True);
      '/': Result := TPolynom.Division(Operande1, Operande2, True);
      '%': Result := TPolynom.ModuloOf(Operande1, Operande2, True);
      '^': Result := TPolynom.PowerOf(Operande1, Operande2, True);
      'r': Result := TPolynom.RootOf(Operande1, Operande2, True);
      'o': Result := TPolynom.InsertOf(Operande1, Operande2, True);
      '|': Result := TPolynom.ScalarProdOf(Operande1, Operande2, True);
    else
      raise EOpNotExistsError.CreateOpNotExists(Operation);
    end;
  except
    on Error: ENotDegreeZeroError do
      raise EOpRequestsDegreeZeroError.CreateOpRequestsDegreeZero(
        Operation, Error.Value);
    on Error: ENotIntegerError do
      raise EOpRequestsIntegerError.CreateOpRequestsInteger(
        Operation, Error.Value);
    on Error: EDivisionError do
      raise EDivisionOpError.CreateDivisionOp(Error.Dividend, Error.Divisor);
    on Error: ERootError do
      raise ERootOpError.CreateRootOp(Error.Degree, Error.Radican);
  end;
end;

function ExecUnaire(Operation: Char; Operande: TPolynom): TPolynom;
begin
  try
    case Operation of
      '­': Result := TPolynom.OpposedOf(Operande, True);
      'a': Result := TPolynom.AbsOf(Operande, True);
      's': Result := TPolynom.SinusOf(Operande, True);
      'c': Result := TPolynom.CosinusOf(Operande, True);
      't': Result := TPolynom.TangentOf(Operande, True);
      'S': Result := TPolynom.ArcSinusOf(Operande, True);
      'C': Result := TPolynom.ArcCosinusOf(Operande, True);
      'T': Result := TPolynom.ArcTangentOf(Operande, True);
      '²': Result := TPolynom.PowerOf(Operande,
          TPolynom.CreateFromExtended(0, 2), True);
      '\': Result := TPolynom.RootOf(Operande,
          TPolynom.CreateFromExtended(0, 2), True);
      '!': Result := TPolynom.FactorialOf(Operande, True);
      'v': Result := TPolynom.CreateAssign(VarsLists[0][Operande.AsInteger]);
      'w': Result := TPolynom.CreateAssign(VarsLists[1][Operande.AsInteger]);
    else
      raise EOpNotExistsError.CreateOpNotExists(Operation);
    end;
  except
    on Error: ENotDegreeZeroError do
      raise EOpRequestsDegreeZeroError.CreateOpRequestsDegreeZero(
        Operation, Error.Value);
    on Error: ENotIntegerError do
      raise EOpRequestsIntegerError.CreateOpRequestsInteger(
        Operation, Error.Value);
    on Error: ENotNaturalError do
      raise EOpRequestsNaturalError.CreateOpRequestsNatural(
        Operation, Error.Value);
    on Error: ERootError do
      raise ERootOpError.CreateRootOp(Error.Degree, Error.Radican);
    on Error: EListError do
      raise EOpRequestsCorrectIndexError.CreateOpRequestsCorrectIndex(
        Operation, AsInteger(Operande.AsExtended));
  end;
end;

function ExtractFrom(Str: string; Debut, Fin: Integer): string;
begin
  Result := Copy(Str, Debut, Fin-Debut+1);
end;

function Priorite(Operation: Char): Byte;
begin
  case Operation of
    '|': Result := 5;
    '+', '-': Result := 4;
    '*', '/', '%': Result := 3;
    '^', 'r', 'o': Result := 2;
    '­', 'a', 's', 'c', 't', 'S', 'C', 'T', '²', '\', '!', 'v',
    'w': Result := 1;
  else
    Result := 0;
  end;
end;

function IndexOperation(Str: string): Integer;
var
  I: Byte;
  NiveauParentheses: Integer;
  MaxPriorite: Byte;
begin
  NiveauParentheses := 0;
  MaxPriorite := 0;
  Result := 0;
  for I := Length(Str) downto 1 do
  begin
    if (NiveauParentheses = 0) and (Priorite(Str[I]) > MaxPriorite) then
    begin
      Result := I;
      MaxPriorite := Priorite(Str[I]);
    end else if Str[I] = '(' then
      Dec(NiveauParentheses)
    else if Str[I] = ')' then
      Inc(NiveauParentheses);
  end;
  if NiveauParentheses < 0 then
    raise ETooManyBracketsError.CreateTooManyBrackets(-NiveauParentheses, '(');
  if NiveauParentheses > 0 then
    raise ETooManyBracketsError.CreateTooManyBrackets(NiveauParentheses, ')');
  if (Result > 1) and (not CharInSet(Str[Result], BinaryOperators)) then
    raise EOpNotExistsError.CreateOpIsNotBinary(Str[Result]);
  if (Result = 1) and (not CharInSet(Str[Result], UnaryOperators)) then
    raise EOpNotExistsError.CreateOpIsNotUnary(Str[Result]);
  if Result = 0 then
    raise EWrongExpressionError.CreateWrongExpression;
end;

function ExtremesApparies(Str: string): Boolean;
var
  NiveauParentheses: Integer;
  I: Integer;
begin
  NiveauParentheses := 0;
  Result := False;
  for I := 1 to Length(Str) do
  begin
    if Str[I] = '(' then
      Inc(NiveauParentheses)
    else if Str[I] = ')' then
      Dec(NiveauParentheses);
    if (I < Length(Str)) and (NiveauParentheses = 0) then
      Exit;
  end;
  Result := True;
end;

function ConvertToMonom(Str: string; var Resultat: TPolynom): Boolean;
var
  I, Degree: Integer;
  Coefficient: Extended;
begin
  try
    Result := False;
    if Pos(' ', Str) > 0 then
      Exit;
    if Pos('-', Str) > 0 then
      Exit;
    if Pos('+', Str) > 0 then
      Exit;
    if NberSubStr('x', Str) > 1 then
      Exit;
    for I := 1 to Length(Str) do
      if Str[I] = '­' then
        Str[I] := '-';
    if Pos('x', Str) = 1 then
      Coefficient := 1
    else
      Coefficient := StrToFloat(GetFirstToken(Str, 'x'));
    if NberSubStr('x', Str) = 0 then
      Degree := 0
    else if Pos('x', Str) = Length(Str) then
      Degree := 1
    else
      Degree := StrToInt(GetLastToken(Str, 'x'));
    Resultat := TPolynom.CreateFromExtended(Degree, Coefficient);
    Result := True;
  except
    on Error: EConvertError do
      Result := False;
  end;
end;

function Eval(Expression: string;
  TestIsAborted: TTestIsAbortedProc = nil): TPolynom;
var
  I: Integer;
  Operande1, Operande2: TPolynom;
begin
  Operande1 := nil;
  Operande2 := nil;
  try
    // On vérifie que Exoression contient uniqument des caractères corrects
    for I := 1 to Length(Expression) do
      if not CharInSet(Expression[I], EvalChars) then
        raise EWrongCharacterError.CreateWrongCharacter(Expression[I]);

    Expression := Trim(Expression);

    // On supprime toutes les parenthèses superflues aux extrémités
    while (Length(Expression) >= 2) and (Expression[1] = '(') and
      (Expression[Length(Expression)] = ')') and
      ExtremesApparies(Expression) do
    begin
      Expression := ExtractFrom(Expression, 2, Length(Expression)-1);
      Expression := Trim(Expression);
    end;

    if Expression = '' then
      raise EWrongExpressionError.CreateWrongExpression;

    if Assigned(TestIsAborted) then
      TestIsAborted;

    if ConvertToMonom(Expression, Result) then
      I := 0
    else
      I := IndexOperation(Expression);
  except
    on Error: EEvalError do
      raise Error.ClassType.CreateEval(Expression, Error.Message);
  end;

  if I > 0 then
  begin
    if I = 1 then
      Operande1 := Eval(ExtractFrom(Expression, 2, Length(Expression)),
        TestIsAborted)
    else
    begin
      Operande1 := Eval(ExtractFrom(Expression, 1, I-1), TestIsAborted);
      Operande2 := Eval(ExtractFrom(Expression, I+1, Length(Expression)),
        TestIsAborted);
    end;
  end;

  try
    if I = 1 then
      Result := ExecUnaire(Expression[I], Operande1)
    else if I > 1 then
      Result := ExecBinaire(Expression[I], Operande1, Operande2);
  except
    on Error: EEvalError do
      raise Error.ClassType.CreateEval(Expression, Error.Message);
  end;
end;

{$ENDREGION}

var
  I: Integer;
initialization
  I := 0;
  while I < VarsListsCount do
  begin
    VarsLists[I] := TPolynomList.Create;
    Inc(I);
  end;
  ScalarProdFunc := DefaultScalarProd;
finalization
  I := 0;
  while I < VarsListsCount do
  begin
    VarsLists[I].Free;
    Inc(I);
  end;
end.

