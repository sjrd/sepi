{*
  Importe l'unité ScMaths dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsScMaths;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, ScMaths, ScLists, Contnrs;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEMathsError = class(EMathsError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEDivisionError = class(EDivisionError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsERootError = class(ERootError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEDegreeError = class(EDegreeError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsENotDegreeZeroError = class(ENotDegreeZeroError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEIntegerError = class(EIntegerError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsENotIntegerError = class(ENotIntegerError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsENaturalError = class(ENaturalError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsENotNaturalError = class(ENotNaturalError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEEvalError = class(EEvalError)
  private
    constructor CreateEval_0(AExpression : string; AMessage : string);
    constructor CreateEval_1(AExpression : string; Format : string; Args : array of const);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEWrongExpressionError = class(EWrongExpressionError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEWrongCharacterError = class(EWrongCharacterError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOperationError = class(EOperationError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOpNotExistsError = class(EOpNotExistsError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOpRequestsError = class(EOpRequestsError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOpRequestsDegreeZeroError = class(EOpRequestsDegreeZeroError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOpRequestsIntegerError = class(EOpRequestsIntegerError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOpRequestsNaturalError = class(EOpRequestsNaturalError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEOpRequestsCorrectIndexError = class(EOpRequestsCorrectIndexError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEDivisionOpError = class(EDivisionOpError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsERootOpError = class(ERootOpError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEBracketsError = class(EBracketsError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsETooManyBracketsError = class(ETooManyBracketsError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEEquationError = class(EEquationError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsECannotSolveEquationError = class(ECannotSolveEquationError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEDegreeTooHighError = class(EDegreeTooHighError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEDegreeTooLowError = class(EDegreeTooLowError)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTComplexList = class(TComplexList)
  private
    function GetItems(Index : integer) : TComplex;
    procedure SetItems(Index : integer; New : TComplex);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPolynom = class(TPolynom)
  private
    function GetMinDegree: integer;
    function GetMaxDegree: integer;
    function GetNegDegree: integer;
    function GetPosDegree: integer;
    function GetIsZero: boolean;
    function GetIsExtended: boolean;
    function GetIsInteger: boolean;
    function GetIsNatural: boolean;
    function GetCoefficients(Exponent : integer) : Extended;
    function GetAsExtended: Extended;
    function GetAsInteger: Int64;
    function GetAsNatural: Int64;
    procedure SetCoefficients(Exponent : integer; New : Extended);
    procedure SetAsExtended(New : Extended);
    procedure SetAsInteger(New : Int64);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPolynomList = class(TPolynomList)
  private
    function GetCount: integer;
    function GetOwnsObjects: boolean;
    function GetPolynoms(Index : integer) : TPolynom;
    procedure SetOwnsObjects(New : boolean);
    procedure SetPolynoms(Index : integer; New : TPolynom);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{--------------------}
{ EMathsError import }
{--------------------}

class function TSepiImportsEMathsError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EMathsError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateMaths', @TSepiImportsEMathsError.CreateMaths,
      'constructor');

    Complete;
  end;
end;

{-----------------------}
{ EDivisionError import }
{-----------------------}

class function TSepiImportsEDivisionError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDivisionError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Dividend', System.TypeInfo(TPolynom));
    AddField('Divisor', System.TypeInfo(TPolynom), True);

    AddMethod('CreateDivision', @TSepiImportsEDivisionError.CreateDivision,
      'constructor(ADividend, ADivisor : TPolynom)');
    AddMethod('Destroy', @TSepiImportsEDivisionError.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{-------------------}
{ ERootError import }
{-------------------}

class function TSepiImportsERootError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ERootError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Degree', System.TypeInfo(TPolynom));
    AddField('Radican', System.TypeInfo(TPolynom), True);

    AddMethod('CreateRoot', @TSepiImportsERootError.CreateRoot,
      'constructor(ADegree, ARadican : TPolynom)');
    AddMethod('Destroy', @TSepiImportsERootError.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ EDegreeError import }
{---------------------}

class function TSepiImportsEDegreeError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDegreeError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Value', System.TypeInfo(TPolynom));

    AddMethod('CreateDegree', @TSepiImportsEDegreeError.CreateDegree,
      'constructor(AValue : TPolynom)');
    AddMethod('Destroy', @TSepiImportsEDegreeError.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{----------------------------}
{ ENotDegreeZeroError import }
{----------------------------}

class function TSepiImportsENotDegreeZeroError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ENotDegreeZeroError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateNotDegreeZero', @TSepiImportsENotDegreeZeroError.CreateNotDegreeZero,
      'constructor(AValue : TPolynom)');

    Complete;
  end;
end;

{----------------------}
{ EIntegerError import }
{----------------------}

class function TSepiImportsEIntegerError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EIntegerError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Value', System.TypeInfo(Extended));

    AddMethod('CreateInteger', @TSepiImportsEIntegerError.CreateInteger,
      'constructor(AValue : Extended)');

    Complete;
  end;
end;

{-------------------------}
{ ENotIntegerError import }
{-------------------------}

class function TSepiImportsENotIntegerError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ENotIntegerError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateNotInteger', @TSepiImportsENotIntegerError.CreateNotInteger,
      'constructor(AValue : Extended)');

    Complete;
  end;
end;

{----------------------}
{ ENaturalError import }
{----------------------}

class function TSepiImportsENaturalError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ENaturalError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Value', System.TypeInfo(Int64));

    AddMethod('CreateNatural', @TSepiImportsENaturalError.CreateNatural,
      'constructor(AValue : Int64)');

    Complete;
  end;
end;

{-------------------------}
{ ENotNaturalError import }
{-------------------------}

class function TSepiImportsENotNaturalError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ENotNaturalError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateNotNatural', @TSepiImportsENotNaturalError.CreateNotNatural,
      'constructor(AValue : Int64)');

    Complete;
  end;
end;

{-------------------}
{ EEvalError import }
{-------------------}

constructor TSepiImportsEEvalError.CreateEval_0(AExpression : string; AMessage : string);
begin
  CreateEval(AExpression, AMessage);
end;

constructor TSepiImportsEEvalError.CreateEval_1(AExpression : string; Format : string; Args : array of const);
begin
  CreateEval(AExpression, Format, Args);
end;

class function TSepiImportsEEvalError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('EEvalError'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(EEvalError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Expression', System.TypeInfo(string));

    AddOverloadedMethod('CreateEval', @TSepiImportsEEvalError.CreateEval_0,
      'constructor(AExpression : string; AMessage : string)');
    AddOverloadedMethod('CreateEval', @TSepiImportsEEvalError.CreateEval_1,
      'constructor(AExpression : string; Format : string; Args : array of const)');
    AddMethod('ClassType', @TSepiImportsEEvalError.ClassType,
      'function: EEvalErrorClass');

    Complete;
  end;
end;

{------------------------------}
{ EWrongExpressionError import }
{------------------------------}

class function TSepiImportsEWrongExpressionError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EWrongExpressionError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateWrongExpression', @TSepiImportsEWrongExpressionError.CreateWrongExpression,
      'constructor');

    Complete;
  end;
end;

{-----------------------------}
{ EWrongCharacterError import }
{-----------------------------}

class function TSepiImportsEWrongCharacterError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EWrongCharacterError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Character', System.TypeInfo(Char));

    AddMethod('CreateWrongCharacter', @TSepiImportsEWrongCharacterError.CreateWrongCharacter,
      'constructor(ACharacter : Char)');

    Complete;
  end;
end;

{------------------------}
{ EOperationError import }
{------------------------}

class function TSepiImportsEOperationError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOperationError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Operation', System.TypeInfo(string));

    AddMethod('CreateOperation', @TSepiImportsEOperationError.CreateOperation,
      'constructor(AOperation : string)');

    Complete;
  end;
end;

{--------------------------}
{ EOpNotExistsError import }
{--------------------------}

class function TSepiImportsEOpNotExistsError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOpNotExistsError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateOpNotExists', @TSepiImportsEOpNotExistsError.CreateOpNotExists,
      'constructor(AOperation : string)');
    AddMethod('CreateOpIsNotBinary', @TSepiImportsEOpNotExistsError.CreateOpIsNotBinary,
      'constructor(AOperation : string)');
    AddMethod('CreateOpIsNotUnary', @TSepiImportsEOpNotExistsError.CreateOpIsNotUnary,
      'constructor(AOperation : string)');

    Complete;
  end;
end;

{-------------------------}
{ EOpRequestsError import }
{-------------------------}

class function TSepiImportsEOpRequestsError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOpRequestsError));

  with Result do
  begin

    Complete;
  end;
end;

{-----------------------------------}
{ EOpRequestsDegreeZeroError import }
{-----------------------------------}

class function TSepiImportsEOpRequestsDegreeZeroError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOpRequestsDegreeZeroError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Value', System.TypeInfo(TPolynom));

    AddMethod('CreateOpRequestsDegreeZero', @TSepiImportsEOpRequestsDegreeZeroError.CreateOpRequestsDegreeZero,
      'constructor(AOperation : string; AValue : TPolynom)');
    AddMethod('Destroy', @TSepiImportsEOpRequestsDegreeZeroError.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{--------------------------------}
{ EOpRequestsIntegerError import }
{--------------------------------}

class function TSepiImportsEOpRequestsIntegerError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOpRequestsIntegerError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Value', System.TypeInfo(Extended));

    AddMethod('CreateOpRequestsInteger', @TSepiImportsEOpRequestsIntegerError.CreateOpRequestsInteger,
      'constructor(AOperation : string; AValue : Extended)');

    Complete;
  end;
end;

{--------------------------------}
{ EOpRequestsNaturalError import }
{--------------------------------}

class function TSepiImportsEOpRequestsNaturalError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOpRequestsNaturalError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Value', System.TypeInfo(Int64));

    AddMethod('CreateOpRequestsNatural', @TSepiImportsEOpRequestsNaturalError.CreateOpRequestsNatural,
      'constructor(AOperation : string; AValue : Int64)');

    Complete;
  end;
end;

{-------------------------------------}
{ EOpRequestsCorrectIndexError import }
{-------------------------------------}

class function TSepiImportsEOpRequestsCorrectIndexError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EOpRequestsCorrectIndexError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Index', System.TypeInfo(integer));

    AddMethod('CreateOpRequestsCorrectIndex', @TSepiImportsEOpRequestsCorrectIndexError.CreateOpRequestsCorrectIndex,
      'constructor(AOperation : string; AIndex : integer)');

    Complete;
  end;
end;

{-------------------------}
{ EDivisionOpError import }
{-------------------------}

class function TSepiImportsEDivisionOpError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDivisionOpError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Dividend', System.TypeInfo(TPolynom));
    AddField('Divisor', System.TypeInfo(TPolynom), True);

    AddMethod('CreateDivisionOp', @TSepiImportsEDivisionOpError.CreateDivisionOp,
      'constructor(ADividend, ADivisor : TPolynom)');
    AddMethod('Destroy', @TSepiImportsEDivisionOpError.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{---------------------}
{ ERootOpError import }
{---------------------}

class function TSepiImportsERootOpError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ERootOpError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('Degree', System.TypeInfo(TPolynom));
    AddField('Radican', System.TypeInfo(TPolynom), True);

    AddMethod('CreateRootOp', @TSepiImportsERootOpError.CreateRootOp,
      'constructor(ADegree, ARadican : TPolynom)');
    AddMethod('Destroy', @TSepiImportsERootOpError.Destroy,
      'destructor',
      mlkOverride);

    Complete;
  end;
end;

{-----------------------}
{ EBracketsError import }
{-----------------------}

class function TSepiImportsEBracketsError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EBracketsError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('CreateBrackets', @TSepiImportsEBracketsError.CreateBrackets,
      'constructor');

    Complete;
  end;
end;

{------------------------------}
{ ETooManyBracketsError import }
{------------------------------}

class function TSepiImportsETooManyBracketsError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ETooManyBracketsError));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddField('HowTooMany', System.TypeInfo(integer));
    AddField('WhichBracket', System.TypeInfo(Char));

    AddMethod('CreateTooManyBrackets', @TSepiImportsETooManyBracketsError.CreateTooManyBrackets,
      'constructor(AHowTooMany : integer; AWhichBracket : Char)');

    Complete;
  end;
end;

{-----------------------}
{ EEquationError import }
{-----------------------}

class function TSepiImportsEEquationError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EEquationError));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------------------}
{ ECannotSolveEquationError import }
{----------------------------------}

class function TSepiImportsECannotSolveEquationError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(ECannotSolveEquationError));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------------}
{ EDegreeTooHighError import }
{----------------------------}

class function TSepiImportsEDegreeTooHighError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDegreeTooHighError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------------}
{ EDegreeTooLowError import }
{---------------------------}

class function TSepiImportsEDegreeTooLowError.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EDegreeTooLowError));

  with Result do
  begin

    Complete;
  end;
end;

{---------------------}
{ TComplexList import }
{---------------------}

function TSepiImportsTComplexList.GetItems(Index : integer) : TComplex;
begin
  Result := Items[Index];
end;

procedure TSepiImportsTComplexList.SetItems(Index : integer; New : TComplex);
begin
  Items[Index] := New;
end;

class function TSepiImportsTComplexList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TComplexList'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TComplexList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddMethod('GetItems', @TSepiImportsTComplexList.GetItems,
      'function(Index : integer) : TComplex');
    AddMethod('SetItems', @TSepiImportsTComplexList.SetItems,
      'procedure(Index : integer; New : TComplex)');

    CurrentVisibility := mvProtected;

    AddMethod('AssignTo', @TSepiImportsTComplexList.AssignTo,
      'procedure(Dest : TPersistent)',
      mlkOverride);
    AddMethod('IsAssignClass', @TSepiImportsTComplexList.IsAssignClass,
      'function(ScListClass : TScListClass) : boolean',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTComplexList.Create,
      'constructor');
    AddMethod('Assign', @TSepiImportsTComplexList.Assign,
      'procedure(Source : TPersistent)',
      mlkOverride);
    AddMethod('Write', @TSepiImportsTComplexList.Write,
      'procedure(New : TComplex)');
    AddMethod('Read', @TSepiImportsTComplexList.Read,
      'function: TComplex');
    AddMethod('Add', @TSepiImportsTComplexList.Add,
      'function(New : TComplex) : integer');
    AddMethod('Insert', @TSepiImportsTComplexList.Insert,
      'function(Index : integer; New : TComplex) : integer');
    AddMethod('Delete', @TSepiImportsTComplexList.Delete,
      'function(Index : integer) : TComplex');

    AddProperty('Items', 'property[index : integer] : TComplex',
      'GetItems', 'SetItems', True);

    Complete;
  end;
end;

{-----------------}
{ TPolynom import }
{-----------------}

function TSepiImportsTPolynom.GetMinDegree: integer;
begin
  Result := MinDegree;
end;

function TSepiImportsTPolynom.GetMaxDegree: integer;
begin
  Result := MaxDegree;
end;

function TSepiImportsTPolynom.GetNegDegree: integer;
begin
  Result := NegDegree;
end;

function TSepiImportsTPolynom.GetPosDegree: integer;
begin
  Result := PosDegree;
end;

function TSepiImportsTPolynom.GetIsZero: boolean;
begin
  Result := IsZero;
end;

function TSepiImportsTPolynom.GetIsExtended: boolean;
begin
  Result := IsExtended;
end;

function TSepiImportsTPolynom.GetIsInteger: boolean;
begin
  Result := IsInteger;
end;

function TSepiImportsTPolynom.GetIsNatural: boolean;
begin
  Result := IsNatural;
end;

function TSepiImportsTPolynom.GetCoefficients(Exponent : integer) : Extended;
begin
  Result := Coefficients[Exponent];
end;

function TSepiImportsTPolynom.GetAsExtended: Extended;
begin
  Result := AsExtended;
end;

function TSepiImportsTPolynom.GetAsInteger: Int64;
begin
  Result := AsInteger;
end;

function TSepiImportsTPolynom.GetAsNatural: Int64;
begin
  Result := AsNatural;
end;

procedure TSepiImportsTPolynom.SetCoefficients(Exponent : integer; New : Extended);
begin
  Coefficients[Exponent] := New;
end;

procedure TSepiImportsTPolynom.SetAsExtended(New : Extended);
begin
  AsExtended := New;
end;

procedure TSepiImportsTPolynom.SetAsInteger(New : Int64);
begin
  AsInteger := New;
end;

class function TSepiImportsTPolynom.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TPolynom'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TPolynom));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FCoefficients', System.TypeInfo(TExtendedList));
    AddField('FMinDegree', System.TypeInfo(integer));

    AddMethod('GetMinDegree', @TSepiImportsTPolynom.GetMinDegree,
      'function: integer');
    AddMethod('GetMaxDegree', @TSepiImportsTPolynom.GetMaxDegree,
      'function: integer');
    AddMethod('GetNegDegree', @TSepiImportsTPolynom.GetNegDegree,
      'function: integer');
    AddMethod('GetPosDegree', @TSepiImportsTPolynom.GetPosDegree,
      'function: integer');
    AddMethod('GetIsZero', @TSepiImportsTPolynom.GetIsZero,
      'function: boolean');
    AddMethod('GetIsExtended', @TSepiImportsTPolynom.GetIsExtended,
      'function: boolean');
    AddMethod('GetIsInteger', @TSepiImportsTPolynom.GetIsInteger,
      'function: boolean');
    AddMethod('GetIsNatural', @TSepiImportsTPolynom.GetIsNatural,
      'function: boolean');
    AddMethod('GetCoefficients', @TSepiImportsTPolynom.GetCoefficients,
      'function(Exponent : integer) : Extended');
    AddMethod('GetAsExtended', @TSepiImportsTPolynom.GetAsExtended,
      'function: Extended');
    AddMethod('GetAsInteger', @TSepiImportsTPolynom.GetAsInteger,
      'function: Int64');
    AddMethod('GetAsNatural', @TSepiImportsTPolynom.GetAsNatural,
      'function: Int64');
    AddMethod('SetCoefficients', @TSepiImportsTPolynom.SetCoefficients,
      'procedure(Exponent : integer; New : Extended)');
    AddMethod('SetAsExtended', @TSepiImportsTPolynom.SetAsExtended,
      'procedure(New : Extended)');
    AddMethod('SetAsInteger', @TSepiImportsTPolynom.SetAsInteger,
      'procedure(New : Int64)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPolynom.Create,
      'constructor');
    AddMethod('CreateAssign', @TSepiImportsTPolynom.CreateAssign,
      'constructor(Source : TPolynom)');
    AddMethod('CreateFromExtended', @TSepiImportsTPolynom.CreateFromExtended,
      'constructor(Exponent : integer; Coefficient : Extended)');
    AddMethod('CreateFromStream', @TSepiImportsTPolynom.CreateFromStream,
      'constructor(Stream : TStream)');
    AddMethod('Destroy', @TSepiImportsTPolynom.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('SetToZero', @TSepiImportsTPolynom.SetToZero,
      'procedure');
    AddMethod('Assign', @TSepiImportsTPolynom.Assign,
      'procedure(Source : TPolynom)');
    AddMethod('ToString', @TSepiImportsTPolynom.ToString,
      'function: string');
    AddMethod('LoadFromStream', @TSepiImportsTPolynom.LoadFromStream,
      'procedure(Stream : TStream)');
    AddMethod('SaveToStream', @TSepiImportsTPolynom.SaveToStream,
      'procedure(Stream : TStream)');
    AddMethod('Add', @TSepiImportsTPolynom.Add,
      'procedure(Source : TPolynom)');
    AddMethod('Sub', @TSepiImportsTPolynom.Sub,
      'procedure(Source : TPolynom)');
    AddMethod('Multiply', @TSepiImportsTPolynom.Multiply,
      'procedure(Source : TPolynom)');
    AddMethod('Divide', @TSepiImportsTPolynom.Divide,
      'procedure(Source : TPolynom)');
    AddMethod('Modulo', @TSepiImportsTPolynom.Modulo,
      'procedure(Source : TPolynom)');
    AddMethod('Power', @TSepiImportsTPolynom.Power,
      'procedure(Source : TPolynom)');
    AddMethod('Root', @TSepiImportsTPolynom.Root,
      'procedure(Source : TPolynom)');
    AddMethod('Insert', @TSepiImportsTPolynom.Insert,
      'procedure(Source : TPolynom)');
    AddMethod('ScalarProd', @TSepiImportsTPolynom.ScalarProd,
      'procedure(Source : TPolynom)');
    AddMethod('Oppose', @TSepiImportsTPolynom.Oppose,
      'procedure');
    AddMethod('Reverse', @TSepiImportsTPolynom.Reverse,
      'procedure');
    AddMethod('Abs', @TSepiImportsTPolynom.Abs,
      'procedure');
    AddMethod('Sinus', @TSepiImportsTPolynom.Sinus,
      'procedure');
    AddMethod('Cosinus', @TSepiImportsTPolynom.Cosinus,
      'procedure');
    AddMethod('Tangent', @TSepiImportsTPolynom.Tangent,
      'procedure');
    AddMethod('ArcSinus', @TSepiImportsTPolynom.ArcSinus,
      'procedure');
    AddMethod('ArcCosinus', @TSepiImportsTPolynom.ArcCosinus,
      'procedure');
    AddMethod('ArcTangent', @TSepiImportsTPolynom.ArcTangent,
      'procedure');
    AddMethod('Factorial', @TSepiImportsTPolynom.Factorial,
      'procedure');
    AddMethod('Addition', @TSepiImportsTPolynom.Addition,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('Substraction', @TSepiImportsTPolynom.Substraction,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('Multiplication', @TSepiImportsTPolynom.Multiplication,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('Division', @TSepiImportsTPolynom.Division,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('ModuloOf', @TSepiImportsTPolynom.ModuloOf,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('PowerOf', @TSepiImportsTPolynom.PowerOf,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('RootOf', @TSepiImportsTPolynom.RootOf,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('InsertOf', @TSepiImportsTPolynom.InsertOf,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('ScalarProdOf', @TSepiImportsTPolynom.ScalarProdOf,
      'class function(Val1, Val2 : TPolynom; ReleaseVals : boolean = False) : TPolynom');
    AddMethod('OpposedOf', @TSepiImportsTPolynom.OpposedOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('ReversedOf', @TSepiImportsTPolynom.ReversedOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('AbsOf', @TSepiImportsTPolynom.AbsOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('SinusOf', @TSepiImportsTPolynom.SinusOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('CosinusOf', @TSepiImportsTPolynom.CosinusOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('TangentOf', @TSepiImportsTPolynom.TangentOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('ArcSinusOf', @TSepiImportsTPolynom.ArcSinusOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('ArcCosinusOf', @TSepiImportsTPolynom.ArcCosinusOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('ArcTangentOf', @TSepiImportsTPolynom.ArcTangentOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');
    AddMethod('FactorialOf', @TSepiImportsTPolynom.FactorialOf,
      'class function(Value : TPolynom; ReleaseValue : boolean = False) : TPolynom');

    AddProperty('MinDegree', 'property: integer',
      'GetMinDegree', '');
    AddProperty('MaxDegree', 'property: integer',
      'GetMaxDegree', '');
    AddProperty('NegDegree', 'property: integer',
      'GetNegDegree', '');
    AddProperty('PosDegree', 'property: integer',
      'GetPosDegree', '');
    AddProperty('IsZero', 'property: boolean',
      'GetIsZero', '');
    AddProperty('IsExtended', 'property: boolean',
      'GetIsExtended', '');
    AddProperty('IsInteger', 'property: boolean',
      'GetIsInteger', '');
    AddProperty('IsNatural', 'property: boolean',
      'GetIsNatural', '');
    AddProperty('Coefficients', 'property[Exponent : integer] : Extended',
      'GetCoefficients', 'SetCoefficients', True);
    AddProperty('AsExtended', 'property: Extended',
      'GetAsExtended', 'SetAsExtended');
    AddProperty('AsInteger', 'property: Int64',
      'GetAsInteger', 'SetAsInteger');
    AddProperty('AsNatural', 'property: Int64',
      'GetAsNatural', 'SetAsInteger');

    Complete;
  end;
end;

{---------------------}
{ TPolynomList import }
{---------------------}

function TSepiImportsTPolynomList.GetCount: integer;
begin
  Result := Count;
end;

function TSepiImportsTPolynomList.GetOwnsObjects: boolean;
begin
  Result := OwnsObjects;
end;

function TSepiImportsTPolynomList.GetPolynoms(Index : integer) : TPolynom;
begin
  Result := Polynoms[Index];
end;

procedure TSepiImportsTPolynomList.SetOwnsObjects(New : boolean);
begin
  OwnsObjects := New;
end;

procedure TSepiImportsTPolynomList.SetPolynoms(Index : integer; New : TPolynom);
begin
  Polynoms[Index] := New;
end;

class function TSepiImportsTPolynomList.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPolynomList));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPolynoms', System.TypeInfo(TObjectList));

    AddMethod('GetCount', @TSepiImportsTPolynomList.GetCount,
      'function: integer');
    AddMethod('GetOwnsObjects', @TSepiImportsTPolynomList.GetOwnsObjects,
      'function: boolean');
    AddMethod('GetPolynoms', @TSepiImportsTPolynomList.GetPolynoms,
      'function(Index : integer) : TPolynom');
    AddMethod('SetOwnsObjects', @TSepiImportsTPolynomList.SetOwnsObjects,
      'procedure(New : boolean)');
    AddMethod('SetPolynoms', @TSepiImportsTPolynomList.SetPolynoms,
      'procedure(Index : integer; New : TPolynom)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPolynomList.Create,
      'constructor(AOwnsObjects : boolean = False)');
    AddMethod('CreateFromStream', @TSepiImportsTPolynomList.CreateFromStream,
      'constructor(Stream : TStream; AOwnsObjects : boolean = False)');
    AddMethod('CreateFromFile', @TSepiImportsTPolynomList.CreateFromFile,
      'constructor(FileName : string; AOwnsObjects : boolean = False)');
    AddMethod('Destroy', @TSepiImportsTPolynomList.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('LoadFromStream', @TSepiImportsTPolynomList.LoadFromStream,
      'procedure(Stream : TStream)');
    AddMethod('LoadFromFile', @TSepiImportsTPolynomList.LoadFromFile,
      'procedure(FileName : string)');
    AddMethod('SaveToStream', @TSepiImportsTPolynomList.SaveToStream,
      'procedure(Stream : TStream)');
    AddMethod('SaveToFile', @TSepiImportsTPolynomList.SaveToFile,
      'procedure(FileName : string)');
    AddMethod('Clear', @TSepiImportsTPolynomList.Clear,
      'procedure');
    AddMethod('Add', @TSepiImportsTPolynomList.Add,
      'function(New : TPolynom) : integer');
    AddMethod('Insert', @TSepiImportsTPolynomList.Insert,
      'function(Index : integer; New : TPolynom) : integer');
    AddMethod('Delete', @TSepiImportsTPolynomList.Delete,
      'procedure(Index : integer)');
    AddMethod('Extract', @TSepiImportsTPolynomList.Extract,
      'function(Polynom : TPolynom) : TPolynom');

    AddProperty('Count', 'property: integer',
      'GetCount', '');
    AddProperty('OwnsObjects', 'property: boolean',
      'GetOwnsObjects', 'SetOwnsObjects');
    AddProperty('Polynoms', 'property[index : integer] : TPolynom',
      'GetPolynoms', 'SetPolynoms', True);

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'ScMaths',
    ['Classes', 'SysUtils', 'Contnrs', 'Math', 'VarCmplx', 'ScUtils', 'ScStrUtils', 'ScLists', 'ScConsts']);

  // Constants
  //TSepiConstant.Create(Result, 'MonomsChars', MonomsChars);
  //TSepiConstant.Create(Result, 'BinaryOperators', BinaryOperators);
  //TSepiConstant.Create(Result, 'UnaryOperators', UnaryOperators);
  //TSepiConstant.Create(Result, 'VarsListsCount', VarsListsCount);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComplex));
  TSepiClass.ForwardDecl(Result, TypeInfo(TComplexList));
  TSepiClass.ForwardDecl(Result, TypeInfo(TPolynom));
  TSepiClass.ForwardDecl(Result, TypeInfo(EEvalError));
  TSepiMetaClass.Create(Result, 'EEvalErrorClass', TypeInfo(EEvalError), True);
  TSepiImportsEMathsError.SepiImport(Result);
  TSepiImportsEDivisionError.SepiImport(Result);
  TSepiImportsERootError.SepiImport(Result);
  TSepiImportsEDegreeError.SepiImport(Result);
  TSepiImportsENotDegreeZeroError.SepiImport(Result);
  TSepiImportsEIntegerError.SepiImport(Result);
  TSepiImportsENotIntegerError.SepiImport(Result);
  TSepiImportsENaturalError.SepiImport(Result);
  TSepiImportsENotNaturalError.SepiImport(Result);
  TSepiImportsEEvalError.SepiImport(Result);
  TSepiImportsEWrongExpressionError.SepiImport(Result);
  TSepiImportsEWrongCharacterError.SepiImport(Result);
  TSepiImportsEOperationError.SepiImport(Result);
  TSepiImportsEOpNotExistsError.SepiImport(Result);
  TSepiImportsEOpRequestsError.SepiImport(Result);
  TSepiImportsEOpRequestsDegreeZeroError.SepiImport(Result);
  TSepiImportsEOpRequestsIntegerError.SepiImport(Result);
  TSepiImportsEOpRequestsNaturalError.SepiImport(Result);
  TSepiImportsEOpRequestsCorrectIndexError.SepiImport(Result);
  TSepiImportsEDivisionOpError.SepiImport(Result);
  TSepiImportsERootOpError.SepiImport(Result);
  TSepiImportsEBracketsError.SepiImport(Result);
  TSepiImportsETooManyBracketsError.SepiImport(Result);
  TSepiImportsEEquationError.SepiImport(Result);
  TSepiImportsECannotSolveEquationError.SepiImport(Result);
  TSepiImportsEDegreeTooHighError.SepiImport(Result);
  TSepiImportsEDegreeTooLowError.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TTestIsAbortedProc));
  TSepiMethodRefType.Create(Result, 'TScalarProdFunc',
    'function(Left, Right : TPolynom) : Extended');
  TSepiImportsTComplexList.SepiImport(Result);
  TSepiImportsTPolynom.SepiImport(Result);
  TSepiImportsTPolynomList.SepiImport(Result);

  // Routines
  TSepiMetaMethod.Create(Result, 'IsInteger', @IsInteger,
    'function(X : Extended) : boolean');
  TSepiMetaMethod.Create(Result, 'VerifyIsInteger', @VerifyIsInteger,
    'procedure(X : Extended)');
  TSepiMetaMethod.Create(Result, 'AsInteger', @AsInteger,
    'function(X : Extended) : Int64');
  TSepiMetaMethod.Create(Result, 'IsNatural', @IsNatural,
    'function(X : Int64) : boolean');
  TSepiMetaMethod.Create(Result, 'VerifyIsNatural', @VerifyIsNatural,
    'procedure(X : Int64)');
  TSepiMetaMethod.Create(Result, 'AsNatural', @AsNatural,
    'function(X : Int64) : Int64');
  TSepiMetaMethod.Create(Result, 'Factorial', @Factorial,
    'function(X : Int64) : Int64');
  TSepiMetaMethod.Create(Result, 'IntPower', @IntPower,
    'function(Base, Exponent : LongWord) : LongWord');
  TSepiMetaMethod.Create(Result, 'DefaultScalarProd', @DefaultScalarProd,
    'function(Left, Right : TPolynom) : Extended');
  TSepiMetaMethod.Create(Result, 'Eval', @Eval,
    'function(Expression : string; TestIsAborted : TTestIsAbortedProc = nil) : TPolynom');

  // Constants
  TSepiConstant.Create(Result, 'MinValueEver', MinValueEver);

  // Global variables
  TSepiArrayType.Create(Result, '$1',
    [0, VarsListsCount-1], TypeInfo(TPolynomList), True);
  TSepiVariable.Create(Result, 'VarsLists',
    VarsLists, '$1');
  TSepiVariable.Create(Result, 'ScalarProdFunc',
    @ScalarProdFunc, 'TScalarProdFunc');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('ScMaths', ImportUnit);
end.

