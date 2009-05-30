{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  S�bastien Doeraene
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
-------------------------------------------------------------------------------}

{*
  Analyseur lexical d'une unit� Delphi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiLexer;

interface

{$D-,L-}

uses
  Windows, Classes, SysUtils, StrUtils, ScStrUtils, SepiCompilerErrors,
  SepiParseTrees, SepiLexerUtils, SepiCompilerConsts, SepiExpressions,
  SepiCompiler, SepiDelphiLikeCompilerUtils, SepiOrdTypes, SepiStdCompilerNodes;

resourcestring
  SIFInstrNotSupported =
    'Instruction du pr�-processeur $IF non support�e sans contexte';
  SErrorInIFInstr = 'Erreur dans la condition d''un $IF ou $ELSEIF';
  SPreProcReachedEndOfFile = 'Fin de fichier atteinte par le pr�-processeur';

const
  FirstTerminal = 0;
  LastTerminal = 111;

  tkEof = SepiLexerUtils.tkEof; /// Fin de fichier

  tkIdentifier = 1; // Identificateur
  tkInteger = 2;    // Nombre entier
  tkFloat = 3;      // Nombre en virgule flottante
  tkStringCst = 4;  // Cha�ne de caract�res

  tkOpenBracket = 5;    // (
  tkCloseBracket = 6;   // )
  tkOpenSqBracket = 7;  // [
  tkCloseSqBracket = 8; // ]
  tkEquals = 9;         // =
  tkComma = 10;         // ,
  tkColon = 11;         // :
  tkSemiColon = 12;     // ;
  tkDot = 13;           // .
  tkRange = 14;         // ..
  tkHat = 15;           // ^
  tkAt = 16;            // @
  tkAssign = 17;        // :=

  tkUnit = 18;           // unit
  tkUses = 19;           // uses
  tkType = 20;           // type
  tkConst = 21;          // const
  tkResourceString = 22; // resourcestring
  tkVar = 23;            // var
  tkOut = 24;            // out

  tkArray = 25;         // array
  tkSet = 26;           // set
  tkOf = 27;            // of
  tkObject = 28;        // object
  tkPacked = 29;        // packed
  tkRecord = 30;        // record
  tkCase = 31;          // case
  tkInterface = 32;     // interface
  tkDispInterface = 33; // dispinterface
  tkClass = 34;         // class
  tkPrivate = 35;       // private
  tkProtected = 36;     // protected
  tkPublic = 37;        // public
  tkPublished = 38;     // published
  tkBegin = 39;         // begin
  tkEnd = 40;           // end
  tkNil = 41;           // nil

  tkProcedure = 42;   // procedure
  tkFunction = 43;    // function
  tkProperty = 44;    // property
  tkConstructor = 45; // constructor
  tkDestructor = 46;  // destructor

  tkPlus = 47;        // +
  tkMinus = 48;       // -
  tkTimes = 49;       // *
  tkDivide = 50;      // /
  tkDiv = 51;         // div
  tkMod = 52;         // mod
  tkShl = 53;         // shl
  tkShr = 54;         // shr
  tkOr = 55;          // or
  tkAnd = 56;         // and
  tkXor = 57;         // xor
  tkNot = 58;         // not
  tkLowerThan = 59;   // <
  tkLowerEq = 60;     // <=
  tkGreaterThan = 61; // >
  tkGreaterEq = 62;   // >=
  tkNotEqual = 63;    // <>

  tkRegister = 64;  // register
  tkCDecl = 65;     // cdecl
  tkPascal = 66;    // pascal
  tkStdCall = 67;   // stdcall
  tkSafeCall = 68;  // safecall
  tkAssembler = 69; // assembler

  tkName = 70;      // name
  tkIndex = 71;     // index
  tkRead = 72;      // read
  tkWrite = 73;     // write
  tkDefault = 74;   // default
  tkNoDefault = 75; // nodefault
  tkStored = 76;    // stored
  tkDispID = 77;    // dispid
  tkReadOnly = 78;  // readonly
  tkWriteOnly = 79; // writeonly
  tkString = 80;    // string

  tkDeprecated = 81;  // deprecated
  tkPlatform = 82;    // platform

  tkOverload = 83;    // overload
  tkVirtual = 84;     // virtual
  tkDynamic = 85;     // dynamic
  tkMessage = 86;     // message
  tkOverride = 87;    // override
  tkAbstract = 88;    // abstract
  tkStatic = 89;      // static
  tkReintroduce = 90; // reintroduce
  tkInline = 91;      // inline

  tkImplementation = 92; // implementation
  tkForward = 93;        // forward
  tkInitialization = 94; // initialization
  tkFinalization = 95;   // finalization

  tkIf = 96;         // if
  tkThen = 97;       // then
  tkElse = 98;       // else
  tkWhile = 99;      // while
  tkDo = 100;        // do
  tkRepeat = 101;    // repeat
  tkUntil = 102;     // until
  tkFor = 103;       // for
  tkTo = 104;        // to
  tkDownTo = 105;    // downto
  tkTry = 106;       // try
  tkExcept = 107;    // except
  tkOn = 108;        // on
  tkFinally = 109;   // finally
  tkRaise = 110;     // raise
  tkInherited = 111; // inherited

  tkPreProcessor = -1; // pre-processor instruction

const
  /// ID message Directive du Compilateur : $MINEMUMSIZE ou $Z
  CDM_MINENUMSIZE = 1;

type
  TSepiDelphiLexer = class;

  {*
    Type de message Directive du Compilateur : $MINEMUMSIZE ou $Z
    @author sjrd
    @version 1.0
  *}
  TCDMMinEnumSize = record
    MsgID: Word;                   /// ID du message (CDM_MINENUMSIZE)
    MinEmumSize: TSepiMinEnumSize; /// Nouvelle taille mininum d'enum
  end;

  {*
    Type d'instruction du pr�-processuer
  *}
  TPreProcInstruction = (
    ppUnknown, ppToggles,
    ppDefine, ppUndef,
    ppIfDef, ppIfNDef, ppIf, ppElse, ppElseIf, ppEndIf, ppIfEnd,
    ppMinEnumSize
  );

  {*
    Pr�-processeur
    @author sjrd
    @version 1.0
  *}
  TPreProcessor = class(TObject)
  private
    FLexer: TSepiDelphiLexer; /// Analyseur propri�taire
    FDefines: TStrings;       /// Liste des defines

    function ReadCommand(out Param: string): TPreProcInstruction;
    procedure Skip(StopOnElseIf: Boolean = False);
    function EvalCondition(const Condition: string): Boolean;

    procedure MinEnumSize(const Param: string);

    procedure HandleToggle(Toggle, Value: Char);
    procedure HandleToggles(const Toggles: string);
  public
    constructor Create(ALexer: TSepiDelphiLexer);
    destructor Destroy; override;

    procedure PreProc;

    property Lexer: TSepiDelphiLexer read FLexer;
    property Defines: TStrings read FDefines;
  end;

  {*
    Noeud condition d'une instruction du pr�-processeur $IF
    @author sjrd
    @version 1.0
  *}
  TPreProcIfConditionNode = class(TSepiHiddenNonTerminal)
  private
    FPreProcessor: TPreProcessor; /// Pr�-processeur propri�taire
  public
    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property PreProcessor: TPreProcessor read FPreProcessor;
  end;

  {*
    Pseudo-routine d'op�ration sur un type
    @author sjrd
    @version 1.0
  *}
  TSepiDefinedDeclaredPseudoRoutine = class(TSepiCustomComputedValue,
    ISepiIdentifierTestPseudoRoutine)
  private
    FPreProcessor: TPreProcessor; /// Pr�-processeur du contexte
    FIsDeclared: Boolean;         /// False pour Defined ; True pour Declared
    FIdentifier: string;          /// Identificateur � tester
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;

    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);
  public
    constructor Create(APreProcessor: TPreProcessor; AIsDeclared: Boolean);

    procedure Complete;

    property PreProcessor: TPreProcessor read FPreProcessor;
    property IsDeclared: Boolean read FIsDeclared;
    property Identifier: string read FIdentifier write FIdentifier;
  end;

  {*
    Analyseur lexical pour le langage Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLexer = class(TSepiCustomManualLexer)
  private
    FPreProcessor: TPreProcessor; /// Pr�-processeur
    FInterfaceOnly: Boolean;      /// Consid�rer 'implementation' comme Eof
  protected
    procedure IdentifyKeyword(const OrigKey: string;
      var SymbolClass: TSepiSymbolClass); override;

    procedure InitLexingFuncs; override;

    function ActionSymbol: Boolean;
    function ActionIdentifier: Boolean;
    function ActionNumber: Boolean;
    function ActionString: Boolean;
    function ActionSingleLineComment: Boolean;
    function ActionMultiLineComment: Boolean;

    procedure NextPreProc;

    property PreProcessor: TPreProcessor read FPreProcessor;
  public
    constructor Create(AErrors: TSepiCompilerErrorList; const ACode: string;
      const AFileName: TFileName = ''); override;
    constructor CreateSpecial(AErrors: TSepiCompilerErrorList;
      const ACode: string; const AFileName: TFileName;
      AInterfaceOnly: Boolean); virtual;
    destructor Destroy; override;

    procedure NextTerminal; override;

    property InterfaceOnly: Boolean read FInterfaceOnly;
  end;

var
  /// Symbol class names, indexed by their IDs
  SymbolClassNames: array of string;

implementation

uses
  SepiDelphiParser, SepiDelphiCompiler;

const
  IdentChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  NumberChars = ['0'..'9'];
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
  StringChars = ['''', '#'];

  PreProcInstrs: array[TPreProcInstruction] of string = (
    '', '',
    'DEFINE', 'UNDEF',
    'IFDEF', 'IFNDEF', 'IF', 'ELSE', 'ELSEIF', 'ENDIF', 'IFEND',
    'MINENUMSIZE'
  );

{---------------------}
{ TPreProcessor class }
{---------------------}

{*
  Cr�e un nouveau pr�-processuer
  @param ALexer   Analyseur lexical propri�taire
*}
constructor TPreProcessor.Create(ALexer: TSepiDelphiLexer);
begin
  inherited Create;

  FLexer := ALexer;
  FDefines := TStringList.Create;

  with TStringList(FDefines) do
  begin
    CaseSensitive := False;
    Sorted := True;
    Duplicates := dupIgnore;

    Add('MSWINDOWS');
    Add('WIN32');
    Add('VER170');
    Add('SEPIPARSER');
  end;
end;

{*
  [@inheritDoc]
*}
destructor TPreProcessor.Destroy;
begin
  FDefines.Free;

  inherited;
end;

{*
  Lit la commande du pr�-processeur
  @param Param   En sortie : param�tre de la commande (ou '' si aucun)
  @return Num�ro de la commande, ou -1 si inconnue
*}
function TPreProcessor.ReadCommand(out Param: string): TPreProcInstruction;
var
  Instr, Command: string;
  CommandIdx: Integer;
begin
  Instr := Lexer.CurTerminal.Representation;
  if not SplitToken(Instr, ' ', Command, Param) then
    Param := '';

  if (Length(Command) >= 2) and (Command[1] in ['A'..'Z']) and
    (Command[2] in ['+', '-', '0'..'9']) then
  begin
    Param := Instr;
    Result := ppToggles;
  end else
  begin
    CommandIdx := AnsiIndexText(Command, PreProcInstrs);

    if CommandIdx < 0 then
      Result := ppUnknown
    else
      Result := TPreProcInstruction(CommandIdx);
  end;
end;

{*
  �limine une portion de code, jusqu'au ELSE ou ENDIF correspondant
  @param StopOnElseIf   Si True, s'arr�te sur un $ELSEIF
*}
procedure TPreProcessor.Skip(StopOnElseIf: Boolean = False);
var
  Depth: Integer;
  Param: string;
begin
  Depth := 1;

  while Depth > 0 do
  begin
    Lexer.NextPreProc;

    case ReadCommand(Param) of
      ppIfDef, ppIfNDef, ppIf:
        Inc(Depth);
      ppElse, ppEndIf, ppIfEnd:
        Dec(Depth);
      ppElseIf:
      begin
        { $ELSEIF decrements Depth and immediately increments it.  Therefore,
          it has no effect; unless Depth reaches 0 � this time and we are
          asked to StopOnElseIf. }
        if (Depth = 1) and StopOnElseIf then
          Break;
      end;
    end;
  end;
end;

{*
  �value une condition bool�enne constante dans le contexte courant
  @param Condition   Condition � tester
  @return Valeur de v�rit� de la condition
*}
function TPreProcessor.EvalCondition(const Condition: string): Boolean;
var
  RootNode: TPreProcIfConditionNode;
  ExprNode: TSepiConstExpressionNode;
begin
  if Lexer.Context = nil then
    Lexer.MakeError(SIFInstrNotSupported, ekFatalError);

  RootNode := TPreProcIfConditionNode.Create(Lexer.Context,
    ntInPreProcessorExpression, Lexer.CurrentPos);
  try
    RootNode.FPreProcessor := Self;

    // Parse the condition expression
    TSepiDelphiParser.Parse(RootNode,
      TSepiDelphiLexer.Create(Lexer.Errors, Condition));

    // Fetch its value into Result
    ExprNode := RootNode.Children[0] as TSepiConstExpressionNode;
    if not ExprNode.CompileConst(Result, ExprNode.SystemUnit.Boolean) then
      Lexer.MakeError(SErrorInIFInstr, ekFatalError);
  finally
    RootNode.Free;
  end;
end;

{*
  Applique la directive $MINENUMSIZE ou $Z
  @param Param   Param�tre
*}
procedure TPreProcessor.MinEnumSize(const Param: string);
var
  MinEnumSizeMsg: TCDMMinEnumSize;
begin
  MinEnumSizeMsg.MsgID := CDM_MINENUMSIZE;

  if Param = '+' then
    MinEnumSizeMsg.MinEmumSize := mesLongWord
  else if Param = '-' then
    MinEnumSizeMsg.MinEmumSize := mesByte
  else
  begin
    case StrToIntDef(Param, 0) of
      1: MinEnumSizeMsg.MinEmumSize := mesByte;
      2: MinEnumSizeMsg.MinEmumSize := mesWord;
      4: MinEnumSizeMsg.MinEmumSize := mesLongWord;
    end;
  end;

  Lexer.Context.Dispatch(MinEnumSizeMsg);
end;

{*
  Applique une directive de type toggle
  @param Toggle   Toggle � appliquer
  @param Value    Valeur du toggle
*}
procedure TPreProcessor.HandleToggle(Toggle, Value: Char);
begin
  if Toggle = 'Z' then
    MinEnumSize(Value);
end;

{*
  Applique une directive avec des toggles
  @param Param   Param�tre
*}
procedure TPreProcessor.HandleToggles(const Toggles: string);
var
  Remaining, Temp, Toggle: string;
begin
  Remaining := Toggles;
  while Remaining <> '' do
  begin
    if SplitToken(Remaining, ',', Toggle, Temp) then
      Remaining := Temp
    else
      Remaining := '';

    Toggle := Trim(Toggle);
    if Length(Toggle) = 2 then
      HandleToggle(Toggle[1], Toggle[2]);
  end;
end;

{*
  Applique une instruction du pr�-processeur
*}
procedure TPreProcessor.PreProc;
var
  Command: TPreProcInstruction;
  Param: string;
  Defined: Boolean;
begin
  Command := ReadCommand(Param);
  Defined := Defines.IndexOf(Param) >= 0;

  case Command of
    ppToggles:
      HandleToggles(Param);
    ppDefine:
      Defines.Add(Param);
    ppUndef:
      if Defined then
        Defines.Delete(Defines.IndexOf(Param));
    ppIfDef:
      if not Defined then
        Skip;
    ppIfNDef:
      if Defined then
        Skip;
    ppIf:
    begin
      while (Command in [ppIf, ppElseIf]) and (not EvalCondition(Param)) do
      begin
        Skip(True); // StopOnElseIf = True
        Command := ReadCommand(Param);
      end;
    end;
    ppElse, ppElseIf:
      Skip;
    ppEndIf, ppIfEnd: ;
    ppMinEnumSize:
      MinEnumSize(Param);
  end;
end;

{-------------------------------}
{ TPreProcIfConditionNode class }
{-------------------------------}

{*
  [@inheritDoc]
*}
function TPreProcIfConditionNode.ResolveIdent(
  const Identifier: string): ISepiExpression;
begin
  if AnsiSameText(Identifier, 'Defined') then
  begin
    Result := MakeExpression;
    ISepiExpressionPart(TSepiDefinedDeclaredPseudoRoutine.Create(
      PreProcessor, False)).AttachToExpression(Result);
  end else if AnsiSameText(Identifier, 'Declared') then
  begin
    Result := MakeExpression;
    ISepiExpressionPart(TSepiDefinedDeclaredPseudoRoutine.Create(
      PreProcessor, True)).AttachToExpression(Result);
  end else
  begin
    Result := inherited ResolveIdent(Identifier);
  end;
end;

{-----------------------------------------}
{ TSepiDefinedDeclaredPseudoRoutine class }
{-----------------------------------------}

{*
  Cr�e la pseudo-routine
*}
constructor TSepiDefinedDeclaredPseudoRoutine.Create(
  APreProcessor: TPreProcessor; AIsDeclared: Boolean);
begin
  inherited Create;

  FPreProcessor := APreProcessor;
  FIsDeclared := AIsDeclared;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDefinedDeclaredPseudoRoutine.AttachToExpression(
  const Expression: ISepiExpression);
var
  AsExpressionPart: ISepiExpressionPart;
begin
  AsExpressionPart := Self;
  Expression.Attach(ISepiIdentifierTestPseudoRoutine, AsExpressionPart);

  if Identifier <> '' then
    inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDefinedDeclaredPseudoRoutine.CompileCompute(
  Compiler: TSepiMethodCompiler; Instructions: TSepiInstructionList;
  var Destination: TSepiMemoryReference; TempVars: TSepiTempVarsLifeManager);
begin
  Assert(False);
end;

{*
  [@inheritDoc]
*}
function TSepiDefinedDeclaredPseudoRoutine.GetIdentifier: string;
begin
  Result := Identifier;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDefinedDeclaredPseudoRoutine.SetIdentifier(const Value: string);
begin
  Identifier := Value;
end;

{*
  Compl�te la pseudo-routine
*}
procedure TSepiDefinedDeclaredPseudoRoutine.Complete;
var
  Value: Boolean;
begin
  Assert(Identifier <> '');

  SetValueType(UnitCompiler.SystemUnit.Boolean);
  AllocateConstant;

  if IsDeclared then
    Value := PreProcessor.Lexer.Context.ResolveIdent(Identifier) <> nil
  else
    Value := PreProcessor.Defines.IndexOf(Identifier) >= 0;

  Boolean(ConstValuePtr^) := Value;
end;

{------------------------}
{ TSepiDelphiLexer class }
{------------------------}

{*
  Cr�e un nouvel analyseur lexical
  @param AErrors     Erreurs de compilation
  @param ACode       Code source � analyser
  @param AFileName   Nom du fichier source
*}
constructor TSepiDelphiLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '');
begin
  CreateSpecial(AErrors, ACode, AFileName, False);
end;

{*
  Cr�e un nouvel analyseur lexical sp�cial
  @param AErrors          Erreurs de compilation
  @param ACode            Code source � analyser
  @param AFileName        Nom du fichier source
  @param AInterfaceOnly   Si True, consid�re 'implementation' come un Eof
*}
constructor TSepiDelphiLexer.CreateSpecial(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName;
  AInterfaceOnly: Boolean);
begin
  inherited Create(AErrors, ACode, AFileName);

  FPreProcessor := TPreProcessor.Create(Self);
  FInterfaceOnly := AInterfaceOnly;
end;

{*
  [@inheritDoc]
*}
destructor TSepiDelphiLexer.Destroy;
begin
  FPreProcessor.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiLexer.IdentifyKeyword(const OrigKey: string;
  var SymbolClass: TSepiSymbolClass);
var
  Key: string;
begin
  Key := LowerCase(OrigKey);

  case Key[1] of
    'a' : if Key = 'abstract'       then SymbolClass := tkAbstract else
          if Key = 'and'            then SymbolClass := tkAnd else
          if Key = 'array'          then SymbolClass := tkArray else
          if Key = 'assembler'      then SymbolClass := tkAssembler;
    'b' : if Key = 'begin'          then SymbolClass := tkBegin;
    'c' : if Key = 'case'           then SymbolClass := tkCase else
          if Key = 'cdecl'          then SymbolClass := tkCDecl else
          if Key = 'class'          then SymbolClass := tkClass else
          if Key = 'const'          then SymbolClass := tkConst else
          if Key = 'constructor'    then SymbolClass := tkConstructor;
    'd' : if Key = 'default'        then SymbolClass := tkDefault else
          if Key = 'deprecated'     then SymbolClass := tkDeprecated else
          if Key = 'destructor'     then SymbolClass := tkDestructor else
          if Key = 'dispid'         then SymbolClass := tkDispID else
          if Key = 'dispinterface'  then SymbolClass := tkDispInterface else
          if Key = 'div'            then SymbolClass := tkDiv else
          if Key = 'do'             then SymbolClass := tkDo else
          if Key = 'downto'         then SymbolClass := tkDownto else
          if Key = 'dynamic'        then SymbolClass := tkDynamic;
    'e' : if Key = 'else'           then SymbolClass := tkElse else
          if Key = 'end'            then SymbolClass := tkEnd else
          if Key = 'except'         then SymbolClass := tkExcept;
    'f' : if Key = 'finally'        then SymbolClass := tkFinally else
          if Key = 'for'            then SymbolClass := tkFor else
          if Key = 'forward'        then SymbolClass := tkForward else
          if Key = 'function'       then SymbolClass := tkFunction;
    'i' : if Key = 'if'             then SymbolClass := tkIf else
          if Key = 'implementation' then SymbolClass := tkImplementation else
          if Key = 'inline'         then SymbolClass := tkInline else
          if Key = 'index'          then SymbolClass := tkIndex else
          if Key = 'inherited'      then SymbolClass := tkInherited else
          if Key = 'interface'      then SymbolClass := tkInterface;
    'm' : if Key = 'message'        then SymbolClass := tkMessage else
          if Key = 'mod'            then SymbolClass := tkMod;
    'n' : if Key = 'name'           then SymbolClass := tkName else
          if Key = 'nil'            then SymbolClass := tkNil else
          if Key = 'nodefault'      then SymbolClass := tkNoDefault else
          if Key = 'not'            then SymbolClass := tkNot;
    'o' : if Key = 'object'         then SymbolClass := tkObject else
          if Key = 'of'             then SymbolClass := tkOf else
          if Key = 'on'             then SymbolClass := tkOn else
          if Key = 'or'             then SymbolClass := tkOr else
          if Key = 'out'            then SymbolClass := tkOut else
          if Key = 'overload'       then SymbolClass := tkOverload else
          if Key = 'override'       then SymbolClass := tkOverride;
    'p' : if Key = 'packed'         then SymbolClass := tkPacked else
          if Key = 'pascal'         then SymbolClass := tkPascal else
          if Key = 'platform'       then SymbolClass := tkPlatform else
          if Key = 'private'        then SymbolClass := tkPrivate else
          if Key = 'procedure'      then SymbolClass := tkProcedure else
          if Key = 'property'       then SymbolClass := tkProperty else
          if Key = 'protected'      then SymbolClass := tkProtected else
          if Key = 'public'         then SymbolClass := tkPublic else
          if Key = 'published'      then SymbolClass := tkPublished;
    'r' : if Key = 'raise'          then SymbolClass := tkRaise else
          if Key = 'read'           then SymbolClass := tkRead else
          if Key = 'readonly'       then SymbolClass := tkReadOnly else
          if Key = 'record'         then SymbolClass := tkRecord else
          if Key = 'register'       then SymbolClass := tkRegister else
          if Key = 'reintroduce'    then SymbolClass := tkReintroduce else
          if Key = 'repeat'         then SymbolClass := tkRepeat else
          if Key = 'resourcestring' then SymbolClass := tkResourceString;
    's' : if Key = 'safecall'       then SymbolClass := tkSafeCall else
          if Key = 'stdcall'        then SymbolClass := tkStdCall else
          if Key = 'stored'         then SymbolClass := tkStored else
          if Key = 'set'            then SymbolClass := tkSet else
          if Key = 'shl'            then SymbolClass := tkShl else
          if Key = 'shr'            then SymbolClass := tkShr else
          if Key = 'static'         then SymbolClass := tkStatic else
          if Key = 'string'         then SymbolClass := tkString;
    't' : if Key = 'then'           then SymbolClass := tkThen else
          if Key = 'to'             then SymbolClass := tkTo else
          if Key = 'try'            then SymbolClass := tkTry else
          if Key = 'type'           then SymbolClass := tkType;
    'u' : if Key = 'unit'           then SymbolClass := tkUnit else
          if Key = 'until'          then SymbolClass := tkUntil else
          if Key = 'uses'           then SymbolClass := tkUses;
    'v' : if Key = 'var'            then SymbolClass := tkVar else
          if Key = 'virtual'        then SymbolClass := tkVirtual;
    'w' : if Key = 'while'          then SymbolClass := tkWhile else
          if Key = 'write'          then SymbolClass := tkWrite else
          if Key = 'writeonly'      then SymbolClass := tkWriteOnly;
    'x' : if Key = 'xor'            then SymbolClass := tkXor;
  end;

  if (SymbolClass = tkImplementation) and InterfaceOnly then
    SymbolClass := tkEof;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiLexer.InitLexingFuncs;
var
  C: Char;
begin
  inherited;

  for C := #0 to #255 do
  begin
    case C of
      '(', ')', '[', ']', '=', ',', ':', ';', '.',
        '^', '@', '+', '-', '*', '/', '<', '>':
        LexingFuncs[C] := ActionSymbol;
      'a'..'z', 'A'..'Z', '_', '&':
        LexingFuncs[C] := ActionIdentifier;
      '0'..'9', '$':
        LexingFuncs[C] := ActionNumber;
      '''', '#':
        LexingFuncs[C] := ActionString;
      '{':
        LexingFuncs[C] := ActionMultiLineComment;
    end;
  end;
end;

{*
  Analyse un symbole ou un commentaire
  @return True pour un symbole, False pour un commentaire
*}
function TSepiDelphiLexer.ActionSymbol: Boolean;
var
  Repr: string;
  SymbolClass: TSepiSymbolClass;
begin
  case Code[Cursor] of
    '(':
    begin
      case Code[Cursor+1] of
        '*':
        begin
          Result := ActionMultiLineComment;
          Exit;
        end;
      else
        CursorForward;
        Repr := '(';
        SymbolClass := tkOpenBracket;
      end;
    end;
    ')':
    begin
      CursorForward;
      Repr := ')';
      SymbolClass := tkCloseBracket;
    end;
    '[':
    begin
      CursorForward;
      Repr := '[';
      SymbolClass := tkOpenSqBracket;
    end;
    ']':
    begin
      CursorForward;
      Repr := ']';
      SymbolClass := tkCloseSqBracket;
    end;
    '=':
    begin
      CursorForward;
      Repr := '=';
      SymbolClass := tkEquals;
    end;
    ',':
    begin
      CursorForward;
      Repr := ',';
      SymbolClass := tkComma;
    end;
    ':':
    begin
      CursorForward;
      if Code[Cursor] = '=' then
      begin
        CursorForward;
        Repr := ':=';
        SymbolClass := tkAssign;
      end else
      begin
        Repr := ':';
        SymbolClass := tkColon;
      end;
    end;
    ';':
    begin
      CursorForward;
      Repr := ';';
      SymbolClass := tkSemiColon;
    end;
    '.':
    begin
      case Code[Cursor+1] of
        '.':
        begin
          CursorForward(2);
          Repr := '..';
          SymbolClass := tkRange;
        end;
      else
        CursorForward;
        Repr := '.';
        SymbolClass := tkDot;
      end;
    end;
    '^':
    begin
      CursorForward;
      Repr := '^';
      SymbolClass := tkHat;
    end;
    '@':
    begin
      CursorForward;
      Repr := '@';
      SymbolClass := tkAt;
    end;
    '+':
    begin
      CursorForward;
      Repr := '+';
      SymbolClass := tkPlus;
    end;
    '-':
    begin
      CursorForward;
      Repr := '-';
      SymbolClass := tkMinus;
    end;
    '*':
    begin
      CursorForward;
      Repr := '*';
      SymbolClass := tkTimes;
    end;
    '/':
    begin
      case Code[Cursor+1] of
        '/':
        begin
          Result := ActionSingleLineComment;
          Exit;
        end;
      else
        CursorForward;
        Repr := '/';
        SymbolClass := tkDivide;
      end;
    end;
    '<':
    begin
      case Code[Cursor+1] of
        '=':
        begin
          CursorForward(2);
          Repr := '<=';
          SymbolClass := tkLowerEq;
        end;
        '>':
        begin
          CursorForward(2);
          Repr := '<>';
          SymbolClass := tkNotEqual;
        end;
      else
        CursorForward;
        Repr := '<';
        SymbolClass := tkLowerThan;
      end;
    end;
    '>':
    begin
      case Code[Cursor+1] of
        '=':
        begin
          CursorForward(2);
          Repr := '>=';
          SymbolClass := tkGreaterEq;
        end;
      else
        CursorForward;
        Repr := '>';
        SymbolClass := tkGreaterThan;
      end;
    end;
  else
    Assert(False);
    Result := False;
    Exit;
  end;

  TerminalParsed(SymbolClass, Repr);
  Result := True;
end;

{*
  Analyse un identificateur
  @return True
*}
function TSepiDelphiLexer.ActionIdentifier: Boolean;
var
  ForceIdent: Boolean;
  BeginPos: Integer;
  Repr: string;
  SymbolClass: TSepiSymbolClass;
begin
  ForceIdent := Code[Cursor] = '&';
  if ForceIdent then
    CursorForward;

  BeginPos := Cursor;
  CursorForward;
  while Code[Cursor] in IdentChars do
    CursorForward;

  Repr := Copy(Code, BeginPos, Cursor-BeginPos);
  SymbolClass := tkIdentifier;

  if not ForceIdent then
    IdentifyKeyword(Repr, SymbolClass);

  TerminalParsed(SymbolClass, Repr);
  Result := True;
end;

{*
  Analyse un nombre
  @return True
*}
function TSepiDelphiLexer.ActionNumber: Boolean;
var
  BeginPos: Integer;
  SymbolClass: TSepiSymbolClass;
begin
  BeginPos := Cursor;
  SymbolClass := tkInteger;

  if Code[Cursor] = '$' then
  begin
    repeat
      CursorForward;
    until not (Code[Cursor] in HexChars);
  end else
  begin
    while Code[Cursor] in NumberChars do
      CursorForward;

    if (Code[Cursor] = '.') and (Code[Cursor+1] in NumberChars) then
    begin
      SymbolClass := tkFloat;
      repeat
        CursorForward;
      until not (Code[Cursor] in NumberChars);
    end;

    if Code[Cursor] in ['e', 'E'] then
    begin
      SymbolClass := tkFloat;
      CursorForward;
      if Code[Cursor] in ['+', '-'] then
        CursorForward;

      while Code[Cursor] in NumberChars do
        CursorForward;
    end;
  end;

  TerminalParsed(SymbolClass, Copy(Code, BeginPos, Cursor-BeginPos));
  Result := True;
end;

{*
  Analyse une cha�ne de caract�res
  @return True
*}
function TSepiDelphiLexer.ActionString: Boolean;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  while Code[Cursor] in StringChars do
  begin
    case Code[Cursor] of
      '''':
      begin
        CursorForward;
        while Code[Cursor] <> '''' do
        begin
          if Code[Cursor] in [#0, #10, #13] then
            MakeError(SStringNotTerminated, ekFatalError)
          else
            CursorForward;
        end;
        CursorForward;
      end;
      '#':
      begin
        CursorForward;
        if Code[Cursor] = '$' then
          CursorForward;
        if not (Code[Cursor] in HexChars) then
          MakeError(SStringNotTerminated, ekFatalError);
        while Code[Cursor] in HexChars do
          CursorForward;
      end;
    end;
  end;

  TerminalParsed(tkStringCst, Copy(Code, BeginPos, Cursor-BeginPos));
  Result := True;
end;

{*
  Analyse un commentaire sur une ligne
  @return False
*}
function TSepiDelphiLexer.ActionSingleLineComment: Boolean;
begin
  while not (Code[Cursor] in [#0, #13, #10]) do
    CursorForward;

  NoTerminalParsed;
  Result := False;
end;

{*
  Analyse un commentaire sur plusieurs lignes
  @return True pour une instruction du pr�-processuer, False sinon
*}
function TSepiDelphiLexer.ActionMultiLineComment: Boolean;
var
  BeginPos: Integer;
  Text: string;
begin
  BeginPos := Cursor;

  // Find end of comment
  if Code[Cursor] = '{' then
  begin
    while not (Code[Cursor] in [#0, '}']) do
      CursorForward;
    CursorForward;
  end else
  begin
    while ((Code[Cursor] <> '*') or (Code[Cursor+1] <> ')')) and
      (Code[Cursor] <> #0) do
      CursorForward;
    CursorForward(2);
  end;

  // Get comment text
  Text := Copy(Code, BeginPos, Cursor-BeginPos);
  if Code[BeginPos] = '{' then
    Text := Copy(Text, 2, Length(Text)-2)
  else
    Text := Copy(Text, 3, Length(Text)-4);

  // Is it a pre-processor instruction?
  if (Text <> '') and (Text[1] = '$') then
  begin
    // Pre-processor instruction
    TerminalParsed(tkPreProcessor, Copy(Text, 2, MaxInt));
    Result := True;
  end else
  begin
    // Pure comment
    NoTerminalParsed;
    Result := False;
  end;
end;

{*
  Analyse le code source jusqu'� trouver une instruction du pr�-processeur
*}
procedure TSepiDelphiLexer.NextPreProc;
begin
  while (not LexingFuncs[Code[Cursor]]) or
    (CurTerminal.SymbolClass <> tkPreProcessor) do
  begin
    if CurTerminal.SymbolClass = tkEof then
      MakeError(SPreProcReachedEndOfFile, ekFatalError);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiLexer.NextTerminal;
begin
  repeat
    while not LexingFuncs[Code[Cursor]] do;

    if CurTerminal.SymbolClass = tkPreProcessor then
      PreProcessor.PreProc;
  until CurTerminal.SymbolClass <> tkPreProcessor;
end;

initialization
  if Length(SymbolClassNames) < LastTerminal+1 then
    SetLength(SymbolClassNames, LastTerminal+1);

  SymbolClassNames[tkEof] := 'tkEof';
  SymbolClassNames[tkIdentifier] := 'tkIdentifier';
  SymbolClassNames[tkInteger] := 'tkInteger';
  SymbolClassNames[tkFloat] := 'tkFloat';
  SymbolClassNames[tkStringCst] := 'tkStringCst';

  SymbolClassNames[tkOpenBracket] := 'tkOpenBracket';
  SymbolClassNames[tkCloseBracket] := 'tkCloseBracket';
  SymbolClassNames[tkOpenSqBracket] := 'tkOpenSqBracket';
  SymbolClassNames[tkCloseSqBracket] := 'tkCloseSqBracket';
  SymbolClassNames[tkEquals] := 'tkEquals';
  SymbolClassNames[tkComma] := 'tkComma';
  SymbolClassNames[tkColon] := 'tkColon';
  SymbolClassNames[tkSemiColon] := 'tkSemiColon';
  SymbolClassNames[tkDot] := 'tkDot';
  SymbolClassNames[tkRange] := 'tkRange';
  SymbolClassNames[tkHat] := 'tkHat';
  SymbolClassNames[tkAt] := 'tkAt';
  SymbolClassNames[tkAssign] := 'tkAssign';

  SymbolClassNames[tkUnit] := 'tkUnit';
  SymbolClassNames[tkUses] := 'tkUses';
  SymbolClassNames[tkType] := 'tkType';
  SymbolClassNames[tkConst] := 'tkConst';
  SymbolClassNames[tkResourceString] := 'tkResourceString';
  SymbolClassNames[tkVar] := 'tkVar';
  SymbolClassNames[tkOut] := 'tkOut';

  SymbolClassNames[tkArray] := 'tkArray';
  SymbolClassNames[tkSet] := 'tkSet';
  SymbolClassNames[tkOf] := 'tkOf';
  SymbolClassNames[tkObject] := 'tkObject';
  SymbolClassNames[tkPacked] := 'tkPacked';
  SymbolClassNames[tkRecord] := 'tkRecord';
  SymbolClassNames[tkCase] := 'tkCase';
  SymbolClassNames[tkInterface] := 'tkInterface';
  SymbolClassNames[tkDispInterface] := 'tkDispInterface';
  SymbolClassNames[tkClass] := 'tkClass';
  SymbolClassNames[tkPrivate] := 'tkPrivate';
  SymbolClassNames[tkProtected] := 'tkProtected';
  SymbolClassNames[tkPublic] := 'tkPublic';
  SymbolClassNames[tkPublished] := 'tkPublished';
  SymbolClassNames[tkBegin] := 'tkBegin';
  SymbolClassNames[tkEnd] := 'tkEnd';
  SymbolClassNames[tkNil] := 'tkNil';

  SymbolClassNames[tkProcedure] := 'tkProcedure';
  SymbolClassNames[tkFunction] := 'tkFunction';
  SymbolClassNames[tkProperty] := 'tkProperty';
  SymbolClassNames[tkConstructor] := 'tkConstructor';
  SymbolClassNames[tkDestructor] := 'tkDestructor';

  SymbolClassNames[tkPlus] := 'tkPlus';
  SymbolClassNames[tkMinus] := 'tkMinus';
  SymbolClassNames[tkTimes] := 'tkTimes';
  SymbolClassNames[tkDivide] := 'tkDivide';
  SymbolClassNames[tkDiv] := 'tkDiv';
  SymbolClassNames[tkMod] := 'tkMod';
  SymbolClassNames[tkShl] := 'tkShl';
  SymbolClassNames[tkShr] := 'tkShr';
  SymbolClassNames[tkOr] := 'tkOr';
  SymbolClassNames[tkAnd] := 'tkAnd';
  SymbolClassNames[tkXor] := 'tkXor';
  SymbolClassNames[tkNot] := 'tkNot';
  SymbolClassNames[tkLowerThan] := 'tkLowerThan';
  SymbolClassNames[tkLowerEq] := 'tkLowerEq';
  SymbolClassNames[tkGreaterThan] := 'tkGreaterThan';
  SymbolClassNames[tkGreaterEq] := 'tkGreaterEq';
  SymbolClassNames[tkNotEqual] := 'tkNotEqual';

  SymbolClassNames[tkRegister] := 'tkRegister';
  SymbolClassNames[tkCDecl] := 'tkCDecl';
  SymbolClassNames[tkPascal] := 'tkPascal';
  SymbolClassNames[tkStdCall] := 'tkStdCall';
  SymbolClassNames[tkSafeCall] := 'tkSafeCall';
  SymbolClassNames[tkAssembler] := 'tkAssembler';

  SymbolClassNames[tkName] := 'tkName';
  SymbolClassNames[tkIndex] := 'tkIndex';
  SymbolClassNames[tkRead] := 'tkRead';
  SymbolClassNames[tkWrite] := 'tkWrite';
  SymbolClassNames[tkDefault] := 'tkDefault';
  SymbolClassNames[tkNoDefault] := 'tkNoDefault';
  SymbolClassNames[tkStored] := 'tkStored';
  SymbolClassNames[tkDispID] := 'tkDispID';
  SymbolClassNames[tkReadOnly] := 'tkReadOnly';
  SymbolClassNames[tkWriteOnly] := 'tkWriteOnly';
  SymbolClassNames[tkString] := 'tkString';

  SymbolClassNames[tkDeprecated] := 'tkDeprecated';
  SymbolClassNames[tkPlatform] := 'tkPlatform';

  SymbolClassNames[tkOverload] := 'tkOverload';
  SymbolClassNames[tkVirtual] := 'tkVirtual';
  SymbolClassNames[tkDynamic] := 'tkDynamic';
  SymbolClassNames[tkMessage] := 'tkMessage';
  SymbolClassNames[tkOverride] := 'tkOverride';
  SymbolClassNames[tkAbstract] := 'tkAbstract';
  SymbolClassNames[tkStatic] := 'tkStatic';
  SymbolClassNames[tkReintroduce] := 'tkReintroduce';
  SymbolClassNames[tkInline] := 'tkInline';

  SymbolClassNames[tkImplementation] := 'tkImplementation';
  SymbolClassNames[tkForward] := 'tkForward';
  SymbolClassNames[tkInitialization] := 'tkInitialization';
  SymbolClassNames[tkFinalization] := 'tkFinalization';

  SymbolClassNames[tkIf] := 'tkIf';
  SymbolClassNames[tkThen] := 'tkThen';
  SymbolClassNames[tkElse] := 'tkElse';
  SymbolClassNames[tkWhile] := 'tkWhile';
  SymbolClassNames[tkDo] := 'tkDo';
  SymbolClassNames[tkRepeat] := 'tkRepeat';
  SymbolClassNames[tkUntil] := 'tkUntil';
  SymbolClassNames[tkFor] := 'tkFor';
  SymbolClassNames[tkTo] := 'tkTo';
  SymbolClassNames[tkDownTo] := 'tkDownTo';
  SymbolClassNames[tkTry] := 'tkTry';
  SymbolClassNames[tkExcept] := 'tkExcept';
  SymbolClassNames[tkOn] := 'tkOn';
  SymbolClassNames[tkFinally] := 'tkFinally';
  SymbolClassNames[tkRaise] := 'tkRaise';
  SymbolClassNames[tkInherited] := 'tkInherited';
end.

