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
  Analyseur lexical d'une unité Delphi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiLexer;

interface

{$D-,L-}

uses
  Windows, Classes, SysUtils, StrUtils, ScUtils, ScStrUtils, ScDelphiLanguage,
  SepiCompilerErrors, SepiParseTrees, SepiLexerUtils, SepiCompilerConsts,
  SepiExpressions, SepiCompiler, SepiDelphiLikeCompilerUtils, SepiOrdTypes,
  SepiStdCompilerNodes;

resourcestring
  SIFInstrNotSupported =
    'Instruction du pré-processeur $IF non supportée sans contexte';
  SErrorInIFInstr = 'Erreur dans la condition d''un $IF ou $ELSEIF';
  SPreProcReachedEndOfFile = 'Fin de fichier atteinte par le pré-processeur';

const
  FirstTerminal = 0;
  LastTerminal = 119;

  tkEof = SepiLexerUtils.tkEof;         /// Fin de fichier
  tkBlank = SepiLexerUtils.tkBlank;     /// Lexème blanc
  tkComment = SepiLexerUtils.tkComment; /// Lexème commentaire

  tkIdentifier = 3; // Identificateur
  tkInteger = 4;    // Nombre entier
  tkFloat = 5;      // Nombre en virgule flottante
  tkStringCst = 6;  // Chaîne de caractères

  tkOpenBracket = 7;     // (
  tkCloseBracket = 8;    // )
  tkOpenSqBracket = 9;   // [
  tkCloseSqBracket = 10; // ]
  tkEquals = 11;         // =
  tkComma = 12;          // ,
  tkColon = 13;          // :
  tkSemiColon = 14;      // ;
  tkDot = 15;            // .
  tkRange = 16;          // ..
  tkHat = 17;            // ^
  tkAt = 18;             // @
  tkAssign = 19;         // :=

  tkUnit = 20;           // unit
  tkUses = 21;           // uses
  tkType = 22;           // type
  tkConst = 23;          // const
  tkResourceString = 24; // resourcestring
  tkVar = 25;            // var
  tkOut = 26;            // out

  tkArray = 27;         // array
  tkSet = 28;           // set
  tkOf = 29;            // of
  tkObject = 30;        // object
  tkPacked = 31;        // packed
  tkRecord = 32;        // record
  tkCase = 33;          // case
  tkInterface = 34;     // interface
  tkDispInterface = 35; // dispinterface
  tkClass = 36;         // class
  tkPrivate = 37;       // private
  tkProtected = 38;     // protected
  tkPublic = 39;        // public
  tkPublished = 40;     // published
  tkStrict = 41;        // strict
  tkBegin = 42;         // begin
  tkEnd = 43;           // end
  tkNil = 44;           // nil

  tkProcedure = 45;   // procedure
  tkFunction = 46;    // function
  tkProperty = 47;    // property
  tkConstructor = 48; // constructor
  tkDestructor = 49;  // destructor

  tkPlus = 50;        // +
  tkMinus = 51;       // -
  tkTimes = 52;       // *
  tkDivide = 53;      // /
  tkDiv = 54;         // div
  tkMod = 55;         // mod
  tkShl = 56;         // shl
  tkShr = 57;         // shr
  tkOr = 58;          // or
  tkAnd = 59;         // and
  tkXor = 60;         // xor
  tkNot = 61;         // not
  tkLowerThan = 62;   // <
  tkLowerEq = 63;     // <=
  tkGreaterThan = 64; // >
  tkGreaterEq = 65;   // >=
  tkNotEqual = 66;    // <>
  tkIn = 67;          // in
  tkIs = 68;          // is
  tkAs = 69;          // as

  tkRegister = 70;  // register
  tkCDecl = 71;     // cdecl
  tkPascal = 72;    // pascal
  tkStdCall = 73;   // stdcall
  tkSafeCall = 74;  // safecall
  tkAssembler = 75; // assembler

  tkName = 76;      // name
  tkIndex = 77;     // index
  tkRead = 78;      // read
  tkWrite = 79;     // write
  tkDefault = 80;   // default
  tkNoDefault = 81; // nodefault
  tkStored = 82;    // stored
  tkDispID = 83;    // dispid
  tkReadOnly = 84;  // readonly
  tkWriteOnly = 85; // writeonly
  tkString = 86;    // string

  tkDeprecated = 87;  // deprecated
  tkPlatform = 88;    // platform

  tkOverload = 89;    // overload
  tkVirtual = 90;     // virtual
  tkDynamic = 91;     // dynamic
  tkMessage = 92;     // message
  tkOverride = 93;    // override
  tkAbstract = 94;    // abstract
  tkStatic = 95;      // static
  tkReintroduce = 96; // reintroduce
  tkInline = 97;      // inline

  tkImplementation = 98;  // implementation
  tkForward = 99;         // forward
  tkInitialization = 100; // initialization
  tkFinalization = 101;   // finalization

  tkIf = 102;        // if
  tkThen = 103;      // then
  tkElse = 104;      // else
  tkWhile = 105;     // while
  tkDo = 106;        // do
  tkRepeat = 107;    // repeat
  tkUntil = 108;     // until
  tkFor = 109;       // for
  tkTo = 110;        // to
  tkDownTo = 111;    // downto
  tkTry = 112;       // try
  tkExcept = 113;    // except
  tkOn = 114;        // on
  tkFinally = 115;   // finally
  tkRaise = 116;     // raise
  tkInherited = 117; // inherited
  tkWith = 118;      // with

  tkPreProcessor = 119; // pre-processor instruction

const
  /// ID message Directive du Compilateur : $MINEMUMSIZE ou $Z
  CDM_MINENUMSIZE = 1;

  /// ID message Direction du Compilateur : $ALIGN ou $A
  CDM_ALIGN = 2;

type
  TSepiDelphiBaseLexer = class;
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
    Type de message Directive du Compilateur : $ALIGN ou $A
    @author sjrd
    @version 1.0
  *}
  TCDMAlign = record
    MsgID: Word;       /// ID du message (CDM_ALIGN)
    MaxAlign: Integer; /// Nouvel alignment maximal
  end;

  {*
    Type d'instruction du pré-processuer
  *}
  TPreProcInstruction = (
    ppUnknown, ppToggles,
    ppDefine, ppUndef,
    ppIfDef, ppIfNDef, ppIf, ppElse, ppElseIf, ppEndIf, ppIfEnd,
    ppMinEnumSize, ppAlign,
    ppInclude
  );

  {*
    Noeud condition d'une instruction du pré-processeur $IF
    @author sjrd
    @version 1.0
  *}
  TPreProcIfConditionNode = class(TSepiHiddenNonTerminal)
  private
    FLexer: TSepiDelphiLexer; /// Analyseur propriétaire
  public
    function ResolveIdent(const Identifier: string): ISepiExpression; override;

    property Lexer: TSepiDelphiLexer read FLexer;
  end;

  {*
    Pseudo-routine d'opération sur un type
    @author sjrd
    @version 1.0
  *}
  TSepiDefinedDeclaredPseudoRoutine = class(TSepiCustomComputedValue,
    ISepiIdentifierTestPseudoRoutine)
  private
    FLexer: TSepiDelphiLexer; /// Analyseur du contexte
    FIsDeclared: Boolean;     /// False pour Defined ; True pour Declared
    FIdentifier: string;      /// Identificateur à tester
  protected
    procedure AttachToExpression(const Expression: ISepiExpression); override;

    procedure CompileCompute(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager); override;

    function GetIdentifier: string;
    procedure SetIdentifier(const Value: string);
  public
    constructor Create(ALexer: TSepiDelphiLexer; AIsDeclared: Boolean);

    procedure Complete;

    property Lexer: TSepiDelphiLexer read FLexer;
    property IsDeclared: Boolean read FIsDeclared;
    property Identifier: string read FIdentifier write FIdentifier;
  end;

  {*
    Analyseur lexical de base pour le langage Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiBaseLexer = class(TSepiCustomManualLexer)
  protected
    procedure IdentifyKeyword(const OrigKey: string;
      var SymbolClass: TSepiSymbolClass); override;

    procedure InitLexingProcs; override;

    procedure ActionSymbol;
    procedure ActionIdentifier;
    procedure ActionNumber;
    procedure ActionString;
    procedure ActionSingleLineComment;
    procedure ActionMultiLineComment;
  end;

  {*
    Analyseur lexical de base pour le langage Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLexer = class(TSepiCustomCompositeLexer)
  private
    FDefines: TStrings; /// Liste des defines

    function ReadCommand(out Param: string): TPreProcInstruction;
    procedure Skip(StopOnElseIf: Boolean = False);
    function EvalCondition(const Condition: string): Boolean;

    procedure DoMinEnumSize(const Param: string);
    procedure DoAlign(const Param: string);

    procedure DoInclude(const Param: string);

    procedure HandleToggle(Toggle, Value: Char);
    procedure HandleToggles(const Toggles: string);

    procedure PreProc;
  protected
    procedure DoNextTerminal; override;
  public
    constructor Create(AErrors: TSepiCompilerErrorList; const ACode: string;
      const AFileName: TFileName = ''); override;
    destructor Destroy; override;

    property Defines: TStrings read FDefines;
  end;

  {*
    Analyseur lexical de base pour le langage Delphi - interface uniquement
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiInterfaceLexer = class(TSepiDelphiLexer)
  private
    /// Étape du lexer
    FStep: (stNormal, stImplementation, stEnd, stFinalDot, stEof);

    FSpecialTerminal: TSepiTerminal;
  protected
    function GetCurTerminal: TSepiTerminal; override;

    procedure DoNextTerminal; override;
  public
    destructor Destroy; override;

    procedure ResetToBookmark(ABookmark: TSepiLexerBookmark;
      FreeBookmark: Boolean = True); override;
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
    'MINENUMSIZE', 'ALIGN',
    'INCLUDE'
  );

  PreProcShortInstrs: array[TPreProcInstruction] of string = (
    '', '', '', '', '', '', '', '', '', '', '', '',
    'A', 'I'
  );

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
      Lexer, False)).AttachToExpression(Result);
  end else if AnsiSameText(Identifier, 'Declared') then
  begin
    Result := MakeExpression;
    ISepiExpressionPart(TSepiDefinedDeclaredPseudoRoutine.Create(
      Lexer, True)).AttachToExpression(Result);
  end else
  begin
    Result := inherited ResolveIdent(Identifier);
  end;
end;

{-----------------------------------------}
{ TSepiDefinedDeclaredPseudoRoutine class }
{-----------------------------------------}

{*
  Crée la pseudo-routine
*}
constructor TSepiDefinedDeclaredPseudoRoutine.Create(
  ALexer: TSepiDelphiLexer; AIsDeclared: Boolean);
begin
  inherited Create;

  FLexer := ALexer;
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
  Complète la pseudo-routine
*}
procedure TSepiDefinedDeclaredPseudoRoutine.Complete;
var
  Value: Boolean;
begin
  Assert(Identifier <> '');

  SetValueType(UnitCompiler.SystemUnit.Boolean);
  AllocateConstant;

  if IsDeclared then
    Value := Lexer.Context.ResolveIdent(Identifier) <> nil
  else
    Value := Lexer.Defines.IndexOf(Identifier) >= 0;

  Boolean(ConstValuePtr^) := Value;
end;

{----------------------------}
{ TSepiDelphiBaseLexer class }
{----------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiDelphiBaseLexer.IdentifyKeyword(const OrigKey: string;
  var SymbolClass: TSepiSymbolClass);
var
  Key: string;
begin
  Key := LowerCase(OrigKey);

  case Key[1] of
    'a' : if Key = 'abstract'       then SymbolClass := tkAbstract else
          if Key = 'and'            then SymbolClass := tkAnd else
          if Key = 'array'          then SymbolClass := tkArray else
          if Key = 'as'             then SymbolClass := tkAs else
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
          if Key = 'in'             then SymbolClass := tkIn else
          if Key = 'inline'         then SymbolClass := tkInline else
          if Key = 'index'          then SymbolClass := tkIndex else
          if Key = 'inherited'      then SymbolClass := tkInherited else
          if Key = 'interface'      then SymbolClass := tkInterface else
          if Key = 'is'             then SymbolClass := tkIs;
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
          if Key = 'strict'         then SymbolClass := tkStrict else
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
    'w' : if Key = 'with'           then SymbolClass := tkWith else
          if Key = 'while'          then SymbolClass := tkWhile else
          if Key = 'write'          then SymbolClass := tkWrite else
          if Key = 'writeonly'      then SymbolClass := tkWriteOnly;
    'x' : if Key = 'xor'            then SymbolClass := tkXor;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiBaseLexer.InitLexingProcs;
var
  C: Char;
begin
  inherited;

  for C := #0 to #255 do
  begin
    case C of
      '(', ')', '[', ']', '=', ',', ':', ';', '.',
        '^', '@', '+', '-', '*', '/', '<', '>':
        LexingProcs[C] := ActionSymbol;
      'a'..'z', 'A'..'Z', '_', '&':
        LexingProcs[C] := ActionIdentifier;
      '0'..'9', '$':
        LexingProcs[C] := ActionNumber;
      '''', '#':
        LexingProcs[C] := ActionString;
      '{':
        LexingProcs[C] := ActionMultiLineComment;
    end;
  end;
end;

{*
  Analyse un symbole ou un commentaire
  @return True pour un symbole, False pour un commentaire
*}
procedure TSepiDelphiBaseLexer.ActionSymbol;
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
          ActionMultiLineComment;
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
          ActionSingleLineComment;
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
    Exit;
  end;

  TerminalParsed(SymbolClass, Repr);
end;

{*
  Analyse un identificateur
  @return True
*}
procedure TSepiDelphiBaseLexer.ActionIdentifier;
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
  while CharInSet(Code[Cursor], IdentChars) do
    CursorForward;

  Repr := Copy(Code, BeginPos, Cursor-BeginPos);
  SymbolClass := tkIdentifier;

  if not ForceIdent then
    IdentifyKeyword(Repr, SymbolClass);

  TerminalParsed(SymbolClass, Repr);
end;

{*
  Analyse un nombre
  @return True
*}
procedure TSepiDelphiBaseLexer.ActionNumber;
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
    until not CharInSet(Code[Cursor], HexChars);
  end else
  begin
    while CharInSet(Code[Cursor], NumberChars) do
      CursorForward;

    if (Code[Cursor] = '.') and CharInSet(Code[Cursor+1], NumberChars) then
    begin
      SymbolClass := tkFloat;
      repeat
        CursorForward;
      until not CharInSet(Code[Cursor], NumberChars);
    end;

    if CharInSet(Code[Cursor], ['e', 'E']) then
    begin
      SymbolClass := tkFloat;
      CursorForward;
      if CharInSet(Code[Cursor], ['+', '-']) then
        CursorForward;

      while CharInSet(Code[Cursor], NumberChars) do
        CursorForward;
    end;
  end;

  TerminalParsed(SymbolClass, Copy(Code, BeginPos, Cursor-BeginPos));
end;

{*
  Analyse une chaîne de caractères
  @return True
*}
procedure TSepiDelphiBaseLexer.ActionString;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  while CharInSet(Code[Cursor], StringChars) do
  begin
    case Code[Cursor] of
      '''':
      begin
        CursorForward;
        while Code[Cursor] <> '''' do
        begin
          if CharInSet(Code[Cursor], [#0, #10, #13]) then
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
        if not CharInSet(Code[Cursor], HexChars) then
          MakeError(SStringNotTerminated, ekFatalError);
        while CharInSet(Code[Cursor], HexChars) do
          CursorForward;
      end;
    end;
  end;

  TerminalParsed(tkStringCst, Copy(Code, BeginPos, Cursor-BeginPos));
end;

{*
  Analyse un commentaire sur une ligne
  @return False
*}
procedure TSepiDelphiBaseLexer.ActionSingleLineComment;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;
  while not CharInSet(Code[Cursor], [#0, #13, #10]) do
    CursorForward;

  TerminalParsed(tkComment, Copy(Code, BeginPos, Cursor-BeginPos));
end;

{*
  Analyse un commentaire sur plusieurs lignes
  @return True pour une instruction du pré-processuer, False sinon
*}
procedure TSepiDelphiBaseLexer.ActionMultiLineComment;
var
  BeginPos: Integer;
  Text: string;
begin
  BeginPos := Cursor;

  // Find end of comment
  if Code[Cursor] = '{' then
  begin
    while not CharInSet(Code[Cursor], [#0, '}']) do
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
  end else
  begin
    // Pure comment
    TerminalParsed(tkComment, Copy(Code, BeginPos, Cursor-BeginPos));
  end;
end;

{------------------------}
{ TSepiDelphiLexer class }
{------------------------}

{*
  [@inheritDoc]
*}
constructor TSepiDelphiLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '');
begin
  inherited;

  SetBaseLexer(TSepiDelphiBaseLexer.Create(AErrors, ACode, AFileName));

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

    {$IFDEF UNICODE}
    Add('UNICODE');
    {$ENDIF}
  end;
end;

{*
  [@inheritDoc]
*}
destructor TSepiDelphiLexer.Destroy;
begin
  FDefines.Free;

  inherited;
end;

{*
  Lit la commande du pré-processeur
  @param Param   En sortie : paramètre de la commande (ou '' si aucun)
  @return Numéro de la commande, ou -1 si inconnue
*}
function TSepiDelphiLexer.ReadCommand(out Param: string): TPreProcInstruction;
var
  Instr, Command: string;
  CommandIdx: Integer;
begin
  Instr := CurTerminal.Representation;
  SplitToken(Instr, ' ', Command, Param);

  if (Length(Command) >= 2) and CharInSet(Command[1], ['A'..'Z']) and
    CharInSet(Command[2], ['+', '-', '0'..'9']) then
  begin
    Param := Instr;
    Result := ppToggles;
  end else
  begin
    CommandIdx := AnsiIndexText(Command, PreProcInstrs);
    if CommandIdx < 0 then
      CommandIdx := AnsiIndexText(Command, PreProcShortInstrs);

    if CommandIdx < 0 then
      Result := ppUnknown
    else
      Result := TPreProcInstruction(CommandIdx);
  end;
end;

{*
  Élimine une portion de code, jusqu'au $ELSE, $ENDIF ou $IFEND correspondant
  @param StopOnElseIf   Si True, s'arrête aussi sur un $ELSEIF
*}
procedure TSepiDelphiLexer.Skip(StopOnElseIf: Boolean = False);
var
  Depth: Integer;
  Param: string;
begin
  Depth := 1;

  while Depth > 0 do
  begin
    // Skip to next compiler directive - do not cross file boundaries
    repeat
      CurLexer.NextTerminal;

      if CurLexer.IsEof then
        MakeError(SPreProcReachedEndOfFile, ekFatalError);
    until CurTerminal.SymbolClass = tkPreProcessor;

    // Update Depth following compiler directive kind
    case ReadCommand(Param) of
      ppIfDef, ppIfNDef, ppIf:
        Inc(Depth);
      ppEndIf, ppIfEnd:
        Dec(Depth);
      ppElse:
      begin
        { $ELSE decrements Depth and immediately increments it.  Therefore, it
          has no effect; unlesse Depth reaches 0 at this time. }
        if Depth = 1 then
          Break;
      end;
      ppElseIf:
      begin
        { $ELSEIF decrements Depth and immediately increments it.  Therefore,
          it has no effect; unless Depth reaches 0 at this time and we are
          asked to StopOnElseIf. }
        if (Depth = 1) and StopOnElseIf then
          Break;
      end;
    end;
  end;
end;

{*
  Évalue une condition booléenne constante dans le contexte courant
  @param Condition   Condition à tester
  @return Valeur de vérité de la condition
*}
function TSepiDelphiLexer.EvalCondition(const Condition: string): Boolean;
var
  RootNode: TPreProcIfConditionNode;
  ExprNode: TSepiConstExpressionNode;
begin
  if Context = nil then
    MakeError(SIFInstrNotSupported, ekFatalError);

  RootNode := TPreProcIfConditionNode.Create(Context,
    ntInPreProcessorExpression, CurrentPos);
  try
    RootNode.FLexer := Self;

    // Parse the condition expression
    TSepiDelphiParser.Parse(RootNode,
      TSepiDelphiBaseLexer.Create(Errors, Condition));

    // Fetch its value into Result
    ExprNode := RootNode.Children[0] as TSepiConstExpressionNode;
    if not ExprNode.CompileConst(Result, ExprNode.SystemUnit.Boolean) then
      MakeError(SErrorInIFInstr, ekFatalError);
  finally
    RootNode.Free;
  end;
end;

{*
  Applique la directive $MINENUMSIZE ou $Z
  @param Param   Paramètre
*}
procedure TSepiDelphiLexer.DoMinEnumSize(const Param: string);
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
    else
      Exit;
    end;
  end;

  Context.Dispatch(MinEnumSizeMsg);
end;

{*
  Applique la directive $ALIGN ou $A
  @param Param   Paramètre
*}
procedure TSepiDelphiLexer.DoAlign(const Param: string);
var
  AlignMsg: TCDMAlign;
begin
  AlignMsg.MsgID := CDM_ALIGN;

  if Param = '+' then
    AlignMsg.MaxAlign := 8
  else if Param = '-' then
    AlignMsg.MaxAlign := 1
  else
    AlignMsg.MaxAlign := StrToIntDef(Param, 8);

  Context.Dispatch(AlignMsg);
end;

{*
  Applique la directive $INCLUDE ou $I
  @param Param   Paramètre
*}
procedure TSepiDelphiLexer.DoInclude(const Param: string);
var
  FileName: TFileName;
begin
  if (Param <> '') and (Param[1] = '''') then
    FileName := StrRepresToStr(Param)
  else
    FileName := Param;

  EnterFile(FileName);
end;

{*
  Applique une directive de type toggle
  @param Toggle   Toggle à appliquer
  @param Value    Valeur du toggle
*}
procedure TSepiDelphiLexer.HandleToggle(Toggle, Value: Char);
begin
  case Toggle of
    'A': DoAlign(Value);
    'Z': DoMinEnumSize(Value);
  end;
end;

{*
  Applique une directive avec des toggles
  @param Param   Paramètre
*}
procedure TSepiDelphiLexer.HandleToggles(const Toggles: string);
var
  Remaining, Temp, Toggle: string;
begin
  Remaining := Toggles;
  while Remaining <> '' do
  begin
    SplitToken(Remaining, ',', Toggle, Temp);
    Remaining := Temp;

    Toggle := Trim(Toggle);
    if Length(Toggle) = 2 then
      HandleToggle(Toggle[1], Toggle[2]);
  end;
end;

{*
  Applique une instruction du pré-processeur
*}
procedure TSepiDelphiLexer.PreProc;
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
        Skip({StopOnElseIf =} True);
        Command := ReadCommand(Param);
      end;
    end;
    ppElse, ppElseIf:
      Skip;
    ppEndIf, ppIfEnd: ;
    ppMinEnumSize:
      DoMinEnumSize(Param);
    ppInclude:
      DoInclude(Param);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiLexer.DoNextTerminal;
begin
  repeat
    inherited;

    if CurTerminal.SymbolClass = tkPreProcessor then
      PreProc;
  until CurTerminal.SymbolClass <> tkPreProcessor;
end;

{---------------------------------}
{ TSepiDelphiInterfaceLexer class }
{---------------------------------}

{*
  [@inheritDoc]
*}
destructor TSepiDelphiInterfaceLexer.Destroy;
begin
  FreeAndNil(FSpecialTerminal);

  inherited;
end;

{*
  [@inheritDoc]
*}
function TSepiDelphiInterfaceLexer.GetCurTerminal: TSepiTerminal;
begin
  if FStep in [stNormal, stImplementation] then
    Result := inherited GetCurTerminal
  else
    Result := FSpecialTerminal;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiInterfaceLexer.DoNextTerminal;
begin
  if FStep = stNormal then
  begin
    inherited;

    if CurTerminal.SymbolClass = tkImplementation then
      FStep := stImplementation;
  end else if FStep <> stEof then
  begin
    FStep := Succ(FStep);
    FreeAndNil(FSpecialTerminal);

    case FStep of
      stEnd:
        FSpecialTerminal := TSepiTerminal.Create(tkEnd, CurrentPos, 'end');
      stFinalDot:
        FSpecialTerminal := TSepiTerminal.Create(tkDot, CurrentPos, '.');
      stEof:
        FSpecialTerminal := TSepiTerminal.Create(tkEof, CurrentPos, SEndOfFile);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiDelphiInterfaceLexer.ResetToBookmark(
  ABookmark: TSepiLexerBookmark; FreeBookmark: Boolean);
begin
  inherited;

  FStep := stNormal;
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
  SymbolClassNames[tkIn] := 'tkIn';
  SymbolClassNames[tkIs] := 'tkIs';
  SymbolClassNames[tkAs] := 'tkAs';

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
  SymbolClassNames[tkWith] := 'tkWith';
end.

