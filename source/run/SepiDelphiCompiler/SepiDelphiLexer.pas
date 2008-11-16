{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
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
  Analyseur lexical d'une unité Delphi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiLexer;

interface

{$D-,L-}

uses
  Classes, SysUtils, StrUtils, ScStrUtils, SepiCompilerErrors, SepiParseTrees;

resourcestring
  SIFInstrNotSupported = 'Instruction du pré-processeur $IF non supportée';
  SBadSourceCharacter = 'Caractère %s incorrect dans un source';
  SEndOfFile = 'Fin de fichier';
  SInternalError = 'Erreur interne';
  SUnterminatedString = 'Chaîne non terminée';
  SPreProcReachedEndOfFile = 'Fin de fichier atteinte par le pré-processeur';

const
  FirstTerminal = 0;
  LastTerminal = 94;

  lexEof = 0;        // End of file
  lexIdentifier = 1; // Identifier
  lexInteger = 2;    // Integer
  lexFloat = 3;      // Floating point number
  lexStringCst = 4;  // String

  lexOpenBracket = 5;    // (
  lexCloseBracket = 6;   // )
  lexOpenSqBracket = 7;  // [
  lexCloseSqBracket = 8; // ]
  lexEquals = 9;         // =
  lexComma = 10;         // ,
  lexColon = 11;         // :
  lexSemiColon = 12;     // ;
  lexDot = 13;           // .
  lexRange = 14;         // ..
  lexHat = 15;           // ^
  lexAt = 16;            // @
  lexAssign = 17;        // :=

  lexUnit = 18;           // unit
  lexUses = 19;           // uses
  lexType = 20;           // type
  lexConst = 21;          // const
  lexResourceString = 22; // resourcestring
  lexVar = 23;            // var
  lexOut = 24;            // out

  lexArray = 25;         // array
  lexSet = 26;           // set
  lexOf = 27;            // of
  lexObject = 28;        // object
  lexPacked = 29;        // packed
  lexRecord = 30;        // record
  lexCase = 31;          // case
  lexInterface = 32;     // interface
  lexDispInterface = 33; // dispinterface
  lexClass = 34;         // class
  lexPrivate = 35;       // private
  lexProtected = 36;     // protected
  lexPublic = 37;        // public
  lexPublished = 38;     // published
  lexBegin = 39;         // begin
  lexEnd = 40;           // end
  lexNil = 41;           // nil

  lexProcedure = 42;   // procedure
  lexFunction = 43;    // function
  lexProperty = 44;    // property
  lexConstructor = 45; // constructor
  lexDestructor = 46;  // destructor

  lexPlus = 47;   // +
  lexMinus = 48;  // -
  lexTimes = 49;  // *
  lexDivide = 50; // /
  lexDiv = 51;    // div
  lexMod = 52;    // mod
  lexShl = 53;    // shl
  lexShr = 54;    // shr
  lexOr = 55;     // or
  lexAnd = 56;    // and
  lexXor = 57;    // xor
  lexNot = 58;    // not

  lexRegister = 59; // register
  lexCDecl = 60;    // cdecl
  lexPascal = 61;   // pascal
  lexStdCall = 62;  // stdcall
  lexSafeCall = 63; // safecall

  lexName = 64;      // name
  lexIndex = 65;     // index
  lexRead = 66;      // read
  lexWrite = 67;     // write
  lexDefault = 68;   // default
  lexNoDefault = 69; // nodefault
  lexStored = 70;    // stored
  lexDispID = 71;    // dispid
  lexReadOnly = 72;  // readonly
  lexWriteOnly = 73; // writeonly
  lexString = 74;    // string

  lexImplementation = 75; // implementation
  lexForward = 76;        // forward
  lexInitialization = 77; // initialization
  lexFinalization = 78;   // finalization

  lexIf = 79;        // if
  lexThen = 80;      // then
  lexElse = 81;      // else
  lexWhile = 82;     // while
  lexDo = 83;        // do
  lexRepeat = 84;    // repeat
  lexUntil = 85;     // until
  lexFor = 86;       // for
  lexTo = 87;        // to
  lexDownTo = 88;    // downto
  lexTry = 89;       // try
  lexExcept = 90;    // except
  lexOn = 91;        // on
  lexFinally = 92;   // finally
  lexRaise = 93;     // raise
  lexInherited = 94; // inherited

  lexPreProcessor = -1; // pre-processor instruction

type
  {*
    Lexing function
    @return True if a terminal has been processed, False otherwise
  *}
  TLexingFunc = function: Boolean of object;

  {*
    Lexer exception, raised in case of lexical error
    @author sjrd
    @version 1.0
  *}
  ELexicalError = class(Exception);

  TLexer = class;

  {*
    Pre-processor
    @author sjrd
    @version 1.0
  *}
  TPreProcessor = class(TObject)
  private
    Lexer: TLexer;     /// Owning lexer
    Defines: TStrings; /// Defines

    procedure Skip;
  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;

    procedure PreProc;
  end;

  {*
    Lexer bookmark - used to go back into the code
    @author sjrd
    @version 1.0
  *}
  TLexerBookmark = class(TObject)
  private
    FCursor: Integer;                /// Cursor in the source code
    FSourcePos: TSepiSourcePosition; /// Position in the source

    FTerminalClass: TSepiSymbolClass;  /// Classe du terminal
    FTerminalPos: TSepiSourcePosition; /// Terminal position
    FTerminalRepr: string;             /// Terminal representation
  public
    constructor Create(ACursor: Integer; const ASourcePos: TSepiSourcePosition;
      ATerminalClass: TSepiSymbolClass;
      const ATerminalPos: TSepiSourcePosition; const ATerminalRepr: string);

    property Cursor: Integer read FCursor;
    property SourcePos: TSepiSourcePosition read FSourcePos;

    property TerminalClass: TSepiSymbolClass read FTerminalClass;
    property TerminalPos: TSepiSourcePosition read FTerminalPos;
    property TerminalRepr: string read FTerminalRepr;
  end;

  {*
    Lexer
    @author sjrd
    @version 1.0
  *}
  TLexer = class(TObject)
  private
    FErrors: TSepiCompilerErrorList;  /// Errors
    Code: string;                     /// Source code being analyzed
    Cursor: Integer;                  /// Current position in the source code
    FCurrentPos: TSepiSourcePosition; /// Current position
    FCurTerminal: TSepiTerminal;      /// Lastly analyzed terminal
    PreProcessor: TPreProcessor;      /// Pre-processor
    FInterfaceOnly: Boolean;          /// Only parse interface

    /// Character-indexed table of lexing functions
    LexingFuncs: array[#0..#255] of TLexingFunc;

    procedure MakeError(const ErrorMsg: string;
      Kind: TSepiErrorKind = ekError);

    procedure MakeTerminal(SymbolClass: TSepiSymbolClass;
      const Representation: string);

    function ActionUnknown: Boolean;
    function ActionEof: Boolean;
    function ActionBlank: Boolean;
    function ActionSymbol: Boolean;
    function ActionIdentifier: Boolean;
    function ActionNumber: Boolean;
    function ActionString: Boolean;
    function ActionSingleLineComment: Boolean;
    function ActionMultiLineComment: Boolean;

    procedure NextPreProc;
  public
    constructor Create(AErrors: TSepiCompilerErrorList; const ACode: string;
      const AFileName: TFileName = ''; AInterfaceOnly: Boolean = False);
    destructor Destroy; override;

    procedure Next;

    function MakeBookmark: TLexerBookmark;
    procedure ResetToBookmark(Bookmark: TLexerBookmark;
      FreeBookmark: Boolean = True);

    property Errors: TSepiCompilerErrorList read FErrors;

    property CurTerminal: TSepiTerminal read FCurTerminal;
    property CurrentPos: TSepiSourcePosition read FCurrentPos;

    property InterfaceOnly: Boolean read FInterfaceOnly;
  end;

var
  /// Symbol class names, indexed by their IDs
  SymbolClassNames: array of string;

implementation

const
  BlankChars = [#9, #10, #13, ' '];
  IdentChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  NumberChars = ['0'..'9'];
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
  StringChars = ['''', '#'];

  PreProcInstrs: array[0..6] of string = (
    'DEFINE', 'UNDEF', 'IFDEF', 'IFNDEF', 'ELSE', 'ENDIF', 'IF'
  );

  ppDefine = 0;
  ppUndef = 1;
  ppIfDef = 2;
  ppIfNDef = 3;
  ppElse = 4;
  ppEndIF = 5;
  ppIf = 6; // not supported


{---------------------}
{ TPreProcessor class }
{---------------------}

{*
  Creates a new pre-processor
  @param ALexer   Owning lexer
*}
constructor TPreProcessor.Create(ALexer: TLexer);
begin
  inherited Create;
  Lexer := ALexer;

  Defines := TStringList.Create;
  with TStringList(Defines) do
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
  Defines.Free;
  inherited;
end;

{*
  Skips a portion a source code, until the matching ELSE or ENDIF instruction
*}
procedure TPreProcessor.Skip;
var
  Depth: Integer;
begin
  Depth := 1;

  while Depth > 0 do
  begin
    Lexer.NextPreProc;
    case AnsiIndexText(
      GetFirstToken(Lexer.CurTerminal.Representation, ' '), PreProcInstrs) of
      ppIfDef, ppIfNDef: Inc(Depth);
      ppElse, ppEndIf: Dec(Depth);
    end;
  end;
end;

{*
  Handles a pre-processor instruction
*}
procedure TPreProcessor.PreProc;
var
  Instr, Command, Param: string;
  Defined: Boolean;
begin
  Instr := Lexer.CurTerminal.Representation;
  if not SplitToken(Instr, ' ', Command, Param) then
    Param := '';
  Defined := Defines.IndexOf(Param) >= 0;

  case AnsiIndexText(Command, PreProcInstrs) of
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
    ppElse:
      Skip;
    ppEndIf: ;
    ppIf:
      Lexer.MakeError(SIFInstrNotSupported, ekFatalError);
  end;
end;

{----------------------}
{ TLexerBookmark class }
{----------------------}

constructor TLexerBookmark.Create(ACursor: Integer;
  const ASourcePos: TSepiSourcePosition; ATerminalClass: TSepiSymbolClass;
  const ATerminalPos: TSepiSourcePosition; const ATerminalRepr: string);
begin
  inherited Create;

  FCursor := ACursor;
  FSourcePos := ASourcePos;

  FTerminalClass := ATerminalClass;
  FTerminalPos := ATerminalPos;
  FTerminalRepr := ATerminalRepr;
end;

{--------------}
{ TLexer class }
{--------------}

{*
  Creates a new lexer and analyze the first terminal
  @param ACode   Source code to analyze
*}
constructor TLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '';
  AInterfaceOnly: Boolean = False);
var
  C: Char;
begin
  inherited Create;

  FErrors := AErrors;
  Code := ACode+#0;
  Cursor := 1;

  FCurrentPos.FileName := AFileName;
  FCurrentPos.Line := 1;
  FCurrentPos.Col := 1;

  FCurTerminal := nil;

  FInterfaceOnly := AInterfaceOnly;

  for C := #0 to #255 do
  begin
    case C of
      #0:
        LexingFuncs[C] := ActionEof;
      #9, #10, #13, ' ':
        LexingFuncs[C] := ActionBlank;
      '(', ')', '[', ']', '=', ',', ':', ';', '.',
        '^', '@', '+', '-', '*', '/':
        LexingFuncs[C] := ActionSymbol;
      'a'..'z', 'A'..'Z', '_', '&':
        LexingFuncs[C] := ActionIdentifier;
      '0'..'9', '$':
        LexingFuncs[C] := ActionNumber;
      '''', '#':
        LexingFuncs[C] := ActionString;
      '{':
        LexingFuncs[C] := ActionMultiLineComment;
    else
      LexingFuncs[C] := ActionUnknown;
    end;
  end;

  PreProcessor := TPreProcessor.Create(Self);

  Next;
end;

{*
  [@inheritDoc]
*}
destructor TLexer.Destroy;
begin
  PreProcessor.Free;
  FCurTerminal.Free;
  inherited;
end;

{*
  Produit une erreur
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur (défaut = Erreur)
*}
procedure TLexer.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind = ekError);
begin
  Errors.MakeError(ErrorMsg, Kind, CurrentPos);
end;

{*
  Produit un terminal
  @param SymbolClass   Class de symbole
*}
procedure TLexer.MakeTerminal(SymbolClass: TSepiSymbolClass;
  const Representation: string);
begin
  FCurTerminal := TSepiTerminal.Create(SymbolClass, CurrentPos,
    Representation);
end;

{*
  Raises a lexical error
  @return Nothing
  @raise ELexicalError
*}
function TLexer.ActionUnknown: Boolean;
begin
  MakeError(Format(SBadSourceCharacter, [Code[Cursor]]), ekFatalError);
  Result := False;
end;

{*
  Analyzes an Eof terminal
  @return True
*}
function TLexer.ActionEof: Boolean;
begin
  MakeTerminal(lexEof, SEndOfFile);
  Result := True;
end;

{*
  Analyzes a blank
  @return False
*}
function TLexer.ActionBlank: Boolean;
begin
  while Code[Cursor] in BlankChars do
  begin
    if Code[Cursor] in [#10, #13] then
    begin
      if Code[Cursor] = #13 then
        Inc(Cursor);
      if Code[Cursor] = #10 then
        Inc(Cursor);
      Inc(FCurrentPos.Line);
      FCurrentPos.Col := 1;
    end else
    begin
      Inc(Cursor);
      Inc(FCurrentPos.Col);
    end;
  end;
  Result := False;
end;

{*
  Analyzes a symbol or a comment
  @return True for a symbol, False for a comment
*}
function TLexer.ActionSymbol: Boolean;
var
  BeginPos: Integer;
  Repr: string;
  SymbolClass: TSepiSymbolClass;
begin
  BeginPos := Cursor;

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
        Inc(Cursor);
        Repr := '(';
        SymbolClass := lexOpenBracket;
      end;
    end;
    ')':
    begin
      Inc(Cursor);
      Repr := ')';
      SymbolClass := lexCloseBracket;
    end;
    '[':
    begin
      Inc(Cursor);
      Repr := '[';
      SymbolClass := lexOpenSqBracket;
    end;
    ']':
    begin
      Inc(Cursor);
      Repr := ']';
      SymbolClass := lexCloseSqBracket;
    end;
    '=':
    begin
      Inc(Cursor);
      Repr := '=';
      SymbolClass := lexEquals;
    end;
    ',':
    begin
      Inc(Cursor);
      Repr := ',';
      SymbolClass := lexComma;
    end;
    ':':
    begin
      Inc(Cursor);
      if Code[Cursor] = '=' then
      begin
        Inc(Cursor);
        Repr := ':=';
        SymbolClass := lexAssign;
      end else
      begin
        Repr := ':';
        SymbolClass := lexColon;
      end;
    end;
    ';':
    begin
      Inc(Cursor);
      Repr := ';';
      SymbolClass := lexSemiColon;
    end;
    '.':
    begin
      case Code[Cursor+1] of
        '.':
        begin
          Inc(Cursor, 2);
          Repr := '..';
          SymbolClass := lexRange;
        end;
      else
        Inc(Cursor);
        Repr := '.';
        SymbolClass := lexDot;
      end;
    end;
    '^':
    begin
      Inc(Cursor);
      Repr := '^';
      SymbolClass := lexHat;
    end;
    '@':
    begin
      Inc(Cursor);
      Repr := '@';
      SymbolClass := lexAt;
    end;
    '+':
    begin
      Inc(Cursor);
      Repr := '+';
      SymbolClass := lexPlus;
    end;
    '-':
    begin
      Inc(Cursor);
      Repr := '-';
      SymbolClass := lexMinus;
    end;
    '*':
    begin
      Inc(Cursor);
      Repr := '*';
      SymbolClass := lexTimes;
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
        Inc(Cursor);
        Repr := '/';
        SymbolClass := lexDivide;
      end;
    end;
  else
    Assert(False);
    Result := False;
    Exit;
  end;

  MakeTerminal(SymbolClass, Repr);
  Inc(FCurrentPos.Col, Cursor-BeginPos);
  Result := True;
end;

{*
  Analyzes an identifier
  @return True
*}
function TLexer.ActionIdentifier: Boolean;
var
  ForceIdent: Boolean;
  BeginPos: Integer;
  Repr, Key: string;
  SymbolClass: TSepiSymbolClass;
begin
  ForceIdent := Code[Cursor] = '&';
  if ForceIdent then
    Inc(Cursor);

  BeginPos := Cursor;
  Inc(Cursor);
  while Code[Cursor] in IdentChars do
    Inc(Cursor);

  Repr := Copy(Code, BeginPos, Cursor-BeginPos);
  Key := LowerCase(Repr);
  SymbolClass := lexIdentifier;

  if not ForceIdent then
  begin
    case Key[1] of
      'a' : if Key = 'and'            then SymbolClass := lexAnd else
            if Key = 'array'          then SymbolClass := lexArray;
      'b' : if Key = 'begin'          then SymbolClass := lexBegin;
      'c' : if Key = 'case'           then SymbolClass := lexCase else
            if Key = 'cdecl'          then SymbolClass := lexCDecl else
            if Key = 'class'          then SymbolClass := lexClass else
            if Key = 'const'          then SymbolClass := lexConst else
            if Key = 'constructor'    then SymbolClass := lexConstructor;
      'd' : if Key = 'default'        then SymbolClass := lexDefault else
            if Key = 'destructor'     then SymbolClass := lexDestructor else
            if Key = 'dispid'         then SymbolClass := lexDispID else
            if Key = 'dispinterface'  then SymbolClass := lexDispInterface else
            if Key = 'div'            then SymbolClass := lexDiv else
            if Key = 'do'             then SymbolClass := lexDo else
            if Key = 'downto'         then SymbolClass := lexDownto;
      'e' : if Key = 'else'           then SymbolClass := lexElse else
            if Key = 'end'            then SymbolClass := lexEnd else
            if Key = 'except'         then SymbolClass := lexExcept;
      'f' : if Key = 'finally'        then SymbolClass := lexFinally else
            if Key = 'for'            then SymbolClass := lexFor else
            if Key = 'forward'        then SymbolClass := lexForward else
            if Key = 'function'       then SymbolClass := lexFunction;
      'i' : if Key = 'if'             then SymbolClass := lexIf else
            if Key = 'implementation' then SymbolClass := lexImplementation else
            if Key = 'index'          then SymbolClass := lexIndex else
            if Key = 'inherited'      then SymbolClass := lexInherited else
            if Key = 'interface'      then SymbolClass := lexInterface;
      'm' : if Key = 'mod'            then SymbolClass := lexMod;
      'n' : if Key = 'name'           then SymbolClass := lexName else
            if Key = 'nil'            then SymbolClass := lexNil else
            if Key = 'nodefault'      then SymbolClass := lexNoDefault else
            if Key = 'not'            then SymbolClass := lexNot;
      'o' : if Key = 'object'         then SymbolClass := lexObject else
            if Key = 'of'             then SymbolClass := lexOf else
            if Key = 'on'             then SymbolClass := lexOn else
            if Key = 'or'             then SymbolClass := lexOr else
            if Key = 'out'            then SymbolClass := lexOut;
      'p' : if Key = 'packed'         then SymbolClass := lexPacked else
            if Key = 'pascal'         then SymbolClass := lexPascal else
            if Key = 'private'        then SymbolClass := lexPrivate else
            if Key = 'procedure'      then SymbolClass := lexProcedure else
            if Key = 'property'       then SymbolClass := lexProperty else
            if Key = 'protected'      then SymbolClass := lexProtected else
            if Key = 'public'         then SymbolClass := lexPublic else
            if Key = 'published'      then SymbolClass := lexPublished;
      'r' : if Key = 'raise'          then SymbolClass := lexRaise else
            if Key = 'read'           then SymbolClass := lexRead else
            if Key = 'readonly'       then SymbolClass := lexReadOnly else
            if Key = 'record'         then SymbolClass := lexRecord else
            if Key = 'register'       then SymbolClass := lexRegister else
            if Key = 'repeat'         then SymbolClass := lexRepeat else
            if Key = 'resourcestring' then SymbolClass := lexResourceString;
      's' : if Key = 'safecall'       then SymbolClass := lexSafeCall else
            if Key = 'stdcall'        then SymbolClass := lexStdCall else
            if Key = 'stored'         then SymbolClass := lexStored else
            if Key = 'set'            then SymbolClass := lexSet else
            if Key = 'shl'            then SymbolClass := lexShl else
            if Key = 'shr'            then SymbolClass := lexShr else
            if Key = 'string'         then SymbolClass := lexString;
      't' : if Key = 'then'           then SymbolClass := lexThen else
            if Key = 'to'             then SymbolClass := lexTo else
            if Key = 'try'            then SymbolClass := lexTry else
            if Key = 'type'           then SymbolClass := lexType;
      'u' : if Key = 'unit'           then SymbolClass := lexUnit else
            if Key = 'until'          then SymbolClass := lexUntil else
            if Key = 'uses'           then SymbolClass := lexUses;
      'v' : if Key = 'var'            then SymbolClass := lexVar;
      'w' : if Key = 'while'          then SymbolClass := lexWhile else
            if Key = 'write'          then SymbolClass := lexWrite else
            if Key = 'writeonly'      then SymbolClass := lexWriteOnly;
      'x' : if Key = 'xor'            then SymbolClass := lexXor;
    end;
  end;

  if (SymbolClass = lexImplementation) and InterfaceOnly then
    SymbolClass := lexEof;

  MakeTerminal(SymbolClass, Repr);
  Inc(FCurrentPos.Col, Cursor-BeginPos);
  Result := True;
end;

{*
  Analyzes a number
  @return True
*}
function TLexer.ActionNumber: Boolean;
var
  BeginPos: Integer;
  SymbolClass: TSepiSymbolClass;
begin
  BeginPos := Cursor;
  SymbolClass := lexInteger;

  if Code[Cursor] = '$' then
  begin
    repeat
      Inc(Cursor);
    until not (Code[Cursor] in HexChars);
  end else
  begin
    while Code[Cursor] in NumberChars do
      Inc(Cursor);

    if (Code[Cursor] = '.') and (Code[Cursor+1] in NumberChars) then
    begin
      SymbolClass := lexFloat;
      repeat
        Inc(Cursor);
      until not (Code[Cursor] in NumberChars);
    end;

    if Code[Cursor] in ['e', 'E'] then
    begin
      SymbolClass := lexFloat;
      Inc(Cursor);
      if Code[Cursor] in ['+', '-'] then
        Inc(Cursor);

      while Code[Cursor] in NumberChars do
        Inc(Cursor);
    end;
  end;

  MakeTerminal(SymbolClass, Copy(Code, BeginPos, Cursor-BeginPos));
  Inc(FCurrentPos.Col, Cursor-BeginPos);
  Result := True;
end;

{*
  Analyzes a string
  @return True
*}
function TLexer.ActionString: Boolean;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  while Code[Cursor] in StringChars do
  begin
    case Code[Cursor] of
      '''':
      begin
        Inc(Cursor);
        while Code[Cursor] <> '''' do
        begin
          if Code[Cursor] in [#0, #10, #13] then
            MakeError(SUnterminatedString, ekFatalError)
          else
            Inc(Cursor);
        end;
        Inc(Cursor);
      end;
      '#':
      begin
        Inc(Cursor);
        if Code[Cursor] = '$' then
          Inc(Cursor);
        if not (Code[Cursor] in HexChars) then
          MakeError(SUnterminatedString, ekFatalError);
        while Code[Cursor] in HexChars do
          Inc(Cursor);
      end;
    end;
  end;

  MakeTerminal(lexStringCst, Copy(Code, BeginPos, Cursor-BeginPos));
  Inc(FCurrentPos.Col, Cursor-BeginPos);
  Result := True;
end;

{*
  Analyzes a single-line comment
  @return False
*}
function TLexer.ActionSingleLineComment: Boolean;
begin
  while not (Code[Cursor] in [#0, #13, #10]) do
    Inc(Cursor);
  Result := False;
end;

{*
  Analyzes a multi-line comment
  @return True for a pre-processor instruction, False otherwise
*}
function TLexer.ActionMultiLineComment: Boolean;
var
  BeginPos: Integer;
  Text: string;
begin
  BeginPos := Cursor;
  if Code[Cursor] = '{' then
  begin
    while not (Code[Cursor] in [#0, '}']) do
      Inc(Cursor);
    Inc(Cursor);
  end else
  begin
    while ((Code[Cursor] <> '*') or (Code[Cursor+1] <> ')')) and
      (Code[Cursor] <> #0) do
      Inc(Cursor);
    Inc(Cursor, 2);
  end;

  Text := Copy(Code, BeginPos, Cursor-BeginPos);
  if Pos(#10, Text) > 0 then
  begin
    Inc(FCurrentPos.Line, NberCharInStr(#10, Text));
    FCurrentPos.Col := Length(GetLastToken(Text, #10))+1;
  end else
    Inc(FCurrentPos.Col, Cursor-BeginPos);

  Result := False;

  if Code[BeginPos] = '{' then
    Text := Copy(Text, 2, Length(Text)-2)
  else
    Text := Copy(Text, 3, Length(Text)-4);

  if (Text <> '') and (Text[1] = '$') then
  begin
    MakeTerminal(lexPreProcessor, Copy(Text, 2, MaxInt));
    Result := True;
  end;
end;

{*
  Analyzes further the source code, until a pre-processor instruction is found
*}
procedure TLexer.NextPreProc;
begin
  while (not LexingFuncs[Code[Cursor]]) or
    (CurTerminal.SymbolClass <> lexPreProcessor) do
  begin
    if CurTerminal.SymbolClass = lexEof then
      MakeError(SPreProcReachedEndOfFile, ekFatalError);
  end;
end;

{*
  Analyzes further the source code
*}
procedure TLexer.Next;
begin
  repeat
    FreeAndNil(FCurTerminal);

    while not LexingFuncs[Code[Cursor]] do;
    if CurTerminal.SymbolClass = lexPreProcessor then
      PreProcessor.PreProc;
  until CurTerminal.SymbolClass <> lexPreProcessor;
end;

{*
  Make a bookmark at current position
  @return Bookmark
*}
function TLexer.MakeBookmark: TLexerBookmark;
begin
  Result := TLexerBookmark.Create(Cursor, CurrentPos,
    CurTerminal.SymbolClass, CurTerminal.SourcePos, CurTerminal.Representation);
end;

{*
  Go back in the source to a given bookmark
  @param Bookmark       Bookmark
  @param FreeBookmark   If True, will free bookmark afterwards
*}
procedure TLexer.ResetToBookmark(Bookmark: TLexerBookmark;
  FreeBookmark: Boolean = True);
begin
  Cursor := Bookmark.Cursor;
  FCurrentPos := Bookmark.SourcePos;

  FCurTerminal.Free;
  FCurTerminal := TSepiTerminal.Create(Bookmark.TerminalClass,
    Bookmark.TerminalPos, Bookmark.TerminalRepr);
end;

initialization
  if Length(SymbolClassNames) < LastTerminal+1 then
    SetLength(SymbolClassNames, LastTerminal+1);

  SymbolClassNames[lexEof] := 'lexEof';
  SymbolClassNames[lexIdentifier] := 'lexIdentifier';
  SymbolClassNames[lexInteger] := 'lexInteger';
  SymbolClassNames[lexFloat] := 'lexFloat';
  SymbolClassNames[lexStringCst] := 'lexStringCst';

  SymbolClassNames[lexOpenBracket] := 'lexOpenBracket';
  SymbolClassNames[lexCloseBracket] := 'lexCloseBracket';
  SymbolClassNames[lexOpenSqBracket] := 'lexOpenSqBracket';
  SymbolClassNames[lexCloseSqBracket] := 'lexCloseSqBracket';
  SymbolClassNames[lexEquals] := 'lexEquals';
  SymbolClassNames[lexComma] := 'lexComma';
  SymbolClassNames[lexColon] := 'lexColon';
  SymbolClassNames[lexSemiColon] := 'lexSemiColon';
  SymbolClassNames[lexRange] := 'lexRange';
  SymbolClassNames[lexDot] := 'lexDot';
  SymbolClassNames[lexHat] := 'lexHat';

  SymbolClassNames[lexUnit] := 'lexUnit';
  SymbolClassNames[lexUses] := 'lexUses';
  SymbolClassNames[lexType] := 'lexType';
  SymbolClassNames[lexConst] := 'lexConst';
  SymbolClassNames[lexResourceString] := 'lexResourceString';
  SymbolClassNames[lexVar] := 'lexVar';
  SymbolClassNames[lexOut] := 'lexOut';

  SymbolClassNames[lexArray] := 'lexArray';
  SymbolClassNames[lexSet] := 'lexSet';
  SymbolClassNames[lexOf] := 'lexOf';
  SymbolClassNames[lexObject] := 'lexObject';
  SymbolClassNames[lexPacked] := 'lexPacked';
  SymbolClassNames[lexRecord] := 'lexRecord';
  SymbolClassNames[lexCase] := 'lexCase';
  SymbolClassNames[lexInterface] := 'lexInterface';
  SymbolClassNames[lexClass] := 'lexClass';
  SymbolClassNames[lexPrivate] := 'lexPrivate';
  SymbolClassNames[lexProtected] := 'lexProtected';
  SymbolClassNames[lexPublic] := 'lexPublic';
  SymbolClassNames[lexPublished] := 'lexPublished';
  SymbolClassNames[lexEnd] := 'lexEnd';

  SymbolClassNames[lexProcedure] := 'lexProcedure';
  SymbolClassNames[lexFunction] := 'lexFunction';
  SymbolClassNames[lexProperty] := 'lexProperty';
  SymbolClassNames[lexConstructor] := 'lexConstructor';
  SymbolClassNames[lexDestructor] := 'lexDestructor';

  SymbolClassNames[lexPlus] := 'lexPlus';
  SymbolClassNames[lexMinus] := 'lexMinus';
  SymbolClassNames[lexTimes] := 'lexTimes';
  SymbolClassNames[lexDivide] := 'lexDivide';
  SymbolClassNames[lexDiv] := 'lexDiv';
  SymbolClassNames[lexMod] := 'lexMod';
  SymbolClassNames[lexShl] := 'lexShl';
  SymbolClassNames[lexShr] := 'lexShr';
  SymbolClassNames[lexOr] := 'lexOr';
  SymbolClassNames[lexAnd] := 'lexAnd';
  SymbolClassNames[lexXor] := 'lexXor';

  SymbolClassNames[lexRegister] := 'lexRegister';
  SymbolClassNames[lexCDecl] := 'lexCDecl';
  SymbolClassNames[lexPascal] := 'lexPascal';
  SymbolClassNames[lexStdCall] := 'lexStdCall';
  SymbolClassNames[lexSafeCall] := 'lexSafeCall';

  SymbolClassNames[lexNoDefault] := 'lexNoDefault';
  SymbolClassNames[lexString] := 'lexString';
end.

