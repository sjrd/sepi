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
  Analyseur tkical d'une unité Delphi
  @author sjrd
  @version 1.0
*}
unit SepiDelphiLexer;

interface

{$D-,L-}

uses
  Classes, SysUtils, StrUtils, ScStrUtils, SepiCompilerErrors, SepiParseTrees,
  SepiLexerUtils, SepiCompilerConsts;

resourcestring
  SIFInstrNotSupported = 'Instruction du pré-processeur $IF non supportée';
  SPreProcReachedEndOfFile = 'Fin de fichier atteinte par le pré-processeur';

const
  FirstTerminal = 0;
  LastTerminal = 99;

  tkEof = SepiLexerUtils.tkEof; /// Fin de fichier

  tkIdentifier = 1; // Identificateur
  tkInteger = 2;    // Nombre entier
  tkFloat = 3;      // Nombre en virgule flottante
  tkStringCst = 4;  // Chaîne de caractères

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

  tkRegister = 64; // register
  tkCDecl = 65;    // cdecl
  tkPascal = 66;   // pascal
  tkStdCall = 67;  // stdcall
  tkSafeCall = 68; // safecall

  tkName = 69;      // name
  tkIndex = 70;     // index
  tkRead = 71;      // read
  tkWrite = 72;     // write
  tkDefault = 73;   // default
  tkNoDefault = 74; // nodefault
  tkStored = 75;    // stored
  tkDispID = 76;    // dispid
  tkReadOnly = 77;  // readonly
  tkWriteOnly = 78; // writeonly
  tkString = 79;    // string

  tkImplementation = 80; // implementation
  tkForward = 81;        // forward
  tkInitialization = 82; // initialization
  tkFinalization = 83;   // finalization

  tkIf = 84;        // if
  tkThen = 85;      // then
  tkElse = 86;      // else
  tkWhile = 87;     // while
  tkDo = 88;        // do
  tkRepeat = 89;    // repeat
  tkUntil = 90;     // until
  tkFor = 91;       // for
  tkTo = 92;        // to
  tkDownTo = 93;    // downto
  tkTry = 94;       // try
  tkExcept = 95;    // except
  tkOn = 96;        // on
  tkFinally = 97;   // finally
  tkRaise = 98;     // raise
  tkInherited = 99; // inherited

  tkPreProcessor = -1; // pre-processor instruction

type
  TSepiDelphiLexer = class;

  {*
    Pré-processeur
    @author sjrd
    @version 1.0
  *}
  TPreProcessor = class(TObject)
  private
    FLexer: TSepiDelphiLexer; /// Analyseur propriétaire
    FDefines: TStrings;       /// Liste des defines

    procedure Skip;
  protected
  public
    constructor Create(ALexer: TSepiDelphiLexer);
    destructor Destroy; override;

    procedure PreProc;

    property Lexer: TSepiDelphiLexer read FLexer;
    property Defines: TStrings read FDefines;
  end;

  {*
    Analyseur lexical pour le langage Delphi
    @author sjrd
    @version 1.0
  *}
  TSepiDelphiLexer = class(TSepiCustomManualLexer)
  private
    FPreProcessor: TPreProcessor; /// Pré-processeur
    FInterfaceOnly: Boolean;      /// Considérer 'implementation' comme Eof
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
      const AFileName: TFileName = ''; AInterfaceOnly: Boolean = False);
    destructor Destroy; override;

    procedure NextTerminal; override;

    property InterfaceOnly: Boolean read FInterfaceOnly;
  end;

var
  /// Symbol class names, indexed by their IDs
  SymbolClassNames: array of string;

implementation

const
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
  Crée un nouveau pré-processuer
  @param ALexer   Analyseur lexical propriétaire
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
  Élimine une portion de code, jusqu'au ELSE ou ENDIF correspondant
*}
procedure TPreProcessor.Skip;
var
  Depth: Integer;
  InstrStr: string;
begin
  Depth := 1;

  while Depth > 0 do
  begin
    Lexer.NextPreProc;
    InstrStr := GetFirstToken(Lexer.CurTerminal.Representation, ' ');

    case AnsiIndexText(InstrStr, PreProcInstrs) of
      ppIfDef, ppIfNDef: Inc(Depth);
      ppElse, ppEndIf: Dec(Depth);
    end;
  end;
end;

{*
  Applique une instruction du pré-processeur
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

{------------------------}
{ TSepiDelphiLexer class }
{------------------------}

{*
  Crée un nouvel analyseur lexical
  @param AErrors          Erreurs de compilation
  @param ACode            Code source à analyser
  @param AFileName        Nom du fichier source
  @param AInterfaceOnly   Si True, considère 'implementation' come un Eof
*}
constructor TSepiDelphiLexer.Create(AErrors: TSepiCompilerErrorList;
  const ACode: string; const AFileName: TFileName = '';
  AInterfaceOnly: Boolean = False);
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
    'a' : if Key = 'and'            then SymbolClass := tkAnd else
          if Key = 'array'          then SymbolClass := tkArray;
    'b' : if Key = 'begin'          then SymbolClass := tkBegin;
    'c' : if Key = 'case'           then SymbolClass := tkCase else
          if Key = 'cdecl'          then SymbolClass := tkCDecl else
          if Key = 'class'          then SymbolClass := tkClass else
          if Key = 'const'          then SymbolClass := tkConst else
          if Key = 'constructor'    then SymbolClass := tkConstructor;
    'd' : if Key = 'default'        then SymbolClass := tkDefault else
          if Key = 'destructor'     then SymbolClass := tkDestructor else
          if Key = 'dispid'         then SymbolClass := tkDispID else
          if Key = 'dispinterface'  then SymbolClass := tkDispInterface else
          if Key = 'div'            then SymbolClass := tkDiv else
          if Key = 'do'             then SymbolClass := tkDo else
          if Key = 'downto'         then SymbolClass := tkDownto;
    'e' : if Key = 'else'           then SymbolClass := tkElse else
          if Key = 'end'            then SymbolClass := tkEnd else
          if Key = 'except'         then SymbolClass := tkExcept;
    'f' : if Key = 'finally'        then SymbolClass := tkFinally else
          if Key = 'for'            then SymbolClass := tkFor else
          if Key = 'forward'        then SymbolClass := tkForward else
          if Key = 'function'       then SymbolClass := tkFunction;
    'i' : if Key = 'if'             then SymbolClass := tkIf else
          if Key = 'implementation' then SymbolClass := tkImplementation else
          if Key = 'index'          then SymbolClass := tkIndex else
          if Key = 'inherited'      then SymbolClass := tkInherited else
          if Key = 'interface'      then SymbolClass := tkInterface;
    'm' : if Key = 'mod'            then SymbolClass := tkMod;
    'n' : if Key = 'name'           then SymbolClass := tkName else
          if Key = 'nil'            then SymbolClass := tkNil else
          if Key = 'nodefault'      then SymbolClass := tkNoDefault else
          if Key = 'not'            then SymbolClass := tkNot;
    'o' : if Key = 'object'         then SymbolClass := tkObject else
          if Key = 'of'             then SymbolClass := tkOf else
          if Key = 'on'             then SymbolClass := tkOn else
          if Key = 'or'             then SymbolClass := tkOr else
          if Key = 'out'            then SymbolClass := tkOut;
    'p' : if Key = 'packed'         then SymbolClass := tkPacked else
          if Key = 'pascal'         then SymbolClass := tkPascal else
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
          if Key = 'repeat'         then SymbolClass := tkRepeat else
          if Key = 'resourcestring' then SymbolClass := tkResourceString;
    's' : if Key = 'safecall'       then SymbolClass := tkSafeCall else
          if Key = 'stdcall'        then SymbolClass := tkStdCall else
          if Key = 'stored'         then SymbolClass := tkStored else
          if Key = 'set'            then SymbolClass := tkSet else
          if Key = 'shl'            then SymbolClass := tkShl else
          if Key = 'shr'            then SymbolClass := tkShr else
          if Key = 'string'         then SymbolClass := tkString;
    't' : if Key = 'then'           then SymbolClass := tkThen else
          if Key = 'to'             then SymbolClass := tkTo else
          if Key = 'try'            then SymbolClass := tkTry else
          if Key = 'type'           then SymbolClass := tkType;
    'u' : if Key = 'unit'           then SymbolClass := tkUnit else
          if Key = 'until'          then SymbolClass := tkUntil else
          if Key = 'uses'           then SymbolClass := tkUses;
    'v' : if Key = 'var'            then SymbolClass := tkVar;
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
  Analyse une chaîne de caractères
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
  @return True pour une instruction du pré-processuer, False sinon
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
  Analyse le code source jusqu'à trouver une instruction du pré-processeur
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
  SymbolClassNames[tkRange] := 'tkRange';
  SymbolClassNames[tkDot] := 'tkDot';
  SymbolClassNames[tkHat] := 'tkHat';

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
  SymbolClassNames[tkClass] := 'tkClass';
  SymbolClassNames[tkPrivate] := 'tkPrivate';
  SymbolClassNames[tkProtected] := 'tkProtected';
  SymbolClassNames[tkPublic] := 'tkPublic';
  SymbolClassNames[tkPublished] := 'tkPublished';
  SymbolClassNames[tkEnd] := 'tkEnd';

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

  SymbolClassNames[tkRegister] := 'tkRegister';
  SymbolClassNames[tkCDecl] := 'tkCDecl';
  SymbolClassNames[tkPascal] := 'tkPascal';
  SymbolClassNames[tkStdCall] := 'tkStdCall';
  SymbolClassNames[tkSafeCall] := 'tkSafeCall';

  SymbolClassNames[tkNoDefault] := 'tkNoDefault';
  SymbolClassNames[tkString] := 'tkString';
end.

