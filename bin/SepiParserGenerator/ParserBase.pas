unit Sepi{#LanguageName#}Parser;

interface

uses
  SysUtils, Contnrs, SepiCompilerErrors, SepiParseTrees, SepiLL1ParserUtils,
  Sepi{#LanguageName#}Lexer;

resourcestring
  SSyntaxError = '%s attendu mais %s trouvé';

const
  ChoiceCount = {#ChoiceCount#};
  FirstNonTerminal = {#FirstNonTerminal#};
  LastNonTerminal = {#LastNonTerminal#};

{#NonTerminalConsts#}
type
  {*
    Choice pushing function
    Pushes a choice to the preditive stack
  *}
  TPushChoiceProc = procedure of object;

  {*
    Parser
    @author sjrd
    @version 1.0
  *}
  TParser = class
  private
    Errors: TSepiCompilerErrorList; /// Errors
    Lexer: TLexer;                  /// Lexer
    CurTerminal: TSepiTerminal;     /// Current terminal
    Current: TSepiNonTerminal;      /// Current non-terminal
    Stack: TSepiLL1ParsingStack;    /// Parsing stack

    /// Push choice functions
    PushChoiceProcs: array[0..ChoiceCount-1] of TPushChoiceProc;

    procedure PushTry(AltRule: TRuleID);
    function SyntaxError(const Expected: string): TSepiNonTerminal;

{#PushChoiceProcsDecls#}
    procedure InternalParse(RootNode: TSepiParseTreeRootNode);
  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;

    class procedure Parse(RootNode: TSepiParseTreeRootNode;
      Lexer: TLexer); overload;
    class procedure Parse(RootNode: TSepiParseTreeRootNode;
      const Code: string; const FileName: string = ''); overload;
  end;

var
  NonTerminalClasses:
    array[FirstNonTerminal..LastNonTerminal] of TSepiNonTerminalClass;

implementation

type
  TExceptionClass = class of Exception;
  TParsingTable = array[FirstNonTerminal..LastNonTerminal,
    FirstTerminal..LastTerminal] of TRuleID;

  TTryTag = class(TObject)
  private
    FBookmark: TLexerBookmark;
    FAltRule: TRuleID;
    FCurrent: TSepiNonTerminal;
  public
    constructor Create(ABookmark: TLexerBookmark; AAltRule: TRuleID;
      ACurrent: TSepiNonTerminal);
    destructor Destroy; override;

    property Bookmark: TLexerBookmark read FBookmark;
    property AltRule: TRuleID read FAltRule;
    property Current: TSepiNonTerminal read FCurrent;
  end;

const
  scNextChildIsFake = -2;
  scBackToParent = -3;
  scPopTry = -4;

  ParsingTable: TParsingTable = (
{#ParsingTable#}  );

function IntInArray(Value: Integer; const IntArray: array of Integer): Boolean;
var
  LowIndex, HighIndex, MidIndex: Integer;
begin
  LowIndex := Low(IntArray);
  HighIndex := High(IntArray);

  while LowIndex <= HighIndex do
  begin
    MidIndex := (LowIndex+HighIndex) div 2;

    if Value < IntArray[MidIndex] then
      HighIndex := MidIndex-1
    else if Value > IntArray[MidIndex] then
      LowIndex := MidIndex+1
    else
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{---------------}
{ TTryTag class }
{---------------}

{*
  Create a try tag
  @param ABookmark   Lexer bookmark
  @param AAltRule    Alternative rule
*}
constructor TTryTag.Create(ABookmark: TLexerBookmark; AAltRule: TRuleID;
  ACurrent: TSepiNonTerminal);
begin
  inherited Create;

  FBookmark := ABookmark;
  FAltRule := AAltRule;
  FCurrent := ACurrent;
end;

{*
  [@inheritDoc]
*}
destructor TTryTag.Destroy;
begin
  FBookmark.Free;

  inherited;
end;

{---------------}
{ TParser class }
{---------------}

{*
  Create a new parser
  @param Lexer   Lexer
*}
constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  Errors := ALexer.Errors;
  Stack := TSepiLL1ParsingStack.Create(ntSource);

{#InitPushChoiceProcs#}
  Lexer := ALexer;
  CurTerminal := Lexer.CurTerminal;
end;

{*
  [@inheritDoc]
*}
destructor TParser.Destroy;
begin
  Lexer.Free;
  Stack.Free;
  inherited Destroy;
end;

{*
  Push un try sur la pile
  @param AltRule   Règle alternative
*}
procedure TParser.PushTry(AltRule: TRuleID);
begin
  Stack.PushTry(TTryTag.Create(
    Lexer.MakeBookmark, AltRule, Current));
end;

{*
  Raises a syntax error
  @param Expected   Expected terminal or non-terminal
*}
function TParser.SyntaxError(const Expected: string): TSepiNonTerminal;
var
  TryTag: TTryTag;
  I: Integer;
begin
  if Stack.IsInTry then
  begin
    TryTag := TTryTag(Stack.UnwindTry);
    try
      Lexer.ResetToBookmark(TryTag.Bookmark, False);
      CurTerminal := Lexer.CurTerminal;
      PushChoiceProcs[TryTag.AltRule];
      Result := TryTag.Current;

      for I := 0 to Result.ChildCount-1 do
        Result.Children[I].Free;
    finally
      TryTag.Free;
    end;
  end else
  begin
    Errors.MakeError(Format(SSyntaxError,
      [Expected, CurTerminal.Representation]), ekFatalError,
      CurTerminal.SourcePos);
    Result := nil;
  end;
end;

{#PushChoiceProcs#}{*
  Parses the code
  @param RootNode   Root node of the syntax tree
*}
procedure TParser.InternalParse(RootNode: TSepiParseTreeRootNode);
var
  Temp: TSepiNonTerminal;
  Symbol: TSepiSymbolClass;
  Rule: TRuleID;
begin
  Current := nil;

  while not Stack.Empty do
  begin
    Symbol := Stack.Pop;

    if Symbol = scNextChildIsFake then
    begin
      // Make a fake non-terminal
      Current := TSepiFakeNonTerminal.Create(Current, Symbol,
        CurTerminal.SourcePos);
    end else if Symbol = scBackToParent then
    begin
      // Current non-terminal is done: go back to parent
      Temp := Current;
      Current := Temp.SyntacticParent;
      Temp.EndParsing;
    end else if Symbol = scPopTry then
    begin
      // Pop a try
      Stack.PopTry;
    end else if Symbol <= LastTerminal then
    begin
      // The prediction is a terminal: recognize it
      if CurTerminal.SymbolClass <> Symbol then
        Current := SyntaxError(SymbolClassNames[Symbol])
      else
      begin
        CurTerminal := TSepiTerminalClass(CurTerminal.ClassType).Clone(
          CurTerminal, Current);
        CurTerminal.Parse;
        Lexer.Next;
        CurTerminal := Lexer.CurTerminal;
      end;
    end else
    begin
      // The prediction is a non-terminal: create it and use the parsing table
      if Current = nil then
        Current := RootNode
      else
        Current := NonTerminalClasses[Symbol].Create(Current, Symbol,
          CurTerminal.SourcePos);

      Current.BeginParsing;

      Rule := ParsingTable[Current.SymbolClass, CurTerminal.SymbolClass];

      if Rule < 0 then
        Current := SyntaxError(SymbolClassNames[Current.SymbolClass])
      else
        PushChoiceProcs[Rule];
    end;

    { If we are now in a fake non-terminal, and it wasn't just added, go back
      to parent. }
    if (Current is TSepiFakeNonTerminal) and (Symbol <> scNextChildIsFake) then
      Stack.Push(scBackToParent);
  end;
end;

{*
  Parses a source code
  @param RootNode   Root node of the syntax tree
  @param Code       Source code to parse
*}
class procedure TParser.Parse(RootNode: TSepiParseTreeRootNode; Lexer: TLexer);
begin
  with Create(Lexer) do
  try
    InternalParse(RootNode);
  finally
    Free;
  end;
end;

{*
  Parses a source code
  @param RootNode   Root node of the syntax tree
  @param Code       Source code to parse
*}
class procedure TParser.Parse(RootNode: TSepiParseTreeRootNode;
  const Code: string; const FileName: string = '');
begin
  Parse(RootNode, TLexer.Create(RootNode.Errors, Code, FileName));
end;

{*
  Initializes SymbolClassNames array
*}
procedure InitSymbolClassNames;
begin
{#InitNodeNames#}end;

{*
  Initializes NonTerminalClasses array
*}
procedure InitNonTerminalClasses;
const
  ClassesToSimplify: array[0..{#SimplifyCount#}] of TSepiSymbolClass = (
    -1{#ClassesToSimplify#}
  );
var
  I: TSepiSymbolClass;
begin
  for I := FirstNonTerminal to LastNonTerminal do
    NonTerminalClasses[I] := TSepiNonTerminal;

  for I := 1 to High(ClassesToSimplify) do
    NonTerminalClasses[ClassesToSimplify[I]] := TSepiChildThroughNonTerminal;
end;

initialization
  if Length(SymbolClassNames) < LastNonTerminal+1 then
    SetLength(SymbolClassNames, LastNonTerminal+1);

  InitSymbolClassNames;
  InitNonTerminalClasses;
end.

