{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2007  Sébastien Doeraene
All Rights Reserved

This file is part of the SCL (Sepi Code Library), which is part of Sepi.

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
  Utilitaires de génération du parser
  @author sjrd
  @version 1.0
*}
unit GeneratorUtils;

interface

uses
  SysUtils, Classes, StrUtils, ScUtils, ScDelphiLanguage, ScIntegerSets;

type
  TGenerator = class
  public
    LanguageName: string;

    DestFileName: TFileName;
    Source: TStrings;

    ChoiceCount: Integer;
    FirstNonTerminal: Integer;
    LastNonTerminal: Integer;

    ClassesToSimplify: TScIntegerSet;
    ClassesAsText: TScIntegerSet;

    NonTerminalConsts: TStrings;
    ChoiceFuncsDecls: TStrings;
    PushChoiceProcsDecls: TStrings;
    ParsingTable: TStrings;
    InitChoiceFuncs: TStrings;
    InitPushChoiceProcs: TStrings;
    ChoiceFuncs: TStrings;
    PushChoiceProcs: TStrings;
    InitNodeNames: TStrings;

    constructor Create(const ALanguageName: string;
      const ADestFileName: TFileName);
    destructor Destroy; override;

    procedure Generate;
  end;

implementation

type
  PTStrings = ^TStrings;

function IntegerSetToStr(IntSet: TScIntegerSet; out Count: Integer): string;
var
  Enumerator: TScIntegerSetEnumerator;
begin
  Count := 0;
  Enumerator := IntSet.GetEnumerator;
  try
    while Enumerator.MoveNext do
    begin
      Result := Result + ', ' + IntToStr(Enumerator.Current);
      Inc(Count);
    end;
  finally
    Enumerator.Free;
  end;
end;

constructor TGenerator.Create(const ALanguageName: string;
  const ADestFileName: TFileName);
begin
  inherited Create;

  LanguageName := ALanguageName;

  DestFileName := ADestFileName;

  Source := TStringList.Create;
  Source.LoadFromFile(Dir+'ParserBase.pas');

  ClassesToSimplify := TScIntegerSet.Create;
  ClassesAsText := TScIntegerSet.Create;

  NonTerminalConsts := TStringList.Create;
  ChoiceFuncsDecls := TStringList.Create;
  PushChoiceProcsDecls := TStringList.Create;
  ParsingTable := TStringList.Create;
  InitChoiceFuncs := TStringList.Create;
  InitPushChoiceProcs := TStringList.Create;
  ChoiceFuncs := TStringList.Create;
  PushChoiceProcs := TStringList.Create;
  InitNodeNames := TStringList.Create;
end;

destructor TGenerator.Destroy;
begin
  NonTerminalConsts.Free;
  ChoiceFuncsDecls.Free;
  PushChoiceProcsDecls.Free;
  ParsingTable.Free;
  InitChoiceFuncs.Free;
  InitPushChoiceProcs.Free;
  ChoiceFuncs.Free;
  PushChoiceProcs.Free;
  InitNodeNames.Free;

  ClassesAsText.Free;
  ClassesToSimplify.Free;

  Source.Free;

  inherited;
end;

procedure TGenerator.Generate;
var
  OutputString: string;
  Count: Integer;
begin
  OutputString := Source.Text;

  OutputString := AnsiReplaceStr(OutputString,
    '{#LanguageName#}', LanguageName);

  OutputString := AnsiReplaceStr(OutputString,
    '{#ChoiceCount#}', IntToStr(ChoiceCount));
  OutputString := AnsiReplaceStr(OutputString,
    '{#FirstNonTerminal#}', IntToStr(FirstNonTerminal));
  OutputString := AnsiReplaceStr(OutputString,
    '{#LastNonTerminal#}', IntToStr(LastNonTerminal));

  OutputString := AnsiReplaceStr(OutputString,
    '{#ClassesToSimplify#}', IntegerSetToStr(ClassesToSimplify, Count));
  OutputString := AnsiReplaceStr(OutputString,
    '{#SimplifyCount#}', IntToStr(Count));
  OutputString := AnsiReplaceStr(OutputString,
    '{#ClassesAsText#}', IntegerSetToStr(ClassesAsText, Count));
  OutputString := AnsiReplaceStr(OutputString,
    '{#AsTextCount#}', IntToStr(Count));

  OutputString := AnsiReplaceStr(OutputString,
    '{#NonTerminalConsts#}', NonTerminalConsts.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#ChoiceFuncsDecls#}', ChoiceFuncsDecls.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#PushChoiceProcsDecls#}', PushChoiceProcsDecls.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#ParsingTable#}', ParsingTable.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#InitChoiceFuncs#}', InitChoiceFuncs.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#InitPushChoiceProcs#}', InitPushChoiceProcs.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#ChoiceFuncs#}', ChoiceFuncs.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#PushChoiceProcs#}', PushChoiceProcs.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#InitNodeNames#}', InitNodeNames.Text);

  OutputString := AnsiReplaceStr(OutputString, '{#FileName#}', 'Parser');

  with TFileStream.Create(DestFileName,
    fmCreate or fmShareExclusive) do
  try
    WriteBuffer(Pointer(OutputString)^, Length(OutputString));
  finally
    Free;
  end;
end;

end.

