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
  Utilitaires de génération du parser
  @author sjrd
  @version 1.0
*}
unit GeneratorUtils;

interface

uses
  SysUtils, Classes, StrUtils, ScUtils, ScIntegerSets;

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

    NonTerminalConsts: TStrings;
    PushChoiceProcsDecls: TStrings;
    ParsingTable: TStrings;
    InitPushChoiceProcs: TStrings;
    PushChoiceProcs: TStrings;
    InitNodeNames: TStrings;

    constructor Create(const ALanguageName: string;
      const ADestFileName: TFileName);
    destructor Destroy; override;

    procedure Generate;
  end;

implementation

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

  NonTerminalConsts := TStringList.Create;
  PushChoiceProcsDecls := TStringList.Create;
  ParsingTable := TStringList.Create;
  InitPushChoiceProcs := TStringList.Create;
  PushChoiceProcs := TStringList.Create;
  InitNodeNames := TStringList.Create;
end;

destructor TGenerator.Destroy;
begin
  NonTerminalConsts.Free;
  PushChoiceProcsDecls.Free;
  ParsingTable.Free;
  InitPushChoiceProcs.Free;
  PushChoiceProcs.Free;
  InitNodeNames.Free;

  ClassesToSimplify.Free;

  Source.Free;

  inherited;
end;

procedure TGenerator.Generate;
var
  OutputString: string;
  Count: Integer;
  AnsiOutputString: AnsiString;
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
    '{#NonTerminalConsts#}', NonTerminalConsts.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#PushChoiceProcsDecls#}', PushChoiceProcsDecls.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#ParsingTable#}', ParsingTable.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#InitPushChoiceProcs#}', InitPushChoiceProcs.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#PushChoiceProcs#}', PushChoiceProcs.Text);
  OutputString := AnsiReplaceStr(OutputString,
    '{#InitNodeNames#}', InitNodeNames.Text);

  OutputString := AnsiReplaceStr(OutputString,
    'TSepiLanguageParser', 'TSepi'+LanguageName+'Parser');

  OutputString := AnsiReplaceStr(OutputString, '{#FileName#}',
    ChangeFileExt(ExtractFileName(DestFileName), ''));

  with TFileStream.Create(DestFileName,
    fmCreate or fmShareExclusive) do
  try
    AnsiOutputString := AnsiString(OutputString);
    WriteBuffer(Pointer(AnsiOutputString)^, Length(AnsiOutputString));
  finally
    Free;
  end;
end;

end.

