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
  Instructions de haut niveau Sepi
  @author sjrd
  @version 1.0
*}
unit SepiInstructions;

interface

uses
  SepiOpCodes, SepiCompiler, SepiAsmInstructions;

type
  {*
    Instruction try..except
    @author sjrd
    @version 1.0
  *}
  TSepiTryExcept = class(TSepiInstruction)
  private
    FAsmInstruction: TSepiAsmTryExcept;        /// Instruction TRYE
    FTryInstructions: TSepiInstructionList;    /// Instructions dans le try
    FExceptInstructions: TSepiInstructionList; /// Instructions dans le except

    function GetExceptObject: TSepiMemoryReference;
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property TryInstructions: TSepiInstructionList read FTryInstructions;
    property ExceptInstructions: TSepiInstructionList read FExceptInstructions;
    property ExceptObject: TSepiMemoryReference read GetExceptObject;
  end;

  {*
    Instruction try..finally
    @author sjrd
    @version 1.0
  *}
  TSepiTryFinally = class(TSepiInstruction)
  private
    FAsmInstruction: TSepiAsmTryFinally;        /// Instruction TRYF
    FTryInstructions: TSepiInstructionList;     /// Instructions dans le try
    FFinallyInstructions: TSepiInstructionList; /// Instructions dans le finally
  protected
    procedure CustomCompile; override;
  public
    constructor Create(AMethodCompiler: TSepiMethodCompiler);

    property TryInstructions: TSepiInstructionList read FTryInstructions;
    property FinallyInstructions: TSepiInstructionList read FFinallyInstructions;
  end;

implementation

{----------------------}
{ TSepiTryExcept class }
{----------------------}

{*
  Crée une instruction try..except
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiTryExcept.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FAsmInstruction := TSepiAsmTryExcept.Create(MethodCompiler);
  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
  FExceptInstructions := TSepiInstructionList.Create(MethodCompiler);

  FAsmInstruction.EndOfTry.InstructionRef := TryInstructions.AfterRef;
  FAsmInstruction.EndOfExcept.InstructionRef := ExceptInstructions.AfterRef;
end;

{*
  Objet exception
  @return Référence mémoire où stocker l'objet exception
*}
function TSepiTryExcept.GetExceptObject: TSepiMemoryReference;
begin
  Result := FAsmInstruction.ExceptObject;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryExcept.CustomCompile;
begin
  FAsmInstruction.Compile;
  FTryInstructions.Compile;
  FExceptInstructions.Compile;
end;

{-----------------------}
{ TSepiTryFinally class }
{-----------------------}

{*
  Crée une instruction try..finally
  @param AMethodCompiler   Compilateur de méthode
*}
constructor TSepiTryFinally.Create(AMethodCompiler: TSepiMethodCompiler);
begin
  inherited Create(AMethodCompiler);

  FAsmInstruction := TSepiAsmTryFinally.Create(MethodCompiler);
  FTryInstructions := TSepiInstructionList.Create(MethodCompiler);
  FFinallyInstructions := TSepiInstructionList.Create(MethodCompiler);

  FAsmInstruction.EndOfTry.InstructionRef := TryInstructions.AfterRef;
  FAsmInstruction.EndOfFinally.InstructionRef := FinallyInstructions.AfterRef;
end;

{*
  [@inheritDoc]
*}
procedure TSepiTryFinally.CustomCompile;
begin
  FAsmInstruction.Compile;
  FTryInstructions.Compile;
  FFinallyInstructions.Compile;
end;

end.

