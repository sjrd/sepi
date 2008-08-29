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
  Expressions Sepi
  @author sjrd
  @version 1.0
*}
unit SepiExpression;

interface

uses
  SepiReflectionCore, SepiCompiler;

type
  {*
    Valeur qui peut être lue à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiReadableValue = interface(ISepiExpressionPart)
    ['{D7207F63-22F1-41C7-8366-2D4A87DD5200}']

    function GetValueType: TSepiType;

    procedure CompileRead(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);
  end;

  {*
    Valeur qui peut être modifiée à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiWritableValue = interface(ISepiExpressionPart)
    ['{E5E9C42F-E9A2-4B13-AE79-E1229013007D}']

    function GetValueType: TSepiType;

    procedure CompileWrite(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; Source: ISepiReadableValue);

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Valeur dont on peut récupérer l'adresse à l'exécution
    @author sjrd
    @version 1.0
  *}
  ISepiAddressableValue = interface(ISepiExpressionPart)
    ['{096E1AD8-04D6-4DA1-9426-A307C7965F73}']

    function GetAddressType: TSepiType;

    procedure CompileLoadAddress(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList; var Destination: TSepiMemoryReference;
      TempVars: TSepiTempVarsLifeManager);

    property AddressType: TSepiType read GetAddressType;
  end;

  {*
    Valeur constante à la compilation
    @author sjrd
    @version 1.0
  *}
  ISepiConstantValue = interface(ISepiExpressionPart)
    ['{78955B99-2FAA-487D-B295-2986E8C720DF}']

    function GetValueType: TSepiType;
    function GetValuePtr: Pointer;

    property ValueType: TSepiType read GetValueType;
    property ValuePtr: Pointer read GetValuePtr;
  end;

  {*
    Valeur à accès direct (par opposition à une valeur à calculer)
    @author sjrd
    @version 1.0
  *}
  ISepiDirectValue = interface(ISepiExpressionPart)
    ['{12678933-C5A4-4BBA-9BBF-3E3BF30B4AD5}']

    function GetValueType: TSepiType;

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Expression qui peut être appelée
    @author sjrd
    @version 1.0
  *}
  ISepiCallable = interface(ISepiExpressionPart)
    ['{CD26F811-9B78-4F42-ACD7-82B1FB93EA3F}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiExpression;
    function GetParamsCompleted: Boolean;

    procedure AddParam(Param: ISepiExpression);
    procedure CompleteParams;

    function GetReturnType: TSepiType;

    procedure CompileNoResult(Compiler: TSepiMethodCompiler;
      Instructions: TSepiInstructionList);

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression read GetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;

    property ReturnType: TSepiType read GetReturnType;
  end;

  {*
    Propriété
    @author sjrd
    @version 1.0
  *}
  ISepiProperty = interface(ISepiExpressionPart)
    ['{9C5A0B51-BDB2-45FA-B2D4-179763CB7F16}']

    function GetParamCount: Integer;
    function GetParams(Index: Integer): ISepiExpression;
    procedure SetParams(Index: Integer; Value: ISepiExpression);
    function GetParamsCompleted: Boolean;

    procedure CompleteParams;

    function GetValueType: TSepiType;

    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: ISepiExpression
      read GetParams write SetParams;
    property ParamsCompleted: Boolean read GetParamsCompleted;

    property ValueType: TSepiType read GetValueType;
  end;

  {*
    Expression nil
    @author sjrd
    @version 1.0
  *}
  ISepiNilValue = interface(ISepiExpressionPart)
    ['{2574C3DC-87FE-426C-931D-5230267A1573}']
  end;

  {*
    Expression qui représente un meta
    @author sjrd
    @version 1.0
  *}
  ISepiMetaExpression = interface(ISepiExpressionPart)
    ['{444E8E70-1173-44E5-AA91-5B28629A9AA4}']

    function GetMeta: TSepiMeta;

    property Meta: TSepiMeta read GetMeta;
  end;

  {*
    Expression qui représente un type
    @author sjrd
    @version 1.0
  *}
  ISepiTypeExpression = interface(ISepiExpressionPart)
    ['{F8DB8648-9F7B-421A-9BEC-0C4AEC9D1C56}']

    function GetType: TSepiType;

    property ExprType: TSepiType read GetType;
  end;

implementation

end.

