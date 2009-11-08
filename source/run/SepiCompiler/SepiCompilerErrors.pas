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
  Gestion des erreurs de compilation Sepi
  @author sjrd
  @version 1.0
*}
unit SepiCompilerErrors;

interface

uses
  SysUtils, Classes, Contnrs, SepiReflectionCore, SepiMembers,
  SepiCompilerConsts;

type
  {*
    Exception déclenchée en cas d'erreur fatale de compilation
  *}
  ESepiCompilerFatalError = class(Exception);

  {*
    Type d'erreur de compilation Sepi
  *}
  TSepiErrorKind = (ekHint, ekWarning, ekError, ekFatalError);

  {*
    Position dans un source Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiSourcePosition = record
    FileName: TFileName; /// Nom du fichier
    Line: Integer;       /// Ligne
    Col: Integer;        /// Colonne
  end;

  {*
    Erreur de compilation Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiCompilerError = class(TObject)
  private
    FErrorMsg: string;              /// Message d'erreur
    FKind: TSepiErrorKind;          /// Type d'erreur
    FPosition: TSepiSourcePosition; /// Position dans le source

    function GetAsText: string;
  public
    constructor Create(const AErrorMsg: string; AKind: TSepiErrorKind;
      const APosition: TSepiSourcePosition); overload;
    constructor Create(const AErrorMsg: string;
      const APosition: TSepiSourcePosition); overload;
    constructor Create(const AErrorMsg: string; AKind: TSepiErrorKind;
      const AFileName: TFileName = ''; ALine: Integer = 0;
      ACol: Integer = 0); overload;
    constructor Create(const AErrorMsg: string;
      const AFileName: TFileName = ''; ALine: Integer = 0;
      ACol: Integer = 0); overload;

    property ErrorMsg: string read FErrorMsg;
    property Kind: TSepiErrorKind read FKind;
    property Position: TSepiSourcePosition read FPosition;

    /// Nom du fichier
    property FileName: TFileName read FPosition.FileName;

    /// Ligne
    property Line: Integer read FPosition.Line;

    /// Colonne
    property Col: Integer read FPosition.Col;

    property AsText: string read GetAsText;
  end;

  {*
    Type de l'événement OnAddError de TSepiCompilerErrorList
    @param Sender   Objet qui a déclenché l'événement
    @param Error    Erreur ajoutée
  *}
  TSepiAddErrorEvent = procedure(Sender: TObject;
    Error: TSepiCompilerError) of object;

  {*
    Liste d'erreurs de compilation Sepi
    @author sjrd
    @version 1.0
  *}
  TSepiCompilerErrorList = class(TObjectList)
  private
    FCurrentFileName: TFileName;     /// Nom du fichier courant
    FOnAddError: TSepiAddErrorEvent; /// Déclenché à l'ajout d'une erreur

    function GetErrors(Index: Integer): TSepiCompilerError;

    function GetErrorCount: Integer;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function MakeError(const ErrorMsg: string; Kind: TSepiErrorKind;
      const Position: TSepiSourcePosition): TSepiCompilerError; overload;
    function MakeError(const ErrorMsg: string;
      const Position: TSepiSourcePosition): TSepiCompilerError; overload;

    function MakeError(const ErrorMsg: string; Kind: TSepiErrorKind;
      const FileName: TFileName; Line: Integer = 0;
      Col: Integer = 0): TSepiCompilerError; overload;
    function MakeError(const ErrorMsg: string; const FileName: TFileName;
      Line: Integer = 0; Col: Integer = 0): TSepiCompilerError; overload;

    function MakeError(const ErrorMsg: string; Kind: TSepiErrorKind;
      Line: Integer = 0; Col: Integer = 0): TSepiCompilerError; overload;
    function MakeError(const ErrorMsg: string; Line: Integer = 0;
      Col: Integer = 0): TSepiCompilerError; overload;

    procedure CheckForErrors;

    property CurrentFileName: TFileName
      read FCurrentFileName write FCurrentFileName;
    property Errors[Index: Integer]: TSepiCompilerError
      read GetErrors; default;

    property ErrorCount: Integer read GetErrorCount;

    property OnAddError: TSepiAddErrorEvent read FOnAddError write FOnAddError;
  end;

const
  /// Position non renseignée
  SepiNoPosition: TSepiSourcePosition = (FileName: ''; Line: 0; Col: 0);

  /// Nombre maximum d'erreurs avant d'abandonner la compilation
  MaxErrors = 100;

var
  /// Nom des types d'erreur de compilation Sepi
  SepiErrorKindNames: array[TSepiErrorKind] of string;

implementation

{--------------------------}
{ TSepiCompilerError class }
{--------------------------}

{*
  Crée une erreur de compilation Sepi
  @param AErrorMsg   Message d'erreur
  @param AKind       Type d'erreur
  @param APosition   Position dans le source
*}
constructor TSepiCompilerError.Create(const AErrorMsg: string;
  AKind: TSepiErrorKind; const APosition: TSepiSourcePosition);
begin
  inherited Create;

  FErrorMsg := AErrorMsg;
  FKind := AKind;
  FPosition := APosition;
end;

{*
  Crée une erreur de compilation Sepi
  @param AErrorMsg   Message d'erreur
  @param APosition   Position dans le source
*}
constructor TSepiCompilerError.Create(const AErrorMsg: string;
  const APosition: TSepiSourcePosition);
begin
  Create(AErrorMsg, ekError, APosition);
end;

{*
  Crée une erreur de compilation Sepi
  @param AErrorMsg   Message d'erreur
  @param AKind       Type d'erreur
  @param AFileName   Nom du fichier
  @param ALine       Ligne (défaut = 0)
  @param ACol        Colonne (défaut = 0)
*}
constructor TSepiCompilerError.Create(const AErrorMsg: string;
  AKind: TSepiErrorKind; const AFileName: TFileName = ''; ALine: Integer = 0;
  ACol: Integer = 0);
begin
  inherited Create;

  FErrorMsg := AErrorMsg;
  FKind := AKind;
  FPosition.FileName := AFileName;
  FPosition.Line := ALine;
  FPosition.Col := ACol;
end;

{*
  Crée une erreur de compilation Sepi de type Erreur
  @param AErrorMsg   Message d'erreur
  @param AFileName   Nom du fichier
  @param ALine       Ligne (défaut = 0)
  @param ACol        Colonne (défaut = 0)
*}
constructor TSepiCompilerError.Create(const AErrorMsg: string;
  const AFileName: TFileName = ''; ALine: Integer = 0; ACol: Integer = 0);
begin
  Create(AErrorMsg, ekError, AFileName, ALine, ACol);
end;

{*
  Représentation textuelle de l'erreur
  @return Représentation textuelle de l'erreur
*}
function TSepiCompilerError.GetAsText: string;
begin
  Result := Format(SSepiCompilerErrorFormat,
    [SepiErrorKindNames[Kind], ExtractFileName(FileName), Line, Col, ErrorMsg]);
end;

{------------------------------}
{ TSepiCompilerErrorList class }
{------------------------------}

{*
  Tableau zero-based des erreurs
  @param Index   Index de l'erreur
  @return Erreur à l'index spécifié
*}
function TSepiCompilerErrorList.GetErrors(Index: Integer): TSepiCompilerError;
begin
  Result := Items[Index] as TSepiCompilerError;
end;

{*
  Nombre d'erreurs de type ekError dans la liste d'erreurs
  @return Nombre d'erreurs
*}
function TSepiCompilerErrorList.GetErrorCount: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to Count-1 do
    if Errors[I].Kind = ekError then
      Inc(Result);
end;

{*
  [@inheritDoc]
*}
procedure TSepiCompilerErrorList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  inherited;

  if (Action = lnAdded) and Assigned(FOnAddError) and
    (TObject(Ptr) is TSepiCompilerError) then
    FOnAddError(Self, TSepiCompilerError(Ptr));
end;

{*
  Ajoute une erreur à la liste, avec une position explicite
  Si c'est une erreur fatale, une exception ESepiCompilerFatalError est ensuite
  déclenchée.
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur
  @param Position   Position dans le source
  @throws ESepiCompilerFatalError L'erreur est une erreur fatale
*}
function TSepiCompilerErrorList.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind;
  const Position: TSepiSourcePosition): TSepiCompilerError;
begin
  Result := TSepiCompilerError.Create(ErrorMsg, Kind, Position);
  Add(Result);

  if Result.Kind = ekFatalError then
    raise ESepiCompilerFatalError.Create(Result.AsText)
  else if ErrorCount > MaxErrors then
    MakeError(STooManyErrors, ekFatalError);
end;

{*
  Ajoute une erreur de type Erreur à la liste, avec une position explicite
  @param ErrorMsg   Message d'erreur
  @param Position   Position dans le source
*}
function TSepiCompilerErrorList.MakeError(const ErrorMsg: string;
  const Position: TSepiSourcePosition): TSepiCompilerError;
begin
  Result := MakeError(ErrorMsg, ekError, Position);
end;

{*
  Ajoute une erreur à la liste
  Si c'est une erreur fatale, une exception ESepiCompilerFatalError est ensuite
  déclenchée.
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur
  @param FileName   Nom du fichier
  @param Line       Ligne (défaut = 0)
  @param Col        Colonne (défaut = 0)
  @throws ESepiCompilerFatalError L'erreur est une erreur fatale
*}
function TSepiCompilerErrorList.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind; const FileName: TFileName; Line: Integer = 0;
  Col: Integer = 0): TSepiCompilerError;
begin
  Result := TSepiCompilerError.Create(ErrorMsg, Kind, FileName, Line, Col);
  Add(Result);

  if Result.Kind = ekFatalError then
    raise ESepiCompilerFatalError.Create(Result.AsText)
  else if ErrorCount > MaxErrors then
    MakeError(STooManyErrors, ekFatalError);
end;

{*
  Ajoute une erreur de type Erreur à la liste
  @param ErrorMsg   Message d'erreur
  @param FileName   Nom du fichier
  @param Line       Ligne (défaut = 0)
  @param Col        Colonne (défaut = 0)
*}
function TSepiCompilerErrorList.MakeError(const ErrorMsg: string;
  const FileName: TFileName; Line: Integer = 0;
  Col: Integer = 0): TSepiCompilerError;
begin
  Result := MakeError(ErrorMsg, ekError, FileName, Line, Col);
end;

{*
  Ajoute une erreur à la liste, dans le fichier courant
  Si c'est une erreur fatale, une exception ESepiCompilerFatalError est ensuite
  déclenchée.
  @param ErrorMsg   Message d'erreur
  @param Kind       Type d'erreur
  @param Line       Ligne (défaut = 0)
  @param Col        Colonne (défaut = 0)
  @throws ESepiCompilerFatalError L'erreur est une erreur fatale
*}
function TSepiCompilerErrorList.MakeError(const ErrorMsg: string;
  Kind: TSepiErrorKind; Line: Integer = 0;
  Col: Integer = 0): TSepiCompilerError;
begin
  Result := MakeError(ErrorMsg, Kind, CurrentFileName, Line, Col);
end;

{*
  Ajoute une erreur de type Erreur à la liste, dans le fichier courant
  @param ErrorMsg   Message d'erreur
  @param Line       Ligne (défaut = 0)
  @param Col        Colonne (défaut = 0)
*}
function TSepiCompilerErrorList.MakeError(const ErrorMsg: string;
  Line: Integer = 0; Col: Integer = 0): TSepiCompilerError;
begin
  Result := MakeError(ErrorMsg, ekError, CurrentFileName, Line, Col);
end;

{*
  Ajoute une erreur fatale si une ou plusieurs erreurs sont présentes
*}
procedure TSepiCompilerErrorList.CheckForErrors;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if Errors[I].Kind = ekError then
      MakeError(SSepiThereWereErrors, ekFatalError);
end;

initialization
  SepiErrorKindNames[ekHint] := SSepiHintName;
  SepiErrorKindNames[ekWarning] := SSepiWarningName;
  SepiErrorKindNames[ekError] := SSepiErrorName;
  SepiErrorKindNames[ekFatalError] := SSepiFatalErrorName;
end.

