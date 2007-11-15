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

Linking this library -the SCL- statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and conditions
of the GNU General Public License cover the whole combination.

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
  Définit une classe de gestion d'un fichier LOG
  @author sjrd
  @version 1.0
*}
unit ScLOGFile;

interface

uses
  Classes;

type
  {*
    Gère un fichier LOG
    Les lignes d'un fichier LOG géré avec cette classe sont de la forme :
    [Date et Heure] Titre de l'entrée ; Description de l'entrée
    Les descriptions sont toujours optionnelles.
    @author sjrd
    @version 1.0
  *}
  TScLogFile = class
  private
    FFile: TFileStream; /// Flux interne du fichier LOG
  public
    constructor Create(const FileName: string; Append: Boolean = False);
    destructor Destroy; override;

    procedure AddLine(const Line: string);

    procedure Add(const Time, Title, Description: string); overload;
    procedure Add(Time: TDateTime; const Title: string;
      const Description: string = ''); overload;
    procedure Add(const Title: string;
      const Description: string = ''); overload;
  end;

implementation

uses
  SysUtils;

{*
  Crée une nouvelle instance de TScLogFile
  @param FileName   Nom du fichier LOG
  @param Append     Si False, le fichier est d'abord vidé
*}
constructor TScLogFile.Create(const FileName: string;
  Append: Boolean = False);
begin
  inherited Create;
  if Append then
    FFile := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyNone)
  else
    FFile := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
end;

{*
  Détruit l'instance
*}
destructor TScLogFile.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

{*
  Ajoute une ligne au fichier LOG
  @param Line   Ligne à ajouter
*}
procedure TScLogFile.AddLine(const Line: string);
const
  CRLF: array[0..1] of Char = (#13, #10);
begin
  FFile.Seek(0, soFromEnd);
  FFile.WriteBuffer(Pointer(Line)^, Length(Line));
  FFile.WriteBuffer(CRLF, 2);
end;

{*
  Ajoute une entrée au fichier LOG
  @param Time          Représentation en chaîne de l'heure de l'ajout
  @param Title         Titre de l'entrée de LOG
  @param Description   Description de l'entrée LOG
*}
procedure TScLogFile.Add(const Time, Title, Description: string);
var
  Line: string;
begin
  Line := '['+Time+'] '+Title;
  if Description <> '' then
    Line := Line+' ; '+Description;
  AddLine(Line);
end;

{*
  Ajoute une entrée au fichier LOG
  @param Time          Heure de l'ajout
  @param Title         Titre de l'entrée LOG
  @param Description   Description de l'entrée LOG
*}
procedure TScLogFile.Add(Time: TDateTime; const Title: string;
  const Description: string = '');
begin
  Add(DateTimeToStr(Time), Title, Description);
end;

{*
  Ajoute une entrée au fichier LOG
  L'heure de l'ajout est déterminée à partir de l'heure courante
  @param Title         Titre de l'entrée LOG
  @param Description   Description de l'entrée LOG
*}
procedure TScLogFile.Add(const Title: string;
  const Description: string = '');
begin
  Add(Now, Title, Description);
end;

end.

