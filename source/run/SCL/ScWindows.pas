{-------------------------------------------------------------------------------
Sepi - Object-oriented script engine for Delphi
Copyright (C) 2006-2009  S�bastien Doeraene
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
  Types et routines d�pendant de la plate-forme Windows
  @author sjrd
  @version 1.0
*}
unit ScWindows;
{$i ..\..\source\Sepi.inc}
interface

uses
  Windows, SysUtils, Classes;

type
  {*
    Type de son
    stFileName : Nom de fichier
    stResource : Nom d'une ressource
    stSysSound : Nom d'un son syst�me
  *}
  TSoundType = (stFileName, stResource, stSysSound);

procedure CreateShellLink(const Source, Dest: string;
  const Description: string = ''; const IconLocation: string = '';
  IconIndex: Integer = 0; const Arguments: string = '';
  const WorkDir: string = ''; ShowCommand: Integer = SW_SHOW);

function ExecuteSound(const Sound: string;
  SoundType: TSoundType = stFileName;
  Synchronous: Boolean = False; Module: HMODULE = 0;
  AddFlags: LongWord = 0): Boolean;

{$REGION 'Modification des ressources'}

function BeginUpdateRes(const FileName: string): Integer;
procedure AddResource(ResHandle: Integer; const ResName: string;
  Resource: TStream; const ResType: string = 'RCDATA');
procedure DelResource(ResHandle: Integer; const ResName: string);
procedure EndUpdateRes(ResHandle: Integer; Cancel: Boolean = False);
procedure AddResToFile(const FileName, ResName: string; Resource: TStream;
  const ResType: string = 'RCDATA');
procedure DelResInFile(const FileName, ResName: string);

{$ENDREGION}

implementation

uses
  ComObj, ShlObj, ActiveX, MMSystem;

{*
  Cr�e un raccourci Windows
  Seuls les param�tres Source et Dest sont obligatoires.
  @param Source         Nom du fichier raccourci
  @param Dest           Destination du raccourci
  @param Description    Description
  @param IconLocation   Nom du fichier contenant l'ic�ne du raccourci
  @param IconIndex      Index de l'ic�ne dans le fichier
  @param Arguments      Arguments appliqu�s � la destination du raccourci
  @param WorkDir        R�pertoire de travail pour l'ex�cution du raccourci
  @param ShowCommand    Commande d'affichage de la destination
*}
procedure CreateShellLink(const Source, Dest: string;
  const Description: string = ''; const IconLocation: string = '';
  IconIndex: Integer = 0; const Arguments: string = '';
  const WorkDir: string = ''; ShowCommand: Integer = SW_SHOW);
var
  Link: IShellLink;
begin
  // Cr�ation de l'objet ShellLink
  Link := CreateComObject(CLSID_ShellLink) as IShellLink;
  // Fichier source
  Link.SetPath(PChar(Source));
  // Description
  if Description <> '' then
    Link.SetDescription(PChar(Description));
  // Emplacement de l'ic�ne
  if IconLocation <> '' then
    Link.SetIconLocation(PChar(IconLocation), IconIndex);
  // Arguments
  if Arguments <> '' then
    Link.SetArguments(PChar(Arguments));
  // Dossier de travail
  if WorkDir <> '' then
    Link.SetWorkingDirectory(PChar(WorkDir));
  // Type de lancement
  Link.SetShowCmd(ShowCommand);
  // Enregistrement
  (Link as IPersistFile).Save(StringToOleStr(Dest), True);
end;

{*
  Ex�cute un son � partir d'un fichier, d'une ressource ou d'un son syst�me
  @param Sound         Nom du son (selon le type de son)
  @param SoundType     Type de son
  @param Synchronous   True pour ex�cuter le son de fa�on synchr�ne
  @param Module        Dans le cas d'une ressource, le module la contenant
  @param AddFlags      Flags additionnels � passer � MMSystem.PlaySound
  @return True si le son a �t� correctement ex�cut�, False sinon
*}
function ExecuteSound(const Sound: string;
  SoundType: TSoundType = stFileName;
  Synchronous: Boolean = False; Module: HMODULE = 0;
  AddFlags: LongWord = 0): Boolean;
var
  Flags: LongWord;
begin
  Flags := AddFlags;
  case SoundType of
    stFileName: Flags := Flags or SND_FILENAME; // Fichier son
    stResource: Flags := Flags or SND_RESOURCE; // Ressource son
    stSysSound: Flags := Flags or SND_ALIAS;    // Alias son syst�me
  end;
  if not Synchronous then // Asynchr�ne ?
    Flags := Flags or SND_ASYNC;
  if SoundType <> stResource then
    Module := 0;
  // Appel de PlaySound et renvoi de la valeur renvoy�e par celle-ci
  Result := PlaySound(PChar(Sound), Module, Flags);
end;

{$REGION 'Modification des ressources'}

{---------------------------------}
{ Ajout-suppression de ressources }
{---------------------------------}

{*
  D�bute la mise � jour des ressources d'un fichier module
  Tout appel � BeginUpdateRes doit �tre compens� par un appel � EndUpdateRes.
  @param FileName   Nom du fichier module
  @return Handle de ressources
*}
function BeginUpdateRes(const FileName: string): Integer;
begin
  // Appel de Windows.BeginUpdateResource
  Result := BeginUpdateResource(PChar(FileName), False);
  // Si Result = 0, il y a eu une erreur API
  if Result = 0 then
    RaiseLastOSError;
end;

{*
  Ajoute une ressource
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param ResName     Nom de la ressource � ajouter
  @param Resource    Flux contenant la ressource
  @param ResType     Type de ressource
*}
procedure AddResource(ResHandle: Integer; const ResName: string;
  Resource: TStream; const ResType: string = 'RCDATA');
var
  MemRes: TMemoryStream;
  MustFreeRes, OK: Boolean;
begin
  MustFreeRes := False;
  // On met dans MemRes un flux m�moire qui contient les donn�es de la ressource
  if Resource is TMemoryStream then
    MemRes := Resource as TMemoryStream
  else
  begin
    MemRes := TMemoryStream.Create;
    MemRes.LoadFromStream(Resource);
    MustFreeRes := True;
  end;
  // Appel de Windows.UpdateResource
  OK := UpdateResource(ResHandle, PChar(ResType), PChar(ResName), 0,
    MemRes.Memory, MemRes.Size);
  // On supprime le flux m�moire si on l'a cr��
  if MustFreeRes then
    MemRes.Free;
  // Si UpdateResource a renvoy� False, il y a eu une erreur
  if not OK then
    RaiseLastOSError;
end;

{*
  Supprime une ressource
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param ResName     Nom de la ressource � supprimer
*}
procedure DelResource(ResHandle: Integer; const ResName: string);
begin
  // Appel de Windows.UpdateResource
  if not UpdateResource(ResHandle, '', PChar(ResName), 0, nil, 0) then
    // Si UpdateResource a renvoy� False, il y a eu une erreur
    RaiseLastOSError;
end;

{*
  Termine la mise � jour des ressources d'un fichier module
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param Cancel      Indique s'il faut annuler les modifications faites
*}
procedure EndUpdateRes(ResHandle: Integer; Cancel: Boolean = False);
begin
  // Appel de Windows.EndUpdateResource
  if not EndUpdateResource(ResHandle, Cancel) then
    // Si EndUpdateResource a renvoy� False, il y a eu une erreur
    RaiseLastOSError;
end;

{*
  Ajoute une ressources � un fichier module
  @param FileName   Nom du fichier module
  @param ResName    Nom de la ressource � ajouter
  @param Resource   Flux contenant la ressource
  @param ResType    Type de ressource
*}
procedure AddResToFile(const FileName, ResName: string; Resource: TStream;
  const ResType: string = 'RCDATA');
var
  ResHandle: Integer;
begin
  ResHandle := BeginUpdateRes(FileName);
  try
    AddResource(ResHandle, ResName, Resource, ResType);
    EndUpdateRes(ResHandle);
  except
    try
      EndUpdateRes(ResHandle, True)
    except
    end;
    raise;
  end;
end;

{*
  Supprime une ressources d'un fichier module
  @param FileName   Nom du fichier module
  @param ResName    Nom de la ressource � supprimer
*}
procedure DelResInFile(const FileName, ResName: string);
var
  ResHandle: Integer;
begin
  ResHandle := BeginUpdateRes(FileName);
  try
    DelResource(ResHandle, ResName);
    EndUpdateRes(ResHandle);
  except
    try
      EndUpdateRes(ResHandle, True)
    except
    end;
    raise;
  end;
end;

{$ENDREGION}

end.

