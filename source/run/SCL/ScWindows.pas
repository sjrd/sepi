{*
  Types et routines dépendant de la plate-forme Windows
  @author sjrd
  @version 1.0
*}
unit ScWindows platform;

interface

uses
  Windows, SysUtils, Classes;

type
  {*
    Type de son
    stFileName : Nom de fichier
    stResource : Nom d'une ressource
    stSysSound : Nom d'un son système
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
  Crée un raccourci Windows
  Seuls les paramètres Source et Dest sont obligatoires.
  @param Source         Nom du fichier raccourci
  @param Dest           Destination du raccourci
  @param Description    Description
  @param IconLocation   Nom du fichier contenant l'icône du raccourci
  @param IconIndex      Index de l'icône dans le fichier
  @param Arguments      Arguments appliqués à la destination du raccourci
  @param WorkDir        Répertoire de travail pour l'exécution du raccourci
  @param ShowCommand    Commande d'affichage de la destination
*}
procedure CreateShellLink(const Source, Dest: string;
  const Description: string = ''; const IconLocation: string = '';
  IconIndex: Integer = 0; const Arguments: string = '';
  const WorkDir: string = ''; ShowCommand: Integer = SW_SHOW);
var
  Link: IShellLink;
begin
  // Création de l'objet ShellLink
  Link := CreateComObject(CLSID_ShellLink) as IShellLink;
  // Fichier source
  Link.SetPath(PChar(Source));
  // Description
  if Description <> '' then
    Link.SetDescription(PChar(Description));
  // Emplacement de l'icône
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
  Exécute un son à partir d'un fichier, d'une ressource ou d'un son système
  @param Sound         Nom du son (selon le type de son)
  @param SoundType     Type de son
  @param Synchronous   True pour exécuter le son de façon synchrône
  @param Module        Dans le cas d'une ressource, le module la contenant
  @param AddFlags      Flags additionnels à passer à MMSystem.PlaySound
  @return True si le son a été correctement exécuté, False sinon
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
    stSysSound: Flags := Flags or SND_ALIAS;    // Alias son système
  end;
  if not Synchronous then // Asynchrône ?
    Flags := Flags or SND_ASYNC;
  if SoundType <> stResource then
    Module := 0;
  // Appel de PlaySound et renvoi de la valeur renvoyée par celle-ci
  Result := PlaySound(PChar(Sound), Module, Flags);
end;

{$REGION 'Modification des ressources'}

{---------------------------------}
{ Ajout-suppression de ressources }
{---------------------------------}

{*
  Débute la mise à jour des ressources d'un fichier module
  Tout appel à BeginUpdateRes doit être compensé par un appel à EndUpdateRes.
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
  @param ResName     Nom de la ressource à ajouter
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
  // On met dans MemRes un flux mémoire qui contient les données de la ressource
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
  // On supprime le flux mémoire si on l'a créé
  if MustFreeRes then
    MemRes.Free;
  // Si UpdateResource a renvoyé False, il y a eu une erreur
  if not OK then
    RaiseLastOSError;
end;

{*
  Supprime une ressource
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param ResName     Nom de la ressource à supprimer
*}
procedure DelResource(ResHandle: Integer; const ResName: string);
begin
  // Appel de Windows.UpdateResource
  if not UpdateResource(ResHandle, '', PChar(ResName), 0, nil, 0) then
    // Si UpdateResource a renvoyé False, il y a eu une erreur
    RaiseLastOSError;
end;

{*
  Termine la mise à jour des ressources d'un fichier module
  @param ResHandle   Handle de ressources obtenu par BeginUpdateRes
  @param Cancel      Indique s'il faut annuler les modifications faites
*}
procedure EndUpdateRes(ResHandle: Integer; Cancel: Boolean = False);
begin
  // Appel de Windows.EndUpdateResource
  if not EndUpdateResource(ResHandle, Cancel) then
    // Si EndUpdateResource a renvoyé False, il y a eu une erreur
    RaiseLastOSError;
end;

{*
  Ajoute une ressources à un fichier module
  @param FileName   Nom du fichier module
  @param ResName    Nom de la ressource à ajouter
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
  @param ResName    Nom de la ressource à supprimer
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

