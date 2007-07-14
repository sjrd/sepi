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
    FFile : TFileStream; /// Flux interne du fichier LOG
  public
    constructor Create(const FileName : string; Append : boolean = False);
    destructor Destroy; override;

    procedure AddLine(const Line : string);

    procedure Add(const Time, Title, Description : string); overload;
    procedure Add(Time : TDateTime; const Title : string;
      const Description : string = ''); overload;
    procedure Add(const Title : string;
      const Description : string = ''); overload;
  end;

implementation

uses
  SysUtils;

{*
  Crée une nouvelle instance de TScLogFile
  @param FileName   Nom du fichier LOG
  @param Append     Si False, le fichier est d'abord vidé
*}
constructor TScLogFile.Create(const FileName : string;
  Append : boolean = False);
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
procedure TScLogFile.AddLine(const Line : string);
const
  CRLF : array[0..1] of Char = (#13, #10);
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
procedure TScLogFile.Add(const Time, Title, Description : string);
var Line : string;
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
procedure TScLogFile.Add(Time : TDateTime; const Title : string;
  const Description : string = '');
begin
  Add(DateTimeToStr(Time), Title, Description);
end;

{*
  Ajoute une entrée au fichier LOG
  L'heure de l'ajout est déterminée à partir de l'heure courante
  @param Title         Titre de l'entrée LOG
  @param Description   Description de l'entrée LOG
*}
procedure TScLogFile.Add(const Title : string;
  const Description : string = '');
begin
  Add(Now, Title, Description);
end;

end.

