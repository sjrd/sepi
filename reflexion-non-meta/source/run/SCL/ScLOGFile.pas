{*
  D�finit une classe de gestion d'un fichier LOG
  @author sjrd
  @version 1.0
*}
unit ScLOGFile;

interface

uses
  Classes;

type
  {*
    G�re un fichier LOG
    Les lignes d'un fichier LOG g�r� avec cette classe sont de la forme :
    [Date et Heure] Titre de l'entr�e ; Description de l'entr�e
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
  Cr�e une nouvelle instance de TScLogFile
  @param FileName   Nom du fichier LOG
  @param Append     Si False, le fichier est d'abord vid�
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
  D�truit l'instance
*}
destructor TScLogFile.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

{*
  Ajoute une ligne au fichier LOG
  @param Line   Ligne � ajouter
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
  Ajoute une entr�e au fichier LOG
  @param Time          Repr�sentation en cha�ne de l'heure de l'ajout
  @param Title         Titre de l'entr�e de LOG
  @param Description   Description de l'entr�e LOG
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
  Ajoute une entr�e au fichier LOG
  @param Time          Heure de l'ajout
  @param Title         Titre de l'entr�e LOG
  @param Description   Description de l'entr�e LOG
*}
procedure TScLogFile.Add(Time : TDateTime; const Title : string;
  const Description : string = '');
begin
  Add(DateTimeToStr(Time), Title, Description);
end;

{*
  Ajoute une entr�e au fichier LOG
  L'heure de l'ajout est d�termin�e � partir de l'heure courante
  @param Title         Titre de l'entr�e LOG
  @param Description   Description de l'entr�e LOG
*}
procedure TScLogFile.Add(const Title : string;
  const Description : string = '');
begin
  Add(Now, Title, Description);
end;

end.

