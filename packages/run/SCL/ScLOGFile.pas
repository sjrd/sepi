{*
  Définit une classe de gestion d'un fichier LOG
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit ScLOGFile;

interface

uses
  Classes;

type
  TScLogFile = class
  // Classe gérant un fichier LOG
  private
    FFile : TFileStream;
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

constructor TScLogFile.Create(const FileName : string;
  Append : boolean = False);
begin
  inherited Create;
  if Append then
    FFile := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyNone)
  else
    FFile := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
end;

destructor TScLogFile.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

procedure TScLogFile.AddLine(const Line : string);
const
  CRLF : array[0..1] of Char = (#13, #10);
begin
  FFile.Seek(0, soFromEnd);
  FFile.WriteBuffer(Pointer(Line)^, Length(Line));
  FFile.WriteBuffer(CRLF, 2);
end;

procedure TScLogFile.Add(const Time, Title, Description : string);
var Line : string;
begin
  Line := '['+Time+'] '+Title;
  if Description <> '' then
    Line := Line+' ; '+Description;
  AddLine(Line);
end;

procedure TScLogFile.Add(Time : TDateTime; const Title : string;
  const Description : string = '');
begin
  Add(DateTimeToStr(Time), Title, Description);
end;

procedure TScLogFile.Add(const Title : string;
  const Description : string = '');
begin
  Add(Now, Title, Description);
end;

end.

