{-------------------------------------------------------------------------------
SepiExplorer - Example program for Sepi
As an example program, SepiExplorer is free of any usage. It is released in the
public domain.
-------------------------------------------------------------------------------}

{*
  Cadre montrant les informations d'un meta
  @author sjrd
  @version 1.0
*}
unit MetaExplorer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, SepiReflectionCore, TypesInfo, MembersInfo;

type
  {*
    Cadre montrant les informations d'un meta
    @author sjrd
    @version 1.0
  *}
  TFrameMetaExplorer = class(TFrame)
    MemoInfo: TMemo;
  private
    FSepiMeta: TSepiMeta; /// Le meta affiché

    procedure SetSepiMeta(Value: TSepiMeta);
  public
    property SepiMeta: TSepiMeta read FSepiMeta write SetSepiMeta;
  end;

implementation

{$R *.dfm}

{--------------------------}
{ TFrameMetaExplorer class }
{--------------------------}

{*
  Modifie le meta affiché
  @param Value   Nouveau meta à afficher
*}
procedure TFrameMetaExplorer.SetSepiMeta(Value: TSepiMeta);
var
  Output: TOutputWriter;
begin
  FSepiMeta := Value;

  MemoInfo.Lines.BeginUpdate;
  try
    MemoInfo.Lines.Clear;

    if SepiMeta <> nil then
    begin
      Output := TOutputWriter.Create(MemoInfo.Lines);
      try
        PrintMetaInfo(Output, SepiMeta);
      finally
        Output.Free;
      end;
    end;
  finally
    MemoInfo.Lines.EndUpdate;
  end;
end;

end.

