{*******************************************************}
{                                                       }
{       Scenarium                                       }
{       Copyright (c) 2024 Kirill Filippenok            }
{       Apache License 2.0                              }
{                                                       }
{*******************************************************}

unit AboutProject;

{$mode ObjFPC} {$LONGSTRINGS ON} {$RANGECHECKS ON} {$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls;

type

  { TfAboutProject }

  TfAboutProject = class(TForm)
    bClose: TBitBtn;
    Image1: TImage;
    lblBuild: TLabel;
    lblAutor: TLabel;
    lblName: TLabel;
    lblVersion: TLabel;
    procedure bCloseClick(Sender: TObject);
  private

  public

  end;

var
  fAboutProject: TfAboutProject;

implementation

{$R *.lfm}

{ TfAboutProject }

procedure TfAboutProject.bCloseClick(Sender: TObject);
begin
  Close;
end;

end.

