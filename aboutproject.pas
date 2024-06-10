unit AboutProject;

{$mode ObjFPC} {$LONGSTRINGS ON} {$RANGECHECKS ON} {$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfAboutProject }

  TfAboutProject = class(TForm)
    bClose: TBitBtn;
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

