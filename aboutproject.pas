unit AboutProject;

{$mode ObjFPC} {$LONGSTRINGS ON} {$RANGECHECKS ON} {$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TfAboutProject }

  TfAboutProject = class(TForm)
    bClose: TBitBtn;
    lblAutor: TLabel;
    lblName: TLabel;
    lblVersion: TLabel;
  private

  public

  end;

var
  fAboutProject: TfAboutProject;

implementation

{$R *.lfm}

end.

