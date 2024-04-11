unit Playlist;

{$mode ObjFPC} {$LONGSTRINGS ON} {$RANGECHECKS ON} {$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils;

type

  TPlaylist = class(TObject)
    private
      FFileNames: Array of TStringList;
      FFilePaths: Array of TStringList;
    public
      constructor Create; overload;
      destructor Destroy; override;
      procedure Add(ScenarioName: String);
      property FileNames: Array of TStringList read FFileNames write FFileNames;
  end;

implementation

end.

