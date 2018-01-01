unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  
  Vcl.ComCtrlsEx;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
  private
    ListViewEdit: TListViewEdit;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DbgTag := '[DSA] ';
  DragFiles := nil;
  ListView1OriginalProc := ListView1.WindowProc;
  ListView1.WindowProc := ListView1WindowProc;
  ListViewEdit := TListViewEdit.Create(ListView1);
  DragAcceptFiles(ListView1.Handle, True);
  Statu_Count   := StatusBar1.Panels[0];
  Statu_Invalid := StatusBar1.Panels[1];
  MediaList := TMediaList.Create;
end;

end.
