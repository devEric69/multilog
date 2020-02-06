unit fhelp;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls;

type

  { TfrmHelp }

  TfrmHelp = class(TForm)
    btnOk: TBitBtn;
    memHelp: TMemo;
    pnlAll: TPanel;
    pnlBottom: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;


implementation

{$R *.lfm}


{ TfrmHelp }

procedure TfrmHelp.FormCreate(Sender: TObject);
{$Include help_user.inc}
begin
	memHelp.Lines.Add(csMemHelp);
end;

end.

