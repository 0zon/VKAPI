unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VKAPI, StrUtils, md5hash;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Label4: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit4: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  VK: TVKontakte;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Err, str: string;
  ErrCode: integer;
begin
  VK := TVKontakte.Create(Edit1.Text);
  with VK do begin
    //HTTPSetSettings('', '', '192.168.167.3:3128', '', '', '');
    if Login(Edit2.Text, Edit3.Text) then
      Label4.Caption := 'logged in'
    else begin
      ErrCode := GetError(str, Err);
      Label4.Caption := str + ': ' + Err + format('%d', [ErrCode]);
    end;
  end;
  //posex(
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  L: TList;
  Err, str: string;
  i: integer;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;

  L := VK.APIGetFriends(['', 'photo_big,sex,contacts,nickname']);
  if Assigned(L) then begin
    for i := 0 to L.Count - 1 do
      Memo1.Lines.Add(TVKFriend(L[i]).LastName + ' ' + TVKFriend(L[i]).NickName + ', ' + TVKFriend(L[i]).id);
  end
  else begin
    VK.GetError(str, Err);
    Label4.Caption := str + ': ' + Err;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  L: TList;
  Err, str: string;
  i: integer;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;

  L := VK.APIGetOnlineFriends();
  if Assigned(L) then begin
    for i := 0 to L.Count - 1 do
      Memo1.Lines.Add(TVKFriend(L[i]).Id);
  end
  else begin
    VK.GetError(str, Err);
    Label4.Caption := str + ': ' + Err;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  L: TList;
  Err, str: string;
  i: integer;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;
  
  L := VK.APIGetMutualFriends([Edit4.Text]);
  if Assigned(L) then begin
    for i := 0 to L.Count - 1 do
      Memo1.Lines.Add(TVKFriend(L[i]).Id);
  end
  else begin
    VK.GetError(str, Err);
    Label4.Caption := str + ': ' + Err;
  end;end;

end.
