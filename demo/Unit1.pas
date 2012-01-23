unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VKAPI, StrUtils, md5hash, ComCtrls;

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
    Button5: TButton;
    Edit5: TEdit;
    Button7: TButton;
    Edit6: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Button6: TButton;
    Edit7: TEdit;
    Edit8: TEdit;
    TabSheet3: TTabSheet;
    Button8: TButton;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
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
  Fl: boolean;
begin
  VK := TVKontakte.Create(Edit1.Text);
  with VK do begin
    if RadioButton1.Checked then
      //Fl := Login(Edit2.Text, Edit3.Text)
      Fl := LoginOAuth(Edit2.Text, Edit3.Text, Edit1.Text, 'audio,wall')
    else
      Fl := LoginTestMode(Edit2.Text, Edit3.Text);
    if Fl then
      Label4.Caption := 'logged in'
    else begin
      ErrCode := GetError(str, Err);
      Label4.Caption := str + ': ' + Err + format('%d', [ErrCode]);
    end;
  end;
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
      Memo1.Lines.Add(TVKFriend(L[i]).LastName + ' ' + TVKFriend(L[i]).NickName + ' ' + TVKFriend(L[i]).FirstName + ' [' + TVKFriend(L[i]).id + ']');
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
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
    for i := 0 to L.Count - 1 do begin
      Memo1.Lines.Add(TVKFriend(L[i]).Id);
      TVKFriend(L[i]).Free;
    end;
    L.Free;
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
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
    for i := 0 to L.Count - 1 do begin
      Memo1.Lines.Add(TVKFriend(L[i]).Id);
      TVKFriend(L[i]).Free;
    end;
    L.Free;
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  L: TList;
  Err, str: string;
  i: integer;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;

  L := VK.APISearchAudio([Edit5.Text, '200', '0']);
  if Assigned(L) then begin
    for i := 0 to L.Count - 1 do begin
      Memo1.Lines.Add(Format('%s - %s (%d:%d) [%s, %s]', [TVKAudio(L[i]).Artist, TVKAudio(L[i]).Title, TVKAudio(L[i]).Duration div 60, TVKAudio(L[i]).Duration mod 60, TVKAudio(L[i]).AudioId, TVKAudio(L[i]).OwnerId]));
      TVKAudio(L[i]).Free;
    end;
    L.Free;
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Err, str: string;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;

  if VK.APIAddAudio([Edit7.Text, Edit8.Text]) then begin
    Memo1.Text := 'Audio successfully added';
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RadioButton1.Checked := true;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Label2.Caption := IfThen(RadioButton1.Checked, 'Login', 'User Id');
  Label3.Caption := IfThen(RadioButton1.Checked, 'Pass', 'Secret');
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  L: TList;
  Err, str: string;
  i: integer;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;

  L := VK.APIGetAudio([Edit6.Text]);
  if Assigned(L) then begin
    for i := 0 to L.Count - 1 do begin
      Memo1.Lines.Add(Format('%s - %s (%d:%d) [%s, %s]', [TVKAudio(L[i]).Artist, TVKAudio(L[i]).Title, TVKAudio(L[i]).Duration div 60, TVKAudio(L[i]).Duration mod 60, TVKAudio(L[i]).AudioId, TVKAudio(L[i]).OwnerId]));
      TVKAudio(L[i]).Free;
    end;
    L.Free;
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  Err, str: string;
begin
  Memo1.Text := '';
  if not Assigned(VK) then
    exit;

  if VK.APIPostOnWall([Edit11.Text, Edit12.Text, Edit13.Text, Edit9.Text, Edit10.Text]) then begin
    Memo1.Text := 'Message/Attachment successfully posted';
  end
  else begin
    VK.GetError(str, Err);
    Memo1.Text := str + ': ' + Err;
  end;
end;

end.
