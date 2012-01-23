(*
 * Wrapper provides access to the VKontakte Platform.
 * Author: 0zon
 *)
unit VKAPI;

interface

uses
  Classes, SysUtils, ActiveX, ComObj, StrUtils, md5hash;

const
  INTERNAL_NAME = 'Delphi wrapper for VK API';
  VERSION       = '0.2.0';
  API_URL       = 'http://api.vkontakte.ru/api.php';
  API_OAUTH_URL = 'https://api.vkontakte.ru/method/';

type
  TVKSex = (vkUnk, vkWomen, vkMan);
  TVKFriend = class
    Id,
    FirstName,
    LastName,
    NickName: String;
    Sex: TVKSex;
    Online: Boolean;
    BirthDate,
    City,
    Country,
    Photo,
    PhotoMedium,
    PhotoBig,
    PhotoRec: String;
    Lists: TList;
    Domain: String;
    HasMobile: Boolean;
    Rate,
    MobilePhone,
    HomePhone,
    University,
    UniversityName,
    Faculty,
    FacultyName: String;
    CanPost,
    CanWritePrivateMessage: Boolean;
    constructor Create(Id: String);
  end;
  TVKAudio = class
    AudioId,
    OwnerId,
    Artist,
    Title: String;
    Duration: integer;
    URL: String;
    constructor Create(Id: String);
  end;
  TAuthMode = (amOAuth, amTest, amOld);
  TVKontakte = class
  private
    FAPIUrl: String;
    FAppID: String;
    FLogin: String;
    FPassword: String;
    FSID: String;
    FMID: String;
    FSecret: String;
    FAccessToken: String;
    FPermisions: Longint;
    FIsLoggedIn: boolean;
    FHTTP: OLEVariant;
    FHTTPUsername, FHTTPPassword, FHTTPProxyServer, FHTTPProxyBypass, FHTTPProxyUsername, FHTTPProxyPassword: String;
    FXMLDoc: OLEVariant;
    FErrorClass: String;
    FError: String;
    FErrorCode: Integer;
    FMode: TAuthMode;
    procedure ClearError;
    function SetError(ErrorClass, Error: String; ErrorCode: Integer): Integer;
    function MD5(PlainText: String): String;
    function GenerateSig(Params: TStringList): String;
    function GetTextByNode(Node: OleVariant; NodeName: String): String;
    function API(MethodName: String; MethodParams: TStringList): boolean;
    function HTTPRequest(Method: String; Url: String; Data: String; Referer: string; Redirect: boolean; var Error: Integer): String;
    function HTTPBuildRequestParams(Params: TStringList): String;
  public
    constructor Create(AppID: String; Permisions: Longint = 15615);
    destructor Destroy; override;
    procedure HTTPSetSettings(Username: String; Password: String; ProxyServer: String;
      ProxyBypass: String; ProxyUsername: String; ProxyPassword: String);
    function Login(Login, Password: String): boolean;
    function LoginOAuth(Login, Password, ClientId: String; Scope: String = ''): boolean;
    function LoginTestMode(MID, Secret: String): boolean;
    function GetError(var ErrorClass, Error: String): Integer;
    property IsLoggedIn: boolean read FIsLoggedIn;
    property Mode: TAuthMode read FMode write FMode;
    { Friends methods }
    function APIGetFriends(Args: array of const): TList;
    function APIGetOnlineFriends(): TList;
    function APIGetMutualFriends(Args: array of const): TList;
    { Audio methods }
    function APISearchAudio(Args: array of const): TList;
    function APIGetAudio(Args: array of const): TList;
    function APIAddAudio(Args: array of const): boolean;
    { Wall methods }
    function APIPostOnWall(Args: array of const): boolean;
  end;

implementation

{ Additional methods }

function StrCut(Str, SubStrL, SubStrR: String) : String;
var
  pL, pR, lL : Integer;
begin
  Result := '';
  if Length(SubStrL) > 0 then begin
    pL := Pos(SubStrL, str);
    if pL = 0 then
      exit;
  end
  else
    pL := 1;

  lL := Length(SubStrL);
  if Length(SubStrR) > 0 then begin
    pR := PosEx(SubStrR, Str, pL + lL);
    if pR = 0 then
      exit;
  end
  else
    pR := Length(Str) + 1;
  
  if pR - (pL + lL) = 0 then
    exit;

  Result := MidStr(str, pL + lL, pR - (pL + lL));
end;

function ByteToHex(b: byte): String;
  function GetChar(b: byte): char;
  begin
    if b < 10 then Result := chr(Ord('0') + b)
    else Result := chr(Ord('a') - 10 + b);
  end;
begin
  Result := GetChar(b div 16) + GetChar(b mod 16);
end;

function UrlEncode(S: String; Space: boolean = false): String;
var
  I : Integer;
begin
  Result := '';
  for I := 1 to Length(S) do begin
    if (Space) and (AnsiChar(S[I]) = ' ') then
      Result := Result + '+'
    else
      if AnsiChar(S[I]) in ['0'..'9', 'A'..'Z', 'a'..'z'] then
          Result := Result + S[I]
      else
          Result := Result + '%' + ByteToHex(Ord(S[I]));
  end;
end;

function GetStrArg(Args: array of const; ArgNum: byte): string;
begin
  Result := '';
  if High(Args) < ArgNum then
    exit;
  case Args[ArgNum].VType of
    vtchar       :
      Result := Args[ArgNum].VChar;
    vtString     :
      Result := Args[ArgNum].VString^;
    vtPChar      :
      Result := Args[ArgNum].VPChar;
    vtAnsiString :
      Result := AnsiString(Args[ArgNum].VAnsiString);
  end;
end;

function NodeExist(Node: OleVariant): boolean;
begin
  Result := TVarData(Node).VDispatch <> nil;
end;

{ TVKFriend }

constructor TVKFriend.Create(Id: String);
begin
  inherited Create;
  Self.Id := Id;
end;

{ TVKAudio }

constructor TVKAudio.Create(Id: String);
begin
  inherited Create;
  Self.AudioId := Id;
end;

{ TVKontakte }

destructor TVKontakte.Destroy;
begin
  CoFreeUnusedLibraries;
  inherited;
end;

constructor TVKontakte.Create(AppID: String; Permisions: Longint = 15615);
begin
  inherited Create;
  FAPIUrl := API_URL;
  FAppID := AppID;
  FLogin := '';
  FPassword := '';
  FSID := '';
  FMID := '';
  FSecret := '';
  FAccessToken := '';
  FIsLoggedIn := false;
  FPermisions := Permisions;
  HTTPSetSettings('', '', '', '', '', '');
  ClearError;

  // Create the WinHttpRequest COM object
  FHttp := CreateOLEObject('WinHttp.WinHttpRequest.5.1');

  // Create the XMLDOM COM object
  FXMLDoc := CreateOLEObject('Microsoft.XMLDOM'); // or 'MSXML2.DOMDocument'
  FXMLDoc.Async := False;
end;

procedure TVKontakte.ClearError;
begin
  SetError('', '', -1);
end;

function TVKontakte.SetError(ErrorClass, Error: String; ErrorCode: Integer): Integer;
begin
  FErrorClass := ErrorClass;
  FError := Error;
  FErrorCode := ErrorCode;

  Result := FErrorCode;
end;

function TVKontakte.GetError(var ErrorClass, Error: String): Integer;
begin
  Result := FErrorCode;
  ErrorClass := FErrorClass;
  Error := FError;
end;

function TVKontakte.MD5(PlainText: String): String;
begin
  Result := lowercase(md5hash.md5(PlainText));
end;

function TVKontakte.GenerateSig(Params: TStringList): String;
var
  i:   Integer;
begin
  Params.Sort(); // todo: sort by key
  Result := '';
  for i := 0 to Params.Count - 1 do
    Result := Result + Params[i];
  Result := md5(FMID + Result + FSecret);
end;

function TVKontakte.GetTextByNode(Node: OleVariant; NodeName: String): String;
var
  ChildNode: OleVariant;
begin
  Result := '';
  if NodeName = '' then
    ChildNode := Node
  else
    ChildNode := Node.SelectSingleNode(NodeName);

  if NodeExist(ChildNode) then
    Result := ChildNode.Text;
end;

function TVKontakte.API(MethodName: String; MethodParams: TStringList): boolean;
var
  Params: TStringList;
  Error:   Integer;
  Response: String;
  Node: OleVariant;
  URL: string;
begin
  Result := false;

  Params := TStringList.Create;
  try
    if FMode = amOAuth then begin
      URL := API_OAUTH_URL + MethodName + '.xml';
      Params.Add('access_token=' + FAccessToken);
      Params.AddStrings(MethodParams);
    end
    else begin
      URL := FAPIUrl;
      Params.Add('api_id=' + FAppID);
      //Params.Add('format=XML'); // todo: json
      if FMode <> amTest then
        Params.Add('v=3.0')
      else
        Params.Add('test_mode=1');
      Params.Add('method=' + MethodName);
      Params.AddStrings(MethodParams);
      Params.Add('sig=' + GenerateSig(Params));
      if FMode <> amTest then
        Params.Add('sid=' + FSID);
    end;
    Response := HTTPRequest('POST', URL, HTTPBuildRequestParams(Params), '', true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  // validate response
  FXMLDoc.LoadXML(Response);
  if FXMLDoc.ParseError.ErrorCode <> 0 then begin
    SetError('XML', FXMLDoc.ParseError.Reason, FXMLDoc.ParseError.ErrorCode);
    Exit;
  end;

  Node := FXMLDoc.SelectSingleNode('/error');
  if NodeExist(Node) then begin
    SetError('Core', 'Response error: ' + Node.Text, 11);
    exit;
  end;

  Node := FXMLDoc.SelectSingleNode('/response');
  if not NodeExist(Node) then begin
    SetError('Core', 'Wrong response', 12);
    exit;
  end;

  Result := true;
end;

procedure TVKontakte.HTTPSetSettings(Username: String; Password: String; ProxyServer: String;
  ProxyBypass: String; ProxyUsername: String; ProxyPassword: String);
begin
  FHTTPUsername := Username;
  FHTTPPassword := Password;
  FHTTPProxyServer := ProxyServer;
  FHTTPProxyBypass := ProxyBypass;
  FHTTPProxyUsername := ProxyUsername;
  FHTTPProxyPassword := ProxyPassword;
end;

function TVKontakte.HTTPRequest(Method: String; Url: String; Data: String; Referer: string; Redirect: boolean; var Error: Integer): String;

const
  SXH_PROXY_SET_PROXY = 2;
  HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
  HTTPREQUEST_SETCREDENTIALS_FOR_PROXY = 1;

var
  ErrorCode: Integer;

begin
  ClearError;

  // Initially set the return value of the function to ''
  Result := '';

  if (FHTTPProxyServer <> '') then
  begin
    //Set proxy server and bypass list
    ErrorCode := FHttp.setProxy(SXH_PROXY_SET_PROXY,
      FHTTPProxyServer, FHTTPProxyBypass);
    if (ErrorCode <> S_OK) then begin
      Error := SetError('HTTP', 'Could not set Proxy server.', 1);
      exit;
    end;
  end;

  ErrorCode := FHttp.setAutoLogonPolicy(0);
  if (ErrorCode <> S_OK) then begin
    Error := SetError('HTTP', 'Could not call setAutoLogonPolicy.', 2);
    exit;
  end;

  {ErrorCode := FHttp.setTimeouts(20000, 20000, 30000, 30000);
  if (ErrorCode <> S_OK) then begin
    Error := SetError('HTTP', 'Could not set timeouts.', 3);
    exit;
  end;}  
  //FHttp.Option(6) := Redirect; // Turn on/off automatic redirects
  {if pos('#', URL) > 0 then
    Url := copy(Url, 1, pos('#', URL) - 1);}
  FHttp.Option(7) := 1; // disable url encode
  if (Method = 'GET') and (Data <> '') then
    ErrorCode := FHttp.Open(Method, Url + '?' + Data, false)
  else
    ErrorCode := FHttp.Open(Method, Url, false);
  if (ErrorCode <> S_OK) then begin
    Error := SetError('HTTP', 'Could not send GET request.', 4);
    exit;
  end;

  if (FHTTPUsername <> '') or (FHTTPPassword <> '') then
  begin
    ErrorCode := FHttp.SetCredentials(
      FHTTPUsername, FHTTPPassword,
      HTTPREQUEST_SETCREDENTIALS_FOR_SERVER);
    if (ErrorCode <> S_OK) then begin
      Error := SetError('HTTP', 'Could not call SetCredentials().', 5);
      exit;
    end;
  end;

  if (FHTTPProxyUsername <> '') then
  begin
    ErrorCode := FHttp.SetCredentials(
      FHTTPProxyUsername, FHTTPProxyPassword,
      HTTPREQUEST_SETCREDENTIALS_FOR_PROXY);
    if (ErrorCode <> S_OK) then begin
      Error := SetError('HTTP', 'Could not call SetCredentials().', 6);
      exit;
    end;
  end;

  FHttp.SetRequestHeader('User-Agent', INTERNAL_NAME + '/' + VERSION);
  if Referer <> '' then
    FHttp.SetRequestHeader('Referer', Referer);

  if Method = 'GET' then
    ErrorCode := FHttp.Send()
  else begin
    FHttp.SetRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    FHttp.SetRequestHeader('Content-Length', inttostr(Length(Data)));

    ErrorCode := FHttp.Send(WideString(Data));
  end;

  if (ErrorCode <> S_OK) then begin
    Error := SetError('HTTP', 'Could not call Send().', 7);
    exit;
  end;

  Result := FHttp.ResponseText;
  if FHttp.Status = 200 then
    Error := -1 // good response code
  else if (Redirect and FHttp.Status div 100 = 3) and (FHttp.Status <> 304) then //redirect
    Result := HTTPRequest('GET', FHTTP.getResponseHeader('Location'), '', Url, Redirect, Error)
  else
    Error := FHttp.Status;
end;

function TVKontakte.HTTPBuildRequestParams(Params: TStringlist): String;
var
  i, p: Integer;
  str: String;
begin
  Result := '';
  for i := 0 to Params.Count - 1 do begin
    str := Params[i];
    p := pos('=', str);
    Result := Result + '&' + copy(str, 1, p) + UrlEncode(copy(str, p + 1, MaxInt));
  end;
  delete(Result, 1, 1);
end;

function TVKontakte.Login(Login, Password: String): boolean;
const
  API_LOGIN_URL = 'http://vkontakte.ru/login.php';
var
  Params: TStringlist;
  Response, app_hash, settings_hash, s, Referrer: String;
  Error: Integer;
  Perm: Longint;
begin
  Result := false;
  Error := -1;

  FLogin := Login;
  FPassword := Password;
  FMode := amOld;

  // step 1
  Params := TStringList.Create;
  try
    Params.Add('app=' + FAppID);
    Params.Add('layout=popup');
    Params.Add('type=browser');
    Params.Add('settings=' + IntToStr(FPermisions));
    Response := HTTPRequest('GET', API_LOGIN_URL, HTTPBuildRequestParams(Params), '', true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  Referrer := FHTTP.Option(1);

  app_hash := StrCut(Response, 'name="app_hash" value="', '"');
  if app_hash = '' then begin
    Error := SetError('Core', 'Login failed.', 1);
    exit;
  end;

  // step 2
  Params := TStringList.Create;
  try
    Params.Add('act=login');
    Params.Add('app=' + FAppID);
    Params.Add('app_hash=' + app_hash);
    Params.Add('email=' + FLogin);
    Params.Add('pass=' + FPassword);
    Params.Add('permanent=1');
    Response := HTTPRequest('POST', 'http://login.vk.com', HTTPBuildRequestParams(Params), '', true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  app_hash := StrCut(Response, 'name="app_hash" value="', '"');
  s := StrCut(Response, 'name=''s'' value=''', '''');
  if (app_hash = '') or (s = '') then begin
    s := StrCut(Response, 'parent.onError(''', '''');
    Error := SetError('Core', 'Login failed. ' + s, 2);
    exit;
  end;

  // step 3
  Params := TStringList.Create;
  try
    Params.Add('act=auth_result');
    Params.Add('app=' + FAppID);
    Params.Add('app_hash=' + app_hash);
    Params.Add('expire=');
    Params.Add('m=4');
    Params.Add('permanent=1');
    Params.Add('s=' + s);
    Response := HTTPRequest('POST', API_LOGIN_URL, HTTPBuildRequestParams(Params), '', true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  FMID := StrCut(Response, '"mid":', ',');
  FSID := StrCut(Response, '"sid":"', '"');
  FSecret := StrCut(Response, 'secret":"', '"');
  settings_hash := StrCut(Response, 'settings_hash":"', '"');

  if not ((FMID <> '') and (FSID <> '') and (FSecret <> '') and (settings_hash <> '')) then begin
    Error := SetError('Core', 'Login failed.', 3);
    exit;
  end;

  // step 4
  Params := TStringList.Create;
  try
    Params.Add('vk=');
    Response := HTTPRequest('GET', 'http://login.vk.com/', HTTPBuildRequestParams(Params), Referrer, true, Error);
  finally
    Params.Free;
  end;
  s := StrCut(Response, '''s'' value=''', '''');
  if (s = '') then begin
    Error := SetError('Core', 'Login failed.', 4);
    exit;
  end;

  // step 5
  Params := TStringList.Create;
  try
    Params.Add('s=' + StrCut(Response, 'value=''', ''''));
    Response := HTTPRequest('POST', 'http://vkontakte.ru/login.php?op=slogin', HTTPBuildRequestParams(Params), FHTTP.Option(1), true, Error);
  finally
    Params.Free;
  end;

  // step 6
  Params := TStringList.Create;
  try
    Params.Add('addMember=1');
    Params.Add('hash=' + settings_hash);
    Params.Add('id=' + FAppID);
    Perm := 1;
    while Perm <= FPermisions do begin
      if FPermisions and Perm = Perm then
        Params.Add('app_settings_' + IntToStr(Perm) + '=1');
      Perm := Perm shl 1;
    end;
    Response := HTTPRequest('POST', 'http://vkontakte.ru/apps.php?act=a_save_settings', HTTPBuildRequestParams(Params), Referrer, true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  FIsLoggedIn := pos('"result":', Response) > 0;
  if not FIsLoggedIn then begin
    s := StrCut(Response, 'error":"', '"');
    Error := SetError('Core', 'Login failed. ' + s, 5);
  end;
  Result := FIsLoggedIn;
end;

function TVKontakte.LoginOAuth(Login, Password, ClientId: String; Scope: String = ''): boolean;
const
  API_OAUTH_LOGIN_URL = 'http://oauth.vkontakte.ru/authorize';
var
  Params: TStringlist;
  Response, app_hash, settings_hash, s, Referrer: String;
  Error: Integer;
  Perm: Longint;
begin
  Result := false;
  Error := -1;

  FLogin := Login;
  FPassword := Password;
  FMode := amOAuth;

  // step 1
  // http://oauth.vkontakte.ru/authorize?client_id=2232793&scope=audio,wall&redirect_uri=blank.html&display=page&response_type=token
  Params := TStringList.Create;
  try
    Params.Add('client_id=' + ClientId);
    Params.Add('scope=' + Scope);
    Params.Add('redirect_uri=blank.html');
    Params.Add('display=page');
    Params.Add('response_type=token');
    Response := HTTPRequest('GET', API_OAUTH_LOGIN_URL, HTTPBuildRequestParams(Params), '', true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  s := StrCut(Response, 'id="login_submit"', '</form>');
  if s = '' then begin
    Error := SetError('Core', 'Login failed.', 1);
    exit;
  end;

  // step 2
  Params := TStringList.Create;
  try
    Params.Add('q=' + StrCut(s, 'name="q" value="', '"'));
    Params.Add('ip_h=' + StrCut(s, 'name="ip_h" value="', '"'));
    Params.Add('from_host=' + StrCut(s, 'name="from_host" value="', '"'));
    Params.Add('from_protocol=' + StrCut(s, 'name="from_protocol" value="', '"'));
    Params.Add('to=' + StrCut(s, 'name="to" value="', '"'));
    Params.Add('expire=' + '86400');
    Params.Add('email=' + FLogin);
    Params.Add('pass=' + FPassword);
    HTTPRequest('POST', StrCut(s, 'action="', '"'), HTTPBuildRequestParams(Params), '', true, Error);
  finally
    Params.Free;
  end;
  if Error >= 0 then
    exit;

  Response := FHTTP.Option(1);
  s := StrCut(Response, 'access_token=', '&');
  FIsLoggedIn := s <> '';
  if not FIsLoggedIn then begin
    //s := StrCut(Response, 'error":"', '"');
    Error := SetError('Core', 'Login failed.', 5);
  end
  else
    FAccessToken := s;
  Result := FIsLoggedIn;
end;

function TVKontakte.LoginTestMode(MID, Secret: String): boolean;
begin
  FMode := amTest;
  FMID := MID;
  FSecret := Secret;
  FIsLoggedIn := true;

  Result := FIsLoggedIn;
end;

function TVKontakte.APIGetFriends(Args: array of const): TList;
// Args[0] - uid
// Args[1] - fields
// Args[2] - name_case
// Args[3] - count
// Args[4] - offset
// Args[5] - lid
var
  Params: TStringlist;
  i: Integer;
  XmlNodeList, NodeUser: OleVariant;
  VKFriend: TVKFriend;
  Res: boolean;
begin
  Result := nil;
  
  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;

  Params := TStringList.Create;
  if High(Args) >= 0 then
    Params.Add('uid=' + GetStrArg(Args, 0));
  if High(Args) >= 1 then
    Params.Add('fields=' + GetStrArg(Args, 1));
  if High(Args) >= 2 then
    Params.Add('name_case=' + GetStrArg(Args, 2));
  if High(Args) >= 3 then
    Params.Add('count=' + GetStrArg(Args, 3));
  if High(Args) >= 4 then
    Params.Add('offset=' + GetStrArg(Args, 4));
  if High(Args) >= 5 then
    Params.Add('lid=' + GetStrArg(Args, 5));
  res := API('friends.get', Params);
  Params.Free;
  if not res then
    exit;

  Result := TList.Create;

  // XML parsing
  XmlNodeList := FXMLDoc.SelectNodes('/response/user');
  if NodeExist(XmlNodeList) and XmlNodeList.Length > 0 then begin
    for i := 0 to XmlNodeList.Length - 1 do begin
      NodeUser := XmlNodeList.Item[i];
      VKFriend := TVKFriend.Create(GetTextByNode(NodeUser, 'uid'));
      with VKFriend do begin
        FirstName := GetTextByNode(NodeUser, 'first_name');
        LastName := GetTextByNode(NodeUser, 'last_name');
        NickName := GetTextByNode(NodeUser, 'nickname');
        Sex := TVKSex(strtoint(GetTextByNode(NodeUser, 'sex')));
        Online := (GetTextByNode(NodeUser, 'online') = '1');
        BirthDate := GetTextByNode(NodeUser, 'bdate');
        City := GetTextByNode(NodeUser, 'city');
        Country := GetTextByNode(NodeUser, 'country');
        Photo := GetTextByNode(NodeUser, 'photo');
        PhotoMedium := GetTextByNode(NodeUser, 'photo_medium');
        PhotoBig := GetTextByNode(NodeUser, 'photo_big');
        PhotoRec := GetTextByNode(NodeUser, 'photo_rec');
        //NodeUser.SelectNodes('/lists/lid');
        Domain := GetTextByNode(NodeUser, 'domain');
        HasMobile := (GetTextByNode(NodeUser, 'has_mobile') = '1');
        Rate := GetTextByNode(NodeUser, 'rate');
        MobilePhone := GetTextByNode(NodeUser, 'mobile_phone');
        HomePhone := GetTextByNode(NodeUser, 'home_phone');
        University := GetTextByNode(NodeUser, 'university');
        UniversityName := GetTextByNode(NodeUser, 'university_name');
        Faculty := GetTextByNode(NodeUser, 'Faculty');
        FacultyName := GetTextByNode(NodeUser, 'FacultyName');
        CanPost := (GetTextByNode(NodeUser, 'can_post') = '1');
        CanWritePrivateMessage := (GetTextByNode(NodeUser, 'can_write_private_message') = '1');
      end;
      Result.Add(VKFriend);
    end;
  end;
end;

function TVKontakte.APIGetOnlineFriends(): TList;
var
  Params: TStringlist;
  i: Integer;
  XmlNodeList, NodeUser: OleVariant;
  VKFriend: TVKFriend;
  Res: boolean;
begin
  Result := nil;

  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;

  Params := TStringList.Create;
  res := API('friends.getOnline', Params);
  Params.Free;
  if not res then
    exit;

  Result := TList.Create;

  // XML parsing
  XmlNodeList := FXMLDoc.SelectNodes('/response/uid');
  if NodeExist(XmlNodeList) and XmlNodeList.Length > 0 then begin
    for i := 0 to XmlNodeList.Length - 1 do begin
      NodeUser := XmlNodeList.Item[i];
      VKFriend := TVKFriend.Create(GetTextByNode(NodeUser, ''));
      Result.Add(VKFriend);
    end;
  end;
end;


function TVKontakte.APIGetMutualFriends(Args: array of const): TList;
// Args[0] - target_uid
// Args[1] - source_uid
var
  Params: TStringlist;
  i: Integer;
  XmlNodeList, NodeUser: OleVariant;
  VKFriend: TVKFriend;
  Res: boolean;
begin
  Result := nil;

  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;
  if High(Args) < 0 then begin
    SetError('Core', 'Not enough parameters.', 10);
    exit;
  end;

  Params := TStringList.Create;
  Params.Add('target_uid=' + GetStrArg(Args, 0));
  if High(Args) >= 1 then
    Params.Add('source_uid=' + GetStrArg(Args, 1));
  res := API('friends.getMutual', Params);
  Params.Free;
  if not res then
    exit;

  Result := TList.Create;

  // XML parsing
  XmlNodeList := FXMLDoc.SelectNodes('/response/uid');
  if NodeExist(XmlNodeList) and XmlNodeList.Length > 0 then begin
    for i := 0 to XmlNodeList.Length - 1 do begin
      NodeUser := XmlNodeList.Item[i];
      VKFriend := TVKFriend.Create(GetTextByNode(NodeUser, ''));
      Result.Add(VKFriend);
    end;
  end;
end;

function TVKontakte.APISearchAudio(Args: array of const): TList;
// Args[0] - q
// Args[1] - count
// Args[2] - offset
// Args[3] - sort
// Args[4] - lyrics
var
  Params: TStringlist;
  i: Integer;
  XmlNodeList, NodeUser: OleVariant;
  VKAudio: TVKAudio;
  Res: boolean;
begin
  Result := nil;

  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;

  Params := TStringList.Create;
  if High(Args) >= 0 then
    Params.Add('q=' + AnsiToUtf8(GetStrArg(Args, 0)));
  if High(Args) >= 1 then
    Params.Add('count=' + GetStrArg(Args, 1));
  if High(Args) >= 2 then
    Params.Add('offset=' + GetStrArg(Args, 2));

  res := API('audio.search', Params);
  Params.Free;
  if not res then
    exit;

  Result := TList.Create;

  // XML parsing
  XmlNodeList := FXMLDoc.SelectNodes('/response/audio');
  if NodeExist(XmlNodeList) and XmlNodeList.Length > 0 then begin
    for i := 0 to XmlNodeList.Length - 1 do begin
      NodeUser := XmlNodeList.Item[i];
      VKAudio := TVKAudio.Create(GetTextByNode(NodeUser, 'aid'));
      with VKAudio do begin
        OwnerId := GetTextByNode(NodeUser, 'owner_id');
        Artist := GetTextByNode(NodeUser, 'artist');
        Title := GetTextByNode(NodeUser, 'title');
        Duration := strtoint(GetTextByNode(NodeUser, 'duration'));
        URL := GetTextByNode(NodeUser, 'url');
      end;
      Result.Add(VKAudio);
    end;
  end;
end;

function TVKontakte.APIGetAudio(Args: array of const): TList;
// Args[0] - uid
// Args[1] - gid
// Args[2] - aids
// Args[3] - need_user
var
  Params: TStringlist;
  i: Integer;
  XmlNodeList, NodeUser: OleVariant;
  VKAudio: TVKAudio;
  Res: boolean;
begin
  Result := nil;

  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;

  Params := TStringList.Create;
  if High(Args) >= 0 then
    Params.Add('uid=' + GetStrArg(Args, 0));
  if High(Args) >= 1 then
    Params.Add('gid=' + GetStrArg(Args, 1));
  if High(Args) >= 2 then
    Params.Add('aids=' + GetStrArg(Args, 2));
  if High(Args) >= 3 then
    Params.Add('need_user=' + GetStrArg(Args, 3));

  res := API('audio.get', Params);
  Params.Free;
  if not res then
    exit;

  Result := TList.Create;

  // XML parsing
  XmlNodeList := FXMLDoc.SelectNodes('/response/audio');
  if NodeExist(XmlNodeList) and XmlNodeList.Length > 0 then begin
    for i := 0 to XmlNodeList.Length - 1 do begin
      NodeUser := XmlNodeList.Item[i];
      VKAudio := TVKAudio.Create(GetTextByNode(NodeUser, 'aid'));
      with VKAudio do begin
        OwnerId := GetTextByNode(NodeUser, 'owner_id');
        Artist := GetTextByNode(NodeUser, 'artist');
        Title := GetTextByNode(NodeUser, 'title');
        Duration := strtoint(GetTextByNode(NodeUser, 'duration'));
        URL := GetTextByNode(NodeUser, 'url');
      end;
      Result.Add(VKAudio);
    end;
  end;
end;

function TVKontakte.APIAddAudio(Args: array of const): boolean;
// Args[0] - aid
// Args[1] - oid
// Args[2] - gid
var
  Params: TStringlist;
  Res: boolean;
begin
  Result := false;

  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;

  Params := TStringList.Create;
  if High(Args) >= 0 then
    Params.Add('aid=' + GetStrArg(Args, 0));
  if High(Args) >= 1 then
    Params.Add('oid=' + GetStrArg(Args, 1));
  if High(Args) >= 2 then
    Params.Add('gid=' + GetStrArg(Args, 2));

  res := API('audio.add', Params);
  Params.Free;
  if not res then
    exit;

  Result := NodeExist(FXMLDoc.SelectSingleNode('/response'));//GetTextByNode(FXMLDoc.SelectSingleNode('/response'), '') = '1';
end;

function TVKontakte.APIPostOnWall(Args: array of const): boolean;
// Args[0] - owner_id
// Args[1] - message
// Args[2] - attachment type
// Args[3] - attachment owner_id
// Args[4] - attachment media_id
// Args[5] - export
var
  Params: TStringlist;
  XmlNodeList: OleVariant;
  Res: boolean;
begin
  Result := false;

  if not FIsLoggedIn then begin
    SetError('Core', 'Should authorize.', 10);
    exit;
  end;

  Params := TStringList.Create;
  if High(Args) >= 0 then
    Params.Add('owner_id=' + GetStrArg(Args, 0));
  if High(Args) >= 1 then
    Params.Add('message=' + AnsiToUtf8(GetStrArg(Args, 1)));
  if High(Args) >= 4 then
    Params.Add('attachment=' + GetStrArg(Args, 2) + GetStrArg(Args, 3) + '_' + GetStrArg(Args, 4));
  if High(Args) >= 5 then
    Params.Add('export=' + GetStrArg(Args, 5));

  res := API('wall.post', Params);
  Params.Free;
  if not res then
    exit;

  // XML parsing
  XmlNodeList := FXMLDoc.SelectSingleNode('/response');

  Result := Length(GetTextByNode(XmlNodeList, 'post_id')) > 0;
end;

initialization
  CoInitialize(nil);

finalization
  CoUninitialize();

{
v0.1.0
[+] Methods APIGetFriends, APIGetOnlineFriends, APIGetMutualFriends

v0.1.1
[+] Test mode
[+] Methods APISearchAudio, APIGetAudio, APIAddAudio

v0.1.2
[~] minor fixes, auth in with adding application
[+] APIPostOnWall

v0.2.0
[~] fixes: http redirect, http request convert to utf8
[+] APIPostOnWall
}
end.