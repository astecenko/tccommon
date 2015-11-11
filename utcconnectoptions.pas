unit utcconnectoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPdes, DCPsha1;

type

  { TTeamCenterConnectInfo }

  TTeamCenterConnectInfo = Class(TObject)
    private
      FBase:string;
      FCaption:string;
      FHost:string;
      FPort:string;
      FPassword:string;
      FUSer:string;
      procedure SetBase(AValue: string);
      procedure SetCaption(AValue: string);
      procedure SetHost(AValue: string);
      procedure SetPassword(AValue: String);
      procedure SetPort(AValue: string);
      procedure SetUser(AValue: string);

    public
      property Base:string read FBase write SetBase;
      property Caption:string read FCaption write SetCaption;
      property Host:string read FHost write SetHost;
      property Port:string read FPort write SetPort;
      property Password:String read FPassword write SetPassword;
      property User:string read FUSer write SetUser;

      constructor Create; overload;
      constructor Create(aBase, aCaption, aHost, aPort, aPassword, aUser:string); overload;
      destructor Destroy; override;
  end;

  { TTeamCenterConnectOptions }

  TTeamCenterConnectOptions = Class(TObject)
    private
      FDCP_des1: TDCP_des;
      FKeyText: string;
      FRootPath:string; //корневой путь к хранилищу опций
      FItems:TList;
      FItemsCaption:TStringList;
      procedure SetItems(AValue: TList);
      procedure SetItemsCaption(AValue: TStringList);
      procedure SetRootPath(AValue: string);
      procedure SetKeyText(AValue: string);

    protected


    public

      property RootPath:string read FRootPath write SetRootPath;
      property KeyText:string read FKeyText write SetKeyText;
      property ItemsCaption:TStringList read FItemsCaption write SetItemsCaption;
      property Items:TList read FItems write SetItems;

      procedure GetItemsCaption(aItems:TStrings); virtual; abstract;
      function GetItemByCaption(aCaption:string):TTeamCenterConnectInfo; virtual; abstract;
      function Add(aItem:TTeamCenterConnectInfo):integer; virtual; abstract;

      procedure Init; virtual; abstract;

      constructor Create; overload;
      destructor Destroy; override;
  end;



  { TTeamCenterConnectOptionsRegistry }

  TTeamCenterConnectOptionsRegistry = Class(TTeamCenterConnectOptions)
    protected

    public
      procedure GetItemsCaption(aItems:TStrings); override;
      function GetItemByCaption(aCaption:string):TTeamCenterConnectInfo; override;
      function Add(aItem:TTeamCenterConnectInfo):integer; override;
      procedure Init; override;
      constructor Create; overload;
      destructor Destroy; override;
  end;
implementation

uses registry, IniFiles;

{ TTeamCenterConnectOptionsRegistry }


procedure TTeamCenterConnectOptionsRegistry.GetItemsCaption(aItems: TStrings);
var
  a:TRegIniFile;
  i:integer;
  s:string;
begin
  aItems.Clear;
  a:=TRegIniFile.Create(RootPath);
  a.ReadSections(aItems);
  for i:=0 to aItems.Count-1 do
    begin
      s:=AnsiToUtf8(a.ReadString(aItems[i],'caption',''));
      if s<>'' then aItems[i]:=s
      else aItems[i]:=AnsiToUtf8(aItems[i]);
    end;
  FreeAndNil(a);
end;

function TTeamCenterConnectOptionsRegistry.GetItemByCaption(aCaption:string): TTeamCenterConnectInfo;
var
  a:TRegIniFile;
  s0,s1,s2,s3,s4:string;
begin
  s0:=Utf8ToAnsi(aCaption);
  a:=TRegIniFile.Create(RootPath);
  s1:=a.ReadString(aCaption,'base','');
  s1:=AnsiToUtf8(FDCP_des1.DecryptString(s1));
  s2:=a.ReadString(s0,'host','127.0.0.1');
  s2:=AnsiToUtf8(FDCP_des1.DecryptString(s2));
  s3:=a.ReadString(s0,'pwd','');
  s3:=AnsiToUtf8(FDCP_des1.DecryptString(s3));
  s4:=a.ReadString(s0,'usr','');
  s4:=AnsiToUtf8(FDCP_des1.DecryptString(s4));
  Result:=TTeamCenterConnectInfo.Create(s1,aCaption,s2,a.ReadString(s0,'port','1521'),s3,s4);
  FreeAndNil(a);
end;

function TTeamCenterConnectOptionsRegistry.Add(aItem: TTeamCenterConnectInfo
  ): integer;
var
  a:TRegistry;
  s:string;
begin
  a:=TRegistry.Create(KEY_ALL_ACCESS);
  a.RootKey:=HKEY_CURRENT_USER;
  s:=RootPath+aItem.Caption+'\';
  a.OpenKey(s,True);
  a.WriteString('caption', aItem.Caption);
  a.WriteString('host',    aItem.Host);
  a.WriteString('port',    aItem.Port);
  a.WriteString('base',    aItem.Base);
  a.WriteString('usr',     aItem.User);
  a.WriteString('pwd',     aItem.Password);
  FreeAndNil(a);
  Result:=0;
end;

procedure TTeamCenterConnectOptionsRegistry.Init;
 var
    a:TMemIniFile;
    i:integer;
    s,s0,s1,s2,s3,s4,s5:string;

begin
  FDCP_des1.InitStr(FKeyText,TDCP_sha1);
  FItems.Clear;
  FItemsCaption.Clear;
  a:=TMemIniFile.Create(RootPath);
  a.UpdateFile;
  a.ReadSections(FItemsCaption);
  for i:=0 to FItemsCaption.Count-1 do
    begin
      s0:=a.ReadString(FItemsCaption[i],'caption','');
      if s0='' then s0:=FItemsCaption[i]
        else s0:=FDCP_des1.DecryptString(s0);
      s1:=a.ReadString(FItemsCaption[i],'base','');
      s1:=FDCP_des1.DecryptString(s1);
      s2:=a.ReadString(FItemsCaption[i],'host','127.0.0.1');
      s2:=FDCP_des1.DecryptString(s2);
      s3:=a.ReadString(FItemsCaption[i],'pwd','');
      s3:=FDCP_des1.DecryptString(s3);
      s4:=a.ReadString(FItemsCaption[i],'usr','');
      s4:=FDCP_des1.DecryptString(s4);
      s5:=a.ReadString(FItemsCaption[i],'port','1521');
      s5:=FDCP_des1.DecryptString(s5);
      FItemsCaption[i]:=s0;
{      s:=AnsiToUtf8(a.ReadString(FItemsCaption[i],'caption',''));
      if s<>'' then FItemsCaption[i]:=s
      else FItemsCaption[i]:=AnsiToUtf8(FItemsCaption[i]);
      s1:=a.ReadString(s0,'base','');
      s1:=Utf8ToAnsi(s1);
      s1:=FDCP_des1.DecryptString(s1);
   //   s1:=AnsiToUtf8(s1);
      s2:=a.ReadString(s0,'host','127.0.0.1');
      s2:=FDCP_des1.DecryptString(s2);
      s3:=a.ReadString(s0,'pwd','');
      s3:=FDCP_des1.DecryptString(s3);
      s4:=a.ReadString(s0,'usr','');
      s4:=FDCP_des1.DecryptString(s4);}
      FItems.Add(TTeamCenterConnectInfo.Create(s1,s0,s2,s5,s3,s4));
    end;
  FreeAndNil(a);
end;

constructor TTeamCenterConnectOptionsRegistry.Create;
begin
  inherited Create;

end;

destructor TTeamCenterConnectOptionsRegistry.Destroy;
begin
  inherited Destroy;
end;

{ TTeamCenterConnectInfo }

procedure TTeamCenterConnectInfo.SetBase(AValue: string);
begin
  if FBase=AValue then Exit;
  FBase:=AValue;
end;

procedure TTeamCenterConnectInfo.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TTeamCenterConnectInfo.SetHost(AValue: string);
begin
  if FHost=AValue then Exit;
  FHost:=AValue;
end;

procedure TTeamCenterConnectInfo.SetPassword(AValue: String);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
end;

procedure TTeamCenterConnectInfo.SetPort(AValue: string);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

procedure TTeamCenterConnectInfo.SetUser(AValue: string);
begin
  if FUSer=AValue then Exit;
  FUSer:=AValue;
end;

constructor TTeamCenterConnectInfo.Create;
begin
  inherited Create;
end;

constructor TTeamCenterConnectInfo.Create(aBase, aCaption, aHost, aPort,
  aPassword, aUser: string);
begin
  Create;
  FBase:=aBase;
  FCaption:=aCaption;
  if aHost='' then
    FHost:='localhost'
   else
    FHost:=aHost;
  if aPort='' then
    FPort:='1521'
   else
    FPort:=aPort;
  FPassword:=aPassword;
  FUSer:=aUSer;
end;

destructor TTeamCenterConnectInfo.Destroy;
begin
  inherited Destroy;
end;

{ TTeamCenterConnectOptions }

procedure TTeamCenterConnectOptions.SetRootPath(AValue: string);
begin
  if FRootPath=AValue then Exit;
  FRootPath:=AValue;
end;

procedure TTeamCenterConnectOptions.SetKeyText(AValue: string);
begin
  if FKeyText=AValue then Exit;
  FKeyText:=AValue;
end;

procedure TTeamCenterConnectOptions.SetItemsCaption(AValue: TStringList);
begin
  if FItemsCaption=AValue then Exit;
  FItemsCaption:=AValue;
end;

procedure TTeamCenterConnectOptions.SetItems(AValue: TList);
begin
  if FItems=AValue then Exit;
  FItems:=AValue;
end;

constructor TTeamCenterConnectOptions.Create;
begin
  inherited Create;
  FItems:=TList.Create;
  FItemsCaption:=TStringList.Create;
  FDCP_des1:=TDCP_des.Create(nil);
end;

destructor TTeamCenterConnectOptions.Destroy;
  var
    i:integer;
    a:TTeamCenterConnectInfo;
begin
  for i:=0 to FItems.Count-1 do
    begin
     a:=TTeamCenterConnectInfo(FItems[i]);
     FreeAndNil(a);
    end;
  FItems.Clear;
  FreeAndNil(FItems);
  FreeAndNil(FItemsCaption);
  FDCP_des1.Burn;
  FreeAndNil(FDCP_des1);
  inherited Destroy;
end;

end.

