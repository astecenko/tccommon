unit uperson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection;

type

  { T_TC_PERSON_INFO }
  T_TC_PERSON_INFO = record
    UID:string[16]; //уникальный идентификатор информации о пользователе
    USER_ID:string[32]; //идентификатор пользователя
    PLM_PUSER_ID: string[32];
    PLM_PUID: string[15];
    USER_NAME:string[128]; //имя пользователя
    STATUS:integer; //0 активен, 1 не активен, 2 активный администратор с максимальным доступом
    LAST_LOGIN_TIME:TDateTime;
  end;

  T_TC_PersonManager = Class;

  { T_TC_Person }
  T_TC_Person = Class(TObject)
    private
      FActive: boolean;
      FAttributeRO: TStringList;
      FAttributeRW: TStringList;
      FID: LongInt;
      FisAdmin: boolean;
      FisCorrector: boolean;
      FLinked: boolean;
      FManager: T_TC_PersonManager;
      FInfo: T_TC_PERSON_INFO;
      FUserName:string;
      procedure SetActive(AValue: boolean);
      procedure SetID(AValue: LongInt);
      procedure SetisAdmin(AValue: boolean);
      procedure SetisCorrector(AValue: boolean);
      procedure SetManager(AValue: T_TC_PersonManager);
      procedure SetUserName(AValue: string);
    public
      property UserName: string read FUserName write SetUserName;
      // Есть ли сязь с учетной записью TeamCenter
      property Linked: boolean read FLinked;
      // Идентификатор пользователя
      property ID:LongInt read FID write SetID;
      property AttributeRW : TStringList read FAttributeRW;
      property AttributeRO : TStringList read FAttributeRO;
      property Info: T_TC_PERSON_INFO read FInfo;
      property Active:boolean read FActive write SetActive;
      property isAdmin:boolean read FisAdmin write SetisAdmin;
      property isCorrector:boolean read FisCorrector write SetisCorrector;
      property Manager:T_TC_PersonManager read FManager write SetManager;

      procedure Refresh;
      function Auth(const aUserName, aPassword: string):boolean;
      procedure UpdateLastLoginTime;


      constructor Create; overload;

      destructor Destroy; override;
  end;

  { T_TC_PersonManager }

  T_TC_PersonManager = Class(TObject)
    private
      FActive: Boolean;
      FConnection: TZConnection;
      procedure SetActive(AValue: Boolean);
      procedure SetConnection(AValue: TZConnection);
    public
      property Connection:TZConnection read FConnection write SetConnection;
      property Active:Boolean read FActive write SetActive;

      procedure Open;
      procedure Close;
      function Exist(const aUserName:string):boolean;
      function GetPerson(const aUserName, aPassword:string):T_TC_Person;
      procedure GetPLMUserIDList(aList:TStrings);
      procedure GetTCUserInfoFromUserID(const aUserID:string; var aUserPUID:string; var aUserName:string);
      function AddUser(const aPuid,aUserID,aPLMUserID,aPLMPUID,aUserName,aPassword:string; const aStatus:integer=0):string;
      function EditUser(const aPuid,aUserID,aPLMUserID,aPLMPUID,aUserName,aPassword:string; const aStatus:integer=0):string;

      procedure GetUsers(aList:TStrings);

      constructor Create; overload;
      constructor Create(aConn: TZConnection); overload;
      destructor Destroy; override;
  end;
implementation
  uses DB, ZDataset, comunit;

{ T_TC_PersonManager }

procedure T_TC_PersonManager.SetConnection(AValue: TZConnection);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
end;

procedure T_TC_PersonManager.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if FActive then Open else Close;
end;

procedure T_TC_PersonManager.Open;
begin
  Connection.Connected:=True;
end;

procedure T_TC_PersonManager.Close;
begin
  Connection.Connected:=False;
end;


// Проверка существования пользвателя
function T_TC_PersonManager.Exist(const aUserName: string): boolean;
  var
    sql1:TZReadOnlyQuery;
    s:string;
begin
  Result:=False;
  s:=Trim(lowercase(aUserName));
  if s<>'' then
    begin
      sql1:=TZReadOnlyQuery.Create(nil);
      sql1.Connection:=Connection;
      sql1.SQL.Text:='select distinct user_id from infodba.tcv_user where user_id='+QuotedStr(s);
      sql1.Open;
      Result := sql1.RecordCount>0;
      sql1.Close;
      FreeAndNil(sql1);
    end;
end;

function T_TC_PersonManager.GetPerson(const aUserName, aPassword: string
  ): T_TC_Person;
  var
    a:T_TC_Person;
begin
  Result:=nil;
  if Exist(aUserName) then
    begin
      a:=T_TC_Person.Create;
      a.Manager:=Self;
      if a.Auth(aUserName, aPassword) then
        Result:=a
       else
        FreeAndNil(a);
    end;
end;

procedure T_TC_PersonManager.GetPLMUserIDList(aList: TStrings);
var
  qer1:TZReadOnlyQuery;
begin
 qer1:=TZReadOnlyQuery.Create(nil);
 qer1.Connection:=Connection;
 qer1.SQL.Text:='select puser_id from infodba.ppom_user';
 qer1.Open;
 aList.Clear;
 while not(qer1.EOF) do
   begin
     aList.Add(qer1.Fields[0].AsString);
     qer1.Next;
   end;
 qer1.Close;
 FreeAndNil(qer1);
end;

procedure T_TC_PersonManager.GetTCUserInfoFromUserID(const aUserID: string;
    var aUserPUID: string; var aUserName: string);
  var
    qer1:TZReadOnlyQuery;
    s:string;
begin
  qer1:=TZReadOnlyQuery.Create(nil);
  qer1.Connection:=Connection;
  s:=Trim(lowercase(aUserID));
  qer1.SQL.Text:='select distinct puid, puser_name from infodba.ppom_user where puser_id='+QuotedStr(s);
  qer1.Open;
  if qer1.RecordCount>0 then
    begin
      aUserPUID:=qer1.Fields[0].AsString;
      aUserName:=qer1.Fields[1].AsString;
    end
   else
    begin
      aUserPUID:='';
      aUserName:='';
    end;
   qer1.Close;
   FreeAndNil(qer1);
end;

function T_TC_PersonManager.AddUser(const aPuid, aUserID, aPLMUserID, aPLMPUID,
  aUserName, aPassword: string; const aStatus: integer): string;
var
  qer1:TZQuery;
  p,s:string;
begin
  Result:='';
  s:=Trim(lowercase(aUserID));
  p:=GetMD5(aPassword);
  qer1:=TZQuery.Create(nil);
  qer1.Connection:=Connection;
  qer1.SQL.Text:='select puid from infodba.tcv_user where puid='+QuotedStr(aPuid);
  qer1.Open;
  if qer1.RecordCount>0 then
    Result:='user with puid='+aPuid+' already exists!'
   else
    begin
      qer1.Close;
      qer1.SQL.Text:='select puid from infodba.tcv_user where user_id='+QuotedStr(s);
      qer1.Open;
      if qer1.RecordCount>0 then
        Result:='user with user_id='+aUserID+' already exists with puid='+qer1.Fields[0].AsString+'!'
       else
        begin
          qer1.Close;
          qer1.SQL.Text:='INSERT INTO INFODBA.TCV_USER VALUES ('+QuotedStr(aPuid)
          +', '+QuotedStr(s)+', '+QuotedStr(aPLMUserID)+', '
          +QuotedStr(aPLMPUID)+', '+QuotedStr(aUserName)+', null, '
          +IntToStr(aStatus)+', '+QuotedStr(p)+')';
          try
            qer1.ExecSQL;
          except
            Result:='SQL database error';
          end;
        end;
    end;
  qer1.Close;
  FreeAndNil(qer1);
end;

function T_TC_PersonManager.EditUser(const aPuid, aUserID, aPLMUserID,
  aPLMPUID, aUserName, aPassword: string; const aStatus: integer): string;
var
  qer1:TZQuery;
  p,s:string;
  changepwd:boolean;
begin
  Result:='';
  changepwd:=aPassword<>'';
  s:=Trim(lowercase(aUserID));
  if changepwd then p:=GetMD5(aPassword);
  qer1:=TZQuery.Create(nil);
  qer1.Connection:=Connection;
  qer1.SQL.Text:='UPDATE INFODBA.TCV_USER SET USER_ID='+QuotedStr(s)+', '
    +'PLM_PUSER_ID='+QuotedStr(aPLMUserID)+', '
    +'PLM_PUID='+QuotedStr(aPLMPUID)+', '
    +'USER_NAME='+QuotedStr(aUserName)+', '
    +'PSTATUS='+IntToStr(aStatus);
  if changepwd then qer1.SQL.Text:=qer1.SQL.Text+', PWD='+QuotedStr(p);
  qer1.SQL.Text:=qer1.SQL.Text+' WHERE PUID='+QuotedStr(aPuid);
  ShowMessageEx(qer1.SQL.Text);
  try
    qer1.ExecSQL;
  except
    Result:='SQL database error';
  end;
  qer1.Close;
  FreeAndNil(qer1);
end;

procedure T_TC_PersonManager.GetUsers(aList: TStrings);
var
  quer1:TZReadOnlyQuery;
begin
  quer1:=TZReadOnlyQuery.Create(nil);
  quer1.Connection:=Connection;
  quer1.SQL.Add('SELECT PUID || ''='' || USER_NAME as USERLIST from INFODBA.TCV_USER');
  quer1.Open;
  aList.Clear;
  while not (quer1.EOF) do
    begin
      aList.Add(quer1.Fields[0].AsString);
      quer1.Next;
    end;
  quer1.Close;
  FreeAndNil(quer1);
end;

constructor T_TC_PersonManager.Create;
begin
  inherited Create;

end;

constructor T_TC_PersonManager.Create(aConn: TZConnection);
begin
  Create;
  FConnection:=aConn;
end;

destructor T_TC_PersonManager.Destroy;
begin

  inherited Destroy;
end;


{ T_TC_Person }

procedure T_TC_Person.SetUserName(AValue: string);
begin
  if FUserName=AValue then Exit;
  FUserName:=AValue;
end;

procedure T_TC_Person.SetID(AValue: LongInt);
begin
  if FID=AValue then Exit;
  FID:=AValue;
end;

procedure T_TC_Person.SetisAdmin(AValue: boolean);
begin
  if FisAdmin=AValue then Exit;
  FisAdmin:=AValue;
end;

procedure T_TC_Person.SetisCorrector(AValue: boolean);
begin
  if FisCorrector=AValue then Exit;
  FisCorrector:=AValue;
end;

procedure T_TC_Person.SetManager(AValue: T_TC_PersonManager);
begin
  if FManager=AValue then Exit;
  FManager:=AValue;
end;

procedure T_TC_Person.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
end;

procedure T_TC_Person.Refresh;
begin

end;

// авторизация
function T_TC_Person.Auth(const aUserName, aPassword: string): boolean;
var
  sql1:TZReadOnlyQuery;
  s,p1,p2:string;
begin
 Result:=False;
 s:=Trim(lowercase(aUserName));
 if s<>'' then
   begin
     sql1:=TZReadOnlyQuery.Create(nil);
     sql1.Connection:=Self.Manager.Connection;
     sql1.SQL.Text:='select distinct * from infodba.tcv_user where user_id='+QuotedStr(s);
     sql1.Open;
     if sql1.RecordCount>0 then
       begin
         p1 := GetMD5(aPassword);
         p2 := sql1.FieldByName('PWD').AsString;
         Result:=p1=p2;
         if Result then
           begin
             with FInfo do
               begin
                 UID:=sql1.FieldByName('PUID').AsString;
                 LAST_LOGIN_TIME:=sql1.FieldByName('LAST_LOGIN_TIME').AsDateTime;
                 USER_ID:=s;
                 USER_NAME:=sql1.FieldByName('USER_NAME').AsString;
                 STATUS:=sql1.FieldByName('PSTATUS').AsInteger;
                 isAdmin:=STATUS=100;
                 isCorrector:=STATUS>19;
                 PLM_PUID:=sql1.FieldByName('PLM_PUID').AsString;
                 PLM_PUSER_ID:=sql1.FieldByName('PLM_PUSER_ID').AsString;
               end;
             UpdateLastLoginTime;
           end;
       end
      else
       Result:=False;
     sql1.Close;
     FreeAndNil(sql1);
   end;
end;

procedure T_TC_Person.UpdateLastLoginTime;
var
   qer1:TZQuery;
begin
  qer1:=TZQuery.Create(nil);
  qer1.Connection:=FManager.Connection;
  qer1.SQL.Text:='UPDATE INFODBA.TCV_USER SET LAST_LOGIN_TIME=SYSDATE WHERE PUID='+QuotedStr(FInfo.UID);
  qer1.ExecSQL;
  FreeAndNil(qer1);
end;


constructor T_TC_Person.Create;
begin
  inherited Create;
  FAttributeRO:= TStringList.Create;
  FAttributeRW:= TStringList.Create;
  FAttributeRO.Duplicates:=dupIgnore;
  FAttributeRO.Sorted:=True;
  FAttributeRW.Duplicates:=dupIgnore;
  FAttributeRW.Sorted:=True;
  FLinked:=False;
end;

destructor T_TC_Person.Destroy;
begin
  FreeAndNil(FAttributeRW);
  FreeAndNil(FAttributeRO);
  inherited Destroy;
end;

end.
