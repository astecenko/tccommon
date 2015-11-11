unit comunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  csDefaultFormatUID='0000000000000000';
  csRootPCID='ICM';

function GetMD5(aStr:string):string;
function Padr(expression: string; nLength: Integer; cFillChar: Char = ' '): string;
function Padl(expression: string; nLength: Integer; cFillChar: Char = ' '):string;
procedure ShowMessageEx(const Msg: string; const ReadOnly: boolean = True; const Width: integer = 400; const Height: integer = 300);
function GetUID:string;
procedure ApplyTemplate(var aTemplate:string; const aPattern, aValue: string);
function Stepen(const X, N:integer):integer;

// Получение по индексу имени поля
function GetPSM_Name(const ind: integer): string;

implementation
  uses
    DCPmd5,Forms,StdCtrls,Controls;

{Дополняет строку expression справа символом cFillChar до длины nLength}
function Padr(expression: string; nLength: Integer; cFillChar: Char = ' '): string;
    var
      n, i: Integer;
      s: string;
    begin
      s := Trim(expression);
      n := Length(s);
      if n=0 then
        begin
          Result:='';
          for i:=1 to nLength do
            Result := Result + cFillChar
        end
       else
      if nLength > n then
        begin
          Result := expression;
          for i := n to nLength - n do
            Result := Result + cFillChar;
        end
       else
        Result := Copy(s, 1, nLength);
    end;

{Дополняет строку expression слева символом cFillChar до длины nLength}

function Padl(expression: string; nLength: Integer; cFillChar: Char = ' '):string;
var
  n, i: Integer;
  s: string;
begin
  s := Trim(expression);
  n := Length(s);
  if nLength > n then
  begin
    Result := '';
    for i := 1 to nLength - n do
      Result := Result + cFillChar;
    Result := Result + s;
  end
  else
    Result := Copy(s, 1, nLength);
end;

procedure ShowMessageEx(const Msg: string; const ReadOnly: boolean = True; const Width: integer = 400; const Height: integer = 300);
  var
    Frm1: TForm;
    Mm1: TMemo;
  begin
    Frm1 := TForm.Create(nil);
    Frm1.Width := 400;
    Frm1.Height := 300;
    Frm1.BorderStyle := bsDialog;
    Mm1 := TMemo.Create(Frm1);
    Mm1.Parent := Frm1;
    Mm1.Align := alClient;
    Mm1.ReadOnly := ReadOnly;
    Mm1.ScrollBars := ssAutoBoth;
    Frm1.Position := poDesktopCenter;
    Mm1.Lines.Text := Msg;
    Frm1.ShowModal;
    FreeAndNil(Frm1);
  end;

function GetUID: string;
begin
  DateTimeToString(Result,'yyyymmddhhnnsszzz',Now);
  Result := Copy(Result,1,16)
end;

procedure ApplyTemplate(var aTemplate: string; const aPattern, aValue: string);
begin
  aTemplate := StringReplace(aTemplate, aPattern,
    aValue, [rfReplaceAll, rfIgnoreCase]);
end;

function Stepen(const X, N: integer): integer;
var
  i:integer;
begin
  Result:=1;
  for i:=1 to N do
    Result:=Result*X;
end;

function GetMD5(aStr: string): string;
  var
    Hash: TDCP_md5;
    Digest: array[0..128] of byte;
    i: integer;
    s: string;
begin
  Hash:= TDCP_md5.Create(nil);
  Hash.Init;
  Hash.UpdateStr(aStr);
  Hash.Final(Digest);
  s:='';
  for i:= 0 to 15 do
    s:= s + IntToHex(Digest[i],-1);
  Result:=s;
  Hash.Burn;
  FreeAndNil(Hash);
end;

function GetPSM_Name(const ind: integer): string;
begin
  Result := 'PSM';
  if (ind > 0) and (ind < 10) then
    Result += '0';
  Result += IntToStr(ind);
end;

end.

