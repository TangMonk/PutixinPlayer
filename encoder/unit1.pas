unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  base64, LazFileUtils, DCPrc4, DCPsha1;

type

  { TForm1 }

  TForm1 = class(TForm)
        Label1: TLabel;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private

  public

  end;

  { EncThread }

  EncThread = class(TThread)

  private
    aFileNamePath: string;
    aForm: TForm1;
  public
    procedure Execute; override;
    constructor Create(aAForm: TForm1; aFileName: string);
    procedure ShowStatus;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function FileToBase64(const AFile: string; var Base64: string): boolean;
var
  MS: TMemoryStream;
  Str: string;
begin
  Result := False;
  if not FileExists(AFile) then
    Exit;
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(AFile);
    if MS.Size > 0 then
    begin
      SetLength(Str, MS.Size div SizeOf(char));
      MS.ReadBuffer(Str[1], MS.Size div SizeOf(char));
      Base64 := EncodeStringBase64(Str);
      Result := True;
    end;
  finally
    MS.Free;
  end;
end;

{ EncThread }

procedure EncThread.Execute;
var
  aFileName, aFileDir: String;
  Source, Dest: TFileStream;
  Cipher: TDCP_rc4;
begin
  aFileName := ExtractFileNameOnly(aFileNamePath);
  aFileDir := ExtractFileDir(aFileNamePath);

  Source := TFileStream.Create(aFileNamePath, fmOpenRead);
  Dest := TFileStream.Create(aFileDir + aFileName + '.putiaudio', fmCreate);

  Cipher := TDCP_rc4.Create(Self.aForm);
  Cipher.InitStr('lasjwqonc56465wd', TDCP_sha1);

  Cipher.EncryptStream(Source, Dest, Source.Size);
  Cipher.Burn;
  Cipher.Free;
  Source.Free;
  Dest.Free;

  Synchronize(@ShowStatus);
end;

constructor EncThread.Create(aAForm: TForm1; aFileName: string);
begin
  inherited Create(false);
  Self.aFileNamePath := aFileName;
  Self.aForm:=aAForm;
end;

procedure EncThread.ShowStatus;
begin
  Self.aForm.Label1.Caption := '将文件拖拽到此处';
  ShowMessage('OK!');
  Self.aForm.AllowDropFiles:=true;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  aFile, aFileName, aFileDir: RawByteString;
  Source, Dest: TFileStream;
  Cipher: TDCP_rc4;
begin
  Label1.Caption := '处理中...';
  for aFile in FileNames do
  begin
       EncThread.Create(Self, aFile);
       AllowDropFiles:=false;
  end;


end;



end.
