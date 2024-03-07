program XQEMUKernel;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, QEMUHandler, VirtualMachine;

type

  { TXQEMUKernel }

  TXQEMUKernel = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TXQEMUKernel }

procedure TXQEMUKernel.DoRun;
var
  I: Byte;
  ErrorMsg: String;
  MainQEMUHandler: TQEMUHandler;
  QEMUMachines: TStringList;
begin
  { Quick check parameters }
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  { Parse parameters }
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { Add main code here }
  MainQEMUHandler := TQEMUHandler.Create();
  QEMUMachines := MainQEMUHandler.ListQEMUMachines;
  WriteLn('- QEMU Version: ', MainQEMUHandler.Version);
  Write('- QEMU Machines: ');
  for I := 1 to QEMUMachines.Count - 1 do
      Write(QEMUMachines[I], ', ');
  WriteLn;
  WriteLn('- QMP Version: ', MainQEMUHandler.QMPVersion);

  MainQEMUHandler.free;
  { stop program loop }
  Terminate;
end;

constructor TXQEMUKernel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TXQEMUKernel.Destroy;
begin
  inherited Destroy;
end;

procedure TXQEMUKernel.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TXQEMUKernel;
begin
  Application:=TXQEMUKernel.Create(nil);
  Application.Title:='XQEMU Kernel';

  Application.Run;

  Application.Free;
end.

