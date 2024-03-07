unit VirtualMachine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, VirtualDevice;

type

  { Class to manage a virtual machine and its devices }
  TVirtualMachine = class

  private

    FName: string;
    FDevices: TList; { List to manage TVirtualDevice instances }

  public

    constructor Create(MachineName: string);
    destructor Destroy; override;

    procedure AddDevice(Device: TVirtualDevice);
    procedure RemoveDevice(Device: TVirtualDevice);

    property Name: string read FName;
    { Property to access devices might also be useful }
  end;

implementation

constructor TVirtualMachine.Create(MachineName: string);
begin

  inherited Create;
  FName := MachineName;
  FDevices := TList.Create;
end;

destructor TVirtualMachine.Destroy;
var
  i: Integer;
begin

  for i := 0 to FDevices.Count - 1 do
    TVirtualDevice(FDevices[i]).Free;
  FDevices.Free;
  inherited Destroy;
end;

procedure TVirtualMachine.AddDevice(Device: TVirtualDevice);
begin

  FDevices.Add(Device);
end;

procedure TVirtualMachine.RemoveDevice(Device: TVirtualDevice);
begin

  FDevices.Remove(Device);
  Device.Free;
end;

end.
