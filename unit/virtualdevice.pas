unit VirtualDevice;

{$mode objfpc}{$H+}

interface

type

  { Enumerated type for devive types }
  TDeviceType = (
    dtUnknown,
    dtCPU,
    dtRAM,
    dtStorage,
    dtNIC,
    dtSerialPort,
    dtParallelPort
    { Extend with other device types as needed }
  );

type

  { Abstract base class for virtual devices. }
  TVirtualDevice = class abstract

  protected

    FDeviceName: string;
    FDeviceType: TDeviceType; { Device type like CPU, RAM, BIOS, HDD }
    FDeviceUUID: string;

    { Virtual methods to be overridden by subclasses }
    function MakeDeviceUUID: string; virtual; abstract;

  public

    constructor Create(DeviceName: string; DeviceType: TDeviceType);
    destructor Destroy; override;

    property DeviceName: string read FDeviceName;
    property DeviceType: TDeviceType read FDeviceType;
    property DeviceUUID: string read FDeviceUUID;
  end;

implementation

constructor TVirtualDevice.Create(DeviceName: string; DeviceType: TDeviceType);
begin

  inherited Create;
  FDeviceName := DeviceName;
  FDeviceType := DeviceType;
  FDeviceUUID := MakeDeviceUUID;
end;

destructor TVirtualDevice.Destroy;
begin

  inherited Destroy;
end;

end.
