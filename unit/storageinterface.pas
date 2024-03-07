unit StorageInterface;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, DeviceInterface;

type

  { Abstract class for storage device interfaces }
  TStorageInterface = class(TDeviceInterface)

  public

    procedure FormatStorage; virtual; abstract;

  end;

implementation

end.

