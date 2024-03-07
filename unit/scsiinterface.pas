unit SCSIInterface;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, StorageInterface;

type

  { SCSI interface subclass }
  TSCSIInterface = class(TStorageInterface)

  public

    procedure Connect; override;
    procedure Disconnect; override;
    procedure FormatStorage; override;
  end;

implementation

{ TSCSIInterface Implementation }

procedure TSCSIInterface.Connect;
begin

  { Specific connection logic for SCSI }
end;

procedure TSCSIInterface.Disconnect;
begin

  { Specific disconnection logic for SCSI }
end;

procedure TSCSIInterface.FormatStorage;
begin

  { Specific formatting logic for SCSI storage }
end;

end.

