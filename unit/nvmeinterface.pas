unit NVMeInterface;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, StorageInterface;

type

  { SCSI interface subclass }
  TNVMeInterface = class(TStorageInterface)

  public

    procedure Connect; override;
    procedure Disconnect; override;
    procedure FormatStorage; override;
  end;

implementation

{ TSCSIInterface Implementation }

procedure TNVMeInterface.Connect;
begin

  { Specific connection logic for SCSI }
end;

procedure TNVMeInterface.Disconnect;
begin

  { Specific disconnection logic for SCSI }
end;

procedure TNVMeInterface.FormatStorage;
begin

  { Specific formatting logic for SCSI storage }
end;

end.

