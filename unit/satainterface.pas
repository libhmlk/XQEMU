unit SATAInterface;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, StorageInterface;

type

  { SCSI interface subclass }
  TSATAInterface = class(TStorageInterface)

  public

    procedure Connect; override;
    procedure Disconnect; override;
    procedure FormatStorage; override;
  end;

implementation

{ TSCSIInterface Implementation }

procedure TSATAInterface.Connect;
begin

  { Specific connection logic for SCSI }
end;

procedure TSATAInterface.Disconnect;
begin

  { Specific disconnection logic for SCSI }
end;

procedure TSATAInterface.FormatStorage;
begin

  { Specific formatting logic for SCSI storage }
end;

end.

