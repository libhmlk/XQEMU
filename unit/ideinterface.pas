unit IDEInterface;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, StorageInterface;

type

  { SCSI interface subclass }
  TIDEInterface = class(TStorageInterface)

  public

    procedure Connect; override;
    procedure Disconnect; override;
    procedure FormatStorage; override;
  end;

implementation

{ TSCSIInterface Implementation }

procedure TIDEInterface.Connect;
begin

  { Specific connection logic for SCSI }
end;

procedure TIDEInterface.Disconnect;
begin

  { Specific disconnection logic for SCSI }
end;

procedure TIDEInterface.FormatStorage;
begin

  { Specific formatting logic for SCSI storage }
end;

end.

