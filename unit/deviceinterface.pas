unit DeviceInterface;

{$mode objfpc}{$H+}

interface

type

  { Enumerated type for interface type }
  TInterfaceType = (
    itStorage,
    itNIC,
    itUSB
  );

type

  { Abstract base class for hardware device interfaces. }
  TDeviceInterface = class abstract

  protected

    FInterfaceName: string;
    FInterfaceType: TInterfaceType;
    FTransferRate: Word;

  public

    constructor Create(InterfaceName: string; InterfaceType: TInterfaceType; TransferRate: Word); virtual;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    property InterfaceName: string read FInterfaceName;
    property InterfaceType: TInterfaceType read FInterfaceType;
    property TransferRate: Word read FTransferRate;
  end;

implementation

constructor TDeviceInterface.Create(InterfaceName: string; InterfaceType: TInterfaceType; TransferRate: Word);
begin

  inherited Create;
  FInterfaceName := InterfaceName;
  FInterfaceType := InterfaceType;
  FTransferRate := TransferRate;
end;

end.
