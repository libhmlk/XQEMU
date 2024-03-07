unit VirtualRAM;

{$mode objfpc}{$H+}

interface

uses
  VirtualDevice; { Include the base class unit }

type

  { Enumerated type for RAM frequency channel }
  TFrequencyChannel = (
    fcDDR41600,
    fcDDR41866,
    fcDDR42133,
    fcDDR42400,
    fcDDR42666,
    fcDDR42933,
    fcDDR43200
    { Extend with other frequency (DDRX) channel as needed }
  );

type

  { Virtual RAM device }
  TVirtualRAM = class(TVirtualDevice)

  private

    FAllocatedSize: Byte;
    FFrequencyChannel: TFrequencyChannel;

  protected

    function MakeDeviceUUID: string; override;

  public

    constructor Create(RAMName: string; AllocatedSize: Byte; FrequencyChannel: TFrequencyChannel);
    destructor Destroy; override;

    { Properties }
    property AllocatedSize: Byte read FAllocatedSize;
    property FrequencyChannel: TFrequencyChannel read FFrequencyChannel;

  end;

implementation

{ TVirtualRAM }

function TVirtualRAM.MakeDeviceUUID: string;
begin

  Result := '';
end;

constructor TVirtualRAM.Create(RAMName: string; AllocatedSize: Byte; FrequencyChannel: TFrequencyChannel);
begin

  inherited Create(RAMName, (dtRAM));
  FAllocatedSize := AllocatedSize;
  FFrequencyChannel := FrequencyChannel;
end;

destructor TVirtualRAM.Destroy;
begin

 inherited Destroy;
end;

end.
