unit VirtualCPU;

{$mode objfpc}{$H+}

interface

uses
  VirtualDevice; { Include the base class unit }

type

  { Virtual CPU device }
  TVirtualCPU = class(TVirtualDevice)

  private

    FCoreNumber: Byte;
    FSocketPerCore: Byte;
    FThreadPerCore: Byte;
    FCacheSize: Word;

  protected

    function MakeDeviceUUID: string; override;

  public

    constructor Create(CPUName: string; CoreNumber: Byte; SocketPerCore: Byte; ThreadPerCore: Byte; CacheSize: Word);
    destructor Destroy; override;

    { Properties }
    property CoreNumber: Byte read FCoreNumber;
    property SocketPerCore: Byte read FSocketPerCore;
    property ThreadPerCore: Byte read FThreadPerCore;
    property CacheSize: Word read FCacheSize;

  end;

implementation

{ TVirtualCPU }

function TVirtualCPU.MakeDeviceUUID: string;
begin

  Result := '';
end;

constructor TVirtualCPU.Create(CPUName: string; CoreNumber: Byte; SocketPerCore: Byte; ThreadPerCore: Byte; CacheSize: Word);
begin

  inherited Create(CPUName, (dtCPU));
  FCoreNumber := CoreNumber;
  FSocketPerCore := SocketPerCore;
  FThreadPerCore := ThreadPerCore;
  FCacheSize := CacheSize;
end;

destructor TVirtualCPU.Destroy;
begin

 inherited Destroy;
end;

end.
