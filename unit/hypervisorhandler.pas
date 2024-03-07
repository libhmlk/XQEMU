unit HypervisorHandler;

{$mode objfpc}{$H+}

interface

type

  { Enumerated type for hypervisors }
  THypervisor = (htUnknown, htQEMU, htVMware, htHyperV, htXen, htKVM);

type

  THypervisorHandler = class abstract

  protected

    { Fields common to all hypervisors }
    FVersion: string;
    FHypervisor: THypervisor;

    { Add other common fields here }

    { Virtual methods to be overridden by subclasses }
    function FindVersion: string; virtual; abstract;

    { Add other necessary methods as abstract }

  public

    constructor Create(Hypervisor: THypervisor);
    destructor Destroy; override;

    { Add other necessary methods as abstract }

    { Properties }
    property Version: string read FVersion;
    property Hypervisor: THypervisor read FHypervisor;

  end;

implementation

constructor THypervisorHandler.Create(Hypervisor: THypervisor);
begin

  inherited Create;
  FVersion := FindVersion;
  FHypervisor := Hypervisor;
end;

destructor THypervisorHandler.Destroy;
begin

  inherited Destroy;
end;

end.


