unit DeviceType;

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

  { Set type for architectures }
  TDeviceTypes = set of TDeviceType;

implementation

end.


