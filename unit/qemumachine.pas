unit QEMUMachine;

{$mode objfpc}{$H+}

interface

type

  { Enumerated type for QEMU supported architectures }
  TQEMUMachine = (
    qaUnknown,
    qaX86_64,
    qaI386,
    qaARM,
    qaAARCH64,
    qaSPARC,
    qaPPC,
    qaRISCV64
    { Extend with other architectures as needed }
  );

  { Set type for architectures }
  TQEMUMachines = set of TQEMUMachine;

implementation

end.

