unit newton;

interface

  {$IFDEF WINDOWS}
    uses
      Windows;
  {$ENDIF}

  {$IFDEF LINUX}
    uses
      X,
      XLib;
  {$ENDIF}

  {$IFDEF DARWIN}
    uses
      X,
      XLib,
      CocoaAll;
  {$ENDIF}

{$I newton.inc}

implementation
end.
