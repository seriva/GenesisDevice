unit uGDResource;

{$mode objfpc}

interface

type
   TGDResource = class
   private
     FName : String;
     FRefCount : Integer;
   public
     property Name : String read FName write FName;
     property RefCount : Integer read FRefCount write FRefCount;
   end;

implementation

end.

