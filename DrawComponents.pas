unit DrawComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, StdCtrls, ExtCtrls, Tools;

type

  { TToolButton }

  TToolButton = class(TSpeedButton)
  public
    Tool: TToolClass;
  end;

implementation

end.

