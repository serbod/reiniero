{
    This file is part of DBGP Plugin for Notepad++
    Copyright (C) 2007  Damjan Zobo Cvetko

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit NppDockingForms;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Forms, Dialogs, Controls, windows,
  NppPlugin, NPPTypes, NPPForms;

type
  TNppDockingForm = class(TNppForm) {TCustomForm}
  private
    { Private declarations }
    FDlgId  : Integer;
    FOnDock : TNotifyEvent;
    FOnFloat: TNotifyEvent;
    procedure RemoveControlParent(control: TControl);
  protected
    { Protected declarations }
    ToolbarData: TToolbarData;
    NppDefaultDockingMask: Cardinal;
    // @todo: change caption and stuff....
    procedure OnWM_NOTIFY(var msg: TWMNotify); message WM_NOTIFY;
    property OnDock: TNotifyEvent read FOnDock write FOnDock;
    property OnFloat: TNotifyEvent read FOnFloat write FOnFloat;
  public
    { Public declarations }
    CmdId: Integer;
    constructor Create; overload;
    constructor Create(AOwner: TNppForm); overload;
    constructor Create(DlgId: Integer); overload; virtual;
    constructor Create(AOwner: TNppForm; DlgId: Integer); overload;  virtual;
    procedure Show;
    procedure Hide;
    procedure RegisterDockingForm(MaskStyle: Cardinal = DWS_DF_CONT_LEFT);
    procedure UpdateDisplayInfo; overload;
    procedure UpdateDisplayInfo(Info: String); overload;
    property DlgID: Integer read FDlgid;
  published
    { Published declarations }
  end;

var
  NppDockingForm: TNppDockingForm;

implementation

{$R *.lfm}

{ TNppDockingForm }

// I don't know how else to hide a constructor.
constructor TNppDockingForm.Create;
begin
  MessageBox(0, 'Do not use this constructor', 'Plugin Framework error', MB_OK);
  Halt(1);
end;
constructor TNppDockingForm.Create(AOwner: TNppForm);
begin
  AOwner := AOwner; // Prevent compiler hint
  MessageBox(0, 'Do not use this constructor', 'Plugin Framework error', MB_OK);
  Halt(1);
end;

constructor TNppDockingForm.Create(DlgId: Integer);
begin
  inherited Create;
  FDlgId := DlgId;
  CmdId := GetNPPPluginInstance.CmdIdFromDlgId(DlgId);
  RegisterDockingForm(NppDefaultDockingMask);
  RemoveControlParent(self);
end;

constructor TNppDockingForm.Create(AOwner: TNppForm; DlgId: Integer);
begin
  inherited Create(AOwner);
  FDlgId := DlgId;
  RegisterDockingForm(NppDefaultDockingMask);
  RemoveControlParent(self);
end;

procedure TNppDockingForm.OnWM_NOTIFY(var msg: TWMNotify);
begin
  // Verify if message is for plugin
  if not GetNPPPluginInstance.isMessageForPlugin(msg.NMHdr.hwndFrom) then
  begin
    inherited;
    exit;
  end;

  // Handle message
  msg.Result := 0;

  if (msg.NMHdr.code = DMN_CLOSE) then
  begin
    DoHide;
  end;

  if ((msg.NMHdr.code and $ffff) = DMN_FLOAT) then
  begin
    // msg.NMHdr.code shr 16 - container
    if Assigned(FOnFloat) then FOnFloat(Self);
  end;

  if ((msg.NMHdr.code and $ffff) = DMN_DOCK) then
  begin
    // msg.NMHdr.code shr 16 - container
    if Assigned(FOnDock) then FOnDock(Self);
  end;

 inherited;
end;

procedure TNppDockingForm.RegisterDockingForm(MaskStyle: Cardinal = DWS_DF_CONT_LEFT);
begin
  HandleNeeded;
  //self.Visible := true;

  FillChar(ToolbarData,sizeof(TToolbarData),0);

  if not Icon.Empty then
  begin
    ToolbarData.IconTab := Icon.Handle;
    ToolbarData.Mask    := ToolbarData.Mask or DWS_ICONTAB;
  end;

  ToolbarData.ClientHandle := Handle;

  ToolbarData.DlgId := FDlgId;
  ToolbarData.Mask  := MaskStyle;

  ToolbarData.Mask  := ToolbarData.Mask or DWS_ADDINFO;

  GetMem(ToolbarData.Title, 500*sizeof(nppPChar));
  GetMem(ToolbarData.ModuleName, 1000*sizeof(nppPChar));
  GetMem(ToolbarData.AdditionalInfo, 1000*sizeof(nppPChar));

  {$IFDEF NPPUNICODE}
    StringToWideChar(Caption+#0, ToolbarData.Title, 500);
    GetModuleFileNameW(HInstance, ToolbarData.ModuleName, 1000);
    StringToWideChar(ExtractFileName(ToolbarData.ModuleName), ToolbarData.ModuleName, 1000);
    StringToWideChar('', ToolbarData.AdditionalInfo, 1);
    GetNPPPluginInstance.SendToNpp(NPPM_DMMREGASDCKDLG, 0, Integer(@ToolbarData));
  {$ELSE}
    StrCopy(ToolbarData.Title, PChar(Caption));
    GetModuleFileNameA(HInstance, ToolbarData.ModuleName, 1000);
    StrLCopy(ToolbarData.ModuleName, PChar(ExtractFileName(ToolbarData.ModuleName)), 1000);
    StrCopy(ToolbarData.AdditionalInfo, PChar(''));
    GetNPPPluginInstance.NppSendMessage(NPPM_DMMREGASDCKDLG, 0, Integer(@ToolbarData));
  {$ENDIF}

  Visible := true;
end;

procedure TNppDockingForm.Show;
begin
  GetNPPPluginInstance.SendToNpp(NPPM_DMMSHOW, 0, LPARAM(Handle));
  inherited;
  DoShow;
end;

procedure TNppDockingForm.Hide;
begin
  GetNPPPluginInstance.SendToNpp(NPPM_DMMHIDE, 0, LPARAM(Handle));
  DoHide;
end;

// This hack prevents the Win Dialog default procedure from an endless loop while
// looking for the prevoius component, while in a floating state.
// I still don't know why the pointer climbs up to the docking dialog that holds this one
// but this works for now.
procedure TNppDockingForm.RemoveControlParent(control: TControl);
var
  wincontrol: TWinControl;
  i, r: integer;
begin
  if (control is TWinControl) then
  begin
    wincontrol := control as TWinControl;
    wincontrol.HandleNeeded;
    r := Windows.GetWindowLong(wincontrol.Handle, GWL_EXSTYLE);
    if (r and WS_EX_CONTROLPARENT) = WS_EX_CONTROLPARENT then
    begin
      Windows.SetWindowLong(wincontrol.Handle, GWL_EXSTYLE, r and not WS_EX_CONTROLPARENT);
    end;
  end;

  for i := control.ComponentCount-1 downto 0 do
    if (control.Components[i] is TControl) then
      RemoveControlParent(control.Components[i] as TControl);
end;

procedure TNppDockingForm.UpdateDisplayInfo;
begin
  UpdateDisplayInfo('');
end;

procedure TNppDockingForm.UpdateDisplayInfo(Info: String);
begin
  {$IFDEF NPPUNICODE}
    StringToWideChar(Info+#0, ToolbarData.AdditionalInfo, 1000);
    GetNPPPluginInstance.SendWToNpp(NPPM_DMMUPDATEDISPINFO, 0, Handle);
  {$ELSE}
    StrLCopy(ToolbarData.AdditionalInfo, PChar(Info), 1000);
    SendMessageA(self.Npp.NppData.NppHandle, NPPM_DMMUPDATEDISPINFO, 0, Handle);
  {$ENDIF}
end;

end.
