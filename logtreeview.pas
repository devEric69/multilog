unit LogTreeView;

{
Copyright (C) 2006 Luiz Américo Pereira Câmara
pascalive@bol.com.br

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Comctrls, Controls, MultiLog, LResources, Graphics;

type

  { TLogTreeView }

{$REGION 'Xls: Comments section'}
{ TLogTreeView is a way to display the TIntegratedLogger'messages, with images.}
{$ENDREGION}
  TLogTreeView = class (TCustomTreeView)
  private
    FoImgList: TImageList;
    FoChannel: TLogChannel;
    Fo_LastNode: TTreeNode;
    Fo_ParentNode: TTreeNode;
    FbShowTime: Boolean;
    FbShowPrefixMethod: Boolean;
    FbShowDynamicFilter_forWhatReasonsToLogActually: Boolean;
    FsTimeFormat: String;
    function GetChannel: TLogChannel;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMessage(AMsg: TrecLogMessage);
    procedure Clear;
    property Channel: TLogChannel read GetChannel;
  published
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DefaultItemHeight;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExpandSignType;
    property Font;
    property HideSelection;
    property HotTrack;
    //property Images;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCustomCreateItem;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectionChanged;
    property Options;
    //property OnStartDock;
    property OnStartDrag;
    //property Items;
    property TreeLineColor;
    property ExpandSignColor;
    property TimeFormat: String read FsTimeFormat write FsTimeFormat;
    property ShowTime: Boolean read FbShowTime write FbShowTime;
    property ShowPrefixMethod: Boolean read FbShowPrefixMethod write FbShowPrefixMethod;
    property ShowDynamicFilter_forWhatReasonsToLogActually: Boolean read FbShowDynamicFilter_forWhatReasonsToLogActually write FbShowDynamicFilter_forWhatReasonsToLogActually;
  end;

  { TLogTreeViewChannel }

  TLogTreeViewChannel = class (TLogChannel)
  private
    Fo_Control: TLogTreeView;
  public
    constructor Create(AControl: TLogTreeView);
    procedure Clear; override;
    procedure Deliver(const AMsg: TrecLogMessage);override;
  end;

implementation


{ TLogTreeViewChannel }

constructor TLogTreeViewChannel.Create(AControl: TLogTreeView);
begin
  Fo_Control:= AControl;
  Active:= True;
  FbShowPrefixMethod:= false;
  FbShowTime:= false;
  FbShow_DynamicFilter_forWhatReasonsToLogActually:= false;
end;

procedure TLogTreeViewChannel.Clear;
begin
  Fo_Control.Clear;
end;

procedure TLogTreeViewChannel.Deliver(const AMsg: TrecLogMessage);
begin
  Fo_Control.AddMessage(AMsg);
end;

{ TLogTreeView }

function TLogTreeView.GetChannel: TLogChannel;
begin
  if FoChannel = nil then
    FoChannel:= TLogTreeViewChannel.Create(Self);
  Result:= FoChannel;
end;

constructor TLogTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FsTimeFormat := 'hh:nn:ss:zzz';
  FoImgList:= TImageList.Create(nil);
  with FoImgList do begin
    AddLazarusResource('info', clDefault);
    AddLazarusResource('error', clDefault);
    AddLazarusResource('warning', clDefault);
    AddLazarusResource('value', clDefault);
    AddLazarusResource('entermethod', clDefault);
    AddLazarusResource('exitmethod', clDefault);
    AddLazarusResource('whatisthis', clDefault);  //conditional
    AddLazarusResource('check', clDefault);
    AddLazarusResource('strings', clDefault);
    AddLazarusResource('callstack', clDefault);
    AddLazarusResource('object', clDefault);
    AddLazarusResource('error', clDefault);
    AddLazarusResource('image', clDefault);
    AddLazarusResource('whatisthis', clDefault);   //heap
    AddLazarusResource('whatisthis', clDefault);   //memory
    AddLazarusResource('whatisthis', clDefault);   //custom data
  end;
  Images:= FoImgList;
end;

destructor TLogTreeView.Destroy;
begin
  FoImgList.Destroy; FoImgList:= nil;
  FreeAndNil(FoChannel);
  inherited Destroy;
end;

procedure TLogTreeView.AddMessage(AMsg: TrecLogMessage);

        procedure ParseStream(AStream:TStream);
        var
          i: Integer;
          oStrList: TStringList;
        begin
          //todo: Parse the String in Stream directly instead of using StringList ??
           oStrList:= TStringList.Create;
           AStream.Position:=0;
           oStrList.LoadFromStream(AStream);
           for i:= 0 to oStrList.Count - 1 do
             Items.AddChild(Fo_LastNode,oStrList[i]);
           Fo_LastNode.Text:= Fo_LastNode.Text+' ('+IntToStr(oStrList.Count)+' Items)';
           oStrList.Destroy;
        end;

var
  oTempStream: TStream;
  sWholeMsg: String;
begin
  sWholeMsg:= '';
  if FbShowTime then
    sWholeMsg:= FormatDateTime(FsTimeFormat, Time) + '   ';
  if FbShowPrefixMethod then
  	sWholeMsg:= sWholeMsg + (ctsLogPrefixesMethod[AMsg.iMethUsed] + ':   ');
  //write second qualifier explaining for which tracking purposes Msg are Logged
  if FbShowDynamicFilter_forWhatReasonsToLogActually then
		sWholeMsg:= sWholeMsg + TLogChannelUtils.SetofToString(AMsg.setFilterDynamic);

  sWholeMsg:= sWholeMsg + AMsg.sMsgText;
  with Items, AMsg do begin
    case AMsg.iMethUsed of
      methEnterMethod:
        begin
          Fo_LastNode:= AddChild(Fo_ParentNode,sWholeMsg);
          Fo_ParentNode:= Fo_LastNode;
        end;
      methExitMethod:
        begin
          if Fo_ParentNode <> nil then
            Fo_LastNode:= AddChild(Fo_ParentNode.Parent,sWholeMsg)
          else
            Fo_LastNode:= AddChild(nil,sWholeMsg);
          Fo_ParentNode:= Fo_LastNode.Parent;
        end;
       methTStrings, methCallStack, methHeapInfo, methException, methMemory:
         begin
           Fo_LastNode:= AddChild(Fo_ParentNode,sWholeMsg);
           if Assigned(pData) and (pData.Size>0) then
             ParseStream(pData)
           else
             Fo_LastNode.Text:= Fo_LastNode.Text+' (No Items)';
         end;
       methObject:
         begin
           Fo_LastNode:= AddChild(Fo_ParentNode,sWholeMsg);
           if Assigned(pData) and (pData.Size>0) then begin
            pData.Position:= 0;
            oTempStream:= TStringStream.Create('');
            ObjectBinaryToText(pData,oTempStream);
            ParseStream(oTempStream);
            oTempStream.Destroy;
           end;
         end;
       else
       begin
         Fo_LastNode:= AddChild(Fo_ParentNode,sWholeMsg);
       end;
    end;
  end;
  //todo: hook TCustomTreeView to auto expand
  if Fo_LastNode.Parent <> nil then
    Fo_LastNode.Parent.Expanded:= True;
  Fo_LastNode.GetFirstChild;
  //todo: optimize painting
  Fo_LastNode.ImageIndex:= AMsg.iMethUsed;
  Fo_LastNode.SelectedIndex:= AMsg.iMethUsed;
end;

procedure TLogTreeView.Clear;
begin
  Items.Clear;
  Fo_LastNode:=nil;
  Fo_ParentNode:=nil;
end;

initialization
  {$i logimages.lrs}
end.

procedure TLogTreeView.Clear;
begin
  Items.Clear;
  FLastNode:=nil;
  FParentNode:=nil;
end;

initialization
  {$i logimages.lrs}
end.

