unit MultiLogLCLHelpers;

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
  Classes, SysUtils, Graphics, MultiLog;

type

  { TLCLLogger }

  TLoggerLCLHelper = class helper for TIntegratedLogger
  private
  public
    procedure SendBitmap(const AText: String; ABitmap: TBitmap); //inline;
    procedure SendBitmap(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; ABitmap: TBitmap);
    procedure SendColor(const AText: String; AColor: TColor); //inline;
    procedure SendColor(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AColor: TColor);
  end;

  function ColorToStr(Color: TColor): String;

implementation

uses
  IntfGraphics, GraphType, FPimage, FPWriteBMP;

function ColorToStr(Color: TColor): String;
begin
  case Color of

    clBlack   : Result:='clBlack';
    clMaroon  : Result:='clMaroon';
    clGreen   : Result:='clGreen';
    clOlive   : Result:='clOlive';
    clNavy    : Result:='clNavy';
    clPurple  : Result:='clPurple';
    clTeal    : Result:='clTeal';
    clGray {clDkGray}   : Result:='clGray/clDkGray';
    clSilver {clLtGray}  : Result:='clSilver/clLtGray';
    clRed     : Result:='clRed';
    clLime    : Result:='clLime';
    clYellow  : Result:='clYellow';
    clBlue    : Result:='clBlue';
    clFuchsia : Result:='clFuchsia';
    clAqua    : Result:='clAqua';
    clWhite   : Result:='clWhite';
    clCream   : Result:='clCream';
    clNone    : Result:='clNone';
    clDefault : Result:='clDefault';
    clMoneyGreen : Result:='clMoneyGreen';
    clSkyBlue    : Result:='clSkyBlue';
    clMedGray    : Result:='clMedGray';
    clScrollBar               : Result:='clScrollBar';
    clBackground              : Result:='clBackground';
    clActiveCaption           : Result:='clActiveCaption';
    clInactiveCaption         : Result:='clInactiveCaption';
    clMenu                    : Result:='clMenu';
    clWindow                  : Result:='clWindow';
    clWindowFrame             : Result:='clWindowFrame';
    clMenuText                : Result:='clMenuText';
    clWindowText              : Result:='clWindowText';
    clCaptionText             : Result:='clCaptionText';
    clActiveBorder            : Result:='clActiveBorder';
    clInactiveBorder          : Result:='clInactiveBorder';
    clAppWorkspace            : Result:='clAppWorkspace';
    clHighlight               : Result:='clHighlight';
    clHighlightText           : Result:='clHighlightText';
    clBtnFace                 : Result:='clBtnFace';
    clBtnShadow               : Result:='clBtnShadow';
    clGrayText                : Result:='clGrayText';
    clBtnText                 : Result:='clBtnText';
    clInactiveCaptionText     : Result:='clInactiveCaptionText';
    clBtnHighlight            : Result:='clBtnHighlight';
    cl3DDkShadow              : Result:='cl3DDkShadow';
    cl3DLight                 : Result:='cl3DLight';
    clInfoText                : Result:='clInfoText';
    clInfoBk                  : Result:='clInfoBk';
    clHotLight                : Result:='clHotLight';
    clGradientActiveCaption   : Result:='clGradientActiveCaption';
    clGradientInactiveCaption : Result:='clGradientInactiveCaption';
    clForm                    : Result:='clForm';
    {
    //todo find the conflicts
    clColorDesktop            : Result:='clColorDesktop';
    cl3DFace                  : Result:='cl3DFace';
    cl3DShadow                : Result:='cl3DShadow';
    cl3DHiLight               : Result:='cl3DHiLight';
    clBtnHiLight              : Result:='clBtnHiLight';
    }
  else
    Result := 'Unknow Color';
  end;//case
  Result := Result + ' ($' + IntToHex(Color, 6) + ')';
end;

procedure SaveBitmapToStream(Bitmap: TBitmap; Stream: TStream);
var
  oIntfImg: TLazIntfImage;
  oImgWriter: TFPCustomImageWriter;
  o_RawImage: TRawImage;
begin
  // adapted from LCL code
  oIntfImg := nil;
  oImgWriter := nil;
  try
    oIntfImg := TLazIntfImage.Create(0,0);
    oIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);

    oIntfImg.GetRawImage(o_RawImage);
    if o_RawImage.IsMasked(True) then
      oImgWriter := TLazWriterXPM.Create
    else
    begin
      oImgWriter := TFPWriterBMP.Create;
      TFPWriterBMP(oImgWriter).BitsPerPixel := oIntfImg.DataDescription.Depth;
    end;

    oIntfImg.SaveToStream(Stream, oImgWriter);
    Stream.Position := 0;
  finally
    oIntfImg.Free;
    oImgWriter.Free;
  end;
end;

{ TLCLLogger }

procedure TLoggerLCLHelper.SendBitmap(const AText: String; ABitmap: TBitmap);
begin
  SendBitmap(FilterDynamic_forWhatReasonsToLogActually,AText,ABitmap);
end;

procedure TLoggerLCLHelper.SendBitmap(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; ABitmap: TBitmap);
var
  oStream: TStream;
begin
  FilterDynamic_forWhatReasonsToLogActually:= FilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  if ABitmap <> nil then begin
    oStream := TMemoryStream.Create;
    //use custom function to avoid bug in TBitmap.SaveToStream
    SaveBitmapToStream(ABitmap, oStream);
  end
  else
    oStream := nil;
  SendStream(methBitmap, AText, oStream);
  oStream.Free;
end;

procedure TLoggerLCLHelper.SendColor(const AText: String; AColor: TColor);
begin
  SendColor(FilterDynamic_forWhatReasonsToLogActually, AText, AColor);
end;

procedure TLoggerLCLHelper.SendColor(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AColor: TColor);
begin
  FilterDynamic_forWhatReasonsToLogActually:= FilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue, AText + ' = ' + ColorToStr(AColor),nil);
end;

end.


