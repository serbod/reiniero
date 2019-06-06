Unit WIA_1_0_TLB;

//  Imported WIA on 22/07/2014 13:22:43 from C:\Windows\System32\wiaaut.dll

{$mode delphi}{$H+}

interface

//  Warning: renamed coclass 'Property' to 'Property_'
//  Warning: renamed property 'String' in IVector to 'String_'
//  Warning: 'POleVariant' not automatable in IVector.Item
//  Warning: 'POleVariant' not automatable in IVector.BinaryData
//  Warning: renamed property 'Type' in IProperty to 'Type_'
//  Warning: 'POleVariant' not automatable in IProperty.Value
//  Warning: renamed property 'Type' in IDeviceEvent to 'Type_'
//  Warning: renamed property 'Type' in IDevice to 'Type_'
//  Warning: renamed property 'Type' in IDeviceInfo to 'Type_'
Uses
  Windows,ActiveX,Classes,Variants,EventSink;
Const
  WIAMajorVersion = 1;
  WIAMinorVersion = 0;
  WIALCID = 0;
  LIBID_WIA : TGUID = '{94A0E92D-43C0-494E-AC29-FD45948A5221}';

  IID_IRational : TGUID = '{3BF1B24A-01A5-4AA3-91F9-25A60B50E49B}';
  CLASS_Rational : TGUID = '{0C5672F9-3EDC-4B24-95B5-A6C54C0B79AD}';
  IID_IImageFile : TGUID = '{F4243B65-3F63-4D99-93CD-86B6D62C5EB2}';
  IID_IVector : TGUID = '{696F2367-6619-49BD-BA96-904DC2609990}';
  IID_IProperties : TGUID = '{40571E58-A308-470A-80AA-FA10F88793A0}';
  IID_IProperty : TGUID = '{706038DC-9F4B-4E45-88E2-5EB7D665B815}';
  CLASS_Vector : TGUID = '{4DD1D1C3-B36A-4EB4-AAEF-815891A58A30}';
  CLASS_Property : TGUID = '{2014DE3F-3723-4178-8643-3317A32D4A2B}';
  CLASS_Properties : TGUID = '{96F887FC-08B1-4F97-A69C-75280C6A9CF8}';
  CLASS_ImageFile : TGUID = '{A2E6DDA0-06EF-4DF3-B7BD-5AA224BB06E8}';
  IID_IFilterInfo : TGUID = '{EFD1219F-8229-4B30-809D-8F6D83341569}';
  CLASS_FilterInfo : TGUID = '{318D6B52-9B1C-4E3B-8D90-1F0E857FA9B0}';
  IID_IFilterInfos : TGUID = '{AF49723A-499C-411C-B19A-1B8244D67E44}';
  CLASS_FilterInfos : TGUID = '{56FA88D3-F3DA-4DE3-94E8-811040C3CCD4}';
  IID_IFilter : TGUID = '{851E9802-B338-4AB3-BB6B-6AA57CC699D0}';
  CLASS_Filter : TGUID = '{52AD8A74-F064-4F4C-8544-FF494D349F7B}';
  IID_IFilters : TGUID = '{C82FFED4-0A8D-4F85-B90A-AC8E720D39C1}';
  CLASS_Filters : TGUID = '{31CDD60C-C04C-424D-95FC-36A52646D71C}';
  IID_IImageProcess : TGUID = '{41506929-7855-4392-9E6F-98D88513E55D}';
  CLASS_ImageProcess : TGUID = '{BD0D38E4-74C8-4904-9B5A-269F8E9994E9}';
  IID_IFormats : TGUID = '{882A274F-DF2F-4F6D-9F5A-AF4FD484530D}';
  CLASS_Formats : TGUID = '{6F62E261-0FE6-476B-A244-50CF7440DDEB}';
  IID_IDeviceCommand : TGUID = '{7CF694C0-F589-451C-B56E-398B5855B05E}';
  CLASS_DeviceCommand : TGUID = '{72226184-AFBB-4059-BF55-0F6C076E669D}';
  IID_IDeviceCommands : TGUID = '{C53AE9D5-6D91-4815-AF93-5F1E1B3B08BD}';
  CLASS_DeviceCommands : TGUID = '{25B047DB-4AAD-4FC2-A0BE-31DDA687FF32}';
  IID_IItems : TGUID = '{46102071-60B4-4E58-8620-397D17B0BB5B}';
  IID_IItem : TGUID = '{68F2BF12-A755-4E2B-9BCD-37A22587D078}';
  CLASS_Item : TGUID = '{36F479F3-C258-426E-B5FA-2793DCFDA881}';
  CLASS_Items : TGUID = '{B243B765-CA9C-4F30-A457-C8B2B57A585E}';
  IID_IDeviceEvent : TGUID = '{80D0880A-BB10-4722-82D1-07DC8DA157E2}';
  CLASS_DeviceEvent : TGUID = '{617CF892-783C-43D3-B04B-F0F1DE3B326D}';
  IID_IDeviceEvents : TGUID = '{03985C95-581B-44D1-9403-8488B347538B}';
  CLASS_DeviceEvents : TGUID = '{3563A59A-BBCD-4C86-94A0-92136C80A8B4}';
  IID_IDevice : TGUID = '{3714EAC4-F413-426B-B1E8-DEF2BE99EA55}';
  IID_IDeviceInfo : TGUID = '{2A99020A-E325-4454-95E0-136726ED4818}';
  CLASS_DeviceInfo : TGUID = '{F09CFB7A-E561-4625-9BB5-208BCA0DE09F}';
  IID_IDeviceInfos : TGUID = '{FE076B64-8406-4E92-9CAC-9093F378E05F}';
  CLASS_DeviceInfos : TGUID = '{2DFEE16B-E4AC-4A19-B660-AE71A745D34F}';
  CLASS_Device : TGUID = '{DBAA8843-B1C4-4EDC-B7E0-D6F61162BE58}';
  IID_ICommonDialog : TGUID = '{B4760F13-D9F3-4DF8-94B5-D225F86EE9A1}';
  CLASS_CommonDialog : TGUID = '{850D1D11-70F3-4BE5-9A11-77AA6B2BB201}';
  IID_IDeviceManager : TGUID = '{73856D9A-2720-487A-A584-21D5774E9D0F}';
  IID__IDeviceManagerEvents : TGUID = '{2E9A5206-2360-49DF-9D9B-1762B4BEAE77}';
  CLASS_DeviceManager : TGUID = '{E1C5D730-7E97-4D8A-9E42-BBAE87C2059F}';

//Enums

Type
  WiaSubType =LongWord;
Const
  UnspecifiedSubType = $0000000000000000;
  RangeSubType = $0000000000000001;
  ListSubType = $0000000000000002;
  FlagSubType = $0000000000000003;
Type
  WiaDeviceType =LongWord;
Const
  UnspecifiedDeviceType = $0000000000000000;
  ScannerDeviceType = $0000000000000001;
  CameraDeviceType = $0000000000000002;
  VideoDeviceType = $0000000000000003;
Type
  WiaItemFlag =LongWord;
Const
  FreeItemFlag = $0000000000000000;
  ImageItemFlag = $0000000000000001;
  FileItemFlag = $0000000000000002;
  FolderItemFlag = $0000000000000004;
  RootItemFlag = $0000000000000008;
  AnalyzeItemFlag = $0000000000000010;
  AudioItemFlag = $0000000000000020;
  DeviceItemFlag = $0000000000000040;
  DeletedItemFlag = $0000000000000080;
  DisconnectedItemFlag = $0000000000000100;
  HPanoramaItemFlag = $0000000000000200;
  VPanoramaItemFlag = $0000000000000400;
  BurstItemFlag = $0000000000000800;
  StorageItemFlag = $0000000000001000;
  TransferItemFlag = $0000000000002000;
  GeneratedItemFlag = $0000000000004000;
  HasAttachmentsItemFlag = $0000000000008000;
  VideoItemFlag = $0000000000010000;
  RemovedItemFlag = $0000000080000000;
Type
  WiaPropertyType =LongWord;
Const
  UnsupportedPropertyType = $0000000000000000;
  BooleanPropertyType = $0000000000000001;
  BytePropertyType = $0000000000000002;
  IntegerPropertyType = $0000000000000003;
  UnsignedIntegerPropertyType = $0000000000000004;
  LongPropertyType = $0000000000000005;
  UnsignedLongPropertyType = $0000000000000006;
  ErrorCodePropertyType = $0000000000000007;
  LargeIntegerPropertyType = $0000000000000008;
  UnsignedLargeIntegerPropertyType = $0000000000000009;
  SinglePropertyType = $000000000000000A;
  DoublePropertyType = $000000000000000B;
  CurrencyPropertyType = $000000000000000C;
  DatePropertyType = $000000000000000D;
  FileTimePropertyType = $000000000000000E;
  ClassIDPropertyType = $000000000000000F;
  StringPropertyType = $0000000000000010;
  ObjectPropertyType = $0000000000000011;
  HandlePropertyType = $0000000000000012;
  VariantPropertyType = $0000000000000013;
  VectorOfBooleansPropertyType = $0000000000000065;
  VectorOfBytesPropertyType = $0000000000000066;
  VectorOfIntegersPropertyType = $0000000000000067;
  VectorOfUnsignedIntegersPropertyType = $0000000000000068;
  VectorOfLongsPropertyType = $0000000000000069;
  VectorOfUnsignedLongsPropertyType = $000000000000006A;
  VectorOfErrorCodesPropertyType = $000000000000006B;
  VectorOfLargeIntegersPropertyType = $000000000000006C;
  VectorOfUnsignedLargeIntegersPropertyType = $000000000000006D;
  VectorOfSinglesPropertyType = $000000000000006E;
  VectorOfDoublesPropertyType = $000000000000006F;
  VectorOfCurrenciesPropertyType = $0000000000000070;
  VectorOfDatesPropertyType = $0000000000000071;
  VectorOfFileTimesPropertyType = $0000000000000072;
  VectorOfClassIDsPropertyType = $0000000000000073;
  VectorOfStringsPropertyType = $0000000000000074;
  VectorOfVariantsPropertyType = $0000000000000077;
Type
  WiaImagePropertyType =LongWord;
Const
  UndefinedImagePropertyType = $00000000000003E8;
  ByteImagePropertyType = $00000000000003E9;
  StringImagePropertyType = $00000000000003EA;
  UnsignedIntegerImagePropertyType = $00000000000003EB;
  LongImagePropertyType = $00000000000003EC;
  UnsignedLongImagePropertyType = $00000000000003ED;
  RationalImagePropertyType = $00000000000003EE;
  UnsignedRationalImagePropertyType = $00000000000003EF;
  VectorOfUndefinedImagePropertyType = $000000000000044C;
  VectorOfBytesImagePropertyType = $000000000000044D;
  VectorOfUnsignedIntegersImagePropertyType = $000000000000044E;
  VectorOfLongsImagePropertyType = $000000000000044F;
  VectorOfUnsignedLongsImagePropertyType = $0000000000000450;
  VectorOfRationalsImagePropertyType = $0000000000000451;
  VectorOfUnsignedRationalsImagePropertyType = $0000000000000452;
Type
  WiaEventFlag =LongWord;
Const
  NotificationEvent = $0000000000000001;
  ActionEvent = $0000000000000002;
Type
  WiaImageIntent =LongWord;
Const
  UnspecifiedIntent = $0000000000000000;
  ColorIntent = $0000000000000001;
  GrayscaleIntent = $0000000000000002;
  TextIntent = $0000000000000004;
Type
  WiaImageBias =LongWord;
Const
  MinimizeSize = $0000000000010000;
  MaximizeQuality = $0000000000020000;
//Forward declarations

Type
 IRational = interface;
 IRationalDisp = dispinterface;
 IImageFile = interface;
 IImageFileDisp = dispinterface;
 IVector = interface;
 IVectorDisp = dispinterface;
 IProperties = interface;
 IPropertiesDisp = dispinterface;
 IProperty = interface;
 IPropertyDisp = dispinterface;
 IFilterInfo = interface;
 IFilterInfoDisp = dispinterface;
 IFilterInfos = interface;
 IFilterInfosDisp = dispinterface;
 IFilter = interface;
 IFilterDisp = dispinterface;
 IFilters = interface;
 IFiltersDisp = dispinterface;
 IImageProcess = interface;
 IImageProcessDisp = dispinterface;
 IFormats = interface;
 IFormatsDisp = dispinterface;
 IDeviceCommand = interface;
 IDeviceCommandDisp = dispinterface;
 IDeviceCommands = interface;
 IDeviceCommandsDisp = dispinterface;
 IItems = interface;
 IItemsDisp = dispinterface;
 IItem = interface;
 IItemDisp = dispinterface;
 IDeviceEvent = interface;
 IDeviceEventDisp = dispinterface;
 IDeviceEvents = interface;
 IDeviceEventsDisp = dispinterface;
 IDevice = interface;
 IDeviceDisp = dispinterface;
 IDeviceInfo = interface;
 IDeviceInfoDisp = dispinterface;
 IDeviceInfos = interface;
 IDeviceInfosDisp = dispinterface;
 ICommonDialog = interface;
 ICommonDialogDisp = dispinterface;
 IDeviceManager = interface;
 IDeviceManagerDisp = dispinterface;
 _IDeviceManagerEvents = dispinterface;

//Map CoClass to its default interface

 Rational = IRational;
 Vector = IVector;
 Property_ = IProperty;
 Properties = IProperties;
 ImageFile = IImageFile;
 FilterInfo = IFilterInfo;
 FilterInfos = IFilterInfos;
 Filter = IFilter;
 Filters = IFilters;
 ImageProcess = IImageProcess;
 Formats = IFormats;
 DeviceCommand = IDeviceCommand;
 DeviceCommands = IDeviceCommands;
 Item = IItem;
 Items = IItems;
 DeviceEvent = IDeviceEvent;
 DeviceEvents = IDeviceEvents;
 DeviceInfo = IDeviceInfo;
 DeviceInfos = IDeviceInfos;
 Device = IDevice;
 CommonDialog = ICommonDialog;
 DeviceManager = IDeviceManager;

//records, unions, aliases


//interface declarations

// IRational : 

 IRational = interface(IDispatch)
   ['{3BF1B24A-01A5-4AA3-91F9-25A60B50E49B}']
   function Get_Value : Double; safecall;
   function Get_Numerator : Integer; safecall;
   procedure Set_Numerator(const plResult:Integer); safecall;
   function Get_Denominator : Integer; safecall;
   procedure Set_Denominator(const plResult:Integer); safecall;
    // Value : Returns the Rational Value as a Double 
   property Value:Double read Get_Value;
    // Numerator : Returns/Sets the Rational Value Numerator 
   property Numerator:Integer read Get_Numerator write Set_Numerator;
    // Denominator : Returns/Sets the Rational Value Denominator 
   property Denominator:Integer read Get_Denominator write Set_Denominator;
  end;


// IRational : 

 IRationalDisp = dispinterface
   ['{3BF1B24A-01A5-4AA3-91F9-25A60B50E49B}']
    // Value : Returns the Rational Value as a Double 
   property Value:Double  readonly dispid 0;
    // Numerator : Returns/Sets the Rational Value Numerator 
   property Numerator:Integer dispid 1;
    // Denominator : Returns/Sets the Rational Value Denominator 
   property Denominator:Integer dispid 2;
  end;


// IImageFile : 

 IImageFile = interface(IDispatch)
   ['{F4243B65-3F63-4D99-93CD-86B6D62C5EB2}']
   function Get_FormatID : WideString; safecall;
   function Get_FileExtension : WideString; safecall;
   function Get_FileData : IVector; safecall;
   function Get_ARGBData : IVector; safecall;
   function Get_Height : Integer; safecall;
   function Get_Width : Integer; safecall;
   function Get_HorizontalResolution : Double; safecall;
   function Get_VerticalResolution : Double; safecall;
   function Get_PixelDepth : Integer; safecall;
   function Get_IsIndexedPixelFormat : WordBool; safecall;
   function Get_IsAlphaPixelFormat : WordBool; safecall;
   function Get_IsExtendedPixelFormat : WordBool; safecall;
   function Get_IsAnimated : WordBool; safecall;
   function Get_FrameCount : Integer; safecall;
   function Get_ActiveFrame : Integer; safecall;
   procedure Set_ActiveFrame(const plResult:Integer); safecall;
   function Get_Properties : IProperties; safecall;
    // LoadFile : Loads the ImageFile object with the specified File 
   procedure LoadFile(Filename:WideString);safecall;
    // SaveFile : Save the ImageFile object to the specified File 
   procedure SaveFile(Filename:WideString);safecall;
    // FormatID : Returns the FormatID for this file type 
   property FormatID:WideString read Get_FormatID;
    // FileExtension : Returns the file extension for this image file type 
   property FileExtension:WideString read Get_FileExtension;
    // FileData : Returns the raw image file as a Vector of Bytes 
   property FileData:IVector read Get_FileData;
    // ARGBData : Returns the raw image bits as a Vector of Long values 
   property ARGBData:IVector read Get_ARGBData;
    // Height : Returns the Height of the image in pixels 
   property Height:Integer read Get_Height;
    // Width : Returns the Width of the image in pixels 
   property Width:Integer read Get_Width;
    // HorizontalResolution : Returns the Horizontal pixels per inch of the image 
   property HorizontalResolution:Double read Get_HorizontalResolution;
    // VerticalResolution : Returns the Vertical pixels per inch of the image 
   property VerticalResolution:Double read Get_VerticalResolution;
    // PixelDepth : Returns the depth of the pixels of the image in bits per pixel 
   property PixelDepth:Integer read Get_PixelDepth;
    // IsIndexedPixelFormat : Indicates if the pixel data is an index into a palette or the actual color data 
   property IsIndexedPixelFormat:WordBool read Get_IsIndexedPixelFormat;
    // IsAlphaPixelFormat : Indicates if the pixel format has an alpha component 
   property IsAlphaPixelFormat:WordBool read Get_IsAlphaPixelFormat;
    // IsExtendedPixelFormat : Indicates if the pixel format is extended (16 bits/channel) 
   property IsExtendedPixelFormat:WordBool read Get_IsExtendedPixelFormat;
    // IsAnimated : Indicates whether the image is animated 
   property IsAnimated:WordBool read Get_IsAnimated;
    // FrameCount : Returns the number of frames in the image 
   property FrameCount:Integer read Get_FrameCount;
    // ActiveFrame : Returns/Sets the current frame in the image 
   property ActiveFrame:Integer read Get_ActiveFrame write Set_ActiveFrame;
    // Properties : A collection of all properties for this image 
   property Properties:IProperties read Get_Properties;
  end;


// IImageFile : 

 IImageFileDisp = dispinterface
   ['{F4243B65-3F63-4D99-93CD-86B6D62C5EB2}']
    // LoadFile : Loads the ImageFile object with the specified File 
   procedure LoadFile(Filename:WideString);dispid 17;
    // SaveFile : Save the ImageFile object to the specified File 
   procedure SaveFile(Filename:WideString);dispid 18;
    // FormatID : Returns the FormatID for this file type 
   property FormatID:WideString  readonly dispid 1;
    // FileExtension : Returns the file extension for this image file type 
   property FileExtension:WideString  readonly dispid 2;
    // FileData : Returns the raw image file as a Vector of Bytes 
   property FileData:IVector  readonly dispid 3;
    // ARGBData : Returns the raw image bits as a Vector of Long values 
   property ARGBData:IVector  readonly dispid 4;
    // Height : Returns the Height of the image in pixels 
   property Height:Integer  readonly dispid 5;
    // Width : Returns the Width of the image in pixels 
   property Width:Integer  readonly dispid 6;
    // HorizontalResolution : Returns the Horizontal pixels per inch of the image 
   property HorizontalResolution:Double  readonly dispid 7;
    // VerticalResolution : Returns the Vertical pixels per inch of the image 
   property VerticalResolution:Double  readonly dispid 8;
    // PixelDepth : Returns the depth of the pixels of the image in bits per pixel 
   property PixelDepth:Integer  readonly dispid 9;
    // IsIndexedPixelFormat : Indicates if the pixel data is an index into a palette or the actual color data 
   property IsIndexedPixelFormat:WordBool  readonly dispid 10;
    // IsAlphaPixelFormat : Indicates if the pixel format has an alpha component 
   property IsAlphaPixelFormat:WordBool  readonly dispid 11;
    // IsExtendedPixelFormat : Indicates if the pixel format is extended (16 bits/channel) 
   property IsExtendedPixelFormat:WordBool  readonly dispid 12;
    // IsAnimated : Indicates whether the image is animated 
   property IsAnimated:WordBool  readonly dispid 13;
    // FrameCount : Returns the number of frames in the image 
   property FrameCount:Integer  readonly dispid 14;
    // ActiveFrame : Returns/Sets the current frame in the image 
   property ActiveFrame:Integer dispid 15;
    // Properties : A collection of all properties for this image 
   property Properties:IProperties  readonly dispid 16;
  end;


// IVector : 

 IVector = interface(IDispatch)
   ['{696F2367-6619-49BD-BA96-904DC2609990}']
   function Get_Item(Index:Integer) : OleVariant; safecall;
   procedure Set_Item(const Index:Integer; const parItem:POleVariant); safecall;
   procedure Set_Item_(const Index:Integer; const parItem:POleVariant); safecall;
   function Get_Count : Integer; safecall;
   function Get_Picture(Width:Integer) : OleVariant; safecall;
   function Get_ImageFile(Width:Integer) : IImageFile; safecall;
   function Get_BinaryData : OleVariant; safecall;
   procedure Set_BinaryData(const pvResult:POleVariant); safecall;
   function Get_String_(Unicode:WordBool) : WideString; safecall;
   function Get_Date : TDateTime; safecall;
   procedure Set_Date(const pdResult:TDateTime); safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Add : If Index is not zero, Inserts a new element into the Vector collection before the specified Index. If Index is zero, Appends a new element to the Vector collection. 
   procedure Add(var Value:OleVariant;Index:Integer);safecall;
    // Remove : Removes the designated element and returns it if successful 
   function Remove(Index:Integer):OleVariant;safecall;
    // Clear : Removes all elements. 
   procedure Clear;safecall;
    // SetFromString : Stores the string Value into the Vector of Bytes including the NULL terminator. Value may be truncated unless Resizable is True. The string will be stored as an ANSI string unless Unicode is True, in which case it will be stored as a Unicode string. 
   procedure SetFromString(Value:WideString;Resizable:WordBool;Unicode:WordBool);safecall;
    // Count : Returns the number of members in the vector 
   property Count:Integer read Get_Count;
    // Picture : If the Vector of Bytes contains an image file, then Width and Height are ignored. Otherwise a Vector of Bytes must be RGB data and a Vector of Longs must be ARGB data. Returns a Picture object on success. See the ImageFile method for more details. 
   property Picture[Width:Integer]:OleVariant read Get_Picture;
    // ImageFile : Used to get the Thumbnail property of an ImageFile which is an image file, The thumbnail property of an Item which is RGB data, or creating an ImageFile from raw ARGB data. Returns an ImageFile object on success. See the Picture method for more details. 
   property ImageFile[Width:Integer]:IImageFile read Get_ImageFile;
    // String : Returns a Vector of Bytes as a String 
   property String_[Unicode:WordBool]:WideString read Get_String_;
    // Date : Returns/Sets the Vector of Integers from a Date 
   property Date:TDateTime read Get_Date write Set_Date;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IVector : 

 IVectorDisp = dispinterface
   ['{696F2367-6619-49BD-BA96-904DC2609990}']
    // Add : If Index is not zero, Inserts a new element into the Vector collection before the specified Index. If Index is zero, Appends a new element to the Vector collection. 
   procedure Add(var Value:OleVariant;Index:Integer);dispid 7;
    // Remove : Removes the designated element and returns it if successful 
   function Remove(Index:Integer):OleVariant;dispid 8;
    // Clear : Removes all elements. 
   procedure Clear;dispid 9;
    // SetFromString : Stores the string Value into the Vector of Bytes including the NULL terminator. Value may be truncated unless Resizable is True. The string will be stored as an ANSI string unless Unicode is True, in which case it will be stored as a Unicode string. 
   procedure SetFromString(Value:WideString;Resizable:WordBool;Unicode:WordBool);dispid 10;
    // Item : Returns/Sets the specified item in the vector by position 
   property Item[Index:Integer]:OleVariant dispid 0; default;
    // Count : Returns the number of members in the vector 
   property Count:Integer  readonly dispid 1;
    // Picture : If the Vector of Bytes contains an image file, then Width and Height are ignored. Otherwise a Vector of Bytes must be RGB data and a Vector of Longs must be ARGB data. Returns a Picture object on success. See the ImageFile method for more details. 
   property Picture[Width:Integer]:OleVariant  readonly dispid 2;
    // ImageFile : Used to get the Thumbnail property of an ImageFile which is an image file, The thumbnail property of an Item which is RGB data, or creating an ImageFile from raw ARGB data. Returns an ImageFile object on success. See the Picture method for more details. 
   property ImageFile[Width:Integer]:IImageFile  readonly dispid 3;
    // BinaryData : Returns/Sets the Vector of Bytes as an array of bytes 
   property BinaryData:OleVariant dispid 4;
    // String : Returns a Vector of Bytes as a String 
   property String_[Unicode:WordBool]:WideString  readonly dispid 5;
    // Date : Returns/Sets the Vector of Integers from a Date 
   property Date:TDateTime dispid 6;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IProperties : 

 IProperties = interface(IDispatch)
   ['{40571E58-A308-470A-80AA-FA10F88793A0}']
   function Get_Item(Index:POleVariant) : IProperty; safecall;
   function Get_Count : Integer; safecall;
    // Exists : Indicates whether the specified Property exists in the collection 
   function Exists(var Index:OleVariant):WordBool;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Item : Returns the specified item in the collection either by position or name. 
   property Item[Index:POleVariant]:IProperty read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IProperties : 

 IPropertiesDisp = dispinterface
   ['{40571E58-A308-470A-80AA-FA10F88793A0}']
    // Exists : Indicates whether the specified Property exists in the collection 
   function Exists(var Index:OleVariant):WordBool;dispid 2;
    // Item : Returns the specified item in the collection either by position or name. 
   property Item[Index:POleVariant]:IProperty  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IProperty : 

 IProperty = interface(IDispatch)
   ['{706038DC-9F4B-4E45-88E2-5EB7D665B815}']
   function Get_Value : OleVariant; safecall;
   procedure Set_Value(const pvResult:POleVariant); safecall;
   procedure Set_Value_(const pvResult:POleVariant); safecall;
   function Get_Name : WideString; safecall;
   function Get_PropertyID : Integer; safecall;
   function Get_Type_ : Integer; safecall;
   function Get_IsReadOnly : WordBool; safecall;
   function Get_IsVector : WordBool; safecall;
   function Get_SubType : WiaSubType; safecall;
   function Get_SubTypeDefault : OleVariant; safecall;
   function Get_SubTypeValues : IVector; safecall;
   function Get_SubTypeMin : Integer; safecall;
   function Get_SubTypeMax : Integer; safecall;
   function Get_SubTypeStep : Integer; safecall;
    // Name : Returns the Property Name 
   property Name:WideString read Get_Name;
    // PropertyID : Returns the PropertyID of this Property 
   property PropertyID:Integer read Get_PropertyID;
    // Type : Returns either a WiaPropertyType or a WiaImagePropertyType 
   property Type_:Integer read Get_Type_;
    // IsReadOnly : Indicates whether the Property Value is read only 
   property IsReadOnly:WordBool read Get_IsReadOnly;
    // IsVector : Indicates whether the Property Value is a vector 
   property IsVector:WordBool read Get_IsVector;
    // SubType : Returns the SubType of the Property, if any 
   property SubType:WiaSubType read Get_SubType;
    // SubTypeDefault : Returns the default Property Value if the SubType is not UnspecifiedSubType 
   property SubTypeDefault:OleVariant read Get_SubTypeDefault;
    // SubTypeValues : Returns a Vector of valid Property Values if the SubType is ListSubType or valid flag Values that can be ored together if the SubType is FlagSubType 
   property SubTypeValues:IVector read Get_SubTypeValues;
    // SubTypeMin : Returns the minimum valid Property Value if the SubType is RangeSubType 
   property SubTypeMin:Integer read Get_SubTypeMin;
    // SubTypeMax : Returns the maximum valid Property Value if the SubType is RangeSubType 
   property SubTypeMax:Integer read Get_SubTypeMax;
    // SubTypeStep : Returns the step increment of Property Values if the SubType is RangeSubType 
   property SubTypeStep:Integer read Get_SubTypeStep;
  end;


// IProperty : 

 IPropertyDisp = dispinterface
   ['{706038DC-9F4B-4E45-88E2-5EB7D665B815}']
    // Value : Returns/Sets the Property Value 
   property Value:OleVariant dispid 0;
    // Name : Returns the Property Name 
   property Name:WideString  readonly dispid 1;
    // PropertyID : Returns the PropertyID of this Property 
   property PropertyID:Integer  readonly dispid 2;
    // Type : Returns either a WiaPropertyType or a WiaImagePropertyType 
   property Type_:Integer  readonly dispid 3;
    // IsReadOnly : Indicates whether the Property Value is read only 
   property IsReadOnly:WordBool  readonly dispid 4;
    // IsVector : Indicates whether the Property Value is a vector 
   property IsVector:WordBool  readonly dispid 5;
    // SubType : Returns the SubType of the Property, if any 
   property SubType:WiaSubType  readonly dispid 6;
    // SubTypeDefault : Returns the default Property Value if the SubType is not UnspecifiedSubType 
   property SubTypeDefault:OleVariant  readonly dispid 7;
    // SubTypeValues : Returns a Vector of valid Property Values if the SubType is ListSubType or valid flag Values that can be ored together if the SubType is FlagSubType 
   property SubTypeValues:IVector  readonly dispid 8;
    // SubTypeMin : Returns the minimum valid Property Value if the SubType is RangeSubType 
   property SubTypeMin:Integer  readonly dispid 9;
    // SubTypeMax : Returns the maximum valid Property Value if the SubType is RangeSubType 
   property SubTypeMax:Integer  readonly dispid 10;
    // SubTypeStep : Returns the step increment of Property Values if the SubType is RangeSubType 
   property SubTypeStep:Integer  readonly dispid 11;
  end;


// IFilterInfo : 

 IFilterInfo = interface(IDispatch)
   ['{EFD1219F-8229-4B30-809D-8F6D83341569}']
   function Get_Name : WideString; safecall;
   function Get_Description : WideString; safecall;
   function Get_FilterID : WideString; safecall;
    // Name : Returns the FilterInfo Name 
   property Name:WideString read Get_Name;
    // Description : Returns a technical Description of what the filter does and how to use it in a filter chain 
   property Description:WideString read Get_Description;
    // FilterID : Returns the FilterID for this filter 
   property FilterID:WideString read Get_FilterID;
  end;


// IFilterInfo : 

 IFilterInfoDisp = dispinterface
   ['{EFD1219F-8229-4B30-809D-8F6D83341569}']
    // Name : Returns the FilterInfo Name 
   property Name:WideString  readonly dispid 1;
    // Description : Returns a technical Description of what the filter does and how to use it in a filter chain 
   property Description:WideString  readonly dispid 2;
    // FilterID : Returns the FilterID for this filter 
   property FilterID:WideString  readonly dispid 3;
  end;


// IFilterInfos : 

 IFilterInfos = interface(IDispatch)
   ['{AF49723A-499C-411C-B19A-1B8244D67E44}']
   function Get_Item(Index:POleVariant) : IFilterInfo; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Item : Returns the specified item in the collection either by position or name 
   property Item[Index:POleVariant]:IFilterInfo read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IFilterInfos : 

 IFilterInfosDisp = dispinterface
   ['{AF49723A-499C-411C-B19A-1B8244D67E44}']
    // Item : Returns the specified item in the collection either by position or name 
   property Item[Index:POleVariant]:IFilterInfo  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IFilter : 

 IFilter = interface(IDispatch)
   ['{851E9802-B338-4AB3-BB6B-6AA57CC699D0}']
   function Get_Name : WideString; safecall;
   function Get_Description : WideString; safecall;
   function Get_FilterID : WideString; safecall;
   function Get_Properties : IProperties; safecall;
    // Name : Returns the Filter Name 
   property Name:WideString read Get_Name;
    // Description : Returns a Description of what the filter does 
   property Description:WideString read Get_Description;
    // FilterID : Returns the FilterID for this Filter 
   property FilterID:WideString read Get_FilterID;
    // Properties : A collection of all properties for this filter 
   property Properties:IProperties read Get_Properties;
  end;


// IFilter : 

 IFilterDisp = dispinterface
   ['{851E9802-B338-4AB3-BB6B-6AA57CC699D0}']
    // Name : Returns the Filter Name 
   property Name:WideString  readonly dispid 1;
    // Description : Returns a Description of what the filter does 
   property Description:WideString  readonly dispid 2;
    // FilterID : Returns the FilterID for this Filter 
   property FilterID:WideString  readonly dispid 3;
    // Properties : A collection of all properties for this filter 
   property Properties:IProperties  readonly dispid 4;
  end;


// IFilters : 

 IFilters = interface(IDispatch)
   ['{C82FFED4-0A8D-4F85-B90A-AC8E720D39C1}']
   function Get_Item(Index:Integer) : IFilter; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Add : Appends/Inserts a new Filter of the specified FilterID into a Filter collection 
   procedure Add(FilterID:WideString;Index:Integer);safecall;
    // Remove : Removes the designated filter 
   procedure Remove(Index:Integer);safecall;
    // Item : Returns the specified item in the collection by position or FilterID 
   property Item[Index:Integer]:IFilter read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IFilters : 

 IFiltersDisp = dispinterface
   ['{C82FFED4-0A8D-4F85-B90A-AC8E720D39C1}']
    // Add : Appends/Inserts a new Filter of the specified FilterID into a Filter collection 
   procedure Add(FilterID:WideString;Index:Integer);dispid 2;
    // Remove : Removes the designated filter 
   procedure Remove(Index:Integer);dispid 3;
    // Item : Returns the specified item in the collection by position or FilterID 
   property Item[Index:Integer]:IFilter  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IImageProcess : 

 IImageProcess = interface(IDispatch)
   ['{41506929-7855-4392-9E6F-98D88513E55D}']
   function Get_FilterInfos : IFilterInfos; safecall;
   function Get_Filters : IFilters; safecall;
    // Apply : Takes the specified ImageFile and returns the new ImageFile with all the filters applied on success 
   function Apply(Source:IImageFile):IImageFile;safecall;
    // FilterInfos : A collection of all available filters 
   property FilterInfos:IFilterInfos read Get_FilterInfos;
    // Filters : A collection of the filters to be applied in this process 
   property Filters:IFilters read Get_Filters;
  end;


// IImageProcess : 

 IImageProcessDisp = dispinterface
   ['{41506929-7855-4392-9E6F-98D88513E55D}']
    // Apply : Takes the specified ImageFile and returns the new ImageFile with all the filters applied on success 
   function Apply(Source:IImageFile):IImageFile;dispid 4;
    // FilterInfos : A collection of all available filters 
   property FilterInfos:IFilterInfos  readonly dispid 1;
    // Filters : A collection of the filters to be applied in this process 
   property Filters:IFilters  readonly dispid 2;
  end;


// IFormats : 

 IFormats = interface(IDispatch)
   ['{882A274F-DF2F-4F6D-9F5A-AF4FD484530D}']
   function Get_Item(Index:Integer) : WideString; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:WideString read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IFormats : 

 IFormatsDisp = dispinterface
   ['{882A274F-DF2F-4F6D-9F5A-AF4FD484530D}']
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:WideString  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IDeviceCommand : 

 IDeviceCommand = interface(IDispatch)
   ['{7CF694C0-F589-451C-B56E-398B5855B05E}']
   function Get_CommandID : WideString; safecall;
   function Get_Name : WideString; safecall;
   function Get_Description : WideString; safecall;
    // CommandID : Returns the commandID for this Command 
   property CommandID:WideString read Get_CommandID;
    // Name : Returns the command Name 
   property Name:WideString read Get_Name;
    // Description : Returns the command Description 
   property Description:WideString read Get_Description;
  end;


// IDeviceCommand : 

 IDeviceCommandDisp = dispinterface
   ['{7CF694C0-F589-451C-B56E-398B5855B05E}']
    // CommandID : Returns the commandID for this Command 
   property CommandID:WideString  readonly dispid 1;
    // Name : Returns the command Name 
   property Name:WideString  readonly dispid 2;
    // Description : Returns the command Description 
   property Description:WideString  readonly dispid 3;
  end;


// IDeviceCommands : 

 IDeviceCommands = interface(IDispatch)
   ['{C53AE9D5-6D91-4815-AF93-5F1E1B3B08BD}']
   function Get_Item(Index:Integer) : IDeviceCommand; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:IDeviceCommand read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IDeviceCommands : 

 IDeviceCommandsDisp = dispinterface
   ['{C53AE9D5-6D91-4815-AF93-5F1E1B3B08BD}']
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:IDeviceCommand  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IItems : 

 IItems = interface(IDispatch)
   ['{46102071-60B4-4E58-8620-397D17B0BB5B}']
   function Get_Item(Index:Integer) : IItem; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Add : Adds a new Item with the specified Name and Flags. The Flags value is created by using the OR operation with members of the WiaItemFlags enumeration. 
   procedure Add(Name:WideString;Flags:Integer);safecall;
    // Remove : Removes the designated Item 
   procedure Remove(Index:Integer);safecall;
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:IItem read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IItems : 

 IItemsDisp = dispinterface
   ['{46102071-60B4-4E58-8620-397D17B0BB5B}']
    // Add : Adds a new Item with the specified Name and Flags. The Flags value is created by using the OR operation with members of the WiaItemFlags enumeration. 
   procedure Add(Name:WideString;Flags:Integer);dispid 2;
    // Remove : Removes the designated Item 
   procedure Remove(Index:Integer);dispid 3;
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:IItem  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IItem : 

 IItem = interface(IDispatch)
   ['{68F2BF12-A755-4E2B-9BCD-37A22587D078}']
   function Get_ItemID : WideString; safecall;
   function Get_Properties : IProperties; safecall;
   function Get_Items : IItems; safecall;
   function Get_Formats : IFormats; safecall;
   function Get_Commands : IDeviceCommands; safecall;
   function Get_WiaItem : IUnknown; safecall;
    // Transfer : Returns an ImageFile object, in this version, in the format specified in FormatID if supported, otherwise using the preferred format for this imaging device. Future versions may return a collection of ImageFile objects. 
   function Transfer(FormatID:WideString):OleVariant;safecall;
    // ExecuteCommand : Issues the command specified by CommandID. CommandIDs are device dependent. Valid CommandIDs for this Item are contained in the Commands collection. 
   function ExecuteCommand(CommandID:WideString):IItem;safecall;
    // ItemID : Returns the ItemID for this Item 
   property ItemID:WideString read Get_ItemID;
    // Properties : A collection of all properties for this item 
   property Properties:IProperties read Get_Properties;
    // Items : A collection of all child items for this item 
   property Items:IItems read Get_Items;
    // Formats : A collection of all supported format types for this item 
   property Formats:IFormats read Get_Formats;
    // Commands : A collection of all commands for this item 
   property Commands:IDeviceCommands read Get_Commands;
    // WiaItem : Returns the underlying IWiaItem interface for this Item object 
   property WiaItem:IUnknown read Get_WiaItem;
  end;


// IItem : 

 IItemDisp = dispinterface
   ['{68F2BF12-A755-4E2B-9BCD-37A22587D078}']
    // Transfer : Returns an ImageFile object, in this version, in the format specified in FormatID if supported, otherwise using the preferred format for this imaging device. Future versions may return a collection of ImageFile objects. 
   function Transfer(FormatID:WideString):OleVariant;dispid 7;
    // ExecuteCommand : Issues the command specified by CommandID. CommandIDs are device dependent. Valid CommandIDs for this Item are contained in the Commands collection. 
   function ExecuteCommand(CommandID:WideString):IItem;dispid 8;
    // ItemID : Returns the ItemID for this Item 
   property ItemID:WideString  readonly dispid 1;
    // Properties : A collection of all properties for this item 
   property Properties:IProperties  readonly dispid 2;
    // Items : A collection of all child items for this item 
   property Items:IItems  readonly dispid 3;
    // Formats : A collection of all supported format types for this item 
   property Formats:IFormats  readonly dispid 4;
    // Commands : A collection of all commands for this item 
   property Commands:IDeviceCommands  readonly dispid 5;
    // WiaItem : Returns the underlying IWiaItem interface for this Item object 
   property WiaItem:IUnknown  readonly dispid 6;
  end;


// IDeviceEvent : 

 IDeviceEvent = interface(IDispatch)
   ['{80D0880A-BB10-4722-82D1-07DC8DA157E2}']
   function Get_EventID : WideString; safecall;
   function Get_Type_ : WiaEventFlag; safecall;
   function Get_Name : WideString; safecall;
   function Get_Description : WideString; safecall;
    // EventID : Returns the EventID for this Event 
   property EventID:WideString read Get_EventID;
    // Type : Returns the Type of this Event 
   property Type_:WiaEventFlag read Get_Type_;
    // Name : Returns the event Name 
   property Name:WideString read Get_Name;
    // Description : Returns the event Description 
   property Description:WideString read Get_Description;
  end;


// IDeviceEvent : 

 IDeviceEventDisp = dispinterface
   ['{80D0880A-BB10-4722-82D1-07DC8DA157E2}']
    // EventID : Returns the EventID for this Event 
   property EventID:WideString  readonly dispid 1;
    // Type : Returns the Type of this Event 
   property Type_:WiaEventFlag  readonly dispid 2;
    // Name : Returns the event Name 
   property Name:WideString  readonly dispid 3;
    // Description : Returns the event Description 
   property Description:WideString  readonly dispid 4;
  end;


// IDeviceEvents : 

 IDeviceEvents = interface(IDispatch)
   ['{03985C95-581B-44D1-9403-8488B347538B}']
   function Get_Item(Index:Integer) : IDeviceEvent; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:IDeviceEvent read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IDeviceEvents : 

 IDeviceEventsDisp = dispinterface
   ['{03985C95-581B-44D1-9403-8488B347538B}']
    // Item : Returns the specified item in the collection by position 
   property Item[Index:Integer]:IDeviceEvent  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IDevice : 

 IDevice = interface(IDispatch)
   ['{3714EAC4-F413-426B-B1E8-DEF2BE99EA55}']
   function Get_DeviceID : WideString; safecall;
   function Get_Type_ : WiaDeviceType; safecall;
   function Get_Properties : IProperties; safecall;
   function Get_Items : IItems; safecall;
   function Get_Commands : IDeviceCommands; safecall;
   function Get_Events : IDeviceEvents; safecall;
   function Get_WiaItem : IUnknown; safecall;
    // GetItem : Returns the Item object specified by ItemID if it exists 
   function GetItem(ItemID:WideString):IItem;safecall;
    // ExecuteCommand : Issues the command specified by CommandID to the imaging device. CommandIDs are device dependent. Valid CommandIDs for this Device are contained in the Commands collection. 
   function ExecuteCommand(CommandID:WideString):IItem;safecall;
    // DeviceID : Returns the DeviceID for this Device 
   property DeviceID:WideString read Get_DeviceID;
    // Type : Returns the Type of Device 
   property Type_:WiaDeviceType read Get_Type_;
    // Properties : A collection of all properties for this imaging device 
   property Properties:IProperties read Get_Properties;
    // Items : A collection of all items for this imaging device 
   property Items:IItems read Get_Items;
    // Commands : A collection of all commands for this imaging device 
   property Commands:IDeviceCommands read Get_Commands;
    // Events : A collection of all events for this imaging device 
   property Events:IDeviceEvents read Get_Events;
    // WiaItem : Returns the underlying IWiaItem interface for this Device object 
   property WiaItem:IUnknown read Get_WiaItem;
  end;


// IDevice : 

 IDeviceDisp = dispinterface
   ['{3714EAC4-F413-426B-B1E8-DEF2BE99EA55}']
    // GetItem : Returns the Item object specified by ItemID if it exists 
   function GetItem(ItemID:WideString):IItem;dispid 8;
    // ExecuteCommand : Issues the command specified by CommandID to the imaging device. CommandIDs are device dependent. Valid CommandIDs for this Device are contained in the Commands collection. 
   function ExecuteCommand(CommandID:WideString):IItem;dispid 9;
    // DeviceID : Returns the DeviceID for this Device 
   property DeviceID:WideString  readonly dispid 1;
    // Type : Returns the Type of Device 
   property Type_:WiaDeviceType  readonly dispid 2;
    // Properties : A collection of all properties for this imaging device 
   property Properties:IProperties  readonly dispid 3;
    // Items : A collection of all items for this imaging device 
   property Items:IItems  readonly dispid 4;
    // Commands : A collection of all commands for this imaging device 
   property Commands:IDeviceCommands  readonly dispid 5;
    // Events : A collection of all events for this imaging device 
   property Events:IDeviceEvents  readonly dispid 6;
    // WiaItem : Returns the underlying IWiaItem interface for this Device object 
   property WiaItem:IUnknown  readonly dispid 7;
  end;


// IDeviceInfo : 

 IDeviceInfo = interface(IDispatch)
   ['{2A99020A-E325-4454-95E0-136726ED4818}']
   function Get_DeviceID : WideString; safecall;
   function Get_Type_ : WiaDeviceType; safecall;
   function Get_Properties : IProperties; safecall;
    // Connect : Establish a connection with this device and return a Device object 
   function Connect:IDevice;safecall;
    // DeviceID : Returns the DeviceID for this Device 
   property DeviceID:WideString read Get_DeviceID;
    // Type : Returns the Type of Device 
   property Type_:WiaDeviceType read Get_Type_;
    // Properties : A collection of all properties for this imaging device that are applicable when the device is not connected 
   property Properties:IProperties read Get_Properties;
  end;


// IDeviceInfo : 

 IDeviceInfoDisp = dispinterface
   ['{2A99020A-E325-4454-95E0-136726ED4818}']
    // Connect : Establish a connection with this device and return a Device object 
   function Connect:IDevice;dispid 4;
    // DeviceID : Returns the DeviceID for this Device 
   property DeviceID:WideString  readonly dispid 1;
    // Type : Returns the Type of Device 
   property Type_:WiaDeviceType  readonly dispid 2;
    // Properties : A collection of all properties for this imaging device that are applicable when the device is not connected 
   property Properties:IProperties  readonly dispid 3;
  end;


// IDeviceInfos : 

 IDeviceInfos = interface(IDispatch)
   ['{FE076B64-8406-4E92-9CAC-9093F378E05F}']
   function Get_Item(Index:POleVariant) : IDeviceInfo; safecall;
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
    // Item : Returns the specified item in the collection either by position or Device ID 
   property Item[Index:POleVariant]:IDeviceInfo read Get_Item; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IDeviceInfos : 

 IDeviceInfosDisp = dispinterface
   ['{FE076B64-8406-4E92-9CAC-9093F378E05F}']
    // Item : Returns the specified item in the collection either by position or Device ID 
   property Item[Index:POleVariant]:IDeviceInfo  readonly dispid 0; default;
    // Count : Returns the number of members in the collection 
   property Count:Integer  readonly dispid 1;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// ICommonDialog : 

 ICommonDialog = interface(IDispatch)
   ['{B4760F13-D9F3-4DF8-94B5-D225F86EE9A1}']
    // ShowAcquisitionWizard : Launches the Windows Scanner and Camera Wizard and returns Nothing. Future versions may return a collection of ImageFile objects. 
   function ShowAcquisitionWizard(Device:IDevice):OleVariant;safecall;
    // ShowAcquireImage : Displays one or more dialog boxes that enable the user to acquire an image from a hardware device for image acquisition and returns an ImageFile object on success, otherwise Nothing 
   function ShowAcquireImage(DeviceType:WiaDeviceType;Intent:WiaImageIntent;Bias:WiaImageBias;FormatID:WideString;AlwaysSelectDevice:WordBool;UseCommonUI:WordBool;CancelError:WordBool):IImageFile;safecall;
    // ShowSelectDevice : Displays a dialog box that enables the user to select a hardware device for image acquisition. Returns the selected Device object on success, otherwise Nothing 
   function ShowSelectDevice(DeviceType:WiaDeviceType;AlwaysSelectDevice:WordBool;CancelError:WordBool):IDevice;safecall;
    // ShowSelectItems : Displays a dialog box that enables the user to select an item for transfer from a hardware device for image acquisition. Returns the selection as an Items collection on success, otherwise Nothing 
   function ShowSelectItems(Device:IDevice;Intent:WiaImageIntent;Bias:WiaImageBias;SingleSelect:WordBool;UseCommonUI:WordBool;CancelError:WordBool):IItems;safecall;
    // ShowDeviceProperties : Displays the properties dialog box for the specified Device 
   procedure ShowDeviceProperties(Device:IDevice;CancelError:WordBool);safecall;
    // ShowItemProperties : Displays the properties dialog box for the specified Item 
   procedure ShowItemProperties(Item:IItem;CancelError:WordBool);safecall;
    // ShowTransfer : Displays a progress dialog box while transferring the specified Item to the local machine. See Item.Transfer for additional information. 
   function ShowTransfer(Item:IItem;FormatID:WideString;CancelError:WordBool):OleVariant;safecall;
    // ShowPhotoPrintingWizard : Launches the Photo Printing Wizard with the absolute path of a specific file or Vector of absolute paths to files 
   procedure ShowPhotoPrintingWizard(var Files:OleVariant);safecall;
  end;


// ICommonDialog : 

 ICommonDialogDisp = dispinterface
   ['{B4760F13-D9F3-4DF8-94B5-D225F86EE9A1}']
    // ShowAcquisitionWizard : Launches the Windows Scanner and Camera Wizard and returns Nothing. Future versions may return a collection of ImageFile objects. 
   function ShowAcquisitionWizard(Device:IDevice):OleVariant;dispid 1;
    // ShowAcquireImage : Displays one or more dialog boxes that enable the user to acquire an image from a hardware device for image acquisition and returns an ImageFile object on success, otherwise Nothing 
   function ShowAcquireImage(DeviceType:WiaDeviceType;Intent:WiaImageIntent;Bias:WiaImageBias;FormatID:WideString;AlwaysSelectDevice:WordBool;UseCommonUI:WordBool;CancelError:WordBool):IImageFile;dispid 2;
    // ShowSelectDevice : Displays a dialog box that enables the user to select a hardware device for image acquisition. Returns the selected Device object on success, otherwise Nothing 
   function ShowSelectDevice(DeviceType:WiaDeviceType;AlwaysSelectDevice:WordBool;CancelError:WordBool):IDevice;dispid 3;
    // ShowSelectItems : Displays a dialog box that enables the user to select an item for transfer from a hardware device for image acquisition. Returns the selection as an Items collection on success, otherwise Nothing 
   function ShowSelectItems(Device:IDevice;Intent:WiaImageIntent;Bias:WiaImageBias;SingleSelect:WordBool;UseCommonUI:WordBool;CancelError:WordBool):IItems;dispid 4;
    // ShowDeviceProperties : Displays the properties dialog box for the specified Device 
   procedure ShowDeviceProperties(Device:IDevice;CancelError:WordBool);dispid 5;
    // ShowItemProperties : Displays the properties dialog box for the specified Item 
   procedure ShowItemProperties(Item:IItem;CancelError:WordBool);dispid 6;
    // ShowTransfer : Displays a progress dialog box while transferring the specified Item to the local machine. See Item.Transfer for additional information. 
   function ShowTransfer(Item:IItem;FormatID:WideString;CancelError:WordBool):OleVariant;dispid 7;
    // ShowPhotoPrintingWizard : Launches the Photo Printing Wizard with the absolute path of a specific file or Vector of absolute paths to files 
   procedure ShowPhotoPrintingWizard(var Files:OleVariant);dispid 8;
  end;


// IDeviceManager : 

 IDeviceManager = interface(IDispatch)
   ['{73856D9A-2720-487A-A584-21D5774E9D0F}']
   function Get_DeviceInfos : IDeviceInfos; safecall;
    // RegisterEvent : Registers the specified EventID for the specified DeviceID. If DeviceID is "*" then OnEvent will be called whenever the event specified occurs for any device. Otherwise, OnEvent will only be called if the event specified occurs on the device specified. 
   procedure RegisterEvent(EventID:WideString;DeviceID:WideString);safecall;
    // UnregisterEvent : Unregisters the specified EventID for the specified DeviceID. UnregisterEvent should only be called for EventID and DeviceID for which you called RegisterEvent. 
   procedure UnregisterEvent(EventID:WideString;DeviceID:WideString);safecall;
    // RegisterPersistentEvent : Registers the specified Command to launch when the specified EventID for the specified DeviceID occurs. Command can be either a ClassID or the full path name and the appropriate command-line arguments needed to invoke the application. 
   procedure RegisterPersistentEvent(Command:WideString;Name:WideString;Description:WideString;Icon:WideString;EventID:WideString;DeviceID:WideString);safecall;
    // UnregisterPersistentEvent : Unregisters the specified Command for the specified EventID for the specified DeviceID. UnregisterPersistentEvent should only be called for the Command, Name, Description, Icon, EventID and DeviceID for which you called RegisterPersistentEvent. 
   procedure UnregisterPersistentEvent(Command:WideString;Name:WideString;Description:WideString;Icon:WideString;EventID:WideString;DeviceID:WideString);safecall;
    // DeviceInfos : A collection of all imaging devices connected to this computer 
   property DeviceInfos:IDeviceInfos read Get_DeviceInfos;
  end;


// IDeviceManager : 

 IDeviceManagerDisp = dispinterface
   ['{73856D9A-2720-487A-A584-21D5774E9D0F}']
    // RegisterEvent : Registers the specified EventID for the specified DeviceID. If DeviceID is "*" then OnEvent will be called whenever the event specified occurs for any device. Otherwise, OnEvent will only be called if the event specified occurs on the device specified. 
   procedure RegisterEvent(EventID:WideString;DeviceID:WideString);dispid 2;
    // UnregisterEvent : Unregisters the specified EventID for the specified DeviceID. UnregisterEvent should only be called for EventID and DeviceID for which you called RegisterEvent. 
   procedure UnregisterEvent(EventID:WideString;DeviceID:WideString);dispid 3;
    // RegisterPersistentEvent : Registers the specified Command to launch when the specified EventID for the specified DeviceID occurs. Command can be either a ClassID or the full path name and the appropriate command-line arguments needed to invoke the application. 
   procedure RegisterPersistentEvent(Command:WideString;Name:WideString;Description:WideString;Icon:WideString;EventID:WideString;DeviceID:WideString);dispid 4;
    // UnregisterPersistentEvent : Unregisters the specified Command for the specified EventID for the specified DeviceID. UnregisterPersistentEvent should only be called for the Command, Name, Description, Icon, EventID and DeviceID for which you called RegisterPersistentEvent. 
   procedure UnregisterPersistentEvent(Command:WideString;Name:WideString;Description:WideString;Icon:WideString;EventID:WideString;DeviceID:WideString);dispid 5;
    // DeviceInfos : A collection of all imaging devices connected to this computer 
   property DeviceInfos:IDeviceInfos  readonly dispid 1;
  end;


// _IDeviceManagerEvents : 

 _IDeviceManagerEvents = dispinterface
   ['{2E9A5206-2360-49DF-9D9B-1762B4BEAE77}']
    // OnEvent : Occurs for any event registered with RegisterEvent 
   function OnEvent(EventID:WideString;DeviceID:WideString;ItemID:WideString):HResult;dispid 1;
  end;

//CoClasses
  CoRational = Class
  Public
    Class Function Create: IRational;
    Class Function CreateRemote(const MachineName: string): IRational;
  end;

  CoVector = Class
  Public
    Class Function Create: IVector;
    Class Function CreateRemote(const MachineName: string): IVector;
  end;

  CoProperty = Class
  Public
    Class Function Create: IProperty;
    Class Function CreateRemote(const MachineName: string): IProperty;
  end;

  CoProperties = Class
  Public
    Class Function Create: IProperties;
    Class Function CreateRemote(const MachineName: string): IProperties;
  end;

  CoImageFile = Class
  Public
    Class Function Create: IImageFile;
    Class Function CreateRemote(const MachineName: string): IImageFile;
  end;

  CoFilterInfo = Class
  Public
    Class Function Create: IFilterInfo;
    Class Function CreateRemote(const MachineName: string): IFilterInfo;
  end;

  CoFilterInfos = Class
  Public
    Class Function Create: IFilterInfos;
    Class Function CreateRemote(const MachineName: string): IFilterInfos;
  end;

  CoFilter = Class
  Public
    Class Function Create: IFilter;
    Class Function CreateRemote(const MachineName: string): IFilter;
  end;

  CoFilters = Class
  Public
    Class Function Create: IFilters;
    Class Function CreateRemote(const MachineName: string): IFilters;
  end;

  CoImageProcess = Class
  Public
    Class Function Create: IImageProcess;
    Class Function CreateRemote(const MachineName: string): IImageProcess;
  end;

  CoFormats = Class
  Public
    Class Function Create: IFormats;
    Class Function CreateRemote(const MachineName: string): IFormats;
  end;

  CoDeviceCommand = Class
  Public
    Class Function Create: IDeviceCommand;
    Class Function CreateRemote(const MachineName: string): IDeviceCommand;
  end;

  CoDeviceCommands = Class
  Public
    Class Function Create: IDeviceCommands;
    Class Function CreateRemote(const MachineName: string): IDeviceCommands;
  end;

  CoItem = Class
  Public
    Class Function Create: IItem;
    Class Function CreateRemote(const MachineName: string): IItem;
  end;

  CoItems = Class
  Public
    Class Function Create: IItems;
    Class Function CreateRemote(const MachineName: string): IItems;
  end;

  CoDeviceEvent = Class
  Public
    Class Function Create: IDeviceEvent;
    Class Function CreateRemote(const MachineName: string): IDeviceEvent;
  end;

  CoDeviceEvents = Class
  Public
    Class Function Create: IDeviceEvents;
    Class Function CreateRemote(const MachineName: string): IDeviceEvents;
  end;

  CoDeviceInfo = Class
  Public
    Class Function Create: IDeviceInfo;
    Class Function CreateRemote(const MachineName: string): IDeviceInfo;
  end;

  CoDeviceInfos = Class
  Public
    Class Function Create: IDeviceInfos;
    Class Function CreateRemote(const MachineName: string): IDeviceInfos;
  end;

  CoDevice = Class
  Public
    Class Function Create: IDevice;
    Class Function CreateRemote(const MachineName: string): IDevice;
  end;

  CoCommonDialog = Class
  Public
    Class Function Create: ICommonDialog;
    Class Function CreateRemote(const MachineName: string): ICommonDialog;
  end;

  T_IDeviceManagerEventsOnEvent = procedure(Sender: TObject;EventID:WideString;DeviceID:WideString;ItemID:WideString) of object;


  CoDeviceManager = Class
  Public
    Class Function Create: IDeviceManager;
    Class Function CreateRemote(const MachineName: string): IDeviceManager;
  end;

  TEvsDeviceManager = Class(TEventSink)
  Private
    FOnOnEvent:T_IDeviceManagerEventsOnEvent;

    fServer:IDeviceManager;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:IDeviceManager read fServer;
    property OnOnEvent : T_IDeviceManagerEventsOnEvent read FOnOnEvent write FOnOnEvent;

  end;

implementation

uses comobj;

Class Function CoRational.Create: IRational;
begin
  Result := CreateComObject(CLASS_Rational) as IRational;
end;

Class Function CoRational.CreateRemote(const MachineName: string): IRational;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Rational) as IRational;
end;

Class Function CoVector.Create: IVector;
begin
  Result := CreateComObject(CLASS_Vector) as IVector;
end;

Class Function CoVector.CreateRemote(const MachineName: string): IVector;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Vector) as IVector;
end;

Class Function CoProperty.Create: IProperty;
begin
  Result := CreateComObject(CLASS_Property) as IProperty;
end;

Class Function CoProperty.CreateRemote(const MachineName: string): IProperty;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Property) as IProperty;
end;

Class Function CoProperties.Create: IProperties;
begin
  Result := CreateComObject(CLASS_Properties) as IProperties;
end;

Class Function CoProperties.CreateRemote(const MachineName: string): IProperties;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Properties) as IProperties;
end;

Class Function CoImageFile.Create: IImageFile;
begin
  Result := CreateComObject(CLASS_ImageFile) as IImageFile;
end;

Class Function CoImageFile.CreateRemote(const MachineName: string): IImageFile;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_ImageFile) as IImageFile;
end;

Class Function CoFilterInfo.Create: IFilterInfo;
begin
  Result := CreateComObject(CLASS_FilterInfo) as IFilterInfo;
end;

Class Function CoFilterInfo.CreateRemote(const MachineName: string): IFilterInfo;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_FilterInfo) as IFilterInfo;
end;

Class Function CoFilterInfos.Create: IFilterInfos;
begin
  Result := CreateComObject(CLASS_FilterInfos) as IFilterInfos;
end;

Class Function CoFilterInfos.CreateRemote(const MachineName: string): IFilterInfos;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_FilterInfos) as IFilterInfos;
end;

Class Function CoFilter.Create: IFilter;
begin
  Result := CreateComObject(CLASS_Filter) as IFilter;
end;

Class Function CoFilter.CreateRemote(const MachineName: string): IFilter;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Filter) as IFilter;
end;

Class Function CoFilters.Create: IFilters;
begin
  Result := CreateComObject(CLASS_Filters) as IFilters;
end;

Class Function CoFilters.CreateRemote(const MachineName: string): IFilters;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Filters) as IFilters;
end;

Class Function CoImageProcess.Create: IImageProcess;
begin
  Result := CreateComObject(CLASS_ImageProcess) as IImageProcess;
end;

Class Function CoImageProcess.CreateRemote(const MachineName: string): IImageProcess;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_ImageProcess) as IImageProcess;
end;

Class Function CoFormats.Create: IFormats;
begin
  Result := CreateComObject(CLASS_Formats) as IFormats;
end;

Class Function CoFormats.CreateRemote(const MachineName: string): IFormats;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Formats) as IFormats;
end;

Class Function CoDeviceCommand.Create: IDeviceCommand;
begin
  Result := CreateComObject(CLASS_DeviceCommand) as IDeviceCommand;
end;

Class Function CoDeviceCommand.CreateRemote(const MachineName: string): IDeviceCommand;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceCommand) as IDeviceCommand;
end;

Class Function CoDeviceCommands.Create: IDeviceCommands;
begin
  Result := CreateComObject(CLASS_DeviceCommands) as IDeviceCommands;
end;

Class Function CoDeviceCommands.CreateRemote(const MachineName: string): IDeviceCommands;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceCommands) as IDeviceCommands;
end;

Class Function CoItem.Create: IItem;
begin
  Result := CreateComObject(CLASS_Item) as IItem;
end;

Class Function CoItem.CreateRemote(const MachineName: string): IItem;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Item) as IItem;
end;

Class Function CoItems.Create: IItems;
begin
  Result := CreateComObject(CLASS_Items) as IItems;
end;

Class Function CoItems.CreateRemote(const MachineName: string): IItems;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Items) as IItems;
end;

Class Function CoDeviceEvent.Create: IDeviceEvent;
begin
  Result := CreateComObject(CLASS_DeviceEvent) as IDeviceEvent;
end;

Class Function CoDeviceEvent.CreateRemote(const MachineName: string): IDeviceEvent;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceEvent) as IDeviceEvent;
end;

Class Function CoDeviceEvents.Create: IDeviceEvents;
begin
  Result := CreateComObject(CLASS_DeviceEvents) as IDeviceEvents;
end;

Class Function CoDeviceEvents.CreateRemote(const MachineName: string): IDeviceEvents;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceEvents) as IDeviceEvents;
end;

Class Function CoDeviceInfo.Create: IDeviceInfo;
begin
  Result := CreateComObject(CLASS_DeviceInfo) as IDeviceInfo;
end;

Class Function CoDeviceInfo.CreateRemote(const MachineName: string): IDeviceInfo;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceInfo) as IDeviceInfo;
end;

Class Function CoDeviceInfos.Create: IDeviceInfos;
begin
  Result := CreateComObject(CLASS_DeviceInfos) as IDeviceInfos;
end;

Class Function CoDeviceInfos.CreateRemote(const MachineName: string): IDeviceInfos;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceInfos) as IDeviceInfos;
end;

Class Function CoDevice.Create: IDevice;
begin
  Result := CreateComObject(CLASS_Device) as IDevice;
end;

Class Function CoDevice.CreateRemote(const MachineName: string): IDevice;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Device) as IDevice;
end;

Class Function CoCommonDialog.Create: ICommonDialog;
begin
  Result := CreateComObject(CLASS_CommonDialog) as ICommonDialog;
end;

Class Function CoCommonDialog.CreateRemote(const MachineName: string): ICommonDialog;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_CommonDialog) as ICommonDialog;
end;

Class Function CoDeviceManager.Create: IDeviceManager;
begin
  Result := CreateComObject(CLASS_DeviceManager) as IDeviceManager;
end;

Class Function CoDeviceManager.CreateRemote(const MachineName: string): IDeviceManager;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DeviceManager) as IDeviceManager;
end;

constructor TEvsDeviceManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoDeviceManager.Create;
  Connect(fServer,_IDeviceManagerEvents);
end;

procedure TEvsDeviceManager.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    1: if assigned(OnOnEvent) then
          OnOnEvent(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));

  end;
end;

end.
