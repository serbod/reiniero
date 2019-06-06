unit flocatecommon;

{*
Common settings for flocate
======================================================================================
MIT X11 License: no warranties, express or implied, but all other use permitted:
Copyright (c) 2014 copyright holders

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
======================================================================================
*}

{$mode objfpc}

interface

uses
  Classes, SysUtils;

const
  BufSize = 1024 * 1024; //1MB buffer for md5 hashing
  DEFAULTCATALOGDESCRIPTION='Default catalog';
  DEFAULTSCANDESCRIPTION='Default scan';
  UNKNOWNDATETIME: TDateTime = -693593; // Some bogus date representing an unknown date/time, well before earliest file, photograph, or sound recording.

type
  TFileBuffer = array[1..BufSize] of byte; //static array of byte
  PFileBuffer = ^TFileBuffer;

implementation

end.

