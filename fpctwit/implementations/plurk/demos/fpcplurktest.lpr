program fpcplurktest;

{
Demonstration program for the plurk library.

Please make sure you have a fpcplurktest.ini with these contents - of course replace with actual values:
[Settings]
ConsumerKey=<your consumer key>
ConsumerSecret=<your consumer secret>
AuthToken=<your auth token>
AuthSecret=<your auth secret>

Copyright (c) 2013 Mario Ray Mahardhika

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.

}

{$mode objfpc}{$H+}
{$ifdef Windows}
  {$APPTYPE CONSOLE}
{$endif}

uses
  Classes,
  SysUtils,
  plurktesthelper;

begin
  try
    with TPlurkTestHelper.Create do
        try
          MainLoop;
        finally
          Free;
        end;
  except
    on e: EFirstRun do
      WriteLn(e.Message);
  end;
end.
