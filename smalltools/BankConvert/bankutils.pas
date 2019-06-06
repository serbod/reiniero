unit bankutils;
{ Utility unit for banking software

  Copyright (c) 2013 Reinier Olislagers

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

interface


uses
  Classes, SysUtils;

// checks if this is or can be a Dutch bank account.
// If so, removes superfluous .s etc.
// If not, return default
//todo: when this grows, use a table or some such for various countries
// also have some detection system or something.
function NormalizeNLAccountDef(Account: string; Default: string): string;


implementation
function ElevenTest(Number: string): boolean;
var
  i:integer;
  Sum: integer=0;
  Digits: integer;
begin
  try
    Digits:=Length(Number);
    for i:=1 to Digits do
      Sum:=(1+Digits-i)*StrToInt(Number[i])+Sum;
    result:=(Sum mod 11=0);
  except
    // E.g. conversion errors
    result:=false;
  end;
end;

function NormalizeNLAccountDef(Account: string; Default: string): string;
// Dutch accounts may be either [1]
//
// - ex Postbank giro accounts. Just a number (apparently 7 digits)
//   sometimes people added a P in front
// However other sources mention 1..7 digits [2]
// - "normal" banks: 9 digits, and must match the "11 proef"
// - [2] mentions 10 digits, too
// [1] http://www.ibannl.org/bankrekeningnummers.php
// [2] http://blafhert.wordpress.com/2011/04/23/elfproef-voor-postbank-rekeningnummers/
var
  IsPostbank: boolean;
  Temp: string;
begin
  //todo: cater for IBAN codes starting with NL
  IsPostbank:=false;
  // Allow . and space in the account number
  Temp:=StringReplace(trim(uppercase(Account)),'.','',[rfReplaceAll]);
  Temp:=StringReplace(Temp,' ','',[rfReplaceAll]);

  // Detect postbank, strip leading P
  if copy(Temp,1,1)='P' then
  begin
    IsPostbank:=true;
    Temp:=Copy(Temp,2,MaxInt);
  end;

  // Strip leading 0s
  while copy(Temp,1,1)='0' do
  begin
    Temp:=copy(Temp,2,MaxInt);
  end;

  case Length(Temp) of
    0: result:=Default;
    1..7: //Giro/postbank account?
      result:=Temp; //todo: make sure only digits
    8: // Assume postbank accounts don't reach this far
      result:=Default; //invalid
    9,10: //Regular bank account?
      if ElevenTest(Temp) then
        result:=Temp
      else
        result:=Default; //invalid
    else
      result:=Default; //invalid
  end;
end;

end.

