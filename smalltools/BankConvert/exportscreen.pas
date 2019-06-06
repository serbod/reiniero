unit exportscreen;
{ Simple/sample export descendant that exports to screen.

  Copyright (c) 2012 Reinier Olislagers

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
  Classes, SysUtils, exportfile, transactioninfo;

type

{ TExportScreen }

TExportScreen = class(TExportFile)
private

public
  procedure ExportFile(Transactions: TTransactionList); override;
  constructor Create;
  destructor Destroy; override;
end;

implementation

{ TExportScreen }

procedure TExportScreen.ExportFile(Transactions: TTransactionList);
var
  Trans: TTransaction;
begin
  for Trans in Transactions do
  begin
    writeln(Trans.Display);
    writeln('');
  end;
end;

constructor TExportScreen.Create;
begin
  inherited Create;
end;

destructor TExportScreen.Destroy;
begin
  inherited Destroy;
end;

end.

