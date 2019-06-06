program tigercgi;

{$mode objfpc}{$H+}

uses
  fpCGI, tigerutil, tigercgimain,
  { Modules that handle of URLs:}
  tigercgi_server, tigercgi_document, tigercgi_image,
  tigerservercore, tigerdb, scan, ocr, imagecleaner;

begin
  Application.Initialize;
  Application.AllowDefaultModule:=false; //all URLs must include the module name (image, document..)
  { If there is only 1 element in the URL, like the "something" in
    http://localhost/cgi-bin/something
    treat it as a module by setting PreferModuleName.
    Otherwise it would have been treated like an action.
  }
  Application.PreferModuleName:=true;
  if Application.EventLog=nil then ;  //initialize event log
  Application.Run;
end.

