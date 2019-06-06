{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}

// Whether or not to include test object and debug test code (which outputs to a log file)
{not $DEFINE OAUTHTEST}

// What network library to use. Currently only Synapse is supported
{$DEFINE TRANSPORT_SYNAPSE}


