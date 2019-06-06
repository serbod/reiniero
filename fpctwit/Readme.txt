fpctwit is an OAuthv1, Twitter and Plurk library with example programs 
(console and GUI).

NOTE: In the default configuration, you WILL need the openssl libraries, e.g. 
sselay32.dll and libeay32.dll, and presumably zlib1.dll.

It is written for FreePascal but meant to be usable in Delphi as well. 
It currently uses the Synapse network library (included) but can be adapted for 
other network libraries.

Twitter is a major user of the OAuthv1 authentication protocol, but there are 
others.

The Twitter library's functionality includes: getting tweets using the Search 
API, tweeting, getting username info, authenticating using PIN/OOB or preset 
credentials (consumerkey+secret,authtoken+secret).
It supports OAuthv1 secure authentication and TLS encryption. The library uses 
modified FPC fpJSON units to allow getting UTF8 data out of the JSON data with 
Tweets returned from Twitter. For newer FPC versions, it will use the native 
fpJSON units as those support UTF8.

The fpctwit console demo program and fpctwitgui GUI demo programs demonstrate 
the functionality of the Twitter and OAuthv1 libraries.

The fpctwit console demo also uses the Twitter library's Streaming API support 
to process and show Tweets and deletion messages.

The fpcplurktest console demo demonstrates use of the Plurk library 
(plurklib.pp).

The applications and libraries are distributed under a very liberal license 
(my code under MIT license; see Synapse and fpjson units for their licenses) 
which allows commercial use as well as interoperability with GPL programs.

Patches are welcome!

Enjoy,

Reinier Olislagers, Mario Ray Mahardhika and Ludo Brands