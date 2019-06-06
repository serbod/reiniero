CheckRide readme
================

1. Help, it doesn't work?
=========================
If you are running CheckRide.exe, your helper probably hasn't completely read this file, so he or she forgot to configure the program correctly.
Symptom: the program shows this line:
Setting up connection to helper Wonko the Sane at changethisline.example.com:33334.
and it seems to work, but your helper cannot take over your screen.
This means that your helper hasn't set up your CheckRide program correctly.

Temporary solution: click Disconnect, ask your helper what to enter in the
Helper server name and
Helper server port fields.
Then press connect.

Permanent solution: let your helper create your own Checkride.conf (see section "5. CheckRide.conf").


2. What is it?
==============
This program allows a person requesting help to let a helper take over her desktop remotely.

It is inspired by earlier scripts (e.g. http://www.runpcrun.com/create_your_own_support_software) that set up a reverse VNC connections, and its name is influenced by Fog Creek's CoPilot.
CheckRide allows remote support without hassle: the helped person just runs the program and waits for the helper to take over.
It sets up an SSL encrypted tunnel and shares the helped person's desktop using VNC to a helper that has a server port open and the helper program listening.
The program for the helped person uses a default address and port to connect to to simplify things.


3. How to use CheckRide.exe
===========================
Start CheckRide.exe. The program will automatically try to connect until you click Disconnect or close the program.
When connecting, tray icons (next to the clock) will appear for the UltraVNC program (eye symbol) and stunnel program (a boxy shape).
CheckRide uses UltraVNC to contact your helper using the stunnel program.
The helper should have started their own program to respond to your connection attempts.
When your helper responds by taking over your screen, the UltraVNC (eye) icon will change color from blue to yellow.
When the connetion is lost, e.g. due to network problems, CheckRide will keep on trying to reconnect.
CheckRide stops UltraVNC and stunnel when disconnecting or closing. You will notice the icons disappear.

You should know enough by now about running CheckRide.
Read on if you're a helper interested in using CheckRideHelper, or a developer.

4. CheckRideHelper
==================
CheckRideHelper.exe is the counterpart for CheckRide.exe: you need to start it on the helper side to take over the helped party's screen.
CheckRideHelper, CheckRide and all other required files (including source) can be installed using CheckRideHelperSetup.exe, see
https://bitbucket.org/reiniero/checkride/downloads
REQUIRED configuration: see next section.

5. Configuration for helper
===========================
You need to have either a fixed external IP address or a fixed DNS name (e.g. www.lazarus.freepascal.com).
Most residential dial-up and broadband (ADSL, cable) connections have a dynamic IP address. In this case, get a free dynamic DNS address via e.g. www.dyndns.org, install the dynamic DNS client and use the host address (e.g. example.dyndns.org) as your hostname when compiling the program.
In your router/modem, forward the relevant port (the one specified as HelperPort in the CheckRide.conf file) to your desktop running CheckRideHelper.exe. See www.portforward.com for further details on port forwarding.

To adjust the host/server name and port Checkride connects to, there are multiple options. The first option is the most elegant and easy for end users:
1. Use CheckRideHelper to modify CheckRide.exe settings: using the Tools...Customize Checkride menu, edit settings.
2. distribute CheckRide.conf with your own settings. These overrule the built-in settings (see below)
3. use command line options to overrule built-in settings and CheckRide.conf, see section below.
4. specify server name and port manually in the CheckRide form (useful for one-off operations).

6. CheckRide.conf
=================
If you place a file CheckRide.conf in the CheckRide application directory, you override the built-in configuration and can specify where the app will connect to.

CheckRideHelper uses CheckRide.conf to determine what port to listen to.

CheckRide.conf should contain something like:
[default]
; Default helper entry
HelperHost=donaldduck.dyndns.org
HelperPort=1337
HelperName=Donald
... where you can customise the HelperHost, HelperPort and HelperName entries.
The host is the host (name or IP address) of the helper's machine.
The port is the TCP port where the helper's CheckRide server listens on.
Obviously, CheckRide.conf contents (at least the port number) need to match both on helper and helped party.

7. Checkride.exe command line switches
======================================
You can overrule the embedded configuration and CheckRide.conf by using command line options. Please use ChecKride -h and CheckRideHelper -h for details.
Some options for CheckRide:
--helperhost <name_or_ip>    : helper host address/IP number
--helperport <portnumber>    : helper port
--helpername <name>          : name to display
--noautoconnect              : don't automatically connect to helper on program startup
--help                       : show command line options


8. CheckRideHelper use
======================
Configuration: see above. 
On start, unless you specify -noautoconnect as a parameter, UltraVNC and stunnel tray icons should appear.
Note: -noautoconnect also works for CheckRide.exe
If you want to change the port on which you can be reached by CheckRide, change the port number in the edit box, press Disconnect and Listen.
You can find out your external IP address by clicking the What is my IP? button. Handy if you want to pass this on to a helped person.


9. Compilation
==============
9.1 Prerequisites
If you want to compile the programs yourself you will need:
Lazarus IDE+FreePascal compiler from www.lazarus.freepascal.org
If you want to modify the installer (CheckRideHelperSetup.iss), you'll need:
Inno Setup (QuickStart Pack recommended): http://www.jrsoftware.org/isdl.php#qsp

Get the source code either:
- with the installer (CheckRideHelperSetup), 
- from the repository at https://bitbucket.org/reiniero/checkride/src

9.2 CheckRide & CheckRideHelper
Edit CheckRide.conf, specifying your own server name and port, and helper name.

When compiling, the Lazarus project executes a program, CheckRideResourceZipper.exe.
This program generates a zip file from CheckRide.conf and the other required executables and files, then adds this zip file to CheckRide.exe.
See the settings in Lazarus, Project, Project Options, Compiler Options, Compilation, Execute after (call on compile, build, run).
If you want to change the files incorporated in CheckRide.exe, modify CheckRideResourceZipper.lpr, recompile it, then build (not compile) CheckRide & CheckRideHelper.

To use your own version of CheckRideHelper, put these files in a directory:
CheckRideHelper.exe
checkride.conf
libeay32.dll
license.txt
readme.txt (this file)
ssleay32.dll
stunnel.pem
ultravnc.ini
vncviewer.exe
zlib1.dll


10. What does this tool use?
============================
The CheckRide and CheckRideHelper programs are wrappers for:
- UltraVNC
- Stunnel
Please see the file License.txt for information on what they are, where to get them and their licenses.


11. License
===========
The CheckRide, CheckRideHelper source code/programs are released as freeware under the MIT license: all use permitted but no liability accepted:

Copyright (C) 2011-2012 by Reinier Olislagers

CheckRide and CheckRideHelper make use of some other programs/components.
Please see the file License.txt for the exact license.


12. Bugs & improvement ideas
============================
12.1 Contributions
I'd be happy to receive bug reports and patches/ideas for improvement via the project's issue tracker:
https://bitbucket.org/reiniero/checkride/issues

12.2 Ideas
These may be acted on, or they may not ;)
- look into installing/using temporary video driver service
- test with existing vnc services
- use stringgrid instead of memo for log output
- nslookup first to get host info
- ping first to get idea of whether host is up (run ping in process, pipe result to screen on linux; run indy or whatever ping on windows)
- replace stunnel with FPC units (synapse?)
- certs in ssl; create them yourself for added security/authentication
- incorporate audio qemu rfb extension if both ends have CheckRide=>telephony over VNC/voice communication
  see: http://www.tigervnc.com/cgi-bin/rfbproto 
- misuse ultravnc chat or something similar (clipboard info?) for a telnet like interface (probably without admin on UAC systems though)
- move vnc/stunnel handling to classes in separate units for reuse
- pre-VNC chat over SSL -> eg measure bandwidth?
