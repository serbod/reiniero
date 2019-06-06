Paper Tiger
===========
Scanning, text recognition and archiving of paper documents... 
with GUI clients but from the command line if necessary 

The Paper Tiger code has a liberal MIT license. 
It uses various other open-source programs.

Functionality
=============
Architecture/functionality:
- scanning documents using sane into TIFF documents
- OCR/text recognition using Tesseract
- storage of documents as PDF file (image file+OCR text) e.g. on a Samba share
- index of documents+notes+full text in Firebird database
- server component written in FreePascal, so no X Windows required.
- command line control on server
- CGI REST server component
- Viewer/scanner GUI written in Lazarus+FreePascal
- Initial support for WIA/TWAIN on Windows to support scanning from desktop


Further possible refinements:
- support for other databases (sqlite, PostgreSQL, MS SQL Server)
- using image cleanup tools such as scantailor and unpaper
- write .deb install pacakge for easy installation on Debian servers
- batch import of images/pdfs

Architecture and development principles
- use other people's work if possible - the Unix way...
- if possible, build using modules: 
  e.g. allow use of multiple OCR engines etc
- store OCR text in the PDF, and store the image tiff.
  This enables external tools to work with the PDFs,
  use the PDFs in other applications etc.
- save all OCR text in database or file 
  (e.g. a Lucene index) in order to allow fast search across all documents
- this means synchronizing PDF text with the full text archive may be required
- develop towards a single point of control: 
  tigerservercore, which may speak multiple protocols, e.g. via plugins
- however, use standard methods of storing data (e.g. full text search 
  components), normalized database schema in order to allow programs/tools that 
	don't speak the protocols mentioned above to get data easily
- these 2 principles clash; the code will need to stabilize until it is wise to 
  directly try to access e.g. the database.
	Even then, breaking changes will not be avoided if e.g. cleanness of design 
	would be compromised

Compilation instructions
========================
FPC 2.7.1/trunk is preferred for the server/CGI programs.
At least FPC 2.6.2 fpweb does not accept the DELETE method.
For the client program, Lazarus trunk has been used for development.

1. Compile hgversion.pas, e.g.:
fpc hgversion.pas

2. Compile the program(s) you want
2.1 With Lazarus:
lazbuild tigercgi.lpi
lazbuild tigerclient.lpi
lazbuild tigerserver.lpi
2.2 With FreePascal:
- Run hgversion first to update the version info
fpc -dCGI tigercgi.lpr
fpc tigerserver.lpr

Installation instructions
=========================
- prerequisites: Linux/*nix (virtual) machine. Windows support may come later.
- prerequisites: have sane installed and configured for your scanner. E.g.:
  aptitude install sane-utils
- prerequisites: have tesseract installed and configured. E.g.:
  aptitude install tesseract-ocr tesseract-ocr-eng #for English language support
  Note: we need version 3 because of hOCR support needed for getting searchable 
	PDFs.
- prerequisites: have exactimage installed (for hocr2pdf), e.g.:
  aptitude install exactimage
- Tesseract must/can then be configured to output hocr, e.g.:
  check you have this file present (adjust config directory to your situation):
  cat /usr/local/share/tessdata/configs/hocr
  If not (again, adjust config file location to your situation):
  cat >> /usr/local/share/tessdata/configs/hocr << "EOF_DOCUMENT"
  tessedit_create_hocr 1
  EOF_DOCUMENT
- prerequisites: have pdftk installed (for concatenating pdfs), e.g.:
  aptitude install pdftk
- nice to have: have scantailor installed (for aligning/cleaning up the tiff 
  images before OCR).
  see installation notes below
	
  
Installing the command line server:
- copy hocrwrap.sh to server directory (e.g. /opt/tigerserver/)
- copy scanwrap.sh to server directory
- copy tigerserver to server directory
- go to the server directory and make files executable, e.g. (replace 
  directory with your own if necessary):
  cd /opt/tigerserver/
  chmod u+rx hocrwrap.sh
  chmod u+rx scanwrap.sh
  chmod u+rx tigerserver
- copy tigerserver.ini.template to tigerserver.ini and edit settings to match 
  your environment

Test by running ./tigerserver --help

Installing the cgi application:
- prerequisites: apache2 or another HTTP server that supports cgi
  aptitude install apache2
-	copy tigercgi to cgi directory (e.g. /usr/lib/cgi-bin). 
	Make sure the user Apache runs under may read and execute the file (e.g. 
	chmod ugo+rx tigercgi)
- copy hocrwrap.sh to cgi directory (e.g. /usr/lib/cgi-bin/)
- copy scanwrap.sh to cgi directory
- copy tigercgi to cgi directory
- copy tigerserver.ini.template to tigerserver.ini in the cgi directory and edit
  settings to match your environment
- go to the cgi directory and make files executable for the apache/www user, 
  e.g. (replace directory with your own if necessary):
  cd /usr/lib/cgi-bin/
	# replace user/groups below with correct user/group if needed, e.g. apache2
  chown www-data:www-data hocrwrap.sh 
  chown www-data:www-data scanwrap.sh
  chown www-data:www-data tigercgi
  chown www-data:www-data tigerserver.ini
	# make scripts executable:
  chmod u+rx hocrwrap.sh
  chmod u+rx scanwrap.sh
  chmod u+rx tigercgi
  chmod u+r  tigerserver.ini
  
Installing the client:
- prerequisites: *nix: imagemagick dev libraries installed: e.g. 
  aptitude install imagemagick
- prerequisites: Windows: imagemagick DLLs e.g. Q16 x86 or x64 (depending on 
  papertiger client bitness) version downloaded from 
	http://www.imagemagick.org/script/binary-releases.php in client directory or 
	in path
- compilation without imagemagick is possible (see source code for compiler 
  define) but the program will be much slower
- copy tigerclient.ini.template to tigerclient.ini and edit settings to match 
  your environment


Building Tesseract 3
====================
If tesseract 3 is not available for your platform, you will need to build it.  
Preliminary notes for building Tesseract 3 on Debian aqueeze
sources:
http://ubuntuforums.org/showthread.php?t=1647350
aptitude install build-essential leptonica libleptonica-dev libpng-dev 
libjpeg-dev libtiff-dev zlib1g-dev

# as root:
cd ~
wget https://tesseract-ocr.googlecode.com/files/tesseract-3.01.tar.gz
tar -zxvf tesseract-3.01.tar.gz
cd tesseract-3.01
./runautoconf
./configure
make
checkinstall #follow the prompts and type "y" to create documentation directory.
# Enter a brief description then press enter twice
ldconfig

#language/training data, e.g. for Dutch and English:
#todo: check dir
cd /usr/local/share/tessdata
wget https://tesseract-ocr.googlecode.com/files/nld.traineddata.gz
gunzip nld.traineddata.gz
wget https://tesseract-ocr.googlecode.com/files/tesseract-ocr-3.01.eng.tar.gz
gunzip tesseract-ocr-3.01.eng.tar.gz

Building scantailor from source
===============================
Scantailor is being developed; we use the scantailor enhanced fork.
http://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
Build instructions:
https://sourceforge.net/apps/mediawiki/scantailor/index.php?title=Building_from_source_code_on_Linux_and_Mac_OS_X

Notes for Debian below.

# get compilers and dependencies
aptitude install build-essential cmake libqt4-dev libjpeg-dev zlib1g-dev \
libpng-dev libtiff-dev libtiff5-alt-dev  libboost-dev libxrender-dev \
#libtiff5-alt-dev for good measure; hope it improves tiff support

Get source from git repository:
cd ~
git clone git://git.code.sf.net/p/scantailor/code scantailor
cd scantailor
git checkout enhanced #check out branch called "enhanced"
cmake .
make
su - #switch to root
cd /home/pascaldev/scantailor #or wherever the files are located
make install
exit #out of root

Miscellaneous notes
===================
Getting PDF viewers to open a certain page:
Adobe Acrobat Reader
http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/pdf_open_parameters.pdf
acrobat.exe /A "page=<pageNo>"
could also use "nameddest=<named destination>"

SumatraPDF
https://code.google.com/p/sumatrapdf/wiki/CommandLineArguments
sumatrapdf -reuse-instance -page <pageNo>
Scrolls the first indicated file to the indicated page.
Tells an already open SumatraPDF to load the indicated files. If there are 
several running instances, behaviour is undefined.

ImageMagick DLLs on Windows
===========================
The following dlls seem sufficient for converting TIFF images for the client - 
I just copied all dlls:
CORE_RL_bzlib_.dll
CORE_RL_jbig_.dll
CORE_RL_jp2_.dll
CORE_RL_jpeg_.dll
CORE_RL_lcms_.dll
CORE_RL_libxml_.dll
CORE_RL_Magick++_.dll
CORE_RL_magick_.dll
CORE_RL_png_.dll
CORE_RL_tiff_.dll
CORE_RL_ttf_.dll
CORE_RL_wand_.dll
CORE_RL_xlib_.dll
CORE_RL_zlib_.dll
X11.dll
Xext.dll

In modules\coders (just copied all dlls)
IM_MOD_RL_aai_.dll
IM_MOD_RL_art_.dll
IM_MOD_RL_avs_.dll
IM_MOD_RL_bgr_.dll
IM_MOD_RL_bmp_.dll
IM_MOD_RL_braille_.dll
IM_MOD_RL_cals_.dll
IM_MOD_RL_caption_.dll
IM_MOD_RL_cin_.dll
IM_MOD_RL_cip_.dll
IM_MOD_RL_clipboard_.dll
IM_MOD_RL_clip_.dll
IM_MOD_RL_cmyk_.dll
IM_MOD_RL_cut_.dll
IM_MOD_RL_dcm_.dll
IM_MOD_RL_dds_.dll
IM_MOD_RL_debug_.dll
IM_MOD_RL_dib_.dll
IM_MOD_RL_djvu_.dll
IM_MOD_RL_dng_.dll
IM_MOD_RL_dot_.dll
IM_MOD_RL_dps_.dll
IM_MOD_RL_dpx_.dll
IM_MOD_RL_emf_.dll
IM_MOD_RL_ept_.dll
IM_MOD_RL_exr_.dll
IM_MOD_RL_fax_.dll
IM_MOD_RL_fd_.dll
IM_MOD_RL_fits_.dll
IM_MOD_RL_fpx_.dll
IM_MOD_RL_gif_.dll
IM_MOD_RL_gradient_.dll
IM_MOD_RL_gray_.dll
IM_MOD_RL_hald_.dll
IM_MOD_RL_hdr_.dll
IM_MOD_RL_histogram_.dll
IM_MOD_RL_hrz_.dll
IM_MOD_RL_html_.dll
IM_MOD_RL_icon_.dll
IM_MOD_RL_info_.dll
IM_MOD_RL_inline_.dll
IM_MOD_RL_ipl_.dll
IM_MOD_RL_jbig_.dll
IM_MOD_RL_jnx_.dll
IM_MOD_RL_jp2_.dll
IM_MOD_RL_jpeg_.dll
IM_MOD_RL_label_.dll
IM_MOD_RL_mac_.dll
IM_MOD_RL_magick_.dll
IM_MOD_RL_map_.dll
IM_MOD_RL_matte_.dll
IM_MOD_RL_mat_.dll
IM_MOD_RL_meta_.dll
IM_MOD_RL_miff_.dll
IM_MOD_RL_mono_.dll
IM_MOD_RL_mpc_.dll
IM_MOD_RL_mpeg_.dll
IM_MOD_RL_mpr_.dll
IM_MOD_RL_msl_.dll
IM_MOD_RL_mtv_.dll
IM_MOD_RL_mvg_.dll
IM_MOD_RL_null_.dll
IM_MOD_RL_otb_.dll
IM_MOD_RL_palm_.dll
IM_MOD_RL_pango_.dll
IM_MOD_RL_pattern_.dll
IM_MOD_RL_pcd_.dll
IM_MOD_RL_pcl_.dll
IM_MOD_RL_pcx_.dll
IM_MOD_RL_pdb_.dll
IM_MOD_RL_pdf_.dll
IM_MOD_RL_pes_.dll
IM_MOD_RL_pict_.dll
IM_MOD_RL_pix_.dll
IM_MOD_RL_plasma_.dll
IM_MOD_RL_png_.dll
IM_MOD_RL_pnm_.dll
IM_MOD_RL_preview_.dll
IM_MOD_RL_ps2_.dll
IM_MOD_RL_ps3_.dll
IM_MOD_RL_psd_.dll
IM_MOD_RL_ps_.dll
IM_MOD_RL_pwp_.dll
IM_MOD_RL_raw_.dll
IM_MOD_RL_rgb_.dll
IM_MOD_RL_rla_.dll
IM_MOD_RL_rle_.dll
IM_MOD_RL_scr_.dll
IM_MOD_RL_sct_.dll
IM_MOD_RL_sfw_.dll
IM_MOD_RL_sgi_.dll
IM_MOD_RL_stegano_.dll
IM_MOD_RL_sun_.dll
IM_MOD_RL_svg_.dll
IM_MOD_RL_tga_.dll
IM_MOD_RL_thumbnail_.dll
IM_MOD_RL_tiff_.dll
IM_MOD_RL_tile_.dll
IM_MOD_RL_tim_.dll
IM_MOD_RL_ttf_.dll
IM_MOD_RL_txt_.dll
IM_MOD_RL_uil_.dll
IM_MOD_RL_url_.dll
IM_MOD_RL_uyvy_.dll
IM_MOD_RL_vicar_.dll
IM_MOD_RL_vid_.dll
IM_MOD_RL_viff_.dll
IM_MOD_RL_wbmp_.dll
IM_MOD_RL_webp_.dll
IM_MOD_RL_wmf_.dll
IM_MOD_RL_wpg_.dll
IM_MOD_RL_xbm_.dll
IM_MOD_RL_xcf_.dll
IM_MOD_RL_xc_.dll
IM_MOD_RL_xpm_.dll
IM_MOD_RL_xps_.dll
IM_MOD_RL_xtrn_.dll
IM_MOD_RL_xwd_.dll
IM_MOD_RL_x_.dll
IM_MOD_RL_ycbcr_.dll
IM_MOD_RL_yuv_.dll

in modules\filters
analyze.dll