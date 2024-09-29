# This works for now 29 sept 2024.
# When "https://fileinfo.com/filetypes/" gets redesigned the script needs to be adopted.
# A more robust parsing is maybe nicer.
# noinspection SpellCheckingInspection
text_set = (
	'SMF' , # StarMath Formula File
	'STY' , # LaTeX Style
	'TNL' , # Super Mario Maker Course Thumbnail
	'MPD' , # MPEG-DASH Media Presentation Description
	'GSITE' , # Google Sites Shortcut
	'BWD' , # BanglaWord Document
	'DOC' , # Microsoft Word Document (Legacy)
	'LST' , # Data List
	'LTXD' , # Light Text Editor Document
	'MAN' , # Unix Manual
	'DOTM' , # Microsoft Word Macro-Enabled Document Template
	'ADOC' , # AsciiDoc Document
	'QBL' , # QuickBooks License File
	'DSC' , # Text Description File
	'FPT' , # FoxPro Table Memo
	'SAM' , # LMHOSTS Sample File
	'UPD' , # Program Update Information
	'EMBED' , # Embed Notes Note
	'DOTX' , # Microsoft Word Template
	'SAVE' , # Nano Temporary Save File
	'ODM' , # OpenDocument Master Document
	'LTX' , # LaTeX Document
	'WTT' , # Write! Document
	'FCF' , # Final Draft Converter File
	'B' , # Brainf*ck Source Code File
	'ORG' , # Emacs Org Text Document
	'DOCX' , # Microsoft Word Document
	'DIZ' , # Description in Zip File
	'MNT' , # FoxPro Menu Memo
	'LXFML' , # LEGO Digital Designer XML File
	'GFORM' , # Google Forms Shortcut
	'STORY' , # Storyist Document
	'ME' , # Readme Text File
	'LUE' , # Norton LiveUpdate Log File
	'FOUNTAIN' , # Fountain Script File
	'ANS' , # ANSI Text File
	'EIO' , # Yozo Office File
	'TMDX' , # TextMaker Document
	'FODT' , # OpenDocument Flat XML Document
	'GPD' , # Generic Printer Description File
	'README' , # Readme File
	'APT' , # Almost Plain Text File
	'VNT' , # Mobile Phone vNote File
	'FBL' , # CADfix Command Level Log File
	'AWW' , # Ability Write Document
	'TEX' , # LaTeX Source Document
	'LOG' , # Log File
	'BF' , # Brainf*ck Source Code File
	'FADEIN.TEMPLATE' , # Fade In Template
	'CEC' , # Studio C Alpha Upgrade File
	'JARVIS' , # Jarvis Subscriber File
	'PWDPL' , # Password Pad Lite Document
	'KLG' , # KOFIA Log
	'IPF' , # OS/2 Help File
	'TXT' , # Plain Text File
	'RPT' , # Generic Report
	'ASC' , # ASCII Text File
	'NFO' , # Warez Information File
	'GDOC' , # Google Docs Shortcut
	'LIS' , # SQR Output File
	'GSD' , # General Station Description File
	'AIM' , # AIMMS ASCII Model File
	'TLB' , # VAX Text Library
	'DROPBOX' , # Dropbox Shared Folder Tracker
	'LST' , # FoxPro Documenting Wizard List
	'OTT' , # OpenDocument Document Template
	'RFT' , # Revisable Form Text Document
	'SDM' , # StarOffice Mail Message
	'SCM' , # Schema File
	'DXB' , # Duxbury Braille File
	'OPEICO' , # Opeico Text File
	'IPYNB' , # Jupyter Notebook
	'TEXT' , # Plain Text File
	'DOCM' , # Microsoft Word Macro-enabled Document
	'WPD' , # WordPerfect Document
	'TMVX' , # TextMaker Document Template
	'EMULECOLLECTION' , # eMule Data File
	'HS' , # Java HelpSet File
	'BIB' , # Bibliography Document
	'_DOCX' , # Renamed Microsoft Word Document
	'1ST' , # Readme File
	'ASC' , # Autodesk ASCII Export File
	'MD5.TXT' , # Message Digest 5 Hash File
	'WPS' , # Microsoft Works Word Processor Document
	'RTF' , # Rich Text Format File
	'BIB' , # BibTeX Bibliography Database
	'ATY' , # Association Type Placeholder
	'ERR' , # Error Log File
	'GSCRIPT' , # Google Apps Script Shortcut
	'STW' , # StarOffice Document Template
	'DTEX' , # DataTex Document
	'BDR' , # Exchange Non-Delivery Report Body File
	'TM' , # TeXmacs Document
	'ETF' , # ENIGMA Transportable File
	'WPW' , # WordPerfect Works Document
	'XY' , # XYWrite Document
	'DM' , # BYOND Dream Maker Code
	'JNP' , # Java Web Start File
	'FLUID' , # Loop Component
	'KNT' , # KeyNote Note File
	'LUF' , # Lipikar Uniform Format File
	'TFRPROJ' , # theFrame Project File
	'RAD' , # Radar ViewPoint Radar Data
	'ODT' , # OpenDocument Text Document
	'RIS' , # Research Information Systems Citation File
	'GTABLE' , # Google Fusion Table Shortcut
	'PAGES' , # Apple Pages Document
	'TEXTCLIPPING' , # Mac OS X Text Clipping File
	'RST' , # reStructuredText File
	'COD' , # Atlantis Word Processor Encrypted Document
	'STRINGS' , # Text Strings File
	'RTX' , # Rich Text Document
	'DOCZ' , # ThinkFree Online Note Document
	'COPF' , # Copy Operation File
	'LICENSE' , # Software License File
	'WPS' , # Kingsoft Writer Document
	'FAQ' , # Frequently Asked Questions Document
	'KLG' , # Log File
	'FDX' , # Final Draft Document
	'SCC' , # Scenarist Closed Caption File
	'EML' , # E-Mail Message
	'MSG' , # Outlook Message Item File
	'FDT' , # Final Draft 5-7 Template
	'GJAM' , # Google Jamboard Shortcut
	'SE' , # Shuttle Document
	'RUN' , # Runscanner Scan File
	'KES' , # Kurzweil 3000 Document
	'OMFL' , # Open Multiple Files File List
	'TAB' , # Tab Separated Data File
	'TMD' , # TextMaker Document
	'GMAP' , # Google My Maps Shortcut
	'MBOX' , # Email Mailbox
	'LP2' , # iLEAP Word Processing Document
	'SAF' , # SafeText File
	'WRI' , # Microsoft Write Document
	'DFTI' , # FlexiWrite Document
	'CHARSET' , # Character Set
	'APPODEAL' , # Appodeal Text File
	'LNK42' , # Windows 93 Desktop Shortcut
	'FDR' , # Final Draft Document
	'GSLIDES' , # Google Slides Shortcut
	'BIBTEX' , # BibTeX Bibliography Database
	'SGM' , # SGML File
	'BEAN' , # Bean Rich Text Document
	'BDP' , # Exchange Diagnostic Message
	'ETX' , # Structure Enhanced Text (Setext) File
	'SLA' , # Scribus Document
	'UOT' , # Uniform Office Document
	'SXW' , # StarOffice Writer Document
	'UTF8' , # Unicode UTF8-Encoded Text Document
	'WPT' , # WordPerfect Template
	'DCA' , # DisplayWrite Document
	'IPSPOT' , # iPhoto Spot File
	'MD' , # MuseData Musical Score
	'QDL' , # QDL Program
	'DX' , # DEC WPS Plus File
	'FRT' , # FoxPro Report Memo
	'BAD' , # Exchange Badmail File
	'DVI' , # Device Independent Format File
	'U3I' , # U3 Application Information File
	'NGLOSS' , # Nisus Writer Glossary
	'ODIF' , # Open Document Interchange Format
	'HZ' , # Chinese (Hanzi) Text
	'CAST' , # Asciicast Terminal Recording
	'JIS' , # Japanese Industry Standard Text
	'SHIM' , # Scoop Shim File
	'XWP' , # XMLwriter Project
	'RVF' , # RichView Format File
	'NOTE' , # Notability Note File
	'ODO' , # Online Operating System Write Document
	'602' , # Text602 Document
	'RTFD' , # Rich Text Format Directory File
	'P7S' , # Digitally Signed Email Message
	'NDOC' , # Naver Word 
	'VWR' , # Velconoe Write Document
	'EMLX' , # Apple Mail Message
	'XWP' , # Xerox Writer Text Document
	'PSW' , # Pocket Word Document
	'SDOC' , # Satra Khmer Document
	'SUBLIME-PROJECT' , # Sublime Text Project File
	'EIT' , # Yozo Office Template File
	'TRELBY' , # Trelby File
	'PWD' , # Pocket Word Document
	'SXG' , # Apache OpenOffice Master Document
	'CHORD' , # Song Chords File
	'DOCXML' , # Microsoft Word XML Document
	'HWP' , # Hangul Text Document
	'XYW' , # XyWrite for Windows Document
	'IDX' , # Outlook Express Mailbox Index File
	'SMS' , # Exported SMS Text Message
	'PIMX' , # Adobe Package Installation Management File
	'VCF' , # Variant Call Format File
	'HWP' , # Hanword Document
	'PLAIN' , # Plain Text File
	'ABW' , # AbiWord Document
	'PWI' , # Pocket Word Document
	'PRT' , # Printer Output File
	'HBK' , # Mathcad Handbook File
	'NOW' , # Readme File
	'ORT' , # Rich Text Editor Document
	'LATEX' , # LaTeX Document
	'BML' , # Braille 2000 Braille File
	'JP1' , # Japanese (Romaji) Text File
	'WPD' , # 602Text Word Processing Document
	'XY3' , # XyWrite III Document
	'ZZS' , # Zyzzyva Search
	'PBJ' , # Perfect Backup Job File
	'TPC' , # Topic Connection Placeholder
	'SLA.GZ' , # Scribus Compressed Document
	'NJK' , # Nunjucks Template
	'BLOONSET' , # Bloons Tower Defense Battles Bloonset
	'JLQM' , # LG QuickMemo Info File
	'DAT' , # Geometry Dash Saved Data
	'HIGHLAND' , # Highland Document
	'CALCA' , # Calca Document
	'PAGES-TEF' , # Pages iCloud Document
	'SCRIVX' , # Scrivener XML Document
	'MWD' , # Mariner Write Document
	'TYP' , # Typst Source Document
	'BNA' , # Barna Word Processor Document
	'NOTES' , # Memento Notes File
	'RZN' , # Red Zion Notes File
	'HHT' , # Help and Support Center HHT File
	'MWP' , # Lotus Word Pro SmartMaster File
	'MELLEL' , # Mellel Document
	'BOC' , # EasyWord Big Document
	'LBT' , # FoxPro Label Memo
	'UNX' , # Unix Text File
	'YTDL' , # Youtube-dl Download Progress File
	'SCRIV' , # Scrivener Document
	'GRAPH' , # GRAPH Hierarchical Data File
	'WP' , # WordPerfect Document
	'WP7' , # WordPerfect 7 Document
	'WP4' , # WordPerfect 4 Document
	'_DOC' , # Renamed Microsoft Word Document
	'ZRTF' , # Nisus Compressed Rich Text File
	'SAFETEXT' , # SafeText File
	'XDL' , # Oracle Expert Definition Language File
	'MW' , # MacWrite Text Document
	'FDF' , # Acrobat Forms Data Format
	'FLR' , # Flare Decompiled ActionScript File
	'LWP' , # Lotus Word Pro Document
	'WPT' , # Kingsoft Writer Template
	'BTD' , # Business-in-a-Box Document
	'SAM' , # Ami Pro Document
	'XBDOC' , # Xiosis Scribe Document
	'UTXT' , # Unicode Text File
	'JOE' , # JOE Document
	'MELL' , # Mellel Word Processing File
	'UOF' , # Uniform Office Document
	'PU' , # PlantUML File
	'ACT' , # FoxPro Documenting Wizard Action Diagram
	'WPL' , # DEC WPS Plus Text Document
	'PFX' , # First Choice Word Processing Document
	'LYX' , # LyX Document
	'WPD' , # ACT! 2 Word Processing Document
	'PVM' , # Photo Video Manifest File
	'NB' , # Nota Bene File
	'CRWL' , # Windows Crawl File
	'EUC' , # Extended Unix Code File
	'WBK' , # WordPerfect Workbook
	'OFL' , # Ots File List
	'JTD' , # JustSystems Ichitaro Document
	'DGS' , # Dagesh Pro Document
	'SESSION' , # Mozilla Firefox Session File
	'ASCII' , # ASCII Text File
	'AWT' , # AbiWord Template
	'PLANTUML' , # PlantUML File
	'UNAUTH' , # SiteMinder Unauthorized Message File
	'FFT' , # Final Form Text File
	'GPN' , # GlidePlan Map Document
	'WP6' , # WordPerfect 6 Document
	'TDF' , # Guide Text Definition File
	'CNM' , # NoteMap Outline File
	'XBPLATE' , # Xiosis Scribe Template
	'PRT' , # Crypt Edit Protected Text Format File
	'SP1' , # Windows XP Service Pack 1 Identification File
	'AWP' , # Ability Write Template
	'RTD' , # RagTime Document
	'FWDN' , # fWriter Document
	'TVJ' , # TrueView Job Ticket
	'PMO' , # Pegasus Saved Message File
	'PWR' , # PowerWrite Document
	'MSS' , # CartoCSS Map Stylesheet
	'QUID' , # Legacy QuidProQuo Document
	'LTR' , # Letter File
	'GV' , # Graphviz DOT File
	'OPENBSD' , # OpenBSD Readme File
	'SDW' , # StarOffice Writer Text Document
	'NJX' , # NJStar Document
	'DWD' , # DavkaWriter File
	'XYP' , # XYWrite III Plus Document
	'VCT' , # Visual Class Library Memo
	'RZK' , # File Crypt Password File
	'WTX' , # Text Document
	'ZW' , # Chinese Text File
	'XWP' , # Crosstalk Session File
	'QPQD' , # QuidWriter Document
	'DEL' , # Delimited ASCII File
	'WP5' , # WordPerfect 5 Document
	'NWCTXT' , # NoteWorthy Composer Text File
	'SCT' , # FoxPro Form Memo
	'BTXT' , # BTXTPad Document
	'LYT' , # TurboTax Install Log File
	'OCR' , # FAXGrapper Fax Text File
	'TMV' , # TextMaker Template
	'DESCRIPTION' , # Youtube-dl Video Description
	'WPA' , # ACT! Word Processing Document
	'BBS' , # Bulletin Board System Text
	'TDF' , # Xserve Test Definition File
	'DNE' , # Netica Text File
	'ASE' , # Autodesk ASCII Scene Export File
	'EBP' , # Express Burn Project
	'EMF' , # Jasspa MicroEmacs Macro File
	'SFX' , # Blood Sound Properties
	'VW' , # Volkswriter Text File
	'XDL' , # XML Schema File
	'WSD' , # WordStar Document
	'KWD' , # KWord Document
	'CYI' , # Clustify Input File
	'PWDP' , # Password Pad Document
	'UDF' , # UYAP Document Format
	'IIL' , # CleanSweep Installation Log
	'VPDOC' , # VoodooPad Document
	'DAT' , # Prison Architect Asset Archive
	'NWM' , # Nisus Macro
	'LOOP' , # Loop Component
	'BRX' , # Beam Report Document
	'DXP' , # Duxbury Print File
	'BXT' , # Balabolka Text Document
	'FGS' , # Fig Figure Settings File
	'PVJ' , # ProofVision Job Ticket
	'NWP' , # Now Contact WP Document
	'SUBLIME-WORKSPACE' , # Sublime Text Workspace File
	'ODP' , # Organ Definition Project
	'PTNX' , # EasyBeadPatterns Pattern
	'QPF' , # QuickPad Encrypted Document
	'CWS' , # Claris Works Template
	'ZZQ' , # Zyzzyva Quiz
	'LNT' , # Laego Note Taker File
	'FLF' , # FIGlet Font File
	'MAILSIGNATURE' , # Apple Mail Email Signature
	'SGT' , # ShareGate Template
	'FDXT' , # Final Draft 8 Template
	'WG' , # WordGrinder Document Set
	'MELTEM' , # Mellel Template
	'WEBDOC' , # Box.net Web Document
	'TID' , # TiddlyWiki Tiddler
	'GTHR' , # Gather Log File
	'JRTF' , # JAmes OS Rich Text File
	'MIN' , # Mint Source File
	'MML' , # Map Markup Language File
	'ARC' , # WWE 2K18 and 2K19 Cache File
	'SCW' , # Movie Magic Screenwriter Document
	'PDPCMD' , # Pdplayer Command File
	'WN' , # WriteNow Document
	'FDS' , # Final Draft Secure Copy
	'SW3' , # Scriptware Screenplay
	'SKCARD' , # Starfish Sidekick Card File
	'GMD' , # GroupMail Message
	'MCW' , # MacWrite II Document
	'DOX' , # MultiMate Document
)

# noinspection SpellCheckingInspection
data_set = (
	'TT20' , # TurboTax Canada 2020 Tax Return
	'LDF' , # SQL Server Transaction Log File
	'TXD' , # Game Texture Dictionary
	'MDL' , # MathWorks Simulink Model
	'VMT' , # Valve Material File
	'XEM' , # PowerDesigner Model Definition File
	'BLD' , # Envisioneer Building Project File
	'FLT' , # Flight Simulator Saved Flight
	'HYV' , # Team Manager Meet Event File
	'NITF' , # National Imagery Transmission Format File
	'PPTX' , # Microsoft PowerPoint Presentation
	'RSV' , # RSView32 Project File
	'ADX' , # Approach Index File
	'MET' , # eMule Resource File
	'DAT' , # Data File
	'PPSM' , # Microsoft PowerPoint Macro-Enabled Show
	'TDB' , # Android Thumbnail Cache
	'OBB' , # Android Opaque Binary Blob File
	'RGO' , # RepliGo File
	'PLW' , # PicoLog Data File
	'TRK' , # CompeGPS Land Track File
	'QUICKENDATA' , # Quicken Essentials for Mac Data File
	'POI' , # Magellan Maestro Point of Interest File
	'MOSAIC' , # MacOSaiX Mosaic File
	'KDC' , # Kaspersky Virus Database File
	'WTB' , # Win-Test Binary Log
	'STYK' , # Stykz Stick Figure
	'ABCD' , # AudioVisual Book Data File
	'NOT' , # Notation File
	'DM2' , # Toad Data Modeler 2 File
	'KPR' , # KPresenter Presentation
	'NCORX' , # Adobe Encore Project
	'PRDX' , # Presentations Document
	'CAPT' , # Capito Data File
	'BGT' , # Graphics Accounts Data File
	'MMC' , # Microsoft Media Catalog
	'WJR' , # ReGet Deluxe Data File
	'AIFB' , # AIF Builder Project File
	'T05' , # TaxCut 2005 Tax Return
	'T20' , # H&R Block 2020 Tax Return
	'EBUILD' , # Portage eBuild Script
	'LDIF' , # LDAP Data Interchange Format File
	'TAX2010' , # TurboTax 2010 Tax Return
	'VOK' , # PocketVok Vocabulary File
	'SQR' , # SQL Program File
	'ALD' , # Dynamics AX Application Label Data File
	'FSC' , # Practical Scriptwriter File
	'QB2017' , # QuickBooks 2017 for Mac Data File
	'HDF' , # Hierarchical Data Format (Version 4)
	'TAX2023' , # TurboTax 2023 Tax Return
	'FLO' , # RFFlow Flowchart File
	'DRL' , # Gerber Drill Rack File
	'SQ' , # Sysquake Program
	'TA9' , # TaxAct 2019 Tax Return File
	'H17' , # H&R Block Canada 2017 Tax Return
	'H13' , # H&R Block Canada 2013 Tax Return
	'RAL' , # WinRail Track Design File
	'AZZ' , # AZZ Cardfile Database File
	'IGC' , # International Gliding Commission Data File
	'PPT' , # Microsoft PowerPoint Presentation (Legacy)
	'TOPC' , # TopicCrunch Project File
	'XFT' , # Adobe Form Designer 5.0 File
	'BIN' , # Generic Binary File
	'GGB' , # GeoGebra Worksheet
	'PRS' , # Harvard Graphics Presentation
	'LOC' , # GPS Location File
	'ADT' , # ACT! Document Template
	'CUB' , # Analysis Services Cube File
	'TMX' , # Translation Memory Exchange File
	'CEL' , # Affymetrix Probe Results File
	'MDF' , # Measurement Data Format File
	'BDIC' , # Chrome Dictionary File
	'MENC' , # Windows Mobile Encrypted File
	'FCPEVENT' , # Final Cut Pro Event File
	'DIF' , # Data Interchange Format
	'JEF' , # Janome Embroidery Format File
	'MLS' , # Milestones Simplicity File
	'TBL' , # StarCraft Information Table
	'OTLN' , # Opal Outline File
	'CDX' , # Compound Index File
	'BLG' , # Windows Binary Performance Log File
	'EXX' , # IBM Linkway MsgPut File
	'CONTACT' , # Windows Contact File
	'EDI' , # Electronic Data Interchange File
	'VCS' , # vCalendar Event
	'PC' , # Personal Composer File
	'PPS' , # Microsoft PowerPoint Slide Show (Legacy)
	'DPN' , # Depiction Data File
	'OVF' , # Open Virtualization File
	'ALI' , # Dynamics AX Label Index File
	'ODP' , # OpenDocument Presentation
	'PCB' , # Printed Circuit Board Design File
	'RFA' , # Revit Family File
	'ROX' , # Roxio Project File
	'CLP' , # CrazyTalk Clip File
	'ZMC' , # ZoneAlarm Quarantine File
	'PKA' , # Packet Tracer Activity File
	'OPJU' , # Origin Unicode Project
	'VDF' , # AntiVir Virus Definitions File
	'T12' , # At Home 2012 Tax Return
	'LMS' , # LenMus Score File
	'JPH' , # JProbe Memory Snapshot
	'IP' , # IconPackager Theme File
	'MAI' , # Microsoft Mail File
	'LCM' , # Lipikar Custom Map File
	'WAB' , # Windows Address Book
	'ENL' , # EndNote Library
	'GRF' , # DPlot Graph File
	'QRP' , # QuickReport File
	'NET' , # netViz Project File
	'GRADE' , # GradeStat Document
	'ROADTRIP' , # Road Trip Planner File
	'TAX2009' , # TurboTax 2009 Tax Return
	'QUICKEN2015' , # Quicken 2015 Data File
	'TTK' , # Catalyst Translation Toolkit
	'NDX' , # dBASE Index (Legacy)
	'LIVEREG' , # Symantec Antivirus Session File
	'DOCKZIP' , # ObjectDock File
	'ZAP' , # ZoneAlarm Pro Data File
	'VSX' , # Visio Stencil XML File
	'TDL' , # Tab Delineated Format File
	'UWL' , # User Word List File
	'PHOTOSLIBRARY' , # Photos Library
	'INK' , # Mimio Notebook
	'TAR' , # Tape Archive
	'PRJ' , # AIMMS Project File
	'TRD' , # TrID Definitions Package
	'DCMD' , # DiskCatalogMaker Thumbnail-Enabled Catalog
	'FOB' , # Dynamics NAV Object Container File
	'SDS' , # OpenOffice Chart
	'CKT' , # CircuitMaker File
	'XLF' , # XLIFF Document
	'TCC' , # TimeCalc Classic Data File
	'JNT' , # Windows Journal File
	'FCS' , # Flow Cytometry Standard File
	'CAV' , # Comodo Virus Definitions File
	'A6P' , # Authorware 6 Project
	'RP' , # Axure RP Project File
	'PTF' , # Scrapbook Flair Template
	'ACC' , # Graphics Accounts Data File
	'PDX' , # Adobe Acrobat Index File
	'OO3' , # OmniOutliner 3 File
	'MDL' , # Rational Rose Model File
	'FDB' , # Art Explosion Catalog
	'MWF' , # MapGuide Author Map Window File
	'IMT' , # My Time Data File
	'RDB' , # N64 ROM Database
	'PAB' , # Personal Address Book
	'Q09' , # QuickTax 2009 Tax Return
	'QPB' , # QuickBooks Point of Sale Data File
	'PHB' , # Motorola Phone Book File
	'IIF' , # Intuit Interchange Format File
	'EMLXPART' , # Mail Message Attachment
	'DVO' , # Cook'n Cookbook File
	'U10' , # UFile 2010 Tax Return
	'PCR' , # PCMark Vantage Benchmark File
	'ACS' , # Agent Character File
	'EGP' , # Easy Grade Pro Gradebook File
	'T10' , # At Home 2010 Tax Return
	'POTM' , # Microsoft PowerPoint Macro-Enabled Template
	'Q08' , # QuickTax 2008 Tax Return
	'KEYCHAIN' , # Mac OS X Keychain File
	'RBT' , # LEGO MINDSTORMS NXT Program
	'RSC' , # Symbian Application Compiled Resource File
	'LIB' , # Generic Data Library
	'3DR' , # 3DMark Results File
	'UCCAPILOG' , # Microsoft UCC API Log File
	'PFC' , # AOL Personal Filing Cabinet
	'OTP' , # OpenDocument Presentation Template
	'SDF' , # Structure Data File
	'BGL' , # Babylon Glossary File
	'FDB' , # Portfolio Catalog
	'HST' , # History File
	'MBG' , # Microsoft Mailbag
	'PJ2' , # xPlan Document
	'BVP' , # VodaHost BlueVoda Project File
	'RFO' , # RoboForm Option File
	'PPF' , # Edgecam Pathtrace Part File
	'WDF' , # Workshare Compare DeltaFile
	'H12' , # At Home Canada 2012 Tax Return
	'L6T' , # Line 6 Tone File
	'DSZ' , # Orchida (OES) Embroidery File
	'KEY' , # Apple Keynote Presentation
	'XLT' , # Microsoft Excel Spreadsheet Template (Legacy)
	'PDB' , # Protein Data Bank File
	'BOX' , # E-mail Mailbox
	'IN' , # Input File
	'PPSX' , # Microsoft PowerPoint Slide Show
	'CRTX' , # Office 2007 Chart Template File
	'BCM' , # Business Contact Manager File
	'IDX' , # Navigation POI File
	'CMA' , # Coach Activity File
	'GEDCOM' , # GEDCOM Genealogy File
	'XML' , # XML File
	'PKT' , # Packet Tracer Network Simulation Model
	'QVW' , # QlikView Document
	'VDB' , # Symantec Virus Database File
	'RET' , # BusinessObjects Template
	'RVT' , # Revit Project File
	'TSV' , # Tab-Separated Values File
	'FOX' , # InfoZoom Data File
	'MMP' , # Master Album Maker Project
	'NOTEBOOK' , # SMART Notebook File
	'SLX' , # MathWorks Simulink Model File
	'SAV' , # SPSS Data File
	'USR' , # SmartMusic Custom File
	'XFD' , # XFD File
	'MNC' , # My Notes Center Notebook
	'SC45' , # SuperCard Project File
	'SEN' , # SenTest Data File
	'XPT' , # SAS Transport File
	'GRV' , # Office Groove File
	'OEACCOUNT' , # Windows Mail Account File
	'EV' , # Echoview File
	'PDX' , # Product Data eXchange File
	'DFPROJ' , # DVD Flick Project
	'T18' , # H&R Block 2018 Tax Return
	'LMX' , # Landmark Exchange File
	'HDA' , # HotDocs Auto-Assemble File
	'ABY' , # AOL Address Book File
	'PKS' , # Oracle Package Spec File
	'I5Z' , # IUCLID 5 Import/Export File
	'DVDPROJ' , # iDVD Project File
	'RCG' , # Recog Character Recognition Set
	'MPKT' , # MOPEKS Trial File
	'SVF' , # Serial Vector Format File
	'DSY' , # Directory Synchronizer Project File
	'II' , # IconWorkshop Extended Information File
	'BCI' , # Belarc Advisor Report File
	'GWK' , # GraphiCode PCB Job File
	'PRO6PLX' , # ProPresenter 6 Playlist Bundle
	'SLP' , # Sisulizer Project File
	'AW' , # Answer Wizard File
	'MNO' , # Macromedia Design Note
	'RPT' , # Crystal Reports File
	'OFT' , # Outlook File Template
	'MPP' , # Microsoft Project File
	'ONE' , # OneNote Document
	'PAF' , # Personal Ancestral File
	'OBJ' , # Relocatable Object Code
	'QDF' , # Quicken Data File
	'KPF' , # Komodo Project File
	'VOI' , # VOI Group File
	'MOHO' , # Moho Animation Project
	'PPTM' , # Microsoft PowerPoint Macro-Enabled Presentation
	'MOX' , # ExtendSim Simulation Software Model
	'PKB' , # Oracle Package Body File
	'OFC' , # Open Financial Connectivity File
	'FLP' , # ActivPrimary Flipchart File
	'TRA' , # WinTrack Railroad Track File
	'VCF' , # vCard File
	'EXIF' , # Exchangeable Image Information File
	'SPS' , # SPSS Program File
	'PDS' , # Planetary Data System File
	'FFWP' , # FormsForWeb Packet
	'EFX' , # eFax Document
	'ENEX' , # Evernote Archive
	'MJK' , # Question File
	'SEO' , # SEO Note File
	'LVM' , # LabVIEW Measurement File
	'ANME' , # Anime Studio Document
	'IBA' , # iBooks Author Document
	'XLC' , # Microsoft Excel Chart
	'OFX' , # Open Financial Exchange File
	'LBL' , # NiceLabel Template File
	'GBR' , # Gerber File
	'LAS' , # LIDAR Data Exchange File
	'STM' , # Exchange Streaming Media File
	'WINDOWSLIVECONTACT' , # Windows Live Contacts File
	'EMB' , # Wilcom Embroidery Design File
	'CSV' , # Comma-Separated Values File
	'ASND' , # Adobe Sound Document
	'RTE' , # Revit Project Template
	'QUICKEN2017' , # Quicken 2017 Data File
	'TRS' , # WinTrack Railroad Track Text File
	'GRK' , # Gradekeeper Class Gradebook File
	'WGT' , # IMPS Data File
	'HL' , # HeuristicLab Storage File
	'MYI' , # MySQL MyISAM Index File
	'GDT' , # gretl Datafile
	'PDAS' , # PDAStore Data Store File
	'FMAT' , # FMAT Runfile
	'T07' , # TaxCut 2007 Tax Return
	'QB2011' , # QuickBooks 2011 File
	'PTZ' , # E-Transcript Bundle File
	'QB2013' , # QuickBooks 2013 File
	'T13' , # H&R Block 2013 Tax Return
	'PST' , # Outlook Personal Information Store
	'AAE' , # Sidecar Image Edit File
	'TAX2018' , # TurboTax 2018 Tax Return
	'RPP' , # REAPER Project File
	'JRPRINT' , # JasperReports Print File
	'WPC' , # WordPad Converter File
	'GCW' , # Microsoft Mathematics Worksheet
	'TPF' , # Transit NXT Pack Translation File
	'XRDML' , # XRDML Data File
	'NPL' , # Nokia Playlist File
	'CELTX' , # Celtx Project File
	'POTX' , # Microsoft PowerPoint Presentation Template
	'XSL' , # XML Stylesheet
	'SLK' , # Symbolic Link File
	'LP7' , # LP7 Digitally Signed File
	'CLG' , # Collage Maker Project File
	'OMP' , # Office Manager Document Archive
	'XMCD' , # Mathcad Worksheet File
	'PMO' , # Broderbund Print Meta Object File
	'T19' , # H&R Block 2019 Tax Return
	'PXJ' , # RecordNow Project
	'GS' , # GemStone Document
	'XPG' , # Autoplay Media Studio Exported Page
	'4DV' , # 4D View Ultrasound File
	'MDM' , # HLM Multivariate Data Matrix File
	'GPI' , # Garmin Point of Interest File
	'IDX' , # HMI Historical Log Index File
	'CDF' , # Common Data Format
	'VXML' , # VoiceXML File
	'MDSX' , # MEGA Saved Session
	'TAX2015' , # TurboTax 2015 Tax Return
	'CVN' , # CineVision Encoding Session
	'FPSL' , # FileMaker Pro Snapshot Link
	'TB' , # Tabbery Tab File
	'TDT' , # THOR Data Tree File
	'KPZ' , # Komodo Template File
	'PRJ' , # Project File
	'TBK' , # ToolBook File
	'VCE' , # Visual CertExam Exam File
	'GED' , # GEDCOM Genealogy Data File
	'WB3' , # Corel Quattro Pro 7 and 8 File
	'LSF' , # Logos Library System File
	'STYKZ' , # Stykz Animation
	'ER1' , # ERWin Entity Relationship Diagram
	'CAP' , # Packet Capture File
	'VCD' , # Value Change Dump File
	'TAX2017' , # TurboTax 2017 Tax Return
	'SIM' , # SAP Tutor File
	'TEF' , # TablEdit Tablature
	'DAM' , # DeltaMaster Analysis Model
	'FXP' , # Adobe Flex Project File
	'QB2014' , # QuickBooks 2014 File
	'TWB' , # Tableau Workbook File
	'OUT' , # Output File
	'INX' , # Compiled Script
	'RTE' , # Navigation Route
	'ADCP' , # Adobe Device Central Project File
	'MDJ' , # StarUML Model File
	'TAX2013' , # TurboTax 2013 Tax Return
	'ABP' , # AVS Barcode Profile
	'M' , # Wolfram Mathematica Language Package
	'CDX' , # ChemDraw Exchange File
	'PTB' , # Power Tab File
	'DCM' , # DiskCatalogMaker Catalog
	'QMTF' , # Quicken Mac Transfer File
	'SDP' , # Session Description Protocol File
	'WEA' , # Ecotect Weather File
	'DII' , # Summation Batch Load File
	'TE3' , # WinTrack Object File
	'ID2' , # Windows Live Messenger Emoticon File
	'RNQ' , # Renque Model File
	'SAR' , # Sibelius Arrange Style
	'GNO' , # GenoPro Genealogy Tree File
	'JDB' , # Symantec Endpoint Protection Update File
	'SQD' , # SQ Data File
	'XMLPER' , # LeCroy Binary Waveform File
	'TAX2008' , # TurboTax 2008 Tax Return
	'CNA' , # Catena Project File
	'NPT' , # Portfolio NetPublish Template
	'OTL' , # NoteTab Outline File
	'T11' , # At Home 2011 Tax Return
	'PD4' , # Windows Live Photo Gallery Information File
	'XPJ' , # RoboHelp HTML Project File
	'T06' , # TaxCut 2006 Tax Return
	'MMP' , # MindManager Mind Map
	'PD5' , # Windows Live Photo Gallery Information File
	'T16' , # H&R Block 2016 Tax Return
	'VRD' , # Visio Report Definition File
	'WGT' , # XNBC Synaptic Weights File
	'XSLT' , # Extensible Stylesheet Language Transformations File
	'XFO' , # XSL-FO Formatted Form
	'EPF' , # Edgecam Educational Part File
	'SCD' , # TurboTax Tax Schedule List
	'HS2' , # HyperRESEARCH 2 Study File
	'OR6' , # Lotus Organizer 6 File
	'MEX' , # Macro Express File
	'FOP' , # InfoZoom Protected Data File
	'DAS' , # DeltaMaster Analysis Session
	'TDM' , # LabVIEW Binary Measurement File
	'PTN' , # PaperPort Thumbnail File
	'XRP' , # RationalPlan Project File
	'ROU' , # Gerber Route File
	'PXL' , # Pocket Excel File
	'XDB' , # Symantec Virus Database File
	'LIX' , # Logos Library System File
	'T08' , # TaxCut 2008 Tax Return
	'ITX' , # FileNet eForms Designer Template
	'PSPD' , # PhotoSuite 5 Project File
	'DSB' , # Wondershare DVD Slideshow Builder Project
	'PPF' , # Transit NXT Pack Project File
	'PKH' , # Oracle Package File
	'IMA' , # Sage ACT! Email Message
	'ULF' , # Universal List File
	'BJO' , # TabRite Tablature File
	'BRW' , # Calyx Point Borrower File
	'PCAPNG' , # Pcap-NG Packet Capture File
	'QIF' , # Quicken Interchange Format File
	'SPV' , # SPSS Output Document
	'QBW' , # QuickBooks for Windows Company File
	'MCDX' , # Mathcad Prime Document
	'XFDF' , # Adobe Acrobat XFDF File
	'CTF' , # WhereIsIt Catalog
	'IES' , # IES Photometric Data File
	'MCD' , # Mathcad Document
	'QFX' , # Quicken Financial Exchange File
	'GAN' , # GanttProject Project File
	'STA' , # ABAQUS Status File
	'POD' , # OpenProj Project File
	'MAT' , # MATLAB MAT-file
	'TAX2019' , # TurboTax 2019 Tax Return
	'MMF' , # Meal-Master Recipe File
	'MDC' , # Multi Dimension Cube File
	'DBD' , # DemoShield Project
	'NDK' , # Lotus Notes Design Elements File
	'DAL' , # DVD-lab Project File
	'NP' , # Portfolio NetPublish File
	'FTW' , # Family Tree Maker File
	'TAX2011' , # TurboTax 2011 Tax Return
	'EXP' , # Export File
	'TT10' , # TurboTax Canada 2010 Tax Return
	'FMC' , # FormConnect Form File
	'MW' , # Maple Worksheet
	'VDX' , # Microsoft Visio XML Drawing
	'HML' , # HostMonitor TestList File
	'MBX' , # Outlook Express Mailbox
	'ITMSP' , # iTunes Store Package
	'PNPROJ' , # Programmer's Notepad Project File
	'SLE' , # Sisulizer Package File
	'SHW' , # Corel Presentation
	'MWS' , # Maple Classic Worksheet
	'MTW' , # Minitab Worksheet File
	'VI' , # LabVIEW Virtual Instrument File
	'EPW' , # EnergyPlus Weather Data File
	'ESX' , # Xactimate Insurance Claims Estimate
	'FLO' , # iGrafx FlowCharter File
	'PHM' , # Pro Home Manager Data File
	'DDC' , # DivX Descriptor File
	'T09' , # At Home 2009 Tax Return
	'TT12' , # TurboTax Canada 2012 Tax Return
	'IGC' , # Anime List Builder Input File
	'LIVEUPDATE' , # Symantec LiveUpdate File
	'SGML' , # Standard Generalized Markup Language File
	'CLM' , # MagicJack Call Log File
	'XDNA' , # DNA Strider Sequence File
	'ROD' , # Actuate Report Object Design File
	'PJM' , # xPlan Model
	'GRR' , # Gradekeeper Class Roster File
	'QDP' , # Quick and Dandy Plotter File
	'BCC' , # Calendar Creator File
	'QB2012' , # QuickBooks 2012 File
	'KPP' , # Kid Pix Presentation
	'NPR' , # Nuendo Project File
	'PXF' , # Transit XV Pack Project File
	'GC' , # GraphClick File
	'DDCX' , # DivX Descriptor 2 File
	'ZDC' , # CD Catalog Expert Database File
	'VTX' , # Visio Template XML File
	'KID' , # Kidspiration Document
	'LGH' , # HMI Historical Log File
	'OPX' , # FlipAlbum Photo Album File
	'IXB' , # Ulead Disc Image
	'KPX' , # Kid Pix Picture
	'WNK' , # Wink Presentation
	'DMSP' , # PhotoSuite Project File
	'LSL' , # LightScribe Label
	'ULZ' , # USBlyzer Data Capture File
	'TDA' , # Palm To-Do File
	'QB2009' , # QuickBooks 2009 File
	'OND' , # Lotus Notes Encapsulated Memo File
	'CSA' , # PNA Code Calset File
	'FHC' , # FHC Data File
	'TFA' , # TreeSoft CashFlow Analysis Data File
	'TT13' , # TurboTax Canada 2013 Tax Return
	'TPB' , # GROMACS Binary Input File
	'TKFL' , # TKLinks Saver Favorite Links
	'SPUB' , # Disc Cover File
	'TDA' , # Java Application Thread Dump
	'MBG' , # Mailbag Assistant Mailbox File
	'BLB' , # Blob Data File
	'SWK' , # StudyWorks Save File
	'FO' , # XSL-FO Form
	'XPJ' , # Micro Planner X-Pert Project File
	'LIX' , # ExtendSim Simulation Software Library
	'TET' , # Textease CT Template
	'CTM' , # CrazyTalk Model File
	'TXF' , # Tax Exchange Format
	'SBX' , # ESRI Spatial Index File
	'STP' , # Analysis Studio Project Information File
	'AWG' , # Activeworlds Object Group File
	'LW4' , # Lightwright File
	'ITM' , # iThink Model File
	'WB2' , # Corel Quattro Pro File
	'IVT' , # Beyond 20/20 Table File
)

# noinspection SpellCheckingInspection
audio_set = (
	'SEQUENCE' , # Online Sequencer Sequence
	'SVP' , # Synthesizer V Project
	'FUR' , # Furnace Module
	'FTM' , # Finale 2012 Score Template
	'ABC' , # ABC Music Notation
	'WEBA' , # WebM Audio File
	'MTM' , # MultiTracker Module
	'EFS' , # Ensoniq SQ-80 File
	'UST' , # UTAU Sequence Text File
	'XRNS' , # Renoise Song File
	'FLP' , # FL Studio Project
	'TG' , # TuxGuitar Document
	'COPY' , # Sony Ericsson Protected Content
	'SDT' , # Electronic Arts Sound Data File
	'MP3' , # MP3 Audio
	'VSQ' , # VOCALOID2 Project File
	'EC3' , # Enhanced Audio Codec 3 File
	'TOC' , # PSP Custom Audio Track
	'SDS' , # MIDI Sample Dump Standard File
	'FLAC' , # Free Lossless Audio Codec File
	'SF2' , # SoundFont 2 Sound Bank
	'ASD' , # Ableton Live Sample Analysis
	'WPROJ' , # Wwise Project File
	'DCF' , # DRM Content Format File
	'STY' , # Band-in-a-Box Styles File
	'MUI' , # Myriad User Instrument File
	'AUP' , # Audacity Project File
	'MKA' , # Matroska Audio
	'MIDI' , # MIDI Music Data
	'CWB' , # Cakewalk Bundle
	'FSC' , # FL Studio Score File
	'CDO' , # Crescendo Music Notation File
	'ALS' , # Ableton Live Set File
	'DFF' , # Direct Stream Digital Audio
	'AMXD' , # Ableton Max Patch File
	'MMLP' , # Music Macro Language Project
	'VOXAL' , # Voxal Project File
	'DSM' , # Digital Sound Module
	'RAD' , # Reality Adlib Tracker Module
	'VDJ' , # VirtualDJ Audio Sample File
	'RIP' , # Hit'n'Mix Audio Mashup File
	'SNGX' , # ChordWizard Song
	'GSM' , # Global System for Mobile Audio
	'ARIA' , # Chipsounds Sound File
	'ACP' , # aacPlus Audio File
	'DCT' , # Dictation Audio File
	'OMG' , # OpenMG Audio File
	'MINIGSF' , # Game Boy Advance Song File
	'SAF' , # Secure Audio File
	'IGP' , # Igor Published Music Notation File 
	'PNA' , # PhatNoise Audio File
	'XMU' , # Expressive Music Container File
	'CWS' , # ChordWizard Song
	'H5S' , # POD HD500 Edit Set List
	'M4R' , # iPhone Ringtone
	'PTXT' , # Pro Tools Session Template
	'CGRP' , # Pro Tools Clip Group File
	'FEV' , # FMOD Audio Events File
	'PHY' , # PhyMod Physical Modeling Data
	'AKP' , # Akai Sampler File
	'KT3' , # Battery 3 Drum Kit File
	'NKI' , # KONTAKT Instrument File
	'SLP' , # SpectraLayers Pro Project
	'MTI' , # MadTracker Instrument
	'VPW' , # VoxPro Wave File
	'SC2' , # Sample Cell II Instrument Definition
	'GP' , # Guitar Pro 7 Document
	'GSF' , # Game Boy Advance Sound File
	'DMSE' , # Sound Editor Project File
	'ITLS' , # iTunes Live Stream URL
	'SFK' , # Sound Forge Pro Audio Peak File
	'BAND' , # GarageBand Project
	'4MP' , # 4-MP3 Database File
	'L' , # Left Audio Channel File
	'MID' , # MIDI File
	'GP5' , # Guitar Pro 5 Tablature File
	'MSCZ' , # MuseScore Compressed Score File
	'RX2' , # REX2 Audio File
	'GBS' , # Game Boy Sound File
	'APL' , # Monkey's Audio Track Information File
	'BUN' , # Cakewalk Bundle File
	'ANG' , # Anghami Audio File
	'PEK' , # Adobe Peak Waveform File
	'RMJ' , # Real Media Jukebox Audio File
	'OGG' , # Ogg Vorbis Audio
	'G726' , # G.726 ADPCM Audio
	'ABM' , # Music Album
	'ACM' , # Interplay Audio File
	'VSQX' , # VOCALOID3 Project File
	'WAV' , # WAVE Audio
	'AFC' , # Mass Effect 2 Audio File
	'MMPZ' , # LMMS Project File
	'ALC' , # Ableton Live Clip File
	'LOGICX' , # Logic Pro Project
	'SFPACK' , # Packed SoundFont File
	'SGP' , # MP3 Audio Mixer Sound Group Project
	'PLA' , # Sansa Playlist File
	'VLC' , # VLC Playlist
	'NCW' , # Native Compressed Wave File
	'STM' , # Scream Tracker 2 Module
	'S3M' , # ScreamTracker 3 Module
	'ACD-ZIP' , # Sony ACID Project With Embedded Media File
	'EMX' , # eMusic Download File
	'PSM' , # Protracker Studio Module
	'QCP' , # PureVoice Audio
	'OVW' , # Logic Pro Overview File
	'CUE' , # Cue Sheet File
	'DM' , # DRM Delivery Message
	'VAG' , # PlayStation Compressed Audio
	'RNS' , # Reason Song File
	'VYF' , # Samsung Digital Voice Recorder File
	'UNI' , # MikMod UniMOD Module
	'ACT' , # ADPCM Compressed Audio
	'MXL' , # Compressed MusicXML File
	'UAX' , # Unreal Audio Package
	'SESX' , # Audition Session
	'AC3' , # Audio Codec 3 File
	'Q2' , # Winamp Equalizer Auto-Load Presets File
	'Q1' , # Winamp Equalizer Presets File
	'SSEQ' , # Nintendo DS Sound Sequence File
	'NRT' , # Nokia Ringtone
	'VQF' , # TwinVQ Audio File
	'ROL' , # Ad Lib Synthesized Instrument
	'FRG' , # Sound Forge Pro Project
	'GPK' , # WaveLab Audio Peak File
	'ZPA' , # Vielklang Audio Metadata File
	'RMX' , # RealJukebox Format
	'M3U8' , # UTF-8 M3U Playlist
	'REX' , # ReCycle Loop File
	'PKF' , # Adobe Audition Peak File
	'FLM' , # FL Studio Mobile Project
	'SIB' , # Sibelius Score
	'RGRP' , # Pro Tools Region Group File
	'SMF' , # Standard MIDI File
	'LOGIC' , # Logic Pro Project File
	'CTS' , # CrazyTalk Script File
	'AOB' , # DVD-Audio Audio Object File
	'FTM' , # FamiTracker Module
	'PTX' , # Pro Tools Session File
	'WAVE' , # WAVE Sound File
	'WFP' , # WaveFront Program File
	'WMA' , # Windows Media Audio
	'MPU' , # MPEG Layer 3 Audio File
	'F32' , # Raw 32-Bit Audio File
	'CIDB' , # iTunes CD Information File
	'ISMA' , # IIS Smooth Streaming Audio File
	'ACD' , # ACID Project File
	'5XE' , # Line 6 POD HD500X Edit Preset File
	'TRAK' , # Traktor Content Pack File
	'S3I' , # Scream Tracker 3 Instrument
	'PANDORA' , # Pandora Android App Executable
	'SBI' , # Sound Blaster Instrument
	'PCAST' , # iTunes Podcast File
	'CKB' , # Cricket Audio Bank File
	'SPH' , # NIST SPHERE Audio File
	'WUS' , # WUTG Tagged Audio
	'3GA' , # 3GPP Audio File
	'M4A' , # MPEG-4 Audio
	'OGA' , # Ogg Vorbis Audio File
	'M3U' , # M3U Media Playlist
	'AT3' , # ATRAC3 Audio File
	'SWA' , # Shockwave Audio
	'RAM' , # Real Audio Metadata File
	'YOOKOO' , # Yookoo Player Playlist File
	'W01' , # Yamaha SY Series Wave File
	'OMA' , # Sony OpenMG Music File
	'NML' , # Traktor Collection File
	'MED' , # Amiga MED Sound File
	'CAF' , # Core Audio File
	'MOD' , # Music Module File
	'XA' , # The Sims Audio File
	'MUS' , # Minecraft Music File
	'MOGG' , # Multitrack Ogg File
	'WAX' , # Windows Media Audio Redirect
	'AUP3' , # Audacity 3 Project File
	'NOTE' , # Notessimo Composition
	'H5B' , # POD HD500 Edit Bundle
	'CWT' , # Cakewalk SONAR Template
	'UW' , # Unsigned Word Audio File
	'B4S' , # Winamp Playlist File
	'FTMX' , # Finale 2014 Score Template
	'WRK' , # Cakewalk Music Project
	'OPUS' , # Opus Audio
	'INS' , # Adlib Tracker Instrument File
	'M4B' , # MPEG-4 Audiobook
	'SDAT' , # Nintendo DS Sound Data File
	'OMF' , # Open Media Framework File
	'WFM' , # Pro Tools Wave Cache File
	'PTT' , # Pro Tools Session Template
	'MTP' , # MadTracker 2 Pattern
	'AIFF' , # Audio Interchange File Format
	'MUX' , # Trackmania Music File
	'SVD' , # Roland Patch File
	'MUS' , # Finale Notation File (Legacy)
	'KMP' , # Korg Keymap File
	'NKM' , # Kontakt Multi Instrument File
	'WPP' , # WavePad Project File
	'AIF' , # Audio Interchange File Format
	'MO3' , # MO3 Audio
	'669' , # UNIS Composer 669 Module
	'RSO' , # NXT Brick Audio File
	'BNK' , # Adlib Instrument Bank
	'XSPF' , # XML Shareable Playlist File
	'LOF' , # Audacity File List
	'DIG' , # Sound Designer Audio File
	'WFB' , # WaveFront Sound Bank
	'AGM' , # DTS Multi-channel Pro Packer File
	'DSF' , # Delusion Digital Sound File
	'RTA' , # TrueRTA Project File
	'MX4' , # Mixcraft 4 Audio Project
	'BIDULE' , # Bidule Layout File
	'VC3' , # VSampler Soundbank File
	'WVC' , # WavPack Correction File
	'IAA' , # INTUS Audio Archive
	'DEWF' , # SoundEdit Recorded Instrument
	'MX3' , # Mixcraft 3 Audio Project
	'BDD' , # CARA Sound Radiation Data File
	'MBR' , # Zune Smooth Streaming File
	'NKX' , # Kontakt Monolith Container File
	'SSND' , # Synclavier Sound File
	'DSS' , # Digital Speech Standard File
	'H4B' , # Line 6 POD HD400 Edit Bundle
	'STAP' , # Soundtrack Pro Audio Project File
	'PTM' , # PolyTracker Module
	'SXT' , # Propellerhead Reason NN-XT Patch File
	'PTS' , # Pro Tools Session
	'AMF' , # Advanced Module File
	'CDDA' , # CD Digital Audio File
	'RTI' , # Real Tracker Instrument
	'GPBANK' , # Guitar Pro Sound Bank File
	'MX5TEMPLATE' , # Mixcraft 5 Audio Project Template
	'BRSTM' , # BRSTM Audio Stream
	'SOU' , # SBStudio II Sound File
	'BNL' , # Talking Reading Pen Audio File
	'DMSA' , # Music Disc Creator Project File
	'MPDP' , # MixPad Project File
	'A2P' , # Adlib Tracker II Pattern File
	'MPTM' , # OpenMPT Module
	'VPL' , # Karaoke Player Playlist
	'CAFF' , # Core Audio File
	'MTF' , # Multi Tracker File
	'ENC' , # Encore Musical Notation
	'CDR' , # Raw CD Audio Data
	'TAK' , # Tom's Lossless Audio Kompressor File
	'CWP' , # Cakewalk SONAR Project
	'W64' , # Sony Wave64 Audio File
	'AA' , # Audible Audio Book File
	'CPR' , # Cubase Project
	'VOX' , # Dialogic Voice Audio File
	'BWW' , # Bagpipe Player File
	'AGR' , # Ableton Groove File
	'CDA' , # CD Audio Track Shortcut
	'SD' , # ESPS Sampled Data
	'DS' , # LMMS DrumSynth File
	'SNG' , # MIDI Song
	'VPR' , # VOCALOID5 Project File
	'ZVD' , # Zyxel Voice File
	'WPK' , # Nero Wave Editor File
	'EMD' , # ABT Extended Module
	'OMX' , # OtsAV Media Library Information File
	'DTSHD' , # DTS-HD Master Audio File
	'F4A' , # Adobe Flash Protected Audio File
	'MUX' , # Myriad Stand-Alone Music Score
	'LSO' , # Logic Audio Project
	'AAC' , # Advanced Audio Coding File
	'NWC' , # NoteWorthy Composer File
	'RAW' , # Raw Audio Data
	'VMD' , # Covox Raw Sample
	'XM' , # Fasttracker 2 Extended Module
	'AMR' , # Adaptive Multi-Rate Codec File
	'WVE' , # WaveEditor Project File
	'MXMF' , # Mobile XMF Ringtone File
	'RA' , # RealAudio File
	'IFF' , # Interchange File Format
	'PVC' , # Panasonic VM1 Voice File
	'GSFLIB' , # Game Boy Advance Song Library
	'STREAMDECKAUDIO' , # Stream Deck Audio
	'SYW' , # SY99/SY85 Audio
	'FPA' , # Finale Performance Assessment
	'HCA' , # High Compression Audio File
	'WTPT' , # WireTap Studio Packaged Track
	'SYH' , # Synchomatic Instrument
	'DRG' , # I-Doser Audio Drug File
	'DRA' , # Nuance Dragon Voice Recording File
	'U' , # AU Audio File
	'VGZ' , # Video Game Music Compressed File
	'MMP' , # MixMeister Playlist
	'RMI' , # RMID MIDI File
	'MUSICXML' , # MusicXML File
	'RCY' , # ReCycle 1.x Document
	'VPM' , # Garmin Voice File
	'SHN' , # Shorten Compressed Audio File
	'5XB' , # Line 6 POD HD500X Edit Bundle
	'NKC' , # Kontakt Library Data File
	'M4P' , # iTunes Music Store Audio File
	'GPX' , # Guitar Pro 6 Document
	'MPA' , # MPEG-2 Audio File
	'AU' , # Sun Microsystems Audio File
	'ICS' , # IC Recorder Sound File
	'8SVX' , # Amiga 8-Bit Sound File
	'8CM' , # Eight Channel Module
	'NPL' , # Cubase Library File
	'MDC' , # MidiCo Karaoke Audio File
	'IGR' , # Igor Engraver File
	'DVF' , # Sony Digital Voice File
	'AB' , # Ambling BookPlayer MP3 File
	'LWV' , # Linguistically Enhanced Sound File
	'NKS' , # Kontakt Monolith Container
	'WV' , # WavPack Audio File
	'SEQ' , # PowerTracks Pro Audio Project File
	'KAR' , # Karaoke MIDI File
	'AMS' , # Velvet Studio Advanced Module System
	'UWF' , # UltraTracker Wave File
	'TAK' , # Music Maker Take File
	'MYR' , # Myriad Music File
	'LA' , # Lossless Audio File
	'H5E' , # POD HD500 Edit Preset
	'MDR' , # ModPlug Compressed Module
	'MX5' , # Mixcraft 5 Audio Project
	'MPGA' , # MPEG-1 Layer 3 Audio File
	'NVF' , # Creative Labs NVF Audio File
	'OBW' , # Superior Drummer Sounds File
	'CFA' , # Adobe Conformed Audio File
	'MMM' , # Music Maker Arrangement File
	'PSF' , # Portable Sound File
	'SONG' , # AudioSauna Song File
	'DS2' , # Olympus DSS Pro Audio File
	'DTS' , # DTS Encoded Audio File
	'ADT' , # ADTS Audio File
	'MUSX' , # Finale Notation File 
	'AVASTSOUNDS' , # Avast Soundpack File
	'A2M' , # Adlib Tracker II File
	'ALL' , # Cubasis Project File
	'VIP' , # MAGIX Samplitude Music Studio Project
	'PEAK' , # Steinberg Peak File
	'DLS' , # Downloadable Sounds File
	'ODM' , # OverDrive Media File
	'DCM' , # DCM Audio Module
	'ZPL' , # Zune Playlist
	'NKSF' , # Native Kontrol Standard Preset
	'GBPROJ' , # GarageBand Project
	'BWF' , # Broadcast Wave File
	'KSC' , # Korg Trinity/Triton Script File
	'CEOL' , # Bosca Ceoil Song
	'KOZ' , # Bell Cellular Music
	'SMA' , # SmartMusic Accompaniment File
	'PSF2' , # PlayStation Sound Format File
	'MT2' , # MadTracker 2 Module
	'PCA' , # Perfect Clarity Audio File
	'PAC' , # SBStudio II Song File
	'SFZ' , # SFZ Sample Definition File
	'RSN' , # Reason Project File
	'KOZ' , # Audiokoz Music File
	'6CM' , # Six Channel Module
	'MSMPL_BANK' , # Korg microSAMPLER Bank Data File
	'OVEX' , # Overture 5 Musical Score
	'WUT' , # WUTG Tag File
	'RNG' , # Nokia Composer Ringtone
	'NSA' , # Nullsoft Streaming Audio File
	'EFK' , # Ensoniq KT File
	'CAPOBUNDLE' , # Capo Project
	'SSP' , # Serato Studio Project
	'PIXIMOD' , # PixiTracker Module
	'ARIAX' , # Chipsounds XML Sound File
	'K26' , # Kurzweil K2600 File
	'RDVXZ' , # RedVox Infrasound Recorder Audio File
	'EFQ' , # Ensoniq SQ1/SQ2/KS-32 File
	'AAXC' , # Encrypted Audible Enhanced Audiobook
	'VB' , # Grand Theft Auto Audio File
	'DPDOC' , # Digital Performer Project
	'PSF1' , # PlayStation Sound Format File
	'RCD' , # KaiOS Audio
	'DW' , # David Whittaker Audio File
	'SPPACK' , # SPPack Sound Sample
	'RPL' , # Dolby Cinema Print Master File
	'ULT' , # UltraTracker Module
	'SSM' , # Sound Clip Archive
	'NRA' , # Nero Audio Compilation
	'WWU' , # Wwise Work Unit
	'HBE' , # Line 6 POD HD Edit Preset File
	'SCS11' , # Show Cue System Cue File
	'M3UP' , # M3U Playlist (UTF-8)
	'SNG' , # n-Track Studio Song Project
	'SFAS' , # Sound Forge Audio Studio Project
	'VAP' , # Dialogic Indexed Voice Audio File 
	'GROOVE' , # ACID Groove File
	'HSB' , # HALion Sound Bank File
	'ACD-BAK' , # MAGIX ACID Project Backup File
	'SLX' , # SpectraLayers Pro Project
	'FMS' , # FamiStudio Project
	'M5P' , # MachFive Preset File
	'CONFORM' , # Conformalizer Change List
	'FZF' , # Casio FZ-1 Full Voice Dump
	'FZV' , # Casio FZ-1 Voice Dump
	'H2SONG' , # Hydrogen Song
	'G721' , # G.721 ADPCM Audio
	'NSMPPROJ' , # Nord Sample Editor Project
	'GIG' , # Tascam GigaSampler File
	'NSMP' , # Nord Sample
	'NMSV' , # Native Instruments Massive Sound File
	'SAP' , # Atari SAP Music File
	'SYN' , # SimSynth Document
	'AAX' , # Audible Enhanced Audiobook File
	'RFL' , # Reason ReFill Sound Bank
	'AMZ' , # Amazon MP3 Downloader File
	'KSF' , # Korg Trinity/Triton Sample File
	'APE' , # Monkey's Audio
	'PHO' , # MBROLA Phonetic Data File
	'A2B' , # Adlib Tracker II Instrument Bank
	'SD' , # Sound Designer Audio File
	'ADV' , # Ableton Device Preset File
	'PTF' , # Pro Tools Session File (Legacy)
	'MPC' , # Musepack Compressed Audio File
	'OTS' , # OtsAV Album File
	'MP2' , # MPEG Layer II Compressed Audio File
	'AIFC' , # Compressed Audio Interchange File
	'PNO' , # Windows 8 Piano Song
	'USF' , # Nintendo 64 Music File
	'CLP' , # Finale Clip
	'SMP' , # SmartMusic Performance File
	'ADG' , # Ableton Device Group
	'CSH' , # Cubase Waveform File
	'MINIUSF' , # Nintendo 64 Song File
	'FDP' , # FMOD Project File
	'OVE' , # Overture Musical Score
	'SND' , # Akai MPC Sample
	'SBK' , # E-MU SoundFont Sound Bank
	'VRF' , # Ventrilo Audio Recording
	'SES' , # Audition Session (Legacy)
	'JAM' , # Line 6 Device Recording
	'TTA' , # True Audio File
	'DMF' , # Delusion Digital Music File
	'XMF' , # Extensible Music File
	'MMF' , # Synthetic Music Mobile Application File
	'MTS' , # MadTracker 2 Sample File
	'R1M' , # RealOne Streaming Media File
	'PSY' , # Psycle Song File
	'5XS' , # Line 6 POD HD500X Edit Setlist File
	'RVX' , # Real Protected Video File
	'RBS' , # MP3 Ringtone File
	'OVW' , # Cubase WAVE Overview File
	'PRG' , # WAVmaker Patch File
	'RTS' , # Real Tracker Sample
	'SMPX' , # SmartMusic Accompaniment File
	'SND' , # Sound File
	'VTX' , # VTX Chiptune File
	'SFAP0' , # Sound Forge Pro Audio Proxy File
	'VOC' , # Creative Labs Audio File
	'BANK' , # FMOD Audio Bank
	'MPD' , # Melodyne Project
	'KRZ' , # Kurzweil K2000 File
	'STX' , # Scream Tracker Music Interface Kit File
	'MGV' , # Yamaha MegaVoice File
	'SMP' , # SampleVision Audio Sample Format
	'BAP' , # Blaze Audio Wave Information File
	'KFN' , # KaraFun Karaoke File
	'USFLIB' , # Nintendo 64 Song Library
	'VMO' , # Siemens Voice Memo File
	'A2I' , # Adlib Tracker II Instrument File
	'NKB' , # Kontakt Audio Bank
	'S3Z' , # Compressed Scream Tracker 3 Module
	'EXS' , # EXS Instrument
	'ADTS' , # Audio Data Transport Stream File
	'NTN' , # NOTION Score
	'TD0' , # Akai Teledisk Sound Library
	'F64' , # Raw 64-Bit Audio File
	'G723' , # G.723 ADPCM Audio
	'PTCOP' , # PxTone Audio File
	'EXPRESSIONMAP' , # Cubase Expression Map File
	'SD2F' , # Sound Designer 2 File
	'RBS' , # Rebirth Song File
	'EMP' , # eMusic Music Download File
	'AMS' , # Extreme Tracker Module
	'WFD' , # WaveFront Drum Kit File
	'SND' , # Macintosh Sound Resource
	'TXW' , # Yamaha TX16W Audio File
	'SFS' , # SFX Sample File
	'NARRATIVE' , # Narrator Document
	'SD2' , # Sound Designer II File
	'WTPL' , # WireTap Studio Library
	'RAX' , # Real Music Store Audio File
	'SVX' , # 8SVX Sound File
	'MTE' , # MadTracker 2 Envelope
	'OFR' , # OptimFROG Audio File
	'MINIPSF2' , # Miniature PlayStation Sound Format File
	'MINIPSF' , # Miniature PlayStation Sound Format File
	'A2W' , # Adlib Tracker II Instrument Bank with Macros
	'USTX' , # OpenUTAU Sequence Text File
	'JSPF' , # JSON Playlist File
	'FFT' , # Adobe Audition Noise Print File
	'REPEAKS' , # REAPER Peak File
	'AY' , # AY Chiptune File
	'PBF' , # Pinnacle Sample Bank
	'JBX' , # Microsoft Play It! Music File
	'MSV' , # Memory Stick Voice File
	'VMF' , # Covox Speech Thing Sample
	'CEL' , # Audition Loop
	'CPT' , # DTS Compact Audio File
	'A2T' , # Adlib Tracker II Tiny Module File
	'VMF' , # Vocaltec Media File
)

# noinspection SpellCheckingInspection
video_set = (
	'STR' , # YouTube Livestream Recording
	'TTML' , # Timed Text Markup Language Subtitles File
	'RXR' , # RecordXR Recording
	'SWF' , # Shockwave Flash Movie
	'AEP' , # After Effects Project
	'MKV' , # Matroska Video
	'PZ' , # Panzoid Video Project
	'KINE' , # KineMaster Project File
	'PLOT' , # Plotagon Studio Project
	'PRPROJ' , # Premiere Pro Project
	'PIV' , # Pivot Animator Animation
	'SFD' , # Sofdec Dreamcast Movie
	'PSV' , # Pluralsight Video File
	'AV1' , # AV1 Video
	'PIC' , # Pictor Paint Image
	'DREAM' , # Procreate Dreams Project
	'PLOTDOC' , # Plotagon Project File
	'ANM' , # DeluxePaint Animation
	'NTP' , # Natron Project File
	'VEG' , # VEGAS Video Project
	'KMPROJECT' , # KineMaster Project
	'MSDVD' , # Windows DVD Maker Project File
	'WLMP' , # Windows Live Movie Maker Project
	'DRP' , # DaVinci Resolve Project
	'AEC' , # Cinema 4D After Effects Composition
	'DCR' , # Digital Court Recorder Video File
	'AMC' , # AMC Video File
	'BIK' , # Bink Video File
	'MSWMM' , # Windows Movie Maker Project
	'PAC' , # PAC Subtitles File
	'MSE' , # MediaShow Slideshow Project File
	'WEBM' , # WebM Video
	'KDENLIVE' , # Kdenlive Project
	'DIR' , # Adobe Director Movie
	'SER' , # Astronomical Capture Video File
	'CINE' , # Phantom Digital Video File
	'SCM' , # ScreenCam Screen Recording
	'SUB' , # MicroDVD Subtitle File
	'EVO' , # SeeVogh Player Video Recording
	'FBR' , # FlashBack Recording
	'FCP' , # Final Cut Project
	'VPJ' , # VideoPad Video Editor Project File
	'WPL' , # Windows Media Player Playlist
	'DCR' , # Liberty Video Recording File
	'MP4' , # MPEG-4 Video
	'RMVB' , # RealMedia Variable Bit Rate File
	'VOB' , # DVD Video Object File
	'CLPI' , # Blu-ray Clip Information File
	'FLC' , # FLIC Animation
	'SBT' , # SBT Subtitle File
	'SRT' , # SubRip Subtitle File
	'DMX' , # Source Filmmaker Project
	'M4S' , # MPEG-DASH Video Segment
	'IFO' , # DVD-Video Disc Information File
	'INP' , # Sony Camcorder Image Management File
	'VP6' , # TrueMotion VP6 Video File
	'3GP' , # 3GPP Multimedia File
	'VSP' , # VideoStudio Project
	'DMSM' , # VideoWave Movie Project File
	'OBJECTION' , # Objection.lol Project
	'MXF' , # Material Exchange Format
	'CPVC' , # Adobe Captivate Video Composition
	'META' , # RealPlayer Metafile
	'CAMPROJ' , # Camtasia Studio Project
	'MVD' , # Movie Studio Movie
	'IVR' , # Internet Video Recording
	'VTT' , # Web Video Text Tracks File
	'LSAV' , # Xiaomi Gallery Hidden Video
	'TRP' , # HD Video Transport Stream
	'MPEG' , # MPEG Video
	'M4U' , # MPEG-4 Playlist
	'SMV' , # VideoLink Mail Video File
	'WMMP' , # Windows Movie Maker Project File
	'MJPG' , # Motion JPEG Video
	'REC' , # Topfield PVR Recording
	'VIDEO' , # aTube Catcher Video File
	'AEPX' , # After Effects XML Project
	'SWI' , # SWiSH Project File
	'MJ2' , # Motion JPEG 2000 Video
	'MP4V' , # MPEG-4 Video
	'SCREENFLOW' , # ScreenFlow Document
	'AMX' , # Adobe Motion Exchange File
	'MGV' , # PSP Video File
	'D3V' , # Datel Video File
	'MANI' , # Mine-imator Project File
	'RMS' , # Secure Real Media File
	'BK2' , # Bink Video 2 File
	'VC1' , # VC-1 Video File
	'MEPX' , # Movavi Video Project
	'PSH' , # Photodex Slide Show
	'ASF' , # Advanced Systems Format File
	'FLV' , # Flash Video
	'PMF' , # PlayStation Portable Movie
	'DV' , # Digital Video File
	'TVSHOW' , # mimoLive Show
	'ZM2' , # ZSNES Movie #2 File
	'ARCUT' , # Prelude Rough Cut File
	'SIV' , # Silicon Imaging Video File
	'RCUT' , # Webinaria Recording Cut
	'ALE' , # Avid Log Exchange File
	'G2M' , # GoToMeeting Recording File
	'DPA' , # DrawPlus Animation File
	'TBC' , # Time-base Corrected Video
	'MPSUB' , # MPlayer Subtitles File
	'JTV' , # JRiver TV File
	'GFP' , # GreenForce-Player Protected Media File
	'MYS' , # Vineyard Captured Video File
	'DMSM3D' , # VideoWave 3D Movie Project File
	'NCOR' , # Adobe Encore Project File
	'DV4' , # Bosch Security Systems CCTV Video File
	'DXR' , # Protected Macromedia Director Movie
	'MP4.INFOVID' , # Parrot AR Drone and Gyro Flyer Video
	'WMV' , # Windows Media Video
	'MPV' , # MPEG-2 Elementary Stream Video
	'DAT' , # VCD Video File
	'XMV' , # Xbox Media Video File
	'M1V' , # MPEG-1 Video File
	'TREC' , # TechSmith Recording File
	'MPROJ' , # Mine-imator Project
	'3GP2' , # 3GPP Multimedia File
	'TS' , # Video Transport Stream File
	'CME' , # Command Multimedia Evidence File
	'SCC' , # ScreenFlow Screen Recording
	'RM' , # RealMedia File
	'IDX' , # VobSub Subtitles Index File
	'264' , # IP/CCTV Video File
	'MVP' , # MAGIX Video Project
	'PDRPROJ' , # PowerDirector for Mobile Project
	'IVA' , # Surveillance Video File
	'M2T' , # HDV Video File
	'M4V' , # iTunes Video File
	'RCD' , # pcAnywhere Recorded Session File
	'DZM' , # DirectorZone Menu Template
	'BUP' , # DVD IFO Backup
	'CAMREC' , # Camtasia Studio Screen Recording
	'ZMV' , # ZSNES Movie File
	'OGV' , # Ogg Video
	'DNC' , # Windows Dancer File
	'HDMOV' , # QuickTime HD Movie File
	'THEATER' , # iMovie Theater File
	'TIX' , # DivX Video Download Activation File
	'DREAM' , # Dream Animated Wallpaper File
	'DVR' , # Microsoft Recorded TV Show
	'MNV' , # PlayStation Movie File
	'STX' , # Pinnacle Studio Project File
	'VRO' , # DVD Video Recording Format
	'BNP' , # Sony Camcorder Video Data File
	'MOVIE' , # QuickTime Movie File
	'ZOOM' , # Zoom Recording File
	'ISMV' , # IIS Smooth Streaming Video File
	'WP3' , # Microsoft Photo Story Project File
	'F4P' , # Adobe Flash Protected Media File
	'M2TS' , # Blu-ray BDAV Video File
	'AWLIVE' , # Active Webcam Video File
	'WVM' , # Google Play Video File
	'TP' , # Beyond TV Transport Stream File
	'MPG' , # MPEG Video
	'DASH' , # MPEG-DASH Video File
	'DZP' , # DirectorZone Particle Effect File
	'CPI' , # AVCHD Video Clip Information
	'MTS' , # AVCHD Video
	'DZT' , # DirectorZone Title File
	'DMB' , # Digital Multimedia Broadcasting File
	'FBR' , # Mercury Screen Recording
	'XYT' , # VivaVideo Effect
	'CST' , # Director External Cast File
	'MEP' , # Movavi Video Project
	'QTCH' , # QuickTime Cache File
	'VII' , # ViiVid Multi-Vantage Video
	'PPJ' , # Premiere 6 Project File
	'MMP' , # MovieMator Video Editor Pro Project
	'PROJECTOR' , # Screencast Transcript
	'BIK2' , # Bink Video 2 File
	'VEP' , # AVS Video Editor Project File
	'IRCP' , # SpeedGrade IRIDAS Composite File
	'BDT2' , # BEEDOCS Timeline 3D 2 Document
	'CAMV' , # Camtasia Video File
	'INT' , # Sony Camcorder Image Management File
	'SEDPRJ' , # Slideshow Studio Elements Project
	'FFD' , # ffDiaporama Project File
	'F4F' , # Flash Video Fragment
	'KTN' , # KTooN Animation Project File
	'AVV' , # Avid Volume Bin File
	'TSV' , # Digital Receiver Video Recording
	'MV' , # SGI Video File
	'SFVIDCAP' , # Sonic Foundry Video Capture File
	'DVR-MS' , # Microsoft Digital Video Recording
	'ZM3' , # ZSNES Movie #3 File
	'AVB' , # Avid Bin File
	'IZZ' , # Isadora Media Control Project
	'IZZY' , # Isadora Project
	'FLI' , # FLIC Animation
	'TIVO' , # TiVo Video File
	'RDB' , # Wavelet Video Images File
	'BSF' , # Blu-ray AVC Video
	'264' , # Ripped Video Data File
	'MOV' , # Apple QuickTime Movie
	'3MM' , # 3D Movie Maker Movie Project
	'PDS' , # PowerDirector Script File
	'PLAYLIST' , # CyberLink PowerDVD Playlist
	'WVX' , # Windows Media Video Redirector
	'SBK' , # SWiSH Project Backup File
	'DAV' , # CCTV DVR Video
	'OGM' , # Ogg Media File
	'3G2' , # 3GPP2 Multimedia File
	'MVP' , # MoviePlus Project File
	'SFERA' , # Sfera 360° File
	'DVDMEDIA' , # RipIt DVD Package
	'H264' , # H.264 Encoded Video File
	'OGX' , # Ogg Vorbis Multiplexed Media File
	'VID' , # Generic Video File
	'YUV' , # YUV Video File
	'VP7' , # TrueMotion VP7 Video File
	'XESC' , # Expression Encoder Screen Capture File
	'60D' , # CCTV Video Clip
	'MP2V' , # MPEG-2 Video File
	'HEVC' , # High Efficiency Video Coding File
	'XVID' , # Xvid-Encoded Video File
	'GTS' , # CaptiveWorks PVR Video File
	'890' , # Cavena Subtitles File
	'G64X' , # Genetec Video File
	'SAN' , # Smush Animation File
	'MPG4' , # MPEG-4 Video File
	'D2V' , # DVD2AVI File
	'DDAT' , # DivX Temporary Video File
	'MPL' , # AVCHD Playlist File
	'LVIX' , # Lightworks Video Index File
	'AAF' , # Advanced Authoring Format File
	'JDR' , # IROAD Dash Cam Video File
	'VGZ' , # DigitalVDO Compressed Video File
	'ISM' , # IIS Smooth Streaming Manifest File
	'G64' , # Genetec Video File
	'AVE' , # Avigilon Native Video Export File
	'EXI' , # YouTube for Android Offline Video Index
	'LRV' , # Low-resolution Video File
	'F4V' , # Flash MP4 Video File
	'TSP' , # Digital TV DVR Recording
	'VPROJ' , # VSDC Video Editor Project
	'HDV' , # High Definition Video Format
	'MP21' , # AXMEDIS MPEG-21 Object
	'MOI' , # MOI Video File
	'WM' , # Windows Media File
	'M4F' , # Sony Network Camera Video
	'MK3D' , # Matroska 3D Video File
	'QTL' , # QuickTime Link File
	'RSX' , # RED Metadata XML File
	'XLMV' , # Xunlei Movie File
	'DCK' , # Resolume Deck File
	'JSS' , # JACOsub Subtitle File
	'MVEX' , # Muvee autoProducer 6 Project File
	'MP5' , # Chinese Portable Media Player Video
	'DIVX' , # DivX-Encoded Movie
	'R3D' , # REDCODE Video File
	'VCR' , # ATI Video Card Recording
	'BDMV' , # Blu-ray Disc Movie Information File
	'LREC' , # Inter-Tel Web Conference Recording
	'3GPP' , # 3GPP Media File
	'M2P' , # MPEG-2 Program Stream File
	'SWT' , # Flash Generator Template
	'AVCHD' , # High Definition Video File
	'NUV' , # NuppelVideo File
	'WCP' , # WinDVD Creator Project File
	'RV' , # RealVideo File
	'SPL' , # FutureSplash Animation
	'XML' , # Cinelerra Video Project
	'K3G' , # 3GP Mobile Phone Video File
	'CED' , # JVC Camera Video Data File
	'WRF' , # WebEx Recording
	'EXO' , # YouTube Video Chunk
	'AVI' , # Audio Video Interleave File
	'MEPS' , # Movavi Slideshow Maker Project
	'DPG' , # Nintendo DS Movie File
	'PCLX' , # Pencil2D Project
	'3GPP2' , # 3GPP2 Multimedia File
	'SQZ' , # Squeeze Project File
	'RUM' , # Bink Video Subtitle File
	'TVLAYER' , # mimoLive TV Layer
	'TDT' , # AVCHD Thumbnail File
	'MEDIA' , # Security Camera Video
	'PREL' , # Premiere Elements Project File
	'BRAW' , # Blackmagic RAW Video
	'BU' , # Samsung CCTV Video File
	'MPE' , # MPEG Movie File
	'3P2' , # 3GPP Multimedia File
	'ARF' , # WebEx Advanced Recording File
	'BDT3' , # BEEDOCS Timeline 3D 3 Document
	'BMC' , # Bolide Movie Creator Project File
	'FLIC' , # FLIC Animation
	'NFV' , # Netflix Video File
	'LSX' , # Streaming Media Shortcut
	'TVS' , # TeamViewer Video Session File
	'TPD' , # Cyberlink TOD Video File
	'MTV' , # MTV Video Format File
	'MOOV' , # Apple QuickTime Movie
	'AETX' , # After Effects XML Project Template
	'W32' , # WinCAPs Subtitle File
	'AET' , # After Effects Project Template
	'DMSD' , # Roxio MyDVD Project
	'RMP' , # RealPlayer Metadata Package File
	'JMV' , # Jnes Movie File
	'VCPF' , # VideoConvert Project File
	'WXP' , # Wax 2.0 Project
	'MJP' , # MJPEG Video File
	'MVC' , # Movie Collector Catalog
	'EDL' , # Edit Decision List File
	'F4M' , # Adobe Flash Media Manifest File
	'VR' , # VR Studio Project File
	'ASX' , # Microsoft ASF Redirector File
	'NVC' , # NeroVision Express Project File
	'IMOVIEPROJ' , # iMovie Project File
	'WMD' , # Windows Media Download Package
	'Y4M' , # YUV4MPEG2 Video File
	'BVR' , # Blue Iris Video
	'IRF' , # CCTV DVR H.264 Video File
	'BDM' , # AVCHD Information File
	'AJP' , # CCTV Video File
	'FTC' , # FluxTime Clip File
	'N3R' , # Panasonic Security System Video File
	'QT' , # Apple QuickTime Movie
	'EPV' , # Wi-Fi Security Camera Footage
	'RAVI' , # Radiometric Video File
	'AVP' , # Avid Project File
	'M2A' , # MPEG-1 Layer 2 Audio File
	'CMV' , # CorelMOVE Animation
	'TVRECORDING' , # BoinxTV TV recording
	'SMIL' , # SMIL Presentation File
	'MOVIE' , # Payday Movie File
	'PRTL' , # Premiere Pro Title
	'AVD' , # Movie Edit Pro Video Information File
	'IMOVIELIBRARY' , # iMovie Library File
	'MVE' , # Interplay MVE Video File
	'OTRKEY' , # OnlineTvRecorder Encoded Video File
	'M2V' , # MPEG-2 Video
	'AVS' , # Application Visualization System File
	'MPL' , # MPL2 Subtitles File
	'PLPROJ' , # Adobe Prelude Project File
	'SDV' , # Studio DV Project File
	'SBZ' , # ShowBiz Project File
	'DVX' , # DivX Video File
	'VFZ' , # Creative Webcam Video Effects File
	'DVT' , # Digital Video Transcript
	'FPDX' , # Utilius Fairplay Project
	'VBC' , # VodBurner Crude Video File
	'TPR' , # TMPGEnc Project File
	'TDA3MT' , # DivX Author Template File
	'LFPACKAGE' , # LumaFusion Project Package
	'SMK' , # Smacker Compressed Movie File
	'CMPROJ' , # Camtasia macOS Project
	'ALPX' , # ACDSee Luxea Video Editor Project
	'ZM1' , # ZSNES Movie #1 File
	'PGI' , # Video Recording File
	'PEG' , # Callipeg Animation Project
	'VDR' , # VirtualDub Signpost File
	'M21' , # MPEG-21 File
	'MPGINDEX' , # Adobe MPEG Index File
	'TP0' , # Mascom PVR Video File
	'RCREC' , # LogMeIn Remote Control Session Recording
	'IVF' , # Indeo Video Format File
	'ISMC' , # Smooth Streaming Client Manifest File
	'VIDEOMELD' , # VideoMeld Project
	'BMK' , # PowerDVD MovieMark File
	'ROQ' , # RoQ Full Motion Video
	'HKM' , # Havok Movie File
	'PXV' , # Pixbend Media File
	'RVL' , # muvee Reveal X Project File
	'SUB' , # VobSub Subtitles File
	'M21' , # AXMEDIS MPEG-21 File
	'AMV' , # Actions Media Video
	'RVID' , # Snapz Pro X Recorded Video File
	'EVO' , # HD DVD Video File
	'FLH' , # FLIC Animation File
	'CMMTPL' , # Camtasia MenuMaker Template
	'INSV' , # Insta360 Panoramic Video
	'WTV' , # Windows Recorded TV Show File
	'AEGRAPHIC' , # Adobe After Effects Graphic
	'GXF' , # General eXchange Format File
	'PHOTOSHOW' , # Roxio PhotoShow Project
	'SEC' , # Samsung Security Video File
	'WMX' , # Windows Media Redirector File
	'DCE' , # DriveCam Event
	'FFM' , # FFmpeg Stream File
	'DAD' , # IMS200 Video Record File
	'PAR' , # Dedicated Micros DVR Recording
	'RMD' , # RED Metadata File
	'PNS' , # Pelco CCTV Video File
	'YOG' , # Outerra Captured Video File
	'QTM' , # Apple QuickTime Movie
	'WOT' , # WebEx Recording File
	'PVR' , # Wintal PVR Video File
	'CLK' , # ClickView Video File
	'NSV' , # Nullsoft Streaming Video File
	'CREC' , # Lync Video File
	'TTXT' , # MPEG-4 Timed Text Subtitles File
	'VIX' , # Lightworks Video Index File
	'XEL' , # Expression Encoder Live Encoding File
	'THP' , # Wii/GameCube Video File
	'FCPROJECT' , # Final Cut Pro X Project File
	'XFL' , # Animate Uncompressed Animation
	'SMI' , # SMIL Presentation
	'PSSD' , # PhotoSuite Slide Show File
	'DV-AVI' , # Microsoft DV-AVI Video File
	'AV' , # Video Surveillance File
	'VSE' , # AVTECH CCTV Video
	'SSA' , # Sub Station Alpha Subtitle File
	'GIFV' , # GIF Video File
	'GCS' , # Cineform Studio Video Project
	'M1PG' , # iFinish Video Clip
	'BLZ' , # TeamViewer Blizz Session File
	'QSV' , # iQIYI Video File
	'BYU' , # Brigham Young University Movie File
	'VLAB' , # VisionLab Studio Project File
	'TOD' , # JVC Everio Video Capture File
	'SEQ' , # NorPix StreamPix Sequence
	'VS4' , # AVTECH CCTV Video Surveillance File
	'FCARCH' , # Final Cut Pro Archive
	'PROQC' , # ProPresenter Quartz Composition
	'IMOVIEMOBILE' , # iOS iMovie Project
	'VFW' , # Video for Windows
	'BS4' , # Mikogo Session Video Recording
	'LVF' , # DVR LVF Video File
	'MOD' , # Camcorder Recorded Video File
	'MPLS' , # Blu-ray Movie Playlist File
	'MODD' , # Sony Video Analysis File
	'KUX' , # Youku KUX Video
	'VIEWLET' , # Qarbon Viewlet
	'QTZ' , # Quartz Composer File
	'VDO' , # VDOLive Media File
	'VF' , # VEGAS Movie Studio Project
	'AECAP' , # After Effects Text Template
	'PRO4DVD' , # ProPresenter DVD Clips Document
	'AXM' , # AXMEDIS Object
	'FLX' , # FLIC Animation
	'DIF' , # Digital Interface Format
	'CMREC' , # Camtasia Recording
	'AXP' , # Pinnacle Studio Movie Project
	'MXV' , # MAGIX Video
	'IMOVIEPROJECT' , # iMovie Project
	'H265' , # H.265 Video
	'VEM' , # Meta Media Video E-Mail File
	'EL8' , # Electa Live Classroom Session Recording
	'WSVE' , # Wondershare Filmora for Mac Project File
	'SPRYZIP' , # LumaFusion Legacy Project
	'CMMP' , # Camtasia MenuMaker Project
	'AXV' , # Annodex Video File
	'DLX' , # Sony VDU Video File
	'JTS' , # Cyberlink AVCHD Video File
	'XEJ' , # Expression Encoder Job File
	'EZT' , # EZTitles Subtitles File
	'USF' , # Universal Subtitles Format File
	'PRO' , # ProPresenter Export File
	'EXP' , # DX Series Exported Video File
	'SMI' , # SAMI Subtitles File
	'FBZ' , # FlashBack Screen Recorder Movie
	'MOFF' , # Sony Video Data File
	'DMSS' , # VideoWave SlideShow Project File
	'VCV' , # ViewCave Video File
	'EYETV' , # EyeTV Recording File
	'PVA' , # PVA Video File
	'MQV' , # Sony Movie Format
	'AVS' , # AviSynth Script
	'RCPROJECT' , # iMovie '08 Project
	'WGI' , # Wildgame Innovations Video File
	'SVI' , # Samsung Video File
	'EYE' , # Eyemail Video Recording File
	'AVR' , # EverFocus Recording File
	'SKM' , # SK Telecom 3GP Video
	'MVY' , # Video easy Project File
	'CAM' , # MSN Messenger Webcam Recording
	'H266' , # H.266 (VVC) Video
	'ANYDESK' , # AnyDesk Remote Session Recording
	'RTS' , # RealPlayer Streaming Media
	'AVS' , # AVS Workgroup of China Video
	'DRC' , # BBC Dirac Video
	'MEPJ' , # Movavi Video Editor Project
	'WFSP' , # Wondershare Fantashow Project
	'CAMTEMPLATE' , # Camtasia Template
	'STL' , # European Broadcasting Union Subtitles File
	'VSR' , # CPCAM CCTV Recording
	'VP5' , # On2 VP5 Video File
	'CIP' , # Cavena Subtitles File
	'CX3' , # CMS Recorded Video File
	'VMLF' , # VAIO Video File
	'VMLT' , # VAIO Video File
	'ANX' , # Annodex Exchange Format File
	'DMSD3D' , # Roxio MyDVD 3D Project
	'MGJSON' , # After Effects Motion Graphics JSON file
	'PRO5DVD' , # ProPresenter DVD Clips Document
	'QTINDEX' , # Adobe QTIndex File
	'AVC' , # Avid Media Composer Script
	'GVP' , # Google Video Pointer
	'MP21' , # MPEG-21 Multimedia File
	'AV3' , # Arbitrator Video Surveillance File
	'RP' , # RealPix Clip
	'SML' , # SMIL Slideshow Presentation
	'TID' , # AVCHD Thumbnail File
	'H263' , # H.263 Video
	'JNR' , # JoinNet Recording File
	'PMP' , # PlayStation Portable Movie
	'VFT' , # VideoStudio Filter File
	'VSH' , # VideoStudio HTML5 Project
)

# noinspection SpellCheckingInspection
image_3d_set = (
	'BBMODEL' , # Blockbench 3D Model
	'HIPNC' , # Houdini Apprentice File
	'GH' , # Grasshopper Binary Definition
	'CRZ' , # Compressed Poser Character Rigging File
	'MESH' , # Godot Engine 3D Mesh File
	'IAVATAR' , # iClone Avatar
	'DDP' , # DreamPlan Home Design Project
	'MD5ANIM' , # id Tech 4 Model Animation File
	'PART' , # PartDesigner Part
	'IRR' , # Irrlicht 3D Scene
	'VRM' , # Virtual Reality Model
	'C4D' , # Cinema 4D Model
	'DUF' , # DAZ User File
	'FSH' , # Fragment Shader File
	'MCSG' , # MagicaCSG 3D Model
	'DFF' , # RenderWare Model File
	'BLEND' , # Blender 3D Data File
	'MAKERBOT' , # MakerBot Print File
	'M3D' , # 3D Model File
	'IV' , # Open Inventor Scene Graph File
	'DSV' , # DAZ Studio UV Mapping File
	'THING' , # MakerBot Thing File
	'ATM' , # Vue Atmospheres File
	'PHY' , # 3ds Max Physique File
	'ZT' , # Mental Ray Image Depth File
	'MC5' , # Poser 5 Material File
	'CMDB' , # Ansys CFD Mesh
	'PMX' , # MikuMikuDance Model File
	'CFG' , # Cal3D Model Configuration File
	'SMD' , # Valve Studiomdl Data File
	'MDL' , # Warcraft 3 3D Model Text File
	'FX' , # Direct3D Effects File
	'MIX' , # 3ds Max Motion Mixer File
	'XAF' , # 3ds Max XML Animation File
	'LXF' , # LEGO Digital Designer Model File
	'X' , # DirectX Model File
	'NM' , # Space Engine Nebula Model File
	'MU' , # Kerbal Space Program Mesh File
	'P3D' , # Peak3D 3D Graphics File
	'AN8' , # Anim8or File
	'MTZ' , # Compressed MetaStream Scene File
	'MDX' , # Warcraft 3 Model File
	'USD' , # Universal Scene Description Format
	'3DS' , # 3D Studio Scene
	'PSA' , # Unreal Engine Skeletal Animation File
	'AMF' , # Additive Manufacturing File
	'VOX' , # Voxlap Voxel Model File
	'FLT' , # OpenFlight Scene Description File
	'CSO' , # Compiled Shader Object File
	'M3D' , # DIALux 3D Object File
	'X3D' , # Xara3D Project
	'3D2' , # Stereo CAD-3D 2.0 Image File
	'GLTF' , # GL Transmission Format File
	'3MF' , # 3D Manufacturing File
	'OBP' , # Bryce Object File
	'MESH' , # 3D Mesh Model
	'WFT' , # GTA 4 Car Model File
	'MD5MESH' , # id Tech 4 3D Mesh File
	'N3D' , # Nuclear 3D File
	'P3L' , # Adobe Photoshop Light Preset File
	'PRM' , # Re-Volt Model File
	'P4D' , # Pix4D Project
	'TRACE' , # TRACES 3D Scene
	'BR7' , # Bryce 7 Scene File
	'MD5CAMERA' , # id Tech 4 Model Camera File
	'IVE' , # OpenSceneGraph Binary File
	'3DXML' , # Dassault Systemes 3D XML File
	'HDZ' , # Compressed Poser Hand Pose File
	'KFM' , # Gamebryo 3D Model File
	'REALITY' , # Reality 3D Scene Format
	'ATL' , # Artlantis 3D Scene File
	'PPZ' , # Compressed Poser Prop File
	'CCP' , # CopperCube JavaScript File
	'GMF' , # Leadwerks Game Model File
	'DAE' , # Digital Asset Exchange File
	'MA' , # Maya ASCII Scene
	'IRRMESH' , # Irrlicht Static Mesh File
	'VPD' , # MikuMikuDance Vocaloid Pose Data File
	'TME' , # Bryce Time File
	'ANIMSET' , # FaceFX Animation Set File
	'FACEFX' , # FaceFX Actor File
	'E57' , # LIDAR Point Cloud Data File
	'3D4' , # Stereo CAD-3D 2.0 Image File
	'T3D' , # Swift 3D Document
	'PLY' , # Polygon Model
	'MHM' , # MakeHuman Model
	'BIP' , # Character Studio Biped File
	'V3D' , # Visual3D.NET Data File
	'SHAPR' , # Shapr3D Model
	'STEL' , # Stella Polyhedron
	'LLM' , # Linden Lab Mesh File
	'PP2' , # Poser Prop File
	'GHX' , # Grasshopper XML Definition
	'GRS' , # Gravity Sketch Sketch File
	'TILT' , # Tilt Brush Sketch
	'SH3D' , # Sweet Home 3D Design
	'XMF' , # Cal3D XML Mesh File
	'Z3D' , # ZModeler 3D File
	'ANIM' , # Maya Animation File
	'DS' , # DAZ Studio 1/2 Script
	'IFC' , # Industry Foundation Classes File
	'PSK' , # Unreal Engine Skeletal Mesh
	'SKP' , # SketchUp Document
	'OBJ' , # Wavefront 3D Object File
	'T3D' , # Unreal Engine 3D Object File
	'PZ2' , # Poser Pose File
	'PKG' , # Midtown Madness 2 3D Model
	'3DA' , # 3D Assembly File
	'RCS' , # RandomControl Scene File
	'LND' , # 3D Landscape File
	'SDB' , # SAP2000 Model File
	'IGI' , # Indigo Image File
	'GLSL' , # GLSL Shader File
	'PL0' , # 3D Home Architect Foundation Floor Plan
	'X3G' , # 3D Print File
	'LXO' , # Modo 3D Image
	'DWF' , # Design Web Format File
	'PKG' , # CoCreate OneSpace Modeling Package
	'USDZ' , # Universal Scene Description Zipped Format
	'ANM' , # 3D Animation File
	'P5D' , # Planner 5D Project
	'FCP' , # Phantasy Star Online 2 Female Cast File
	'CG' , # Cg Program
	'MNM' , # Character Studio Marker Name File
	'ANIM' , # Unity Animation File
	'BLK' , # BLK360 Data File
	'PRC' , # Product Representation Compact File
	'MGF' , # Materials and Geometry Format
	'MXM' , # Maxwell Material File
	'MEB' , # PRO100 3D Interior Catalog Element
	'DN' , # Adobe Dimension Document
	'DES' , # Corel Designer File
	'TRI' , # FaceGen Polygonal Model File
	'MB' , # Maya Binary Scene
	'OFF' , # Object File Format
	'A8S' , # Anim8or Script
	'SH3F' , # Sweet Home 3D Model Library
	'CHR' , # CryENGINE Character File
	'SPV' , # SPIR-V Module
	'CG3' , # Cabri 3D Document
	'MRML' , # 3D Slicer Scene Description File
	'MQO' , # Metasequoia Document
	'MXS' , # Maxwell Studio Scene File
	'XSI' , # Softimage XSI 3D Image
	'SIS' , # arivis ImageCore File
	'P3M' , # Adobe Photoshop Material Preset File
	'3DM' , # Rhino 3D Model
	'HIP' , # Houdini Project File
	'SC4MODEL' , # SimCity 4 Model File
	'M3G' , # Mobile 3D Graphics 3D Model
	'A2C' , # Alice Object File
	'PMD' , # Poser Binary Morph File
	'NIF' , # Gamebryo Model File
	'MDD' , # Point Oven Deformation Data File
	'VSH' , # Vertex Shader File
	'B3D' , # Blitz3D Entity Model File
	'A3D' , # Alternativa Player 3D Export File
	'ALBUM' , # GLC_Player Album File
	'3DP' , # 123D Catch Photo Scene Data File
	'MS3D' , # MilkShape 3D Model
	'BIF' , # Bifrost Mesh Cache
	'HRZ' , # Compressed Poser Hair File
	'V3V' , # Vector Art 3D Model File
	'FCZ' , # Compressed Poser Face Pose File
	'OBZ' , # Compressed 3D Object File
	'FPF' , # IKEA Home Planner File
	'TCN' , # Techne Model File
	'DSA' , # DAZ Studio 3+ Script
	'GLB' , # Binary GL Transmission Format File
	'MSH' , # Godot Engine 3D Mesh File
	'ARFX' , # Spark AR Effect
	'MCZ' , # Compressed Poser Material File
	'FBX' , # Autodesk FBX Interchange File
	'WRP' , # Geomagic 3D Wrap File
	'BLD' , # Skyscraper Simulator Building Script
	'SHP' , # 3D Studio Shape
	'OL' , # Valve Hammer Object Layout File
	'3DL' , # LightConverse 3D Model File
	'FXT' , # FaceFX Actor Template File
	'BRO' , # Broadleaf Tree Model
	'3DMF' , # QuickDraw 3D Metafile
	'CGFX' , # CgFX Shader File
	'AREXPORT' , # Spark AR Effects Package
	'FG' , # FaceGen Project File
	'CHR' , # 3ds Max Characters File
	'W3D' , # Adobe Shockwave 3D File
	'U3D' , # Universal 3D File
	'GEO' , # VRML Geography File
	'CMOD' , # Celestia Model
	'GMT' , # rFactor Model File
	'UMS' , # Rune 3D Mesh File
	'VISUAL_PROCESSED' , # World of Tanks 3D Model Surface File
	'KTZ' , # Kahootz Project File
	'VS' , # Vertex Shader File
	'NXS' , # Nexus 3D Model
	'XR' , # Xuver 3D Scene File
	'ARM' , # ArmorPaint Asset
	'PAR' , # Solid Edge Part File
	'VROID' , # VRoid Studio Avatar
	'GRN' , # Granny 3D File
	'MC' , # Molecular Constructor Molecule File
	'MCX-8' , # Mastercam X8 Model
	'FXL' , # CryENGINE Facial Expression Library
	'HLSL' , # High Level Shader Language File
	'FXS' , # messiahStudio Scene File
	'MUD' , # Mudbox 3D Scene File
	'CRF' , # Cal3D Binary Materials File
	'BVH' , # Biovision Hierarchy Animation File
	'MTL' , # OBJ Material Template Library File
	'CMF' , # Cal3D Binary Mesh File
	'CM2' , # Poser Camera Set File
	'LWS' , # LightWave 3D Scene
	'PMD' , # MikuMikuDance Polygon Model Data
	'BR4' , # Bryce 4 Scene File
	'FNC' , # Vue Functions File
	'C3D' , # C3D Model File
	'VVD' , # Vivid 3D Scanner Element File
	'MTX' , # MetaStream Scene File
	'S' , # Microsoft Train Simulator Shape File
	'ACT' , # Genesis3D Actor File
	'D3D' , # Game Maker 3D File
	'LWO' , # LightWave 3D Object
	'MSH' , # Orbiter 3D Mesh File
	'SRF' , # LightWave Surface File
	'MAX' , # 3ds Max Scene File
	'FP' , # Fragment Program File
	'IGES' , # IGES Drawing
	'GLF' , # Space Engine Shader File
	'YDL' , # PowerFlip YAODL 3D Image File
	'CPY' , # 3ds Max Copy Track File
	'VMD' , # Vocaloid Motion Data File
	'SBSAR' , # Substance Archive
	'VP' , # Vertex Program File
	'SGN' , # Signet Bureau DRM File
	'BR6' , # Bryce 6 Scene File
	'QC' , # Half-Life Model Compiler Script
	'VRL' , # VRML Virtual World
	'DSB' , # DAZ Studio Binary Script
	'CSD' , # Manga Studio Scene File
	'CCB' , # CopperCube 3D Scene
	'AOF' , # Artlantis Object File
	'VEG' , # Vue Vegetation file
	'SESSION' , # CATIA 4 Session File
	'VUE' , # Vue Scene File
	'BIO' , # Mudbox Bio 3D File
	'F3D' , # Fusion 360 Archive File
	'CSM' , # Character Studio Marker File
	'CAL' , # 3ds Max Pose Adjustment File
	'EGG' , # Panda3D Model File
	'IGS' , # Indigo Renderer Scene File
	'WRL' , # VRML World
	'3DF' , # 3D Format
	'MAXC' , # 3ds Max Container File
	'BRG' , # Age of Mythology Model File
	'PGAL' , # Photo! 3D Album Gallery
	'PL2' , # 3D Home Architect Second Level Floor Plan
	'CR2' , # Poser Character Rigging File
	'VOB' , # Vue Objects File
	'GMMOD' , # Game Maker 3D Model File
	'C3Z' , # OpenQwaq 3D Model File
	'P3R' , # Adobe Photoshop Render Settings Preset File
	'3DC' , # 3DCT 3D Image
	'FXM' , # messiahStudio Motion File
	'PZZ' , # Compressed Poser Scene File
	'TVM' , # TV3D Model
	'DSF' , # DAZ Studio Asset File
	'PRV' , # Vue Preview File
	'NFF' , # Neutral File Format
	'OGF' , # S.T.A.L.K.E.R. Model File
	'PZ3' , # Poser Scene File
	'IK' , # CryENGINE IK Animation File
	'MESH' , # DirectX Mesh File
	'DAZ' , # DAZ Studio 3D Scene
	'TMD' , # PlayStation Game Model File
	'VISUAL' , # World of Tanks 3D Model Surface File
	'FUN' , # Cubify Invention File
	'JCD' , # JewelCAD Design
	'XOF' , # Reality Lab 3D Image File
	'CLARA' , # Clara Archive
	'FXA' , # OC3 Entertainment FaceFX Actor File
	'SKL' , # Maya Skeleton File
	'PREVIZ' , # FrameForge Storyboard Studio Previsualization
	'3DC' , # 3DCrafter Scene File
	'SM' , # Space Engine 3D Mesh File
	'DSE' , # DAZ Studio Encrypted Script
	'P2Z' , # Compressed Poser Pose File
	'DBM' , # DAZ Brick Material File
	'MC6' , # Poser Material Collection File
	'HR2' , # Poser Hair File
	'MDG' , # Digital Geometric Model File
	'CMZ' , # Compressed Poser Camera Set File
	'KMCOBJ' , # Kinemac Sprite Object
	'SMC' , # Solibri Model Checker File
	'DFS' , # BrainSuite Surface File
	'S3G' , # MakerBot 3D Print File
	'KMC' , # Kinemac Animation File
	'FPJ' , # CryENGINE Facial Editor Project File
	'PSKX' , # Unreal Engine Static Mesh
	'SPLINE' , # Spline 3D Model
	'ZMBX' , # Mecabricks Zip-Compressed 3D Scene
	'BIP' , # KeyShot 3D File
	'FRY' , # fryrender Scene File
	'ARPATCH' , # Spark AR Patch Asset
	'DRF' , # VIZ Render File
	'ARPROJPKG' , # Spark AR Project Package
	'V3O' , # Emergency 3 and 4 Model File
	'REAL' , # Aero Experience
	'HXN' , # Hexagon Model File
	'PRIMITIVES_PROCESSED' , # World of Tanks 3D Model File
	'PRO' , # TurboFloorPlan 3D Design File
	'3DMK' , # 123D Make Project File
	'J3O' , # jMonkeyEngine 3D Scene
	'XMM' , # 3ds Max XML Animation Map File
	'ATF' , # Alternativa Texture File
	'THL' , # Total 3D Home Design File
	'DMC' , # Mimic Configuration File
	'3DW' , # 3D World Studio Environment
	'PREFAB' , # Unity Prefab File
	'VTX' , # Anim8or 3D Model
	'XPR' , # Pro/ENGINEER Part Instance Accelerator File
	'CAS' , # Total War Model File
	'AOI' , # Art of Illusion 3D Scene
	'M3' , # Blizzard MDX3 Model File
	'BR5' , # Bryce 5 Scene File
	'PL1' , # 3D Home Architect Floor Plan
	'DDD' , # GLBasic 3D Data File
	'FIG' , # 3ds Max Figure File
	'SBFRES' , # Compressed Breath of the Wild Model File
	'BR3' , # Bryce 3 Scene File
	'DBS' , # GAMBIT Mesh File
	'XRF' , # Cal3D XML Materials File
	'FP3' , # FloorPlan 3D Design File
	'MAT' , # 3ds Max Materials File
	'CMS' , # Car Mechanic Simulator 3D Car Model File
	'DBC' , # DAZ Brick Camera File
	'N2' , # Nitrous Minecraft GLSL Shader File
	'LP' , # Lightscape Preparation File
	'JAS' , # Cheetah3D Model File
	'WOW' , # SparkMaker 3D Print File
	'TS1' , # VirtualBus Texture Description File
	'RFT' , # Revit Family Template File
	'FC2' , # Poser Face Pose File
	'OCT' , # Radiance Octree File
	'TRI' , # Triangle Mesh File
	'BSK' , # Bryce Skies File
	'TPS' , # Bryce Tree Shape File
	'PIGS' , # Packaged Indigo Renderer Scene File
	'GLM' , # Ghoul 2 Model File
	'3DX' , # Rhino 3D Model File
	'MP' , # Maya PLE Project File
	'DSI' , # DAZ Studio Layered Image Set
	'LDM' , # VolumeViz Multi-Resolution Volume File
	'RAD' , # VRAD Lights File
	'SI' , # Softimage Image File
	'MPJ' , # messiahStudio Project File
	'EXP' , # CATIA 4 Export File
	'3D' , # Stereo CAD-3D Image File
	'GLSLESF' , # OpenGL ES Fragment Shader
	'LPS' , # Bryce Leaf Shape File
	'EGM' , # FaceGen Statistical Shape Model
	'RAD' , # Radiance Scene File
	'FSQ' , # CryENGINE Facial Editor Sequence File
	'R3D' , # RISA-3D Model File
	'STO' , # PRO100 3D Interior Design Project
	'PRO' , # Professional Home Design Project
	'YAODL' , # PowerFlip 3D Image File
	'VSO' , # Compiled Vertex Shader File
	'ASAT' , # Assemble SAT 3D Model File
	'XV0' , # Lattice XVL Structure File
	'DSO' , # DAZ Studio Object File
	'DBL' , # DAZ Brick Light File
	'LTZ' , # Compressed Poser Light Set File
	'PAT' , # 3D Patch File
	'PRIMITIVES' , # World of Tanks 3D Model File
	'MMPP' , # Material Maker Paint Project
	'DIF' , # Torque Game Engine Model File
	'NSBTA' , # Nintendo DS Texture Animation File
	'CHRPARAMS' , # CryENGINE Character Parameters File
	'IGM' , # Indigo Renderer Material File
	'MBX' , # Mecabricks 3D Scene
	'CGA' , # Crytek Geometry Animation File
	'VAC' , # MikuMikuDance Accessory Settings File
	'SCW' , # Supercell 3D Model
	'XSF' , # Cal3D XML Skeleton File
	'CSF' , # Cal3D Binary Skeleton File
	'XAF' , # Cal3D XML Animation File
	'BBSCENE' , # Blockbench 3D Scene
	'S3O' , # Spring 3D Object
	'PIGM' , # Packed Indigo Renderer Material File
	'ZVF' , # Zipped Intergraph Smart Review Project
	'SVF' , # Intergraph Smart Review Project
	'CAL' , # CryENGINE Character Animation List
	'3DV' , # 3D VRML World
	'TMO' , # 3DCG Animation and Pose File
	'TGO' , # Terragen Object File
	'P21' , # Express STEP Data Model File
	'FPE' , # FPS Creator Entity File
	'HD2' , # Poser Hand Pose File
	'DSI' , # Double-Precision Spectral Image
	'IGMESH' , # Indigo Renderer Mesh File
	'EMCAM' , # Mastercam Learning Edition Model
	'3DON' , # Scrutinize 3D Mesh File
	'CAF' , # Cal3D Binary Animation File
	'MOT' , # LightWave Motion File
	'LT2' , # Poser Light Set File
	'ANIMSET_INGAME' , # FaceFX In-Game Animation Set File
	'BRK' , # Draw Bricks Structure
	'DSD' , # DAZ Studio Object Morph File
	'TDDD' , # 3D Data Description File
	'FBM' , # Marine Vessel Model File
	'VMO' , # Virtools Media Object File
	'BTO' , # Bryce Tree File
	'FACEFX_INGAME' , # FaceFX In-Game Actor File
	'ARPROJ' , # Spark AR Project
	'FUSE' , # Adobe Fuse 3D Model
	'RDS' , # Ray Dream Studio Scene File
	'STC' , # Shade to Manga Studio File
	'WRZ' , # Compressed VRML World File
	'ANIM' , # Antibody Animation File
	'RIG' , # Antibody Model Rig File
	'RAY' , # Rayshade Image
)

# noinspection SpellCheckingInspection
raster_image_set = (
	'BIF' , # Ventana Whole Slide Image
	'JXL' , # JPEG XL Image
	'PXD' , # Pixelmator Pro Image
	'SPRITE2' , # Scratch 2.0 Sprite File
	'XPM' , # X11 Pixmap Graphic
	'ICON' , # Icon Image
	'AFPHOTO' , # Affinity Photo Document
	'ASE' , # Aseprite Sprite File
	'PSDC' , # Adobe Photoshop Cloud Document
	'LRPREVIEW' , # Adobe Lightroom Preview File
	'8CI' , # TI-84 Plus C Pic Vars Image
	'SUMO' , # Sumo Paint Image
	'QOI' , # Quite OK Image Format
	'GIF' , # Graphical Interchange Format File
	'MNR' , # AutoCAD Menu Resource File
	'SPRITE3' , # Scratch 3.0 Sprite File
	'PSD' , # Adobe Photoshop Document
	'TBN' , # Kodi Thumbnail Image
	'PTEX' , # Ptex Texture File
	'PLP' , # PixelLab Project
	'SNAGX' , # Snagit 2022 Capture
	'AVATAR' , # Google Talk Avatar File
	'BPG' , # Better Portable Graphics Image
	'PNG' , # Portable Network Graphic
	'YSP' , # BYOB Sprite File
	'SPRITE' , # Scratch Sprite File
	'TGA' , # Targa Graphic
	'FLIF' , # Free Lossless Image Format File
	'TPF' , # TexMod Package File
	'DDS' , # DirectDraw Surface Image
	'PISKEL' , # Piskel Sprite
	'DIB' , # Device-Independent Bitmap Image
	'SAI' , # PaintTool SAI Image
	'SPR' , # Half-Life Sprite
	'PDN' , # paint.net Image
	'JPEG' , # JPEG Image
	'HDR' , # High Dynamic Range Image File
	'PZP' , # PhotoSuite Project File
	'VICAR' , # VICAR Image File
	'SIX' , # Sixel Image
	'PPP' , # PhotoPad Project
	'PSP' , # Pixel Studio Project
	'NWM' , # Sony NWM Display Screen File
	'CT' , # Scitex Continuous Tone Image
	'SLD' , # AutoCAD Slide File
	'IPV' , # ibis Paint Artwork
	'LINEA' , # Linea Sketch Drawing
	'JLS' , # JPEG-LS Image
	'PAM' , # Portable Arbitrary Map Image
	'SKTZ' , # Sony Sketch Drawing
	'WIC' , # Java Wavelet Image
	'SKITCH' , # Skitch Image
	'OC4' , # openCanvas 4 Event File
	'IPICK' , # iPick Football Image
	'APS' , # Greeting Card Studio Project File
	'OPLC' , # Nokia Operator Logo File
	'PCX' , # Paintbrush Bitmap Image File
	'CLIP' , # Clip Studio Paint Illustration
	'KRA' , # Krita Image Document
	'PM' , # Unix XV Graphic File
	'JPG' , # JPEG Image
	'HEIF' , # High Efficiency Image Format
	'WEBP' , # WebP Image
	'JPS' , # Stereo JPEG Image
	'OTA' , # OTA Bitmap Image
	'LIP' , # Clip Studio Paint File
	'TFC' , # Unreal Engine 3 Texture File Cache
	'PWP' , # PhotoWorks Image File
	'POV' , # POV-Ray Raytracing Format
	'MNG' , # Multiple-Image Network Graphic
	'EXR' , # OpenEXR Image
	'ITC2' , # iTunes Cover Flow Data File 2
	'XCF' , # GIMP Image File
	'FITS' , # Flexible Image Transport System File
	'WBZ' , # Webshots Download Picture File
	'LZP' , # LazPaint Image
	'PSDX' , # Photoshop Touch Document
	'73I' , # TI-73 Screenshot File
	'WBC' , # Webshots Collection File
	'DJVU' , # DjVu Image
	'LSA' , # Xiaomi Gallery Hidden Photo
	'USERTILE-MS' , # Windows 8 User Tile File
	'PPF' , # Picture Publisher Image File
	'CDC' , # AutoCAD DesignCenter Preview Cache File
	'CPC' , # CPC Compressed Image File
	'TIFF' , # Tagged Image File Format
	'BMP' , # Bitmap Image
	'PMG' , # Adobe Photoshop Photomerge Panoramic Composition File
	'OZJ' , # MU Online Image File
	'ACCOUNTPICTURE-MS' , # Windows 8 Account Picture File
	'CAN' , # Canon Navigator Fax Document
	'RGF' , # LEGO MINDSTORMS EV3 Robot Graphics File
	'PBM' , # Portable Bitmap Image
	'2BP' , # Pocket PC Bitmap Image File
	'JPC' , # JPEG 2000 Code Stream File
	'SNAG' , # Snagit for Windows Capture
	'ECW' , # Enhanced Compression Wavelet Image
	'TM2' , # PlayStation 2 Texture Image Map
	'CDG' , # Compact Disc Plus Graphics Image
	'MDP' , # FireAlpaca Image
	'STEX' , # Godot Engine 3 StreamTexture File
	'MPF' , # Microsoft Media Package FIle
	'PI2' , # Portrait Innovations Photo
	'PX' , # Pixel Image File
	'VNA' , # JVC JLIP Image
	'PDD' , # Adobe PhotoDeluxe Image
	'AWD' , # Microsoft Fax Document
	'PFI' , # PhotoFiltre Studio Image
	'PSPIMAGE' , # PaintShop Pro Image
	'NOL' , # Nokia Operator Logo File
	'PNI' , # Popnoggin Image File
	'XBM' , # X BitMap Graphic
	'MSP' , # Microsoft Paint Bitmap Image
	'NLM' , # Nokia Logo File
	'DRZ' , # Draz Paint Image
	'PNC' , # Panasonic Network Camera Image
	'KFX' , # Kofax Image File
	'CMR' , # U.S. Sectional Chart Map File
	'FF' , # Farbfeld Image
	'PIXELA' , # Pixela Project
	'URT' , # Utah Raster Toolkit File
	'ICN' , # Windows Icon File
	'HEIC' , # High Efficiency Image Format
	'RPF' , # Rich Pixel Format File
	'VRIMG' , # V-Ray Image File
	'TN' , # LG Phone Image
	'DGT' , # DST Thumbnail File
	'TG4' , # Tiled Group 4 Raster Image File
	'APNG' , # Animated Portable Network Graphic
	'JNG' , # JPEG Network Graphic
	'FBM' , # Fuzzy Bitmap Image
	'FIL' , # Symbian Application Logo File
	'VPE' , # Photoshop Vanishing Point Export File
	'FPX' , # FlashPix Bitmap Image File
	'JPF' , # JPEG 2000 Image
	'SPP' , # Serif PhotoPlus Picture File
	'FAC' , # FACE Image File
	'RSR' , # Poser Model Preview File
	'ASEPRITE' , # Aseprite Sprite File
	'DTW' , # Desktop Wallpaper
	'PAT' , # Pattern File
	'PGM' , # Portable Gray Map Image
	'JPE' , # JPEG Image
	'PPM' , # Portable Pixmap Image
	'IWI' , # Infinity Ward Texture File
	'BMQ' , # Re-Volt Mipmap File
	'KTX' , # Khronos Texture
	'I3D' , # Houdini Image 3D File
	'GIM' , # PlayStation Portable Image File
	'PTG' , # ArtRage Painting
	'MDP' , # MediBang Paint Pro Image
	'PSP' , # PaintShop Pro Image
	'TIF' , # Tagged Image File
	'THM' , # Video Thumbnail File
	'PSB' , # Photoshop Large Document Format
	'CAN' , # Canvas Painting
	'OTB' , # Nokia Over The Air Bitmap Image
	'ART' , # Visual Paradigm Online Artwork
	'OZT' , # MU Online Image File
	'CTEX' , # Godot Engine 4 Compressed Texture
	'JBIG2' , # JBIG2
	'SPH' , # MikuMikuDance Sphere Mapping File
	'WBM' , # Wireless Bitmap Image
	'WB2' , # Webshots Picture File
	'BMZ' , # Compressed Bitmap Image
	'LJP' , # Lossless JPEG Image
	'SPA' , # MikuMikuDance Sphere Mapping File
	'CALS' , # CALS Raster Graphic File
	'GMBCK' , # Game Maker Background Image File
	'PP5' , # Picture Publisher 5 Image File
	'J2K' , # JPEG 2000 Image
	'LB' , # Lens Blur Project File
	'HIF' , # High Efficiency Image Format
	'1SC' , # Bio-Rad Quantity One Gel Image
	'G3N' , # Zetafax Fax Image
	'CPD' , # Compressed PhotoDefiner Image File
	'VRPHOTO' , # VR Photo Image
	'KTX2' , # Khronos Texture 2.0
	'MPO' , # Multi Picture Object File
	'VIFF' , # Visualization Image File Format
	'PXO' , # Pixelorama Project
	'WB0' , # Webshots Picture File
	'PNS' , # PNG Stereo Image
	'JXR' , # JPEG XR Image
	'GIH' , # GIMP Image Hose File
	'AVIFS' , # AVIF Image Sequence
	'WBMP' , # Wireless Bitmap Image
	'ZIF' , # Zoomify Image Format File
	'PIC' , # Houdini Raster Image
	'LMNR' , # Luminar 2018 Project File
	'SIG' , # Broderbund Sign File
	'ARR' , # Amber Graphic
	'PIC' , # Generic Picture File
	'INFO' , # ZoomBrowser Image Index File
	'PGF' , # Progressive Graphics File
	'ABM' , # Photo Album
	'SIG' , # QuickTac SIG File
	'HDP' , # HD Photo File
	'PJPG' , # Progressive JPEG Image
	'LBM' , # Deluxe Paint Bitmap Image
	'CIMG' , # CImg Image File
	'BTI' , # Nintendo Texture File
	'PICTCLIPPING' , # Picture Clipping File
	'CE' , # ComputerEyes Image
	'FACE' , # FACE Graphic File
	'SAI2' , # PaintTool SAI Version 2 Image
	'JP2' , # JPEG 2000 Core Image
	'RTL' , # Raster Transfer Language File
	'TEX' , # Texture File
	'JPX' , # JPEG 2000 Image File
	'PXM' , # Pixelmator Image
	'DJV' , # DjVu Image
	'JPG2' , # JPEG 2000 Image
	'QTIF' , # QuickTime Image File
	'CPT' , # Corel PHOTO-PAINT Image
	'VDA' , # Targa Bitmap Image File
	'RIFF' , # Painter Raster Image
	'PE4' , # Photo Explorer Thumbnail Archive
	'PNT' , # MacPaint File
	'PVR' , # POWERVR Texture File
	'AGP' , # ArtGem Project File
	'ILBM' , # Interleaved Bitmap Image
	'OTI' , # OpenDocument Image Template
	'OCI' , # openCanvas Image
	'RCL' , # Recolored Project File
	'PZS' , # PhotoSuite Slide Show File
	'LIF' , # Leica Image File
	'OC3' , # openCanvas 3 Event File
	'KDI' , # KD Player Skin Image File
	'GBR' , # GIMP Brush File
	'UFO' , # Ulead File Object
	'TIF' , # GeoTIFF Image
	'001' , # Fax File
	'VSS' , # Visio Stencil File
	'SID' , # MrSID Image
	'GRO' , # Graphic Object Bitmap
	'SUP' , # Subtitle Bitmap File
	'INT' , # SGI Integer Image
	'RLI' , # RealWorld Graphics Layered Image
	'APD' , # ACDSee Photo Document
	'S2MV' , # StarCraft 2 Map Preview File
	'AWD' , # Artweaver Document
	'GGR' , # GIMP Gradient File
	'CIT' , # Intergraph CCITT Image
	'PRW' , # Artlantis Shader Preview File
	'AIS' , # ACDSee Image Sequence File
	'WB1' , # Webshots Picture File
	'SFC' , # Motic Microscope Image
	'JIA' , # Digital Photo Navigator Album
	'DM4' , # DigitalMicrograph 4 Image
	'GP4' , # CCITT Group 4 Fax File
	'INSP' , # Insta360 Panoramic Image
	'PXD' , # Pixlr Layered Image
	'JPG_LARGE' , # Twitter Large JPEG Image
	'DCM' , # DICOM Image
	'AVIF' , # AVIF Image
	'THUMB' , # JAlbum Thumbnail File
	'MCS' , # Mathcad Image
	'V' , # Subsampled Raw YUV Image
	'PCD' , # Kodak Photo CD Image
	'WI' , # Wavelet Image
	'WDP' , # Windows Media Photo File
	'MBM' , # Kerbal Space Program Texture File
	'PROCREATE' , # Procreate Artwork
	'NEO' , # NeoChrome Bitmap Image
	'HPI' , # Hemera Photo Objects Image File
	'JIF' , # JPEG Interchange Format Image
	'RAS' , # Sun Raster Graphic
	'NCD' , # Nero CoverDesigner File
	'WMP' , # Windows Media Photo File
	'BMC' , # Bitmap Cache File
	'SNAGPROJ' , # Snagit for Mac Capture
	'BMX' , # Siemens Mobile Animations File
	'RIF' , # Painter Raster Image
	'QMG' , # Samsung Theme Graphics File
	'ICA' , # Image Object Content Architecture File
	'PSE' , # Photoshop Elements Photo Project
	'JFI' , # JPEG File Interchange Image
	'KODAK' , # Kodak Photo CD File
	'SPE' , # WinSpec CCD Capture File
	'ITHMB' , # iPod and iPhone Photo Thumbnails File
	'ORA' , # OpenRaster Image File
	'CIN' , # Kodak Cineon Bitmap Image
	'SUN' , # Sun Raster Graphic
	'ZIF' , # Zooming Image Format File
	'MSK' , # Paint Shop Pro Mask File
	'PXZ' , # Pixlr Zipped Layered Image
	'RGB' , # RGB Bitmap
	'SDR' , # SmartDraw Drawing
	'TARGA' , # Targa Graphic
	'MAX' , # PaperPort Scanned Document
	'GMSPR' , # Game Maker Sprite File
	'POP' , # Samsung Popcon Character File
	'IVUE' , # Live Picture IVUE Image
	'WPB' , # openCanvas 1.1 Image
	'GPD' , # Graphic PhotoDefiner Image File
	'PC2' , # Degas Elite Medium Res Image File
	'PC1' , # Degas Elite Low Res Image File
	'SRF' , # Garmin Vehicle Image
	'PP4' , # Picture Publisher Bitmap File
	'T2B' , # CyBook Thumbnail Image
	'PJP' , # Progressive JPEG Image
	'SKM' , # SketchUp Material File
	'SKY' , # Photoshop Sky Replacement Preset
	'PYXEL' , # Pyxel Image Document
	'WBP' , # Webshots Collection File
	'AVB' , # Microsoft Comic Chat Character
	'OZB' , # MU Online Image File
	'PZA' , # PhotoSuite Album File
	'HDRP' , # HDRtist Pro Document
	'OC5' , # openCanvas 5 Event File
	'PIXADEX' , # Pixadex Icon
	'MYL' , # MyLogo Maker Image
	'FPOS' , # Photo Pos Pro Image
	'SPJ' , # Microsoft ICE Panorama Project
	'GCDP' , # Greeting Card Studio Design Project
	'360' , # 360desktop Panorama File
	'KRA~' , # Krita Image Backup
	'BM2' , # Subspace Bitmap File
	'JBIG' , # Joint Bi-level Image Group File
	'MONOPIC' , # Monodraw Picture
	'SVSLIDE' , # Sakura VisionTek Slide Image
	'PSXPRJ' , # PhotoScape X Project
	'AFX' , # Auto FX PhotoGraphic Edges Image
	'IPX' , # IPIX Image
	'FSTHUMB' , # Filmora Thumbnail
	'MONOSNIPPET' , # Monodraw Snippet
	'J2C' , # JPEG 2000 Code Stream
	'CD5' , # Chasys Draw Image File
	'PICNC' , # Houdini 3D Compositing Image
	'MIX' , # Microsoft Image Exchange File
	'AB3' , # PhotoImpact 3 Album
	'QTI' , # QuickTime Image File
	'MBM' , # Multi Bitmap File
	'TJP' , # Tiled JPEG File
	'CID' , # Navigator Chart Image Description File
	'DMI' , # BYOND Dream Maker Icon File
	'PBS' , # PixBuilder Studio Image
	'XWD' , # X Windows Dump Image
	'DPX' , # Digital Picture Exchange File
	'JBF' , # Paint Shop Pro Browser Cache File
	'AGIF' , # Active GIF Creator Project
	'ART' , # AOL Compressed Image File
	'PXR' , # Pixar Image File
	'DCX' , # Zsoft Multi-Page Paintbrush File
	'BSS' , # Resident Evil Background Images File
	'PANO' , # Camera Panoramic Picture
	'PSF' , # PhotoStudio File
	'ZVI' , # Zeiss Vision Image
	'8CA' , # TI-84 Plus C Image Var Format
	'TEXTURE' , # Diesel Engine Game Texture File
	'9.PNG' , # NinePatchDrawable Image
	'RGBA' , # RGBA Bitmap
	'DT2' , # Windows Live Messenger Emoticon Image File
	'BW' , # Black and White SGI Image File
	'CUT' , # Dr. Halo Bitmap Image File
	'PSPBRUSH' , # PaintShop Pro Brush File
	'DICOM' , # DICOM Image
	'APX' , # Ability Photopaint Studio Image
	'SGD' , # GeneSnap Image File
	'SVA' , # Manga Browser Comic File
	'MAC' , # MacPaint Image
	'JIFF' , # JPEG Image File Format
	'DRP' , # DrawPad Image Project
	'NCR' , # NCR Image
	'DDT' , # Age of Mythology Textures File
	'CPG' , # Manga Studio Page File
	'FPPX' , # Microsoft Fresh Paint Painting File
	'SEP' , # Separated Image File
	'RLE' , # Run Length Encoded Bitmap
	'PAC' , # STAD Graphic File
	'U' , # Subsampled Raw YUV Image
	'KIC' , # Kodak Compressed Image File
	'DIC' , # DICOM Image
	'8XI' , # TI-83/84 Plus Picture File
	'GFIE' , # Greenfish Icon Editor Pro Graphic
	'SHG' , # Segmented Hyper-Graphic
	'NDPI' , # Hamamatsu TIFF-like Slide Image
	'PAP' , # PanoramaStudio Project File
	'ODI' , # OpenDocument Image
	'RCU' , # RealWorld Layered Cursor Image
	'SCN' , # Bio-Rad Gel Image
	'JTF' , # JPEG Tagged Interchange Format
	'JB2' , # JBIG2 Image
	'CAL' , # CALS Raster Graphic
	'SKYPEEMOTICONSET' , # Skype Emoticon Set File
	'QIF' , # QuickTime Image File
	'CAM' , # CASIO Digital Camera Picture File
	'JFIF' , # JPEG File Interchange Format
	'INK' , # Pantone Reference File
	'SFW' , # Seattle FilmWorks Image
	'MXI' , # Maxwell Image
	'SVS' , # Aperio ScanScope Slide Image
	'JBR' , # Paint Shop Pro Brushes File
	'OE6' , # openCanvas 6 Event File
	'MIFF' , # Magick Image
	'SCN' , # Leica Whole Slide Image
	'PNM' , # Portable Any Map Image
	'YUV' , # YUV Encoded Image File
	'AIC' , # Advanced Image Coding File
	'JPD' , # Joint PhotoDefiner Image
	'TPS' , # TexturePacker Sprite Sheet File
	'EPP' , # Canon Easy-PhotoPrint Image File
	'SOB' , # OpenOffice.org Impress Bitmap Styles File
	'TUB' , # PaintShop Pro Picture Tube
	'SBP' , # Sketchbook Express iCloud Image
	'ACORN' , # Acorn Image
	'UGA' , # Ulead GIF Animator File
	'JWL' , # Roxio Jewel Case File
	'RVG' , # RVG X-ray Image
	'IVR' , # Image Worlds File
	'UGOIRA' , # Ugoira Animated Image File
	'MRB' , # Multiple Resolution Bitmap File
	'SCT' , # Scitex Continuous Tone Image
	'MAT' , # Vue Material File
	'MIC' , # Image Composer File
	'MIPMAPS' , # Mipmap Texture File
	'WVL' , # Wavelet Image
	'PTK' , # Puntotek Embroidery Design File
	'PTX' , # Pentax RAW Image File
	'PJPEG' , # Progressive JPEG Image
	'RSB' , # Red Storm Bitmap File
	'PI1' , # Degas Low Resolution Image File
	'DDB' , # Device Dependent Bitmap
	'SMP' , # Xionics SMP Image
	'HR' , # TRS-80 Image
	'OMF' , # OMF Interchange Image File
	'BS' , # Printfox Bitmap Image
	'RRI' , # RealWorld Image
	'SGI' , # Silicon Graphics Image
	'JBG' , # Joint Bi-level Image Group File
	'ADC' , # Scanstudio 16 Color Image
	'C4' , # JEDMICS Image File
	'Y' , # Subsampled Raw YUV Image
	'CSF' , # Content Sealed Format
	'MRXS' , # MIRAX Virtual Slide File
	'PSPTUBE' , # PaintShop Pro Picture Tube File
	'DC2' , # Kodak Photo-Enhancer File
	'BRN' , # Cube Graphics File
	'ICPR' , # IconUtils Project File
	'TRIF' , # Tiled Raster Interchange Format
	'VDOC' , # VinylMaster Document
	'CPX' , # Corel CMX Compressed File
	'MIP' , # Multiple Image Print File
	'FSYMBOLS-ART' , # FSymbols Draw ASCII Art
	'PSDB' , # PixelStyle Image
	'DINO' , # Dinopixel Drawing
	'PAL' , # Dr. Halo Color Palette File
	'JAS' , # Paint Shop Pro Compressed Graphic
	'THM' , # GoPro Thumbnail Image
	'PXICON' , # CandyBar Icon
	'ACR' , # American College of Radiology File
	'PC3' , # Degas Elite High Res Image File
	'RIC' , # NXT Image File
	'AAI' , # Dune HD AAI Image
	'PFR' , # Paint Shop Pro Picture Frame File
	'DM3' , # DigitalMicrograph Image
	'COLZ' , # Adobe Collage File
	'FAX' , # Fax Document
	'WBD' , # Webshots Picture File
	'BMF' , # Binary Material File
	'LDOC' , # CutOut Project
	'OIR' , # FV3000 Specimen Image
	'PALM' , # Palm OS Bitmap Image
	'G3F' , # Zetafax Fax Image
	'NCT' , # Nero CoverDesigner Template
	'INV' , # Invivo Image
	'UPF' , # Panono Unstitched Panorama Format
	'PSPFRAME' , # PaintShop Pro Picture Frame File
	'VMU' , # NanoZoomer Uncompressed Virtual Microscope Specimen
	'NPSD' , # NPS Image Editor Document
	'GVRS' , # Gridfour Virtual Raster Store Image
	'T2K' , # Canfield Image File
	'XFACE' , # X-Face Image
	'DC6' , # Diablo II Graphics File
	'PE4' , # PhotoImpact Thumbnail Cache
	'QPTIFF' , # Vectra Whole Slide Image
	'GFB' , # GIFBlast Compressed Image File
	'VIC' , # VICAR Image
	'FRM' , # Painter Frame Stack File
	'DVL' , # Dalim Virtual Library File
	'SCN' , # TreePaint Image
	'SID' , # ScanIt Document
	'CPBITMAP' , # iOS Wallpaper Image
	'TSR' , # TIFF Simple Rare File
	'KPG' , # Kai's Power Goo Graphic
	'CPS' , # Corel Photo House File
	'PTS' , # PTGui Project File
	'BRT' , # Bryce Textures File
	'WPE' , # openCanvas 1.1 Event File
	'TLA' , # Tune Styler Startup Logo
	'PIX' , # BRL-CAD Raw Image File
	'PTX' , # Paint Shop Pro Texture File
	'VST' , # Targa Bitmap Image
	'RGB' , # Q0 Image
	'BLKRT' , # Block Artist Image File
	'IC3' , # High Resolution Imagic Graphics File
	'IC2' , # Medium Resolution Imagic Graphics File
	'IC1' , # Low Resolution Imagic Graphics File
	'IPHOTOPROJECT' , # iPhoto Print Project
)

# noinspection SpellCheckingInspection
vector_image_set = (
	'SVG' , # Scalable Vector Graphic
	'SVGZ' , # Compressed SVG File
	'VSTM' , # Visio Macro-Enabled Drawing Template
	'SHAPES' , # Pixelmator Pro Shapes
	'AI' , # Adobe Illustrator Artwork
	'VSDX' , # Microsoft Visio Drawing
	'GVDESIGN' , # Gravit Designer File
	'CDR' , # CorelDRAW File
	'EP' , # Pencil Document
	'CMX' , # Corel Presentation Exchange Image
	'APM' , # Aldus Placeable Metafile Image
	'FH8' , # FreeHand 8 Drawing File
	'FCM' , # CanvasWorkspace Fabric Cutting Design
	'SLDDRT' , # SolidWorks Drawing Sheet
	'AFDESIGN' , # Affinity Design Document
	'VSTX' , # Microsoft Visio Drawing Template
	'STD' , # Apache OpenOffice Drawing Template
	'DPR' , # Digital InterPlot File
	'EPS' , # Encapsulated PostScript File
	'DRW' , # Drawing File
	'FH10' , # FreeHand 10 Drawing File
	'CSY' , # Canvas Symbol File
	'EPSF' , # Encapsulated PostScript Format File
	'WMF' , # Windows Metafile
	'ODG' , # OpenDocument Graphic File
	'PFD' , # Micrografx Optima! File
	'FH9' , # FreeHand 9 Drawing File
	'CDMZ' , # ConceptDraw MINDMAP Document
	'CDD' , # ConceptDraw DIAGRAM Document (Legacy)
	'PS' , # PostScript File
	'LMK' , # Sothink Logo Maker Image
	'CDRAPP' , # CorelDRAW.app Image File
	'PSID' , # PostScript Image Data File
	'GLOX' , # Microsoft Office SmartArt Graphics Layout File
	'FH4' , # FreeHand 4 Drawing File
	'POBJ' , # Photo Pos Pro Vector Object File
	'FT9' , # FreeHand 9 Drawing Template
	'FXG' , # Flash XML Graphics File
	'FH7' , # FreeHand 7 Drawing File
	'IGX' , # iGrafx Document
	'DRW' , # Corel Drawing File
	'DPP' , # DrawPlus Drawing File
	'INK' , # InkML Image
	'EMZ' , # Windows Compressed Enhanced Metafile
	'XAR' , # Xara Xtreme Drawing
	'VSD' , # Microsoft Visio Drawing (Legacy)
	'DRAWIO' , # diagrams.net Diagram File
	'NODES' , # Stick Nodes 2D Object
	'TEX.EMZ' , # Street Fighter IV Texture File
	'CVD' , # Canvas X Drawing
	'FHD' , # FreeHand Drawing File
	'SSK' , # SmartSketch 95 Document
	'AIC' , # Adobe Illustrator Cloud Document
	'PLT' , # HPGL Plot File
	'SK' , # Skencil Drawing
	'ECS5' , # Easy Cut Studio Version 5 Project
	'XMMAT' , # MindManager XML Map Template
	'DRAWIT' , # DrawIt Drawing
	'MVG' , # Magick Vector Graphics File
	'VSDM' , # Visio Macro-Enabled Drawing
	'CVX' , # Canvas Drawing
	'OTG' , # OpenDocument Graphic Template
	'AC6' , # ArtCut 6 Document
	'PMG' , # PageMaker Group File
	'SVM' , # StarView Metafile
	'PEN' , # Logitech io2 Drawing
	'AIT' , # Adobe Illustrator Template
	'PLT' , # AutoCAD Plotter Document
	'PIXIL' , # Pixilart Project
	'WPG' , # WordPerfect Graphic
	'PUPPET' , # Adobe Character Animator Puppet
	'SXD' , # StarOffice Drawing
	'FT8' , # FreeHand 8 Template
	'PD' , # FlexiSIGN 5 Plotter Document
	'HPGL' , # HP Graphics Language Plotter File
	'SCV' , # ScanVec CASmate Sign File
	'RDL' , # MicroStation Redline File
	'CDX' , # CorelDRAW Compressed File
	'ESC' , # EasySignCut Pro Project
	'HPG' , # HPGL Plot File
	'PICT' , # Picture File
	'VECTORNATOR' , # Vectornator Drawing
	'HVIF' , # Haiku Vector Icon File
	'CDTX' , # ConceptDraw DIAGRAM XML Template
	'FIG' , # Xfig Drawing
	'DIA' , # Dia Diagram File
	'GSD' , # Graphtec Vector Graphics File
	'MP' , # LaTeX MetaPost File
	'CLARIFY' , # Clarify Document
	'MGC' , # Microsoft Clip Organizer Media Catalog
	'FH11' , # FreeHand 11 Drawing File
	'SKETCH' , # Sketch Drawing
	'ASY' , # Asymptote Module
	'VML' , # Vector Markup Language File
	'IMD' , # Microsoft VisioModeler File
	'YDR' , # YouiDraw Drawing Project
	'TPL' , # Canvas Template
	'JSL' , # Corel PaintShop Pro Shape Library
	'IDEA' , # Adobe Ideas Document
	'CVS' , # Canvas 3 Drawing File
	'CDDZ' , # ConceptDraw DIAGRAM Document
	'GRAFFLE' , # OmniGraffle Drawing
	'PAT' , # CorelDRAW Pattern File
	'EMF' , # Enhanced Windows Metafile
	'WMZ' , # Compressed Windows Metafile
	'SK1' , # sK1 Vector Graphic File
	'MAKER' , # Adobe Character Animator Puppet Maker Template
	'EPGZ' , # Pencil Project File
	'MGTX' , # MindGenius XML Template File
	'FH3' , # FreeHand 3 Drawing File
	'FH5' , # FreeHand 5 Drawing File
	'MMAT' , # MindManager Map Template
	'ISF' , # Ink Serialized Format
	'DHS' , # HyperSnap Drawing
	'WPI' , # Inkling Sketch Binary File
	'EZDRAW' , # EazyDraw Graphic
	'DRAWING' , # Artboard Drawing
	'INK' , # Inker Image
	'TNE' , # Manga Studio Tone File
	'YLC' , # YouiDraw Logo Creator Image
	'CVXCAD' , # Canvas X Pro Drawing
	'SCUT5' , # Sure Cuts A Lot Version 5 Project
	'CDTZ' , # ConceptDraw DIAGRAM Template
	'GSTENCIL' , # OmniGraffle Stencil
	'MGCB' , # Equation Illustrator Project File
	'SK2' , # ChemSketch Drawing
	'CVG' , # Calamus Vector Graphic File
	'TLC' , # The Logo Creator File
	'VST' , # Microsoft Visio Drawing Template (Legacy)
	'CVI' , # Canvas Image File
	'SNAGSTYLES' , # Snagit Style Archive
	'OVR' , # The Overlay Maker Overlay File
	'MGMX' , # MindGenius XML Map File
	'SMF' , # Serif Metafile
	'DXB' , # Drawing Exchange Binary
	'DED' , # Dr. Engrave Document
	'SDA' , # StarOffice Drawing
	'CDSX' , # ConceptDraw DIAGRAM XML SlideShow
	'STN' , # Genuine Fractals Image
	'OVP' , # The Overlay Maker Package File
	'DPX' , # DrawPlus Template
	'VEC' , # CX-Designer Shape File
	'INK' , # Pocket PC Handwritten Note
	'CV5' , # Canvas 5 Drawing File
	'EGC' , # EximiousSoft GIF Creator Document
	'SVF' , # Simple Vector Format Graphic
	'PFV' , # PhotoFiltre Studio Vectorial Path
	'CGM' , # Computer Graphics Metafile
	'FTN' , # Flowton Diagram File
	'FH6' , # FreeHand 6 Drawing File
	'AC5' , # ArtCut 5 Document
	'CIL' , # Clip Gallery Packaged File
	'AF3' , # Micrografx ABC FlowCharter 3/4 File
	'FMV' , # Frame Vector Metafile
	'FIF' , # Fractal Image File
	'ABC' , # Micrografx ABC FlowCharter 6 File
	'CDX' , # ConceptDraw DIAGRAM XML Document
	'CNV' , # Canvas 6-8 Drawing File
	'SKETCHPAD' , # Sketchpad Drawing File
	'PL' , # Unix Color Plot File
	'AF2' , # Micrografx ABC FlowCharter 2 File
	'HPL' , # HP-GL Plotter File
	'DESIGN' , # Microsoft Expression Design Drawing
	'PIC' , # QuickDraw Picture
	'ZGM' , # Zenographics Image File
	'DCS' , # Desktop Color Separation File
	'DDRW' , # ClarisDraw Drawing
	'UFR' , # Ultra Fractal File
	'AWG' , # Ability Draw Document
	'FT10' , # FreeHand 10 Drawing Template
	'FS' , # FlexiSIGN Document
	'XMMAP' , # MindManager XML Map
	'HGL' , # HP Graphics Language File
	'CDMM' , # ConceptDraw MINDMAP Document
	'ARTB' , # Artboard Document
	'GKS' , # Graphics Kernel System File
	'FT7' , # FreeHand 7 Drawing Template
	'ART' , # Art Document
	'PCT' , # Picture File
	'DSF' , # Designer File
	'COR' , # CorelDRAW Drawing
	'VBR' , # GIMP Parametric Brush File
	'CURVE' , # Curve Drawing
	'CDMT' , # ConceptDraw MINDMAP Template
	'QCC' , # Quilt Manager Pattern File
	'CVDTPL' , # Canvas X Draw Template
	'MGMF' , # MindGenius Map File
	'CDT' , # ConceptDraw DIAGRAM Template (Legacy)
	'CCX' , # Corel Clipart File
	'GTEMPLATE' , # OmniGraffle Template
	'MGMT' , # MindGenius 2-3 Template
	'XPR' , # Microsoft Expression Design Graphic
	'FT11' , # FreeHand 11 Drawing Template
	'CDMTZ' , # ConceptDraw MINDMAP Template
	'DS4' , # Designer 4 Drawing
	'DSG' , # Stitch Era Design File
	'AMDN' , # Amadine Drawing
	'GEM' , # GEM Metafile
	'NDB' , # MyDraw Binary Drawing
	'NDX' , # MyDraw XML Drawing
	'NDTX' , # MyDraw XML Drawing Template
	'YAL' , # Arts & Letters Clipart Library
	'CAG' , # ClipArt Gallery Package
	'P' , # Applause Picture
	'PWS' , # Print Workshop Image
	'PCS' , # Microsoft Office ClipArt File
	'GLS' , # Sothink SWF Easy Shape File
	'MGS' , # MGCSoft Vector Shapes File
	'CWT' , # ClarisWorks Texture File
	'IGT' , # iGrafx Document Template
	'NAP' , # NAP Metafile
)

# noinspection SpellCheckingInspection
page_layout_set = (
	'FIG' , # Figma Design
	'CHAI' , # Chaimera Artwork File
	'DRMZ' , # Drumlin Fully Secure Mobile PDF File
	'FLO' , # Automate Flowchart File
	'FSD' , # Flowsheet Document
	'AFBOOK' , # Affinity Publisher Book
	'AFPUB' , # Affinity Publisher Document
	'XMT' , # XMind Template
	'CHAIT' , # Chaimera Artwork Template
	'PDFXML' , # Adobe PDFXML Document
	'XTP' , # InfoPath Template Part File
	'LSC' , # Logo! Soft Comfort File
	'MFP' , # MediaFACE Project File
	'AP' , # ArtPro File
	'CST' , # Manga Studio Story File
	'ELP' , # eCommerce Landing Page
	'FD2' , # PictureMate Borders File
	'SBV' , # Superbase Form Definition
	'PDF' , # Portable Document Format File
	'INX' , # Adobe InDesign Interchange File
	'PDO' , # Pepakura Designer File
	'ANH' , # Project ROME File
	'ICAP' , # InCopy Package File
	'FGC' , # Greeting Card Factory Card File
	'OLA' , # Online Access File
	'HCR' , # Half-Fold Card File
	'CEDPRJ' , # Ashampoo CoverEditor Project
	'ZNO' , # Zinio Electronic Magazine File
	'MCSP' , # My Craft Studio Project File
	'FEY' , # FCS Express Layout File
	'MIF' , # FrameMaker Interchange Format File
	'CAL' , # Calendar File
	'A6PROJ' , # Artisan 6 Project
	'MWL' , # XMS Print Layout File
	'ISD' , # Flexera InstallShield Dialog Box File
	'IFD' , # InForm Document
	'PGS' , # PageStream Document
	'PSPROJ' , # The Print Shop Project File
	'HPT' , # HotDocs PDF Template
	'PDR' , # ProntoDoc for Word Recovered Document
	'LTF' , # Laser App Temp Form File
	'LBL' , # Now Contact Label Template
	'GSP' , # Silhouette Studio SD Card Document
	'FP3' , # FastReport Prepared Report
	'CDF' , # Computable Document Format File (Legacy)
	'PPX' , # Serif PagePlus Template File
	'LST' , # Now Contact List Template
	'STUDIO3' , # Silhouette Studio Version 3 Document
	'CL2ARC' , # Comic Life 2 Document Archive
	'ZDP' , # Avery DesignPro Label File
	'INP' , # Urdu InPage Document
	'FM' , # FrameMaker Document
	'HMK' , # Hallmark Card Studio File
	'FCDT' , # FormsCentral Design Template
	'SMMX' , # SimpleMind Mind Map
	'BTW' , # BarTender Label
	'CSD' , # Compact Shared Document
	'PUB' , # Publisher Document
	'BLK' , # ABBYY FineReader Area Template
	'BCF' , # Business Card Designer Pro File
	'PCL' , # Printer Command Language Document
	'4UI' , # ForeUI Plot File
	'PMD' , # PageMaker Document
	'MFO' , # MediaFace Online Saved File
	'WLP' , # Worldlabel.com Label Designer File
	'PMX' , # PrintMaster Project File
	'AFTEMPLATE' , # Affinity Template
	'GDE' , # The Guide Document
	'FLB' , # FileMaker Pro Label File
	'INFOPATHXML' , # Microsoft InfoPath Form
	'MVDX' , # MindView Windows Document
	'DTX' , # Documented LaTeX File
	'VFC' , # VentaFax Cover Page File
	'PZF' , # GraphPad Prism Project
	'CLKD' , # Clicker Docs File
	'IDMS' , # Adobe InDesign Snippet
	'NB' , # Mathematica Notebook
	'XPS' , # XML Paper Specification File
	'DOT' , # Microsoft Word Document Template (Legacy)
	'IDML' , # Adobe InDesign Markup Language File
	'PFF' , # Formatta Portable Form File
	'PNH' , # Project ROME Plug-in Archive
	'DTP' , # Publish-iT Document
	'AFP' , # Advanced Function Presentation File
	'COMICDOC' , # Comic Life Document
	'RELS' , # Open Office XML Relationships File
	'FR3' , # FastReport Form File
	'PRN' , # Print to File
	'FORM' , # NetBeans Java GUI Designer Form
	'BMML' , # Balsamiq Mockups Markup Language File
	'P65' , # PageMaker 6.5 Document
	'FAX' , # Now Contact Fax Template
	'AV' , # Final Draft AV Script
	'JTP' , # Windows Journal Template
	'OXPS' , # OpenXPS File
	'PFL' , # PDFill Project
	'CLT' , # Clarity Legal Transcript File
	'RMR' , # ResumeMaker File
	'IND' , # Adobe InDesign Document
	'CWT' , # ChordWizard Template
	'WEBTEMPLATE' , # iWeb Template 
	'MRG' , # Now Contact Merge Template
	'Q3C' , # Quick 3D Cover Project File
	'PWB' , # Promethean Whiteboard Activity
	'DE' , # DrawExpress Diagram File
	'CLD' , # Canon CD Label Template
	'PEX' , # West Publisher Portable Exhibit File
	'INCX' , # InCopy CS3 Interchange File
	'RSM' , # WinWay Resume Deluxe Resume File
	'IDAP' , # Adobe InDesign Assignment Package
	'INDL' , # Adobe InDesign Library
	'DRMX' , # Drumlin Fully Secure PDF File
	'INCD' , # InCopy Document
	'CL2TPL' , # Comic Life 2 Template
	'ILDOC' , # QuickSilver Document
	'FDD' , # FormDocs Document
	'INDD' , # Adobe InDesign Document
	'PSR' , # Powersoft Report File
	'TEMPLATE' , # Pages Template
	'HFD' , # HotDocs Form Document
	'QXD' , # QuarkXPress Document
	'XZFX' , # ZipLogix ZipForm File
	'MAX' , # OmniPage Scanned Document
	'OPD' , # OmniPage Document
	'QXP' , # QuarkXPress Project File
	'GWB' , # Interwrite Presentation File
	'T2D' , # TouchDraw Document
	'MML' , # FrameMaker Maker Markup Language File
	'LLD' , # Logo! Soft Comfort LAD File
	'COMPOSITIONTEMPLATE' , # Invantive Composition Template
	'MDI' , # Microsoft Document Imaging File
	'RWT' , # ReadWriteThink Timeline
	'PDM' , # ProntoDoc for Word Mobile Display Document Template
	'PM5' , # PageMaker 5.0 Document
	'FADEIN' , # Fade In Document
	'COMICLIFE' , # Comic Life Document
	'JTX' , # Jrju Text File
	'PWT' , # Pocket Word Template
	'FXM' , # WinFax Document
	'SPL' , # sPlan Schematic Diagram File
	'SP' , # SignPlot Traffic Sign File
	'AVERY' , # Avery Design & Print Label File
	'RLF' , # ArcGIS Report Layout File
	'CL2DOC' , # Comic Life 2 Document
	'FOLIO' , # Adobe Folio File
	'IMX' , # iMindMap Map File
	'ENV' , # Now Contact Envelope Template
	'ZDL' , # Avery DesignPro Label File
	'LPDF' , # Localized PDF File
	'SPL7' , # sPlan 7 Schematic Diagram File
	'OFD' , # OfficeForms Document
	'CPH' , # Corel Print House File
	'RAP' , # Raptor Flowchart File
	'XIF' , # ScanSoft Pagis File
	'ARTICLE' , # Twixl Publisher Article
	'RPC' , # Rich Page Content Document
	'PDER' , # ProntoDoc for Excel Conventional Document
	'BOOK' , # BookSmart Book File
	'PDF-1' , # Renamed PDF File
	'BK' , # FrameMaker Book File
	'PDH' , # ProntoDoc for Word Browser Display Document Template
	'PUBLICATION' , # Twixl Publisher Publication
	'BOOK' , # FrameMaker Book File
	'XSN' , # InfoPath Form Template File
	'SNP' , # Access Report Snapshot
	'DPD' , # Ovation Pro Document
	'TDS' , # Adobe LiveCyle Designer Template
	'EDDX' , # Edraw XML FIle
	'CAJ' , # Chinese Academic Journal File
	'PTX' , # E-Transcript File
	'PM6' , # PageMaker 6.0 Document
	'PDG' , # Print Shop Deluxe Design File
	'ENC' , # CopySafe Protected PDF File
	'XDW' , # Fuji Xerox DocuWorks File
	'SUBLIME-SNIPPET' , # Sublime Text Snippet File
	'QPT' , # QuarkXPress Project Template
	'IMM' , # iMindMap Map File
	'RB4' , # Resume Builder 4 File
	'ICML' , # InCopy Document
	'INDS' , # Adobe InDesign Snippet File
	'INDT' , # Adobe InDesign Template
	'NUD' , # Now Up-To-Date Calendar File
	'CBF' , # Calendar Builder File
	'BCP' , # Business Card Designer Plus File
	'CD2' , # Click'N Design 3D File
	'MARS' , # Adobe MARS File
	'SDT' , # SmartDraw Template File
	'KITSP' , # KIT Scenarist Project
	'ISALETEMPLATE' , # iSale Template
	'CH3' , # Harvard Graphics DOS Chart File
	'MAILSTATIONERY' , # Apple Mail Stationery File
	'EL4' , # Easy-PhotoPrint EX Calendar File
	'FLOWCHART' , # PureFlow Flowchart
	'GOFIN' , # DRUKI Gofin Form File
	'OMP' , # OpenMind Windows Document
	'SJD' , # Scrapbook Factory Journal File
	'DWDOC' , # DrawWell Document
	'BRO' , # Design & Print Page Design File
	'CLKC' , # Clicker Connect Set File
	'CDML' , # Creately Diagram File
	'CPE' , # Fax Cover Page File
	'CONSIS' , # Sciral Consistency Document
	'PSG' , # Page Segment File
	'CDOC' , # Clicker Document
	'EDRWX' , # XPS Drawing Document
	'ICMT' , # InCopy Template
	'NSA' , # Noteshelf for Android Notebook
	'FRT' , # ADLForms Template
	'ZDS' , # DesignPro Label File
	'A6PAGE' , # Artisan 6 Page
	'WMGA' , # Web Manga Document
	'MGA' , # Manga Document
	'ADF' , # ARIS Express Document
	'LBL' , # dBASE Label File (Legacy)
	'EBRF' , # Electronic Braille Ready Format
	'PPP' , # Serif PagePlus Document
	'WWF' , # World Wide Fund Non-Printable Document
	'COV' , # Fax Cover Page File
	'PZFX' , # GraphPad Prism XML Project
	'SBK' , # Scrapbook Factory File
	'QDF' , # Label Matrix Label Design
	'DCX' , # FAXserve Fax Document
	'UXF' , # UML eXchange Format
	'DTL' , # Now Contact Detail Template
	'TP3' , # Harvard Graphics DOS Template File
	'QXB' , # QuarkXPress Book File
	'VP' , # Ventura Publication File
	'RFD' , # Recogniform Form Designer File
	'FRF' , # Free Report Form File
	'DRA' , # Scriptor Script File
	'NPP' , # Art Explosion Publisher Pro Document
	'MFT' , # MediaFACE Project Template
	'CW' , # CardWorks Template
	'MVTX' , # MindView Template
	'CL2LYT' , # Comic Life 2 Layout File
	'WEBTHEME' , # iWeb Theme Template 
	'ISALE' , # iSale Auction
	'HFT' , # HotDocs Form Template
	'QXT' , # QuarkXPress Template
	'BIZ' , # Broderbund Business Card File
	'MTC' , # Make the Cut! Project
	'SIMP' , # Software Ideas Modeler Project
	'CSZ' , # Cornerstone Document Template
	'MVD' , # MindView OS X Document
	'FRDOC' , # FineReader Document
	'CLKB' , # Clicker Books File
	'FPE' , # Free PDF Editor Document
	'CNDX' , # Avery DesignPro for Mac Label File
	'ISPX' , # iStudio Publisher Document
	'STUDIO' , # Silhouette Studio Document
	'PM4' , # PageMaker 4 Document
	'BRO' , # CreataCard Brochure Project
	'PDWR' , # ProntoDoc for Word Conventional Document
	'OO3TEMPLATE' , # OmniOutliner Template
	'SPUB' , # Swift Publisher Document
	'DMTEMPLATE' , # Direct Mail Template
	'GDOCX' , # Google Drive Document
	'HPD' , # HotDocs PDF Document
	'WPT' , # Works Template
	'COMIC' , # Comic Life Comic
	'INDB' , # Adobe InDesign Book File
	'GEM' , # Ventura Publisher Document
	'HWDT' , # Thinkfree Office NEO Word Template
	'PXL' , # Printer Command Language XL Document
	'AO' , # ActionOutline Outline File
	'RPX' , # Report XML File
	'SCUT4' , # Sure Cuts A Lot Version 4 Project
	'VPD' , # Visual Paradigm Online Diagram
	'PA' , # Print Artist Project
	'LAB' , # WordPerfect Label Definition File
	'CRTR' , # MultiAd Creator Pro Document
	'ARTPAGE' , # Artisan 5 Page
	'STARC' , # Story Architect Project
	'PDS' , # The Print Shop Project
	'PAGE' , # Artisan Page Template
	'CVW' , # CaseView Document
	'MINDER' , # Minder Mind Map
	'LMA' , # Logo! UDF File
	'IMTX' , # iMindMap Template File
	'RWT' , # ReadWriteThink Printing Press Draft
	'BMPR' , # Balsamiq Mockups Project
	'ZFX' , # ZipForm File
	'CPY' , # eCopy Desktop File
	'INCT' , # InCopy Template
	'INDP' , # Adobe InDesign CS3 Package File
	'IDPK' , # Adobe InDesign Package for GoLive File
	'RPTR' , # RafflePrinter Document
	'BPF' , # Baan ERP Document
	'CLKBD' , # Clicker Board Set
	'CADOC' , # Clicker App Document
	'WCP' , # WinCHM Project File
	'JWC' , # JewelCase Maker File
	'FLW' , # Kivio Flow Chart
	'JSD' , # HP JetSuite Document
	'SMA' , # SmartPlant Enhanced Report
	'INLX' , # Adobe InDesign Interchange Library
	'BOOKTEMPLATE' , # BookSmart Template
	'FRM' , # ADLForms Form
	'PDW' , # ProntoDoc for Word Conventional Document Template
	'TOTALSLAYOUT' , # Totals Invoice Layout File
	'PDP' , # Adobe Portable Document Format File
	'PJL' , # Printer Job Language File
	'FP' , # FinePrint File
	'MCSX' , # My Craft Studio Professional File
	'ADB' , # Now Contact Address Book Template
	'FDT' , # FormDocs Template
	'PDE' , # ProntoDoc for Excel Document Template
	'ANTMPL' , # Adobe Edge Animate Template File
	'JTX' , # XPS Document
	'ISALLIC' , # iSale License File
	'MBBK' , # ManaBook Book Kit File
	'ADF' , # Actual Drawing File
	'PDZ' , # ProntoDoc for Word Structured Text Document Template
	'SBPAGE' , # Storybook Creator 4 Page
)

# noinspection SpellCheckingInspection
spreadsheet_set = (
	'PRESTO' , # Presto Project Spreadsheet
	'FODS' , # OpenDocument Flat XML Spreadsheet
	'XLSX' , # Microsoft Excel Spreadsheet
	'OTS' , # OpenDocument Spreadsheet Template
	'XLS' , # Microsoft Excel Spreadsheet (Legacy)
	'XLSM' , # Microsoft Excel Macro-Enabled Spreadsheet
	'DEF' , # SmartWare II Data File
	'123' , # Lotus 1-2-3 Spreadsheet
	'XLR' , # Works Spreadsheet
	'GNUMERIC' , # Gnumeric Spreadsheet
	'PMVX' , # PlanMaker Spreadsheet Template
	'NUMBERS-TEF' , # Numbers iCloud Document
	'SXC' , # StarOffice Calc Spreadsheet
	'XLSB' , # Microsoft Excel Binary Spreadsheet
	'XLTM' , # Microsoft Excel Macro-Enabled Spreadsheet Template
	'XL' , # Microsoft Excel Spreadsheet
	'ODS' , # OpenDocument Spreadsheet
	'CELL' , # Thinkfree Office NEO Cell Workbook
	'XAR' , # Microsoft Excel Auto-Recovery File
	'CHIP' , # Microarray Annotation File
	'STC' , # StarOffice Calc Spreadsheet Template
	'BKS' , # Microsoft Works Spreadsheet Backup File
	'DEX' , # Microsoft Excel Spreadsheet
	'NUMBERS' , # Apple Numbers Spreadsheet
	'EDXZ' , # Edraw Compressed XML FIle
	'CTS' , # TreeSheets Hierarchical Spreadsheet
	'TMV' , # TimeMap Visual
	'SDC' , # Apache OpenOffice Calc Spreadsheet
	'RDF' , # Report Definition File
	'EFU' , # Everything File List
	'_XLSX' , # Renamed Microsoft Excel Spreadsheet
	'NMBTEMPLATE' , # Numbers Spreadsheet Template
	'PMD' , # PlanMaker Spreadsheet (Legacy)
	'GNM' , # Gnumeric Spreadsheet
	'XLTX' , # Microsoft Excel Spreadsheet Template
	'PMDX' , # PlanMaker Spreadsheet
	'_XLS' , # Renamed Microsoft Excel Spreadsheet (Legacy)
	'WQ2' , # Quattro Pro for DOS Spreadsheet File
	'IMP' , # Improv Spreadsheet
	'OGWU' , # Origin Unicode Workbook File
	'NCSS' , # NCSS Dataset File
	'XLSHTML' , # Microsoft Excel HTML Spreadsheet
	'AST' , # Ability Spreadsheet Template
	'GSHEET' , # Google Sheets Shortcut
	'QPW' , # Quattro Pro Spreadsheet
	'DIS' , # Oracle Discoverer Workbook
	'WKS' , # Lotus 1-2-3 Spreadsheet
	'FP' , # FileMaker Pro Spreadsheet
	'UOS' , # Uniform Office Spreadsheet
	'WQ1' , # Quattro Pro for DOS Spreadsheet File
	'12M' , # Lotus 1-2-3 SmartMaster File
	'MAR' , # Mariner Calc Spreadsheet
	'XLSMHTML' , # Microsoft Excel MIME HTML Spreadsheet
	'OGW' , # Origin Workbook File
	'ESS' , # EasySpreadsheet Spreadsheet
	'EDX' , # Edraw XML FIle
	'WKQ' , # Quattro Pro for DOS Spreadsheet File
	'AWS' , # Ability Spreadsheet File
	'WKS' , # Works Spreadsheet
	'FCS' , # First Choice Spreadsheet
	'PMV' , # PlanMaker Spreadsheet Template
	'WR1' , # Lotus Symphony Worksheet File
	'HCDT' , # Thinkfree Office NEO Cell Template
	'XLTHTML' , # Microsoft Excel HTML Spreadsheet Template
	'DFG' , # Data Flask Grid File
	'TMVT' , # TimeMap Template
	'WKI' , # Lotus 2 Worksheet
	'WKU' , # Lotus 1-2-3 Spreadsheet
	'WLS' , # 602Tab Spreadsheet
)

# noinspection SpellCheckingInspection
database_set = (
	'TE' , # Textease CT Database File
	'TRM' , # Oracle Trace Map File
	'NMONEY' , # Denaro Account
	'DTSX' , # DTS Settings File
	'GDB' , # InterBase Database File
	'ITW' , # InTouch With Database
	'TEACHER' , # SMART Response Teacher Database File
	'DDL' , # Data Definition Language File
	'ITDB' , # iTunes Database File
	'MDF' , # SQL Server Database
	'PDB' , # Program Database
	'SQLITEDB' , # SQLite Database
	'DB' , # Mobile Device Database
	'MUSICLIBRARY' , # Apple Music Library
	'FDB' , # Legacy Family Tree Database
	'SDX' , # StarMoney User Database File
	'DACONNECTIONS' , # RemObjects Data Abstract Connections File
	'TVDB' , # Apple TV Database
	'DACPAC' , # SQL Server Data Tier Application Package
	'DBC' , # FoxPro Database
	'TEMX' , # Cemetery Registry Database File
	'CDB' , # Pocket Access Database
	'DASCHEMA' , # RemObjects Data Abstract Schema File
	'DBX' , # Dropbox Encrypted Database File
	'ACCFT' , # Microsoft Access Data Type Template
	'ACCDC' , # Microsoft Access Signed Package
	'V12' , # All The Right Type Database File
	'ACCDE' , # Access Execute Only Database
	'ACCDT' , # Microsoft Access Database Template
	'MARSHAL' , # Marshal Data Migration Model File
	'FMPSL' , # FileMaker Pro 12 Snapshot Link
	'UDL' , # Microsoft Universal Data Link File
	'NYF' , # myBase Database File
	'ODB' , # OpenDocument Database
	'TRC' , # SQL Server Trace File
	'DB' , # Database File
	'SQLITE3' , # SQLite 3 Database
	'ECO' , # ECCO Database File
	'PAN' , # Panorama Database File
	'BTR' , # Btrieve Database File
	'APPROJ' , # ActivePresenter Project
	'HIS' , # FindinSite Database Definition File
	'ALF' , # ACT! Lookup File
	'SQL' , # Structured Query Language Data File
	'SIS' , # Sisma Database File
	'ABS' , # Absolute Database File
	'DB3' , # SQLite Database File
	'GDB' , # GPS Database File
	'$ER' , # GroupWise Database
	'FIL' , # ACL Database Table
	'ROD' , # RIB office Database File
	'CPD' , # RoboHelp Cache Project Database
	'DP1' , # DataPower Database File
	'4DL' , # 4th Dimension Database Log File
	'NV2' , # NewViews 2 Database File
	'CDB' , # Symbian Phonebook Database
	'FIC' , # WinDev Hyper File Database
	'FP3' , # FileMaker Pro 3 Database
	'SQLITE' , # SQLite Database
	'CRYPT12' , # WhatsApp Encrypted Database File
	'DB.CRYPT' , # WhatsApp Encrypted Database
	'MDB' , # Microsoft Access Database
	'OQY' , # Microsoft Excel OLAP Query
	'DB.CRYPT12' , # WhatsApp Encrypted Database File
	'WMDB' , # Windows Media Database File
	'DXL' , # Domino XML Language File
	'QVD' , # QlikView Data File
	'P97' , # Win97 Database
	'DB-WAL' , # SQLite Database Write-Ahead Log File
	'ASK' , # askSam Database
	'DLIS' , # DLIS Well Log Data File
	'DB.CRYPT8' , # WhatsApp Encrypted Database File
	'CRYPT8' , # WhatsApp Encrypted Database File
	'DBF' , # Database File
	'DCB' , # Concordance Database File
	'FM' , # FileMaker Database
	'FLEXOLIBRARY' , # Final Cut Pro Library Database
	'PX' , # PC-Axis Statistical Database
	'FDB' , # Firebird Database File
	'MUSICDB' , # Apple Music Library
	'MDW' , # Access Workgroup
	'HDB' , # HansaWorld Database File
	'DBS' , # SQLBase Database File
	'ACCDB' , # Access 2007 Database
	'USR' , # FileMaker Pro Database File
	'CRYPT15' , # WhatsApp Encrypted Database File
	'FRM' , # MySQL Database Format File
	'XLD' , # Microsoft Excel Database
	'MAR' , # Microsoft Access Report
	'ADP' , # Access Data Project
	'DB-JOURNAL' , # SQLite Rollback Journal File
	'CRYPT7' , # WhatsApp Encrypted Database File
	'PDM' , # PowerDesigner Database File
	'SDF' , # SQL Server Compact Database File
	'CRYPT1' , # WhatsApp Encrypted Database File
	'SDB' , # ServerBoss Database File
	'MAV' , # Access View File
	'DSN' , # Database Source Name File
	'ADF' , # ACT! Data File
	'CRYPT14' , # WhatsApp Encrypted Database File
	'FP7' , # FileMaker Pro 7+ Database
	'NNT' , # Eudora Address Book File
	'LUMINAR' , # Luminar 4 Catalog Database
	'GRDB' , # Gramps Database File
	'AQ' , # Ancestral Quest Database File
	'ABX' , # WordPerfect Address Book
	'CRYPT10' , # WhatsApp Encrypted Database File
	'MMBAK' , # Money Manager Expense & Budget Backup
	'EXB' , # Evernote Database File
	'MBTILES' , # Mapbox Tileset
	'AC' , # Ancestral Quest Collaborative Database File
	'IBD' , # MySQL InnoDB Table
	'TNFP' , # Tonfotos People Information Database
	'PQA' , # Palm Query Application
	'MWB' , # MySQL Workbench Document
	'SDB' , # OpenOffice Base Database File
	'MPD' , # Microsoft Project Database
	'EDB' , # Exchange Information Store Database
	'NSF' , # Lotus Notes Database
	'TRC' , # Oracle Trace File
	'DBT' , # Database Text File
	'CRYPT6' , # WhatsApp Encrypted Database File
	'FDB' , # Microsoft Dynamics NAV Database File
	'4DD' , # 4th Dimension Database Data File
	'VVV' , # Virtual Volumes View Catalog
	'KDB' , # Keypass Database File
	'FPT' , # FileMaker Pro Database Memo File
	'MAW' , # Access Data Access Page
	'SCX' , # FoxPro Form
	'SDB' , # MonKey Office SQLite Database File
	'RPD' , # RIB Project Database File
	'NDF' , # SQL Server Secondary Database File
	'CHCK' , # Microsoft Exchange Server Database Check File
	'DB-SHM' , # SQLite Database Shared Memory File
	'CRYPT' , # WhatsApp Encrypted Database
	'FMP12' , # FileMaker Pro 12 Database
	'LWX' , # Lightwright Database File
	'MAQ' , # Microsoft Access Query
	'CDB' , # CardScan Contacts Database
	'RODX' , # Ancestry Genealogical Database File
	'IB' , # InterBase Database
	'LGC' , # SimpleK Database File
	'NWDB' , # Neat Database File
	'MDN' , # Blank Access Database Template
	'IHX' , # IN-HEH Timeline Database
	'MYD' , # MySQL Database Data File
	'CRYPT9' , # WhatsApp Encrypted Database File
	'SDB' , # SparkleDB Database File
	'ACCDR' , # Access Runtime Application
	'CRYPT5' , # WhatsApp Encrypted Database File
	'WDB' , # Microsoft Works Database
	'ABCDDB' , # Apple Address Book Contact List 
	'RMGC' , # RootsMagic Data File
	'REALM' , # Realm Database
	'DQY' , # Microsoft Excel Query
	'UDB' , # Dynamics AX User Database File
	'MAF' , # Microsoft Access Form
	'ORA' , # Oracle Database Configuration File
	'BAK' , # Microsoft SQL Server Database Backup
	'QRY' , # Query File
	'VIS' , # Visual Importer Script
	'CAT' , # Advanced Disk Catalog Database
	'TPS' , # Clarion TopSpeed Data File
	'PDB' , # Palm Desktop Database File
	'SDY' , # StarMoney User Database File
	'RCTD' , # RabbitCT Dataset
	'DB2' , # dBASE II Database
	'CMA' , # TM1 Exported Cube File
	'DAD' , # RemObjects Data Abstract Driver File
	'MDBHTML' , # Microsoft Access Database HTML File
	'SDC' , # Paragon Dictionary Database File
	'BACPAC' , # Compressed Microsoft SQL Database
	'TSD' , # Tamino Schema
	'ADB' , # Alpha Five Database File
	'FTB' , # Family Tree Builder Genealogy Database File
	'ANB' , # Analyst's Notebook Chart
	'TRASH_DATABASE' , # Files by Google Trash Database
	'XMLFF' , # XML Flat File
	'EPIM' , # EssentialPIM Database File
	'NRMLIB' , # Neat Cabinet File
	'KEXI' , # Kexi Database
	'ORX' , # RadiantOne VDS Database Schema
	'SPQ' , # SPSS Database Query File
	'IPJ' , # ProFicient SPC Manufacturing Intelligence Project File
	'CAF' , # Cathy Catalog File
	'SAS7BDAT' , # SAS Dataset File
	'MRG' , # MySQL Merge File
	'ADE' , # Access Project Extension
	'PNZ' , # Panorama Database Set
	'DCX' , # FoxPro Database Index
	'ODB' , # Abaqus Output Database
	'IDB' , # IDA Database File
	'NV' , # NewViews Database File
	'ACCDW' , # Microsoft Access Database Link File
	'ECX' , # ECCO Corrupted Database File
	'SBF' , # Superbase Database
	'FMP' , # FileMaker Pro Database
	'MFD' , # Music Finder Database
	'JET' , # JET Database File
	'TMD' , # MySQL Temporary Database File
	'MUD' , # Textease Multi User Database
	'MDT' , # GeoMedia Access Database Template
	'CKP' , # SQL Server Checkpoint File
	'DSK' , # Simple IDs Database
	'RSD' , # RealSQLDatabase File
	'FP4' , # FileMaker Pro 4 Database
	'^^^' , # Pervasive.SQL Database File
	'ODL' , # OneDrive Event Log
	'NS2' , # Lotus Notes 2 Database
	'DADIAGRAMS' , # RemObjects Data Abstract Data Diagrams File
	'ERX' , # Entity Relationship Data Model File
	'RBF' , # Redatam Binary File
	'MDZ' , # Access Database Template
	'FP5' , # FileMaker Pro 5 Database
	'FM5' , # FileMaker 5 Database
	'WRK' , # SQL Server Log Shipping File
	'BC3' , # FIEBDC-3 Database File
	'ADB' , # Ability Database File
	'OWC' , # OutWit Catch Database
	'DBV' , # Database Variable Field File
	'XDB' , # PowerDesigner Database Definition File
	'VPD' , # VoxPro Database File
	'JTX' , # ESE Transaction Log
	'P96' , # Win96 Database
	'KEXIC' , # Kexi Database Connection File
	'ADN' , # Access Blank Project Template
	'GWI' , # GroupWise Database Shortcut File
	'ROD' , # Ancestry Genealogical Database File
	'DCT' , # Visual FoxPro Database Memo
	'KNO' , # SuperMemo Knowledge Collection
	'DAB' , # OrangeCD Database File
	'SDA' , # StarMoney User Database Archive
	'TVDB' , # TreeView Database
	'EDB' , # Windows Search Index Database
	'NS3' , # Lotus Notes 3 Database
	'PVOC' , # ProVoc Database File
	'ICDB' , # Icaros Thumbnail Database
	'ODLSENT' , # Sent OneDrive Event Log
	'FKR' , # Free Keylogger Report
	'MAS' , # Access Stored Procedure
	'FOL' , # PFS First Choice Database File
	'NS4' , # Lotus Notes 4 Database
	'FCD' , # First Choice Database
	'KEXIS' , # Kexi Database Shortcut File
	'R2D' , # Borland Reflex Database
)

# noinspection SpellCheckingInspection
executable_set = (
	'APK' , # Android Package File
	'JAR' , # Java Archive
	'AHK' , # AutoHotkey Script
	'FAP' , # Flipper Application Package
	'IPA' , # iOS Application
	'RUN' , # Linux Executable File
	'CMD' , # Windows Command File
	'XBE' , # Xbox Executable File
	'0XE' , # F-Secure Renamed Virus File
	'VLX' , # Compiled AutoLISP File
	'WORKFLOW' , # Automator Workflow
	'U3P' , # U3 Smart Application
	'BMS' , # QuickBMS Script
	'BAT' , # DOS Batch File
	'EXE' , # Windows Executable File
	'BIN' , # Unix Executable File
	'X86' , # Linux Executable File
	'8CK' , # TI-84 Plus C Silver Edition Application File
	'ELF' , # Nintendo Wii Game File
	'GADGET' , # Windows Gadget
	'AIR' , # Adobe AIR Installation Package
	'XAP' , # Silverlight Application Package
	'APP' , # macOS Application Bundle
	'MPK' , # Makino Package
	'SHORTCUT' , # Apple Shortcuts Shortcut
	'SCPT' , # AppleScript Script File
	'FBA' , # Fitbit OS App Package
	'WIDGET' , # Microsoft Windows Mobile Widget
	'RBF' , # LEGO MINDSTORMS EV3 Robot Brick File
	'X86_64' , # 64-bit Linux Executable File
	'AC' , # Autoconf Script
	'COM' , # DOS Command File
	'XLM' , # Microsoft Excel Macro
	'RXE' , # Lego Mindstorms NXT Executable Program
	'APPIMAGE' , # Linux Software Package
	'PIF' , # Program Information File
	'TPK' , # Tizen Package
	'SH' , # Bash Shell Script
	'OUT' , # Compiled Executable File
	'73K' , # TI-73 Application
	'SCRIPT' , # Generic Script File
	'EX5' , # MetaTrader 5 Program File
	'COMMAND' , # Terminal Command File
	'CELX' , # Celestia Script
	'EBS2' , # E-Run 2.0 Script File 
	'SCB' , # Scala Published Script
	'BA_' , # Renamed BAT File
	'PS1' , # Windows PowerShell Cmdlet File
	'PAF.EXE' , # PortableApps.com Program File
	'SCAR' , # SCAR Script
	'XEX' , # Xbox 360 Executable File
	'SCR' , # Script File
	'ISU' , # InstallShield Uninstaller Script
	'FAS' , # Compiled Fast-Load AutoLISP File
	'COFFEE' , # CoffeeScript JavaScript File
	'ACTION' , # Automator Action
	'TCP' , # Tally Compiled Program File
	'GM9' , # GodMode9 Script
	'SHB' , # Windows Document Shortcut
	'ACC' , # GEM Accessory File
	'RFU' , # Remote Firmware Update
	'HTA' , # HTML Application
	'CGI' , # Common Gateway Interface Script
	'SK' , # Skript File
	'EX_' , # Renamed Windows Executable File
	'XBAP' , # XAML Browser Application File
	'NEXE' , # Chrome Native Client Executable
	'ECF' , # SageCRM Component File
	'FXP' , # FoxPro Compiled Program
	'VPM' , # Vox Proxy Macro File
	'PLSC' , # Messenger Plus! Live Script File
	'WS' , # Windows Script
	'RPJ' , # Real Pac Batch Job File
	'VBS' , # VBScript File
	'MLX' , # MATLAB Live Code File
	'DLD' , # EdLog Compiled Program
	'COF' , # MPLAB COFF File
	'VXP' , # Mobile Application File
	'CACTION' , # Automator Converter Action
	'WSH' , # Windows Script Host Settings
	'PLX' , # Perl Executable File
	'MM' , # NeXtMidas Macro File
	'EX_' , # Compressed Executable File
	'IIM' , # iMacro Macro File
	'PHAR' , # PHP Archive
	'89K' , # TI-89 Application
	'SERVER' , # MySQL Server Script
	'A7R' , # Authorware 7 Runtime File
	'MEL' , # Maya Embedded Language Script
	'ESH' , # Extended Shell Batch File
	'DEK' , # Eavesdropper Batch File
	'CHEAT' , # Navi Cheatsheet
	'EBS' , # E-Run 1.x Script
	'PEX' , # ProBoard Executable File
	'FPI' , # FPS Creator Intelligence Script
	'GPE' , # GP2X Video Game
	'WCM' , # WordPerfect Macro
	'PYC' , # Python Compiled File
	'JS' , # JScript Executable Script
	'EX4' , # MetaTrader 4 Program
	'JSF' , # Java Script Command File
	'JSX' , # ExtendScript Script File
	'ACR' , # ACRobot Script
	'PWC' , # PictureTaker File
	'EXE1' , # Renamed EXE File
	'EAR' , # Java Enterprise Archive File
	'ICD' , # SafeDisc Encrypted Program
	'SNAP' , # Snap Application Package
	'VEXE' , # Virus Executable File
	'AZW2' , # Kindle Active Content App File
	'CEL' , # Celestia Script File
	'ROX' , # Actuate Report Object Executable File
	'ZL9' , # ZoneAlarm Quarantined EXE File
	'RGS' , # Registry Script
	'MCR' , # 3ds Max Macroscript File
	'EPK' , # LG Firmware Package
	'PAF' , # Portable Application Installer File
	'MS' , # Maxwell Script
	'TIAPP' , # TiTanium App
	'FRS' , # Flash Renamer Script
	'PYO' , # Python Optimized Code
	'OTM' , # Outlook Macro File
	'MSL' , # Magick Scripting Language File
	'UVM' , # UnitVM Executable File
	'APPLESCRIPT' , # AppleScript File
	'SREC' , # S-record File
	'ATMX' , # Automation Anywhere Task
	'89Z' , # TI-89 Program
	'HMS' , # HostMonitor Script File
	'ACTC' , # Action(s) Collection File
	'MRP' , # Mythroad Platform File
	'N' , # Neko Bytecode File
	'WIDGET' , # Yahoo! Widget
	'CSH' , # C Shell Script
	'MRC' , # mIRC Script File
	'WIZ' , # Microsoft Wizard File
	'BEAM' , # Compiled Erlang File
	'PRG' , # Program File
	'OSX' , # PowerPC Executable File
	'MCR' , # JitBit Macro Recorder Macro
	'SCT' , # Windows Scriptlet
	'CYW' , # Rbot.CYW Worm File
	'SPR' , # FoxPro Generated Screen File
	'MCR' , # Tecplot Macro
	'EBM' , # EXTRA! Basic Macro
	'TMS' , # Telemate Script
	'TIPA' , # TrollStore IPA File
	'YGH' , # YGH Trojan Executable
	'PRG' , # GEM Application
	'FKY' , # FoxPro Macro
	'XQT' , # SuperCalc Macro File
	'FAS' , # QuickSilver Fast Save Lisp File
	'APP' , # Symbian OS Application
	'MXE' , # Macro Express Playable Macro
	'ACTM' , # AutoCAD Action Macro File
	'UDF' , # Microsoft Excel User-defined Function
	'KIX' , # KiXtart Script File
	'KX' , # KiXtart Tokenized Script File
	'IPK' , # webOS TV Application
	'SEED' , # Linux Preseed File
	'VBSCRIPT' , # Visual Basic Script
	'APP' , # FoxPro Generated Application
	'EZS' , # EZ-R Stats Batch Script
	'THM' , # Thermwood Macro File
	'LO' , # Interleaf Compiled Lisp File
	'VBE' , # VBScript Encoded Script File
	'E_E' , # Renamed EXE File
	'GS' , # Geosoft Script
	'JSE' , # JScript Encoded File
	'SCPTD' , # AppleScript Script Bundle
	'PRC' , # Palm Resource Code File
	'HPF' , # HP9100A Program File
	'PRG' , # Commander X16 Program
	'XYS' , # XYplorer Script File
	'MS' , # 3ds Max Script
	'DMC' , # Medical Manager Script
	'MHM' , # Microsoft Hardware Macro
	'SCA' , # Scala Script File
	'WPK' , # WordPerfect Macro
	'EXZ' , # Zipped Executable File
	'S2A' , # SEAL2 Application
	'PXO' , # LG Mobile Phone Executable File
	'LS' , # LightWave LScript Source Code
	'ITA' , # VTech InnoTab Application File
	'EHAM' , # ExtraHAM Executable File
	'QIT' , # QIT Trojan Horse File
	'ES' , # SageCRM Script File
	'ARSCRIPT' , # ArtRage Script
	'RBX' , # Rembo-C Compiled Script
	'MEM' , # Macro Editor Macro
	'SAPK' , # SICK AppPool Application
	'WSF' , # Windows Script File
	'EBACMD' , # EBA Command Center Data File
	'NCL' , # NirCmd Script File
	'DXL' , # Rational DOORS Script
	'UPX' , # Ultimate Packer for eXecutables File
	'KSH' , # Unix Korn Shell Script
	'MAM' , # Microsoft Access Macro
	'HAM' , # HAM Executable File
	'BTM' , # 4DOS Batch File
	'MIO' , # MioEngine Application File
	'IPF' , # SMS Installer Script
	'VDO' , # Heathen Virus File
	'GPU' , # GP2X Utility Program
	'EXOPC' , # ExoPC Application
	'DS' , # TWAIN Data Source
	'MAC' , # Application Macro File
	'SBS' , # SPSS Script
	'CFS' , # CFS Console Program
	'ASB' , # Alphacam Stone VB Macro File
	'STS' , # #TASK Script File
	'RFS' , # Reflection FTP Client Script
	'WPM' , # WordPerfect Macro File
	'AFMACROS' , # Affinity Photo Macros File
	'UW8' , # MicroW8 Cartridge File
	'MLAPPINSTALL' , # MATLAB App Installation Package
	'RPG' , # DTPS Robot Program
	'P' , # MATLAB P-Code File
	'PVD' , # Instalit Script
	'PYZ' , # Python Application Zip File
	'QPX' , # Visual FoxPro Compiled Query Program
	'AFMACRO' , # Affinity Photo Macro File
	'ORE' , # Ore Executable File
	'EZT' , # EZT Malicious Worm File
	'SMM' , # Ami Pro Macro
	'MAMC' , # Murgee Auto Mouse Click Script
	'73P' , # TI-73 Program
	'BNS' , # Compiled Rabbit Network Script
)

# noinspection SpellCheckingInspection
game_set = (
	'U8' , # Mario Kart Wii Archive
	'OSR' , # osu! Replay File
	'BNS' , # Portal Bonus Map Script
	'SIMS3PACK' , # The Sims 3 Package File
	'GBX' , # Nadeo Game File
	'NPA' , # Steins;Gate Archive
	'USX' , # Unreal Static Meshes
	'ESG' , # Enigmo Saved Game File
	'W3N' , # Warcraft 3 Expansion Campaign Map File
	'KODU' , # Kodu Game File
	'MII' , # Wii Virtual Avatar File
	'VPK' , # Valve Pak
	'TTR' , # TETR.IO Replay
	'PSS' , # PlayStation 2 Game Video File
	'PXP' , # Counter-Strike PODBot Experience File
	'SFAR' , # Mass Effect 3 DLC File
	'SCWORLD' , # Survivalcraft World File
	'MCA' , # Minecraft Anvil Region
	'OSZ' , # osu! Beatmap File
	'REP' , # Grand Theft Auto Replay File
	'UNITY3D' , # Unity Web Player Archive
	'P3T' , # PlayStation 3 Theme File
	'SAMI' , # Grand Theft Auto San Andres Mod Installer File
	'ESS' , # The Elder Scrolls Saved Game
	'SIMS3' , # The Sims 3 Game Save File
	'SHA' , # GodMode9 SHA SysNAND Crosscheck File
	'ZS9' , # ZSNES Slot 9 Saved State File
	'AGE3SAV' , # Age of Empires 3 Save File
	'BFS' , # FlatOut Game Data File
	'WZ' , # MapleStory Game Data File
	'ARCH00' , # F.E.A.R. Game Archive
	'BZW' , # BZFlag World File
	'W3X' , # Warcraft 3 Expansion Map FIle
	'PAPA' , # Game Save Backup File
	'OSK' , # osu! Skin File
	'MP2S' , # Max Payne 2 Saved Game
	'PCC' , # Mass Effect Package
	'HONMOD' , # HON Modification Manager File
	'GCM' , # GameCube ROM File
	'GMA' , # Garry's Mod Add-on
	'DEK' , # Magic: The Gathering Deck File
	'GBA' , # Game Boy Advance ROM
	'V64' , # Doctor V64 Nintendo 64 Game ROM
	'WTD' , # Grand Theft Auto IV Textures File
	'GSC' , # Call of Duty Game Script
	'ZS2' , # ZSNES Slot 2 Saved State File
	'MCSTRUCTURE' , # Minecraft Structure
	'ISR' , # GoMoku Game File
	'KV3' , # KeyValues3 Game Data File
	'ZS4' , # ZSNES Slot 4 Saved State File
	'XOM' , # Worms 3D Object File
	'ZS5' , # ZSNES Slot 5 Saved State File
	'WLD' , # Terraria World
	'REZ' , # LithTech Game Engine Resource
	'YDC' , # Yu-Gi-Oh! Card Deck File
	'SII' , # SCS Software Game Data
	'MIS' , # Marble Blast Mission File
	'MCWORLD' , # Minecraft World Backup
	'MAE' , # My Avatar Editor Character File
	'FORGE' , # Ubisoft Game Data File
	'W3M' , # Warcraft 3 Map File
	'B' , # Grand Theft Auto 3 Saved Game File
	'PACKAGE' , # Electronic Arts Game Package File
	'MCR' , # Minecraft Region File
	'NDS' , # Nintendo DS Game ROM
	'MPM' , # Max Payne Mod
	'UNR' , # Unreal Level Map
	'YDR' , # Yu-Gi-Oh! Power of Chaos Replay File
	'SCHEMATIC' , # Minecraft Schematic
	'SC2ASSETS' , # Blizzard StarCraft 2 Assets File
	'XP3' , # KiriKiri Package File
	'UPK' , # Unreal Engine 3 Package
	'Z64' , # Nintendo 64 Game ROM
	'WOTREPLAY' , # World of Tanks Replay File
	'SMC' , # Super Nintendo Game ROM
	'YCM' , # Yugioh Card Maker File
	'Z4' , # Z-machine Z-code Version 4 File
	'SMRAILROADSSAVEDGAME' , # Sid Meier's Railroads! Saved Game
	'DEK' , # Yugioh Virtual Desktop Deck File
	'MENU' , # Quake 3 Engine Menu File
	'GMRES' , # GameMaker Resource File
	'TBM' , # Toribash Mod File
	'WAD' , # Doom WAD File
	'SCS' , # Prism3D Game Data
	'DEM' , # Video Game Demo File
	'LRF' , # League of Legends Replay File
	'BMZ' , # Portal Bonus Map Zip File
	'AGE3SCN' , # Age of Empires 3 Scenario File
	'PCK' , # Perfect World Data File
	'ESM' , # Elder Scrolls or Fallout 4 Master File
	'MP2M' , # Max Payne 2 Mod
	'H4R' , # Heroes of Might and Magic IV Data File
	'GBC' , # Game Boy Color ROM
	'LITEMOD' , # Minecraft LiteLoader Mod File
	'NBS' , # Minecraft Note Block Studio File
	'UASSET' , # Unreal Asset
	'ESP' , # Elder Scrolls or Fallout 4 Plug-in
	'SC2REPLAY' , # Blizzard StarCraft 2 Replay File
	'NAR' , # Nexon game archive
	'SPC' , # SNES Soundtrack File
	'GDG' , # GDevelop Project File
	'LTX' , # S.T.A.L.K.E.R. Properties File
	'LMU' , # RPG Maker Map File
	'TOR' , # Star Wars: The Old Republic Asset File
	'GB' , # Game Boy ROM File
	'UNITYPACKAGE' , # Unity Asset Package
	'SCS' , # SimCity Societies Saved Game File
	'RVDATA' , # RPG Maker VX Data File
	'CGZ' , # Cube Map File
	'3DSX' , # Nintendo 3DS Homebrew Launcher File
	'AM1' , # Adventure Maker 
	'WOWSREPLAY' , # World of Warships Replay File
	'ZS0' , # ZSNES Slot 0 Saved State File
	'BO2' , # Minecraft BO2 Object File
	'DSG' , # Doom Saved Game
	'PWF' , # Counter-Strike PODBot Waypoint File
	'BSP' , # Quake or Source Engine Game Map
	'SC2SAVE' , # Blizzard StarCraft 2 Saved Game
	'BIN' , # Sega Genesis Game ROM
	'PGN' , # Portable Game Notation File
	'WAD' , # Nintendo Wii Game Data File
	'VDF' , # Valve Data File
	'PAK' , # Video Game Package
	'DAT_NEW' , # Minecraft World Level File
	'SAVE' , # The Sims 4 Save File
	'CRP' , # Cities: Skylines Colossal Raw Asset Package
	'DAT' , # Minecraft Data File
	'SFO' , # PSP Game Parameters File
	'MCSERVER' , # Minecraft Server File
	'XPD' , # PlayStation Store PSP License File
	'SC4' , # SimCity 4 Saved File
	'ACWW' , # Animal Crossing Wild World Pattern File
	'MCPACK' , # Minecraft Bedrock Edition Add-On
	'CT' , # Cheat Engine Cheat Table
	'GEN' , # Sega Genesis ROM
	'PLR' , # Terraria Player Profile
	'NES' , # Nintendo Entertainment System (NES) ROM
	'MGX' , # Rise of Nations Saved Game
	'DAZIP' , # Dragon Age: Origins Game File
	'SC2MAP' , # Blizzard StarCraft 2 Map File
	'AGE3YSAV' , # Age of Empires 3: The Asian Dynasties Save File
	'MDL' , # Half-Life Model File
	'BIG' , # Electronic Arts Game Data File
	'W3G' , # Warcraft 3 Replay File
	'VMF' , # Valve Map File
	'MPQ' , # Blizzard Mo'PaQ File
	'N64' , # Nintendo 64 ROM File
	'AAO' , # America's Army Map File
	'LVL' , # Game Level File
	'CDP' , # Trainz Simulator Content Dispatcher Pack
	'REP' , # StarCraft Replay File
	'SCHEM' , # Minecraft Schematic
	'SFC' , # Super Famicom ROM
	'VTF' , # Valve Texture Format
	'PSV' , # PlayStation 2 Save File
	'GAM' , # Saved Game File
	'PKG' , # PlayStation Store Downloaded Package
	'3DS' , # Nintendo 3DS ROM
	'PBP' , # PSP Firmware Update File
	'WTF' , # World of Warcraft Text File
	'BMD' , # MU Online Game Data File
	'BSA' , # Bethesda Softworks Archive
	'PK3' , # Quake 3 Engine Game Data
	'ZS3' , # ZSNES Slot 3 Saved State File
	'DDT' , # Football Manager Keep Player Data File
	'MRS' , # GunZ Game Data File
	'MGL' , # Age of Empires 2 Replay File
	'UT4MOD' , # Unreal Tournament 2004 Module
	'RPL' , # Toribash Replay
	'SDZ' , # Spring Content Package
	'ACF' , # Steam Application Cache File
	'FRZ' , # Snes9x Save State
	'SC2ARCHIVE' , # Blizzard StarCraft 2 Archive File
	'INFO' , # Minecraft Forge Information File
	'LSL' , # LiveSplit Layout
	'ARP' , # Audition Replay File
	'VRCW' , # VRChat World
	'SLI' , # OMSI Spline Configuration File
	'GMS' , # Garry's Mod Save File
	'ZTMP' , # Steam Compressed Game Resource File
	'RTTEX' , # Robinson Technologies Texture File
	'SC4DESC' , # SimCity 4 Description File
	'SABER' , # Beat Saber Custom Saber
	'AGE3XSAV' , # Age of Empires 3: The WarChiefs Save File
	'PEX' , # Compiled Papyrus Script
	'XS' , # Age of Empires 3 Map AI Generator
	'STENCYL' , # Stencyl Game File
	'SG0' , # Humongous Entertainment Game Save File
	'SC2MA' , # StarCraft 2 Map Data File
	'ZS7' , # ZSNES Slot 7 Saved State File
	'PQHERO' , # Puzzle Quest Saved Game
	'SC2MOD' , # Blizzard StarCraft 2 Modification File
	'LSD' , # RPG Maker Game Save File
	'LUXB' , # Lux Map File
	'IDX0' , # Runescape Cache Index File
	'MD2' , # Quake 2 3D Model
	'MASSEFFECTSAVE' , # Mass Effect Saved Game
	'S2Z' , # Heroes of Newerth Game Data File
	'NLTRACK' , # NoLimits Roller Coaster Track File
	'GRF' , # Ragnarok Client Archive
	'FF' , # Call of Duty 4 Fast File
	'RGSS3A' , # RPG Maker VX Ace RGSS Encrypted Archive
	'XPK' , # WWE RAW Model File
	'BIN' , # Nintendo DS Binary File
	'SAV' , # Nintendo DS Save File
	'SAV' , # Video Game Save File
	'TTARCH' , # Telltale Games Archive
	'IWD' , # Call of Duty Game Data File
	'SPB' , # Flight Simulator Mission File
	'FUK' , # Postal 2 Map File
	'SM' , # StepMania Song File
	'BLP' , # Blizzard Texture File
	'BAR' , # Age of Empires 3 Data File
	'IPL' , # GTA Item Placement File
	'SIMS2PACK' , # Downloaded Sims 2 Package File
	'COMPILED' , # Compiled Game Resource File
	'BSB' , # BioShock Saved Game File
	'MAS' , # rFactor Track File
	'RVZ' , # Compressed Wii / GameCube ROM
	'MAP' , # Quake Engine Game Map
	'GAME' , # GameSalad Exported Game File
	'UTX' , # Unreal Texture Package
	'ESP' , # Fallout 3 Plugin File
	'FPS' , # FPS Creator Segment File
	'SPLANE' , # Simple Planes Plane File
	'WORLD' , # The Sims 3 Create a World Tool File
	'WORLD' , # WorldPainter File
	'QC' , # QuakeC Source Code File
	'M2' , # World of Warcraft Model Object
	'CGF' , # Crytek Geometry Format File
	'CPN' , # Age of Mythology Campaign File
	'JAP' , # Nonogram Puzzle File
	'MAHJONGTITANSSAVE-MS' , # Mahjong Titans Saved Game
	'NRO' , # Nintendo Switch Executable File
	'BIF' , # BioWare Infinity Engine File
	'SGM' , # VisualBoyAdvance Saved Game
	'BFG' , # Big Fish Games Application File
	'RXDATA' , # RMXP Data File
	'VMV' , # VisualBoyAdvance Recording
	'RVDATA2' , # RPG Maker VX Ace Data File
	'USM' , # USM Game Video
	'CARC' , # Nintendo Compressed Archive File
	'RGSSAD' , # RPG Maker VX RGSS Encrypted Archive
	'PHN' , # Algodoo Phun Phunlet
	'ESM' , # Fallout 3 Master File
	'GDSHADER' , # Godot Engine Shader File
	'SAD' , # Black & White Audio File
	'AGE3REC' , # Age of Empires 3 Recording
	'VFS0' , # Metro 2033 Sound File
	'XCI' , # NX Card Image File
	'VOL' , # Video Game Archive
	'UDK' , # Unreal Engine Map File
	'BA2' , # Bethesda Softworks Archive
	'W3Z' , # Warcraft 3 Saved Game
	'XMB' , # Age of Empires Game Data File
	'BRF' , # X-Wing Mission Briefing
	'68K' , # SEGA Mega Drive & Genesis Classics ROM File
	'WOTBREPLAY' , # World of Tanks Blitz Replay
	'BPS' , # BPS Patch File
	'SMZIP' , # StepMania Package
	'UC' , # UnrealScript Class
	'FSSAVE' , # Flight Simulator X Games Explorer File
	'NTRK' , # Pacific FIghters Online Track File
	'COURSE' , # Super Mario ReMaker Course File
	'MWL' , # Super Mario World Level
	'CHD' , # Compressed Hunks of Data File
	'FOMOD' , # Fallout Mod Archive
	'AIN' , # Source Engine Compiled AI Nodegraph File
	'WBT' , # Toribash Help File
	'SVS' , # Game Boy Advance Saved State File
	'FGD' , # Forge Game Data File
	'LMP' , # Quake Engine Lump File
	'CBV' , # ChessBase Database File
	'SCX' , # Microsoft Game Scenario File
	'USA' , # Unreal Saved Game File
	'PUP' , # PlayStation 4 or 5 Update File
	'SID' , # Steam Game Data Backup File
	'U' , # Unreal Tournament Class Package
	'CXI' , # Nintendo 3DS Executable Image
	'HOT' , # The Sims Sound File
	'WAM' , # Worms Armageddon Mission File
	'ASR' , # Audio Streaming Resource Archive
	'IBT' , # Texture and Model File
	'JMF' , # Jackhammer Project
	'GBASKIN' , # Game Boy Advance Controller Skin File
	'Z3' , # Z-machine Z-code Version 3 File
	'ENG' , # Chess Engine
	'SRM' , # Super Nintendo ROM Save
	'LDW' , # Virtual Villagers Saved Game File
	'DMB' , # BYOND Game Executable
	'MSTXT' , # Message Studio Text File
	'ZS1' , # ZSNES Slot 1 Saved State File
	'DEK' , # LackeyCCG Card Deck File
	'DM_83' , # Wolfenstein 1.02 Demo Movie File
	'PROJECT' , # Defold Project Settings File
	'BIK' , # Video Game Movie File
	'SQF' , # ArmA Script
	'TIM' , # The Incredible Machine Level
	'BLZ' , # N-Gage Mobile Video Game File
	'SSC' , # StepMania Song File
	'FML' , # WWE RAW Character File
	'NSZ' , # Compressed Nintendo Submission Package
	'RAW' , # Dolphin Virtual SD Card
	'ESL' , # Elder Scrolls Light Master File
	'LSS' , # LiveSplit Splits
	'MAP' , # Halo Map File
	'BUS' , # OMSI Bus Configuration File
	'MCAPM' , # Minecraft Anvil PocketMine Data File
	'ZTD' , # Zoo Tycoon Data Archive
	'SCM' , # CLEO Compiled Script
	'RVPROJ' , # RPG Maker VX Project File
	'SAV' , # Mass Effect 3 Saved Game
	'DUC' , # Action Replay Max DS Save File
	'XEN' , # Guitar Hero III Data File
	'AGE3XREC' , # Age of Empires 3: The WarChiefs Recording
	'GDI' , # Dreamcast Gigabyte Disc Image
	'DL' , # Zoo Tycoon 2 Data File
	'WORLD' , # Terraria Mobile World File
	'TED' , # Alamo Map File
	'EMD' , # Fritz TableBase
	'YDK' , # Yu-Gi-Oh! Deck File
	'PKX' , # Decrypted 3DS Pokémon Save File
	'BLS' , # Blockland Saved Game File
	'MD3' , # Quake 3 3D Model
	'FCS' , # FCEUX Save State File
	'NGAGE' , # N-Gage Game File
	'NGP' , # NeoGeo Pocket ROM
	'UNITYPROJ' , # Unity 3D Project
	'SGA' , # Relic Entertainment Game Archive
	'CSB' , # CRIWARE Sound Bank
	'ZMAP' , # Zombies Ate My Neighbors Map File
	'ADT' , # World of Warcraft Map File
	'STA' , # MAME Saved State File
	'RFM' , # rFactor Modification File
	'VCM' , # Vice City Mod Manager File
	'DV2' , # Divinity 2 Data File
	'BRRES' , # Wii BRRES Resource Package
	'UXX' , # Unreal Tournament Cache
	'AUD' , # Westwood Studios Audio
	'PSSG' , # EGO Engine Textures File
	'IPS' , # Internal Patching System Patch File
	'SC2BANK' , # StarCraft 2 Bank File
	'XNB' , # XNA Game Studio Binary Package
	'TIGER' , # Tomb Raider Game Data Archive
	'0' , # M3 DS Real Saved Game
	'IMG' , # Grand Theft Auto Data File
	'NARC' , # Nintendo DS Archive File
	'HUM' , # OMSI Human Configuration File
	'UKX' , # Unreal Animation Package
	'H3M' , # Heroes 3 Map File
	'BGL' , # Flight Simulator Scenery File
	'UPS' , # UPS Patch File
	'FAR' , # The Sims Archive File
	'DOL' , # GameCube Executable File
	'ESCAPE' , # Prison Architect Escape Mode Save File
	'UT3' , # Unreal Tournament 3 Map File
	'WZ' , # Warzone 2100 Data File
	'FOS' , # Fallout Save
	'SCX' , # Game Scenario File
	'ZIP' , # MAME Game ROM
	'SWC' , # Super Nintendo Wild Card ROM File
	'STORMREPLAY' , # Heroes of the Storm Replay File
	'BEJEWELED2DELUXESAVEDGAME' , # Bejeweled 2 Deluxe Saved Game
	'GR2' , # Granny 3D File
	'ELF' , # PlayStation Executable
	'MGX' , # Age of Empires 2 Expansion Replay File
	'PK4' , # Doom 3 Engine Game Data File
	'Z2F' , # Zoo Tycoon 2 Object File
	'LGP' , # Final Fantasy Media Archive
	'GALAXY' , # Blizzard Galaxy File
	'CBH' , # ChessBase Database Header File
	'POD' , # Terminal Reality Game Data File
	'PLN' , # Flight Simulator Flight Plan
	'NSBMD' , # Nintendo DS Model File
	'OMOD' , # Oblivion Mod Archive
	'FSM' , # FreeSpace Mission Definition
	'ZST' , # ZSNES Save File
	'GCF' , # Game Cache File
	'ROM' , # N64 Game ROM File
	'ZS6' , # ZSNES Slot 6 Saved State File
	'UT2MOD' , # Unreal Tournament 2003/2004 Module File
	'UMD' , # Splinter Cell Blacklist Game Data File
	'UNF' , # UNIF ROM File
	'OVH' , # OMSI Vehicle Configuration File
	'DEC' , # Magic: The Gathering Online Deck File
	'AGE3YREC' , # Age of Empires 3: The Asian Dynasties Recording
	'SGB' , # Dawn of War Map File
	'PCSAV' , # Mass Effect 2 Saved Game
	'TWT' , # Carmageddon II Game Data
	'WDB' , # Blizzard World of Warcraft Database File
	'CHK' , # StarCraft Chunk File
	'UTC' , # Bioware Creature File
	'H5U' , # Heroes of Might and Magic V Mod
	'Z5' , # Z-machine Z-code Version 5 File
	'CLIP' , # Grand Theft Auto 5 Clip
	'VB' , # Virtual Boy ROM File
	'SC4LOT' , # SimCity 4 Lot File
	'SCN' , # Age of Empires / Mythology Scenario
	'ZBLORB' , # Z-machine Package Game File
	'PLAYER' , # Terraria Mobile Player File
	'LL' , # List & Label Preview File
	'001' , # Snes9x Slot 2 Saved State
	'ERF' , # BioWare Entity Resource File
	'HPS' , # HPL Script
	'VOB' , # Live for Speed Car File
	'BIN' , # GOG.com Game Data File
	'LOVE' , # LÖVE Game Package
	'SHADER' , # Unity Shader Asset
	'BO3' , # Minecraft BO3 Object
	'DEM' , # Unreal Engine Game Replay File
	'J2I' , # Jazz 2 Internet Link File
	'MIS' , # Saved Game Mission File
	'NS1' , # Nestopia Save State
	'FS2' , # FreeSpace 2 Mission Definition
	'MASSEFFECTPROFILE' , # Mass Effect Player Profile
	'EGM' , # Enigmo Custom Game File
	'REPLAY' , # Rocket League Replay
	'DM2' , # Quake 2 Demo File
	'DNF' , # Duke Nukem Forever Map File
	'J2L' , # Jazz Jackrabbit 2 Level File
	'MSPROJ' , # Message Studio Project
	'RGD' , # Raft Saved Game File
	'SDT' , # Grand Theft Auto 3 Audio File
	'SGF' , # Smart Game Format File
	'DWI' , # Dance With Intensity Step File
	'WAGAME' , # Worms Armageddon Game Recording
	'DM_82' , # Wolfenstein 1.00 Demo Movie File
	'SGPBPRJ' , # Baltie Project
	'DUN' , # Dungeon Defenders Saved Game File
	'ZDS' , # GZDoom Save File
	'VMV' , # VirtuaNES Recording
	'FCM' , # fCraft World File
	'UMOD' , # Unreal Tournament Module File
	'GBCSKIN' , # Game Boy Color Controller Skin File
	'SC2DATA' , # Blizzard StarCraft 2 Data File
	'PSSL' , # PlayStation Shader Language File
	'BOOKWORMDELUXESAVEDGAME' , # Bookworm Deluxe Saved Game
	'BLACKHAWKSTRIKER2' , # Blackhawk Striker 2 Saved Game
	'GFX' , # GFx Scaleform File
	'FL' , # Freelancer Saved Game File
	'MAPLET' , # Maplet World File
	'BME' , # BeatMania 7-Key Song File
	'IDX255' , # Runescape Cache File
	'G3X' , # RealFlight Content File
	'DNS' , # Duke Nukem Forever Save File
	'CTY' , # SimCity City File
	'RADQ' , # Need for Madness Game Archive
	'PRK' , # Thrillville Theme Park
	'GPF' , # GRAVITY Ragnarok Online Patch File
	'LSW' , # RPG Maker Saved Game File
	'FPG' , # FPS Creator Game Project File
	'MUL' , # Ultima Online Multi File
	'SAV' , # GameCube Saved Game
	'PBN' , # Portable Bridge Notation File
	'NCA' , # Nintendo Content Archive
	'BMS' , # Be-Music Source File
	'PLR' , # rFactor Player File
	'LIP' , # Bethesda Softworks Lip Sync File
	'FST' , # Crazy Machines 2 Object File
	'QWD' , # QuakeWorld Demo File
	'BLASTERBALL3SAVEDGAME' , # BlasterBall 3 Saved Game
	'KWREPLAY' , # Command & Conquer 3 Replay File
	'DM_84' , # Wolfenstein 2.06 Demo Movie File
	'PLAYMISSION' , # MissionPlayer Game File
	'SPRITE' , # SuperTux Sprite File
	'DIF' , # MAME CHD Diff File
	'WU8' , # Wiimm U8 Mario Kart Wii Archive
	'NCF' , # No Cache File
	'ECW' , # EclipseCrossword Word List File
	'NSBTX' , # Nintendo DS Model Texture File
	'MINESWEEPERSAVE-MS' , # Minesweeper Saved Game
	'WHIRLD' , # Unity Whirld Package
	'PUZ' , # Across Lite Crossword Puzzle
	'SCN' , # SimCity 2000 Scenario
	'TOC' , # World of Warcraft Add-on Table of Contents File
	'SOEPSX' , # Player Studio Viewer Item File
	'NL2SCRIPT' , # NoLimits 2 Script File
	'SUD' , # Sudoku Puzzle File
	'JG4' , # BigJig 4 Jigsaw Puzzle
	'E2GM' , # Pangea Enigmo 2 Custom Game File
	'CTX' , # Valve ICE Encoded Script
	'BNK' , # Test Drive Unlimited Data Package
	'ZS8' , # ZSNES Slot 8 Saved State File
	'AC' , # AC3D 3D Definition File
	'JST' , # Jnes Save State
	'BLORB' , # Interactive Fiction Package Game File
	'SD7' , # Spring Content Package
	'SRL' , # Nintendo DS ROM
	'DMO' , # Duke Nukem 3D Demo File
	'UVX' , # Unreal Tournament 2003 Saved Game
	'CDP2' , # Trainz Classics Content Dispatcher Pack 2
	'ERB' , # Evil Genius Resource File
	'WAL' , # Quake 2 Texture
	'DWD' , # DiamondWare Digitized Audio
	'RPGPROJECT' , # RPG Maker MV Project
	'XDS' , # DS Game Maker Project File
	'SQM' , # Operation Flashpoint Mission File
	'UT2' , # Unreal Tournament Map
	'VRMANIFEST' , # SteamVR Manifest
	'NSBCA' , # Nintendo DS Model Animation File
)

# noinspection SpellCheckingInspection
cad_set = (
	'BAK' , # AutoCAD Drawing Backup
	'G' , # FlashForge G-Code File
	'CIR' , # Micro-Cap Schematic
	'AXM' , # FormIt Sketch
	'BDC' , # West Point Bridge Designer Design File
	'CIRCUIT' , # KTechlab Circuit Design File
	'PSM' , # Solid Edge Sheet Metal File
	'IBA' , # Lectra Clothing Design Pieces File
	'SIM' , # SimLab Composer Scene
	'DLV' , # CATIA 4 Export File
	'SMB' , # Autodesk Shape Manager Binary File
	'EPF' , # EAGLE Project
	'CPA' , # CADSTAR PCB Archive File
	'DSNX' , # RoadEng Location Design Document
	'AFS' , # STAAD.foundation Project File
	'MCX' , # MICRO CADAM-X/6000 Model Data File
	'PLUSH' , # Plushify Project
	'OPT' , # Opterecenja File
	'MP12' , # Multisim 12 Project
	'FCSTD1' , # FreeCAD Backup Document
	'TCT' , # TurboCAD Drawing Template
	'MSM' , # Multisim Circuit Design File
	'DB1' , # Tekla Structures Model File
	'FPC' , # FreePCB Printed Circuit Board Layout
	'EDF' , # Edificius Project
	'JVSG' , # Video Surveillance Project
	'MC9' , # Mastercam 9 Geometry File
	'PSV' , # Pipe System Viewer File
	'RTD' , # Robot Structural Analysis Design File
	'LCF' , # Archicad Library Container File
	'TERX' , # RoadEng Terrain Design Document
	'CAD' , # BobCAD-CAM File
	'MS14' , # Multisim 14 Circuit Design File
	'CATPRODUCT' , # CATIA V5 Assembly File
	'JOB' , # MetaCAM Nest Job File
	'MODEL' , # CATIA 3D Model
	'GCODE' , # G-code 3D Printer File
	'RSG' , # RaySupreme Graph
	'JT' , # JT Open CAD File
	'GSM' , # Graphic Description Language File
	'LI3D' , # Live Interior 3D Document
	'IDE' , # Inventor iFeature
	'LOGICLY' , # Logicly Circuit
	'PC7' , # PowerCADD 7 Drawing File
	'CTB' , # Chitubox Sliced 3D Model
	'CIB' , # Luminaire Data File
	'VET' , # Lectra Cutter's Must File
	'AFD' , # Alphacam Flame Drawing
	'NC' , # DSTV Numerical Control File
	'CBDDLP' , # Chitubox Sliced 3D Model
	'MTO' , # FastCAM MTO Text File
	'XV3' , # Lattice XVL Geometry File
	'DC3' , # DesignCAD 3D ASCII Drawing
	'ASY' , # LTspice Symbol File
	'DWT' , # AutoCAD Drawing Template
	'CYP' , # Home Design Project
	'ADI' , # AutoCAD Device-Independent Binary Plotter File
	'FZP' , # Fritzing XML Part Description
	'3DL' , # Sierra LandDesigner 3D File
	'CAD' , # Autodesk QuickCAD File
	'PAT' , # AutoCAD Hatch Pattern File
	'123DX' , # 123D Design Model File
	'ICD' , # IronCAD 2D Drawing File
	'DES' , # Pro/DESKTOP CAD File
	'FZB' , # Fritzing Bin File
	'SAT' , # ACIS SAT 3D Model
	'PSS' , # AutoCAD Plot Stamp Settings File
	'SKF' , # AutoSketch Drawing
	'MS13' , # Multisim 13 Circuit Design File
	'DST' , # AutoCAD Sheet Set
	'GDS' , # Graphic Data System File
	'SCH' , # gEDA Schematic File
	'JVSGZ' , # Compressed Video Surveillance Project
	'MHS' , # Xilinx XPS Hardware Specification File
	'PWT' , # AutoCAD Publish To Web Template
	'EDN' , # EDIF Implementation Netlist File
	'SPT' , # SpeedTree Tree Data File
	'FCW' , # FastCAD Windows Drawing
	'SLDPRT' , # SolidWorks Part
	'BLK' , # AutoCAD Block Template File
	'STL' , # Stereolithography File
	'PHJ' , # PhCNC Project File
	'SCAD' , # OpenSCAD Script
	'BCD' , # RealView Debugger Board Chip Definition File
	'DRU' , # EAGLE Design Rules
	'NWF' , # Navisworks File Set
	'DC2' , # DesignCAD 2D ASCII Drawing
	'LDR' , # LDraw Model File
	'CF2' , # Common File Format File
	'DWFX' , # Design Web Format XPS File
	'PLN' , # Archicad Solo Project
	'NC' , # Mastercam Numerical Control File
	'IGS' , # IGES Drawing
	'123D' , # Autodesk 123D Drawing
	'BDL' , # CoCreate Bundle File
	'LIN' , # AutoCAD Linetype File
	'DSN' , # OrCAD Design File
	'BBCD' , # BobCAD-CAM Document
	'MP11' , # Multisim 11 Project File
	'MIN' , # Okuma CNC Program File
	'MP10' , # Multisim 10 Project File
	'SLDASM' , # SolidWorks Assembly
	'ISO' , # Arbortext IsoDraw Document
	'IDV' , # Inventor Design View
	'PLA' , # Archicad Project Archive
	'XNC' , # Exchange NC File
	'TC2' , # TurboCAD 2D Mac Drawing
	'DC1' , # DevCad Document
	'DWS' , # AutoCAD Drawing Standards File
	'ISOZ' , # Compressed Arbortext IsoDraw Illustration
	'FZBZ' , # Fritzing Bundled Bin File
	'MDE' , # Archicad Education Module
	'STP' , # STEP 3D CAD File
	'IAM' , # Inventor Assembly
	'CDW' , # KOMPAS Document
	'SHX' , # AutoCAD Compiled Shape File
	'DGN' , # MicroStation Design
	'X_B' , # Parasolid Model Part File
	'BMF' , # FloorPlan File
	'FZ' , # Fritzing Project File
	'RIG' , # vRigger Design File
	'RSM' , # RouterSim Network Visualizer File
	'VWX' , # Vectorworks Design
	'FMZ' , # Form•Z Project File
	'PLP' , # Archicad Teamwork Project
	'FZZ' , # Fritzing Shareable Project File
	'CZD' , # CAD Zone Drawing
	'IF' , # Procedimientos-Uno IFWin Project File
	'SVD' , # SolidView Design File
	'PRT' , # CADKEY Part File
	'ASC' , # LTspice Circuit Schematic File
	'FCSTD' , # FreeCAD Document
	'PRT' , # Creo Parametric Part
	'ART' , # ArtCAM Model
	'DWG' , # AutoCAD Drawing
	'CATPART' , # CATIA V5 Part File
	'LTL' , # Luminaire Data File
	'PRJPCBSTRUCTURE' , # Altium Designer PCB Project Structure
	'GXC' , # General CADD Pro Component
	'123' , # PCB123 Circuit Board Design
	'ANY' , # AnyRail Layout Plan
	'SAB' , # ACIS SAB Model
	'CYP' , # Home Designer Pro Project File
	'HSC' , # Aspen HYSYS Simulation File
	'ACT' , # Alma CAD/CAM Document
	'TSF' , # Strater Template File
	'UNT' , # AutoCAD Unit Definition File
	'CAM' , # FastCAM CAM File
	'GX' , # FlashForge 3D Printing File
	'GXM' , # General CADD Pro Macro File
	'EZD' , # EzCad2 Laser Engraving Design
	'MODFEM' , # Femap Model
	'LIBPKG' , # Altium Designer Integrated Library Package
	'DFX' , # Drafix CAD File
	'DXF' , # Drawing Exchange Format File
	'STEP' , # STEP 3D Model
	'MCD' , # MiniCAD Design File
	'XISE' , # Xilinx ISE Project File
	'PRT' , # Unigraphics Part File
	'TAK' , # TAK 2000 Thermal Model File
	'JBC' , # Jam STAPL Byte-Code File
	'NC1' , # Mastercam Numerical Control File
	'MP7' , # Multisim 7 Project File
	'PSF' , # AutoCAD PostScript Patterns File
	'RRA' , # vRigger Rigging Gear File
	'EASM' , # eDrawings Assembly File
	'CDL' , # CADKEY Wireframe Design File
	'CGR' , # CATIA Graphical Representation File
	'JAM' , # Jam STAPL File
	'PRG' , # PC-DMIS Part Program File
	'FNC' , # QuickCAM 3D CNC File
	'TCD' , # TurboCAD Mac Deluxe Drawing
	'EWB' , # Electronics Workbench Circuit Design File
	'DGK' , # Delcam 3D Model File
	'DXX' , # AutoCAD Drawing Interchange Attribute File
	'PRO' , # KiCad Project
	'WDP' , # AutoCAD Electrical Project File
	'CFF' , # Common File Format
	'ICS' , # IronCAD 3D Scene
	'IPT' , # Inventor Part
	'XBF' , # XCAF Binary 3D Model
	'RED' , # CADViewer Redline Markup File
	'DXE' , # AutoCAD Data Extraction Template
	'ELD' , # Eulumdat Formatted Luminaire Data File
	'DSNWRK' , # Altium Designer Workspace File
	'ULD' , # Unified Luminaire Data File
	'TCM' , # TurboCAD Mac Drawing
	'LIZD' , # Live Interior 3D Compressed Document
	'EZP' , # AutoCAD Ecscad Project Backup File
	'G3D' , # GOM 3D File
	'G' , # BRL-CAD Geometry File
	'BPM' , # Bizagi Process Modeler
	'OPJ' , # OrCAD Project File
	'NGC' , # Xilinx Generated Netlist File
	'ARD' , # ArtiosCAD Workspace File
	'SCH' , # EAGLE Schematics File
	'DLX' , # DIALux Document
	'CATDRAWING' , # CATIA V5 Drawing
	'TOPVIW' , # TopSolid'Viewer Document
	'SEW' , # Home Embroidery Format
	'LDT' , # Eulumdat Formatted Luminaire Data File
	'CAM' , # Gerber CAM Job File
	'A2L' , # ASAP2 ECU Description File
	'123C' , # Autodesk 123C Drawing
	'IPN' , # Inventor Presentation
	'IDW' , # Inventor Drawing
	'WDF' , # Wintech Drawing File
	'DC' , # DesignCAD Design File
	'BRD' , # KiCad PCB Design File
	'PHO' , # Gerber Photoplot File
	'CNC' , # CNC Machine Tool Path
	'DRWDOT' , # SolidWorks Drawing Template
	'GXD' , # General CADD Pro Drawing
	'HSF' , # HOOPS Stream Format File
	'GCD' , # Generic CADD Drawing File
	'DRG' , # AllyCAD Drawing File
	'BXL' , # Accelerated Designs PCB Library File
	'TSC' , # TINA Design File
	'2D' , # VersaCAD 2D Drawing File
	'MIN' , # MAX Interchange Notation File
	'GEO' , # TruTops Geometry File
	'PRT' , # Solid Edge Part File
	'3W' , # XYZprinting Sliced Model File
	'LAY6' , # Sprint Layout 6 PCB Layout File
	'MS11' , # Multisim 11 Circuit Design File
	'BIT' , # Xilinx Bitstream File
	'SCH' , # KiCad Schematic File
	'FPD' , # Front Panel Designer Project
	'DFT' , # Solid Edge Draft Document
	'SLDDRW' , # SolidWorks Drawing
	'CNC' , # PartMaster CNC File
	'RDF' , # Relux Project File
	'DCD' , # DesignCAD Drawing
	'OLB' , # OrCAD Symbol Library File
	'CDDX' , # Circuit Diagram Document
	'FZM' , # Fritzing Module File
	'BREP' , # BRep 3D Model File
	'MS12' , # Multisim 12 Circuit Design File
	'PM3' , # Crouzet Logic Software M3 File
	'EQN' , # GAL Equation File
	'IFCXML' , # Industry Foundation Classes XML File
	'DRW' , # Personal Designer Drawing File
	'LIA' , # P-CAD ASCII Library file 
	'NPL' , # Xilinx ISE 5-6 Project File
	'BPMC' , # Bizagi Process Modeler Collaboration
	'NEU' , # Pro/ENGINEER Neutral File
	'IC3D' , # IC3D Scene File
	'ASMDOT' , # SolidWorks Assembly Template 
	'ASM' , # Solid Edge Assembly File
	'EASMX' , # eDrawings Assembly XPS File
	'RCM' , # Autodesk Reality Capture Mesh
	'SZA' , # HiCAD Drawing File
	'X_T' , # Parasolid Model Part File
	'EPRT' , # eDrawings File
	'MP13' , # Multisim 13 Project
	'BIMX' , # BIM Explorer File
	'SYM' , # gEDA Symbol File
	'EWD' , # EasyWOOD Design File
	'TGI' , # TowGeom Input File
	'GINSPECT_PRJ' , # GOM Inspect Project File
	'DGB' , # FlashCAD Drawing Database
	'PC6' , # PowerCADD 6 Drawing File
	'FAN' , # Form•Z Animation File
	'ADT' , # AutoCAD Audit Log
	'TCD' , # Technobox CAD Drawing
	'T3001' , # TARGET 3001! Circuit Design Project File
	'DRA' , # Dolphin PartMaster Drawing File
	'IPF' , # iMPACT Project File
	'TC3' , # TurboCAD 2D/3D Mac Drawing
	'PRTDOT' , # SolidWorks Part Template
	'MMG' , # Model Master 3 Axis Flat File
	'VDS' , # Visual Design Stream
	'MFP' , # Markforged Print
	'LOGICLYLIB' , # Logicly Integrated Circuit Library
	'LFVIEW' , # Leapfrog Viewer Scene
	'HUS' , # Husqvarna Embroidery File
	'WBT' , # Webots World
	'CHITUBOX' , # Chitubox Project
	'SCHDOC' , # Altium Designer Schematic Document
	'SCDOC' , # SpaceClaim Document
	'NWD' , # Navisworks Document
	'EXB' , # CAXA Draft
	'IPJ' , # Inventor Project
	'BRD' , # Cadence Allegro PCB Design
	'CRV' , # VCarve Pro Design File
	'NWC' , # Navisworks Cache File
	'VND' , # Type3 Design File
	'PSU' , # PSU Designer Document
	'LYR' , # bestIMAGE Design File
	'MP14' , # Multisim 14 Project
	'PIPE' , # PIPE-FLO Project File
	'CND' , # Melco Condensed Embroidery File
	'RCD' , # LEGO MINDSTORMS Recording File
	'TCW' , # TurboCAD Drawing File
	'TBP' , # Tekla BIMsight Project
	'FLX' , # FelixCAD Drawing
	'VTF' , # 3DESIGN CAD File
	'MVS' , # Microchip Verification Specification File
	'MDL' , # Lectra Modaris Clothing Design File
	'TCP' , # TurboCAD Mac Pro Drawing
	'CMP' , # Solid Edge Wire Harness File
	'SDG' , # Strater Project File
	'NGD' , # Xilinx Netlist File
	'LYC' , # bestIMAGE Compressed Design File
	'EDRW' , # SolidWorks eDrawings File
	'TCX' , # TurboCAD 3D Model Text File
	'RML' , # Redline Markup Language File
	'TPL' , # Archicad Template
	'PRJPCB' , # Altium Designer PCB Project
	'PCS' , # Pfaff Embroidery File
	'PLC' , # Archicad Teamwork Draft
	'PRJSCR' , # Altium Designer Script Project
	'KIT' , # 2020 Design Live File
	'ATT' , # Alphacam Lathe Tool File
	'DRW' , # ANVIL Drawing File
	'TOPPRJ' , # TopSolid Project
	'GXH' , # General CADD Pro Hatch Pattern File
	'DVG' , # Graphic Works Vector Graphic
	'ISE' , # Xilinx ISE Project
	'DBQ' , # AutoCAD Query Set
	'3DC' , # RoboCell Cell Setup File
	'MP8' , # Multisim 8 Project
	'TOP' , # TopSolid Design Document
	'CKD' , # KeyCreator Design File
	'SBP' , # OpenSBP CNC Program File
	'RS' , # RapidSketch Document
	'GBX' , # Gerber PCB File
	'BSW' , # StruWalker 3D Model
	'SMG' , # SolidWorks Composer File
	'PRJMBD' , # Altium Designer Multi-board Project
	'IQP' , # IntelliQuilter Pattern
	'PWD' , # Solid Edge Weldment Document
	'DRA' , # OrCAD Drawing File
	'QPM' , # Quick Pallet Maker Input Data
	'MP9' , # Multisim 9 Project File
	'EZC' , # AutoCAD Ecscad Components Backup File
	'MS9' , # Multisim 9 Circuit Design File
	'MS7' , # Multisim 7 Circuit Design File
	'FPP' , # Front Panel Project
	'DXO' , # DesignXOver Speaker Circuit Design File
	'SMT' , # Autodesk Shape Manager Text File
	'STA' , # Vectorworks Design Template
	'FM' , # FeatureCAM Part
	'PIPD' , # PIPE-FLO Demo Project File
	'BBCDX' , # BobCAD-CAM Document
	'CKT' , # KeyCreator Design Template
	'CYG' , # Home Design Object
	'AD_PRT' , # Alibre Design Part
	'IDCL' , # Inventor Desktop Content Library File
	'RCV' , # RoofCon Viewer Project File
	'PBB' , # Protobot Beta Project
	'L3B' , # LEGO 3D Model File
	'PPA' , # Archicad Teamwork Project Archive
	'BSWX' , # BIMReview 3D Model
	'BPZ' , # Easy Blue Print Symbol Library
	'SCH' , # ExpressSCH Schematics File
	'MOD' , # Femap Model
	'DSG' , # Control Studio Document
	'DC2' , # DevCad Cam Pro Document
	'SST' , # Artec Studio Project
	'IFCZIP' , # Compessed IFC File
	'HCP' , # HydroCAD Project File
	'UPF' , # Unified Power Format File
	'CEL' , # MicroStation Cell Library
	'MOD' , # Archicad Module
	'FCD' , # FastCAD DOS Drawing
	'CMP' , # Generic CADD Component File
	'PCA' , # Archicad Teamwork Draft Archive
)

# noinspection SpellCheckingInspection
gis_set = (
	'GMAP' , # Garmin Map File
	'NM2' , # Navitel Map
	'SP3' , # NGS SP3 File
	'SBN' , # GT-31 Binary Data File
	'SSF' , # Trimble Standard Storage Format File
	'DT0' , # DTED Level 0 File
	'AVL' , # ArcView Legend File
	'MXT' , # ArcGIS Map Template
	'WOR' , # MapInfo Workspace File
	'QGS' , # QGIS Project File
	'WLX' , # MapSource Web Link File
	'DIV' , # DIVA-GIS Project File
	'APL' , # ArcPad Layer File
	'3DL' , # iGO Landmark File
	'MPK' , # ArcGIS Map Package
	'3D' , # Survex 3D Cavern File
	'SAF' , # Street Atlas USA Map File
	'OSC' , # OpenStreetMap Change File
	'DEM' , # Digital Elevation Model
	'OSM' , # OpenStreetMap Map
	'EMBR' , # Spatial Geodatabase File
	'PTM' , # MapPoint Map File
	'GST' , # MapInfo Geoset File
	'GML' , # Geography Markup Language File
	'DT1' , # DTED Level 1 File
	'TFRD' , # Tape Image Format Requirements Document
	'3DC' , # iGO 3D Building File
	'NMC' , # ArcGIS Explorer Map Content File
	'ATX' , # ArcGIS Attribute Index File
	'MSD' , # Map Service Definition File
	'CXF' , # Coordinates Export Format
	'MIF' , # MapInfo Interchange Format File
	'FMW' , # FME Workbench File
	'PMF' , # ESRI Published Map File
	'KMZ' , # Google Earth Placemark File
	'HDR' , # ESRI BIL Header File
	'MXD' , # ArcGIS Map Document
	'BT' , # Binary Terrain File
	'TIMESTAMP' , # Geodatabase Timestamp File
	'KML' , # Keyhole Markup Language File
	'DIX' , # DIVA-GIS Export File
	'OBF' , # OsmAnd Offline Map File
	'FBL' , # iGO Map File
	'GEOJSON' , # GeoJSON File
	'LAN' , # ERDAS LAN File
	'CSF' , # GeoMedia Coordinate System File
	'SDF' , # Spatial Data Format File
	'NMF' , # ArcGIS Explorer Map File
	'DMT' , # DeLorme Transfer File
	'E00' , # ArcInfo Coverage Export File
	'GRB' , # GRIB Meteorological Data File
	'QPJ' , # QGIS Map Projection File
	'AUX' , # Auxiliary File
	'GPRX' , # Geoxa Project File
	'PRM' , # Route 66 Map File
	'GSM' , # MapViewer Project File
	'PYT' , # Python Toolbox
	'QGZ' , # Zipped QGIS Project
	'JOZ' , # Java OpenStreetMap Session
	'CML' , # OGC Web Map Context Document
	'JNX' , # BirdsEye JNX Raster Map
	'LEN' , # IMAGINE Lens Flare File
	'GVSP' , # Geocortex Viewer for Silverlight Project
	'GSI' , # Golden Software Interchange File
	'PIX' , # PCI Geomatics Database File
	'WLD' , # ESRI World File
	'ADF' , # ESRI ArcInfo Binary Grid Format
	'IMI' , # Magellan Map File
	'RMP' , # Magellan Raster Map File
	'GSR' , # Golden Software Reference File
	'GPX' , # GPS Exchange File
	'REF' , # IDRISI Raster Image Reference File
	'GPS' , # Survey Pro Coordinate File
	'RDF' , # ArcGIS Report Document File
	'RDC' , # IDRISI Raster Documentation File
	'AQM' , # AlpineQuest Map File
	'IMD' , # GIS Image Metadata File
	'DMF' , # Geosystem Digitals Map File
	'GSB' , # Golden Software Boundary File
	'HGT' , # SRTM Elevation Data File
	'MWX' , # MapGuide Author Map Window XML File
	'SBN' , # ESRI Spatial Binary File
	'SVX' , # Survex Cavern Data File
	'VDC' , # IDRISI Vector Documentation File
	'VCT' , # IDRISI Vector Image
	'TAB' , # MapInfo TAB File
	'MMM' , # MiraMon Maps File
	'STYLE' , # ArcGIS Style Manager File
	'RGN' , # Garmin Regional Map File
	'JGW' , # JPEG World File
	'CUB' , # ISIS Cube File
	'AXE' , # AutoRoute Map File
	'GLB' , # STK Globe File
	'FIT' , # Garmin Activity File
	'USR' , # Lowrance GPS Data File
	'SGY' , # SEG-Y Data File
	'SDW' , # MrSID World File
	'MID' , # MapInfo Data File
	'ETA' , # Google Earth Placemark File
	'MMZ' , # MiraMon Compressed Map File
	'JPGW' , # JPEG World File
	'GMF' , # Geology Multi-File
	'VEC' , # IDRISI Vector File
	'JOS' , # Java OpenStreetMap Session Definition
	'SLD' , # Styled Layer Descriptor File
	'MWM' , # Maps.me Map
	'STT' , # Streets & Trips Map Template
	'FME' , # FME Mapping File
	'FMI' , # FME Include File
	'LPK' , # ArcGIS Layer Package
	'3DD' , # ArcGlobe Document
	'GPKG' , # GeoPackage Database
	'IMG' , # ERDAS IMAGINE Image File
	'PTT' , # MapPoint Map Template
	'MDC' , # Merkaartor Document
	'XOL' , # Swiss Map Overlay
	'TFW' , # World File for TIFF
	'JPW' , # World File for JPEG
	'MPS' , # Pocket Streets Map File
	'DLG' , # Digital Line Graph
	'SHP' , # ESRI Shapefile
	'OV2' , # TomTom Points of Interest Database
	'FDS' , # FME Custom Format File
	'QLV' , # QGIS layerVersion Save File
	'TTKGP' , # TatukGIS Project File
	'OCD' , # OCAD Map File
	'NM7' , # Navitel 7 Map
	'IMG' , # Garmin Map File
	'SXD' , # ArcScene Document
	'PIN' , # MNS Projection Information File
	'NV2' , # Navionics Chart File
	'TH' , # Therion Data File
	'ERS' , # ER Mapper Data File
	'TCX' , # Training Center XML File
	'BIL' , # ESRI BIL File
	'AT5' , # Lowrance Map File
	'NGT' , # Noni GPSPlot Track File
	'477' , # MNS Shape Definition Companion File
	'SMP' , # IDRISI Palette File
	'APR' , # ArcView Project File
	'NM3' , # Navitel 5 Map
	'NMAP' , # NaviComputer Map File
	'SEGY' , # SEG-Y Data File
	'SMM' , # Map Manager Schema File
	'QRT' , # QuickRoute Map
	'OSB' , # OpenStreetMap Binary Map File
	'MNH' , # Master Navigator Header File
	'FFS' , # FME Desktop Feature Store File
	'MAP' , # NAVIGON Map File
	'EST' , # Streets & Trips Map File
	'RST' , # IDRISI Raster Image
	'BPW' , # Bitmap World File
	'FLS' , # ArcView GIS Windows Help Supporting File
	'TPX' , # DeLorme Topo Project File
	'FMV' , # FME Desktop Viewspace File
	'GTM' , # GPS TrackMaker Data File
	'DT2' , # DTED Level 2 File
	'RRD' , # Reduced Resolution Dataset File
	'MAP' , # Mapjects Server Webparts File 
	'GPF' , # Geosoft Project File
	'ANZ' , # ENVI Image Annotation
	'GWS' , # Geosoft Workspace File
	'GSR2' , # Golden Software Reference File
	'AXT' , # Microsoft AutoRoute Template
	'SBF' , # Septentrio Binary File
	'AQX' , # AlpineQuest XML Map Configuration
	'WKT' , # Well-Known Text Geometry
	'TRB' , # Tripy Road Book
	'SVP' , # Subsurface Viewer Project
	'SXF' , # Secure eXchange Format File
	'CVI' , # CassiniVision Map Image
	'MGM' , # MGMaps File
	'QCT' , # Memory-Map QuickChart File
	'SYM' , # TatukGIS Symbols File
	'JPR' , # Fugawi Projection File
	'SDM' , # Spatial Data Modeling Language File
	'GFW' , # GIF World File
	'COR' , # Trimble Corrected SSF File
	'WFD' , # Wayfinder Map File
	'DVC' , # IDRISI Vector Definition File
)

# noinspection SpellCheckingInspection
web_set = (
	'ASTX' , # Active Server Template Extended File
	'SCSS' , # Sass Cascading Style Sheet
	'GMI' , # Gemtext Document
	'MML' , # Mathematical Markup Language File
	'KEY' , # Privacy-Enhanced Mail Private Key
	'XD' , # Adobe XD Document
	'SSP' , # Scala Server Page
	'BTAPP' , # uTorrent App
	'XHTM' , # Extensible Hypertext Markup Language Document
	'H5P' , # HTML5 Content Package
	'ARO' , # SteelArrow Web Application File
	'P7B' , # PKCS #7 Certificate File
	'ASAX' , # ASP.NET Server Application File
	'A5W' , # Alpha Five Webpage File
	'MJS' , # Node.js ES Module File
	'BML' , # Bean Markup Language File
	'JSPA' , # Java Servlet Alias
	'VRT' , # Virtual World
	'DLL' , # DLL Dynamic Webpage
	'PHP' , # PHP Source Code File
	'WHTT' , # HTTrack Website Copier File
	'CSHTML' , # ASP.NET Razor Webpage
	'DHTML' , # Dynamic HTML file
	'CRT' , # Security Certificate
	'WEBSITE' , # Internet Explorer Pinned Site Shortcut
	'DOWNLOAD' , # Partially Downloaded File
	'XUL' , # XML User Interface Language File
	'JS' , # JavaScript File
	'OBML15' , # Opera Mini Saved Webpage
	'WEBBOOKMARK' , # Safari Web Bookmark File
	'FWTEMPLATE' , # Freeway Template
	'RJS' , # Ruby Javascript File
	'WKDOWNLOAD' , # Epiphany Partially Downloaded File
	'VSDISCO' , # DISCO Dynamic Discovery Document
	'NOJEKYLL' , # GitHub Pages NoJekyll File
	'ASTRO' , # Astro Website Component
	'P7' , # PKCS #7 Digital Certificate File
	'DCR' , # Shockwave Media File
	'CSS' , # Cascading Style Sheet
	'ZUL' , # ZK User Interface File
	'DOCHTML' , # Microsoft Word HTML Document
	'PAC' , # Proxy Auto-Config File
	'SPC' , # Software Publisher Certificate File
	'HTACCESS' , # Apache HTACCESS File
	'P7C' , # PKCS #7 Certificate File
	'PEM' , # Privacy Enhanced Mail Certificate
	'TPL' , # HTTP File Server Template
	'SITE' , # GoLive Website Project File
	'A4P' , # A4Desk Pro Project File
	'SEAM' , # Seam Framework Java Servlet File
	'ASPX' , # Active Server Page Extended Webpage
	'JSON' , # JavaScript Object Notation File
	'KIT' , # CodeKit File
	'APPCACHE' , # HTML5 Cache Manifest File
	'SVR' , # Compressed Virtual World
	'DAP' , # Access Data Access Page
	'DWT' , # Dreamweaver Webpage Template
	'HTM' , # Hypertext Markup Language File
	'WBS' , # WebBlender Project File
	'OAM' , # Adobe Edge Animate Widget File
	'HTC' , # HTML Component File
	'URL' , # URL Shortcut File
	'ASR' , # ActionScript Remote Document
	'GSP' , # IMail Error Message File
	'AP' , # Active Page
	'ASCX' , # ASP.NET User Control File
	'WSDL' , # Web Services Description Language File
	'EDGE' , # Adobe Edge Project File
	'OBML16' , # Opera Mini Saved Webpage
	'CHAT' , # IRC Chat Configuration File
	'MASTER' , # ASP.NET Master Page
	'GNE' , # Flickr Webpage
	'BR' , # Brotli Compressed File
	'CHM' , # Compiled HTML Help File
	'SHTML' , # Server Side Include HTML File
	'STRM' , # Stream Location File
	'EPIBRW' , # Web File Location
	'RW3' , # RapidWeaver 3 Site File
	'WARC' , # Web Archive
	'PUBLIC' , # OpenSSL Public Key
	'XPD' , # XML Pipeline Document
	'DISCO' , # DISCO Discovery Document
	'GSP' , # Groovy Server Page
	'FWP' , # Microsoft Expression Web Package
	'PRO' , # Adobe Proto Design File
	'CSR' , # Certificate Signing Request File
	'P12' , # Personal Information Exchange File
	'SITES2' , # iWeb Website Design Project
	'PHP4' , # PHP 4 Web Page
	'BOK' , # ProStores Dynamic Webpage
	'PHP2' , # PHP 2 Web Page
	'WEBMANIFEST' , # Progressive Web Application Manifest
	'HAR' , # HTTP Archive FIle
	'OPML' , # Outline Processor Markup Language File
	'ASP' , # Active Server Page
	'NZB' , # NewzBin Usenet Index File
	'DER' , # DER Digital Certificate
	'OBML' , # Opera Mini Saved Webpage
	'VDW' , # Visio Web Drawing
	'XSS' , # XML Style Sheet
	'MHTML' , # MIME HTML File
	'PHTML' , # PHP Web Page
	'AXD' , # ASP.NET Web Handler File
	'RHTML' , # Ruby HTML Web Page
	'CON' , # Concept Application Source File
	'RSS' , # Rich Site Summary
	'ASHX' , # ASP.NET Web Handler File
	'HTML' , # Hypertext Markup Language File
	'SRF' , # Server Response File
	'ALX' , # ActiveX Layout Control File
	'FMP' , # 123 Flash Menu Project
	'DUCKLOAD' , # DuckDuckGo Partially Downloaded File
	'SPARKLE' , # Sparkle Website Project
	'CHA' , # IRC Chat Configuration
	'HYPERESOURCES' , # Hype Resources Folder
	'RAZOR' , # Razor Component File
	'OLP' , # Office Live Package
	'JSON5' , # JSON5 Data File
	'ASA' , # ASP Configuration File
	'CMS' , # Content Management System
	'QF' , # Qompose Web Page Template
	'SHT' , # HTML File with Server Side Includes
	'BROWSER' , # ASP.NET Browser Definition File
	'AEX' , # Alpha Five Compiled Global Functions File
	'DO' , # Java Servlet
	'JNLP' , # Java Web Start File
	'SDB' , # SocialDecks Blog File
	'CRL' , # Certificate Revocation List File
	'PPTMHTML' , # Microsoft PowerPoint MIME HTML Presentation
	'UHTML' , # UC Browser Saved Webpage
	'NODE' , # Node.js Binary Addon File
	'XBEL' , # XBEL Bookmarks File
	'WEB' , # Xara Web Document
	'WDGT' , # Opera Widget
	'XFDL' , # XFDL File
	'JSP' , # Jakarta Server Page
	'FWTEMPLATEB' , # Freeway Template Bundle
	'EWP' , # Ewisoft Website Project File
	'HYPESYMBOL' , # Hype Symbol File
	'WEBLOC' , # macOS Website Location
	'ZHTML' , # RabbitWeb Webpage
	'CFM' , # ColdFusion Markup File
	'CER' , # Internet Security Certificate
	'VRML' , # VRML File
	'PAGE' , # HybridJava Web Page
	'DBM' , # ColdFusion Server File
	'AWM' , # AllWebMenus Project File
	'MJML' , # Mailjet Markup Language File
	'DOTHTML' , # Microsoft Word HTML Document Template
	'WML' , # Wireless Markup Language File
	'COMPRESSED' , # WordCompress Compacted Webpage
	'MHT' , # MHTML Web Archive
	'XHT' , # XHTML File
	'JSPX' , # XML Jakarta Server Page
	'PSP' , # PL/SQL Server Page
	'JSONL' , # JSON Lines File
	'SHTM' , # HTML Server Side Include File
	'MAFF' , # Mozilla Archive Format File
	'SITEMAP' , # ASP.NET Site Map File
	'DML' , # DynaScript File
	'TPL' , # PrestaShop Template File
	'ITMS' , # iTunes Music Store Link File
	'JSS' , # JavaScript Style Sheet File
	'XHTML' , # Extensible Hypertext Markup Language File
	'UCF' , # WebEx Media File
	'WPP' , # WebPlus Project File
	'STML' , # SSI HTML File
	'RT' , # RealText Streaming Text File
	'VBD' , # Visual Basic ActiveX Document
	'DISCOMAP' , # DISCO Discovery Output File
	'SASS' , # Syntactically Awesome StyleSheets File
	'STC' , # Contribute Connection Key File
	'HXS' , # Help 2 Compiled Help File
	'XBL' , # Extensible Binding Language File
	'NDJSON' , # Newline Delimited JSON File
	'ZVZ' , # Possible Virus File
	'CSP' , # Concept Server Page
	'HYPE' , # Hype Document
	'HDML' , # Handheld Device Markup Language File
	'JHTML' , # Java HTML Web Page
	'XWS' , # Xara Web Designer Graphic
	'SVC' , # WCF Web Service File
	'PUB' , # Public Key File
	'PRF' , # PICS Rules File
	'ESPROJ' , # Espresso Project File
	'PRIVATE' , # OpenSSL Private Key
	'WBXML' , # WAP Binary XML File
	'OGNC' , # Dynamic Web Page
	'FWTB' , # Freeway Template Bundle
	'SITES' , # iWeb Website Design Project
	'HTX' , # HTML Extension File
	'SRL' , # OpenSSL Security Certificate Serial Number
	'PHTM' , # PHP-Based Web Page
	'WBN' , # Web Bundle
	'ADR' , # Opera Bookmarks File
	'NOD' , # NetObjects Fusion File
	'MUSE' , # Adobe Muse Website Project
	'CODASITE' , # Coda Site File
	'IQY' , # Internet Query
	'ZHTML' , # Secure IE Zipped HTML File
	'HYPETEMPLATE' , # Hype Document Template
	'WEBARCHIVE' , # Safari Web Archive
	'QBO' , # QuickBooks Online Bank Statement File
	'ECE' , # Escenic Dynamic Web Page
	'MSPX' , # Microsoft ASP.NET Web Page
	'TVVI' , # TitanTV Television Listing File
	'DOCMHTML' , # Microsoft Word MIME HTML Document
	'HDM' , # HDML File
	'MVC' , # MivaScript Compiled File
	'WGP' , # Web Gallery Project
	'WOA' , # WebObjects Application
	'LASSO' , # Lasso Database-driven Webpage
	'FREEWAY' , # Freeway Document
	'IDC' , # Internet Database Connector File
	'LBC' , # Cloaked Affiliate Link Builder Saved Link
	'OTH' , # OpenDocument HTML Template
	'ATT' , # Web Form Post Data File
	'VBHTML' , # ASP.NET Razor Web Page
	'PHP3' , # PHP 3 Web Page
	'ZFO' , # XSL-FO Online Form
	'WEBHISTORY' , # Safari Web History File
	'WIDGET' , # HybridJava Widget
	'MAPX' , # Mapjects Client Webparts File
	'MAP' , # Image Map
	'SWZ' , # Adobe Flash Player Cache File
	'ITPC' , # iTunes Podcast Link
	'JWS' , # Java Web Services File
	'BWP' , # BuRg3r Web Page
	'ATOM' , # Atom Syndication Format File
	'TVPI' , # TitanTV Television Listing File
	'ASMX' , # ASP.NET Web Service File
	'STL' , # Certificate Trust List
	'CDF' , # Channel Definition Format
	'WGT' , # Opera Widget File
	'RWSW' , # RapidWeaver Site File
	'RFLW' , # Edge Reflow Data File
	'WEBARCHIVEXML' , # Android Web Browser Archive
	'PHP5' , # PHP 5 Web Page
	'FACES' , # JavaServer FacesServlet Pointer
	'MVR' , # IBM HotMedia Multimedia File
	'LESS' , # LESS Style Sheet
	'RWP' , # RapidWeaver Package
	'VLP' , # LiveSite Project
	'PTW' , # AutoCAD Publish To Web File
	'NXG' , # eSite Builder NXG Web Page
	'QRM' , # Qworum Message File
	'SAVEDDECK' , # Nokia Saved Web Page
	'CCBJS' , # CopperCube JavaScript File
	'AN' , # Adobe Edge Animate Project File
	'WN' , # WN Web File
	'PPTHTML' , # Microsoft PowerPoint HTML Presentation
	'STP' , # SharePoint Template
	'STM' , # SSI Web Page
	'SUCK' , # SiteSucker Website File
	'JST' , # JavaServer Page Document
	'CPHD' , # CloudChan Pre-processed Hypertext Document
	'QBX' , # Blocks Landing Page
	'CFML' , # ColdFusion Markup Language File
	'IWDGT' , # iWeb Widget File
	'RWTHEME' , # RapidWeaver Theme
	'JVS' , # JavaScript Proxy Autoconfig File
	'JCZ' , # Liquid Motion Animation
	'HTML.MEM' , # Hypertext Markup Language Memory File
	'WPX' , # WebPlus Project Template
	'CPG' , # Cool Page Project File
	'KCMSF' , # Korean Central News Agency Website Script
	'MOZ' , # Mozilla Cache File
	'ARC' , # Internet Archive Web Archive
	'FCGI' , # FastCGI File
)

# noinspection SpellCheckingInspection
plugin_set = (
	'APLG' , # Audials Plug-in
	'AMXX' , # AMX Mod X Plugin File
	'MILK' , # MilkDrop Winamp Plug-in Preset
	'ASI' , # ASI Mod Script
	'CCIP' , # Curse Client Install Package
	'REZ' , # Escape Velocity Plug-in File
	'SCM' , # GIMP Script-Fu Script
	'RPLN' , # InDesign Required Plug-in File
	'TPI' , # EDIUS Plugin File
	'DLR' , # finalRender Plugin
	'IADCLASS' , # iAd Producer Plugin Class Description File
	'RBZ' , # SketchUp Plugin
	'TGP' , # Terragen Plugin
	'ACROPLUGIN' , # Adobe Acrobat Plug-in
	'BAV' , # AVG AntiVirus The Bat! Plugin File
	'CLEO' , # CLEO Grand Theft Auto Mod File
	'AMX' , # AMX Mod Plugin File
	'LNG' , # Winamp Language File
	'MDA' , # Access Add-in
	'CRX' , # Chromium Extension
	'ECF' , # Outlook Add-in
	'FB2K-COMPONENT' , # Foobar2000 Component Install File
	'OIV' , # OpenIV Mod Package File
	'VBOX-EXTPACK' , # Oracle VM VirtualBox Extension Pack
	'MDE' , # Compiled Access Add-in File
	'INX' , # Inkscape Extension Descriptor File
	'ALP' , # Ableton Live Pack File
	'ASSETPROJ' , # Camtasia Studio Asset Information File
	'VSIX' , # Visual Studio Extension
	'8BI8' , # Photoshop Plug-in
	'NY' , # Audacity Nyquist Plug-in
	'DLO' , # 3ds Max Plug-in
	'8BX' , # PhotoDeluxe Plug-in
	'8BI' , # Photoshop Plug-in
	'SKYPECHATSTYLE' , # Chat Style File
	'RWPLUGIN' , # RapidWeaver Plugin
	'AAXPLUGIN' , # Avid Audio eXtension Plugin File
	'PLGX' , # KeePass 2.x Plugin
	'OEX' , # Opera Extension
	'AVX' , # Avid Plugin File
	'SPARC' , # Skype Plugin Archive
	'EAZ' , # ArcGIS Explorer Add-in File
	'ZXP' , # Extension Manager Package
	'FLT' , # Adobe Audition Filter
	'XPI' , # Cross-platform Installer Package
	'WLZ' , # Winamp Language Pack
	'NBM' , # NetBeans Module
	'INFO' , # Drupal Module Information File
	'BLU' , # FileMaker Pro Runtime Extension
	'XPT' , # Mozilla Firefox Component
	'AEX' , # After Effects Plug-in
	'API' , # Acrobat Plug-in
	'PRM' , # Premiere Plug-in
	'8BF' , # Photoshop Filter Plug-in
	'SAFARIEXTZ' , # Apple Safari Extension
	'8BF' , # Filter Factory Image Filter
	'8BA' , # Photoshop Plug-in
	'OXT' , # Apache OpenOffice Extension
	'MODULE' , # Drupal Module File
	'ACCDU' , # Access Add-in File
	'VDF' , # VirtualDub Video Filter
	'VST' , # VST Audio Plug-in
	'PLW' , # 32-bit Windows IDA Plugin Module
	'FZIP' , # Foxit Reader Add-on
	'X32' , # Adobe Xtra File
	'IADPAGE' , # iAd Producer Plugin Page Template
	'APLP' , # Audials Plug-in Package
	'HVPL' , # iTunes Visual Plug-in File
	'BKZ' , # BaKoMa TeX Installation Module
	'XSIADDON' , # Softimage Add-on File
	'XLAM' , # Microsoft Excel Macro-Enabled Add-In
	'QTX' , # QuickTime Extension
	'XLV' , # Microsoft Excel Visual Basic Module
	'PLUGIN' , # Adobe Photoshop Plug-in
	'VST3' , # VST 3 Audio Plug-in
	'8LI' , # Photoshop Scripting Plug-in
	'BUNDLE' , # macOS Loadable Bundle
	'AIP' , # Adobe Illustrator Plug-in
	'MXADDON' , # Maxthon Add-On File
	'WLL' , # Microsoft Word Add-in File
	'LNG' , # Acrobat Language Plugin
	'SUBLIME-PACKAGE' , # Sublime Text Package File
	'AVX' , # ArcView Extension File
	'TKO' , # AMX Software Module File
	'NET' , # FileMaker Networking Module
	'PLUGIN' , # Mac OS X Plugin
	'JSXBIN' , # Binary ExtendScript Script File
	'RPM' , # RealPlayer Plug-in
	'XLL' , # Microsoft Excel Add-in
	'P64' , # 64-bit Windows IDA Plugin Module
	'RHP' , # Rhino Plug-in
	'CMP' , # HotDocs Clause Component File
	'M3P' , # Mach3 Plugin File
	'MFX' , # Clickteam Fusion Extension
	'AGMODULE' , # Adobe Lightroom Module
	'BMI' , # 3ds Max Plug-in
	'QLPLUGIN' , # QuickLook Plugin
	'EPK2' , # E-Prime 2.0 Package File
	'ACCDA' , # Access Add-in File
	'S4DIX' , # Scratch for Discord Extension
	'FDA' , # Free Download Manager Add-on
	'SEPLUGIN' , # AppleScript Editor Plug-in
	'MDIMPORTER' , # Metadata Importer
	'XMD' , # Bitdefender Plugin File
	'PIM' , # Pro Tools Controller Plug-in Mappings File
	'APD' , # Eclipse Plugin Descriptor File
	'MXP' , # Adobe Extension Package
	'AIX' , # MIT App Inventor Extension
	'AAUI' , # Acrobat User Interface File
	'BRM' , # Bryce Plug-in File
	'8BE' , # PhotoDeluxe Plug-in
	'FSB' , # Form•Z Compiled Script
	'FXT' , # Finale Plug-in
	'PPA' , # Microsoft PowerPoint Add-in
	'NBP' , # NeoBook Plugin File
	'MMIP' , # MediaMonkey Installation Package
	'IBPLUGIN' , # Interface Builder Plug-in
	'FWACTION' , # Freeway Action File
	'8BY' , # PhotoDeluxe Plug-in
	'IADCOMPONENT' , # iAd Producer Plugin Component File
	'PPAM' , # Microsoft PowerPoint 2007 Add-In
	'APL' , # ACDSee Plugin File
	'DLU' , # 3ds Max Utility Plug-in File
	'DPM' , # Pro Tools Plugin File
	'ARX' , # AutoCAD Runtime Extension File
	'FMX' , # FileMaker Plug-in
	'VSL' , # Visio Add-on
	'COMPONENT' , # Mac OS X System Component
	'PFL' , # PhotoFiltre Plugin
	'MTX' , # MadTracker 2 Extension
	'MLL' , # Maya Plug-in
	'KMM' , # K-Meleon Macro Module
	'8BS' , # Photoshop 5.0 Selection Plug-in
	'EXT' , # Norton Commander Extension
	'SPE' , # SPSS Extension
	'DBX' , # AutoCAD Database Extension File
	'APE' , # AVS Plugin Effects File
	'VDPLUGIN' , # VirtualDub Plugin File
	'8BC' , # Photoshop 5.0 Plug-in
	'PLN' , # Adobe InDesign Plug-in
	'EBAPLUGIN' , # EBA Command Center Plug-in
	'ROCK' , # Rockbox Plug-in
	'Q9S' , # Mr. Contrast Photoshop Plug-In
	'IADSTYLE' , # iAd Producer Plugin Style Template
	'Q1Q' , # LunarCell Photoshop Plug-in
	'APEXT' , # AudioPipes Extension
	'OFX' , # OpenFX Plugin
	'FMPLUGIN' , # FileMaker Plug-in
	'ZPI' , # DivX Connected Plug-in File
	'RXT' , # Retrospect Backup Extension
	'LRPLUGIN' , # Adobe Lightroom Classic Plug-in
	'FWACTIONB' , # Freeway Action Bundle File
	'RPLIB' , # Axure RP Widget Library File
	'ASEPRITE-EXTENSION' , # Aseprite Extension
	'NVDA-ADDON' , # NVDA Add-On
	'TMBUNDLE' , # TextMate Bundle File
	'MLTBX' , # MATLAB Toolbox Package
	'BBLM' , # BBEdit Language Module
	'8BL' , # Photoshop Plug-in
	'FLT' , # Graphics Filter
	'PLG' , # Sibelius Plug-in
	'MODE' , # Coda Syntax Mode File
	'PLX64' , # 64-bit Linux IDA Plugin Module
	'QTR' , # QuickTime Extension Resource
	'OSAX' , # AppleScript Scripting Addition
	'BZPLUG' , # AfterShot Pro Plugin Bundle
	'DFP' , # Fusion Plugin File
	'SPD' , # SPSS Custom Dialog File
	'INDESIGNPLUGIN' , # Adobe InDesign Plug-in
	'XLA' , # Microsoft Excel Add-In File
	'PPMOD' , # Papers Please Mod File
	'T3X' , # TYPO3 Compressed Extension
	'FS' , # Mac OS X File System Plug-in
	'XBA' , # OpenOffice Basic Module File
	'ABA' , # Avant Browser Add-on File
	'QAR' , # QlikView Extension
	'FPI' , # Foxit Reader Plugin
	'Q2Q' , # Flexify Photoshop Plug-in
	'CODAPLUGIN' , # Coda Plug-in
	'PSET' , # Adobe InDesign Plug-in Set File
	'WBM' , # Webmin Module
	'SUGAR' , # Espresso Plug-in
	'ADDON' , # CFS Console Add-on
	'P' , # LightWave Plug-in
	'CCX' , # Click & Create Extension
	'ZLB' , # Form•Z Symbol Library File
	'Q8R' , # Flood Photoshop Plug-in
	'M2P' , # Maxthon 2 Browser Plugin File
	'MOX' , # Montax Imposer Template Document
	'LRMODULE' , # Adobe Lightroom Module
	'PLX' , # 32-bit Linux IDA Plugin Module
	'CST' , # Gary Gadget Plug-in
	'XCODEPLUGIN' , # Xcode Plug-in
	'BRO' , # Bryce Plug-in
	'IDEPLUGIN' , # Xcode IDE Plug-in
	'QLGENERATOR' , # Quick Look Generator File
	'CVT' , # Canvas External Tool File
	'Q9R' , # Glitterato Photoshop Plug-In
	'SO-ABI' , # Cocoa AbiWord Plug-in
	'QPX' , # QuickTime Player Extension
	'DVTPLUGIN' , # Xcode DVT Plug-in
	'ABI' , # AbiWord Plug-in
	'LUAC' , # Compiled Lua Script
	'AFZPLUG' , # AfterShot Pro Plugin Archive
	'XNT' , # QuarkXPress Extension File
	'XADD' , # Expression Web Add-in
	'SA9' , # Hue and Cry Photoshop Plug-in
	'COLORPICKER' , # Color Picker Plugin
	'TBP' , # Autodesk Topobase File
	'PBA' , # OpenOffice.org Basic Protected Module File
	'Q5R' , # Melancholytron Photoshop Plug-in
	'BRI' , # Bryce Plug-in File
	'LRWEBENGINE' , # Adobe Lightroom Web Gallery File
	'ASEPRITE-DATA' , # Aseprite Theme Auxillary File
	'IADACTION' , # iAd Producer Plugin Action Template
	'IADPLUG' , # iAd Producer Plugin
	'DMKIT' , # Drumaxx Drum Kit
	'ATX' , # Animation Master Plugin File
	'XCPLUGIN' , # Xcode Plug-in
	'WOWSL' , # WOW Slider
	'SYN' , # TextPad Syntax File
	'RPI' , # Render Plug-in
	'ARPACK' , # ArtRage Package
	'REV' , # LiveCode Plugin
	'COX' , # Click & Create Extension
	'FZP' , # Form•Z Plug-in
	'MPE1' , # MediaPortal 1 Extension
	'MAT' , # V-Ray Materials File
	'MFW' , # MobileFrame Workflow Pack File
	'WIE' , # WordPress Widget Export File
	'EXV' , # Adobe Extension Script
	'Q7Q' , # India Ink Photoshop Plug-in
	'Q4Q' , # Solar Cell Photoshop Plug-in
)

# noinspection SpellCheckingInspection
font_set = (
	'VFB' , # FontLab Studio Font File
	'PFA' , # Printer Font ASCII File
	'FNT' , # Windows Font File
	'SFD' , # Spline Font Database
	'VLW' , # Processing Font File
	'JFPROJ' , # JSON Font Project File
	'WOFF' , # Web Open Font Format File
	'PFB' , # Printer Font Binary File
	'OTF' , # OpenType Font
	'BDF' , # Glyph Bitmap Distribution Format
	'GLIF' , # Glyph Interchange Format File
	'FOT' , # Font Resource File
	'WOFF2' , # Web Open Font Format 2.0 File
	'TTF' , # TrueType Font
	'ODTTF' , # Obfuscated OpenType Font
	'FON' , # Windows Font Library
	'CHR' , # Borland Character Set
	'FNT' , # Bitmap Font Generator Font Descriptor
	'PMT' , # PageMaker Template File
	'TTC' , # TrueType Font Collection
	'AMFM' , # Adobe Multiple Font Metrics File
	'BMFC' , # Bitmap Font Configuration
	'MF' , # METAFONT File
	'PF2' , # GRUB Font File
	'COMPOSITEFONT' , # Windows Composite Font File
	'ETX' , # TeX Font Encoding File
	'GXF' , # General CADD Pro Font File
	'PFM' , # Printer Font Metrics File
	'ABF' , # Adobe Binary Screen Font File
	'PCF' , # PaintCAD Font
	'SFP' , # Soft Font Printer File
	'DFONT' , # Mac OS X Data Fork Font
	'GF' , # METAFONT Bitmap File
	'MXF' , # Maxis Font File
	'UFO' , # Unified Font Object File
	'TTE' , # Private Character Editor File
	'VNF' , # Vision Numeric Font
	'BF' , # Birdfont Project File
	'XFN' , # Ventura Printer Font
	'PFR' , # Portable Font Resource File
	'TFM' , # TeX Font Metric File
	'GDR' , # Symbian OS Font File
	'AFM' , # Adobe Font Metrics File
	'XFT' , # ChiWriter Printer Font
	'EOT' , # Embedded OpenType Font
	'TXF' , # Celestia Font Texture File
	'ACFM' , # Adobe Composite Font Metrics File
	'PK' , # Packed METAFONT File
	'SUIT' , # Macintosh Font Suitcase
	'FFIL' , # Mac Font Suitcase
	'NFTR' , # Nintendo DS Font Type File
	'T65' , # PageMaker Template File
	'EUF' , # Private Character Editor File
	'CHA' , # Character Layout File
	'YTF' , # Google Picasa Font Cache
	'MCF' , # Watchtower Library Font File
	'LWFN' , # Adobe Type 1 Mac Font File
	'F3F' , # Crazy Machines Font File
	'FEA' , # AFDKO Feature Definitions File
	'PFT' , # ChiWriter Printer Font
	'SFT' , # ChiWriter Screen Font
)

# noinspection SpellCheckingInspection
system_set = (
	'RMT' , # Router Firmware File
	'ADMX' , # Group Policy Administrative Template File
	'VGA' , # VGA Display Driver
	'HVE' , # Windows Registry Hive File
	'AML' , # ACPI Machine Language File
	'REG' , # Registry File
	'SHSH2' , # iOS SHSH Blob
	'CUR' , # Windows Cursor Image
	'REGTRANS-MS' , # Registry Transaction Log File
	'CLB' , # COM+ Catalog File
	'ANI' , # Windows Animated Cursor
	'WDGT' , # Dashboard Widget
	'EDJ' , # Enlightenment Theme File
	'NT' , # Windows NT Startup File
	'ICNS' , # macOS Icon Resource
	'DLL' , # Dynamic Link Library
	'DESKTHEMEPACK' , # Windows 8 Desktop Theme Pack File
	'ICONPACKAGE' , # IconPackager Theme File
	'PK2' , # Silkroad Online Game Data File
	'SERVICE' , # Systemd Service Unit File
	'DMP' , # Windows Memory Dump
	'DVD' , # DOS Device Driver
	'FTF' , # FlashTool Firmware File
	'IOPLIST' , # Mac OS X IOPLIST File
	'THEME' , # Microsoft Plus! Desktop Theme
	'VX_' , # Compressed Virtual Device Driver File
	'HIV' , # Windows Registry Hive File
	'MOD' , # GRUB Module
	'LNK' , # Windows Shortcut
	'MDMP' , # Windows Minidump
	'ELF' , # Executable and Linkable Format File
	'SEARCHCONNECTOR-MS' , # Windows Search Connector File
	'EBD' , # Windows EBD System File
	'LOCKFILE' , # Mac OS X PLIST Lock File
	'HTT' , # Hypertext Template
	'QVM' , # Q Virtual Machine
	'SYS' , # Windows System File
	'PCK' , # System Center Configuration Manager Package File
	'SFCACHE' , # ReadyBoost Cache File
	'ICO' , # Icon File
	'MANIFEST' , # Windows Application Manifest File
	'FIRM' , # Boot9Strap 3DS Firmware File
	'BASH_HISTORY' , # Bash History File
	'NFO' , # System Information File
	'ASEC' , # Android Secure Application File
	'DESKLINK' , # Desktop Shortcut
	'ION' , # File Description File
	'PROP' , # Android Build Properties File
	'MUI' , # Multilingual User Interface File
	'ETL' , # Microsoft Event Trace Log File
	'BASHRC' , # Bash Non-Interactive Login Shell File
	'CGZ' , # Linux Drivers Archive
	'TIMER' , # Systemd Unit Configuration File
	'MENU' , # macOS Menu Extra
	'RC1' , # Samsung Mobile Device Firmware File
	'THA' , # Windows Thai Noise-Words List File
	'H1S' , # Windows Assistance Platform Help File
	'PWL' , # Windows Password List
	'ZONE.IDENTIFIER' , # Windows Zone Identifier File
	'PREFPANE' , # Mac OS X System Preference Pane
	'WLU' , # Windows Legacy Update
	'LM' , # Language Model File
	'MAPIMAIL' , # Send To Mail Recipient
	'CM0013' , # Samsung Smart TV Index File
	'MSC' , # Microsoft Management Console Snap-in Control File
	'DAT' , # Windows Registry Hive File
	'BIN' , # Router Firmware File
	'000' , # Indexing Service Data File
	'LOG1' , # Windows Registry Hive Log File
	'EFI' , # Extensible Firmware Interface File
	'SCR' , # Windows Screensaver
	'FX' , # ReadyBoot Trace File
	'ADM' , # Administrative Template File
	'INF_LOC' , # Windows Driver Information Cache File
	'LFS' , # Samsung Phone params.lfs File
	'MUI_CCCD5AE0' , # Hid User Library
	'MLC' , # Microsoft Language Interface Pack
	'SDB' , # Application Compatibility Database
	'C32' , # Syslinux COM32 Module
	'CAB' , # Windows Cabinet File
	'ITEMDATA-MS' , # Windows 8 Start Screen Item Data File
	'FOTA' , # Firmware Over-the-Air File
	'MSSTYLES' , # Windows XP Style
	'DRPM' , # Delta RPM File
	'MOBILECONFIG' , # Apple Mobile Configuration File
	'PIT' , # Samsung Phone Firmware File
	'3FS' , # Puppy Linux Save State File
	'SYS' , # Motorola Cell Phone Driver
	'PNF' , # Precompiled INF File
	'TRX_DLL' , # Windows Startup File
	'MTZ' , # MIUI Theme
	'73U' , # TI-73 Explorer Operating System File
	'BASH_PROFILE' , # Bash Interactive Login Shell File
	'NLS' , # Microsoft National Language Support File
	'LIBRARY-MS' , # Windows Library Description File
	'SBN' , # Cisco IP Phone Firmware File
	'WEBPNP' , # Web Point and Print File
	'HHK' , # HTML Help Index
	'208' , # ASUS Notebook BIOS 208 Update File
	'BCD' , # Windows Boot Configuration Data File
	'0' , # Shared Library Link File
	'DIT' , # Active Directory Information Tree File
	'MBR' , # Master Boot Record File
	'DTHUMB' , # Android Data Thumbnail
	'INS' , # Internet Naming Service File
	'DIAGCAB' , # Troubleshooting Pack Cabinet File
	'PLASMOID' , # Plasma Widget
	'IMG3' , # iPhone IMG3 File
	'NLT' , # Windows Transliteration File
	'DRV' , # Device Driver
	'PS2' , # Microsoft Search Catalog Index File
	'PDR' , # Windows Port Driver
	'HDMP' , # Windows Heap Dump
	'CI' , # Windows Content Indexer Catalog File
	'0' , # Android CA Digital Certificate
	'POL' , # Windows Policy File
	'SDT' , # Siemens Desktop Theme File
	'PPD' , # PostScript Printer Description File
	'NB0' , # Device Boot Loader Image
	'CAT' , # Windows Catalog File
	'CPL' , # Windows Control Panel Item
	'SBF' , # Android System File
	'BIO' , # BIOS File
	'CANNEDSEARCH' , # Apple Predefined Search
	'SAVEDSEARCH' , # Spotlight Saved Search
	'WDF' , # Windows Driver Foundation File
	'1' , # Unix Section 1 Manual Page
	'VXD' , # Virtual Device Driver
	'MSP' , # Windows Installer Patch
	'SHD' , # Windows Print Job Shadow File
	'CM0012' , # Configuration Management File
	'HLP' , # Windows Help File
	'JOB' , # Windows Task Scheduler Job File
	'LOG2' , # Windows Registry Hive Log 2 File
	'PAT' , # DiskStation Manager Installation File
	'SQM' , # Service Quality Monitoring File
	'AX' , # DirectShow Filter
	'8CU' , # TI-84 Plus C Operating System File
	'WPX' , # Printer Description File
	'CNT' , # Help Contents File
	'MBN' , # Multi Boot Image
	'CRASH' , # Mac OS X Crash Log File
	'KEXT' , # Mac OS X Kernel Extension
	'FPBF' , # Mac OS X Burn Folder
	'2FS' , # Puppy Linux Save State File
	'IDX' , # Index File
	'8XU' , # TI-83 and TI-84 Plus Operating System File
	'PRF' , # Windows System File
	'IME' , # Windows Input Method Editor File
	'CONFIGPROFILE' , # Apple Configuration Profile
	'ODEX' , # Optimized Dalvik Executable
	'ADML' , # Group Policy Language-Specific Administrative Template
	'LST' , # GRUB Boot List File
	'DATABASE_UUID' , # Android Stale Thumbnails Identifier
	'SHSH' , # iPhone/iPod Touch SHSH Blob File
	'EMERALD' , # Emerald Theme
	'SCHEMAS' , # GConf Schema Definition File
	'DFU' , # iOS DFU File
	'TDZ' , # Drobo Firmware File
	'PANIC' , # Kernel Panic File
	'DUB' , # Windows Spelling Dictionary Identifier
	'CPQ' , # Compaq OEM Disc Configuration File
	'SWP' , # Swap File
	'FFX' , # Microsoft Find Fast Index
	'NTFS' , # NTFS Partition File
	'89U' , # TI-89 Titanium Operating System File
	'JOURNAL' , # Linux Journald Event Log
	'UTV' , # SmartCast TV Firmware Update
	'B84' , # TI-84 Plus CE Bundle Image
	'LEX' , # Spelling Dictionary File
	'CHG' , # Windows Net Logon File
	'386' , # Windows Virtual Device Driver
	'BLF' , # CLFS Base Log File
	'WER' , # Windows Error Report
	'AOS' , # ARCHOS PCtablet Firmware File
	'KO' , # Linux Kernel Module File
	'HCD' , # Samsung Android Firmware File
	'SAVER' , # Mac OS X Screen Saver
	'3' , # Unix Section 3 Manual Page
	'MUM' , # Windows Vista Update Package
	'PROFILE' , # Bash Shell Profile
	'HHC' , # HTML Help Table of Contents
	'ADV' , # Audio Driver File
	'PROVISIONPROFILE' , # Apple Provision Profile
	'CHK' , # Netgear Router Firmware File
	'PRINTEREXPORT' , # Windows Printer Migration File
	'STR' , # Windows Screensaver File
	'GROUP' , # Windows Contacts Group File
	'DIMAX' , # DiMAX Firmware Image File
	'KDZ' , # LG Mobile Device Firmware
	'METADATA_NEVER_INDEX' , # Spotlight Never Index File
	'IM4P' , # IMG4 Payload File
	'DEVICEMETADATA-MS' , # Device Metadata Package
	'KWI' , # Toyota Entune Update File
	'CAP' , # ASUS BIOS Update File
	'BMK' , # Windows Help Bookmarks
	'PID' , # Creative Driver File
	'RUF' , # Samsung DVD and Blu-ray Player Firmware File
	'ATAHD' , # ATA Hard Drive Flag File
	'NBH' , # ROM Update Utility Image File
	'GRP' , # Windows Program Manager Group
	'MI4' , # Audio Player Firmware File
	'FFO' , # Find Fast Document Properties Cache
	'PRT' , # Printer Driver File
	'CPI' , # Codepage Information File
	'SPL' , # Windows Print Spool File
	'VDEX' , # Validated Dalvik Executable File
	'DEV' , # Windows Device Driver File
	'KBD' , # Keyboard Layout Script
	'EVTX' , # Windows 7 Event Log File
	'MYDOCS' , # Send To My Documents
	'ICL' , # Windows Icon Library File
	'FLG' , # Puppy Linux Partition Flag File
	'KS' , # Kickstart File
	'THUMBNAILS' , # Android Thumbnails Folder
	'XRM-MS' , # Microsoft Security Certificate
	'CUSTOMDESTINATIONS-MS' , # Windows Jump List File
	'CPR' , # Windows Phone Display Settings File
	'TRASHINFO' , # Linux Trash Metadata File
	'EFIRES' , # EFI Image File
	'HPJ' , # Help Project File
	'ESCOPY' , # ES File Explorer File Manager Temporary File
	'MUN' , # Windows System Resource Archive
	'RCV' , # BIOS Recovery File
	'OZIP' , # OPPO Smartphone Firmware Update
	'SEFW' , # Secure Element Firmware
	'FTR' , # IRIX File Type Rules File
	'CHT' , # Windows Chinese Traditional Noise-Word List File
	'CHS' , # Windows Chinese Noise-Word List File
	'IMG2' , # iPhone Firmware Image
	'2' , # Unix Section 2 Manual Page
	'RFW' , # Rockchip Firmware File
	'4' , # Unix Section 4 Manual Page
	'EVT' , # Windows Event Viewer Log File
	'BK2' , # Windows Indexing Service Data File
	'DLX' , # Renamed DLL File
	'CMO' , # ASUS BIOS File
	'SIN' , # Sony Xperia Firmware File
	'TBRES' , # Microsoft Store TokenBroker Cache File
	'SPRX' , # Signed PlayStation Relocatable Executable
	'SCF' , # Shell Command File
	'DIAGPKG' , # Troubleshooting Pack File
	'IFW' , # INTUS Firmware File
	'UCE' , # Windows UCE System File
	'GMMP' , # WCS Gamut Map Model Profile
	'MEM' , # Parallels Desktop Memory Dump File
	'NETWORKCONNECT' , # Apple Network Connect Document
	'BOM' , # Mac OS X Bill of Materials File
	'XFB' , # Xserve RAID Firmware Binary File
	'WGZ' , # S60 Web Runtime Widget Package
	'TRASH' , # ChromeOS Trash Storage
	'KC' , # macOS Kernel Extension Boot File
	'MMV' , # Magnex Firmware File
	'ISDATA' , # Icon Services Data
	'TRASHES' , # macOS USB Flash Drive Trash Folder
	'SO.0' , # Unix Shared Library File
	'PS1' , # Microsoft Indexing Service Data File
	'SCAP' , # EFI Firmware File
	'FTS' , # Windows Help Full Text Search Index
	'GRL' , # Windows Update Status File
	'DSS' , # DCC Active Designer File
	'PRO' , # TurboIntegrator Process Definition File
	'KEY' , # Android Passcode File
	'DOCK' , # Mac OS X Dock Extra
	'ME' , # Puppy Linux Partition Flag File
	'FFA' , # Find Fast Status File
	'FL1' , # Lenovo BIOS Update File
	'LPD' , # LPD Daemon Print Permissions File
	'MSSTYLE' , # Windows XP Unsigned Style
	'BK1' , # Windows Indexing Service File
	'IPOD' , # Rockbox Firmware File
	'AUTOMATICDESTINATIONS-MS' , # Windows Jump List File
	'IUS' , # InfoTMIC Firmware Update File
	'RC2' , # Samsung Mobile Device Firmware File
	'IDI' , # EVA Controller Event Log
	'DIC' , # Windows 10 Custom Spelling Dictionary
	'JETKEY' , # iOS Support Jet File
	'SB' , # Slax Bundle
	'VGD' , # Generic CADD VGA Driver
	'RS' , # Windows Rating System File
	'PPM' , # Post Programmable Memory File
	'5' , # Unix Section 5 Manual Page
	'DIAGCFG' , # Troubleshooting Pack Configuration File
	'RCO' , # PlayStation Portable Resource File
	'PUSH_DEVICEID' , # Android Device Identification File
	'QKY' , # Quirky Kernel
	'HSH' , # Windows Catalog Search Index File
	'7' , # Unix Section 7 Manual Page
	'6' , # Unix Section 6 Manual Page
	'ROKU' , # Roku TV Firmware Update
	'KL' , # Android Key Layout File
	'LPD' , # Lookout Protocol Driver File
	'WPH' , # Phoenix BIOS File
	'IPTHEME' , # IconPackager Raw Theme File
	'8' , # Unix Section 8 Manual Page
	'DYC' , # Xerox Printer Driver Configuration File
	'BUD' , # Binary Printer Description File
	'LOCALIZED' , # macOS Directory Localization File
	'JPN' , # Windows Japanese Noise-Word List File
	'TA' , # Sony Firmware File
	'FID' , # Microsoft Catalog Indexing Service File
	'SELF' , # Signed Executable and Linkable Format
	'ANN' , # Windows Help Annotations
	'INTERNETCONNECT' , # Apple Internet Connect Document
	'SPX' , # System Profiler XML Report
	'ITS' , # Internet Document Set
	'FFL' , # Find Fast Document List
	'TNO' , # TI-Nspire Operating System File
	'MRC' , # MIUI Theme Content Package
	'MRM' , # MIUI Theme Metadata
	'TCO2' , # TI-Nspire CX II Operating System File
	'B83' , # TI-83 Premium CE Bundle Image
	'XGF' , # Xiegu G Series Firmware Update
	'FTG' , # Full Text Group
	'BBFW' , # Apple Baseband Firmware Update
	'KOR' , # Windows Korean Noise-Words List File
	'REGLNK' , # Registry Editor Shortcut File
	'HELP' , # Mac OS X Help Book
	'CDMP' , # WCS Device Model Profile
	'DIFFBASE' , # Windows Diagnostics Tracking Diffbase File
	'RVP' , # Sharp Blu-ray Player Firmware File
	'IM4M' , # IMG4 Manifest
)

# noinspection SpellCheckingInspection
settings_set = (
	'GXT' , # Grand Theft Auto Text File
	'STB' , # AutoCAD Plot Style Table File
	'STYLE' , # SketchUp Style
	'GODOT' , # Godot Engine Project
	'MYCOLORS' , # Stardock MyColors Theme File
	'DCL' , # AutoCAD Dialog Definition File
	'VSTPRESET' , # VST3 Preset File
	'KPL' , # Krita Color Palette
	'SKZ' , # SuperKaramba Theme
	'VITAL' , # Vital Synthesizer Preset
	'EQP' , # MathType Preferences
	'FLW' , # Fusion Flow File
	'COPRESET' , # Capture One Preset File
	'OFFICEUI' , # Microsoft Office UI Customization File
	'SET' , # Settings File
	'ALX' , # BlackBerry Application Loader XML File
	'AUX' , # LaTeX Auxiliary File
	'DSD' , # AutoCAD Drawing Set Description File
	'SIF' , # Windows Setup Information File
	'ISS' , # Inno Setup Script
	'CNF' , # MySQL Configuration File
	'NSX' , # AppStudio Project
	'OSS' , # Microsoft Outlook Saved Search File
	'HD3D' , # Home Design 3D Project
	'IRS' , # Adobe Save For Web Settings
	'VQC' , # Virtual CD Quick Copy File
	'SETTINGS' , # Visual Studio Settings File
	'THEMEPACK' , # Windows 7 Theme Pack
	'CFG' , # Celestia Configuration File
	'SBV' , # YouTube Captions File
	'BS7' , # Windows 7 Boot Updater Skin
	'TSI' , # Traktor Settings File
	'PKG' , # Symbian Package File
	'TSCPROJ' , # TechSmith Camtasia Project
	'ISP' , # IIS Internet Service Provider Settings
	'NP4' , # NetPoint 4 Schedule File
	'RMSKIN' , # Rainmeter Skin File
	'SED' , # IExpress Self Extraction Directive File
	'RPROJ' , # R Project
	'XUI' , # Xbox 360 User Interface File
	'FXP' , # FX Preset
	'CFG' , # Citrix Server Connection File
	'PRFPSET' , # Premiere Pro Filter Preset File
	'DOLPHINVIEW' , # Dolphin Folder View Settings File
	'LH3D' , # Live Home 3D Project
	'FVP' , # File Viewer Plus Batch Presets File
	'CFG' , # Configuration File
	'PRM' , # Parameter File
	'XTP' , # AutoCAD Exported Tool Palettes File
	'TSK' , # Pocket PC Skin
	'SKIN' , # ASP.NET Skin File
	'LRTEMPLATE' , # Adobe Lightroom Template
	'UDCX' , # Universal Data Connection File
	'L4D' , # Cinema 4D Layout
	'A2THEME' , # Aston 2 Theme File
	'CEX' , # SolidWorks Enterprise PDM Vault Export File
	'TPF' , # SPSS Text Wizard Document
	'VSSETTINGS' , # Visual Studio Settings File
	'OVPN' , # OpenVPN Configuration File
	'NRI' , # Nero ISO CD Compilation File
	'QSS' , # Qt Style Sheet
	'DINFO' , # DivX Temporary Video Info File
	'RDP' , # Remote Desktop Configuration File
	'CUI' , # Autodesk Custom Workspace File
	'MST' , # Windows Installer Setup Transform File
	'ASL' , # Photoshop Style
	'ACO' , # Adobe Photoshop Color Swatch File
	'MMRC' , # MADRIX MIDI Remote Configuration
	'CUIX' , # AutoCAD Custom User Interface File
	'MDS' , # TestComplete Project File
	'IIP' , # Install Creator Pro Project File
	'COSTYLE' , # Capture One Style File
	'ONETOC2' , # Microsoft OneNote Table of Contents
	'CHX' , # AutoCAD Standards Check File
	'ATC' , # AutoCAD Tool Catalog File
	'DOK' , # DesktopOK Icons Layout File
	'S2ML' , # StarCraft 2 Map Localization File
	'ICM' , # Image Color Matching Profile
	'MSN' , # Host Blocking File
	'ASEF' , # Adobe Swatch Exchange File
	'LVA' , # Logitech Video Effects Avatar File
	'TRX' , # PASSOLO Translation List File
	'M2S' , # Maxthon 2 Browser Skin File
	'VCPREF' , # Norton Antivirus Preferences File
	'RLL' , # Microsoft Resource Library
	'IHW' , # IN-HEH Timeline Workspace
	'PROPDESC' , # Property Description
	'BTSEARCH' , # BitTorrent Search Engine File
	'VTPR' , # Studio Store Visualizer Project
	'MNU' , # AutoCAD Interface Layout File
	'MOTR' , # Apple Motion Transition Project Template
	'BCP' , # BlackMagic Custom Palette
	'WME' , # Windows Media Encoder Session File
	'ATZ' , # Aston Compiled Theme
	'AIP' , # Advanced Installer Project
	'BLOB' , # Valve Steam Archive
	'OTPU' , # Origin Unicode Graph Template
	'ADPP' , # Adobe Device Profile Package
	'VITALBANK' , # Vital Synthesizer Soundbank
	'CNF' , # Telnet Configuration File
	'QTP' , # QuickTime Preferences
	'ICST' , # InCopy Document Preset File
	'FETCHMIRROR' , # Fetch Mirror Document
	'BOOT' , # InstallShield Boot File
	'QRC' , # Qt Resource Collection File
	'ZON' , # OmniPage Zone Template File
	'AIU' , # Advanced Installer Updates Configuration File
	'PR' , # Source Insight Project
	'GTP' , # GNOME Theme Package File
	'PTB' , # PowerToys Settings Backup
	'QVPP' , # QlikView Extension Properties Page File
	'C2R' , # Windows Media Center Click-To-Record File
	'QBTHEME' , # qBittorrent Theme
	'TLO' , # SPSS TableLooks File
	'LRSMCOL' , # Adobe Lightroom Smart Collection Settings File
	'CHROMAEFFECTS' , # Synapse 3 Chroma Configuration
	'OBT' , # Openbox Theme File
	'DBG' , # Visual FoxPro Debugger Configuration File
	'NVP' , # NVivo for Windows Project
	'UIS' , # WindowBlinds User Interface Specification
	'DCST' , # Adobe InDesign Document Presets File
	'3DL' , # 3D Lookup Table File
	'CFG' , # LightWave Configuration File
	'PROPERTIES' , # Java Properties File
	'RPS' , # 3ds Max Render Preset Settings
	'GID' , # Windows Help Global Index File
	'MST' , # Corel Presentations Master Template File
	'VIM' , # Vim Settings File
	'FFX' , # After Effects Preset File
	'XTREME' , # Winstep Xtreme Theme Pack
	'CONF' , # Generic Configuration File
	'XPADDERCONTROLLER' , # Xpadder Controller Profile
	'OBI' , # Outlook RSS Subscription File
	'CONF' , # Unix Configuration File
	'APPLICATION' , # ClickOnce Deployment Manifest File
	'EHI' , # HTTP Injector Config File
	'SOL' , # Flash Local Shared Object File
	'PC3' , # AutoCAD Plotter Configuration File
	'SFO' , # CuteFTP Search File
	'LOOK' , # SpeedGrade Look File
	'MCL' , # Windows Media Center Link File
	'INF' , # Setup Information File
	'ATN' , # Photoshop Actions File
	'PIP' , # Office Personalized Settings File
	'ISS' , # InstallShield Silent Response File
	'XDR' , # XML-Data Reduced File
	'RCF' , # SonicWALL VPN Configuration File
	'PMP' , # AutoCAD Plot Model Parameter File
	'DOWNLOADHOST' , # MSN Download Settings File
	'OSDX' , # Search Connector Description File
	'WAL' , # Winamp Modern Skin
	'FTP' , # FlashFXP XML File
	'NPV4' , # NapsternetV Configuration File
	'BIN' , # BlackBerry IT Policy File
	'ZAP' , # Zero Administration Package File
	'KSF' , # KMPlayer Skin File
	'QXW' , # Q Light Controller+ Workspace File
	'PSC1' , # Windows PowerShell Console File
	'XMS' , # Music Studio Project
	'ACV' , # Photoshop Adjustment Curve
	'ODC' , # Office Data Connection File
	'WFC' , # Windows Wireless Network Settings File 
	'CHL' , # WinFast PVR2 Channel List
	'PROFILE' , # Citrix Application Profile
	'BGI' , # BgInfo Configuration File
	'ACROBATSECURITYSETTINGS' , # Adobe Acrobat Security Settings File
	'INI' , # Windows Initialization File
	'DS_STORE' , # Mac OS X Folder Settings File
	'APPREF-MS' , # Microsoft Application Reference File
	'NKP' , # Kontakt Presets File
	'TMTHEME' , # TextMate Theme File
	'FT' , # Edgecam Feature Template
	'SCPRESETS' , # Snap Converter Presets File
	'MOEF' , # Apple Motion Effect Project Template
	'CLG' , # Windows Catalog File
	'DSX' , # DAZ Studio XML File
	'CPX' , # Oracle ADF Binding Context File
	'DICPROOF' , # Microsoft Dictionary Proofing File
	'UCT' , # UC Browser Theme File
	'GPS' , # GenePix Settings File
	'DIRECTORY' , # KDE Folder View Properties File
	'PTF' , # PSP Theme File
	'EMM' , # MindMaple Map
	'MSM' , # Windows Installer Merge Module
	'KYS' , # Adobe Photoshop Keyboard Shortcuts File
	'WCZ' , # Chameleon Clock Wallpaper File
	'OEM' , # OEM Setup File
	'CTB' , # AutoCAD Color-Based Plot Style File
	'ICD' , # Installable Client Driver File
	'PLIST' , # Property List
	'FD' , # LaTeX Font Definition File
	'VBOX' , # Oracle VM VirtualBox Settings File
	'CVA' , # HP System Software Manager Information File
	'PSF' , # Photoshop Proof Settings File
	'TPARK' , # ThemePark Project File
	'WMS' , # Windows Media Skin
	'IDPP' , # Adobe InDesign Preflight Profile
	'RDF' , # Resource Description Framework File
	'THE' , # Microsoft Plus! Theme File
	'VMXF' , # VMware Team Member File
	'GPL' , # GIMP Palette
	'IPCC' , # iPhone Carrier Bundle 
	'DUCK' , # Cyberduck Bookmark
	'AVS' , # Avid Project Preferences File
	'RDW' , # Real-DRAW Project File
	'WFP' , # Wondershare Filmora Project File
	'OPT' , # Options File
	'VMC' , # Windows Virtual Machine Configuration File
	'INS' , # Internet Settings File
	'PS1XML' , # Windows PowerShell Display Configuration File
	'OSP' , # OpenShot Video Editor Project File
	'NPS' , # Natron Node Presets File
	'P2M' , # PhotoWorks Appearance File
	'PJS' , # TestComplete Project Suite File
	'BCMX' , # Business Contact Manager Customization
	'BLW' , # Adobe Photoshop Black and White Presets File
	'EQF' , # Winamp Equalizer Preset File
	'OPS' , # Office Profile Settings File
	'SLT' , # Mozilla User Profile Folder
	'FC' , # FirstClass Settings File
	'EFTX' , # Office 2007 Theme Effect File
	'WMZ' , # Windows Media Player Skin Package
	'FCC' , # Forms Credential Collector File
	'VNC' , # VNC Configuration File
	'PRF' , # Outlook Profile File
	'BKS' , # NTBackup Settings File
	'ACT' , # Adobe Color Table File
	'ACB' , # Adobe Photoshop Color Book File
	'SETTINGCONTENT-MS' , # Windows Settings File
	'ENS' , # EndNote Style File
	'KBD' , # 3ds Max Keyboard Shortcuts File
	'MOF' , # Managed Object Format File
	'CMP' , # Windows Connection Manager Profile
	'A2M' , # TS-AudioToMIDI Settings File
	'WVE' , # Wondershare Filmora Project File
	'DR5' , # Dramatica Story Expert Project
	'TSZ' , # Trillian Skin
	'CSKIN' , # CD Art Display Skin File
	'VIMRC' , # Vim Runtime Configuration File
	'EXPORTEDUI' , # Microsoft Office Exported UI Customization File
	'GMW' , # Global Mapper Workspace File
	'DXP' , # Dexpot Profile File
	'RULESET' , # Visual Studio Code Analysis Rule Set File
	'ZPI' , # PDF Converter Index File
	'MOTI' , # Apple Motion Title Project Template
	'PROPS' , # Visual Studio Project Property File
	'VUE' , # FoxPro 2.x View Settings
	'CPG' , # ESRI Code Page File
	'VPH' , # VirtualPhotographer Custom Settings
	'SZ' , # Winamp Classic Skin Download
	'VPS' , # Virtual CD Copy Template
	'BCS' , # Batch Compiler Specification File
	'ICURSORFX' , # iCursor Effect File
	'PREF' , # Preferences File
	'EPP' , # EditPad Pro Project
	'ENZ' , # EndNote Connection File
	'ALV' , # Adobe Photoshop Levels File
	'JSON.CRYPT14' , # WhatsApp Backup Settings
	'WCX' , # RemoteApp and Desktop Connections Configuration File
	'MXS' , # Painter Color Mixer Pad
	'CSPLAN' , # SPSS Sampling Plan File
	'MLK' , # MasterCook Look File
	'FAT' , # Zinf Theme File
	'CSF' , # Adobe Color Settings File
	'TDF' , # Title Definition Format File
	'VLT' , # VLC Media Player Skin File
	'ICA' , # Citrix ICA File
	'FMT' , # FoxPro Format File
	'POLICY' , # Java Policy Implementation File
	'PAL' , # Painter Custom Palettes File
	'NGRR' , # Guitar Rig Preset
	'SSL2' , # Sunlight ScanLibrary 2 File
	'BAU' , # Apache OpenOffice AutoText File
	'VSPROPS' , # Visual Studio Project Property File
	'MGK' , # ImageMagick Configuration File
	'SYNW-PROJ' , # SynWrite Project
	'SW2' , # Softwrap License File
	'PGP' , # AutoCAD Program Parameters File
	'PRX' , # Windows Media Profile File
	'LVF' , # Logitech Video Effects File
	'ICC' , # ICC Profile
	'PIE' , # GlovePIE Controller Script
	'CURSOR' , # KDE Plasma Custom Cursor Specification
	'GTKRC' , # GTK+ Theme File
	'ARS' , # After Effects Render Settings File
	'UTZ' , # UIQ Theme Package
	'STE' , # Dreamweaver Site Settings File
	'FLST' , # Adobe InDesign Flattener Presets File
	'XST' , # WebSphere Query Template
	'GWS' , # GeoMedia GeoWorkspace File
	'EWPRJ' , # Ultiboard Layout Project
	'GRD' , # Photoshop Gradient File
	'SQD' , # Configuration Settings File
	'PMJ' , # Pegasus Mail Configuration File
	'TDESKTOP-THEME' , # Telegram Desktop Theme
	'STARTUPINFO' , # E-Prime 2.0 Startup Info File
	'PRF' , # Director Preferences File
	'DAR' , # DVD Architect Project
	'ACW' , # Windows Accessibility Wizard File
	'INI' , # Symbian OS Configuration File
	'ASP' , # Adobe Color Separation Setup File
	'KEY' , # Keyboard Definition File
	'ASE' , # Adobe Swatch Exchange File
	'RTS' , # Royal TS Remote Connection File
	'SMT' , # Samsung Theme File
	'RPK' , # RadLight Media Player Skin
	'ALL' , # Java RMI Policy File
	'HID' , # Sony Ericsson Remote Configuration
	'MPT' , # Microsoft Project Template
	'MNS' , # AutoCAD Interface Settings File
	'CDT' , # CorelDRAW Template (Legacy)
	'CAMP' , # WCS Color Appearance Model Profile File
	'NET' , # Dynagen Network Topology File
	'WSZ' , # Winamp Classic Skin
	'XPL' , # LcdStudio Playlist File
	'MTF' , # Motorola Theme File
	'XLB' , # Microsoft Excel Toolbar Settings File
	'REG' , # Registration Information File
	'MOTN' , # Apple Motion Generator Project Template
	'DDF' , # Diamond Directive File
	'HT' , # HyperTerminal Session File
	'STORYISTTHEME' , # Storyist Application Theme File
	'SSS' , # WindowBlinds Substyle File
	'MCW' , # Monitor Calibration Wizard File
	'DCP' , # Adobe DNG Camera Profile
	'JDF' , # Adobe Acrobat Job Definition File
	'DSW' , # Visual C++ 6 Workspace File
	'COMP' , # Fusion Composition File
	'PDP' , # Palo Alto Software Plan Component File
	'PMC' , # Performance Monitor Counter File
	'AXT' , # Adobe Photoshop Extract File
	'JWS' , # Java Workspace Settings File
	'SCRIBE' , # VideoScribe Project
	'THEME' , # GTK Theme Index File
	'THMX' , # Office 2007 Theme
	'SL' , # Software License File
	'EXE4J' , # Exe4j Configuration File
	'UPF' , # MicroStation User Preferences File
	'FMP' , # AutoCAD Font Map File
	'NPFX' , # Norton Internet Security Firewall Settings File
	'HME' , # Windows Mobile Theme File
	'DVTCOLORTHEME' , # Xcode Color Theme File
	'GVIMRC' , # GVim Runtime Configuration File
	'FLMPST' , # FL Studio Mobile Plug-in Preset
	'GROWLTICKET' , # Growl Notification File
	'WF4' , # Wordfast Project
	'MATERIAL' , # Rigs of Rods Texture Reference File
	'STT' , # SPSS Table Template
	'EUM' , # Enterprise User Monitor Configuration File
	'CPTM' , # Captivate Theme File
	'NJI' , # Nero Job Information File 
	'FBT' , # ABBYY FineReader Document Options File
	'MAILHOST' , # MSN Mailhost Settings File
	'SKI' , # Motorola Phone Skin File
	'SPP' , # Substance Painter Project
	'FRAMES' , # xScope Frames File
	'MSKN' , # MediaMonkey Skin File
	'SPF' , # Slingplayer Favorites File
	'RWSTYLE' , # RapidWeaver Style File
	'WSB' , # Windows Sandbox Configuration
	'NWV' , # Dragon NaturallySpeaking User Archive
	'TSM' , # TwinCAT System Manager Configuration File
	'IKMP' , # IK Multimedia Preset FIle
	'LCC' , # Capture One Lens Cast Correction File
	'XSOSD' , # ProgDVB Skin
	'ATH' , # Alienware AlienFX Theme File
	'TVTEMPLATE' , # mimoLive Template
	'NDC' , # Personal Communications Settings File
	'EQL' , # MathType Settings
	'PIO' , # Pro Tools I/O Settings File
	'NCFG' , # ArcGIS Explorer Configuration File
	'COS' , # Capture One Settings File
	'OVERLAY' , # Streamlabs Desktop Overlay
	'BOX' , # Painter Custom Toolbox File
	'HWT' , # Huawei EMUI Theme
	'PPKG' , # Windows Provisioning Package
	'AHU' , # Adobe Photoshop HSL File
	'OMS' , # HP Printer Substrate Presets Package File
	'GA' , # Genetic Art Image Parameters
	'WC' , # Valve Hammer Configuration File
	'BITPIM' , # BitPim Configuration File
	'IX' , # dtSearch Index File
	'SXIE' , # ShareX Image Effect File
	'PHB' , # PhtotoBase Album File
	'CDRT' , # CorelDRAW Template
	'KCB' , # Kindle Create Book File
	'WIF' , # Weaving Interchange Format File
	'T3D' , # TicTacTi Advertisement Definition File
	'CMATE' , # ControllerMate File
	'NVC' , # Nikon Vignette Correction File
	'VCOMPS' , # V-Comp Setup File
	'SNAGTHEME' , # Snagit Theme
	'RDO' , # Xerox Rawster Document Object File
	'EV3E' , # LEGO MINDSTORMS EV3 Experiment
	'PCTL' , # Kaspersky Parental Control Settings File
	'VMCX' , # Virtual Machine Shell Information File
	'NKSN' , # Kontakt Snapshot File
	'ONC' , # Open Network Configuration
	'AHL' , # eMule Metadata File
	'TRK' , # Finale Track-to-Staff Mapping File
	'PXB' , # Pixelmator Brush File
	'RAD' , # Citrix Rapid Application Delivery File
	'INI' , # Gravis UltraSound Bank Setup File
	'AVE' , # Avid User File
	'MMP' , # Symbian Project Specification File
	'FTPQUOTA' , # Ftpquota File
	'MPH' , # Photostory Deluxe Project
	'ARG' , # AutoCAD Profile
	'QAT' , # Microsoft Office Quick Access Toolbar File
	'ND' , # QuickBooks Network Data File
	'PRF' , # QuarkXPress Preferences File
	'AHS' , # Adobe Halftone Screen File
	'USERPROFILE' , # Norton AntiVirus User Profile File
	'GIN' , # GEMS Engine Control Unit File
	'ITT' , # IconTweaker Theme File
	'TVC' , # Turbo View & Convert Batch Presets File
	'TERMINAL' , # Terminal Settings File
	'DRM' , # Cubase Drum Map File
	'ZPF' , # Form•Z Preferences File
	'WORK' , # Bibble Work Queue File
	'DTSCONFIG' , # SSIS Package Configuration File
	'TTS' , # ToolBook Translation System File
	'XUR' , # Xbox 360 Binary User Interface File
	'SRS' , # Outlook Send/Receive Settings File
	'ASK' , # Ableton Live Skin
	'SKN' , # Avant Browser Skin File
	'IAF' , # Outlook Internet Account File
	'ASW' , # ACDSee Slideshow Wizard File
	'LOP' , # MasterCook Layout File
	'MXSKIN' , # Maxthon Skin File
	'VBX6SETTINGS' , # VirusBarrier X6 Settings File
	'CTBODYFITTING' , # CrazyTalk Animator Actor Fitting File
	'TWC' , # TTWin Configuration File
	'SGT' , # SPSS Chart Template
	'SPJ' , # SPSS Production Job File
	'SKN' , # Symbian OS Skin File
	'AMS' , # Adobe Monitor Setup File
	'KDS' , # KD Player Skin File
	'PRO5TEMPLATE' , # ProPresenter 5 Template
	'RFQ' , # RoboForm SearchCard File
	'IIT' , # Install Creator Project File
	'AWCAV' , # ActiveWorlds Custom Avatar File
	'AST' , # Adobe Color Separations Table
	'FWT' , # FacetWin Configuration File
	'AOM' , # After Effects Output Module
	'BRG' , # ProjectWise User Settings File
	'XTODVD' , # ConvertXtoDVD Project File
	'STD' , # PROMT Translator Document
	'CYBERDUCKLICENSE' , # Cyberduck Donation Key
	'CPF' , # Cab Provisioning Format File
	'TYPEIT4ME' , # TypeIt4Me Clippings File
	'ACB' , # AutoCAD Color Book File
	'DSF' , # Dramatica Pro Project
	'CPR' , # Adobe Captivate Preferences File
	'SCH' , # Strater Scheme File
	'VMX' , # Cubase Mixer Settings File
	'FTH' , # FileMaker Theme File
	'SNX' , # PISnoop Workspace File
	'FNC' , # Frogans Player Network Certificate
	'RCT' , # Visual Studio Resource Template
	'VMPL' , # VMware Policy File
	'PSP' , # Photoshop Preferences File
	'RESMONCFG' , # Resource Monitor Configuration File
	'QF' , # Nokia Maps Version File
	'SSL' , # Sunlight ScanLibrary File
	'BCP' , # Batch Compiler Preset File
	'MMDC' , # MediaMonkey Device Configuration File
	'GCSX' , # Microsoft Office SmartArt Color File
	'GQSX' , # Microsoft Office SmartArt Styles File
	'DUN' , # Dial Up Network File
	'PXG' , # Pixelmator Gradient File
	'XCU' , # OpenOffice.org Configuration File
	'RNQS' , # Universal Pokemon Randomizer Settings
	'MPDCONF' , # Music Player Daemon File
	'WZCONFIG' , # WinZip Configuration File
	'CSAPLAN' , # SPSS Analysis Plan File
	'SPFX' , # Squeeze Presets File
	'LXSOPT' , # Liquid XML Studio Project Options File
	'XVM' , # VMware Console Configuration File
	'SLBK' , # Smart Launcher Home Screen Backup
	'MOBIRISE' , # Mobirise Project File
	'DEFT' , # Juice Grinder Defaults File
	'ZON' , # Collection Building Custom Zone File
	'PDADJ' , # PhotoDirector Preset File
	'DXLS' , # DashXL Skin Set File
	'LIGHTKEYPROJ' , # Lightkey Project
	'ABS' , # TurboZIP Auto Compress Script
	'MASK' , # SpeedGrade Color Mask File
	'CYBERDUCKPROFILE' , # Cyberduck Connection Profile
	'QVT' , # QlikView Theme File
	'BLT' , # AIM Buddy List
	'PROFIMAIL' , # ProfiMail Settings File
	'GPS' , # GOM Player Skin File
	'EXP' , # SonicWALL Preference File
	'LYT' , # Xcalibur Layout File
	'RASKINPLACE' , # Raskin Place Layout File
	'PML' , # Pyre Properties File
	'CPS' , # Captivate Styles File
	'CLR' , # CryptLoad Router Information File
	'MSW' , # Painter Color Mixer Swatches
)

# noinspection SpellCheckingInspection
encoded_set = (
	'BHX' , # BinHex Encoded File
	'ECD' , # Encrypted Cryptee Document
	'KDE' , # KryptoStorage Container File
	'PURGE' , # Globe Ransomware Encrypted File
	'C9R' , # Cryptomator Encrypted Data
	'RZK' , # Red Zion Key File
	'POOP' , # POOP Ransomware Encrypted File
	'VLT' , # WinVault File Archive
	'PACK' , # Pack200 Packed Jar File
	'SUF' , # Ccrypt Encrypted File
	'FILEBOLT' , # Filebolt Encrypted File
	'GXK' , # Galaxkey Secured File
	'SEF' , # Encryptafile Signature File
	'SDFI' , # Softing Data File
	'CNG' , # CryptoNG Encrypted Archive
	'EIUR' , # EIUR Ransomware Encrypted File
	'BPK' , # Nero SecurDisc Public Key File
	'CPIO' , # Unix CPIO Archive
	'LASTLOGIN' , # Minecraft User Credential File
	'BLOWER' , # Blower Ransomware Encrypted File
	'SNK' , # Strong Name Key File
	'PCV' , # Picocrypt Encrypted File
	'ACID' , # ACID Encrypted File
	'ATSOFTS' , # LetEncrypt Encrypted File
	'AXX' , # AxCrypt Encrypted File
	'SCB' , # Euro Truck Simulator 2 Product Key File
	'RAP' , # Scarab Ransomware Encrypted File
	'BCUP' , # Buttercup Vault
	'DCO' , # Safetica Free Encrypted Virtual Disk Archive
	'ENX' , # Max PC Safe Encrypted File
	'PXF' , # Pendix Firmware File
	'SIGNATURE' , # e-Filing Digital Signature File
	'BFA' , # Blowfish Encrypted File
	'LOCKER' , # NordLocker Encrypted Archive
	'ADAME' , # Adame Ransomware Encrypted File
	'GERO' , # GERO Ransomware Encrypted File
	'GDCB' , # GandCrab Ransomware Encrypted File
	'ODIN' , # Locky Ransomware Encrypted File
	'R2U' , # R2U Ransomware Encrypted File
	'EFR' , # Encryptafile Private Key File
	'ASC' , # PGP ASCII Armored File
	'REM' , # BlackBerry Encrypted Media
	'GPG' , # GNU Privacy Guard Encrypted File
	'SDTID' , # SecurID Soft Token File
	'BIN' , # MacBinary Encoded File
	'PLP' , # Photo Locker Picture
	'TAR.MD5' , # Android System File
	'JCEKS' , # JCEKS Keystore File
	'CUID2' , # Baidu Device ID File
	'ZPS' , # Zebra Portable Safe File
	'MEO' , # MEO Encrypted Archive
	'SAFE' , # SIGLock Encrypted File 
	'WOLF' , # Wolf RPG Editor Game Data Archive
	'BIT' , # FinalCrypt Encrypted Data File
	'NC' , # mcrypt Encrypted File
	'FILM' , # Filmkey Player Media File
	'FC' , # Paradise Ransomware Encrypted File
	'STXT' , # Sealed Text File
	'SHY' , # ShyFile Encrypted File
	'DDOC' , # DigiDoc Signature File
	'NXL' , # Nextlabs Encrypted Data File
	'XTBL' , # XTBL Ransomware Encrypted File
	'CRYPTRA' , # Cryptra Encrypted File
	'CERBER2' , # Cerber2 Ransomware Encrypted File
	'BCA' , # BCArchive Encrypted Archive
	'MJD' , # Adobe Acrobat MIME Encoded Job Definition File
	'SCB' , # Scrambls Encrypted File
	'KRAB' , # GandCrab V4 Ransomware Encrypted File
	'SDOC' , # Sealed Word Document
	'HQX' , # BinHex 4.0 Encoded File
	'APKM' , # Android App Bundle Mirror
	'RSDF' , # RapidShare Download File
	'ENC' , # Encoded File
	'XXX' , # Extractor Ransomware Encrypted File
	'PWV' , # Password Vault Archive
	'LOCKED' , # Ransomware Encrypted File
	'BPW' , # Bitser Password File
	'CDOC' , # Encrypted DigiDoc File
	'MME' , # Multi-Purpose Internet Mail
	'EDOC' , # Electronically Certified Document
	'RENSENWARE' , # Rensenware Encrypted File
	'HOOP' , # HOOP Ransomware Encrypted File
	'MSE' , # 3ds Max Encrypted MAXScript File
	'QSCX' , # QSCX Ransomware Encrypted File
	'EOC' , # EncryptOnClick Encrypted File
	'SQZ' , # digitalSQZ Encrypted File
	'B2A' , # Btoa Encoded File
	'SIA' , # Sia Metadata File
	'KK' , # SyncCrypt Ransomware Encrypted File
	'DCF' , # Safetica Free Encrypted Archive
	'SRF' , # Samsung Smart TV Recording
	'DED' , # DED Cryptor Ransomware Encrypted File
	'SALMA' , # Salma Ransomware Encrypted File
	'PYENC' , # PyFileEncrypt Encrypted File
	'JMC' , # JM-Crypt Encrypted File
	'UFR' , # Upfiring File
	'ZIP.ENC' , # Facebook User Information Encrypted Archive
	'SEB' , # Safe Exam Browser Configuration File
	'CRYPT' , # CryptXXX Ransomware Encrypted File
	'UUE' , # Uuencoded File
	'UU' , # Uuencoded File
	'PDC' , # Safeguard PDF Security Protected PDF
	'EFU' , # Encryptafile Public Key File
	'ESLOCK' , # ES File Explorer File Manager Encrypted File
	'VIIVO' , # Viivo Encrypted File
	'BSK' , # Nero SecurDisc Private Key File
	'XMDX' , # SofTest Answer File
	'JKS' , # Java Keystore File
	'VDATA' , # Vaulty Vault File
	'AES' , # AES Crypt Encrypted File
	'VP' , # Verilog Encrypted Source Code File
	'LCN' , # License File
	'NULL' , # Null Ransomware Encrypted File
	'CPT' , # Ccrypt Encrypted Archive
	'XXE' , # XXEncoded File
	'COOT' , # Coot Ransomware Encrypted File
	'MERRY' , # Merry X-Mas Ransomware Encrypted File
	'QEWE' , # QEWE Ransomware Encrypted File
	'JAC' , # JaStaCry Encrypted File
	'YKCOL' , # Locky Ransomware Encrypted File
	'SXLS' , # Sealed Microsoft Excel Spreadsheet
	'CRYPTO' , # Encrypto Encrypted File
	'EFDC' , # EFDC Ransomware Encrypted File
	'PSW6' , # Password Depot 6 File
	'AZS' , # AirZip FileSECURE File
	'CERBER' , # Cerber Ransomware Encrypted File
	'MIM' , # Multi-Purpose Internet Mail Message File
	'CRYPT1' , # UltraCrypter Ransomware Encrypted File
	'PFX' , # PKCS #12 Certificate File
	'MNC' , # AutoCAD Compiled Menu File
	'PDEX' , # Orient Computer Encrypted Data File
	'HTPASSWD' , # Apache HTACCESS File
	'ENCRYPTED' , # Crypren Ransomware Encrypted File
	'HID' , # KeepSafe File
	'MIME' , # Multi-Purpose Internet Mail Extension
	'DM' , # LG Encrypted Gallery File
	'EMC' , # Striata Reader Encrypted Document
	'KODE' , # KodeFile Encrypted File
	'SFI' , # SafeFolder Encrypted File
	'GIVEMENITRO' , # Nitro Ransomware
	'DIME' , # Direct Internet Message Encapsulation File
	'DC4' , # ViaThinkSoft (De)Coder 4 File
	'ADOBE' , # Dharma Ransomware Encrypted File
	'SPDF' , # Sealed PDF File
	'FGSF' , # Files by Google Safe Folder Encrypted File
	'KEYSTORE' , # Java Keystore File
	'MICRO' , # TeslaCrypt 3.0 Ransomware Encrypted File
	'HID2' , # KeepSafe File
	'GFE' , # Glarysoft Encrypted File
	'IDEA' , # Tresor IDEA Encrypted File
	'WNRY' , # WannaCry Virus Encrypted File
	'CRYPTED' , # WinOptimizer Encrypted File
	'ESF' , # Password Manager Container File
	'AFP' , # FileProtector Encrypted File
	'DLC' , # Download Link Container File
	'SA' , # Xiaomi Mobile Phone Hidden File
	'WNCRY' , # Wana Decrypt0r 2.0 Encrypted File
	'BIP' , # Dharma Ransomware Encrypted File
	'FPENC' , # FileProtect Encrypted File
	'R5A' , # 7ev3n Ransomware Encrypted File
	'CONTI' , # Conti Ransomware Encrypted File
	'WPE' , # WordPerfect Entrust Document
	'JMCE' , # JM-Crypt Encrypted File
	'AURORA' , # Aurora Ransomware Encrypted File
	'KIFR' , # KIFR Ransomware Encrypted File
	'SJPG' , # Sealed JPG File
	'WLU' , # Jaff Ransomware Encrypted File
	'WALLET' , # Wallet Ransomware
	'PFILE' , # Rights Management Protected File
	'ELBIE' , # Elbie Ransomware Encrypted File
	'XEF' , # WinAce Encrypted File
	'JMCX' , # JM-Crypt Encrypted File
	'AEP' , # Advanced Encryption Package Encrypted File
	'CEF' , # CenturionMail Encrypted Package
	'##' , # Encrypt Easy Encrypted File
	'PDY' , # StarMoney Encrypted Document
	'YNC' , # yEnc Encoded File
	'CODERCRYPT' , # CoderWare Encrypted File
	'WRYPT' , # Panwrypter Depleted Storage Volume File
	'NBES' , # Nbes Ransomware Encrypted File
	'CCF' , # CryptLoad Container File
	'LILITH' , # Lilith Ransomware Encrypted File
	'NITZ' , # NITZ Ransomware Encrypted File
	'MAAS' , # MAAS Ransomware Encrypted File
	'WIOT' , # WIOT Ransomware Encrypted File
	'RYK' , # Ryuk Ransomware Encrypted File
	'PTRZ' , # PTRZ Ransomware Encrypted File
	'JCRYPT' , # JCRYPT File
	'EDFW' , # Efficient Diary File
	'DIM' , # DIME File
	'EEGF' , # EEGF Ransomware Encrypted File
	'DJVU' , # STOP DJVU Ransomware Encrypted File
	'TCVP' , # TCVP Ransomware Encrypted File
	'SSPQ' , # SSPQ Ransomware Encrypted File
	'SDO' , # Signed Document
	'WRUI' , # WRUI Ransomware Encrypted File
	'SPD' , # Sealed Acrobat Document
	'KSD' , # KeepSafe File
	'PPDF' , # Rights Management Protected File
	'ZZZZZ' , # Ransomware Encrypted File
	'FUN' , # Jigsaw Ransomware Encrypted File
	'DCD' , # DisCryptor Encrypted Database
	'MTZU' , # MTZU Ransomware Encrypted File
	'SGZ' , # SigzaLock Encrypted File 
	'MFS' , # MetFS Encrypted File System
	'PFO' , # Private Folder
	'UUD' , # UUDecoded File
	'BFE' , # Bcrypt Encrypted File
	'MEDUSA' , # Medusa Ransomware Encrypted File
	'MKF' , # BlackBerry Encrypted Media Key File
	'K3Y' , # USB Raptor Password File
	'WERD' , # Werd Ransomware Encrypted File
	'SSOI' , # SSOI Ransomware Encrypted File
	'PARADISE' , # Paradise Ransomware Encrypted File
	'UIWIX' , # UIWIX Ransomware Encrypted File
	'RDI' , # Rohos Disk Image File
	'GOOD' , # Scatter Ransomware Encrypted File
	'ZEPTO' , # Zepto Virus File
	'PKEY' , # PowerKey Encrypted File
	'HEX' , # BinHex Encoded File
	'KLQ' , # Kaspersky Quarantine File
	'KXX' , # Keyman Developer Encrypted Keyboard File
	'ENX' , # eDataSecurity Management Encrypted File
	'EEWT' , # EEWT Ransomware Encrypted File
	'JMCK' , # JM-Crypt Key File
	'EFL' , # Encryptafile Encrypted File
	'CHML' , # Chameleon Encrypted Database File
	'MCRP' , # MobyExplorer Encrypted File
	'LQQW' , # LQQW Ransomware Encrypted File
	'RZX' , # File Crypt Encrypted File
	'NMO' , # NMO Ransomware Encrypted File
	'SWITCH' , # Switch Package
	'BVD' , # Bitdefender Vault File
	'CGP' , # PixelCryptor Encrypted File
	'REPP' , # Repp Ransomware Encrypted File
	'LOCKY' , # Locky Ransomware Encrypted File
	'LUCY' , # Black Rose Lucy Ransomware Encrypted File
	'EXC' , # eDataSecurity Management Self-extracting File
	'LVIVT' , # Lvivtotoro Encrypted Game File
	'WLS' , # R-Link Update File
	'YENC' , # yEnc File
	'E4A' , # Encrypt4all Archive
	'KKK' , # KKK Ransomware Encrypted File
	'SME' , # SmartEncryptor Encrypted File
	'IWA' , # iWork Archive File
	'THOR' , # Locky Ransomware Encrypted File
	'EFJI' , # EFJI Ransomware Encrypted File
	'VTYM' , # VTYM Ransomware
	'RRBB' , # RRBB Ransomware Encrypted File
	'MCQ' , # VirusScan Quarantined Data
	'DEVOS' , # Devos Ransomware Encrypted File
	'KCXZ' , # KCXZ Ransomware Encrypted File
	'SF' , # APK Digital Signature
	'PPENC' , # Privacy Protector Encrypted File
	'AESIR' , # Locky Ransomware Encrypted File
	'LITAR' , # Litar Virus Encrypted File
	'SAGE' , # Sage Ransomware Encrypted File
	'STOP' , # STOP Ransomware Encrypted File
	'BC5B' , # BC5B Ransomware Encrypted File
	'JMCR' , # JM-Crypt Encrypted File
	'PXX' , # Keyman Developer Encrypted Customization File
	'JMCP' , # JM-Crypt Encrypted File
	'GZQUAR' , # Bitdefender Antivirus Quarantine File
	'UEA' , # Protector Suite QL Encrypted Archive
	'FSM' , # Splitty Master Split File
	'LILOCKED' , # Lilocked Ransomware Encrypted File
	'SLE' , # Steganos Safe Encrypted Drive
	'MBA' , # Martus Bulletin Archive
	'CADQ' , # CADQ Ransomware Encrypted File
	'DHARMA' , # Dharma Ransomware Encrypted File
	'CLX' , # Ceelox SecureMail Secure Message
	'CTBL' , # CTB-Locker Ransomware Encrypted File
	'FONIX' , # Fonix Ransomware Encrypted File
	'RCRYPTED' , # Ryuk Ransomware Encrypted File
	'RUMBA' , # RUMBA Ransomware Encrypted File
	'RADMAN' , # RADMAN Ransomware Encrypted File
	'U2K' , # U2K Ransomware Encrypted File
	'AZE' , # Amaze Encrypted File
	'DJVUS' , # STOP DJVUS Ransomware Encrypted File
	'WNCRYT' , # Wana Decrypt0r 2.0 Temporary File
	'SEF' , # Password Manager Container File
	'KS' , # Keystore File
	'BTOA' , # Binary-to-ASCII Encoded File
	'AZF' , # AirZip FileSECURE File
	'CAROTE' , # Carote Ransomware Encrypted File
	'SXML' , # Sealed XML File
	'CUID' , # Baidu SDK File
	'VOOM' , # VOOM Ransomware Encrypted File
	'WCRY' , # WannaCry Virus Encrypted File
	'AAA' , # CryptoWall Ransomware Encrypted File
	'EXTR' , # COW App Extractor File
	'HBX' , # BinHex Encoded File
	'LXV' , # JumpDrive Secure II Vault
)

# noinspection SpellCheckingInspection
compressed_set = (
	'MINT' , # Linux Mint Installer File
	'ZHELP' , # Ziphelp Package
	'B6Z' , # B6Z Archive
	'ZST' , # Zstandard Compressed File
	'FZPZ' , # Fritzing Part File
	'APZ' , # Autoplay Media Studio Exported Project
	'UFS.UZIP' , # Compressed Unix File System File
	'VRPACKAGE' , # SimLab VR Package
	'SFG' , # Synfig Studio Compressed Project
	'GZIP' , # Gnu Zipped File
	'XAPK' , # Compressed Android Package
	'RAR' , # WinRAR Compressed Archive
	'PKG.TAR.XZ' , # Arch Linux Software Package
	'PUP' , # PlayStation 3 Update File
	'CIT' , # Cite Report Project
	'TPSR' , # TeamViewer Pilot Session Report File
	'TZST' , # Zstandard Compressed Tar File
	'TBZ' , # Bzip Compressed Tar Archive
	'S00' , # ZipSplitter Part 1 File
	'SY_' , # Compressed SYS File
	'P7Z' , # S/MIME Compressed Email Message
	'PKG' , # macOS Installer Package
	'TAR.XZ' , # XZ Compressed Tar Archive
	'NPK' , # MikroTik Software Package
	'SIT' , # StuffIt Archive
	'7Z' , # 7-Zip Compressed File
	'HTMI' , # HyperText Media Interoperable File
	'DEB' , # Debian Software Package
	'BZ2' , # Bzip2 Compressed File
	'BNDL' , # Game Data Bundle File
	'ZPAQ' , # ZPAQ Archive
	'ZPI' , # Zipped File
	'RTE' , # RTE Encoded File
	'S7Z' , # Mac OS X 7-Zip File
	'PIT' , # PackIt Archive
	'ICE' , # ICE Compressed Archive
	'ECAR' , # DIKSHA Offline Content Archive
	'WICK' , # Wick Editor Project
	'ARDUBOY' , # Arduboy Game Package
	'COMPPKG.HAUPTWERK.RAR' , # Hauptwerk Component Package
	'QDA' , # Quadruple D Archive
	'ECS' , # Sony Ericsson Phone Backup File
	'CB7' , # Comic Book 7-Zip Archive
	'HBE' , # HyperBac Compressed and Encrypted Archive
	'LZM' , # Slax Module
	'TBZ2' , # Bzip2-Compressed TAR File
	'MPKG' , # macOS Meta-Package
	'ITA' , # IconTweaker Theme Archive
	'DL_' , # Compressed DLL File
	'OPK' , # GCW Zero Open Package
	'ZL' , # Zlib Compressed File
	'EPI' , # EclipsePackager2000 Compressed File
	'SMPF' , # Sprite Monkey Project
	'SQX' , # SQX Archive
	'PF' , # Private File
	'ZZ' , # Pigz Zlib Compressed File
	'001' , # Split Archive Part 1
	'DZ' , # Dzip File
	'PAR' , # Parchive Index File
	'B1' , # B1 Compressed Archive
	'REV' , # RAR Recovery Volume Set
	'JSONLZ4' , # Compressed Firefox User Profile Data File
	'CBR' , # Comic Book RAR Archive
	'002' , # Split Archive Part 2
	'7Z.002' , # 7-Zip Split Archive Part 2 File
	'UHA' , # UHarc Compressed Archive
	'LEMON' , # LemonShare.net Download
	'ZIP' , # Zipped File
	'R00' , # WinRAR Compressed Archive
	'WA' , # Windows Addon Archive
	'TAZ' , # Tar Zipped File
	'PWA' , # Password Agent File
	'VIP' , # Virtual Instrument Package
	'NEX' , # Navigator Extension
	'KGB' , # KGB Archive File
	'PAK' , # PAK Compressed Archive
	'Q' , # Quantum Compressed File
	'PCV' , # MozBackup Profile Backup
	'A02' , # ALZip Fourth Split Archive File
	'APK' , # Alpine Linux Package
	'SFX' , # Windows Self-extracting Archive
	'RPM' , # Red Hat Package Manager File
	'PIMA' , # Adobe Application Manager Package
	'TAR.GZ' , # Compressed Tarball File
	'XIP' , # macOS Signed Archive
	'C00' , # WinAce Split Archive File
	'ZIX' , # WinZix Compressed File
	'PAR2' , # Parchive 2 File
	'TX_' , # Compressed Text File
	'LZ4' , # LZ4 Compressed File
	'LPKG' , # Liferay Application
	'DAR' , # DAR Disk Archive
	'ARCHIVER' , # Archiver Compressed File Archive
	'WHL' , # Python Wheel Package
	'SFS' , # Squashfs File Archive
	'APEX' , # Android Pony Express Package File
	'GZ' , # Gnu Zipped Archive
	'7Z.001' , # 7-Zip Split Archive Part 1 File
	'CBZ' , # Comic Book Zip Archive
	'BZ' , # Bzip Compressed File
	'SITX' , # StuffIt X Archive
	'LZ' , # Lzip Compressed File
	'SDOCX' , # Samsung Notes Note
	'A01' , # ALZip Third Split Archive File
	'RZ' , # Rzip Compressed File
	'PEA' , # PEA File Archive
	'SHK' , # ShrinkIt Archive
	'LZMA' , # LZMA Compressed File
	'SPD' , # S Note File
	'GMZ' , # Compressed GameMaker File
	'TGS' , # Telegram Animated Sticker File
	'Z03' , # Third Split Zip File
	'VPK' , # PlayStation Vita Application Package
	'F' , # Freeze Compressed File
	'WASTICKERS' , # Sticker Maker Sticker Pack
	'JAR.PACK' , # Pack200 Packed Jar File
	'PIZ' , # Zipped File
	'SIFZ' , # Synfig Studio Compressed Project
	'SPA' , # Spotify Application
	'CDZ' , # Compressed CD Image File
	'F3Z' , # Fusion 360 Shared Project Archive
	'XZ' , # XZ Compressed Archive
	'XX' , # XXEncoded File
	'MEMO' , # Samsung Memo
	'Z' , # Unix Compressed File
	'BH' , # BlakHole Archive
	'XOPP' , # Xournal++ Notebook
	'BUNDLE' , # Game Data Bundle File
	'SDOC' , # Samsung Notes Note
	'WAR' , # Java Web Archive
	'ACE' , # WinAce Compressed Archive
	'ARC' , # Compressed File Archive
	'PACKAGE' , # Linux Autopackage File
	'SHAR' , # Unix Shar Archive
	'PKZ' , # Packet Tracer Compressed Archive
	'OZ' , # Opera Job Management Compressed File
	'UBZ' , # OpenBoard Document
	'SDC' , # Secure Download Cabinet
	'CTZ' , # Cherrytree Password-protected XML Document
	'S02' , # ZipSplitter Part 3 File
	'SFM' , # S Memo File
	'004' , # Split Archive Part 4
	'CXARCHIVE' , # CrossOver Bottle Archive
	'VOCA' , # PhotoVOCA Communication Board File
	'P19' , # Parchive Recovery Volume
	'OAR' , # OpenSimulator Archive
	'R03' , # WinRAR Split Archive Part 3
	'GCA' , # GCA File Archive
	'SNB' , # S Note File
	'SH' , # Unix Shell Archive
	'A00' , # ALZip Second Split Archive File
	'LZH' , # LZH Compressed File
	'DD' , # DiskDoubler Archive
	'ARJ' , # ARJ Compressed File Archive
	'R0' , # WinRAR Compressed Archive
	'R30' , # WinRAR Split Archive Part 30
	'C01' , # WinAce Split Archive Part 1 File
	'TAR.BZ2' , # Compressed Tarball File
	'RK' , # WinRK Archive
	'ZIPX' , # Extended Zip Archive
	'PUP' , # Puppy Linux DotPup Installer Package
	'RNC' , # RNC ProPack Archive
	'MZP' , # MAXScript Zip Package
	'BZIP2' , # Bzip2 Compressed Archive
	'S01' , # ZipSplitter Part 2 File
	'J' , # JAR Archive
	'AR' , # Unix Static Library
	'NZ' , # NanoZip Compressed File
	'JEX' , # Joplin Export File
	'AYT' , # Quran Content Archive
	'GZA' , # IZArc BGA Archive File
	'PA' , # PowerArchiver Compressed File
	'SPT' , # TM File Packer Compressed Archive
	'ARK' , # PowerDesk Pro Archive
	'KEXTRACTION' , # Keka Extraction Package
	'TGZ' , # Gzipped Tar File
	'XMCDZ' , # Mathcad Compressed Worksheet File
	'TAR.LZMA' , # LZMA Compressed Tarball
	'PET' , # Puppy Linux Install Package
	'XAR' , # Extensible Archive Format File
	'STKDOODLZ' , # Sticker Doodle Sticker
	'ALZ' , # ALZip Archive
	'FP8' , # FP8 (= Fast PAQ8) Compressed File
	'PAQ8P' , # PAQ8P Data Archive
	'MLPROJ' , # MATLAB Archived Project
	'JHH' , # Tetris Online Poland Game Data Archive
	'ZFSENDTOTARGET' , # Compressed Folder
	'Z04' , # Fourth Split Zip File
	'SREP' , # SuperREP Compressed File
	'FDP' , # MySafe Encrypted Data
	'MOVPKG' , # High-Resolution Lossless Media File
	'JGZ' , # Gzipped Javascript File
	'IPK' , # Itsy Package
	'RP9' , # RetroPlatform Disk Image Archive
	'ZSPLIT' , # Archiver Compressed Split File
	'CBT' , # Comic Book TAR File
	'LBR' , # LU Library Archive
	'R2' , # WinRAR Multi-Volume Archive Part 2
	'R01' , # WinRAR Split Archive Part 1
	'ZOO' , # Zoo Compressed File
	'ARI' , # ARI Compressed Archive
	'TAR.LZ' , # Lzip Compressed Tarball
	'CZIP' , # ZipGenius CryptoZip File
	'HKI' , # WinHKI Archive
	'TG' , # Gzip Compressed Tar Archive
	'SEA' , # Self-Extracting Archive
	'SAR' , # SAPCAR Archive
	'TCX' , # TestComplete Script Extension Package
	'ZI_' , # Renamed Zip File
	'LHA' , # LHARC Compressed Archive
	'IADPROJ' , # iAd Producer Project
	'Z00' , # Split Zip Archive
	'XEZ' , # eManager Template Package
	'MOZLZ4' , # Compressed Firefox User Profile Data File
	'CTX' , # Cherrytree Password-protected SQLite Document
	'SNAPPY' , # Snappy Compressed File
	'RSS' , # RAM Structural System Model File
	'ISH' , # ISH Compressed Archive
	'CAR' , # CAR Archive
	'HYP' , # Hyper Compressed Archive
	'000' , # DoubleSpace Compressed File
	'PAX' , # PAX Archive
	'Z01' , # First Split Zip File
	'ZI' , # Renamed Zip File
	'SPL' , # Splunk Application Package
	'ISX' , # SimulationX Project
	'EDZ' , # EPLAN Electric P8 Data Archive Zipped File
	'MBZ' , # Moodle Backup FIle
	'MZP' , # WinArchiver Mountable Archive
	'LQR' , # Squeeze-Compressed LBR File
	'ZAP' , # FileWrangler Archive
	'LIBZIP' , # Camtasia Studio Zipped Library File
	'KWGT' , # Kustom Widget
	'WUX' , # Compressed Wii U Disk Image File
	'LHZD' , # Live Home 3D Compressed Project
	'R04' , # WinRAR Split Archive Part 4
	'PSZ' , # Compressed PostScript File
	'TZ' , # Zipped Tar Archive
	'CBA' , # Comic Book ACE Archive
	'MXC' , # MaxiCompress Compressed Archive
	'COMPPKG_HAUPTWERK_RAR' , # Hauptwerk Component Package
	'NAR' , # Nokia Image Archive
	'PBI' , # PC BSD Installer Package
	'Z02' , # Second Split Zip File
	'LZR' , # Crunch-Compressed LBR File
	'ECSBX' , # Error-Correcting SeqBox Container File
	'PAQ8F' , # PAQ8F Compressed Archive
	'LZX' , # Amiga LZX Compressed Archive
	'BZA' , # IZArc BGA Archive
	'VIB' , # VSphere Installation Bundle
	'HKI1' , # WinHKI HKI1 Archive
	'DGC' , # DGCA File Archive
	'B64' , # Base64 MIME-Encoded File
	'HKI3' , # WinHKI HKI3 Archive
	'WDZ' , # WDZip Compressed Archive
	'IZE' , # IZArc Archive
	'XOJ' , # Xournal Notebook
	'C10' , # WinAce Split Archive Part 10
	'VMCZ' , # Hyper-V Compressed Virtual Machine
	'BZIP' , # Bzip Compressed Archive
	'ZZ' , # Zzip Compressed Archive
	'IPG' , # iPod Game File
	'LAYOUT' , # LayOut Document
	'EGG' , # ALZip Archive
	'TXZ' , # XZ Compressed Tar Archive
	'SHR' , # Unix Shell Archive File
	'CPGZ' , # Compressed CPIO Archive
	'EFW' , # Renamed Zip or Executable File
	'MD' , # MDCD Compressed Archive
	'MAR' , # MSN Explorer Archive
	'BA' , # Scifer External Header Archive
	'HPKG' , # Haiku Package File
	'PKG.TAR.ZST' , # Arch Linux Installation Package
	'WACZ' , # Web Archive Collection Zipped
	'PACK.GZ' , # Pack200 Compressed Archive
	'PVMP' , # Parallels Packed Virtual Machine
	'GZ2' , # Misnamed BZ2 File
	'ZW' , # Zooper Widget Template
	'VSI' , # Visual Studio Content Installer File
	'SPM' , # Salt Package Manager Formula Package
	'TAR.Z' , # Zipped Tarball File
	'PRS' , # PRS Archive
	'HPK' , # HPack Compressed Archive
	'UC2' , # UltraCompressor 2 Archive
	'STG' , # ScreenToGif Project
	'C02' , # WinAce Split Archive Part 1 File
	'TLZ' , # Tar LZMA Compressed File
	'HBC2' , # HyperBac Compressed File Archive
	'XZM' , # Porteus Module
	'YC' , # YAC Compressed File
	'HA' , # HA Compressed Archive
	'LZO' , # LZO Compressed File
	'GAR' , # Ghidra Project Archive
	'GZI' , # Unix Gzip File
	'HBC' , # HyperBac Compressed Archive
	'MOU' , # WinMount Compressed File
	'CPT' , # Compact Pro Archive
	'ZIM' , # SimLab Composer Package
	'SNAGITSTAMPS' , # Snagit Stamp Archive
	'YZ1' , # Yamazaki Zipper Archive
	'PAE' , # PowerArchiver Encrypted Archive
	'UZIP' , # FreeBSD Compressed Disk Image
	'SNZ' , # Snappy Compressed File
	'PAQ6' , # PAQ6 Compressed Archive
	'FCX' , # FCX Compressed File
	'WAFF' , # Internet Explorer for Mac Web Archive
	'TAR.ZIP' , # Zipped Tar Archive
	'UZED' , # Zipped Unicode GEDCOM Genealogy File
	'JIC' , # Java Icon File
	'DAF' , # DupArchive Format File
	'ZABW' , # Compressed AbiWord Document
	'KZ' , # KuaiZip Compressed File
	'ASICE' , # ASiC-Extended Data Container
	'R02' , # WinRAR Split Archive Part 2
	'PIM' , # PIM Archive
	'UFDR' , # UFED Report
	'BDOC' , # Binary DigiDoc Signature File
	'WOT' , # Web Of Trust File
	'AIN' , # AIN Compressed Archive
	'VWI' , # Microsoft Visio Workflow Interchange File
	'TRS' , # Linear Saw Component Information Archive 
	'PVMZ' , # Parallels Compressed Virtual Machine
	'SDN' , # Shareware Distributors Network File
	'R21' , # WinRAR Split Archive Part 21
	'ARH' , # ProTool Compressed Project File
	'BOO' , # Booasm Compressed Archive
	'DIST' , # Mac OS X Distribution Script
	'SEN' , # Scifer Internal Header Archive
	'CP9' , # ChoicePoint Encrypted File
	'SIPA' , # Smint.io Portal Archive
	'PUZ' , # Packed Publisher File
	'TLZMA' , # LZMA Compressed Tar Archive
	'SQF' , # Squashfs File System
	'ANA' , # Animate Asset Package
	'PAQ7' , # PAQ7 Compressed Archive
	'R1' , # WinRAR Multi-Volume Archive Part 1
	'ZWI' , # Zipped Wiki Article
	'Y' , # Amiga Yabba Compressed Archive
	'OSF' , # OsmAnd Package
	'P01' , # Parchive Recovery Volume
	'S09' , # ZipSplitter Part 10 File
	'PAQ8L' , # PAQ8L Data Archive
	'KSP' , # KeyShot Package
	'STPROJ' , # iAd Producer Project
	'HKI2' , # WinHKI HKI2 Archive
	'WLB' , # WinList Protocol Bundle
	'CONDA' , # Conda Package
	'ODLGZ' , # Gzipped OneDrive Event Log
	'SBX' , # GLBasic Shoebox File
	'TAR.GZ2' , # Misnamed BZ2 Tarball
	'VEM' , # Virtual Expander Compressed File
	'BZABW' , # Compressed AbiWord Document
	'VFS' , # Animated Slide File
	'ZED' , # Zipped GEDCOM Genealogy File
	'PXL' , # Package and Extension Library Application
	'PAQ8' , # PAQ8 Data Archive
	'VMS' , # NanoZoomer Virtual Microscope Specimen
	'SBX' , # SeqBox Container File
	'SQZ' , # The Master Genealogist Backup Project
	'XFP' , # FormsForWeb Extensible Form Package
)

# noinspection SpellCheckingInspection
disk_image_set = (
	'VFD' , # Virtual Floppy Disk
	'DMG' , # Apple Disk Image
	'ISO' , # Disc Image File
	'CSO' , # Compressed ISO Disk Image
	'VMWAREVM' , # VMware Fusion Virtual Machine
	'MDS' , # Media Descriptor Sidecar File
	'VMDK' , # Virtual Machine Disk File
	'DVD' , # CloneCD DVD Information File
	'ROM' , # Read Only Memory Image
	'IMG' , # Disc Image Data File
	'MDF' , # Media Disc Image File
	'DSK' , # Disk Image
	'DCF' , # Disk Copy Fast Disk Image File
	'D01' , # VirtualDrive Disc Image Part 2 File
	'WLZ' , # WinImage Compressed Disk Image Set
	'I02' , # DVD Shrink Part 3 File
	'L01' , # EnCase Logical Evidence File
	'VAPORCD' , # Norum Vapor CD
	'EX01' , # EnCase Evidence Image File
	'MBI' , # Multi-Bootable Information File
	'BWS' , # BlindWrite Sub Code File
	'HD' , # IBochs Virtual Hard Disk
	'DMGPART' , # Mac OS X Disk Image Part
	'CFS' , # Compact File Set Archive
	'VDI' , # VirtualBox Virtual Disk Image
	'ASHDISC' , # Ashampoo Burning Studio Image
	'VHD' , # Virtual PC Virtual Hard Disk
	'DBR' , # DeepBurner Disc Project
	'D64' , # Commodore 64 1541 Disk Image
	'MACVM' , # Parallels Virtual Machine (Virtualization Framework Based)
	'LVI' , # Lazesoft Backup Disk Image
	'DAA' , # PowerISO Direct-Access-Archive
	'XVD' , # Xbox Virtual Disk
	'D00' , # VirtualDrive Disc Image Part File
	'SDI' , # Windows System Deployment Image
	'QCOW' , # QEMU Copy On Write Disk Image
	'MD0' , # Alcohol Disk Image Segment 1
	'BIN' , # Binary Disc Image
	'LCD' , # CDSpace Emulated Disk Image
	'IMA' , # Disk Image
	'WUD' , # Wii U Disk Image File
	'VHDX' , # Windows 8 Virtual Hard Drive File
	'MDX' , # Extended Media Descriptor File
	'NRG' , # Nero CD/DVD Image File
	'HDI' , # Hard Disk Image
	'AVHD' , # Hyper-V SnapShot File
	'SUB' , # CloneCD Subchannel File
	'QCOW2' , # QEMU Copy On Write Version 2 Disk Image
	'IBP' , # IsoBuster Managed Image Information
	'WIM' , # Windows Imaging Format File
	'BIF' , # Boot Information File
	'TOAST' , # Toast Disc Image
	'ISO' , # PlayStation 2 ROM File
	'UIBAK' , # UltraISO Backup Disk Image
	'XVA' , # Xen Project Virtual Appliance
	'CIF' , # Easy CD Creator Disk Image
	'E01' , # EnCase Image File
	'I01' , # DVD Shrink Part 2 File
	'HDS' , # Parallels Desktop Hard Disk File
	'BWT' , # BlindWrite 4 Track Information FIle
	'TIB' , # Acronis True Image File
	'CDT' , # CD-Text File
	'DISK' , # Linux Virtual Hard Disk
	'CDM' , # NTI CD-Maker Image
	'CCD' , # CloneCD Control File
	'ECM' , # ECM Disc Image
	'EDE' , # Ensoniq EPS Disk Image
	'SDSK' , # SafeHouse Private Storage Volume
	'DAX' , # PSP Compressed ISO Disc Image
	'ADZ' , # Compressed Amiga Disk File
	'XDI' , # WinArchiver Extended Disc Image File
	'VC4' , # Virtual CD Disc Image
	'ISZ' , # Zipped ISO Disk Image
	'IMG' , # Floppy Disk Image
	'SWM' , # Split Windows Imaging Format File
	'IPF' , # Interchangeable Preservation File
	'IBDAT' , # IsoBuster Data File
	'PVM' , # Parallels Virtual Machine
	'CDI' , # DiscJuggler Disc Image
	'2MG' , # Apple IIGS Disk Image
	'VCD' , # Virtual CD
	'IMG' , # Macintosh Disk Image
	'CDR' , # Macintosh DVD/CD Master
	'UIF' , # Universal Image Format Disc Image
	'HC' , # VeraCrypt Container
	'ADF' , # Amiga Disk File
	'GCD' , # Prassi CD Image
	'SCO' , # TotalRecovery Backup Image
	'MFI' , # HDClone MFI Disk Image
	'MRIMG' , # Macrium Reflect Disk Image
	'FLP' , # Floppy Disk Image
	'EUI' , # Ensoniq EPS Compacted Disk Image
	'DVDR' , # DVD/CD-R Master Image
	'SQFS' , # Squash FS Bootable File
	'BWI' , # BlindWrite CD/DVD Disc Image
	'MD1' , # GEAR CD Disc Image
	'HFS' , # HFS Disk Image File
	'P01' , # Toast CD Image
	'X64' , # Commodore 64 Disk Image
	'VCD' , # FarStone Virtual Drive
	'OMG' , # Image File
	'TZX' , # ZX Spectrum Tape Image File
	'NDIF' , # Apple New Disk Image Format File
	'TC' , # TrueCrypt Volume
	'LX01' , # EnCase Logical Evidence File
	'B5I' , # BlindWrite 5 Disk Image
	'GBI' , # gBurner Project File
	'MLC' , # MobaLiveCD File
	'UTM' , # UTM Virtual Machine
	'PMF' , # Partition Image File
	'TAP' , # Commodore 64 Cassette Tape Image
	'TOC' , # Brasero Table of Contents File
	'000' , # Virtual CD Disc Image File
	'SMI' , # Self-Mounting Disk Image
	'CISO' , # Compact ISO File
	'ST' , # Atari ST Disk Image
	'FDI' , # Amiga Formatted Disk Image
	'EDK' , # Ensoniq KT Disk Image
	'UDF' , # Universal Disk Format File
	'FLG' , # IsoPuzzle Flag File
	'T64' , # Commodore 64 Tape Image File
	'IMD' , # ImageDisk Disk Image File
	'AFF' , # AFF Disk Image
	'I00' , # DVD Shrink Part 1 File
	'IMAGE' , # Apple Disk Image
	'FDD' , # Parallels Desktop Floppy Disk Image File
	'PARTIMG' , # Partimage File
	'CL5' , # Easy CD Creator 5 Saved Project
	'BWZ' , # WinImage Batch Configuration File
	'IXA' , # Ulead Disc Image format
	'AFD' , # AFF Disk Image Part File
	'GKH' , # Ensoniq EPS Family Disk Image
	'MD1' , # Alcohol Disk Image Segment 2
	'SPARSEIMAGE' , # Mac OS X Sparse Image
	'VDI' , # Virtuo CD Manager Disk Image
	'WOZ' , # Apple II Disk Image
	'QED' , # QEMU Enhanced Disk Image
	'NKIT' , # Nintendo Kit Game ROM File
	'PXI' , # PlexTools Disc Image
	'P01' , # GEAR CD/DVD Disc Image
	'RAW' , # Fedora Linux Disk Image
	'XA' , # CD-ROM eXtended Architecture Disc Image
	'RCL' , # Easy CD and DVD Creator 6 Project
	'BWA' , # BlindWrite Disk Information File
	'PGD' , # PGP Disk Image
	'WMT' , # WinMount Virtual Disk File
	'HFV' , # HFS Disk Image
	'IBADR' , # IsoBuster Address File
	'VCX' , # Virtual Disc Definition File
	'INFINITEMACDISK' , # Infinite Mac Disk Image
	'PQI' , # PowerQuest Drive Image
	'P2G' , # Power2Go Project File
	'VCO' , # Virtual CD Collection File
	'HDD' , # Parallels Desktop Hard Disk
	'IMAGE' , # Squeak Virtual Image
	'B6I' , # BlindWrite 6 Disc Image
	'WII' , # Scrambled Wii Disc Image
	'DISC' , # Roxio Toast Document
	'NN' , # Nero CD File List
	'CUE' , # CDRWIN Cue Sheet
	'PDI' , # InstantCopy Disc Image
	'DMS' , # Amiga Disk Masher Image
	'IBQ' , # IsoBuster Managed Image
	'GI' , # Global Image
	'C2D' , # WinOnCD Disc Image
	'IBB' , # ImgBurn Project File
	'MD2' , # Alcohol Disk Image Segment 2
	'BDF' , # MIDAS BDF Disk Image File
	'EDA' , # Ensoniq ASR Disk Image
	'B6T' , # BlindWrite 6 Track Information File
	'RDF' , # PowerProducer Disc Image
	'IMZ' , # WinImage Compressed Disk Image
	'GDRIVE' , # Gizmo Virtual Drive File
	'WBI' , # Compact ISO File
	'DXP' , # CDBurnerXP Data Compilation File
	'B5T' , # BlindWrite 5 Disc Track Information File
	'CD' , # CD-i OptImage
	'86F' , # 86Box Floppy Disk Image 
	'TD0' , # Teledisk Archive
	'EDV' , # Ensoniq VFX-SD Disk Image
	'SIMG' , # Synclavier Disk Image File
	'ATR' , # Atari Disk Image
	'WINCLONE' , # Winclone Image
	'VOLARCHIVE' , # CopyCatX Volume Archive
	'ADF' , # Archimedes Disk File
	'G41' , # Commodore 1541 Disk Image
	'SOPT' , # Synclavier Optical Disk Image File
	'SPARSEBUNDLE' , # Mac OS X Sparse Bundle
	'LNX' , # Atari Lynx ROM
	'XMD' , # Extended Media Disc Image
	'NCD' , # NTI CD-Maker Disc Image
	'P2I' , # Power2Go Disc Image
	'FCD' , # Virtual CD Format
	'MINISO' , # Mini Image Destination File
	'MIR' , # Mini-Image Ripper Disc Image
	'DDI' , # DiskDupe Disk Image
	'FD' , # IBochs Floppy Disk Image
	'D88' , # Toshiba Pasopia 7 Disk File
	'WIL' , # WinImage Disk Image List
	'AFM' , # AFF Metadata File
	'PGX' , # Hopedot VOS Data File
	'RPKG' , # Symbian OS Z Drive ROM
	'NFI' , # Dreambox Disc Image
	'RATDVD' , # RatDVD Disk Image
	'AA' , # Generic CD Image
	'VHDPMEM' , # Hyper-V Virtual Machine Persistent Memory Device
	'EDQ' , # Ensoniq SQ1/SQ2/KS32 Disk Image
	'D71' , # Commodore 64 1571 Disk Image
	'D81' , # Commodore 64 1581 Disk Image
	'PO' , # ProDOS Order Apple II Disk Image
	'DO' , # DOS 3.3 Order Apple II Disk Image
	'TAO' , # Track-At-Once CD or DVD Image
	'DAO' , # Disk-At-Once CD or DVD Image
	'VC8' , # Virtual CD 8 Disc Image
	'EDS' , # Ensoniq SQ-80 Disk Image
	'XMF' , # GameJack Disc Image File
	'VC6' , # Virtual CD 6 CD Image
	'UFS' , # FreeBSD Unix File System File
	'K3B' , # K3b Disk Burning Project File
	'TAP' , # Oric Tape Image
)

# noinspection SpellCheckingInspection
developer_set = (
	'AWK' , # AWK Script
	'APPXBUNDLE' , # Windows App Bundle
	'UNITY' , # Unity Scene File
	'LGO' , # Logo Instructions File
	'IN' , # Autoconf Input File
	'RBXL' , # Roblox Location
	'KV' , # Kivy Language File
	'CS' , # C# Source Code File
	'SB' , # Scratch Project File
	'SB2' , # Scratch 2.0 Project File
	'SB3' , # Scratch 3.0 Project
	'IPR' , # IntelliJ IDEA Project File
	'PYD' , # Python Dynamic Module
	'YPR' , # BYOB Project File
	'KT' , # Kotlin Source Code File
	'APPX' , # Windows App Package 
	'RBXM' , # Roblox Model
	'INO' , # Arduino Sketch
	'CLASS' , # Java Class File
	'GITATTRIBUTES' , # Git Attributes File
	'PY' , # Python Script
	'CPP' , # C++ Source Code File
	'GCH' , # Precompiled Header File
	'LUC' , # Compiled Lua Source File
	'TRX' , # Visual Studio Test Results File
	'MM' , # Objective-C++ Source File
	'GM81' , # Game Maker 8.1 Project File
	'C' , # C/C++ Source Code File
	'VBP' , # Visual Basic Project File
	'ASM' , # Assembly Language Source Code File
	'ARSC' , # Android Package Resource Table
	'SCRIPTTERMINOLOGY' , # AppleScript Script Terminology File
	'CAPROJ' , # Construct Project File
	'AIDL' , # Android Interface Definition Language File
	'TT' , # Visual Studio Text Template
	'YMP' , # YaST Metapackage File
	'PAS' , # Delphi Unit Source File
	'XQ' , # XQuery File
	'PH' , # Perl Header File
	'SC' , # Scala Worksheet
	'MD' , # Markdown Documentation File
	'SLOGO' , # StarLogo Project File
	'AM4' , # AutoPlay Media Studio 4 Project File
	'HAS' , # Haskell Script
	'LUA' , # Lua Source Code
	'B' , # BASIC Source File
	'PWN' , # Pawn Source Code File
	'PO' , # Portable Object
	'GS' , # Google Apps Script
	'RS' , # Rust Source Code File
	'PBG' , # Pixel Bender Graph File
	'MF' , # Java Manifest File
	'SWC' , # Flex Components Archive
	'SWIFT' , # Swift Source Code File
	'CONFIG' , # Configuration File
	'CC' , # C++ Source Code File
	'TK' , # Tk Script
	'VDPROJ' , # Visual Studio Setup and Deployment Project
	'PATCH' , # Patch File
	'JAVA' , # Java Source Code File
	'FS' , # Visual F# Source File
	'HS' , # Haskell Script
	'COD' , # Compiled Source Code
	'DOX' , # Visual Basic Binary UserDocument
	'R' , # R Script File
	'Y' , # Yacc Source File
	'SUO' , # Visual Studio Solution User Options File
	'OCX' , # ActiveX Control
	'NK' , # NUKE Script
	'GMK' , # GameMaker Project File
	'GMX' , # GameMaker File
	'ISE' , # InstallShield Express Project File
	'RESOURCES' , # Visual Studio Resource File
	'SMALI' , # Android Smali Assembly Language File
	'V12.SUO' , # Visual Studio 2013 Solution User Options File
	'YML' , # YAML Document
	'DEF' , # Module-Definition File
	'DEX' , # Dalvik Executable File
	'HPP' , # C++ Header File
	'SO' , # Shared Library
	'RES' , # C++ Complied Resource Script
	'CD' , # Visual Studio Class Diagram
	'CSPROJ' , # Visual Studio C# Project
	'BBC' , # BBC BASIC Data File
	'BET' , # BETA Source File
	'FBP' , # wxFormBuilder Project
	'BPL' , # Borland Package Library
	'ML' , # ML Source Code File
	'SLN' , # Visual Studio Solution File
	'MK' , # Makefile
	'S' , # Source Code File
	'PJX' , # FoxPro Project
	'MXML' , # Flex MXML Component
	'ENTITLEMENTS' , # Mac App Sandboxing Entitlements File
	'BLUEJ' , # BlueJ Package File
	'DTD' , # Document Type Definition File
	'RUL' , # InstallShield Rules File
	'SWD' , # Flash Debug File
	'O' , # Compiled C Object File
	'RB' , # Ruby Source Code
	'HBS' , # Handlebars Template
	'MPX' , # FoxPro Compiled Program Menu
	'ERL' , # Erlang Source Code File
	'RESX' , # .NET Managed Resources File
	'DGML' , # Visual Studio Directed Graph Document
	'CCS' , # CodeCharge Studio Project File
	'VCXPROJ' , # Visual C++ Project
	'AIA' , # MIT App Inventor Project
	'LISP' , # Lisp Source Code File
	'WDGT' , # Adobe Captivate Widget File
	'PBXBTREE' , # Xcode Auto-Complete File
	'SSI' , # Adobe Dreamweaver Server Side Include File
	'XAMLX' , # Visual Studio Workflow Service File
	'ANE' , # Adobe AIR Native Extension
	'NUSPEC' , # NuGet Specification File
	'XCDATAMODELD' , # Xcode Core Data Model File
	'RC' , # Resource Script
	'XSD' , # XML Schema Definition
	'RES' , # Windows Resource File
	'PROTO' , # Protocol Buffer File
	'AU3' , # AutoIt v3 Script
	'CXP' , # CX-Programmer Project File
	'XT' , # Xdebug Trace File
	'XAML' , # XAML File
	'V' , # Verilog Source Code File
	'PB' , # PureBasic Source File
	'IPR' , # InstallShield Professional Project File
	'CTP' , # CakePHP Template
	'S19' , # Motorola S19 File Record
	'ADS' , # Ada Specification File
	'PL' , # Perl Script
	'H' , # C/C++/Objective-C Header File
	'EX' , # Euphoria Source Code
	'PYW' , # Python GUI Source File
	'FXML' , # FXML Source Code File
	'PBJ' , # Pixel Bender Bytecode File
	'APPXUPLOAD' , # Windows 8 App Upload Package
	'RPY' , # Python Script
	'YAML' , # YAML Document
	'D' , # D Source Code File
	'W32' , # Win32 Makefile
	'IDB' , # Visual Studio Intermediate Debug File
	'MSIX' , # MSIX Installer Package
	'TARGETS' , # MSBuild Targets File
	'MRT' , # Stimulsoft Report File
	'VB' , # Visual Basic Project Item File
	'AS3PROJ' , # FlashDevelop ActionScript 3 Project File
	'IML' , # IntelliJ IDEA Module
	'REXX' , # Rexx Source File
	'DPROJ' , # Delphi Project
	'MO' , # Machine Object File
	'FPM' , # FPS Creator Map File
	'L' , # Lex Source File
	'TWIG' , # Twig Template
	'VBG' , # Visual Basic Project Group File
	'AP_' , # ADT Android Developer Package
	'SCC' , # SourceSafe Source Code Control File
	'4DB' , # 4th Dimension Database Structure File
	'CU' , # CUDA Source Code File
	'SC' , # SuperCollider Source Code File
	'CSX' , # Visual C# Script
	'AS' , # ActionScript File
	'ASI' , # Alpha Five Variable File
	'BAS' , # BASIC Source Code File
	'TESTSETTINGS' , # Visual Studio Test Settings File
	'CSP' , # Caché Server Page
	'FTL' , # FreeMarker Template File
	'GM6' , # GameMaker 6 Project File
	'FRX' , # Visual Basic Binary Form File
	'AGI' , # Asterisk Gateway Interface File
	'TPU' , # Turbo Pascal Unit
	'FXC' , # FilePackager Configuration
	'HH' , # C++ Header File
	'JSFL' , # Animate JSFL Script File
	'PBXUSER' , # Xcode Project User Data File
	'GLD' , # MPLAB C Compiler Linker Script File
	'TLH' , # Typelib Generated C/C++ Header File
	'GSPROJ' , # GameSalad Windows Project File
	'GITIGNORE' , # Git Ignore File
	'DCP' , # Delphi Compiled Package
	'JSPF' , # Java Server Page Fragment
	'PRO' , # Qt Project File
	'PAS' , # Pascal Source File
	'ABC' , # ActionScript Byte Code File
	'DPR' , # Delphi Project
	'PBXPROJ' , # Xcode Project Data File
	'NUPKG' , # NuGet Package
	'INC' , # Include File
	'LIVECODE' , # LiveCode Stack File
	'I' , # Visual Studio Intermediate File
	'FRAMEWORK' , # Mac OS X Application Framework
	'MAK' , # Makefile
	'CLS' , # Program Class File
	'AM7' , # AutoPlay Media Studio 7 Project
	'F90' , # Fortran 90 Source Code File
	'SUD' , # Super Project Analyzer File
	'VBX' , # Visual Basic Custom Control
	'APA' , # RSView Development Project Archive
	'STORYBOARD' , # Xcode Interface Builder Storyboard
	'UI' , # User Interface File
	'ASC' , # ActionScript Communication File
	'KDEVPRJ' , # KDevelop Project File
	'DPL' , # Delphi Package Library
	'DMD' , # SQL Developer Data Modeler File
	'NIB' , # Interface Builder User Interface File
	'XCWORKSPACE' , # Xcode Workspace
	'KDEVELOP' , # KDevelop Project Data File
	'PLC' , # PL/B Source File
	'IDL' , # Interface Definition Language File
	'SVN-BASE' , # Subversion Base File
	'V' , # Coq Source Code File
	'VM' , # Velocity Template
	'MARKDOWN' , # Markdown Documentation File
	'M' , # Objective-C Implementation File
	'VHD' , # VHDL Source File
	'AS2PROJ' , # FlashDevelop ActionScript 2 Project File
	'RC2' , # Visual Studio Resources File
	'F' , # Fortran Source Code
	'JIC' , # JTAG Indirect Configuration File
	'PIKA' , # Pika Software Builder Project File
	'CBP' , # Code::Blocks Project File
	'WIQ' , # Visual Studio Work Item Query File
	'AUTOPLAY' , # AutoPlay Media Studio Project File
	'HAL' , # HansaWorld Application Language File
	'NXC' , # Not eXactly C Source Code File
	'W' , # OpenEdge Architect Source Code File
	'SMA' , # AMX Mod Plugin Source File
	'RSRC' , # Macintosh Resource File
	'ALB' , # Alpha Five Library
	'AM6' , # AutoPlay Media Studio 6 Project
	'DSGM' , # DS Game Maker Project
	'PRG' , # RAPID Program File
	'SLTNG' , # StarLogo TNG Project File
	'WDP' , # Windev Project File
	'WDL' , # World Definition Language Script
	'ISM' , # InstallShield Project File
	'BDSPROJ' , # Borland Developer Studio Project
	'FSSCRIPT' , # Visual F# Script
	'WDW' , # WinDev Window File
	'LTB' , # LithTech Binary File
	'TUR' , # Turing Program Source File
	'DBML' , # Visual Studio OR Design File
	'XPP' , # X++ Source Code File
	'LNT' , # PC-lint/FlexeLint Configuration File
	'CLW' , # Visual C++ ClassWizard File
	'M4' , # Macro Processor Library
	'SYM' , # Symbols File
	'RBC' , # Rembo-C Script
	'ICONSET' , # Mac OS X Icon Set Folder
	'HAML' , # Haml Source Code File
	'WSC' , # Windows Script Component
	'LSPROJ' , # Visual Studio LightSwitch Project
	'MCP' , # CodeWarrior Project
	'NVV' , # NVIDIA Vertex Shader File
	'PBK' , # Pixel Bender Kernel File
	'OCA' , # Custom Control Library Type File
	'LDS' , # Binutils LD Linker Script
	'DF1' , # Omnis Native Datafile
	'FOR' , # Fortran Source Code File
	'CXX' , # C++ Source Code File
	'NLS' , # NetLogo Source File
	'NSI' , # NSIS Script
	'WSP' , # SharePoint Solution Package
	'SAS' , # SAS Program File
	'RDLC' , # Visual Studio Client Report Definition File
	'ILK' , # Incremental Linking File
	'M' , # MATLAB Source Code File
	'BB' , # Blitz Source Code File
	'OCTEST' , # Xcode Objective-C Unit Test Bundle
	'NBC' , # Next Byte Codes Source Code File
	'XCCONFIG' , # Xcode Configuration Settings File
	'OMO' , # OMake Object File
	'PL1' , # PL/I Source Code
	'PKGDEF' , # Visual Studio Shell File
	'ASM' , # Visual Studio Assembler Source Code File
	'GEM' , # RubyGems Package
	'KPL' , # Kids Programming Language File
	'SUP' , # Super Project Definition File
	'MYAPP' , # Visual Studio Application XML File
	'PLAYGROUND' , # Xcode Playground File
	'MER' , # RSView Development Runtime File
	'IPCH' , # Intellisense Precompiled Header File
	'UML' , # UML Data Object Model
	'HXX' , # C++ Source Code Header File
	'PL' , # Prolog Source Code File
	'V11.SUO' , # Visual Studio 2012 Solution User Options File
	'XOJO_XML_PROJECT' , # Xojo XML Project File
	'DIFF' , # Patch File
	'CTXT' , # BlueJ Context File
	'MSHC' , # Microsoft Help Container File 
	'OWL' , # OWL Source Code File
	'MSHI' , # Microsoft Help Index File
	'XOML' , # Windows Workflow File
	'INL' , # C++ Inline File
	'MSS' , # Microprocessor Software Specification File
	'MSHA' , # Microsoft Help Asset File
	'CDF' , # CryENGINE Character Definition File
	'ERB' , # Ruby ERB Script
	'CP' , # Xcode C++ Source File
	'ACD' , # RSLogix 5000 Program
	'PCP' , # Windows Installer Patch Creation Properties File
	'CSI' , # ContentServ Include File
	'REFRESH' , # Visual Studio Refresh File
	'PYX' , # Pyrex Source Code File
	'JPR' , # JBuilder Project
	'SRC' , # Source Code File
	'EXP' , # Symbols Export File
	'A2W' , # Alice World
	'XOJO_PROJECT' , # Xojo Project File
	'PM' , # Perl Module
	'DGSL' , # Visual Shader Graph File
	'CCN' , # Compressed Multimedia Fusion File
	'FORTH' , # Forth Language File
	'TCL' , # Tcl Script
	'VDP' , # Visual Studio Deployment Project
	'VSMACROS' , # Visual Studio Binary Macro Project
	'DM1' , # ER/Studio Data Model File
	'EDMX' , # ADO.NET Entity Data Model Designer File
	'PLI' , # PL/I Source Code File
	'NW' , # Node-Webkit App Package
	'WXL' , # WiX Localization File
	'VBPROJ' , # Visual Studio Visual Basic.NET Project
	'PRI' , # Package Resource Index File
	'BSC' , # Visual Studio Source Browser Information File
	'MV' , # MivaScript File
	'NSH' , # NSIS Header File
	'ASVF' , # Asphyre Sphinx Archive File
	'WXS' , # WiX Source File
	'A' , # Static Library
	'LBI' , # Dreamweaver Library Item
	'PPC' , # Mobile Data Studio Project File
	'FTN' , # Fortran Source Code File
	'PXD' , # Pyrex Definition File
	'PTL' , # Rational Rose Petal File
	'IST' , # InstallShield Project Template File
	'CBL' , # COBOL Source Code File
	'DEC' , # Declaration File
	'HPF' , # High Performance Fortran File
	'VSSSCC' , # Visual Studio Solution Source Control File
	'SS' , # SilverStripe Source Code File
	'PCH' , # Precompiled Header File
	'MOM' , # Managed Object Model
	'CAF' , # CryENGINE Character Animation File
	'T' , # Turing Source Code File
	'POT' , # Portable Object Template
	'VC' , # Verge Code File
	'TLD' , # Tag Library Descriptor File
	'RESW' , # Windows Resources File
	'BBPROJECT' , # BBEdit Project File
	'TMLANGUAGE' , # TextMate Language Grammar File
	'TEXTFACTORY' , # BBEdit Text Factory
	'PRI' , # Qt Project Include File
	'VSZ' , # Visual Studio Wizard File
	'XCSNAPSHOTS' , # Xcode Snapshot
	'WIXMST' , # WiX Transform File
	'GSZIP' , # GameSalad Marketplace Asset File
	'WIXLIB' , # WiX Library File
	'P3D' , # Panda3D Multifile 
	'XOJO_BINARY_PROJECT' , # Xojo Binary Project File
	'FBZ7' , # Compressed FinalBuilder 7 Project
	'RAV' , # Rave Reports Project File
	'WIXOBJ' , # WiX Object File
	'CVSRC' , # CVS Command File
	'LUCIDSNIPPET' , # Lucid Snippet
	'WIXOUT' , # WiX XML Output File
	'JCP' , # JCreator Project File
	'WDGTPROJ' , # Dashcode Widget Project
	'PKGUNDEF' , # Visual Studio Shell File
	'XQL' , # XML Query Language File
	'XCODEPROJ' , # Xcode Project
	'LXSPROJ' , # Liquid XML Studio Project File
	'RISE' , # RISE Editor Model File
	'TESTRUNCONFIG' , # Visual Studio Test Run Configuration File
	'GEMSPEC' , # Gem Specification File
	'RODL' , # RemObjects Definition Language File
	'DPKW' , # Delphi Package
	'VTM' , # Visual Tool Markup Language Document
	'REX' , # Rexx Source File
	'LICX' , # Visual Studio License File
	'GMO' , # GNU Machine Object File
	'VSPSCC' , # Visual Studio Project Source Control File
	'IWS' , # IntelliJ IDEA Web Page
	'ODL' , # Object Description Language File
	'GS3' , # GameStarter File
	'SRC.RPM' , # Red Hat Package Manager Source File
	'FXL' , # FaceFX Language File
	'GED' , # Game Editor Project File
	'XCAPPDATA' , # Xcode App Data File
	'FSPROJ' , # FireStarter Project File
	'CLIPS' , # Coda Clips File
	'COB' , # COBOL Source Code File
	'BS2' , # BASIC Stamp 2 Code File
	'FXCPROJ' , # FX Composer Project File
	'PDM' , # VB Project Information File
	'BCP' , # Borland C++ Makefile
	'LBS' , # Omnis Library
	'RBP' , # Real Studio Project File
	'RNC' , # RELAX NG Compact Syntax File
	'VBZ' , # Visual Basic Project Template
	'GAMEPROJ' , # GameSalad Project File
	'VDM' , # VDM Specification File
	'GORM' , # Gorm Interface Resource File
	'XOJO_MENU' , # Xojo Menu File
	'IWB' , # IWBasic Source Code File
	'VSPX' , # Visual Studio Performance Report Data File
	'AM5' , # AutoPlay Media Studio 5 Project File
	'CP' , # Captivate Source File
	'EQL' , # Embedded SQL File
	'DBPROJ' , # Visual Studio Database Project File
	'RSS' , # Symbian Application Resource File
	'MDZIP' , # MagicDraw Project Archive
	'VSPS' , # Visual Studio Serialized Performance Report
	'WPW' , # WinDev Mobile Window File
	'TLI' , # Typelib Generated C/C++ Inline File
	'CCP' , # CodeCharge Studio Page File
	'TNS' , # TI-Nspire Document
	'CTL' , # Visual Basic UserControl Object File
	'ENT' , # External Entity
	'PSC' , # Papyrus Script
	'XCARCHIVE' , # Xcode Archive
	'BRX' , # BREW Application Resource File
	'LPROJ' , # Localized Project Folder
	'IDT' , # Windows Installer Database Text Archive File
	'DBA' , # CryENGINE Animation Database File
	'GREENFOOT' , # Greenfoot Project Archive
	'LICENSES' , # Visual Studio Licensed Classes File
	'4TH' , # Forth Language File
	'SPEC' , # RPM Specification File
	'DSP' , # Visual C++ 6 Project
	'VGC' , # ViziGen Code Generation Template
	'EDML' , # Adobe Extension Data Markup Language Document
	'FSPROJ' , # Visual F# Project File
	'XIB' , # Interface Builder File
	'P' , # Pascal Source Code
	'PLE' , # Messenger Plus! Live Encrypted Log File
	'TDS' , # Turbo Debugger Symbols File
	'MOD' , # Fortran Module
	'VTV' , # Adobe Dreamweaver Validator Configuration File
	'CSN' , # Adobe Code Snippet Document
	'BBPROJECTD' , # BBEdit Project Document
	'POD' , # Perl POD File
	'DPK' , # Delphi Package
	'R' , # Rez Source Code File
	'APS' , # Visual C++ Resource File
	'ADDIN' , # Visual Studio Add-in Definition File
	'NED' , # OMNeT++ Network Description File
	'PRG' , # Visual FoxPro Program File
	'INFORM' , # Inform Project 
	'GROUPPROJ' , # Delphi Project Group File
	'VTML' , # Visual Tool Markup Language File
	'RBW' , # Ruby Script
	'VSMPROJ' , # Visual Studio Text Macro Project
	'DCPROJ' , # Dashcode Project
	'FSX' , # Visual F# Script File
	'DBO' , # DarkBASIC Object
	'VSMDI' , # Visual Studio Test Metadata File
	'TU' , # Turing Source File
	'GFAR' , # Greenfoot Archive
	'SQLPROJ' , # Visual Studio SQL Server Project
	'SDEF' , # AppleScript Dictionary Document
	'XQM' , # XQuery Module
	'PSM1' , # Windows PowerShell Script Module File
	'FXPL' , # Adobe Flash FXP Library
	'MSP' , # MaxScript Page File
	'CFC' , # ColdFusion Component File
	'PDE' , # Processing Development Environment Source Code File
	'WIXPROJ' , # WiX Project File
	'LHS' , # Literate Haskell Script
	'VSP' , # Visual Studio Performance Report File
	'EXW' , # Euphoria Source code File
	'RKT' , # Racket Source Code File
	'DCU' , # Delphi Compiled Unit
	'DFM' , # Delphi Form
	'CSI' , # EdLog Program Data File
	'GROOVY' , # Groovy Source Code File
	'NCB' , # Visual C++ IntelliSense Database
	'BPG' , # Borland Project Group
	'LIT' , # Literate Haskell Script
	'RESJSON' , # Windows JavaScript Resources File
	'DBPRO' , # DarkBASIC Professional Project File
	'MSL' , # Mapping Specification Language File
	'DBA' , # DarkBASIC Source Code File
	'SCRIPTSUITE' , # AppleScript Script Suite File
	'VCP' , # eMbedded Visual C++ Project File
	'FSI' , # Visual F# Signature File
	'WIXPDB' , # WiX Debug File
	'WIXMSP' , # WiX XML Patch File
	'ARTPROJ' , # Artifacts Project
	'NQC' , # Not Quite C Source Code File
	'XQUERY' , # XQuery Source Code File
	'MAGIK' , # Magik Source Code File
	'SBPROJ' , # OpenGL Shader Builder Project
	'WXI' , # WiX Include File
	'WFM' , # dBASE Form
	'NFM' , # Delphi .NET Form File
	'SSC' , # SourceSafe Status File
	'DEVICEIDS' , # Device Identification FIle
	'JPX' , # JBuilder Project
	'GMD' , # Game Maker Program Code
	'R' , # REBOL Script
)

# noinspection SpellCheckingInspection
backup_set = (
	'IV2I' , # Norton Ghost Incremental Virtual Volume Image
	'SPASS' , # Samsung Pass Backup
	'SMEM' , # Smart Switch Text Message Backup
	'JWLIBRARY' , # JW Library Backup
	'ABK' , # Automatic Backup File
	'BKZ' , # FileFort Backup File
	'ACP' , # Alfresco Content Package
	'BLEND1' , # Blender Document Backup File
	'WX' , # Wanam Xposed Backup File
	'DSB' , # Dell DataSafe Backup File
	'SCRIPA' , # Scarlet IPA Backup
	'QBMB' , # QuickBooks Backup File
	'SBB' , # Office Accounting Company Backup File
	'VBOX-PREV' , # Oracle VM VirtualBox Settings Backup File
	'SPG' , # TCP Optimizer Backup File
	'MBK' , # dBASE Multiple Index Backup File
	'SQB' , # SQL Backup SQL Server Backup File
	'BAK2' , # Backup File
	'JPA' , # Akeeba Backup Archive
	'ADI' , # AOMEI Backupper Disk Backup File
	'ABU1' , # ASUS Backup File
	'BDB' , # Microsoft Works Database Backup File
	'NBA' , # Nero BackItUp Archive
	'BACKUP1' , # Split Android Backup File
	'003' , # Split Archive Part 3
	'SIS' , # Steam Game Backup Information File
	'PVHD' , # Paragon Virtual Hard Drive
	'BFF' , # Backup File Format
	'RDB' , # Retrospect Backup Set
	'CBU' , # Comodo Backup File
	'DUP0' , # Kies Sync Duplicate File
	'SV2I' , # Symantec System Index File
	'LCB' , # Living Cookbook Backup File
	'KB2' , # Kleo Backup File
	'WJF' , # WinZip Job File
	'FBU' , # FEBE Firefox Backup File
	'MDDATA' , # iPhone Backup File
	'IMAZINGAPP' , # iMazing App Data Backup
	'BA9' , # TaxAct 2019 Tax Return Backup File
	'LDABAK' , # Legistant Encrypted Backup File
	'NMM' , # Samsung Memo Backup File
	'DSS' , # DiskStation Backup Configuration File
	'SPF' , # ShadowProtect Full Backup File
	'ARC' , # Norton Backup Archive
	'GHO' , # Norton Ghost Backup File
	'BMK' , # BillMinder Backup File
	'BIF' , # Get Backup Project File
	'SBU' , # Samsung Backup File
	'V2I' , # Norton Ghost Virtual Volume Image
	'FBW' , # HP Recovery Manager Backup File
	'FBF' , # Free Backup Fix Backup File
	'BAKX' , # Finale 2014 Score Backup
	'FPSX' , # Nokia Firmware Format File
	'BAK' , # Chromium Bookmarks Backup
	'ABU' , # Asus App Backup File
	'STG' , # ActiveSync Backup File
	'ORIG' , # Original File
	'__A' , # File Splitter & Joiner Encrypted File
	'IPD' , # BlackBerry Backup File
	'CSD' , # Steam Game Data Backup File
	'BLEND2' , # Blender Document Backup 2 File
	'MBF' , # Microsoft Money Backup File
	'NMMM' , # Samsung Memo Backup File
	'BPS' , # Works Document Backup
	'CK9' , # Cook'n Cookbook Backup File
	'JBK' , # Juno Backup File
	'ONEPKG' , # Microsoft OneNote Package
	'NBF' , # Backup Now Archive
	'BAK' , # Backup File
	'BACKUPDB' , # Time Machine Backup Folder
	'PAQ' , # Hewlett-Packard Software Restore File
	'TLG' , # QuickBooks Transaction Log File
	'VBM' , # Veeam Backup Metadata File
	'TMP' , # Temporary File
	'BKUP' , # Backup File
	'SIM' , # Steam Game Backup Information File
	'SV$' , # AutoCAD Automated Backup File
	'SNA' , # Drive Snapshot Primary Backup File
	'SRR' , # ReScene Metadata File
	'BPA' , # StorageSync Backup Archive
	'CSM' , # Steam Game Backup File
	'WALLETX' , # Enpass Data File
	'DA0' , # Windows Registry Backup File
	'ATE' , # Office Accounting Compressed Backup File
	'SN1' , # Drive Snapshot Backup Continuation 1
	'MIG' , # Windows Migration Backup File
	'OLD' , # Backup File
	'ASD' , # Microsoft Word AutoSave File
	'RMBAK' , # Registry Mechanic Backup File
	'NCO' , # Nero BackItUp File
	'BA6' , # TaxAct 2016 Tax Return Backup File
	'MDINFO' , # iPhone Backup Information File
	'WIN' , # TWRP Backup
	'SKB' , # SketchUp Backup Document
	'SPI' , # ShadowProtect Incremental Backup File
	'PBD' , # EaseUS Todo Backup File
	'AFI' , # AOMEI Backupper File Backup File
	'IBK' , # IncrediMail Account Backup File
	'BOOKEXPORT' , # BookSmart Backup Book File
	'DPB' , # DataPilot Backup File
	'INPROGRESS' , # Time Machine Backup Progress File
	'BAK~' , # AbiWord Auto-Saved Document
	'ATI' , # Office Accounting Updated Company File
	'AS4' , # askSam Backup File
	'QIC' , # Windows Backup File
	'BA7' , # TaxAct 2017 Tax Return Backup File
	'TIG' , # TI Connect Backup File
	'NRS' , # NovaBACKUP Restore Script
	'BA8' , # TaxAct 2018 Tax Return Backup File
	'ADI' , # Active@ Disk Image Backup File
	'$DB' , # dBASE Temporary File
	'RBS' , # Windows Installer Rollback Script
	'TIBX' , # Acronis True Image Backup
	'BACKUP.METADATA' , # Seedvault Backup Metadata File
	'BAK' , # VEGAS Video Project Backup
	'NBF' , # Nokia Backup File
	'FUL' , # Microsoft Backup File List
	'ABBU' , # Address Book Backup
	'PFI' , # Paragon Backup Index File
	'KMNB' , # Keep My Notes Backup File
	'ENC' , # Smart Switch Encrypted App Backup
	'NPF' , # NTI Partition File
	'VPCBACKUP' , # Windows Virtual PC Backup File
	'NOY' , # NOY Backup File
	'BAK3' , # Backup File
	'BACKUP' , # Android Backup
	'CBK' , # Backup Configuration File
	'FBK' , # Microsoft Dynamics NAV Backup File
	'BKF' , # Windows Backup Utility File
	'SDC' , # Stardock Central Download Archive
	'BIFX' , # Get Backup Project
	'BCK' , # VMX Backup File
	'DBK' , # Sony Ericsson Mobile Phone Backup File
	'BPN' , # Archicad Project Backup File
	'$$$' , # Temporary File
	'LLX' , # LabVIEW VI Library Backup File
	'BCM' , # Microsoft Works Communications Script Backup
	'JPS' , # Akeeba Backup Archive
	'DIM' , # Active@ Raw Disk Image Backup File
	'WBB' , # WinBackup Archive
	'AQZ' , # Ancestral Quest Backup Database File
	'GBP' , # Genie Timeline Backup Index File
	'TINI' , # Chrome OS Crostini Backup
	'QBX' , # QuickBooks Accountant Transfer File
	'OBK' , # Backup File
	'DASH' , # Dashlane Profile
	'CCCTASK' , # Carbon Copy Cloner Task Configuration
	'ABF' , # Analysis Services Backup File
	'QBA.TLG' , # QBA Transaction Log File
	'IMAZING' , # iMazing iPhone Backup
	'BA0' , # TaxAct 2020 1040 Tax Return Backup File
	'NBK' , # NovaBACKUP Job File
	'GB1' , # Game Maker Backup File
	'VRB' , # Veeam Incremental Backup File
	'FH' , # Symantec Backup Exec File
	'NBU' , # Nokia Phone Backup File
	'TMP' , # Finale Temporary File
	'WBK' , # Microsoft Word Document Backup File
	'MEM' , # FoxPro Variable File
	'BAC' , # Backup File
	'SAV' , # Parallels Desktop Saved State Image File
	'BUP' , # Backup File
	'BAK' , # Act! Database Backup
	'QSF' , # Qualtrics Survey File
	'DBK' , # dBASE Database Backup
	'001' , # Norton Ghost Disk Backup
	'BPM' , # PowerDesigner Business Process Model
	'RBF' , # Windows Installer Rollback File
	'QUALSOFTCODE' , # J&ASoft Code Archive File
	'TDB' , # eBay Turbo Lister Backup File
	'AB' , # Android Debug Bridge Backup
	'BKP' , # Backup File
	'BAK' , # Finale 2012 Score Backup
	'BAK' , # MobileTrans Backup
	'CRDS' , # Windows CardSpace Backup File
	'WIN' , # FoxPro Window Settings
	'GHS' , # Norton Ghost Image Segment
	'SN2' , # Drive Snapshot Backup Continuation 2
	'DOV' , # Temp File
	'VBK' , # Veeam Backup File
	'BKP' , # Zapback Backup File
	'TBK' , # FoxPro Memo Backup
	'TRN' , # SQL Server Transaction Log Backup File
	'BKC' , # Backup4all Backup Catalog
	'WBX' , # Winbox Address Book
	'BM3' , # Sony Ericsson Backup File
	'PBX5SCRIPT' , # Personal Backup Script
	'GS-BCK' , # Genius Scan Backup File
	'SNMM' , # Smart Switch Notes Backup
	'WIN' , # Windows Backup File
	'LBF' , # LG Backup File
	'WBCAT' , # Windows Backup Catalog
	'FTMB' , # Family Tree Maker Backup File
	'PTB' , # Sage 50 Backup File
	'ASVX' , # Finale 2014 Auto-Saved Score
	'QUICKEN2017BACKUP' , # Quicken 2017 Backup File
	'FZA' , # Form•Z Autosave File
	'113' , # Iomega Disk Backup
	'QBM' , # QuickBooks Portable Company File
	'ARC' , # Symbian OS Backup File
	'NDA' , # Nero Backup File
	'W01' , # Samsung Recovery File
	'XLK' , # Microsoft Excel Backup File
	'PDB' , # PowerDesigner Database Backup File
	'MPB' , # MyPhoneExplorer Backup File
	'BCKP' , # Ad-Aware Quarantined File
	'GB2' , # Game Maker Backup File
	'PBF' , # Paragon Backup File
	'NBAK' , # Neat Backup File
	'EXML' , # Smart Switch Backup Encrypted XML File
	'FLKA' , # Folder Lock Portable Locker File
	'BBB' , # BlackBerry Mobile Phone Backup File
	'RDB' , # Remote Desktop Backup
	'EBABACKUP' , # EBA Command Center Backup
	'NBK' , # Data Backup Job File
	'MABK' , # Moto Android Backup File
	'NBD' , # Data Backup Data File
	'OPB' , # Outplayed Backup File
	'QUICKEN2015BACKUP' , # Quicken 2015 Backup File
	'TIBKP' , # Titanium Backup File
	'RBF' , # Stellar Insta Backup Image
	'FWBACKUP' , # Freeway Backup
	'CENON~' , # Cenon Backup File
	'73B' , # TI-73 Backup File
	'VMSG' , # Lumia Text Message Backup
	'TMR' , # Free Countdown Timer Backup File
	'AEA' , # AdWords Editor Archive
	'FBC' , # Family Tree Maker for DOS Backup File
	'BKP' , # Backup Your Mobile Backup Data
	'ABA' , # Palm Address Book Archive
	'UCI' , # UFS Explorer Backup File
	'QBMD' , # QuickBooks Backup File
	'SPS' , # SyncBack Settings File
	'NFC' , # Nokia Phone Backup Copy File
	'NFB' , # Nokia Phone Backup File
	'FHF' , # Free Hide Folder Backup File
	'PRV' , # Free Folder Hider Backup File
	'2FAS' , # 2FAS Token Backup
	'NPS' , # NTI Partition Set
	'QUICKEN2016BACKUP' , # Quicken 2016 Backup File
	'BK' , # Smart Switch Backup
	'ORI' , # Original File
	'BK1' , # Autodesk Backup File
	'PBF' , # PowerBackup Data File
	'BPB' , # PowerDesigner Business Process Backup File
	'ACR' , # Acer eRecovery Management Backup File
	'SME' , # Samsung Mobile Backup File
	'YRCBCK' , # Weblink Backup
	'JDC' , # JDownloader Links File
	'ASV' , # Finale 2012 Auto-Saved Score
	'MDBACKUP' , # iPhone Data Backup File
	'XBK' , # XenServer Backup File
	'BK1' , # ERwin Backup File
	'PBB' , # AT&T Phonebook File
	'PQB' , # Pleco Chinese Dictionary Backup File
	'NBS' , # NTI Backup Set
	'NBD' , # NovaBACKUP File
	'BBZ' , # BerryBuzz Backup File
	'NBI' , # Nero BackItUp Information File
	'ASHBAK' , # Ashampoo Backup Archive
	'__B' , # File Splitter & Joiner Encrypted Archive File
	'CMF' , # Connected Backup File
	'QV~' , # ViewletBuilder Project Backup File
	'FLKB' , # Folder Lock Basic Locker File
	'OEB' , # Outlook Express Backup Wizard File
	'QBK' , # QuickTax Backup File
	'DNA' , # CA Backup and Migration Backup File
	'MYNOTESBACKUP' , # Keep My Notes Backup
	'TIS' , # True Image Script
	'QBB' , # QuickBooks Backup File
	'NB7' , # NovaBACKUP Output File
	'NWBAK' , # NeatWorks Backup File
	'WSPAK' , # WebsitePanel Backup File
	'VPB' , # Photos Legacy Video Project Backup
	'~CW' , # Circuit Wizard Backup File
	'SNS' , # Save-N-Sync Project File
	'WPB' , # Windows Phone Device Manager Backup File
	'OYX' , # Approach Alternate Database Index
	'DIY' , # My Drivers Backup Setup File
	'CAA' , # CleanApp Archive
	'@@@' , # MS-DOS 2.0 to 3.2x Backup Control File
	'CBS' , # Comodo Backup Script
	'FBK' , # Family Tree Maker for Windows Backup File
	'MBW' , # MBRWizard Archive
	'BACKUP' , # Garmin BaseCamp Backup
	'ICBU' , # Calendar Backup File
	'PDU' , # LG Backup Extracted MMS Message
	'MV_' , # Movie Studio Project Backup
	'TTBK' , # Canadian TurboTax 2010 Backup File
	'BPK' , # BlueStacks App Migration Backup
	'CHATALL' , # ChatALL Backup
	'PBXSCRIPT' , # Personal Backup Script
	'PQB-BACKUP' , # Pleco Chinese Dictionary Backup File
	'NRBAK' , # NeatReceipts Backup File
	'ZBFX' , # zebNet Backup File Extended
	'SAFENOTEBACKUP' , # Safe Note Backup File
	'PVC' , # Parallels Desktop Copied Configuration File
	'DBA' , # Palm Datebook Backup File
	'PBJ' , # PowerBackup Job File
	'MBK' , # MediaTek Backup File
	'FITNOTES' , # FitNotes Data Backup
	'RBC' , # Retrospect Backup Catalog File
	'YOTI' , # Yoti Recovery File
	'ORRS' , # Deliveries Package Tracker Backup
	'ABEX' , # Enhanced Android Backup
	'TBK' , # dBASE Database Text Backup
	'RBF' , # Retrospect Backup File
	'OFBR' , # Oxygen Forensics Important Data Backup
	'BA1' , # TaxAct 2021 1040 Tax Return Backup
	'RRR' , # Registry Mechanic Backup File
	'SBS' , # Secure Backup System File
	'MSIM' , # mSecure Password Manager Backup File
	'RBK' , # Windows 95 Registry Backup File
	'QUICKENBACKUP' , # Quicken Essentials for Mac Backup File
	'ICF' , # Zoom Router Configuration File
	'PBA' , # PowerBackup Archive
	'SALL' , # SimplySync Backup Configuration File
	'FZB' , # Form•Z Project Backup File
	'PSA' , # Plesk Backup File
	'RMGB' , # RootsMagic Backup File
	'J01' , # Akeeba Multi-part Backup Archive
	'OBK' , # OrangeCD Backup File
	'SCBACKUP' , # Secret Calculator Folder Backup
)

# noinspection SpellCheckingInspection
misc_set = (
	'ADI' , # Dynamics AX Developer Documentation Index File
	'BRUSHSET' , # Procreate Brush Set
	'ART80' , # BERNINA ARTlink 8 Embroidery File
	'CBDS' , # Comic Book DS File
	'NAR' , # Ukagaka Ghost
	'WCLD2' , # WordClouds.com Save File
	'AAWDEF' , # Ad-Aware Definitions File
	'DOWNLOADING' , # Pando Incomplete Download File
	'IPSW' , # iPod and iPhone Software Update File
	'SR0' , # SecuROM Analysis File
	'MCFI' , # Maya Initial Fluid Cache File
	'T$M' , # AVG Internet Security Temporary File
	'PDPCOMP' , # Pdplayer Composition File
	'SIS.DM' , # Ovi Store DRM-Protected Application
	'JAM' , # FigJam Board
	'ISN' , # Installer Source File
	'CRDOWNLOAD' , # Chrome Partially Downloaded File
	'DCT' , # Dictionary File
	'TORRENT' , # BitTorrent File
	'AOD' , # Dynamics AX Object Data File
	'MSU' , # Windows Vista Update Package
	'HXE' , # Microsoft Help 2 Sample Definition File
	'DESKTOP' , # Desktop Entry File
	'LOOV' , # Smadav Virus Definitions File
	'ZVPL' , # Visual Paradigm License File
	'DTAPART' , # DownThemAll! Partially Downloaded File
	'LDB' , # Microsoft Access Lock File
	'INFO' , # Texinfo Document
	'PARTIAL' , # Internet Explorer Partially Downloaded File
	'P10' , # Certificate Request File
	'TFIL' , # Blizzard Software Update File
	'BP3' , # AutoCAD Batch Plot File
	'ESD' , # Windows Electronic Software Download
	'OB!' , # Orbit Downloader Incomplete Download
	'MSF' , # Mail Summary File
	'IDLK' , # Adobe InDesign Lock File
	'OPDOWNLOAD' , # Opera Partially Downloaded File
	'GTA' , # Microsoft Groove Tool Archive
	'SEARCH-MS' , # Windows Saved Search File
	'AVASTLIC' , # Avast License File
	'INFO' , # Generic Information File
	'JAD' , # Java Application Descriptor File
	'MTD' , # Musicnotes Digital Sheet Music File
	'_EML' , # Windows Live Mail Email File
	'LRC' , # Lyrics File
	'FB!' , # FlashGet Incomplete Download File
	'MPCPL' , # Media Player Classic Playlist
	'PART' , # Partially Downloaded File
	'MOBILEPROVISION' , # Xcode Mobile Provisioning Profile
	'NTH' , # Nokia Series 40 Theme File
	'SFV' , # Simple File Verification File
	'UNKNOWN' , # Unknown File Type
	'LOCK' , # Lock File
	'RFP' , # RoboForm Passcard File
	'VBT' , # iAntiVirus Temporary Scan File
	'TTX' , # Trados TagEditor File
	'ID' , # Lotus Notes User ID File
	'SVN-WORK' , # Subversion Cache File
	'DAT' , # Inno Setup Uninstaller Data File
	'SSLF' , # HLSW Shared Server List File
	'SSD' , # WindowBlinds Skin File List
	'INETLOC' , # Internet Location
	'XWF' , # OmniPage Workflow File
	'LTF' , # Frogans Shortcut File
	'FPFV' , # File Protected From Virus
	'RFN' , # RoboForm Safenote File
	'GROWLREGDICT' , # Growl Temp File
	'PAD' , # ACT! Database Pointer File
	'AUZ' , # Ableton Authorization File
	'MGT' , # Musicnotes Guitar Guru Song File
	'XLNK' , # XML Shortcut File
	'NWP' , # Magentic Wallpaper File
	'GLINK' , # Google Drive External File Shortcut
	'ICMA' , # InCopy Assignment File
	'MCF' , # Monocurl Project
	'MLS' , # CrystalPlayer Playlist
	'EF2' , # Internet Download Manager Export File
	'INLK' , # Adobe InDesign Lock File
	'3DC' , # RacerMate 3D Course File
	'VP3' , # Husqvarna Viking / Pfaff Embroidery File
	'CUL' , # Windows Cursor Library
	'SNF' , # Starry Night Pro Document
	'SKBA' , # SketchBook Pro Animation File
	'FREAKSHOW' , # Freakshow Industries License File
	'EFL' , # External Format Language File
	'!UT' , # uTorrent Incomplete Download
	'EGT' , # EagleGet Temporary File
	'DAT' , # Piriform Portable Mode Flag
	'ROV' , # Report Object Variable
	'SIS' , # Symbian Installation File
	'FRM' , # Form File
	'CHW' , # Compiled Help Index File
	'PLA' , # iriver iQuickList File
	'TDL' , # MathType Translator Definition Language
	'GG' , # Google Desktop Gadget File
	'GP4' , # Guitar Pro 4 Tablature
	'RTM' , # LabVIEW Run-Time Menu File
	'ADADOWNLOAD' , # Adobe Download Assistant Partial Download File
	'JMX' , # JMix File
	'DB' , # Windows Thumbnail Cache
	'CVR' , # Microsoft Crash Report File
	'G3A' , # Casio Prizm Add-in File
	'BBL' , # BibTeX Generated Bibliography File
	'FLA' , # Animate Animation
	'PSW' , # Windows Password Reset Disk File
	'CLS' , # LaTeX Document Class File
	'RSA' , # RSA Certificate File
	'CHK' , # Saved File Fragment
	'SISX' , # Symbian OS 9.1+ Installation File
	'RWZ' , # Outlook Rules Wizard File
	'MDL' , # Flight Simulator Airplane Model
	'GSP' , # Geometer's Sketchpad File
	'MVI' , # AutoCAD Movie Command File
	'BFC' , # Windows Briefcase File
	'DRC' , # DRM Rights Object
	'TD' , # Thunder Incomplete Download File
	'EZW' , # easyOFFER Real Estate Form
	'SKR' , # PGP Private Keyring
	'!BT' , # BitTorrent Incomplete Download File
	'DAT' , # Exchange E-mail Attachment
	'CACHE' , # Cache File
	'SLF' , # Symantec License File
	'PSI' , # PrimalScript Online Help Shortcut
	'APPUP' , # Erlang Application Upgrade File
	'_NWS' , # Windows Live Mail Newsgroup Copy File
	'CSH' , # Photoshop Custom Shapes File
	'OLB' , # OLE Object Library
	'UNL' , # Garmin Unlock File
	'SEC' , # PGP Secret Key Ring
	'BC' , # BitComet Partially Downloaded File
	'ODF' , # OpenDocument Formula
	'TPM' , # Transformation Parameters Model File
	'TEC' , # TECkit Compiled Mapping File
	'P2P' , # FolderShare Placeholder File
	'BUP' , # McAfee Quarantined File
	'WBA' , # WindowBlinds Compressed Skin
	'DLM' , # Akamai Download Manager File
	'FILE' , # Generic File
	'MD5' , # MD5 Checksum File
	'PVK' , # Private Key File
	'WJF' , # WinJournal Journal File
	'FDR' , # Windows Error Report File
	'RML' , # Elixir Report Template File
	'VNCLOC' , # Screen Sharing VNC Internet Location
	'TLDR' , # tldraw Drawing
	'APF' , # Adobe Profile File
	'MSI' , # Windows Installer Package
	'JQZ' , # JQuiz File
	'RND' , # PGP Random Seed File
	'APPDOWNLOAD' , # Mac App Store Partially Downloaded App
	'GDRAW' , # Google Drawings Shortcut
	'WTC' , # Logitech Webcam File
	'TPL' , # Document Template
	'BPS' , # BPS Virus File
	'SDR' , # Dell Support File
	'KEY' , # Software License Key File
	'SISX.DM' , # Nokia Ovi Store DRM-Protected Application
	'PAMP' , # Poweramp Album Art File
	'KHD' , # Dynamics AX Kernel Help Data File
	'CFS' , # Lucene Compound Index File
	'CALIBRE' , # Calibre Device Data File
	'CLKK' , # Clicker Keyboard
	'ARR' , # Advanced RAR Password Recovery Auto-Save
	'AZ!' , # Vuze Partial Download File
	'MTHD' , # MOPEKS Method File
	'ACL' , # AutoCorrect List File
	'COS2' , # Pinnacle Studio Cache File
	'ZTF' , # DVD Label Template File
	'TNEF' , # Transport Neutral Encapsulation Format
	'CXF' , # Picasa Collage File
	'DISKDEFINES' , # Linux LiveCD Information File
	'SHS' , # Microsoft Scrap File
	'SCW' , # Score Writer Document
	'ABR' , # Adobe Photoshop Brushes
	'SST' , # Serialized Certificate Store File
	'NUP' , # Smart Security Update File
	'QUA' , # Avira AntiVir Quarantined File
	'BC!' , # BitComet Incomplete File
	'MRK' , # DPOF Auto Print Order File
	'SDX' , # Kivuto Secure Download Manager File
	'HEX' , # Hexadecimal Source File
	'DWL' , # TurboCAD Drawing Lock File
	'WLX' , # Windows Live Mesh Sync File
	'XSLIC' , # XenServer License File
	'DBX' , # Outlook Express E-mail Folder
	'PES' , # Brother PE Embroidery Format
	'XMP' , # Extensible Metadata Platform File
	'RJS' , # RealJukebox Skin File
	'PTH' , # 4D Path Document
	'PLF' , # Papyrus License File
	'DOC#' , # LibreOffice Document Lock File
	'NA2' , # Netscape Address Book File
	'PLSK' , # Messenger Plus! Live Skin Pack
	'TOC' , # Eudora Table of Contents
	'FLF' , # Microsoft Dynamics NAV License File
	'VMG' , # Nokia Text Message
	'SIG' , # Signature File
	'ZML' , # Zooming Markup Language File
	'EXP' , # Melco Embroidery File
	'LIC' , # Software License File
	'THM' , # Sony Ericsson Theme File
	'000' , # Windows CE Installation Data File
	'ICS' , # Calendar File
	'MAR' , # Mozilla Archive
	'ZZZ' , # CCleaner Securely Deleted File
	'0' , # Hacha Split File Segment
	'Z80' , # TI-83 and 84 Plus Calculator File
	'JMT' , # JMatch File
	'FND' , # Windows Saved Search
	'SFT' , # Microsoft App-V Sequencer File
	'MGO' , # MacGourmet Recipe File
	'RSA' , # Minecraft JAR Codesign File
	'ICALENDAR' , # iCalendar File
	'SNM' , # Netscape Mail Index
	'KMR' , # KnowledgeMill Link File
	'LAY' , # DVD Studio Pro Layout File
	'MNL' , # AutoCAD Menu LISP File
	'CLKX' , # Clicker Grid Set
	'VMF_AUTOSAVE' , # Valve Hammer Editor Autosave File
	'ERR' , # AutoCAD Error Log File
	'DAP' , # Download Accelerator Plus Partial Download
	'HXK' , # Microsoft Help 2 Index File
	'REQ' , # Canon SD Card Version Request File
	'ZM1' , # ZoneAlarm Renamed MDA File
	'IFL' , # 3ds Max Image File List
	'XSD' , # Pattern Maker for Cross Stitch 4 Pattern
	'VBT' , # Spyware Doctor Temporary Scan File
	'ENF' , # EndNote Filter File
	'SC' , # Space Engine Script
	'PLS' , # Multimedia Playlist File
	'WDSEML' , # Thunderbird Searchable Email File
	'JC!' , # FlashGet Incomplete Download
	'RPMSG' , # Outlook Restricted Permission Message
	'CRC' , # Total Commander Checksum File
	'NOMEDIA' , # Android No Media File
	'CTG' , # Canon Digital Camera Catalog
	'MDF' , # Translation Memory Data Find File
	'ERL' , # GameGuard Error Log File
	'LID' , # Kodak EasyShare Album File
	'ARIA2' , # aria2 Control File
	'ZM3' , # ZoneAlarm Renamed NCH File
	'MAT' , # Microsoft Access Table Shortcut File
	'XXX' , # Compucon Singer Embroidery File
	'MANIFEST' , # Steam Manifest File
	'VIR' , # Virus-Infected File
	'ASS' , # Aegisub Advanced SubStation Alpha File
	'BT!' , # BitSpirit Incomplete Download File
	'LOGONXP' , # LogonStudio Windows XP Logon Screen
	'DST' , # Tajima Embroidery Format
	'JMS' , # Masher Project
	'BST' , # BibTeX Style Document
	'CVF' , # CPU-Z Validation File
	'OST' , # Outlook Offline Data File
	'CAS' , # Autodesk Cascade License File
	'ING' , # MasterCook Ingredients File
	'BQY' , # Brio Query File
	'EXD' , # Control Information Cache File
	'MMAP' , # MindManager Map
	'RTC' , # Live Meeting Connection File
	'SIDD' , # iTunes Authorization Data File
	'DWLIBRARY' , # Paperless Document Library
	'SWJ' , # SolidWorks Journal File
	'MGDATABASE' , # MacGourmet Deluxe Database File
	'FMELIC' , # FME License File
	'CHUNK001' , # File Splitter Split Archive Part 1
	'SIDN' , # iTunes Authorization Data File
	'SPK' , # Synology Package
	'TSTREAM' , # SwarmPlayer Streaming Torrent
	'MMO' , # Memory Map Overlay File
	'CFL' , # IMVU Product File
	'XNK' , # Exchange Shortcut
	'VOR' , # StarOffice Template
	'83P' , # TI-83 Calculator File
	'DAL' , # Download Accelerator Plus URL List
	'NK2' , # Outlook 2002 Nickname File
	'MIF' , # Management Information Format
	'CDS' , # ConceptDraw DIAGRAM SlideShow (Legacy)
	'BOOTSKIN' , # Stardock Bootskin File
	'LXA' , # Microsoft Speech Lexicon File
	'SMWT' , # Adobe InDesign Filter
	'PANDO' , # Pando File
	'SWP' , # Vi Swap File
	'MSO' , # Microsoft Office Macro Reference File
	'SNT' , # Sticky Notes File
	'SPD' , # Form·Z Suspended Render File
	'NJB' , # Nikon Photo Index File
	'AEPKEY' , # Advanced Encryption Package Key File
	'SSC' , # Stellarium Script
	'CDF-MS' , # ClickOnce Compiled Manifest File
	'MAB' , # Mozilla Address Book
	'MSRCINCIDENT' , # Windows Remote Assistance Invitation File
	'TEMP' , # Temporary (Temp) File
	'DIC' , # Dictionary File
	'LCK' , # Program Lock File
	'SHA512' , # SHA 512 Hash File
	'CP3' , # Grand Prix 3 Cockpit Shape File
	'ICAL' , # iCalendar File
	'SXM' , # StarMath Formula
	'WWD' , # Works Wizard File
	'TIP' , # TuneUp Utilities Icon Package
	'LYR' , # ArcView Layer File
	'8XP' , # TI-83 Plus Calculator File
	'TLS' , # TuneUp Utilities Logon Screen
	'ZM2' , # ZoneAlarm Renamed MDZ File
	'RND' , # Random Hash Data File
	'GAU' , # Flight Simulator Gauge File
	'P7R' , # Certificate Request Response File
	'LINK' , # iPod Link File
	'FLK' , # AccountEdge Company Lock File
	'Z1' , # ZoneAlarm Renamed VB File
	'MONTAGE' , # Montage Screenplay File
	'VOL' , # Volfs File System Directory
	'VMHF' , # VMware Hot Fix File
	'RXC' , # Roxio Certificate File
	'ADD' , # Dynamics AX Developer Documentation File
	'VPA' , # VPchat Chat Gestures File
	'CRD' , # Windows CardSpace File
	'CL' , # Cursor Library
	'REQ' , # SSL Certificate Request File
	'PPK' , # PuTTY Private Key File
	'JC' , # FlashGet Partial Download
	'CCH' , # .NET Security Resolution Cache File
	'KWM' , # WebMoney Key File
	'DSC' , # Nikon Disk Identification File
	'DECRYPT' , # Decrypted Microsoft ESD File
	'P7M' , # S/MIME Email Message
	'EXT' , # Generic Email Attachment
	'SHX' , # Motorola Flash Superfile
	'MTA' , # Samsung AllShare Metadata File
	'MZZ' , # Microsoft .NET Download Package
	'BKMK' , # Yummy FTP Bookmark File
	'MTF' , # MasterCook Tips File
	'MFIL' , # Blizzard Software Update File
	'PEKEY' , # PhysicsEditor License Key
	'RFB' , # RoboForm Bookmark File
	'GOD' , # God Configuration File
	'PLA' , # PlantStudio Plant File
	'XOL' , # PowerDesigner Object Language File
	'KYR' , # Lotus Domino Key Ring File
	'ICONTAINER' , # CandyBar iContainer File
	'DUID' , # DHCPv6 Unique Identifier
	'BPDX' , # Acrobat Batch PDX File
	'BWS' , # Brady Workstation Label
	'ZCI' , # PowerCD Multimedia File
	'LOGONVISTA' , # LogonStudio Windows Vista Logon Screen
	'CLKW' , # Clicker Word Bank
	'VP4' , # PREMIER+ / Husqvarna Viking / Pfaff Embroidery File
	'STH' , # Lotus Domino Stash File
	'LWTP' , # LimeWire Theme Pack
	'DSTUDIO' , # DownloadStudio Incomplete Download
	'INCA' , # InCopy CS3 Assignment File
	'KHI' , # Dynamics AX Kernel Help Index File
	'AHI' , # Dynamics AX Online Help Index File
	'LINX' , # iPod Links Folder
	'IMAPMBOX' , # IMAP Mailbox
	'SKINDEX' , # Mail Index File
	'FTPLOC' , # FTP Location
	'NAV2' , # WinAVI Temporary Navigation File
	'HS' , # Motorola Flash Flex File
	'DR' , # DRM Rights Object
	'TPM' , # Trusted Platform Module Password File
	'MC2' , # MasterCook 5 Cookbook File
	'MNX' , # FoxPro Menu
	'TLB' , # OLE Type Library
	'MDMP' , # Mindclouds.com Save File
	'MJDOC' , # MacJournal Document
	'AVGDX' , # AVG Diagnostics File
	'WJE' , # WinJournal Entry
	'PTM' , # Protheus Patch
	'CDSZ' , # ConceptDraw DIAGRAM SlideShow
	'CNV' , # Office Converter File
	'HMX' , # Help & Manual 4 Project
	'_SYS.LFO' , # Adware File
	'CONTOUR' , # Contour Screenplay File
	'VPC7' , # Virtual Machine Package
	'ART70' , # BERNINA ARTlink 7 Embroidery File
	'JCW' , # JCross File
	'WL' , # Wolfram Language Package
	'RWLIBRARY' , # Paperless Receipt Library
	'SHA1' , # SHA-1 Hash File
	'FW' , # Firmware Update
	'AIF' , # Symbian Application Information File
	'UPG' , # Upgrade File
	'PKPASS' , # Apple Wallet Pass
	'ART' , # BERNINA Embroidery File
	'NSS' , # Magentic Screensaver File
	'QDS' , # Windows Directory Query File
	'VFS' , # Virtual File System Index
	'TNSP' , # TI-Nspire PublishView Document
	'MCFP' , # Maya Fluid Cache Playback File
	'W' , # Google Toolbar Search History File
	'MAG' , # Access Diagram Shortcut File
	'RELOC' , # Windows EXE Relocation Section File
	'SPLIT' , # Archiver Split File
	'NFS' , # Network File System Temporary File
	'TBS' , # TuneUp Utilities Boot Screen
	'MCO' , # Live Messenger Winks File
	'SUM' , # Garmin Checksum File
	'EDC' , # Kryptel Lite Encrypted File
	'MBS' , # Opera Mailbox File
	'MBB' , # Kodak EasyShare Data File
	'DCTMP' , # DC++ Partially Downloaded File
	'SMRD' , # Adobe InDesign Filter File
	'BMK' , # Nokia Device Bookmark File
	'DCOVER' , # Disc Cover File
	'PGP' , # PGP Security Key
	'NICK' , # Outlook 2000 Nickname File
	'ALT' , # Dynamics AX Temporary File
	'LETTER' , # Undeliverable Mail File
	'DISABLED' , # Spybot - Search & Destroy Disabled File
	'MSO' , # Outlook In-Line Office Attachment
	'PSAR' , # PSP Update File
	'MB' , # Kodak EasyShare Data File
	'FEEDBACK' , # Expression SketchFlow Feedback File
	'YPS' , # Yahoo! Messenger Data File
	'IMY' , # iMelody Ringtone File
	'JCL' , # JCloze File
	'ULS' , # NetMeeting User Location Service File
	'AC$' , # AutoCAD Temporary File
	'HXA' , # Microsoft Help 2 Attribute Definition File
	'BLF' , # MasterWorks Working File
	'BLI' , # Thomson Speedtouch Firmware File
	'RFT' , # RoboForm Identity File
	'PTR' , # Flash Intro Project File
	'LICENSEKEY' , # iPartition License Key File
	'AML' , # Microsoft Assistance Markup Language File
	'CLKT' , # Clicker Grid Set Template
	'TPKEY' , # TexturePacker License Key File
	'STORYMILL' , # StoryMill Project File
	'WRTS' , # Wrts File
	'NCW' , # Nero CoverDesigner Wizard File
	'VPC6' , # Virtual Machine Package
	'EBN' , # Philips Firmware Update File
	'ITC' , # iTunes Cover Flow Data File
	'SSW' , # Sony Ericsson Firmware File
	'MWF' , # Translation Memory Word Find File
	'AFPLOC' , # Apple Filing Protocol Location
	'DWC' , # Emulex DWC Firmware Update File
	'PVM' , # HP Photo Album
	'DTF' , # Domino Temporary File
	'SLUPKG-MS' , # XrML Digital License Package
	'SHORTCUT' , # Asutype Shortcut
	'CLR' , # Animate Color Set
	'QXL' , # QuarkXPress Library
	'SHV' , # Husqvarna Viking Embroidery File
	'CERBER3' , # Cerber Ransomware Virus File
	'MAP' , # TECkit Mapping File
	'DWN' , # Firmware Update File
	'PN' , # Powernoodle File (Discontinued)
	'RADIUMKEY2' , # Radium 2 License File
	'RMF' , # Adobe Rights Management Document
	'NDL' , # Lotus Notes Doclink File
	'IN' , # Spyware File
	'SLB' , # AutoCAD Slide Library
	'SBC' , # Office Accounting Company Shortcut File
	'ACB' , # AOL Cabinet File
	'CTF' , # AVG Update Control File
	'MAILTOLOC' , # Mail Internet Location File
	'WORDLIST' , # Desktop Poet Word List File
	'CHKSPLIT' , # Archiver Split Checksum File
	'DEK' , # Memorize-It Flashcard Deck
	'YBD' , # YobiDrive Link File
	'SIDB' , # iTunes Authorization Data File
	'CML' , # CryptoMailer Encrypted File
	'HDK' , # HotDocs Registration File
	'SC2LOCALE' , # Blizzard StarCraft 2 Localization File
	'LOOKLIBRARY' , # Painter Looks Library
	'VMDK-CONVERTTMP' , # VMWare Fusion Temporary File
	'ZTR' , # DVD Label Template Media File
	'RMH' , # Rights-Managed HTML Document
	'INDK' , # Adobe Shortcut Set File
	'DMX-INFO' , # DriverMax Driver Information File
	'DA2' , # Adaware License File
	'AOI' , # Dynamics AX Application Object Index File
	'NTF' , # MediaFACE II CD Label
	'PAPERS' , # Painter Paper Texture File
	'CDLX' , # ConceptDraw DIAGRAM XML Library
	'XPR' , # PressIt Disc Label
	'AHD' , # Dynamics AX Online Help Data File
	'XENSEARCH' , # XenCenter Saved Search File
	'MERLINLICENSE' , # Merlin License File
	'MWLIC' , # MoneyWell License File
	'MAILPLANELICENSE' , # Mailplane License File
	'RASKINLICENSE' , # Raskin License File
	'RAT' , # PICS Rating File
	'CTF' , # Content Transformation File
	'DHV' , # Husqvarna Viking Embroidery Design File
	'HYP' , # Dictionary Information File
	'EZLOG' , # Skype Extras Manager Log File
	'WZMUL' , # WinZip Registration File
	'FNLF' , # Scarlett Plug-in Suite License File
	'RFC' , # RoboForm Contact File
	'VMHR' , # VMware Hot Fix Request File
	'MXN' , # Miinoto Exchangeable Note File
	'STMB' , # Synclavier Timbre File
	'OTC' , # OpenDocument Chart Template
	'HXT' , # Microsoft Help 2 Table of Contents File
	'CDL' , # ConceptDraw DIAGRAM Library
	'TCR' , # TouchMagix Creative Suite Published File
	'MCF' , # MasterCook 2 Cookbook File
)

extension_dict={
	
	'text':text_set,
	'data':data_set,
	'audio':audio_set,
	'video':video_set,
	'3d_image':image_3d_set,
	'raster_image':raster_image_set,
	'vector_image':vector_image_set,
	'page_layout':page_layout_set,
	'spreadsheet':spreadsheet_set,
	'database':database_set,
	'executable':executable_set,
	'game':game_set,
	'cad':cad_set,
	'gis':gis_set,
	'web':web_set,
	'plugin':plugin_set,
	'font':font_set,
	'system':system_set,
	'settings':settings_set,
	'encoded':encoded_set,
	'compressed':compressed_set,
	'disk_image':disk_image_set,
	'developer':developer_set,
	'backup':backup_set,
	'misc':misc_set,
	}

