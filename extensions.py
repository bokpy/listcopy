#!/bin/python3
#Extendions found at https://fileinfo.com/filetypes/common
#the lists where made with the help of Opera Aria A.I.

import re
import subprocess
#import exiftool as xftl ,did not work for me. Will use subprocess
import json
from collections import deque

# noinspection SpellCheckingInspection
VIDEO_EXT = [
    "\.STR",          # YouTube Livestream Recording
    "\.TTML",         # Timed Text Markup Language Subtitles File
    "\.RXR",          # RecordXR Recording
    "\.SWF",          # Shockwave Flash Movie
    "\.AEP",          # After Effects Project
    "\.MKV",          # Matroska Video
    "\.PZ",           # Panzoid Video Project
    "\.PLOT",         # Plotagon Studio Project
    "\.KINE",         # KineMaster Project File
    "\.PRPROJ",       # Premiere Pro Project
    "\.PIV",          # Pivot Animator Animation
    "\.SFD",          # Sofdec Dreamcast Movie
    "\.PSV",          # Pluralsight Video File
    "\.AV1",          # AV1 Video
    "\.PLOTDOC",      # Plotagon Project File
    "\.ANM",          # DeluxePaint Animation
    "\.DREAM",        # Procreate Dreams Project
    "\.OBJECTION",    # Objection.lol Project
    "\.NTP",          # Natron Project File
    "\.VEG",          # VEGAS Video Project
    "\.MSDVD",        # Windows DVD Maker Project File
    "\.KMPROJECT",    # KineMaster Project
    "\.WLMP",         # Windows Live Movie Maker Project
    "\.DRP",          # DaVinci Resolve Project
    "\.AEC",          # Cinema 4D After Effects Composition
    "\.DCR",          # Digital Court Recorder Video File
    "\.AMC",          # AMC Video File
    "\.BIK",          # Bink Video File
    "\.MSWMM",        # Windows Movie Maker Project
    "\.PAC",          # PAC Subtitles File
    "\.MSE",          # MediaShow Slideshow Project File
    "\.WEBM",         # WebM Video
    "\.KDENLIVE",     # Kdenlive Project
    "\.DIR",          # Adobe Director Movie
    "\.SER",          # Astronomical Capture Video File
    "\.CINE",         # Phantom Digital Video File
    "\.SCM",          # ScreenCam Screen Recording
    "\.SUB",          # MicroDVD Subtitle File
    "\.FBR",          # FlashBack Recording
    "\.FCP",          # Final Cut Project
    "\.EVO",          # SeeVogh Player Video Recording
    "\.VPJ",          # VideoPad Video Editor Project File
    "\.WPL",          # Windows Media Player Playlist
    "\.DCR",          # Liberty Video Recording File
    "\.MP4",          # MPEG-4 Video
    "\.RMVB",         # RealMedia Variable Bit Rate File
    "\.VOB",          # DVD Video Object File
    "\.FLC",          # FLIC Animation
    "\.CLPI",         # Blu-ray Clip Information File
    "\.SBT",          # SBT Subtitle File
    "\.SRT",          # SubRip Subtitle File
    "\.DMX",          # Source Filmmaker Project
    "\.M4S",          # MPEG-DASH Video Segment
    "\.IFO",          # DVD-Video Disc Information File
    "\.INP",          # Sony Camcorder Image Management File
    "\.VP6",          # TrueMotion VP6 Video File
    "\.3GP",          # 3GPP Multimedia File
    "\.DMSM",         # VideoWave Movie Project File
    "\.MXF",          # Material Exchange Format
    "\.VSP",          # VideoStudio Project
    "\.CPVC",         # Adobe Captivate Video Composition
    "\.META",         # RealPlayer Metafile
    "\.CAMPROJ",      # Camtasia Studio Project
    "\.MVD",          # Movie Studio Movie
    "\.IVR",          # Internet Video Recording
    "\.VTT",          # Web Video Text Tracks File
    "\.TRP",          # HD Video Transport Stream
    "\.MPEG",         # MPEG Video
    "\.M4U",          # MPEG-4 Playlist
    "\.MJPG",         # Motion JPEG Video
    "\.SMV",          # VideoLink Mail Video File
    "\.WMMP",         # Windows Movie Maker Project File
    "\.REC",          # Topfield PVR Recording
    "\.VIDEO",        # aTube Catcher Video File
    "\.AEPX",         # After Effects XML Project
    "\.MJ2",          # Motion JPEG 2000 Video
    "\.SWI",          # SWiSH Project File
    "\.MP4V",         # MPEG-4 Video
    "\.MGV",          # PSP Video File
    "\.AMX",          # Adobe Motion Exchange File
    "\.SCREENFLOW",   # ScreenFlow Document
    "\.MANI",         # Mine-imator Project File
    "\.D3V",          # Datel Video File
    "\.RMS",          # Secure Real Media File
    "\.BK2",          # Bink Video 2 File
    "\.VC1",          # VC-1 Video File
    "\.MEPX",         # Movavi Video Project
    "\.PSH",          # Photodex Slide Show
    "\.ASF",          # Advanced Systems Format File
    "\.FLV",          # Flash Video
    "\.PMF",          # PlayStation Portable Movie
    "\.DV",           # Digital Video File
    "\.LSAV",         # Xiaomi Gallery Hidden Video
    "\.TVSHOW",       # mimoLive Show
    "\.ZM2",          # ZSNES Movie #2 File
    "\.ARCUT",        # Prelude Rough Cut File
    "\.SIV",          # Silicon Imaging Video File
    "\.RCUT",         # Webinaria Recording Cut
    "\.ALE",          # Avid Log Exchange File
    "\.G2M",          # GoToMeeting Recording File
    "\.DPA"           # DrawPlus Animation File
]
# noinspection SpellCheckingInspection
AUDIO_EXT = [
    "\.SEQUENCE",
    "\.SVP",
    "\.FUR",
    "\.FTM",
    "\.ABC",
    "\.WEBA",
    "\.MTM",
    "\.EFS",
    "\.UST",
    "\.XRNS",
    "\.FLP",
    "\.TG",
    "\.COPY",
    "\.MP3",
    "\.VSQ",
    "\.EC3",
    "\.TOC",
    "\.SDS",
    "\.FLAC",
    "\.SF2",
    "\.ASD",
    "\.L",
    "\.DCF",
    "\.WPROJ",
    "\.STY",
    "\.MUI",
    "\.AUP",
    "\.MKA",
    "\.MIDI",
    "\.FSC",
    "\.CDO",
    "\.FEV",
    "\.VPW",
    "\.SAF",
    "\.H5S",
    "\.ITLS",
    "\.PHY",
    "\.M4R",
    "\.CGRP",
    "\.OMG",
    "\.CWS",
    "\.XMU",
    "\.RIP",
    "\.KT3",
    "\.SNGX",
    "\.MMLP",
    "\.MTI",
    "\.SC2",
    "\.CWB",
    "\.ACP",
    "\.XFS",
    "\.DCT",
    "\.IGP",
    "\.VOXAL",
    "\.DSM",
    "\.GSF",
    "\.MINIGSF",
    "\.AKP",
    "\.DFF",
    "\.PNA",
    "\.NKI",
    "\.VDJ",
    "\.ALS",
    "\.AMXD",
    "\.PTXT",
    "\.DMSE",
    "\.GP",
    "\.SLP",
    "\.RAD",
    "\.ARIA",
    "\.SFK",
    "\.MID",
    "\.BAND",
    "\.4MP",
    "\.GSM",
    "\.GP5",
    "\.RX2",
    "\.MSCZ",
    "\.GBS",
    "\.APL",
    "\.BUN",
    "\.PEK",
    "\.ANG",
    "\.RMJ",
    "\.OGG",
    "\.ABM",
    "\.ACM",
    "\.G726",
    "\.VSQX",
    "\.WAV",
    "\.MMPZ",
    "\.SGP",
    "\.AFC",
    "\.ALC",
    "\.LOGICX",
    "\.SFPACK",
    "\.PLA",
    "\.VLC",
    "\.STM",
    "\.ACD-ZIP",
    "\.OVW",
    "\.PSM",
    "\.S3M",
    "\.QCP",
    "\.EMX"
]
# noinspection SpellCheckingInspection
D3_EXT = [
    "\.BBMODEL",      # Blockbench 3D Model
    "\.HIPNC",        # Houdini Apprentice File
    "\.GH",           # Grasshopper Binary Definition
    "\.CRZ",          # Compressed Poser Character Rigging File
    "\.MESH",         # Godot Engine 3D Mesh File
    "\.VRM",          # Virtual Reality Model
    "\.IAVATAR",      # iClone Avatar
    "\.REAL",         # Aero Experience
    "\.DDP",          # DreamPlan Home Design Project
    "\.PART",         # PartDesigner Part
    "\.MD5ANIM",      # id Tech 4 Model Animation File
    "\.IRR",          # Irrlicht 3D Scene
    "\.C4D",          # Cinema 4D Model
    "\.DUF",          # DAZ User File
    "\.FSH",          # Fragment Shader File
    "\.MCSG",         # MagicaCSG 3D Model
    "\.DFF",          # RenderWare Model File
    "\.BLEND",        # Blender 3D Data File
    "\.MAKERBOT",     # MakerBot Print File
    "\.M3D",          # 3D Model File
    "\.IV",           # Open Inventor Scene Graph File
    "\.CMDB",         # Ansys CFD Mesh
    "\.ZT",           # Mental Ray Image Depth File
    "\.ATM",          # Vue Atmospheres File
    "\.MC5",          # Poser 5 Material File
    "\.DSV",          # DAZ Studio UV Mapping File
    "\.PHY",          # 3ds Max Physique File
    "\.THING",        # MakerBot Thing File
    "\.PMX",          # MikuMikuDance Model File
    "\.CFG",          # Cal3D Model Configuration File
    "\.SMD",          # Valve Studiomdl Data File
    "\.MDL",          # Warcraft 3 3D Model Text File
    "\.XAF",          # 3ds Max XML Animation File
    "\.MIX",          # 3ds Max Motion Mixer File
    "\.FX",           # Direct3D Effects File
    "\.LXF",          # LEGO Digital Designer Model File
    "\.X",            # DirectX Model File
    "\.NM",           # Space Engine Nebula Model
    "\.MU",           # Kerbal Space Program Mesh File
    "\.P3D",          # Peak3D 3D Graphics File
    "\.AN8",          # Anim8or File
    "\.MTZ",          # Compressed MetaStream Scene File
    "\.MDX",          # Warcraft 3 Model File
    "\.3DS",          # 3D Studio Scene
    "\.PSA",          # Unreal Engine Skeletal Animation File
    "\.VOX",          # Voxlap Voxel Model File
    "\.FLT",          # OpenFlight Scene Description File
    "\.CSO",          # Compiled Shader Object File
    "\.M3D",          # DIALux 3D Object File
    "\.X3D",          # Xara3D Project
    "\.3D2",          # Stereo CAD-3D 2.0 Image File
    "\.3MF",          # 3D Manufacturing File
    "\.OBP",          # Bryce Object File
    "\.MESH",         # 3D Mesh Model
    "\.WFT",          # GTA 4 Car Model File
    "\.ATL",          # Artlantis 3D Scene File
    "\.KFM",          # Gamebryo 3D Model File
    "\.IRRMESH",      # Irrlicht Static Mesh File
    "\.N3D",          # Nuclear 3D File
    "\.VPD",          # MikuMikuDance Vocaloid Pose Data File
    "\.3DXML",        # Dassault Systemes 3D XML File
    "\.P4D",          # Pix4D Project
    "\.GLTF",         # GL Transmission Format File
    "\.MD5MESH",      # id Tech 4 3D Mesh File
    "\.PRM",          # Re-Volt Model File
    "\.USD",          # Universal Scene Description Format
    "\.IVE",          # OpenSceneGraph Binary File
    "\.ANIMSET",      # FaceFX Animation Set File
    "\.MD5CAMERA",    # id Tech 4 Model Camera File
    "\.P3L",          # Adobe Photoshop Light Preset File
    "\.E57",          # LIDAR Point Cloud Data File
    "\.BR7",          # Bryce 7 Scene File
    "\.T3D",          # Swift 3D Document
    "\.TILT",         # Tilt Brush Sketch
    "\.3D4",          # Stereo CAD-3D 2.0 Image File
    "\.SHAPR",        # Shapr3D Model
    "\.HDZ",          # Compressed Poser Hand Pose File
    "\.GRS",          # Gravity Sketch Sketch File
    "\.GHX",          # Grasshopper XML Definition
    "\.GMF",          # Leadwerks Game Model File
    "\.LLM",          # Linden Lab Mesh File
    "\.CCP",          # CopperCube JavaScript File
    "\.BIP",          # Character Studio Biped File
    "\.DAE",          # Digital Asset Exchange File
    "\.PPZ",          # Compressed Poser Prop File
    "\.STEL",         # Stella Polyhedron
    "\.PP2",          # Poser Prop File
    "\.TRACE",        # TRACES 3D Scene
    "\.AMF",          # Additive Manufacturing File
    "\.FACEFX",       # FaceFX Actor File
    "\.MHM",          # MakeHuman Model
    "\.V3D",          # Visual3D.NET Data File
    "\.TME",          # Bryce Time File
    "\.MA",           # Maya ASCII Scene
    "\.REALITY",      # Reality 3D Scene Format
    "\.SH3D",         # Sweet Home 3D Design
    "\.XMF",          # Cal3D XML Mesh File
    "\.ANIM",         # Maya Animation File
    "\.DS",           # DAZ Studio 1/2 Script
    "\.IFC",          # Industry Foundation Classes File
    "\.PSK"           # Unreal Engine Skeletal Model File
]
# noinspection SpellCheckingInspection
RASTER_IMAGE_EXT = [
    "\.BIF",          # Ventana Whole Slide Image
    "\.JXL",          # JPEG XL Image
    "\.SPRITE2",      # Scratch 2.0 Sprite File
    "\.QOI",          # Quite OK Image Format
    "\.XPM",          # X11 Pixmap Graphic
    "\.ICON",         # Icon Image
    "\.AFPHOTO",      # Affinity Photo Document
    "\.ASE",          # Aseprite Sprite File
    "\.PSDC",         # Adobe Photoshop Cloud Document
    "\.PXD",          # Pixelmator Pro Image
    "\.LRPREVIEW",    # Adobe Lightroom Preview File
    "\.8CI",          # TI-84 Plus C Pic Vars Image
    "\.SUMO",         # Sumo Paint Image
    "\.GIF",          # Graphical Interchange Format File
    "\.MNR",          # AutoCAD Menu Resource File
    "\.PSD",          # Adobe Photoshop Document
    "\.SPRITE3",      # Scratch 3.0 Sprite File
    "\.PTEX",         # Ptex Texture File
    "\.SNAGX",        # Snagit 2022 Capture
    "\.TBN",          # Kodi Thumbnail Image
    "\.PLP",          # PixelLab Project
    "\.AVATAR",       # Google Talk Avatar File
    "\.BPG",          # Better Portable Graphics Image
    "\.PNG",          # Portable Network Graphic
    "\.YSP",          # BYOB Sprite File
    "\.SPRITE",       # Scratch Sprite File
    "\.TGA",          # Targa Graphic
    "\.FLIF",         # Free Lossless Image Format File
    "\.TPF",          # TexMod Package File
    "\.KRA",          # Krita Image Document
    "\.DDS",          # DirectDraw Surface Image
    "\.DIB",          # Device-Independent Bitmap Image
    "\.SAI",          # PaintTool SAI Image
    "\.SPR",          # Half-Life Sprite
    "\.PDN",          # paint.net Image
    "\.PISKEL",       # Piskel Sprite
    "\.JPEG",         # JPEG Image
    "\.HDR",          # High Dynamic Range Image File
    "\.WIC",          # Java Wavelet Image
    "\.PAM",          # Portable Arbitrary Map Image
    "\.IPV",          # ibis Paint Artwork
    "\.VICAR",        # VICAR Image File
    "\.SKITCH",       # Skitch Image
    "\.PSP",          # Pixel Studio Project
    "\.PZP",          # PhotoSuite Project File
    "\.SKTZ",         # Sony Sketch Drawing
    "\.LINEA",        # Linea Sketch Drawing
    "\.OC4",          # openCanvas 4 Event File
    "\.JLS",          # JPEG-LS Image
    "\.NWM",          # Sony NWM Display Screen File
    "\.APS",          # Greeting Card Studio Project File
    "\.IPICK",        # iPick Football Image
    "\.CT",           # Scitex Continuous Tone Image
    "\.PPP",          # PhotoPad Project
    "\.OPLC",         # Nokia Operator Logo File
    "\.SIX",          # Sixel Image
    "\.SLD",          # AutoCAD Slide File
    "\.CLIP",         # Clip Studio Paint Illustration
    "\.PM",           # Unix XV Graphic File
    "\.JPG",          # JPEG Image
    "\.PCX",          # Paintbrush Bitmap Image File
    "\.HEIF",         # High Efficiency Image Format
    "\.WEBP",         # WebP Image
    "\.JPS",          # Stereo JPEG Image
    "\.OTA",          # OTA Bitmap Image
    "\.LIP",          # Clip Studio Paint File
    "\.TFC",          # Unreal Engine 3 Texture File Cache
    "\.ITC2",         # iTunes Cover Flow Data File 2
    "\.POV",          # POV-Ray Raytracing Format
    "\.PWP",          # PhotoWorks Image File
    "\.MNG",          # Multiple-Image Network Graphic
    "\.XCF",          # GIMP Image File
    "\.WBZ",          # Webshots Download Picture File
    "\.FITS",         # Flexible Image Transport System File
    "\.73I",          # TI-73 Screenshot File
    "\.PSDX",         # Photoshop Touch Document
    "\.WBC",          # Webshots Collection File
    "\.LZP",          # LazPaint Image
    "\.EXR",          # OpenEXR Image
    "\.DJVU",         # DjVu Image
    "\.USERTILE-MS",  # Windows 8 User Tile File
    "\.PPF",          # Picture Publisher Image File
    "\.CPC",          # CPC Compressed Image File
    "\.CDC",          # AutoCAD DesignCenter Preview Cache File
    "\.TIFF",         # Tagged Image File Format
    "\.PMG",          # Adobe Photoshop Photomerge Panoramic Composition File
    "\.OZJ",          # MU Online Image File
    "\.ACCOUNTPICTURE-MS", # Windows 8 Account Picture File
    "\.BMP",          # Bitmap Image
    "\.CAN",          # Canon Navigator Fax Document
    "\.PBM",          # Portable Bitmap Image
    "\.RGF",          # LEGO MINDSTORMS EV3 Robot Graphics File
    "\.2BP",          # Pocket PC Bitmap Image File
    "\.STEX",         # Godot Engine 3 StreamTexture File
    "\.SNAG",         # Snagit for Windows Capture
    "\.JPC",          # JPEG 2000 Code Stream File
    "\.MDP",          # FireAlpaca Image
    "\.ECW"           # Enhanced Compression Wavelet Image
]
# noinspection SpellCheckingInspection
VECTOR_IMAGE_EXT = [
    "\.SVG",          # Scalable Vector Graphic
    "\.SVGZ",         # Compressed SVG File
    "\.SHAPES",       # Pixelmator Pro Shapes
    "\.VSTM",         # Visio Macro-Enabled Drawing Template
    "\.AI",           # Adobe Illustrator Artwork
    "\.VSDX",         # Microsoft Visio Drawing
    "\.GVDESIGN",     # Gravit Designer File
    "\.CDR",          # CorelDRAW File
    "\.EP",           # Pencil Document
    "\.CMX",          # Corel Presentation Exchange Image
    "\.APM",          # Aldus Placeable Metafile Image
    "\.FCM",          # CanvasWorkspace Fabric Cutting Design
    "\.FH8",          # FreeHand 8 Drawing File
    "\.EPSF",         # Encapsulated PostScript Format File
    "\.SLDDRT",       # SolidWorks Drawing Sheet
    "\.AFDESIGN",     # Affinity Design Document
    "\.VSTX",         # Microsoft Visio Drawing Template
    "\.DPR",          # Digital InterPlot File
    "\.EPS",          # Encapsulated PostScript File
    "\.DRW",          # Drawing File
    "\.FH10",         # FreeHand 10 Drawing File
    "\.CSY",          # Canvas Symbol File
    "\.STD",          # Apache OpenOffice Drawing Template
    "\.PS",           # PostScript File
    "\.WMF",          # Windows Metafile
    "\.PFD",          # Micrografx Optima! File
    "\.FH9",          # FreeHand 9 Drawing File
    "\.CDMZ",         # ConceptDraw MINDMAP Document
    "\.CDD",          # ConceptDraw DIAGRAM Document (Legacy)
    "\.ODG",          # OpenDocument Graphic File
    "\.LMK",          # Sothink Logo Maker Image
    "\.POBJ",         # Photo Pos Pro Vector Object File
    "\.FT9",          # FreeHand 9 Drawing Template
    "\.CDRAPP",       # CorelDRAW.app Image File
    "\.PSID",         # PostScript Image Data File
    "\.GLOX",         # Microsoft Office SmartArt Graphics Layout File
    "\.FH4",          # FreeHand 4 Drawing File
    "\.FH7",          # FreeHand 7 Drawing File
    "\.FXG",          # Flash XML Graphics File
    "\.VSD",          # Microsoft Visio Drawing (Legacy)
    "\.IGX",          # iGrafx Document
    "\.DRW",          # Corel Drawing File
    "\.DPP",          # DrawPlus Drawing File
    "\.EMZ",          # Windows Compressed Enhanced Metafile
    "\.INK",          # InkML Image
    "\.XAR",          # Xara Xtreme Drawing
    "\.NODES",        # Stick Nodes 2D Object
    "\.PLT",          # HPGL Plot File
    "\.ECS5",         # Easy Cut Studio Version 5 Project
    "\.CVD",          # Canvas X Drawing
    "\.MVG",          # Magick Vector Graphics File
    "\.FHD",          # FreeHand Drawing File
    "\.SSK",          # SmartSketch 95 Document
    "\.AIC",          # Adobe Illustrator Cloud Document
    "\.XMMAT",        # MindManager XML Map Template
    "\.TEX.EMZ",      # Street Fighter IV Texture File
    "\.DRAWIT",       # DrawIt Drawing
    "\.SK",           # Skencil Drawing
    "\.DRAWIO",       # diagrams.net Diagram File
    "\.VSDM",         # Visio Macro-Enabled Drawing
    "\.CVX",          # Canvas Drawing
    "\.OTG",          # OpenDocument Graphic Template
    "\.AC6",          # ArtCut 6 Document
    "\.PMG",          # PageMaker Group File
    "\.SVM",          # StarView Metafile
    "\.PEN",          # Logitech io2 Drawing
    "\.AIT",          # Adobe Illustrator Template
    "\.PLT",          # AutoCAD Plotter Document
    "\.PD",           # FlexiSIGN 5 Plotter Document
    "\.PIXIL",        # Pixilart Project
    "\.FT8",          # FreeHand 8 Template
    "\.PUPPET",       # Adobe Character Animator Puppet
    "\.WPG",          # WordPerfect Graphic
    "\.SCV",          # ScanVec CASmate Sign File
    "\.HPGL",         # HP Graphics Language Plotter File
    "\.RDL",          # MicroStation Redline File
    "\.CDX",          # CorelDRAW Compressed File
    "\.ESC",          # EasySignCut Pro Project
    "\.HPG",          # HPGL Plot File
    "\.PICT",         # Picture File
    "\.VECTORNATOR",  # Vectornator Drawing
    "\.CDTX",         # ConceptDraw DIAGRAM XML Template
    "\.HVIF",         # Haiku Vector Icon File
    "\.FIG",          # Xfig Drawing
    "\.GSD",          # Graphtec Vector Graphics File
    "\.DIA",          # Dia Diagram File
    "\.MP",           # LaTeX MetaPost File
    "\.ASY",          # Asymptote Module
    "\.SKETCH",       # Sketch Drawing
    "\.VML",          # Vector Markup Language File
    "\.MGC",          # Microsoft Clip Organizer Media Catalog
    "\.SMF",          # Serif Metafile
    "\.CLARIFY",      # Clarify Document
    "\.FH11"          # FreeHand 11 Drawing File
]
# noinspection SpellCheckingInspection
TEXT_EXT = [
    "\.SMF",         # StarMath Formula File
    "\.STY",         # LaTeX Style
    "\.MPD",         # MPEG-DASH Media Presentation Description
    "\.GSITE",       # Google Sites Shortcut
    "\.DOC",         # Microsoft Word Document (Legacy)
    "\.LST",         # Data List
    "\.LTXD",        # Light Text Editor Document
    "\.MAN",         # Unix Manual
    "\.DOTM",        # Microsoft Word Macro-Enabled Document Template
    "\.ADOC",        # AsciiDoc Document
    "\.DSC",         # Text Description File
    "\.QBL",         # QuickBooks License File
    "\.UPD",         # Program Update Information
    "\.FPT",         # FoxPro Table Memo
    "\.SAM",         # LMHOSTS Sample File
    "\.EMBED",       # Embed Notes Note
    "\.DOTX",        # Microsoft Word Template
    "\.SAVE",        # Nano Temporary Save File
    "\.WTT",         # Write! Document
    "\.LTX",         # LaTeX Document
    "\.FCF",         # Final Draft Converter File
    "\.B",           # Brainfuck Source Code File
    "\.ORG",         # Emacs Org Text Document
    "\.DOCX",        # Microsoft Word Document
    "\.DIZ",         # Description in Zip File
    "\.MNT",         # FoxPro Menu Memo
    "\.LXFML",       # LEGO Digital Designer XML File
    "\.ODM",         # OpenDocument Master Document
    "\.GFORM",       # Google Forms Shortcut
    "\.STORY",       # Storyist Document
    "\.ME",          # Readme Text File
    "\.EIO",         # Yozo Office File
    "\.LUE",         # Norton LiveUpdate Log File
    "\.FOUNTAIN",    # Fountain Script File
    "\.ANS",         # ANSI Text File
    "\.TMDX",        # TextMaker Document
    "\.GPD",         # Generic Printer Description File
    "\.README",      # Readme File
    "\.APT",         # Almost Plain Text File
    "\.VNT",         # Mobile Phone vNote File
    "\.FBL",         # CADfix Command Level Log File
    "\.AWW",         # Ability Write Document
    "\.TEX",         # LaTeX Source Document
    "\.LOG",         # Log File
    "\.FADEIN.TEMPLATE", # Fade In Template
    "\.BF",          # Brainf*ck Source Code File
    "\.KLG",         # KOFIA Log
    "\.IPF",         # OS/2 Help File
    "\.CEC",         # Studio C Alpha Upgrade File
    "\.PWDPL",       # Password Pad Lite Document
    "\.JARVIS",      # Jarvis Subscriber File
    "\.FODT",        # OpenDocument Flat XML Document
    "\.TXT",         # Plain Text File
    "\.RPT",         # Generic Report
    "\.GDOC",        # Google Docs Shortcut
    "\.NFO",         # Warez Information File
    "\.LIS",         # SQR Output File
    "\.GSD",         # General Station Description File
    "\.AIM",         # AIMMS ASCII Model File
    "\.TLB",         # VAX Text Library
    "\.DROPBOX",     # Dropbox Shared Folder Tracker
    "\.ASC",         # Autodesk ASCII Export File
    "\.LST",         # FoxPro Documenting Wizard List
    "\.RFT",         # Revisable Form Text Document
    "\.SCM",         # Schema File
    "\.SDM",         # StarOffice Mail Message
    "\.OPEICO",      # Opeico Text File
    "\.DXB",         # Duxbury Braille File
    "\.IPYNB",       # Jupyter Notebook
    "\.ASC",         # ASCII Text File
    "\.TEXT",        # Plain Text File
    "\.DOCM",        # Microsoft Word Macro-enabled Document
    "\.WPD",         # WordPerfect Document
    "\.TMVX",        # TextMaker Document Template
    "\.EMULECOLLECTION", # eMule Data File
    "\.WPS",         # Microsoft Works Word Processor Document
    "\.OTT",         # OpenDocument Document Template
    "\.HS",          # Java HelpSet File
    "\.BIB",         # Bibliography Document
    "\._DOCX",       # Renamed Microsoft Word Document
    "\.1ST",         # Readme File
    "\.MD5.TXT",     # Message Digest 5 Hash File
    "\.RTF",         # Rich Text Format File
    "\.BIB",         # BibTeX Bibliography Database
    "\.ERR",         # Error Log File
    "\.BDR",         # Exchange Non-Delivery Report Body File
    "\.DTEX",        # DataTex Document
    "\.TM",          # TeXmacs Document
    "\.WPW",         # WordPerfect Works Document
    "\.GSCRIPT",     # Google Apps Script Shortcut
    "\.FLUID",       # Loop Component
    "\.BWD",         # BanglaWord Document
    "\.KNT",         # KeyNote Note File
    "\.ETF",         # ENIGMA Transportable File
    "\.DM",          # BYOND Dream Maker Code
    "\.ATY",         # Association Type Placeholder
    "\.JNP",         # Java Web Start File
    "\.XY",          # XYWrite Document
    "\.LUF",         # Lipikar Uniform Format File
    "\.TFRPROJ",     # theFrame Project File
    "\.RAD",         # Radar ViewPoint Radar Data
    "\.RIS",         # Research Information Systems Citation File
    "\.ODT"          # OpenDocument Text Document
]
# noinspection SpellCheckingInspection
CAD_FILE_EXT = [
    "\.BAK",      # AutoCAD Drawing Backup
    "\.AXM",      # FormIt Sketch
    "\.CIR",      # Micro-Cap Schematic
    "\.G",        # FlashForge G-Code File
    "\.BDC",      # West Point Bridge Designer Design File
    "\.CIRCUIT",  # KTechlab Circuit Design File
    "\.PSM",      # Solid Edge Sheet Metal File
    "\.IBA",      # Lectra Clothing Design Pieces File
    "\.SIM",      # SimLab Composer Scene
    "\.DLV",      # CATIA 4 Export File
    "\.SMB",      # Autodesk Shape Manager Binary File
    "\.EPF",      # EAGLE Project
    "\.CPA",      # CADSTAR PCB Archive File
    "\.DSNX",     # RoadEng Location Design Document
    "\.AFS",      # STAAD.foundation Project File
    "\.TCT",      # TurboCAD Drawing Template
    "\.MCX",      # MICRO CADAM-X/6000 Model Data File
    "\.FCSTD1",   # FreeCAD Backup Document
    "\.PLUSH",    # Plushify Project
    "\.MP12",     # Multisim 12 Project
    "\.OPT",      # Opterecenja File
    "\.GSM",      # Graphic Description Language File
    "\.EDF",      # Edificius Project
    "\.DB1",      # Tekla Structures Model File
    "\.FPC",      # FreePCB Printed Circuit Board Layout
    "\.MSM",      # Multisim Circuit Design File
    "\.MC9",      # Mastercam 9 Geometry File
    "\.RTD",      # Robot Structural Analysis Design File
    "\.PSV",      # Pipe System Viewer File
    "\.JVSG",     # Video Surveillance Project
    "\.LCF",      # Archicad Library Container File
    "\.TERX",     # RoadEng Terrain Design Document
    "\.MS14",     # Multisim 14 Circuit Design File
    "\.CAD",      # BobCAD-CAM File
    "\.CATPRODUCT",# CATIA V5 Assembly File
    "\.JOB",      # MetaCAM Nest Job File
    "\.MODEL",    # CATIA 3D Model
    "\.GCODE",    # G-code 3D Printer File
    "\.JT",       # JT Open CAD File
    "\.DC3",      # DesignCAD 3D ASCII Drawing
    "\.CTB",      # Chitubox Sliced 3D Model
    "\.LOGICLY",  # Logicly Circuit
    "\.RSG",      # RaySupreme Graph
    "\.LI3D",     # Live Interior 3D Document
    "\.CIB",      # Luminaire Data File
    "\.3DL",      # Sierra LandDesigner 3D File
    "\.VET",      # Lectra Cutter's Must File
    "\.CAD",      # Autodesk QuickCAD File
    "\.CBDDLP",   # Chitubox Sliced 3D Model
    "\.XV3",      # Lattice XVL Geometry File
    "\.IDE",      # Inventor iFeature
    "\.MTO",      # FastCAM MTO Text File
    "\.AFD",      # Alphacam Flame Drawing
    "\.PC7",      # PowerCADD 7 Drawing File
    "\.DWT",      # AutoCAD Drawing Template
    "\.FZP",      # Fritzing XML Part Description
    "\.CYP",      # Home Design Project
    "\.NC",       # DSTV Numerical Control File
    "\.ASY",      # LTspice Symbol File
    "\.ADI",      # AutoCAD Device-Independent Binary Plotter File
    "\.PAT",      # AutoCAD Hatch Pattern File
    "\.123DX",    # 123D Design Model File
    "\.ICD",      # IronCAD 2D Drawing File
    "\.DES",      # Pro/DESKTOP CAD File
    "\.SAT",      # ACIS SAT 3D Model
    "\.FZB",      # Fritzing Bin File
    "\.PSS",      # AutoCAD Plot Stamp Settings File
    "\.SKF",      # AutoSketch Drawing
    "\.MS13",     # Multisim 13 Circuit Design File
    "\.DST",      # AutoCAD Sheet Set
    "\.GDS",      # Graphic Data System File
    "\.SCH",      # gEDA Schematic File
    "\.EDN",      # EDIF Implementation Netlist File
    "\.JVSGZ",    # Compressed Video Surveillance Project
    "\.MHS",      # Xilinx XPS Hardware Specification File
    "\.PWT",      # AutoCAD Publish To Web Template
    "\.SPT",      # SpeedTree Tree Data File
    "\.SLDASM",   # SolidWorks Assembly
    "\.FCW",      # FastCAD Windows Drawing
    "\.SLDPRT",   # SolidWorks Part
    "\.BLK",      # AutoCAD Block Template File
    "\.SCAD",     # OpenSCAD Script
    "\.CF2",      # Common File Format File
    "\.PHJ",      # PhCNC Project File
    "\.BCD",      # RealView Debugger Board Chip Definition File
    "\.DRU",      # EAGLE Design Rules
    "\.DC2",      # DesignCAD 2D ASCII Drawing
    "\.NWF",      # Navisworks File Set
    "\.DWFX",     # Design Web Format XPS File
    "\.PLN",      # Archicad Solo Project
    "\.NC",       # Mastercam Numerical Control File
    "\.IGS",      # IGES Drawing
    "\.BDL",      # CoCreate Bundle File
    "\.LIN",      # AutoCAD Linetype File
    "\.123D",     # Autodesk 123D Drawing
    "\.STL",      # Stereolithography File
    "\.DSN",      # OrCAD Design File
	"\.FCStd"
]

ext_classes={'video':VIDEO_EXT,'audio':AUDIO_EXT,'3d-model':D3_EXT,
             'raster-image':RASTER_IMAGE_EXT,'vector-image':VECTOR_IMAGE_EXT,
             'text':TEXT_EXT,'cad':CAD_FILE_EXT}

count_spaces=re.compile(r'^\s*')
find_colon=re.compile(r'^\:*')
def count_and_split(string):
    # count the leading spaces of string
    # split string in striped key,value paars 
    global count_spaces
    match = count_spaces.match(string)
    count=len(match.group(0))
    match = find_colon.match(string)
    
    colon = 0
    match = re.search(r":", string)
    if match:
        colon = match.start()
        
    key=string[:colon]
    value=string[colon+1:]
    #print(f'>{key}<>{value}<')
    return count,key.strip(),value.strip()

''' GIMP
(define (extract-image-metadata filename)
  (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-get-active-drawable image))))
    (if (not (null? image))
        (begin
          (let ((metadata (car (gimp-image-get-metadata image))))
            (file-png-save RUN-NONINTERACTIVE image drawable "output.png" "output.png")
            (gimp-message (string-append "Metadata: " metadata)))
          (gimp-image-delete image)))))
gimp -i -b '(extract-image-metadata "path/to/your/image.jpg")' -b '(gimp-quit 0)'
'''
def error2(program,site='Try a search'):
    print(f'You need to install "{program}"')
    print(f'from: "{site}".')
    exit(1)
    
def do_exiftool(picture_file):
    try:
        meta = subprocess.run(['exiftool','-j','-all',picture_file],
                              capture_output=True,text=True)
    except OSError as e:
        print(f'do_exiftool Got {e.errno} "{e.strerror}')
        if e.errno==2:
            error2('exiftool','https://exiftool.org')
        return None
    try:
        meta_data=json.loads(meta.stdout)
    except json.decoder.JSONDecodeError as e:
        print (f'ERROR DATA START\n\n{meta.stdout}\n\nERROR DATA END')
        print (f'{e} "{picture_file}"')
        return None
        
    return meta_data
        
def do_convert(picture_file):
    try:
        meta = subprocess.run(['convert',picture_file,'json:'],
                              capture_output=True,text=True)
    except OSError as e:
        print(f'do_convert Got {e.errno} "{e.strerror}')
        if e.errno==2:
            error2("convert","https://imagemagick.org/magick")
        return None
    if meta.stdout:
        return json.loads(meta.stdout)
    return None
    
def do_magick(picture_file):
    try:
        meta = subprocess.run(['magick','identify','-verbose',picture_file],
                              capture_output=True,text=True)
    except OSError as e:
        print(f'do_magick Got {e.errno} "{e.strerror}')
        if e.errno==2:
            error2("magick",'https://imagemagick.org/')
        
    if not meta.stdout:
        return None
    lines=meta.stdout.split('\n')
    # for line_string in lines:
    #     print(line_string)
    # return
    # prev_level=0
    # key_stack=deque()
    # branche={}
    exif={}
    for line_string in lines:
        level,key,value=count_and_split(line_string)
        if value == '':
            continue
        exif[key]=value
    return exif
 
def get_picture_meta_data(picture_file,program = 'magick'):
    """Try to read neta data of a picture and
    on success return a diprogram = 'magick'ct with the data."""
    
    if program == 'magick':
        return do_magick(picture_file)
    
    if program == 'convert':
        return do_convert(picture_file)
    
    if program == 'exiftool':
        return do_exiftool(picture_file)
        
''''JPEG	.jpg, .jpeg	EXIF, IPTC, XMP
PNG	.png	tEXt, iTXt, zTXt (limited)
TIFF	.tif, .tiff	EXIF, IPTC
BMP	.bmp	Limited metadata
GIF	.gif	Limited metadata
HEIF	.heif, .heic	EXIF, XMP
RAW	Various (e.g., .raw, .cr2, .nef)	EXIF, proprietary formats
https://imagemagick.org/script/download.php#linux'''

def string_extensions(reg_list)->str:
    """forms a string from a list from extensions."""
    no_slash=[x[2:] for x in reg_list]
    glued="|".join(no_slash)
    return glued
    
def create_regular_expression(reg_str:str,re_flags=re.IGNORECASE):
    """create_regular_expression from a extension list
    parameter re_flags: a combination with operator '|' of re.ASCII re.DEBUG
    re.DOTALL re.IGNORECASE re.LOCALE  re.MULTILINE re.TEMPLATE  re.UNICODE
    re.VERBOSE"""
    return re.compile(r'\.(' + reg_str + r')$',flags=re_flags)

if __name__ == '__main__':
    # reg_str=string_extensions(VIDEO_EXT)
    # print(reg_str)
    # reg=create_regular_expression(reg_str)
    # print(reg)
    print(get_picture_meta_data("/home/bob/ik.jpg"))

   
