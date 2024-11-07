#!/usr/bin/python3
#from extensionsets import extension_dict
syntax_text=f'''
Fore every class of files categorized by a comma separated list of "mime types" and/or "extension(s)" 
a substitution path can be defined by a list of labels.

This labels are retrieved if possible in order by:
 
 a call to "exiftool" (labels are formatted to lowercase space and / replaced by an underscore.

for audio: if needed followed with a "https://api.acoustid.org/v2/lookup" request.
           "brainzmusic.py" expects to find a key in "~/.local/listcopy/AcoustID.key"
           maybe not needed but easy to get from "acoustid.org".

for images with gps data: geological label data is retrieved from "Overpass" "OpenStreetMap".

The subdirectories of the original path can be copied.
Positive numbers indicate a subdirectory above the source directory.
Negative numbers indicate a subdirectory below the filename (-1 is the filename).
Zero full path above the source directory.

syntax: <path>       = <filetype>[,<filetype>]:/<tag>[/<name>];
        <join>       = <+{{str}}+>
        <switch>     = (<alternative 1>|<alternative 2>[|...|<alternative n>)]
        <filetype>   = <mime>|<extension>[,<filetype>] lowercase = mime, uppercase = extension
        <tag>        = <label|subdir|literal>{{string}}
        <tag>        = <tag>[<join><tag>]
        <tag>        = <tag>/<tag>
        <name>       = name:<tag>

Example: image,video:/label{{artist}}/label{{album }}+" year "+ label{{year}}/name:label{{ title }}
         image:/label{{addr:city}}/label{{addr:street}} +" "+ label{{addr:housenumber'}}/subdir{{-1}}
         audio/flac:/literal:{{flac music}}/label{{album}}/name:label{{ title }}
         MP3,WAV,OGG,AAC:/literal:{{" music "}}/label{{year}}/name:label{{ title }}
         default:/name:subdir{{-1}}
                 
<filetype> for "extension" look in "extensionsets.py"
           for "mime" see "listfiles.py --show-mime general" 
               "listfiles --show-mime general_mime_type"
           default fits all.
           
<tag> for "label" all labels the current program can retrieve.
Order is important so put the most specific in front.
'''

def main() -> None:
	pass


if __name__ == '__main__':
	main()
