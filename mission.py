#!/usr/bin/python3

def main() -> None:
	pass
"""#!/usr/bin/p

consignment={

# values set in listcopy.py main
"dest_path"    : "Base path to store the copy of the files"
"language"     : "preferred language not implemented (yet?)"
"ok_file"      : "file path to the file to keep track of the progress"
"bad_file"     : "file to list files that failed to copy with the error in comment"
"gps_info"     : "file to cache data retrieved from OpenStreetMap"
"dry_run"      : "Set only if "args.dry_run" is set."
"throttle"     : if args.throttle set to args.throttle."
"store_labels" : if args.labels set to an empty start set()

# values set in listcopy.py target_fs_properties
"FsMaxFileSize"   : "maximum file size for the fs type "dest_path" lives on."
"FsBlockSize"     : "block size for the fs type "dest_path" lives on."

# values set in listcopy.py process_filelisting 
"bad_file_handle" : "file handle open for writing failed files."

# values set in filelistiter.py InputFileIterator
"last_file_accessed" : " last file that was processed so probably not compleet."

}

mission={

# initiated by filelistiter.py InputFileIterator.file_reaper
    "source_full_path"   : " full path to source file", 
    "source_scanned_dir" : " root path which was scanned by "listfiles.py",
    "completed"          : " file count  file_reaper did yield",
    "last_file_accessed" : 
   	
 # initiated by listcopy.py process_filelisting(consignment) -> split_source_path(mission)
    "source_dir"
	"extension"
	"basename"
	"stem_name"
	
 # initiated by listcopy.py process_filelisting(consignment) 
    "verbose"            : " copy of consignment["verbose"] = args.verbose" ,3
    "dest_base_dir"      : " Base to copy the files to = consignment["dest_path"]."
    
# values set in PathSeeker.compose_path -> TreeOfKnowledge.reset
    "source_tail_path"   : "filepath after "source_scanned_dir""
    "Error"              : "An error message set if "exiftool" reports an error."
    "mime_general"       : "General mime type reported by "exiftool"."
    "extension"          : "extension from the "source_full_path"."
    "FileTypeExtension"  : "extension as reported by "exiftool"."
    
# values set in PathSeeker.compose_path -> TreeOfKnowledge.check_extension
    "target_path"        : "Path above "dest_base_dir" to the copy target." 
    
# initiated by listcopy.py process_filelisting(consignment)     
    "target_full_path"   : os.path.join("target_dir","target_path")  
    
    "target_dir"         : "Target directory"
     
}

"/home/bob/usb/Media/foto/sjoukje/DCIM/100NIKON/DSCN3248.JPG"

"""

if __name__ == '__main__':
	main()
