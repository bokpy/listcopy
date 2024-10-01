#!/usr/bin/python3
import argparse
import os
import re
from collections import deque
from time import sleep

import listutils as lu
import extensions as ext


DEBUGEXIT=exit

FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$',
           '\.tmp$','\.log$','\.err$','~$','/AppData/',
           '\.ini$','/NTUSER.DAT',]

parser = argparse.ArgumentParser(
prog='listfiles.py',
description='Create a list of files matching some criteria. '
            'This listing is to be used to copy this files with listcopy.py. '
            'You can edit this file further as long copying was not started. '
            'There after the bookkeeping would become out of sink. ',
epilog='Have Fun'
)

parser.add_argument('scandir',
                    help="The directory('s) to scan for files.",
                    nargs='*',
                    action='store'
                    )
#x x x x x x x x x x x x x x x x
ext_str=','.join(ext.extension_dict.keys())

parser.add_argument('-x','--extension',
                    help='select files by extension individual or one of these classes: ' + ext_str,
                    #choices=ext.extension_dict.keys(),
                    nargs='*',
                    metavar='',
                    action='store'
                    )
#M M M M M M M M M M M M M M M M M
parser.add_argument('-M','--mime-type',
                    help='select files by a list of mime types the "file -i" command knows.',
                    #choices=ext.ext_classes.keys(),
                    nargs='*',
                    metavar='',
                    action='store'
                    )
#o o o o o o o o o o o o o o o o o
parser.add_argument('-o', '--output',
                    help='File to save the listing.',
                    action='store',
                    metavar='',
                    nargs='?'
                    )
#a a a a a a a a a a a a a a a a
parser.add_argument('-a', '--append',
                    help='Append to a saved listing.',
                    action='store',
                    metavar='',
                    nargs='?'
                    )
#f f f f f f f f f f f f f f f f
parser.add_argument('-f', '--filter',
                    help=f'don\'t copy {FILTEROUT}.',
                    action='store_true'
                    )
#S S S S S S S S S S S S S S S S
parser.add_argument('-S','--skip',
                    nargs='*',
                    help='Paths that contain a part that match with one of these '
                         'regular expressions are skipped.',
					metavar='',
                    )
#m m m m m m m m m m m m m m m m
parser.add_argument('-m', '--match',
                    help='Only paths that contain a part that match with one of these '
                         'regular expressions are listed.',
                    action='store',
                    metavar='',
                    nargs='*'
                    )
#b b b b b b b b b b b b b b b b
parser.add_argument('-b', '--bigger',
                    help='Only list files bigger than this, size + [ KMG].',
                    action='store',
                    metavar='',
                    nargs=1
                    )
#s s s s s s s s s s s s s s s s
parser.add_argument('-s', '--smaller',
                    help='Only list files smaller than this, size + [ KMG].',
                    action='store',
                    metavar='',
                    nargs=1
                    )
#show-mime show-mime show-mime show-mime
parser.add_argument('--show-mime',
                    help=f'Show "general" mime types or encodings of given "general mime type" "{ext.MAGIC_FILE}" ',
                    #default=None,
                    metavar='',
                    action='store',
                    nargs='?'
                    )
args = parser.parse_args()

class FileListing:
    initiated=False
    excl_re=None
    incl_re=None
    ext_select =None
    check_size=False
    bigger =1e8
    smaller=0
    magic=None
    tumble=lu.Tumbler()
    
    def __init__(self,args,directory,output_file):
        """
        List filtered files to the output file.
        :param directory: Directory to scan for criteria matching files.
        :param output_file: Open fd to write the results to.
        """
        self.catalog=directory
        self.outp   =output_file
        self.args   = args
        if not self.initiated:
            self.make_filters()
            self.initiated=True
        self.current_entry=None
        self.string_path=None
        self.write(lu.DATA_BEGIN_MARKER)
        self.write(directory)
        self.walk()
        self.write(lu.DATA_END_MARKER)
        
    def write(self,data=None):
        if not data:
            data=self.string_path
        data+='\n'
        try:
            self.outp.write(data)
            self.tumble.step()
            
        except OSError as e:
            print(f'Writing: "{data}" failed.')
            print(f'errno {e.errno} "{e.strerror}"')
            exit(e.errno)
    
    def make_filters(self):
        """
        compose and compile regular expressions to filter path's in or out
        :return: side effects self.(excl_re,incl_re,ext_re,bigger,smaller)
        """
        
        sa=self.args
        excl_str=skip_str=filter_str=incl_str=ext_str=match_str=''
        incl_list=[]
        
        # construct the regular expression that excludes paths
        if sa.filter: filter_str="|".join(lu.FILTEROUT)
        if sa.skip:   skip_str  ="|".join(sa.skip)
        if filter_str and skip_str: # concatenate if there a two
            excl_str=skip_str + '|' + filter_str
        else: # one the real one or the empty string goes in excl_str
            excl_str=skip_str + filter_str
        if excl_str:
            self.excl_re=re.compile(excl_str)
        
        # construct extension checker
        if sa.extension:
            self.ext_select=ext.SelectOnExtension(sa.extension)
            # self.ext_select.show()
            # DEBUGEXIT(0)
        
        # construct the regular expression that filters for paths with a matching substring
        if sa.match:
            match_str="|".join(sa.match)
            self.incl_re=re.compile(match_str,flags=re.IGNORECASE)
        
        # selection on mime type
        if sa.mime_type:
            self.magic=ext.MagicMime(sa.mime_type)
            
        # if size matters
        if sa.bigger:
            self.bigger=lu.kilo_mega(sa.bigger)
            self.check_size=True
  
        if sa.smaller:
            self.smaller=lu.kilo_mega(sa.smaller)
            self.check_size=True
        
    
    def filter(self)->bool:
        """
        test the entry <DirEntry> against the selection criteria.
        :return: True if all tests are passed with success.
        """
        cur=self.current_entry
        if self.check_size:
            size=cur.stat().st_size
            if size < self.bigger or size > self.smaller:
                return False
        if isinstance(cur.path,bytes):
            try:
                path = cur.path.decode(encoding ='utf-8', errors = 'ignore')
            except UnicodeDecodeError as e:
                print(f'UnicodeDecodeError {e}')
                print(f'May be "unicode_broom.py" can solve the problem.')
                print(f'Bee careful with your data always backup in time.')
                exit(1)
        else:
            path=cur.path
           
        if self.excl_re:
            if self.excl_re.search(path):
                #DEBUGPRINT(f'excl_re fired: "{path}"')
                return False
   
        if self.ext_select:
            if not self.ext_select.check(path):
                #DEBUGPRINT(f'not ext_re fired: "{path}"')
                return False
        
        if self.magic:
            if not self.magic.check(path):
                return False
            
        if self.incl_re:
            if not self.incl_re.search(path):
                #DEBUGPRINT(f'not incl_re fired: "{path}"')
                return False
        self.string_path=path
        return True
    
    def walk(self):
        dir_stack=deque()
        push=dir_stack.append
        pop=dir_stack.pop
        push(bytes(self.catalog, 'ascii'))
        def empty():
            return len(dir_stack) == 0
        
        while not empty():
            cur_dir=pop()
            for entry in os.scandir(cur_dir):
                if entry.is_symlink():
                    continue
                if entry.is_dir():
                    #DEBUGPRINT(f'Push: "{entry.path}"')
                    push(entry.path)
                    continue
                self.current_entry=entry
                if self.filter():
                    self.write() # writes self.string_path

 
def main() -> None:
    
    if args.show_mime:
        low= args.show_mime.lower()
        if low =='general':
            ext.show_mime_types()
        else:
            ext.show_mime_types( encoding=low)
    elif not args.scandir:
        parser.print_help()
        return
     
    output_file=None
    if args.append:
        output_file=args.append
        print (f'Append listing to: "{output_file}"')
        open_mode='a'
    elif args.output:
        output_file=args.output
        print (f'Write listing to: "{output_file}"')
        open_mode='w'
    
    if output_file:
        output_file=os.path.expanduser(output_file)
    for catalogue in args.scandir:
        catalogue=os.path.expanduser(catalogue)
        print(f'Start scanning: "{catalogue}"')
        if open_mode=='w':
            print (f'Write to: "{output_file}"')
        else:
           print (f'Append to: "{output_file}')
        with open(output_file,open_mode) as f:
            FileListing(args,catalogue,f)
        open_mode='a'
            
if __name__ == '__main__':
    main()