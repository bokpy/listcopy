#!/usr/bin/python3
import argparse
import os
import sys
from os import scandir
import re
from collections import deque
import time

#from time import sleep
from listutils import Tumbler,kilo_mega,DATA_BEGIN_MARKER,DATA_END_MARKER
import extensions as ext

def silent(*args,**kwargs):
    pass

verbose=silent
#verbose=print

logfile = None

DEBUGPRINT=print
DEBUGEXIT=exit
FILTEROUT=['/Cookies/','/Microsoft/','/Windows/','/Cache','#.*#$','\.lnk$','\.tmp$','\.log$','\.err$','~$','^~','/AppData/','\.ini$','/NTUSER.DAT','\.thumbnails','^{.*}$','NTUSER\.']

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
ext_str=', '.join(ext.extension_dict.keys())

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
                    action='store',
                    )
#m m m m m m m m m m m m m m m m
parser.add_argument('-m', '--match',
                    help='Only paths that contain a part that match with one of these '
                         'regular expressions are listed.',
                    action='store',
                    metavar='regular expressions',
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
#wholesale wholesale wholesale wholesale wholesale wholesale wholesale
parser.add_argument('-w','--wholesale',
                    help='List everything like "ls -R" (overrides all other filters).',
                    action='store_true'
                    )
#show-mime show-mime show-mime show-mime
parser.add_argument('--show-mime',
                    help=f'Show "general" mime types or encodings of given "general mime type" in "{ext.MAGIC_FILE}" ',
                    #default=None,
                    metavar='general',
                    action='store',
                    nargs='?'
                    )
#verbose verbose verbose verbose verbose verbose verbose verbose
parser.add_argument('-v', '--verbose',
                    help='Verbose.',
                    action='store_true',
                    )
#unsuccessful unsuccessful unsuccessful unsuccessful unsuccessful unsuccessful
parser.add_argument('-U', '--unsuccessful',
                    help='Unsuccessful files,log file.',
                    action='store',
                    metavar='',
                    nargs=1
                    )

def log_error(filepath,error):
    global logfile
    if not logfile:
        return
    logfile.write(f'"{filepath}" # {error}\n')
    verbose(f'ERROR: "{filepath}" {error}.')

def unicode_exception(badline):
    return badline.encode('ascii', 'replace').decode('ascii')

def scandir_iterator(directory):
    count = 0
    dir_stack = deque()
    dir_stack.append(directory)
    while dir_stack:
        scan = dir_stack.pop()
        try:
            files = os.scandir(scan)
        except FileNotFoundError as e:  # [Errno 2] No such file or directory
            verbose(f'FileNotFoundError: {e}')
            continue
        except PermissionError as e:
            log_error(scan,e)
            continue

        for file in files:
            save_path=unicode_exception(file.path)
            if file.is_symlink():
                verbose(f'symlink: "{save_path}"')
                continue
            if file.is_dir():
                try:
                    verbose(f'  dir: "{save_path}"')
                except UnicodeError as e:
                    print('scandir_iterator: {e}')
                dir_stack.append(save_path)
                continue
            verbose(f'\r{count:05}',end=' -> ')
            #yield save_path
            #DEBUGPRINT
            yield file.path
            count += 1

class FileListing:
    initiated  = False
    excl_re    = None
    incl_re    = None
    ext_select = None
    check_size = False
    bigger     = 1e8
    smaller    = 0
    magic      = None
    tumble     = Tumbler()
    
    def __init__(self,args,directory,output_file):
        """
        List filtered files to the output file.
        :param directory: Directory to scan for criteria matching files.
        :param output_file: Open fd to write the results to.
        """
        print(f'Scan: "{directory}" ',end='')
        self.catalog = directory
        self.outp    = output_file
        self.args    = args
        if not self.initiated:
            self.make_filters()
            self.initiated=True
        self.current_entry=None
        self.string_path=None
        self.count = 0
        self.write(' '.join(sys.argv))
        self.write(DATA_BEGIN_MARKER)
        self.write(directory)
        self.walk()
        self.write(DATA_END_MARKER)
        self.write(' '.join(sys.argv))
        print(f'\nDone scanning files written to "{output_file.name}"')
        
    def write(self,data):
        global verbose
        if isinstance(data,str):
            data+='\n'
        # if not data:
        #     data=self.string_path
        byte_data=data.encode('UTF-8',errors='ignore')
        try:
            self.outp.write(byte_data)
        except OSError as e:
            print(f'Listing: "{unicode_exception(data)}" failed.')
            print(f'errno {e.errno} "{e.strerror}"')
            exit(e.errno)
        if verbose != print:
            self.tumble.step()
        else:
            try:
                print( f'Listed: "{data}"')
            except UnicodeEncodeError as e:
                # 'utf-8' codec can't encode character '\udcab' in position 68: surrogates not allowed
                # print(f'Writing: "{data}" failed.')
                print(f'UnicodeEncodeError {e}')
                print(f'In file: "{unicode_exception(data)}".')

    def make_filters(self):
        """
        compose and compile regular expressions to filter path's in or out
        :return: side effects self.(excl_re,incl_re,ext_re,bigger,smaller)
        """

        excl_str=skip_str=filter_str=incl_str=ext_str=match_str=''
        incl_list=[]
        
        # construct the regular expression that excludes paths
        if self.args.filter: filter_str="|".join(FILTEROUT)
        if self.args.skip:   skip_str  ="|".join(self.args.skip)

        if filter_str and skip_str: # concatenate if there a two
            excl_str=skip_str + '|' + filter_str
        else: # one the real one or the empty string goes in excl_str
            excl_str=skip_str + filter_str
        if excl_str:
            self.excl_re=re.compile(excl_str,flags=re.IGNORECASE)
            # DEBUGPRINT(f'{ext_str}')
            # DEBUGPRINT(self.excl_re)
            # input('make filters')
        
        # construct extension checker
        if self.args.extension:
            self.ext_select=ext.SelectOnExtension(self.args.extension)
            # self.ext_select.show()
            # DEBUGEXIT(0)
        
        # construct the regular expression that filters for paths with a matching substring
        if self.args.match:
            match_str="|".join(self.args.match)
            self.incl_re=re.compile(match_str,flags=re.IGNORECASE)
        
        # selection on mime type
        if self.args.mime_type:
            self.magic=ext.MagicMime(self.args.mime_type)
            
        # if size matters
        if self.args.bigger:
            self.bigger=kilo_mega(self.args.bigger)
            self.check_size=True
  
        if self.args.smaller:
            self.smaller=kilo_mega(self.args.smaller)
            self.check_size=True

    def filter(self,filepath)->bool:
        """
        test the entry <DirEntry> against the selection criteria.
        :return: True if all tests are passed with success.
        """
        if self.check_size:
            size=os.stat(filepath).st_size
            if size < self.bigger or size > self.smaller:
                return False

        path=filepath
        if isinstance(filepath,bytes):
            try:
                path = filepath.decode(encoding ='utf-8', errors = 'ignore')
            except UnicodeDecodeError as e:
                print(f'UnicodeDecodeError {e}')
                print(f'Maybe "unicode_broom.py" can solve the problem.')
                print(f'Bee careful with your data always backup in time.')
                exit(1)

        if self.excl_re:
            if self.excl_re.findall(path):
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
        DEBUGTIMEOUT= time.time()+5*60
        for file in scandir_iterator(self.catalog):
            if time.time() > DEBUGTIMEOUT:
                DEBUGEXIT('\nFileListing,walk timed out.')
            try:
                #DEBUGPRINT(f'{self.args.wholesale=} or {self.filter(file)=}')
                if self.args.wholesale or self.filter(file):
                    self.write(file) # writes self.string_path
                continue
                verbose(f'rejected: "{file}"')
            except OSError as e:
                if logfile:
                    logfile.write(f'"{file}" # {e}')
                verbose(f'ERROR: "{file}" {e} ')

def main() -> None:
    args = parser.parse_args()
    print (args)
    if args.unsuccessful :
        global logfile
        try:
           logfile=open(args.unsuccessful[0],'w')
        except OSError as e:
            exit(f'Open "{args.unsuccessful[0]}" {e}')

    global verbose
    if args.verbose:
        verbose=print
        verbose('verbose output set.')
    #DEBUGEXIT('DEBUG')
    #DEBUGPRINT(f'{args.show_mime=} {args.scandir}')
    if args.show_mime:
        low= args.show_mime.lower()
        if low =='general':
            ext.show_mime_types()
        else:
            ext.show_mime_types( encoding=low)
    elif not args.scandir:
        parser.print_help()
        print (f'Arguments red: {args}')
        return
     
    output_file=None
    if args.append:
        output_file=args.append
        verbose(f'Append: ',end='')
        open_mode='ab'
    elif args.output:
        output_file=args.output
        verbose(f'Write: ',end='')
        open_mode='wb'

    if output_file:
        output_file=os.path.expanduser(output_file)
    for catalogue in args.scandir:
        catalogue=os.path.expanduser(catalogue)
        #print(f'Start scanning: "{catalogue}" ',end='')
        verbose(f' "{output_file}"')
        with open(output_file,open_mode) as f:
            FileListing(args,catalogue,f)
        open_mode='ab'
    print (args)

if __name__ == '__main__':

    main()