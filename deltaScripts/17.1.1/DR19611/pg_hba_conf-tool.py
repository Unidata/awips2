#!/usr/bin/env python

# pg_hba_conf-tool.py - Scans and modifies pg_hba.conf for certificate-based
# authentication changes.
#
# Modification History
#
# Name                Date         Comments
# ---------------------------------------------------------------------------
# David Friedman      2016-12-07   DR 19611 - Initial creation
# David Friedman      2016-12-22   DR 19637 - Handle AX DB server config.

from getopt import GetoptError, getopt
from os import chown, fdopen, remove, rename, stat
from os.path import basename, dirname, exists
from shutil import copymode
import sys
from tempfile import mkstemp
from time import gmtime, strftime
from traceback import print_exc

def pmsg(level, msg, fp=None):
    fp = fp or sys.stderr
    fp.write('%s: %s\n' % (level, msg))

def pinf(msg):
    sys.stdout.write(msg + '\n')

def perr(msg):
    pmsg('error', msg)

UPDATE_OPTION = 'update-for-ssl'
DISABLE_OPTION = 'disable-remote-non-ssl'
ENABLE_OPTION = 'enable-remote-non-ssl'
CHECK_OPTION = 'check'
MODE_OPTIONS = [UPDATE_OPTION, DISABLE_OPTION, ENABLE_OPTION, CHECK_OPTION]
# We do not care about 'local' connections
CONNECTION_TYPES = ('host', 'hostssl', 'hostnossl')
DISABLED_TAG = ' # disabled for 17.1.1'

class Line(object):
    """Represents a line in a pg_hba.conf file.

    Contains both the raw text and the parsed entry, if one is found.
    If the line is a commented-out entry, the parsed entry can still be
    present."""

    def __init__(self, text, parsed):
        self.text = text
        self.parsed = parsed
        self.commented = False
        self.tagged_as_disabled = text.find(DISABLED_TAG) >= 0
    def make_hostnossl(self):
        # Assumes already parsed as 'host' only and not something else
        self.text = self.text.replace('host', 'hostnossl', 1)
    def comment(self):
        self.text = '# ' + self.text + DISABLED_TAG
    def uncomment(self):
        if self.text[-len(DISABLED_TAG):] == DISABLED_TAG:
            self.text = self.text[:-len(DISABLED_TAG)]
        i = self.text.find('#')
        if i >= 0:
            self.text = self.text[i + 1:].lstrip()

class Entry(object):
    """A parsed pg_hba.conf entry"""

    def __init__(self):
        self.type = None
        self.database = None
        self.user = None
        self.address_fields = None
        self.auth_method = None
        self.auth_options = None
    def is_localhost(self):
        af = self.address_fields
        if af:
            if len(af) == 1:
                return af[0].lower() in ('127.0.0.1/32', '::1/128', 'localhost')
            else:
                return ([x.lower() for x in af[0:2]] in
                        (['127.0.0.1','255.255.255.255'],
                            ['::1','ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff']))
        return False
    def __eq__(self, other):
        return (self.type == other.type and self.database == other.database
            and self.user == other.user and self.address_fields == other.address_fields
            and self.auth_method == other.auth_method
            and self.auth_options == other.auth_options)

def load(path):
    """Load and parse the specified pg_hba.conf file"""
    with open(path, 'r') as f:
        return [parse_line(l.rstrip('\n')) for l in list(f)]

def split_comment(line):
    i = line.find('#')
    if i >= 0:
        return (line[:i], line[i + 1:])
    else:
        return (line, '')

def parse_line(line, in_comment=False):
    """Attempt to parse a line of a pg_hba.conf file.

    Return a Line object representing the line.  Should always succeed
    even if parsing the entry fails."""
    try:
        nc, c = split_comment(line)
        if not nc and not in_comment:
            cl = parse_line(c, True)
            l = Line(line, cl.parsed)
            l.commented = True
            return l
        else:
            p = None
            # Note: does not handle quoting
            tokens = nc.split()
            if len(tokens) >= 5:
                if tokens[0] in CONNECTION_TYPES:
                    p = Entry()
                    p.type = tokens[0]
                    p.database = tokens[1]
                    p.user = tokens[2]
                    if '/' in tokens[3]:
                        i = 4
                    else:
                        i = 5
                    p.address_fields = tokens[3:i]
                    if i < len(tokens):
                        p.auth_method = tokens[i]
                        p.auth_options = tokens[i+1:]
                        if p.auth_method.upper() == 'METHOD':
                            p = None # sample line
                        elif '"' in line:
                            raise ValueError("Cannot handle quotes in line: " + line)
            return Line(line, p)
    except Exception:
        print_exc()
        return Line(line, None)

def get_site_ip_address_ranges(hba):
    """Attempt to determine all of the site-specific IP address ranges by
    looking for all non-localhost entries for the "metadata" database.
    
    If there are no such entries, look for the entries with the database
    field set to "all"."""
    result = []
    for db in ('metadata', 'all'):
        for line in hba:
            p = line.parsed
            if not line.commented and p and p.database == db and p.type in ('host', 'hostnossl'):
                if not p.is_localhost():
                    result.append(p.address_fields)
        if result:
            break
    return result

def get_required_ssl_lines(hba):
    """Generate all of the certificate-based authentication entries that should
    be in pg_hba.conf, taking the site-specific address ranges into account."""
    site_ip_addr_ranges = get_site_ip_address_ranges(hba)
    required_lines = []
    for r in site_ip_addr_ranges:
        text = 'hostssl      all         all         %s          cert clientcert=1' % (' '.join(r),)
        required_lines.append(parse_line(text))
    required_lines += [
        parse_line('hostssl      all         all         127.0.0.1/32          cert clientcert=1'),
        parse_line('hostssl      all         all         ::1/128               cert clientcert=1')
    ]
    return required_lines

def scan_and_update(hba, update=False, report=False):
    """Scan the contents of a pg_hba.conf file, making changes for
    certificate-based authentication (if 'update' is True) and/or
    reporting the state of the file (if 'report' is True.)"""
    seen_host_lines = False
    seen_remote_non_cba = False
    ssl_required = get_required_ssl_lines(hba)
    for line in hba:
        p = line.parsed
        if p and not line.commented:
            if p.type == 'host':
                seen_host_lines = True
                if update:
                    line.make_hostnossl()
            elif p.type == 'hostnossl':
                if not p.is_localhost():
                    seen_remote_non_cba = True
            elif p.type == 'hostssl':
                if p.auth_method != 'cert' or "clientcert=1" not in p.auth_options:
                    seen_remote_non_cba = True
                for i in range(0, len(ssl_required)):
                    if p == ssl_required[i].parsed:
                        del ssl_required[i]
                        break
    if update:
        hba += ssl_required
    if report:
        if seen_host_lines:
            pinf('Has "host" lines that may be ambiguous and should be changed to either "hostnossl" or "hostssl".')
        if seen_remote_non_cba:
            pinf('Allows remote access without certificate-based authentication.')
        else:
            pinf('Does not allow remote access without certificate-based authentication.')
        if ssl_required:
            pinf("Missing the following entries for certificate-based authentication.")
            pinf('\n'.join(['  ' + line.text for line in ssl_required]))
        else:
            pinf("Has all requried certificate-based authentication entries.")
    return hba

def update_for_ssl(hba):
    return scan_and_update(hba, update=True)

def disable_remote_non_ssl(hba):
    for line in hba:
        p = line.parsed
        if p and not line.commented and p.type in ('host', 'hostnossl') and not p.is_localhost():
            line.comment()
    return hba

def enable_remote_non_ssl(hba):
    for line in hba:
        p = line.parsed
        if p and line.commented and line.tagged_as_disabled and p.type == 'hostnossl':
            line.uncomment()
    return hba

def check(hba):
    scan_and_update(hba, report=True)

def usage():
    pinf('''usage: %s [mode option] [other options]...
Mode Options:
  --check                   Check status of the file.
  --update-for-ssl          Change ambigous "host" entries to "hostnossl" and
                            add required "hostssl" entries.
  --disable-remote-non-ssl  Comment out any entries that allow remote access
                            without SSL and cetificate-based authentication.
  --enable-remote-non-ssl   Restore entries marked as disabled by the
                            the --disable-remote-no-ssl mode.

  Modes that modify the file create a backup: {file}.{date}[.{n}]

Other Options:
  -f FILE          Operate on FILE instead of /awips2/data/pg_hba.conf
  -o               Write modified contents to standard output instead of the
                   input file.
''' % (basename(sys.argv[0]),))

def write_hba(hba, f):
    for line in hba:
        f.write(line.text)
        f.write('\n')

def store(hba, path):
    """Store authentication file contents to the specified path.

    Stores to the specified path, making a backup of the original."""
    backup_sfx = strftime('%Y-%m-%d', gmtime())
    h, temp_path = mkstemp(dir=dirname(path))
    try:
        st = stat(path)
        with fdopen(h, 'w') as f:
            write_hba(hba, f)
        chown(temp_path, st.st_uid, st.st_gid)
        copymode(path, temp_path)
        i = 0
        def make_backup_path():
            s = path + '.' + backup_sfx
            if i > 0:
                s += '.' + str(i)
            return s
        backup_path = make_backup_path()
        while True:
            if not exists(backup_path):
                break
            i += 1
            backup_path = make_backup_path()
        rename(path, backup_path)
        rename(temp_path, path)
        temp_path = None
    finally:
        if temp_path:
            remove(temp_path)

func_for_mode = {
    UPDATE_OPTION: update_for_ssl,
    ENABLE_OPTION: enable_remote_non_ssl,
    DISABLE_OPTION: disable_remote_non_ssl,
    CHECK_OPTION: check
}

def main():
    mode = None
    path = '/awips2/data/pg_hba.conf'
    to_stdout = False
    try:
        opts, args = getopt(sys.argv[1:], 'f:o', MODE_OPTIONS)
    except GetoptError as e:
        perr(str(e))
        usage()
        sys.exit(1)

    for k, v in opts:
        if k == '-f':
            path = v
        elif k == '-o':
            to_stdout = True
        elif k[0:2] == '--' and k[2:] in MODE_OPTIONS:
            mode = k[2:]
    if args:
        usage()
        sys.exit(1)
    if mode:
        hba = load(path)
        hba = func_for_mode[mode](hba)
        if mode != CHECK_OPTION:
            if not to_stdout:
                store(hba, path)
            else:
                write_hba(hba, sys.stdout)
    else:
        usage()
        sys.exit(1)

if __name__ == '__main__':
    main()
