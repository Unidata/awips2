#!/awips2/python/bin/python

# Task #2033903
#
# Instructions for installs:
#
# Run this script on a localization server (dv3 or dv4) with EDEX running.
# Check output for ERROR messages indicating that localization files need
# manual updating, and notify the site of those files.
#
#
# Description:
#
# The Apache Velocity Tools 3.1 upgrade removed support for ListTool, which
# was passed into velocity templates as $list. This script runs against velocity
# (*.vm) files and removes/replaces uses of $list, so that they will work with
# this upgrade.
#
# This documentation of the removed ListTool indicates the methods that it
# provided:
# https://velocity.apache.org/tools/2.0/apidocs/org/apache/velocity/tools/generic/ListTool.html
#
# For $list method calls, such as ${list.contains(${bullets}, "wind")}, the
# method is just called directly on the list object instead:
# $bullets.contains("wind)
#
# This handles the 'size', 'isEmpty', 'get', 'contains', and 'equals' methods.
# Other methods are not converted (the only other methods should be 'set',
# 'isArray', and 'isList').
#
# For macro definitions and calls that have $list as an argument, the argument
# is simply removed.
#
# By default, this script runs against all non-base level *.vm files in
# /awips2/edex/data/utility. It outputs error messages if it finds $list uses
# that it doesn't know how to remove/replace.
#
# The -p/--path arg may be specified to change the directory/file to update,
# and the -r/--revert option may be included to restore the unconverted backup
# versions of the files, instead of converting them.
#

import os, re, argparse, shutil, sys
import datetime
import time
import traceback
from getpass import getuser

from dynamicserialize.dstypes.com.raytheon.uf.common.localization import FileUpdatedMessage
from dynamicserialize.dstypes.com.raytheon.uf.common.localization import LocalizationContext, LocalizationLevel, LocalizationType
from ufpy import ThriftClient

# yyyymmdd
formatted_date = datetime.date.today().strftime('%Y%m%d')

# Regex for separator between args in macro definitions and calls
arg_sep_regex = r'(?:\s*,\s*|\s+)'

def get_ref_regex(capture):
    '''
    Get a regex for matching a reference to a variable, property, or method
    call.

    Examples:
        $x
        ${x}
        $x.y.z
        ${x.y.z}
        $x.y.z()

    @param capture: False to capture nothing, True to capture the reference
        without dollar sign or brackets (such as "x.y.z()" from "${x.y.z()}".
        It will be captured to either group 1 or group 2, with the other being
        empty, so "\1\2" can be used to get the reference in a replacement
        regex.
    '''

    # Majority of regex for matching a reference to a variable, property or
    # method. Includes everything but the dollar sign and optional outer
    # brackets.
    ref_regex_majority = (
        r'\w+'                  # match var name (x)
        r'(?:\.\w+'             # start group to match any number of properties or method names (x.y.z)
        r'(?:\(\s*\))?'         # optional parentheses for method names (x.y.z())
        r')*'                   # close property/method group
    )
    if capture:
        ref_regex_majority = r'(' + ref_regex_majority + r')'

    # OR together the two possible versions of the reference: without brackets
    # (captured as group 1) and with brackets (captured as group 2).
    return r'(?:\$' + ref_regex_majority + r'|\$\{' + ref_regex_majority + r'\})'

def update_no_arg_list_methods(text):
    '''
    Replace $list method calls that take no args (other than the list reference
    itself). This currently supports the 'size' and 'isEmpty' methods.

    Examples:
        ${list.size( ${x.y} )}  ->  $x.y.size()
        $list.isEmpty($a.b())   ->  $a.b().isEmpty()

    @param text: VM file text to update method calls in
    @return: updated text with $list method calls replaced
    '''

    for method_name in ['size', 'isEmpty']:
        # Majority of the regex, just missing the leading $ and optional outer brackets {}
        # Example: list.size( ${x.y} )
        regex_majority = r'list\.' + method_name + r'\(\s*' + get_ref_regex(capture=True) + r'\s*\)'
        # regex without outer brackets
        regex = r'\$' + regex_majority
        # regex with outer brackets
        regex2 = r'\$\{' + regex_majority + r'\}'

        # Replace with calling method directly on list reference: $x.y.size()
        regex_replacement = r'$\1\2.' + method_name + r'()'
        text = re.sub(regex, regex_replacement, text)
        text = re.sub(regex2, regex_replacement, text)
    return text

def update_single_arg_list_methods(text):
    '''
    Replace $list method calls that take one arg (in addition to the list
    reference itself). This currently supports the 'get', 'contains', and
    'equals' methods.

    Examples:
        ${list.get( ${x.y}, 0 )}              ->  $x.y.get(0)
        $list.contains($a.b(), "test")        ->  $a.b().contains("test")
        $list.equals($listObj, $otherListObj) ->  $listObj.equals($otherListObj)

    @param text: VM file text to update method calls in
    @return: updated text with $list method calls replaced
    '''
    for method_name in ['get', 'contains', 'equals']:
        # Regex portion for matching up until the comma before the arg: list.get($x.y,
        # Also excludes leading dollar sign and optional outer brackets.
        regex_start_to_arg_sep = r'list\.' + method_name + r'\(\s*' + get_ref_regex(capture=True) + r'\s*,\s*'

        # Replace cases without outer brackets
        regex_without_brackets = r'\$' + regex_start_to_arg_sep
        # Match up until method call's closing parenthesis, handling one nested
        # method call for things like this: $list.get($listObj, $listObj.size() - 1)})
        regex_without_brackets += (
            r'([^()]+'                  # capture everything up until method call's closing parenthesis
            r'(?:\([^()]*\)[^()]*)?'    # handle optional nested method call and then try to match up until method's closing parenthesis again
            r')\)'                      # close capture group and match list method's closing parenthesis
        )
        text = re.sub(regex_without_brackets, r'$\1\2.' + method_name + r'(\3)', text)

        # Handle one nested pair of brackets for things like this:
        # #if(${list.contains($dupcounties, "${area.state_zone}")})
        regex_second_arg_to_bracket_close = (
            r'([^{}]+'                  # capture everything up until list operation's closing bracket
            r'(?:\{[^{}]*\}[^{}]*)?'    # handle optional nested brackets and then try to match up until list's closing bracket again
            r')\}'                      # close capture group and match list's closing bracket
        )

        # Preserve brackets if the characters immediately following the close
        # bracket could incorrectly appear to be property names without the
        # brackets. Example: ${list.get($x, 0).name}xyz.
        regex_preserve_brackets = r'\$\{' + regex_start_to_arg_sep + regex_second_arg_to_bracket_close + r'(?=\.?\w)'
        text = re.sub(regex_preserve_brackets, r'${\1\2.' + method_name + r'(\3}', text)

        # Remove brackets if they aren't needed (we could always preserve
        # brackets, but this matches manual changes that have already been
        # done, so testing is easier with this)
        regex_remove_brackets = r'\$\{' + regex_start_to_arg_sep + regex_second_arg_to_bracket_close
        text = re.sub(regex_remove_brackets, r'$\1\2.' + method_name + r'(\3', text)
    return text

def replace_macro_def_or_call_match(match_obj):
    '''
    Function to pass to re.sub() for macro definitions and calls. This
    replaces the matched macro definitions and calls with versions that have
    the $list arg removed.

    @param match_obj: match object for macro definition/call regex
    @return replacement text for match
    '''
    start = match_obj.group(1)
    pre_args = match_obj.group(2)
    post_args = match_obj.group(3)
    end = match_obj.group(4)

    # If there are args on both sides of the list arg, keep the separator
    # just before it. Otherwise, remove both separators.

    # Remove the separator right after the list arg
    post_args = re.sub(r'^' + arg_sep_regex, '', post_args)
    if not pre_args or not post_args:
        # Remove the separator just before the list arg
        pre_args = re.sub(arg_sep_regex + r'$', '', pre_args)
    return start + pre_args + post_args + end

def update_macro_definitions(text):
    '''
    Remove $list arg from macro definitions.

    Example macro definitions:
        #macro(inserttorwatches $watches $list $secondtimezone $dateUtil $timeFormat $watchPhrase)
        #macro(printcoords $coordinates $list)
    or, with commas
        #macro(handleClosestPoints $list, $points1, $points2, $stormType, $nearText, $nearThreshold, $overText, $overThreshold, $units, $useSecondRef)

    @param text: VM file text to update macro definitions in
    @return: updated text with $list removed from macro definitions
    '''
    # Macro directive and macro name, such as '#{macro} (macroname, '
    macro_start_regex = r'(?:\#macro|#\{macro\})\s*\(\s*\w+' + arg_sep_regex
    # Brackets don't work for args in macro definitions
    macro_arg_regex = r'\$\w+'
    # Any number of args preceding the list arg
    pre_macro_args_regex = r'(?:' + macro_arg_regex + arg_sep_regex + ')*'
    # Any number of args following the list arg
    post_macro_args_regex = r'(?:' + arg_sep_regex + macro_arg_regex + ')*'
    list_arg_regex = r'\$list'
    macro_end_regex = r'\s*\)'

    # Concatenate the pieces, capturing each piece as a group (except for the
    # list arg)
    macro_regex = (r'(' + macro_start_regex + r')('+ pre_macro_args_regex
                 + r')' + list_arg_regex + r'(' + post_macro_args_regex
                 + r')(' + macro_end_regex + r')')

    text = re.sub(macro_regex, replace_macro_def_or_call_match, text)
    return text

def update_macro_calls(text):
    '''
    Remove $list from macro calls.

    Example macro calls:
        #inserttorwatches(${watches}, ${list}, ${secondtimezone}, ${dateUtil}, ${timeFormat}, ${watchPhrase})
        #printcoords(${areaPoly} ${list})
        #locationsList("Locations impacted include..." ${otherLead} 0 ${cityList} ${otherPoints} ${areas} ${dateUtil} ${timeFormat} 0)

    @param text: VM file text to update macro calls in
    @return: updated text with $list removed from macro calls
    '''

    # Macro calls can include @ sign like "#@macroname(..." - probably not used
    # but included just in case
    macro_name_regex = r'@?\w+'
    call_start_regex = r'#(?:' + macro_name_regex + r'|\{' + macro_name_regex + r'\})' + r'\s*\(\s*'

    # All sorts of args can be passed into macro calls, so try to match all of them 

    # Reference to a variable, variable property, or variable method
    ref_arg_regex = get_ref_regex(capture=False)
    # String arg - double-quoted or single-quoted
    str_arg_regex = r'"[^"]*"|' + r"'[^']*'"
    # 4 or 4. or .4 or 4.3
    num_arg_regex = r'\d+|\d+\.\d*|\d*\.\d+'
    # Range or array arg, such as [1, 2, 3]. We intentionally don't handle
    # nested arrays, since they are hard to parse and unlikely to be used.
    range_array_arg_regex = '\[[^[\]]*\]'
    bool_arg_regex = r'true|false'
    # OR all the arg types together
    call_arg_regex = r'(?:' + r'|'.join((ref_arg_regex, str_arg_regex, num_arg_regex, range_array_arg_regex, bool_arg_regex)) + r')'

    # Any number of args preceding the list arg
    pre_call_args_regex = r'(?:' + call_arg_regex + arg_sep_regex + r')*'
    # Any number of args following the list arg
    post_call_args_regex = r'(?:' + arg_sep_regex + call_arg_regex + r')*'
    # List arg, with or without brackets
    list_arg_regex = r'(?:\$list|\$\{list\})'
    call_end_regex = r'\s*\)'

    # Concatenate the pieces, capturing each piece as a group
    call_regex = (r'(' + call_start_regex + r')(' + pre_call_args_regex + r')'
                + list_arg_regex + r'(' + post_call_args_regex + r')('
                + call_end_regex + r')')

    text = re.sub(call_regex, replace_macro_def_or_call_match, text)
    return text

def delete_md5(filepath):
    '''
    Delete the md5 file associated with the given filepath, if one exists.

    @param filepath: full path to the file to delete the md5 file for
    '''
    md5_filepath = filepath + '.md5'
    if os.path.exists(md5_filepath):
        os.remove(md5_filepath)

def update_file(dirpath, filename):
    '''
    Function to run a file through a converter that removes references to the
    $list variable from the text. A backup copy (filename.vm.yyyymmdd) of the
    input file will be created, and the file will then be updated in place and
    its associated md5 file will be deleted.

    Error messages will be output if the file has any $list references that we
    don't know how to update.

    @param dirpath: path to directory containing the VM file
    @param filename: name of VM file to update
    @return True if file was successfully updated, False if it has unhandled
        $list references
    '''
    filepath = os.path.join(dirpath, filename)

    # Read the contents of the file
    with open(filepath, "r") as f:
        input_script = f.read()
        print(f"Reading file '{filepath}'")

    updated_script = input_script

    # Replace no-arg list methods (size/isEmpty)
    updated_script = update_no_arg_list_methods(updated_script)
    # Replace single-arg list methods (get/contains/equals)
    updated_script = update_single_arg_list_methods(updated_script)
    # Remove $list args from macro definitions
    updated_script = update_macro_definitions(updated_script)
    # Remove $list args from macro calls
    updated_script = update_macro_calls(updated_script)

    # Check if any $list references remain that we failed to handle. Check for
    # '$list' or ${list' uses that are not followed by more variable name
    # characters.
    unhandled_list_regex = r'\$\{?list[^\w]'
    if re.search(unhandled_list_regex, updated_script):
        print("ERROR: Unhandled '$list' or '${list' references in '"
              + filepath + "' must be manually updated")
        success = False
    else:
        success = True

    # Check if anything was converted, if not, move to the next file.
    if input_script != updated_script:
        # Create a backup of the file first, using the format filename.vm.yyyymmdd
        backup_file = filepath + "." + formatted_date
        if os.path.exists(backup_file):
            print(f"WARN: Backup file already exists and will not be updated: '{backup_file}'")
        else:
            shutil.copy2(filepath, backup_file)
            st = os.stat(filepath)
            os.chown(backup_file, st.st_uid, st.st_gid)

        # Save the converted script to the original file
        with open(filepath, 'w') as f:
            f.write(updated_script)
            print(f"***Converted file '{filepath}'")

        # Remove the corresponding MD5 file. It will be automatically regenerated
        delete_md5(filepath)

    else:
        print("No changes made.")

    return success

def revert_file(dirpath, filename):
    '''
    Function to revert previously made changes to converted files. This
    function moves the most recently backed up copy of the file to the original
    filepath.

    @param dirpath: path to directory containing the VM file
    @param filename: name of VM file to revert
    '''
    filepath = os.path.join(dirpath, filename)

    # Does the vm file have a backup copy?
    # Look for the file using the format filename.vm.yyyymmdd
    file_backup_regex = filename + r'\.\d{8}'
    matching_files = [file for file in os.listdir(dirpath)
                      if re.fullmatch(file_backup_regex, file)]

    if not matching_files:
        print(f"No backup file found for '{filepath}'")
        return

    # grab the file with the latest date suffix
    backup_file = max(matching_files)
    backup_path = os.path.join(dirpath, backup_file)

    print(f"Reverting to backup file '{backup_path}'")

    st = os.stat(filepath)
    shutil.move(backup_path, filepath)
    os.chown(filepath, st.st_uid, st.st_gid)
    delete_md5(filepath)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-r", "--revert", action="store_true", help="Option to revert changes that were previously made with this tool.")
    parser.add_argument("-p", "--path", help="Option to specifically select the file or directory to run the tool on.")
    args = parser.parse_args()

    # Verify user is awips or root
    if getuser() not in [ 'awips', 'root' ]:
        print("This script must be run as user awips or root.")
        sys.exit(1)

    if args.path:
        if not os.path.exists(args.path):
            print(f"ERROR: '{args.path}' is not a valid file or directory.")
            sys.exit(1)
        search_path = args.path
    else:
        # Default to the utility dir
        search_path = '/awips2/edex/data/utility/'

    conversion_success = True
    all_files = []

    # Is the path a file or a directory?
    if os.path.isdir(search_path):
        # Recursively update all non-base level VM files in the directory
        for dirpath, dirnames, filenames in os.walk(search_path):
            for filename in filenames:
                if filename.endswith(".vm"):
                    filepath = os.path.join(dirpath, filename)
                    # Don't modify base-level files
                    if not re.match(r'/awips2/edex/data/utility/[^/]+/base/', filepath):
                        all_files.append(filepath)
                        if args.revert:
                            revert_file(dirpath, filename)
                        else:
                            success = update_file(dirpath, filename)
                            conversion_success = conversion_success and success
    else:
        all_files.append(search_path)
        # It's a file
        if args.revert:
            revert_file(os.path.dirname(search_path), os.path.basename(search_path))
        else:
            conversion_success = update_file(os.path.dirname(search_path), os.path.basename(search_path))

    # For all localization files, send a FileUpdatedMessage to edex so that path
    # observers (specifically Backup Service) are triggered for it. We just do it
    # for all files so that if this script is accidentally run with edex down first,
    # re-running it will still send these messages.
    thrift = ThriftClient.ThriftClient('localhost')
    loc_path_regex = re.compile('/awips2/edex/data/utility/([^/]+)/([^/]+)/([^/]+)/(.*)')
    backup_service_success = True
    for filepath in all_files:
        loc_path_match = loc_path_regex.match(filepath)
        if loc_path_match:
            loc_type = loc_path_match.group(1)
            loc_level = loc_path_match.group(2)
            context_name = loc_path_match.group(3)
            loc_rel_path = loc_path_match.group(4)

            context = LocalizationContext()
            context.setLocalizationType(LocalizationType(loc_type))
            context.setLocalizationLevel(LocalizationLevel(loc_level))
            context.setContextName(context_name)

            fum = FileUpdatedMessage()
            fum.setTimeStamp(round(time.time() * 1000))
            fum.setFileName(loc_rel_path)
            fum.setChangeType('UPDATED')
            fum.setContext(context)

            try:
                thrift.sendRequest(fum)
            except:
                print(f'WARN: Unable to send file update notification to EDEX for {filepath}\n{traceback.format_exc()}')
                backup_service_success = False

    # Print out summary
    print()
    if conversion_success:
        print('SUCCESS: All files were successfully updated.')
    else:
        print('ERROR: Some files were not successfully converted. See above ERROR messages for files needing manual updates.')

    if not backup_service_success:
        print('ERROR: Re-run this script with EDEX up to trigger Backup Service.')

    if not conversion_success or not backup_service_success:
        sys.exit(2)

if __name__ == '__main__':
    main()
