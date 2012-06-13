/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.common.util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/**
 * Contains common file utilities. Methods are generally static to use without a
 * class instance. Methods in class should not log directly; rather they should
 * throw an appropriate exception.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2007            njensen     Initial creation
 * 23Jul2008    1130       MW Fegan    Corrected deleteDir(...) to
 *                                      return false when unable to
 *                                      obtain directory listing.
 * Sep 16, 2008 1250       jelkins     Added join function
 * 
 * </pre>
 * 
 * @author njensen
 */
public class FileUtil {
    /**
     * Displayable string of valid filename characters
     */
    public static final String VALID_FILENAME_CHARS = "\"A-Za-z0-9._- \"";

    private static final Pattern VALID_FILENAME = Pattern
            .compile("^[A-Za-z0-9._\\- ]+$");

    public final static String fileSeparatorRegex = "[/\\\\]";

    /**
     * Joins one or more path components into a single path string. Path
     * components are separated by the operating system dependent
     * File.separator.
     * 
     * @param pathComponents
     *            a list of strings representing components of a file path
     * @return a string with all the path components joined together with the
     *         File.separator character.
     */
    public static String join(String... pathComponents) {

        StringBuilder fullPath = new StringBuilder();
        for (String component : pathComponents) {
            if ((fullPath.length() > 0)
                    && (fullPath.charAt(fullPath.length() - 1) != File.separatorChar)
                    && ((component.isEmpty()) || (component.charAt(0) != File.separatorChar))) {
                fullPath.append(File.separatorChar);
            }
            fullPath.append(component);

        }

        return fullPath.toString();
    }

    /**
     * Lists files that match the filter in the directory
     * 
     * @param directory
     *            the directory to scan for files
     * @param filter
     *            the filename filter
     * @param recurse
     *            whether or not to go into subdirectories
     * @return the files that match the filter
     */
    public static ArrayList<File> listFiles(File directory,
            FilenameFilter filter, boolean recurse) {
        // List of files / directories
        ArrayList<File> files = new ArrayList<File>();

        // Get files / directories in the directory
        File[] entries = directory.listFiles();
        if (entries == null) {
            entries = new File[0];
        }

        // Go over entries
        for (int i = 0; i < entries.length; i++) {
            File entry = entries[i];
            // If there is no filter or the filter accepts the
            // file / directory, add it to the list
            if (filter == null || filter.accept(directory, entry.getName())) {
                files.add(entry);
            }

            // If the file is a directory and the recurse flag
            // is set, recurse into the directory
            if (recurse && entry.isDirectory()) {
                files.addAll(listFiles(entry, filter, recurse));
            }
        }

        return files;
    }

    /**
     * Delete a directory. Also deletes any sub-directories. This method fails
     * early if any problem is detected. That is, if a directory containing 10
     * files is being deleted and the delete of the first file fails, the
     * directory will remain with nine files.
     * 
     * @param dir
     *            Directory to delete
     * @return True if directory was deleted, false otherwise
     */
    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            File[] files = dir.listFiles();
            if (files != null) {
                for (File file : dir.listFiles()) {
                    boolean success = deleteDir(file);
                    if (!success) {
                        return false;
                    }
                }
            } else {
                /*
                 * To get here, you need to experience problems, such as an I/O
                 * error, that prevent getting a directory listing. As a result,
                 * we cannot delete the directory.
                 */

                return false;
            }
        }

        // The directory is now empty so delete it
        return dir.delete();
    }

    /**
     * Recursively copies all files from one directory to another.
     * <p>
     * If the destination directory does not exist, it will be created.
     * 
     * TODO add options for controlling file overwriting
     * 
     * @param source
     * @param destination
     * @throws IOException
     */
    public static void copyDirectory(File source, File destination)
            throws IOException {

        if (source.isDirectory()) {

            if (!destination.exists()) {
                destination.mkdir();
            }

            String[] files = source.list();

            for (String file : files) {
                copyDirectory(new File(source, file), new File(destination,
                        file));
            }
        } else {

            InputStream in = new FileInputStream(source);
            OutputStream out = new FileOutputStream(destination);

            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }

            in.close();
            out.close();

        }
    }

    /**
     * Mangles a file name using the AWIPS I GFE algorithm.
     * 
     * Replaces all special characters in a file name with _xx to ensure it will
     * be a valid file name.
     * 
     * @param name
     *            unmangled file name
     * @return mangled file name
     */
    public static String mangle(String name) {
        StringBuilder mangled = new StringBuilder();
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (Character.isLetterOrDigit(ch)) {
                // normal character
                mangled.append(ch);
            } else {
                // special character
                mangled.append('_');
                mangled.append((char) ((ch / 16) + 'A'));
                mangled.append((char) ((ch % 16) + 'A'));
            }
        }
        return mangled.toString();
    }

    /**
     * Test a file name to see if it is already mangled
     * 
     * @param name
     * @return true if mangled
     */
    public static boolean isMangled(String name) {
        int special = 0;
        int hexValue = 0;
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);

            if (ch == '_') {
                special = 2;
                hexValue = 0;
                continue;
            }

            if (special > 0) {
                int value = (ch - 'A');
                if (value >= 16) {
                    return false;
                }

                hexValue = (hexValue << 4) + value;
                if (--special == 0) {
                    char c = (char) hexValue;
                    if (Character.isLetterOrDigit(c)) {
                        return false;
                    }
                }
            } else if (Character.isLetterOrDigit(ch)) {
                continue;
            } else {
                return false;
            }
        }
        return true;
    }

    /**
     * Unmangles a file name using the AWIPS I GFE algorithm.
     * 
     * Replaces all occurrences _xx with the corresponding special character.
     * 
     * @param name
     *            mangled name
     * @return unmangled name
     */
    public static String unmangle(String name) {
        StringBuilder unmangled = new StringBuilder();

        int special = 0;
        int hexValue = 0;
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);

            if (ch == '_') {
                special = 2;
                hexValue = 0;
                continue;
            }

            if (special > 0) {
                hexValue = (hexValue << 4) + (ch - 'A');
                if (--special == 0) {
                    unmangled.append((char) hexValue);
                }
            } else {
                unmangled.append(ch);
            }
        }

        return unmangled.toString();
    }

    public static void copyFile(File fileToCopy, File outputFile)
            throws IOException {

        FileInputStream fis = null;
        FileOutputStream fos = null;
        try {
            fis = new FileInputStream(fileToCopy);
            outputFile.getParentFile().mkdirs();
            fos = new FileOutputStream(outputFile);
            byte[] bytes = new byte[2048];
            int len = fis.read(bytes);
            while (len > -1) {
                fos.write(bytes, 0, len);
                len = fis.read(bytes);
            }
        } finally {
            if (fos != null) {
                fos.close();
            }
            if (fis != null) {
                fis.close();
            }
        }
    }

    public static String file2String(File file) throws IOException {
        return new String(file2bytes(file));
    }

    /**
     * Converts the contents of a file to a byte array
     * 
     * @param file
     *            The file data to convert
     * @return An array of bytes that represent the file data
     * @throws IOException
     */
    public static byte[] file2bytes(File file) throws IOException {
        InputStream is = null;
        byte[] bytes = null;

        try {
            is = new FileInputStream(file);

            // Get the size of the file
            long length = file.length();

            // You cannot create an array using a long type.
            // It needs to be an int type.
            // Before converting to an int type, check
            // to ensure that file is not larger than Integer.MAX_VALUE.
            if (length > Integer.MAX_VALUE) {
                // File is too large
            }

            // Create the byte array to hold the data
            bytes = new byte[(int) length];

            // Read in the bytes
            int offset = 0;
            int numRead = 0;
            while (offset < bytes.length
                    && (numRead = is.read(bytes, offset, bytes.length - offset)) >= 0) {
                offset += numRead;
            }

            // Ensure all the bytes have been read in
            if (offset < bytes.length) {
                throw new IOException("Could not completely read file "
                        + file.getName());
            }
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException e1) {
                    // ignore
                }
            }
        }

        return bytes;
    }

    /**
     * Converts the contents of a file to a byte array
     * 
     * @param file
     *            The file data to convert
     * @param compressed
     *            If the file is compressed or not
     * @return An array of bytes that represent the file data
     * @throws IOException
     */
    public static byte[] file2bytes(File file, boolean compressed)
            throws IOException {
        InputStream is = null;
        byte[] bytes = null;

        try {
            // Get the size of the file
            long length = file.length();

            // You cannot create an array using a long type.
            // It needs to be an int type.
            // Before converting to an int type, check
            // to ensure that file is not larger than Integer.MAX_VALUE.
            if (length > Integer.MAX_VALUE) {
                // File is too large
            }

            is = new FileInputStream(file);
            if (compressed) {
                int bufferSize = 8 * 1024;
                if (bufferSize < length) {
                    bufferSize = (int) length;
                }

                // set GZIP input stream to size of file on disk.
                is = new GZIPInputStream(is, (int) length);
                ByteArrayOutputStream baos = null;
                try {
                    baos = ByteArrayOutputStreamPool.getInstance().getStream();
                    bytes = new byte[bufferSize];
                    int numRead = 0;
                    while ((numRead = is.read(bytes)) >= 0) {
                        baos.write(bytes, 0, numRead);
                    }

                    bytes = baos.toByteArray();
                } finally {
                    if (baos != null) {
                        try {
                            baos.close();
                        } catch (Exception e) {
                            // ignore
                        }
                    }
                }
            } else {
                // Create the byte array to hold the data
                bytes = new byte[(int) length];

                // Read in the bytes
                int offset = 0;
                int numRead = 0;
                while (offset < bytes.length
                        && (numRead = is.read(bytes, offset, bytes.length
                                - offset)) >= 0) {
                    offset += numRead;
                }

                // Ensure all the bytes have been read in
                if (offset < bytes.length) {
                    throw new IOException("Could not completely read file "
                            + file.getName());
                }
            }
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException e1) {
                    // ignore
                }
            }
        }

        return bytes;
    }

    /**
     * Writes the contents of the bytes parameter to the output file
     * 
     * @param outBytes
     *            The data to store
     * @param outFile
     *            The file to write this data
     * @throws IOException
     */
    public static void bytes2File(byte[] outBytes, File outFile)
            throws IOException {
        bytes2File(outBytes, outFile, false);
    }

    /**
     * Writes the contents of the bytes parameter to the output file
     * 
     * @param outBytes
     *            The data to store
     * @param outFile
     *            The file to write this data
     * @throws IOException
     */
    public static void bytes2File(byte[] outBytes, File outFile,
            boolean compress) throws IOException {
        if (!outFile.getParentFile().exists()) {
            outFile.getParentFile().mkdirs();
        }

        OutputStream out = null;
        try {
            int buffer = 8 * 1024;
            out = new FileOutputStream(outFile);
            if (compress) {
                out = new GZIPOutputStream(out, buffer);
            }

            // only write out buffer at a time
            for (int counter = 0; counter < outBytes.length; counter += buffer) {
                if ((outBytes.length - counter) - buffer >= 0) {
                    out.write(outBytes, counter, buffer);
                } else {
                    out.write(outBytes, counter, (outBytes.length - counter));
                }
            }
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Checks if a file has been modified since a particular time.
     * 
     * @param file
     *            the file or directory to check
     * @param lastModified
     *            the time to compare against
     * @param recursive
     *            whether or not to recursively check if the file is a directory
     * @return true if the file (or subdirectories and files if recursive is
     *         true) has been modified since the time
     */
    public static boolean hasBeenModifiedSince(File file, long lastModified,
            boolean recursive) {
        boolean result = false;
        if (file.exists()) {
            if (file.isDirectory() && recursive) {
                for (File f : file.listFiles()) {
                    if (hasBeenModifiedSince(f, lastModified, recursive)) {
                        result = true;
                        break;
                    }
                }
            } else {
                if (file.lastModified() > lastModified) {
                    result = true;
                }
            }
        }
        return result;
    }

    /**
     * Convert a file path to contain OS {@link File#separator} values
     * 
     * @param aPath
     *            A path to a file
     * @return A path that contains the correct path separators
     */
    public static String convertFilePath(String aPath) {

        // cannot use File.separator as the replacement string for '\', as it
        // will replace with "" + '\.

        String replacement = (File.separatorChar == '\\') ? "\\\\"
                : File.separator;
        if (aPath != null && aPath.length() > 0) {
            return aPath.replaceAll(fileSeparatorRegex, replacement);
        } else {
            return aPath;
        }
    }

    /**
     * Convert a file path to contain Edex separator values
     * 
     * @param aPath
     *            A path to a file
     * @return A path that contains the correct path separators for edex
     */
    public static String edexPath(String aPath) {

        if (aPath != null && aPath.length() > 0) {
            // Remove drive letter
            if (aPath.length() > 1 && aPath.charAt(1) == ':') {
                aPath = aPath.substring(2);
            }
            return aPath.replace("\\", "/");
        } else {
            return aPath;
        }
    }

    /**
     * Validate a filename contains only allowable characters
     * 
     * @param fileName
     * @return true if fileName is valid
     */
    public static boolean isValidFilename(String fileName) {
        return VALID_FILENAME.matcher(fileName).matches();
    }

}
