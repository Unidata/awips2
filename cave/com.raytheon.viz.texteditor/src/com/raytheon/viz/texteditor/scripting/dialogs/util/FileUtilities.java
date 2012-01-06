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
package com.raytheon.viz.texteditor.scripting.dialogs.util;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;

/**
 * Contains file utilities for the Text WS Scripting component.
 * All methods are static.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2009            mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public final class FileUtilities {

    /**
     * 
     */
    private FileUtilities() {
        // no initialization desired
    }
    private static final String UNWRITABLE_ERR_FMT = "Attempted to write to unwritable file [%s]";
    private static final int BLKSIZ = 8192;

    /** Read the entire content of a Reader into a String */
    public static String readerToString(Reader is) throws IOException {
       StringBuffer sb = new StringBuffer();
       char[] b = new char[BLKSIZ];
       int n;

       // Read a block. If it gets any chars, append them.
       while ((n = is.read(b)) > 0) {
          sb.append(b, 0, n);
       }

       // Only construct the String object once, here.
       return sb.toString();
    }

    /** Read the content of a Stream into a String */
    public static String inputStreamToString(InputStream is)
    throws IOException {
       return readerToString(new InputStreamReader(is));
    }
    /** read a file into a single string */
    public static String loadFileToString(File file) throws IOException {
        Reader is = new FileReader(file);
        return readerToString(is);
    }
    public static String loadFileToString(String path) throws IOException {
        return loadFileToString(new File(path));
    }
    public static String loadFileToString(String path, String file) throws IOException {
        return loadFileToString(new File(path,file));
    }
    /** list the files in a directory */
    public static ArrayList<File> listFiles(File directory,
            FilenameFilter filter, boolean recurse) {
        System.out.println(directory);
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
    /** writes the string to the specified file */
    public static void writeStringToFile(File file, String contents) throws IOException {
        if (file.isFile() && !file.canWrite()) {
            throw new IOException(String.format(UNWRITABLE_ERR_FMT, file.toString()));
        }
        Writer os = null;
        try {
            os = new FileWriter(file);
            os.write(contents);
        } finally {
            if (os != null) {
                os.close();
            }
        }
    }
    public static void writeStringToFile(String path, String contents) throws IOException {
        writeStringToFile(new File(path),contents);
    }
    public static void writeStringToFile(String path, String file, String contents) throws IOException {
        writeStringToFile(path + File.separator + file,contents);
    }
}
