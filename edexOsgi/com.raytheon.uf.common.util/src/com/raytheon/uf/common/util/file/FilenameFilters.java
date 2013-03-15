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
package com.raytheon.uf.common.util.file;

import java.io.File;
import java.io.FilenameFilter;

/**
 * Consolidates common filename filters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2013 1794       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class FilenameFilters {

    /**
     * Reverses a {@link FilenameFilter}
     */
    private static class ReverseFilter implements FilenameFilter {
        private final FilenameFilter filter;

        private ReverseFilter(FilenameFilter filter) {
            this.filter = filter;
        }

        @Override
        public boolean accept(File dir, String name) {
            return !filter.accept(dir, name);
        }
    }

    /**
     * {@link FilenameFilter} that matches files with the specified extension.
     */
    private static class FileExtensionFilenameFilter implements FilenameFilter {

        private final String fileExtension;

        /**
         * @param fileExtension
         */
        private FileExtensionFilenameFilter(String fileExtension) {
            this.fileExtension = fileExtension;
        }

        @Override
        public boolean accept(File dir, String name) {
            return name.endsWith(fileExtension);
        }
    }

    /**
     * {@link FilenameFilter} that matches files with the specified prefix.
     */
    private static class StartsWithPrefix implements FilenameFilter {
        private final String prefix;

        private StartsWithPrefix(String prefix) {
            this.prefix = prefix;
        }

        @Override
        public boolean accept(final File dir, final String name) {
            return name != null && name.trim().startsWith(prefix);
        }
    };

    /**
     * {@link FilenameFilter} that denies files with the specified prefix.
     */
    private static class SumFilter implements FilenameFilter {
        private final FilenameFilter[] filters;

        private SumFilter(FilenameFilter[] filters) {
            this.filters = filters;
        }

        @Override
        public boolean accept(final File dir, final String name) {
            for (FilenameFilter filter : filters) {
                if (!filter.accept(dir, name)) {
                    return false;
                }
            }

            return true;
        }
    };

    /**
     * No construction.
     */
    private FilenameFilters() {
    }

    /**
     * Accepts all files.
     */
    public static final FilenameFilter ACCEPT_ALL = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return true;
        }
    };

    /**
     * Denies all files.
     */
    public static final FilenameFilter ACCEPT_NONE = new ReverseFilter(
            ACCEPT_ALL);

    /**
     * Denies all files starting with "."
     */
    public static final FilenameFilter NO_LINUX_HIDDEN_FILES = new ReverseFilter(
            byFilePrefix("."));

    /**
     * Accepts directories.
     */
    public static final FilenameFilter ACCEPT_DIRECTORIES = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return new File(dir.getPath(), name).isDirectory();
        }
    };

    /**
     * Accepts directories.
     */
    public static final FilenameFilter ACCEPT_FILES = new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return new File(dir.getPath(), name).isFile();
        }
    };

    /**
     * Returns a {@link FilenameFilter} that matches the specified file
     * extension.
     * 
     * @param fileExtension
     *            the file extension to match
     * @return the file name filter
     */
    public static FilenameFilter byFileExtension(final String fileExtension) {
        return new FileExtensionFilenameFilter(fileExtension);
    }

    /**
     * Returns a {@link FilenameFilter} that matches the specified file name
     * prefix.
     * 
     * @param prefix
     *            the file prefix to match
     * @return the file name filter
     */
    public static FilenameFilter byFilePrefix(final String prefix) {
        return new StartsWithPrefix(prefix);
    }

    /**
     * Returns a {@link FilenameFilter} that returns true only if all filters
     * return true.
     * 
     * @param filters
     *            the filters
     * @return the file name filter
     */
    public static FilenameFilter byFilters(FilenameFilter filters) {
        // This method version forces at least one filter to supplied
        return byFilters(new FilenameFilter[] { filters });
    }

    /**
     * Returns a {@link FilenameFilter} that returns true only if all filters
     * return true.
     * 
     * @param filters
     *            the filters
     * @return the file name filter
     */
    public static FilenameFilter byFilters(FilenameFilter... filters) {
        return new SumFilter(filters);
    }

    /**
     * Returns a {@link FilenameFilter} that returns the reverse of the
     * specified filter. For instance, if the supplied filter would return true
     * if the file ends with .xml this version returns a filter that would
     * return false.
     * 
     * @param filter
     *            the filter to reverse
     * @return the reversed filter
     */
    public static FilenameFilter reverse(FilenameFilter filter) {
        return new ReverseFilter(filter);
    }
}
