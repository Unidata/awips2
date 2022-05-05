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
package com.raytheon.edex.plugin.gfe.log;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDate;
import java.util.EnumSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Bean for purging GFESuite logs. Should be scheduled to run from a cron.
 * <p>
 * Searches for date-specific directories with YYYYMMDD names under the
 * configured top-level directory. Old directories are compressed into ZIP
 * files. Old compressed files are removed.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bphillip     Initial creation
 * Sep 05, 2013  #2307     dgilling     Use better PythonScript constructor.
 * Feb 26, 2015  #4128     dgilling     Switch to IFPServer.getActiveSites().
 * Jul 15, 2016  #5747     dgilling     Refactor based on FilePurger.
 * Feb 16, 2018  6895      tgurney      Move FilePurger to uf.edex.maintenance
 * May 14, 2019  DCS 21081 dfriedman    Support recursion and compression.
 * Aug 18, 2020  22148     ryu          Fix issue of zip archive not containing
 *                                      log files.
 *
 * </pre>
 *
 * @author bphillip
 */
public class LogPurger {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LogPurger.class);

    private static final String PATTERN_YEAR = "year";

    private static final String PATTERN_MONTH = "month";

    private static final String PATTERN_DAY = "day";

    private static final String PATTERN_ZIP = "zip";

    /** Pattern to match YYYYMMDD and YYYYMMDD.zip names. */
    private static final Pattern datePathPattern = Pattern.compile(
            "^(?<" + PATTERN_YEAR + ">\\d{4})(?<" + PATTERN_MONTH + ">\\d{2})" +
            "(?<" + PATTERN_DAY + ">\\d{2})(?<" + PATTERN_ZIP + ">\\.zip)?$",
            Pattern.CASE_INSENSITIVE);

    /**
     * The top-level directory in which to scan for YYYYMMDD directories. Must
     * be an absolute path.
     */
    private String topDir;

    /**
     * The number of levels under topDir to search for YYYYMMDD directories.
     */
    private int searchDepth;

    /**
     * Number of days to keep a YYYYMMDD directory before it is
     * compressed/removed. Can be zero to not keep any directories after the
     * logging day has ended.
     */
    private int leaveUncompressedDays;

    /**
     * Number of days to keep compressed files. If zero or less than
     * leaveUncompressedDays, no compressed files will be created.
     */
    private int retainCompressedDays;


    public String getTopDir() {
        return topDir;
    }

    public void setTopDir(String topDir) {
        this.topDir = topDir;
    }

    public int getSearchDepth() {
        return searchDepth;
    }

    public void setSearchDepth(int searchDepth) {
        this.searchDepth = searchDepth;
    }

    public int getLeaveUncompressedDays() {
        return leaveUncompressedDays;
    }

    public void setLeaveUncompressedDays(int leaveUncompressedDays) {
        this.leaveUncompressedDays = Math.max(0, leaveUncompressedDays);
    }

    public int getRetainCompressedDays() {
        return retainCompressedDays;
    }

    public void setRetainCompressedDays(int retainCompressedDays) {
        this.retainCompressedDays = Math.max(0, retainCompressedDays);
    }

    /**
     * Main entry point for the purge process.
     */
    public void purge()  {
        try {
            Path topPath = topDir != null && !topDir.isEmpty() ? Paths.get(topDir) : null;
            if (topPath == null || !topPath.isAbsolute()) {
                statusHandler.error(String.format("topDir \"%s\" not set or not an absolute path", topDir));
                return;
            }
            statusHandler.info(String.format("Purging daily directories %d level(s) deep under %s: Leave directories uncompressed for %d day(s); retain compressed files for %d day(s).",
                    searchDepth, topDir, leaveUncompressedDays, retainCompressedDays));
            final long today = LocalDate.now().toEpochDay();
            Files.walkFileTree(Paths.get(topDir).normalize(),
                    EnumSet.of(FileVisitOption.FOLLOW_LINKS), searchDepth,
                    new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir,
                        BasicFileAttributes attrs) throws IOException {
                    Matcher m = datePathPattern
                            .matcher(dir.getFileName().toString());
                    if (m.matches() && m.group(PATTERN_ZIP) == null) {
                        handleDirectory(dir, fileAge(m));
                        return FileVisitResult.SKIP_SUBTREE;
                    } else {
                        return FileVisitResult.CONTINUE;
                    }
                }

                @Override
                public FileVisitResult visitFile(Path file,
                        BasicFileAttributes attrs) throws IOException {
                    Matcher m = datePathPattern
                            .matcher(file.getFileName().toString());
                    if (m.matches()) {
                        long age = fileAge(m);
                        /*
                         * If a directory is at the max search depth it will be
                         * passed to this method instead of preVisitDirectory.
                         */
                        if (attrs.isDirectory() && m.group(PATTERN_ZIP) == null) {
                            handleDirectory(file, age);
                        } else if (attrs.isRegularFile()
                                && m.group(PATTERN_ZIP) != null
                                && age > retainCompressedDays) {
                            try {
                                Files.delete(file);
                            } catch (Exception e) {
                                        statusHandler.error(String.format(
                                                "Error deleting %s: %s", file,
                                                e));
                            }
                        }
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file,
                        IOException exc) throws IOException {
                    logVisitFailed(file, exc);
                    return FileVisitResult.CONTINUE;
                }

                private long fileAge(Matcher m) {
                    LocalDate fileDate = LocalDate.of(
                            Integer.parseInt(m.group(PATTERN_YEAR)),
                            Integer.parseInt(m.group(PATTERN_MONTH)),
                            Integer.parseInt(m.group(PATTERN_DAY)));
                    return today - fileDate.toEpochDay();
                }
            });
        } catch (Exception e) {
            statusHandler.error("Error while scanning " + topDir, e);
        }
    }

    /**
     * Compress/remove {@code dir} depending on its age and the configured
     * retention periods. If the compression process is a complete failure, does
     * not delete the directory.
     *
     * @param dir
     *            directory to process
     * @param age
     *            directory age
     */
    private void handleDirectory(Path dir, long age) {
        if (age > leaveUncompressedDays) {
            if (age <= retainCompressedDays) {
                if (!compressDirectory(dir)) {
                    return;
                }
            }
            deleteDirectory(dir);
        }
    }

    /**
     * Recursively delete the given directory. Continues deletion if there are
     * errors.
     *
     * @param directory
     */
    private void deleteDirectory(Path directory) {
        try {
            Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult postVisitDirectory(Path dir,
                        IOException exc) throws IOException {
                    delete(dir);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file,
                        BasicFileAttributes attrs) throws IOException {
                    delete(file);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file,
                        IOException exc) throws IOException {
                    logVisitFailed(file, exc);
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            statusHandler.error(String.format("Error deleting %s: %s", directory, e));
        }
    }

    private static void delete(Path file) {
        try {
            Files.delete(file);
        } catch (Exception e) {
            statusHandler.error(String.format("Error deleting %s: %s", file, e));
        }
    }

    /**
     * Compress the given directory into a zip file in the directory's parent.
     * @param directory
     * @return true if at least some files were compressed; false otherwise
     */
    private boolean compressDirectory(Path directory) {
        Path top = directory.normalize();
        Path zipFile = top
                .resolveSibling(top.getFileName().toString() + ".zip");
        
        // a zip file has already been created
        if (zipFile.toFile().exists()) {
            if (zipFile.toFile().length() > 1000) {
                return true; // let the directory be removed
            } else {
                return false; // leave the directory alone
            }
        }
        
        boolean[] storedSome = new boolean[1];
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(
                zipFile.toFile()))) {
            Files.walkFileTree(top, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult preVisitDirectory(Path dir,
                        BasicFileAttributes attrs) throws IOException {
                    if (!top.equals(dir)) {
                        Path rel = top.relativize(dir);
                        ZipEntry entry = getZipEntry(rel.toString() + "/", attrs);
                        zos.putNextEntry(entry);
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file,
                        BasicFileAttributes attrs) throws IOException {
                    boolean opened = false;
                    try (FileInputStream ins = new FileInputStream(file.toFile())) {
                        opened = true;
                        Path rel = top.relativize(file);
                        ZipEntry entry = getZipEntry(rel.toString(), attrs);
                        zos.putNextEntry(entry);
                        FileUtil.copy(ins, zos);
                        storedSome[0] = true;
                    } catch (Exception e) {
                        statusHandler.error(String.format(
                                "Error compressing %s into %s: %s", file,
                                zipFile, e));
                        /*
                         * If opening file failed, just omit it and continue.
                         * Propagate other failures.
                         */
                        if (opened) {
                            throw e;
                        }
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFileFailed(Path file,
                        IOException exc) throws IOException {
                    logVisitFailed(file, exc);
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception e) {
            statusHandler.error(String.format("Error compressing %s to %s: %s",
                    top, zipFile, e), e);
            if (!storedSome[0]) {
                delete(zipFile);
                return false;
            }
        }
        return true;
    }

    private static ZipEntry getZipEntry(String path, BasicFileAttributes attrs) {
        ZipEntry entry = new ZipEntry(path);
        entry.setCreationTime(attrs.creationTime());
        entry.setLastAccessTime(attrs.lastAccessTime());
        entry.setLastModifiedTime(attrs.lastModifiedTime());
        return entry;
    }

    private static void logVisitFailed(Path file, IOException e) {
        statusHandler.error(String.format("Error accessing %s: %s", file, e));
    }

}
