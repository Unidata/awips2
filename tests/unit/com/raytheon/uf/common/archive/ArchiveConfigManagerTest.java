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
package com.raytheon.uf.common.archive;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.archive.exception.ArchiveException;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Test ArchiveConfigManager Archive Ingest Purge and Archive Creation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2013  1965       bgonzale    Initial creation.
 *                                     Added additional test data for file newer than purge
 *                                     time but in directory that is older than purge time.
 * Aug 28, 2013 2299       rferrel     purgeExpiredFromArchive now returns number of files purged.
 * Apr 14, 2014 3023       rferrel     Remove archive purge test no long works with implementation
 *                                      cluster locks.
 * Apr 17, 2014 3045       rferrel     Commented out test so no longer have dependence ArchivPurgeManger.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class ArchiveConfigManagerTest {

    private static final String RAW = "Raw";

    private static final String PROCESSED = "Processed";

    private static final String SAT_CAT_NAME_RAW = "Satellite";

    private static File TEST_DIR = TestUtil
            .setupTestClassDir(ArchiveConfigManagerTest.class);

    private final DateFormat yyyyFormat = new SimpleDateFormat("yyyy");

    private final DateFormat MMFormat = new SimpleDateFormat("MM");

    private final DateFormat ddFormat = new SimpleDateFormat("dd");

    private final DateFormat kkFormat = new SimpleDateFormat("kk");

    private final DateFormat mmFormat = new SimpleDateFormat("mm");

    private final Collection<File> archiveFiles = new ArrayList<File>();

    private final Collection<File> purgeFiles = new ArrayList<File>();

    private final Collection<File> allFiles = new ArrayList<File>();

    private final Collection<DisplayData> archiveSelectedDisplays = new HashSet<DisplayData>();

    private Calendar referenceCalendar;

    private Calendar archiveStart;

    private Calendar archiveEnd;

    private File archiveDir;

    private static String TEST_ARCHIVE_DIR = "testArchiveDir";

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        if (referenceCalendar == null) {
            setupTimes();
        }

        PathManagerFactoryTest.initLocalization();

        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

        ArchiveConfig archiveProcessed = manager.getArchive(PROCESSED);
        archiveProcessed.setRootDir(TEST_DIR.getAbsolutePath() + "/archive/");

        ArchiveConfig archiveRaw = manager.getArchive(RAW);
        archiveRaw.setRootDir(TEST_DIR.getAbsolutePath() + "/data_store/");

        // MessageFormat keys
        // args:
        // {0} dir-yyyy
        // {1} dir-MM
        // {2} dir-dd
        // {3} dir-kk
        // {4} file-yyyy
        // {5} file-MM
        // {6} file-dd
        // {7} file-kk
        // {8} file-mm
        // {9} file-epochMS
        // {10} file-epochSec

        // **** grib1 ****
        MessageFormat grib1Format_Raw = new MessageFormat(
                "/grib/{0}{1}{2}/{3}/NWS_160/GRID255/{7}{8}Z_F001_APCP-ZETA98_KSTR_{6}{7}{8}_125544891.grib.{4}{5}{6}{7}");
        createTestFiles(grib1Format_Raw, archiveRaw, "Model", false,
                archiveStart, archiveEnd);
        MessageFormat grib1Format_Processed = new MessageFormat(
                "/grid/GFS160/BL/GFS160-{4}-{5}-{6}-{7}-FH-162.h5");
        createTestFiles(grib1Format_Processed, archiveProcessed, "Model",
                false, archiveStart, archiveEnd);

        // **** sat ****
        MessageFormat satFormat_Raw = new MessageFormat(
                "/sat/{0}{1}{2}/{3}/GOES-13/{7}{8}Z_SOUND-VIS_10km_EAST-CONUS-TIGE59_KNES_128453.satz.{4}{5}{6}{7}");
        createTestFiles(satFormat_Raw, archiveRaw, SAT_CAT_NAME_RAW, true,
                archiveStart, archiveEnd);
        // MessageFormat satFormat_Processed = new MessageFormat(
        // "/satellite/East CONUS/Sounder Visible imagery/satellite-{4}-{5}-{6}-{7}.h5");
        // createTestFiles(satFormat_Processed, archiveProcessed, "Satellite",
        // true, archiveStart, archiveEnd);

        // **** acars ****
        MessageFormat acarsFormat_Raw = new MessageFormat(
                "/acars/acars_encrypted/{0}{1}{2}/{3}/IUAB01_CWAO_{6}{7}{8}_22714956.bufr.{4}{5}{6}{7}");
        createTestFiles(acarsFormat_Raw, archiveRaw, "Observation", false,
                archiveStart, archiveEnd);
        MessageFormat acarsFormat_Processed = new MessageFormat(
                "/acars/acars-{4}-{5}-{6}-{7}.h5");
        createTestFiles(acarsFormat_Processed, archiveProcessed, "Observation",
                false, archiveStart, archiveEnd);

        // **** binlightning ****
        MessageFormat binlightningFormat_Raw = new MessageFormat(
                "/binlightning/{0}{1}{2}/{3}/SFUS41_KWBC_{6}{7}{8}_22725485.nldn.{4}{5}{6}{7}");
        createTestFiles(binlightningFormat_Raw, archiveRaw, "Observation",
                false, archiveStart, archiveEnd);
        MessageFormat binlightningFormat_Processed = new MessageFormat(
                "/binlightning/binlightning-{4}-{5}-{6}-{7}.h5");
        createTestFiles(binlightningFormat_Processed, archiveProcessed,
                "Observation", false, archiveStart, archiveEnd);

        // **** bufrsigwx ****
        MessageFormat bufrsigwxFormat_Raw = new MessageFormat(
                "/bufrsigwx/{0}{1}{2}/{3}/JUWE96_KKCI_{6}{7}{8}_31368878.bufr.{4}{5}{6}{7}");
        createTestFiles(bufrsigwxFormat_Raw, archiveRaw, "Products", false,
                archiveStart, archiveEnd);
        MessageFormat bufrsigwxFormat_Processed = new MessageFormat(
                "/bufrsigwx/SWH/sigwxCAT-{4}-{5}-{6}-{7}.h5");
        createTestFiles(bufrsigwxFormat_Processed, archiveProcessed,
                "Products", false, archiveStart, archiveEnd);

        // *** manual ****
        MessageFormat manualFormat_Raw1 = new MessageFormat(
                "manual/mpe/ZETA98_BDHRMOSAIC{4}{5}{6}{7}{8}z_15180450.grib");
        createTestFiles(manualFormat_Raw1, archiveRaw, "Local", false,
                archiveStart, archiveEnd);
        MessageFormat manualFormat_Raw2 = new MessageFormat(
                "manual/mpe/ZETA98_{0}{1}{2}{3}z_16122536.grib");
        createTestFiles(manualFormat_Raw2, archiveRaw, "Local", false,
                archiveStart, archiveEnd);
        MessageFormat manualFormat_RawE1 = new MessageFormat(
                "manual/000-KOUNVFTOUN-NXUS98-KOUN-13{5}{6}{7}{8}-___-{10}");
        createTestFiles(manualFormat_RawE1, archiveRaw, "Local", false,
                archiveStart, archiveEnd);
        MessageFormat manualFormat_RawE2 = new MessageFormat(
                "manual/AQIOUN.wan{10}");
        createTestFiles(manualFormat_RawE2, archiveRaw, "Local", false,
                archiveStart, archiveEnd);
        MessageFormat manualFormat_Raw3 = new MessageFormat(
                "manual/wrf4nssl_{0}{1}{2}{3}.f00.OUN_subset.16122536");
        createTestFiles(manualFormat_Raw3, archiveRaw, "Local", false,
                archiveStart, archiveEnd);
        MessageFormat manualFormat_Raw4 = new MessageFormat(
                "manual/ZETA98.LAPS.{4}{5}{6}_{7}{8}");
        createTestFiles(manualFormat_Raw4, archiveRaw, "Local", false,
                archiveStart, archiveEnd);

        // **** manual using file last modified time.
        createModTestFiles("manual/FOUS74KTUA.16130407.100",
                "manual/FOUS74KTUA.16130407.200",
                "manual/FOUS74KTUA.16130407.300",
                "manual/FOUS74KTUA.16130407.400", archiveRaw, "Local", false,
                archiveStart, archiveEnd);

        // create test archive data dir
        archiveDir = new File(TEST_DIR, TEST_ARCHIVE_DIR);

    }

    private int getRetentionHours(ArchiveConfig archive, CategoryConfig category) {
        return category == null || category.getRetentionHours() == 0 ? archive
                .getRetentionHours() : category.getRetentionHours();
    }

    private CategoryConfig getCategory(ArchiveConfig archive,
            String categoryName) {
        CategoryConfig category = null;
        for (CategoryConfig c : archive.getCategoryList()) {
            if (c.getName().equals(categoryName)) {
                category = c;
                break;
            }
        }
        if (category == null) {
            // This is part of setup and asserts will not give stack trace.
            System.err.println(String.format("category: %s not in archive: %s",
                    categoryName, archive.getName()));
            throw new IllegalArgumentException("bad category name: "
                    + categoryName);
        }
        return category;
    }

    private void createTestFiles(MessageFormat fileNameFormat,
            ArchiveConfig archive, String categoryName, boolean isSelected,
            Calendar start, Calendar end) throws IOException {

        CategoryConfig category = getCategory(archive, categoryName);
        int retentionHours = getRetentionHours(archive, category);
        String rootDir = archive.getRootDir();

        // create data file newer than purge time, within archive time, and
        // isSelected
        File dataFile = create_DataFile(end, fileNameFormat, rootDir);
        if (isSelected) {
            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

            archiveFiles.add(dataFile);
            archiveSelectedDisplays.addAll(manager.getDisplayData(
                    archive.getName(), categoryName, true));
        }
        System.out
                .println("{newer than purge/within archive/isSelected}\n\tFor archive:"
                        + archive.getName()
                        + " category:"
                        + categoryName
                        + "\n\tcreated file: "
                        + dataFile.getAbsolutePath()
                                .substring(rootDir.length()));

        // create data file newer than purge time, within archive time, but not
        // in selected
        Calendar moreThanOneDayOld = (Calendar) referenceCalendar.clone();
        moreThanOneDayOld.add(Calendar.DAY_OF_MONTH, -1);
        dataFile = create_DataFile(moreThanOneDayOld, fileNameFormat, rootDir);
        System.out
                .println("{newer than purge/within archive/Not Selected}\nFor archive:"
                        + archive.getName()
                        + " category:"
                        + categoryName
                        + "\n\tcreated file: "
                        + dataFile.getAbsolutePath()
                                .substring(rootDir.length()));

        // create data file older than purge time
        Calendar lessThanExpiredCalendar = (Calendar) referenceCalendar.clone();
        lessThanExpiredCalendar.add(Calendar.HOUR, (-1 * retentionHours - 1));
        dataFile = create_DataFile(lessThanExpiredCalendar, fileNameFormat,
                rootDir);
        purgeFiles.add(dataFile);
        System.out.println("{older than purge}\nFor archive:"
                + archive.getName() + " category:" + categoryName
                + "\n\tcreated file: "
                + dataFile.getAbsolutePath().substring(rootDir.length()));

        // // create data file newer than purge time, but in a directory that is
        // // older than purge time, and outside of archive time frame
        Calendar newerThanArchiveEnd = (Calendar) end.clone();
        // newerThanArchiveEnd.add(Calendar.HOUR, 3);
        // dataFile = create_DataFile(lessThanExpiredCalendar,
        // newerThanArchiveEnd, fileNameFormat, rootDir);
        // System.out
        // .println("{newer than purge/in directory older than purge/outside of archive}\nFor archive:"
        // + archive.getName()
        // + " category:"
        // + categoryName
        // + "\n created file: " + dataFile.getAbsolutePath());

        // create data file newer than purge time and outside of archive time
        // frame
        newerThanArchiveEnd = (Calendar) end.clone();
        newerThanArchiveEnd.add(Calendar.HOUR, 3);
        dataFile = create_DataFile(newerThanArchiveEnd, fileNameFormat, rootDir);
        System.out
                .println("{newer than purge/outside of archive}\nFor archive:"
                        + archive.getName()
                        + " category:"
                        + categoryName
                        + "\n\tcreated file: "
                        + dataFile.getAbsolutePath()
                                .substring(rootDir.length()));
    }

    private void createModTestFiles(String newFilename, String oldFilename,
            String purgeFilename, String outsideFilename,
            ArchiveConfig archive, String categoryName, boolean isSelected,
            Calendar start, Calendar end) throws IOException {

        CategoryConfig category = getCategory(archive, categoryName);
        int retentionHours = getRetentionHours(archive, category);
        String rootDir = archive.getRootDir();

        // create data file newer than purge time, within archive time, and
        // isSelected
        File dataFile = create_ModFile(end, newFilename, rootDir);

        if (isSelected) {
            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

            archiveFiles.add(dataFile);
            archiveSelectedDisplays.addAll(manager.getDisplayData(
                    archive.getName(), categoryName, true));
        }
        System.out
                .println("{newer modTime than purge/within archive/}\n\tFor archive:"
                        + archive.getName()
                        + " category:"
                        + categoryName
                        + "\n\tcreated file: "
                        + dataFile.getAbsolutePath()
                                .substring(rootDir.length()));

        // create data file newer than purge time, within archive time, but not
        // in selected
        Calendar moreThanOneDayOld = (Calendar) referenceCalendar.clone();
        moreThanOneDayOld.add(Calendar.DAY_OF_MONTH, -1);
        dataFile = create_ModFile(moreThanOneDayOld, oldFilename, rootDir);
        System.out
                .println("{newer modTime than purge/within archive/Not Selected}\nFor archive:"
                        + archive.getName()
                        + " category:"
                        + categoryName
                        + "\n\tcreated file: "
                        + dataFile.getAbsolutePath()
                                .substring(rootDir.length()));

        // create data file older than purge time
        Calendar lessThanExpiredCalendar = (Calendar) referenceCalendar.clone();
        lessThanExpiredCalendar.add(Calendar.HOUR, (-1 * retentionHours - 1));
        dataFile = create_ModFile(lessThanExpiredCalendar, purgeFilename,
                rootDir);
        purgeFiles.add(dataFile);
        System.out.println("{older than purge}\nFor archive:"
                + archive.getName() + " category:" + categoryName
                + "\n\tcreated file: "
                + dataFile.getAbsolutePath().substring(rootDir.length()));

        // // create data file newer than purge time, but in a directory that is
        // // older than purge time, and outside of archive time frame
        Calendar newerThanArchiveEnd = (Calendar) end.clone();
        // newerThanArchiveEnd.add(Calendar.HOUR, 3);
        // dataFile = create_DataFile(lessThanExpiredCalendar,
        // newerThanArchiveEnd, fileNameFormat, rootDir);
        // System.out
        // .println("{newer than purge/in directory older than purge/outside of archive}\nFor archive:"
        // + archive.getName()
        // + " category:"
        // + categoryName
        // + "\n created file: " + dataFile.getAbsolutePath());

        // create data file newer than purge time and outside of archive time
        // frame
        newerThanArchiveEnd = (Calendar) end.clone();
        newerThanArchiveEnd.add(Calendar.HOUR, 3);
        dataFile = create_ModFile(newerThanArchiveEnd, outsideFilename, rootDir);
        System.out
                .println("{newer modTime than purge/outside of archive}\nFor archive:"
                        + archive.getName()
                        + " category:"
                        + categoryName
                        + "\n\tcreated file: "
                        + dataFile.getAbsolutePath()
                                .substring(rootDir.length()));
    }

    private void setupTimes() {
        referenceCalendar = TimeUtil.newGmtCalendar();
        referenceCalendar.set(Calendar.MINUTE, 0);
        referenceCalendar.set(Calendar.SECOND, 0);

        archiveStart = (Calendar) referenceCalendar.clone();
        archiveEnd = (Calendar) referenceCalendar.clone();

        archiveStart.add(Calendar.HOUR, -20);
        archiveEnd.add(Calendar.HOUR, -3);

        yyyyFormat.setCalendar(referenceCalendar);
        MMFormat.setCalendar(referenceCalendar);
        ddFormat.setCalendar(referenceCalendar);
        kkFormat.setCalendar(referenceCalendar);
        mmFormat.setCalendar(referenceCalendar);
    }

    private File create_DataFile(Calendar referenceCalendar,
            MessageFormat fileFormat, String rootDir) throws IOException {
        return create_DataFile(referenceCalendar, referenceCalendar,
                fileFormat, rootDir);
    }

    private File create_DataFile(Calendar directoryReferenceCalendar,
            Calendar fileReferenceCalendar, MessageFormat fileFormat,
            String rootDir) throws IOException {
        Date directoryReferenceTime = directoryReferenceCalendar.getTime();
        Date fileReferenceTime = fileReferenceCalendar.getTime();

        String dir_yyyy = yyyyFormat.format(directoryReferenceTime);
        String dir_MM = MMFormat.format(directoryReferenceTime);
        String dir_dd = ddFormat.format(directoryReferenceTime);
        String dir_kk = kkFormat.format(directoryReferenceTime);
        String file_yyyy = yyyyFormat.format(fileReferenceTime);
        String file_MM = MMFormat.format(fileReferenceTime);
        String file_dd = ddFormat.format(fileReferenceTime);
        String file_kk = kkFormat.format(fileReferenceTime);
        String file_mm = mmFormat.format(fileReferenceTime);
        String file_epochMS = String.format("%013d",
                fileReferenceTime.getTime());
        String file_epochSec = String.format("%010d",
                fileReferenceTime.getTime() / TimeUtil.MILLIS_PER_SECOND);
        String[] formatArgs = new String[] { dir_yyyy, dir_MM, dir_dd, dir_kk,
                file_yyyy, file_MM, file_dd, file_kk, file_mm, file_epochMS,
                file_epochSec };

        String filename = fileFormat.format(formatArgs, new StringBuffer(),
                new FieldPosition(0)).toString();
        File resultFile = new File(rootDir, filename);
        String dirname = FilenameUtils
                .getFullPath(resultFile.getAbsolutePath());
        File dir = new File(dirname);

        dir.mkdirs();
        resultFile.createNewFile();
        allFiles.add(resultFile);
        return resultFile;
    }

    private File create_ModFile(Calendar fileReferenceCalendar,
            String filename, String rootDir) throws IOException {
        Date fileReferenceTime = fileReferenceCalendar.getTime();

        File resultFile = new File(rootDir, filename);
        String dirname = FilenameUtils
                .getFullPath(resultFile.getAbsolutePath());
        File dir = new File(dirname);

        dir.mkdirs();
        resultFile.createNewFile();
        allFiles.add(resultFile);
        resultFile.setLastModified(fileReferenceTime.getTime());
        return resultFile;
    }

    private Collection<String> createFileNameListRemoveTestDir(
            Collection<File> files) {
        List<String> result = new ArrayList<String>(files.size());
        for (File f : files) {
            String absPath = f.getAbsolutePath();
            String testDirPath = TEST_DIR.getAbsolutePath();
            testDirPath = testDirPath.endsWith(File.separator) ? testDirPath
                    : testDirPath + File.separator;
            // remove test directory path
            String fileRelativePath = absPath.replace(testDirPath, "");
            // remove one directory up
            int index = fileRelativePath.indexOf(File.separator);
            fileRelativePath = index >= 0 ? fileRelativePath.substring(index)
                    : fileRelativePath;
            result.add(fileRelativePath);
        }
        Collections.sort(result);
        return result;
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        FileUtil.deleteDir(TEST_DIR);
        FileUtil.deleteDir(archiveDir);
    }

    @Test
    public void testArchiveManagerCreateArchive() throws IOException,
            ArchiveException {
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        Collection<File> filesInCreatedArchive = manager.createArchive(
                archiveDir, archiveSelectedDisplays, archiveStart, archiveEnd);

        assertEquals(
                "The expected archive files and the archived files are not the same",
                createFileNameListRemoveTestDir(archiveFiles),
                createFileNameListRemoveTestDir(filesInCreatedArchive));

        // check archive directory for files.
        Collection<File> filesFoundInArchive = FileUtils.listFiles(archiveDir,
                FileFilterUtils.trueFileFilter(),
                FileFilterUtils.trueFileFilter());

        assertEquals(
                "The archive files reported and the files found in the archive are not the same",
                createFileNameListRemoveTestDir(filesInCreatedArchive),
                createFileNameListRemoveTestDir(filesFoundInArchive));
    }

    /*
     * With the implementation of cluster task locking (to prevent database
     * Archive, Archive purge and Archive case creation from interfering with
     * each other) this unit test fails since unable to access the
     * awips.cluster_task table to perform the locks.
     */
    // @Test
    // public void testArchiveManagerPurge() throws IOException {
    // ArchivePurgeManager manager = ArchivePurgeManager.getInstance();
    // Collection<File> filesFoundInPurge = new ArrayList<File>();
    // int purgeCount = 0;
    //
    // for (ArchiveConfig a : manager.getArchives()) {
    // purgeCount += manager.purgeExpiredFromArchive(a);
    // }
    //
    // // assertEquals(
    // //
    // //
    // "The expected number of purged files and number of purge files not the same",
    // // purgeCount, purgeFiles.size());
    //
    // for (File file : allFiles) {
    // if (!file.exists()) {
    // filesFoundInPurge.add(file);
    // }
    // }
    // System.out.println("purgeCount: " + purgeCount + ", pureFiles.size:"
    // + purgeFiles.size() + ", filesFoundInPurge.size(): "
    // + filesFoundInPurge.size());
    //
    // for (File file : purgeFiles) {
    // if (!filesFoundInPurge.contains(file)) {
    // System.out.println("not purged: " + file.getAbsolutePath());
    // }
    // }
    //
    // assertEquals(
    // "The expected purge files and the files purged are not the same",
    // createFileNameListRemoveTestDir(purgeFiles),
    // createFileNameListRemoveTestDir(filesFoundInPurge));
    // }

}
