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
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager.DisplayData;
import com.raytheon.uf.common.archive.config.CategoryConfig;
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
 * May 7, 2013  1965       bgonzale    Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ArchiveConfigManagerTest {

    private static final String RAW = "Raw";

    private static final String SAT_CAT_NAME = "Satellite";

    private static final String satNameForArchive = "GOES-13";

    private static File TEST_DIR = TestUtil
            .setupTestClassDir(ArchiveConfigManagerTest.class);

    private final DateFormat yyyyMMFormat = new SimpleDateFormat("yyyyMM");

    private final DateFormat ddFormat = new SimpleDateFormat("dd");

    private final DateFormat hhFormat = new SimpleDateFormat("HH");

    private final DateFormat mmFormat = new SimpleDateFormat("mm");

    private ArchiveConfigManager manager;

    private ArchiveConfig archive;

    private Collection<File> archiveFiles = new ArrayList<File>();

    private Collection<File> expiredFiles = new ArrayList<File>();

    private Collection<File> purgeFiles = new ArrayList<File>();

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
        File testLocalization = TestUtil
                .setupTestClassDir(PathManagerFactoryTest.class);

        PathManagerFactoryTest.initLocalization();

        // after setting up test localization get the production config files
        // and copy them to the the test localization directory.
        // utility/common_static/base/archive
        File prodConfigDir = new File(
                "../edexOsgi/com.raytheon.uf.edex.archive/utility");
        Collection<File> configs = FileUtils.listFiles(prodConfigDir,
                FileFilterUtils.trueFileFilter(),
                FileFilterUtils.trueFileFilter());
        File destDir = new File(testLocalization,
                "utility/common_static/base/archive");
        for (File srcConfig : configs) {
            FileUtils.copyFileToDirectory(srcConfig, destDir);
        }

        manager = ArchiveConfigManager.getInstance();
        archive = manager.getArchive(RAW);

        // configure the test archive to use the test data dir
        archive.setRootDir(TEST_DIR.getAbsolutePath() + "/data_store/");

        // MessageFormat keys
        // {0} yyyyMM {1} dd {2} hh {3} mm

        // **** grib1 ****
        MessageFormat grib1Format = new MessageFormat(
                "/grib/{0}{1}/18/NWS_160/GRID255/{2}{3}Z_F001_APCP-ZETA98_KSTR_{1}{2}{3}_125544891.grib.{0}{1}{2}");
        CategoryConfig grib1Cat = getCategory(archive, "Model grib");
        createTestFiles(grib1Format, getRetentionHours(archive, grib1Cat),
                false, archiveStart, archiveEnd);

        // **** sat ****
        CategoryConfig satCat = getCategory(archive, SAT_CAT_NAME);
        MessageFormat satFormat = new MessageFormat(
                "/sat/{0}{1}/{2}/GOES-13/{2}{3}Z_SOUND-VIS_10km_EAST-CONUS-TIGE59_KNES_128453.satz.{0}{1}{2}");
        createTestFiles(satFormat, getRetentionHours(archive, satCat), true,
                archiveStart, archiveEnd);

        // **** acars ****
        CategoryConfig otherCat = getCategory(archive, "Model other");
        int otherCatRetentionHours = getRetentionHours(archive, otherCat);
        MessageFormat acarsFormat = new MessageFormat(
                "/acars/{0}{1}/{2}/IUAB01_CWAO_{1}{2}{3}_22714956.bufr.{0}{1}{2}");
        createTestFiles(acarsFormat, otherCatRetentionHours, false,
                archiveStart, archiveEnd);

        // **** binlightning ****
        MessageFormat binlightningFormat = new MessageFormat(
                "/binlightning/{0}{1}/{2}/SFUS41_KWBC_{1}{2}{3}_22725485.nldn.{0}{1}{2}");
        createTestFiles(binlightningFormat, otherCatRetentionHours, false,
                archiveStart, archiveEnd);

        // **** bufrsigwx ****
        MessageFormat bufrsigwxFormat = new MessageFormat(
                "/bufrsigwx/{0}{1}/{2}/JUWE96_KKCI_{1}{2}{3}_31368878.bufr.{0}{1}{2}");
        createTestFiles(bufrsigwxFormat, otherCatRetentionHours, false,
                archiveStart, archiveEnd);

        // create test archive data dir
        archiveDir = new File(TEST_DIR, TEST_ARCHIVE_DIR);
    }

    private int getRetentionHours(ArchiveConfig archive, CategoryConfig category) {
        return category.getRetentionHours() == 0 ? archive.getRetentionHours()
                : category.getRetentionHours();
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
        return category;
    }

    private void createTestFiles(MessageFormat dataFileNameFormat,
            int retentionHours, boolean isSelected, Calendar start, Calendar end)
            throws IOException {
        // create data file newer than purge time, within archive time, and
        // isSelected
        File dataFile = createDataFile(end, dataFileNameFormat);
        if (isSelected) {
            archiveFiles.add(dataFile);
        }

        // create data file newer than purge time, within archive time, but not
        // in selected
        Calendar moreThanOneDayOld = (Calendar) referenceCalendar.clone();
        moreThanOneDayOld.add(Calendar.DAY_OF_MONTH, -1);
        createDataFile(moreThanOneDayOld, dataFileNameFormat);

        // create data file older than purge time
        Calendar lessThanExpiredCalendar = (Calendar) referenceCalendar.clone();
        lessThanExpiredCalendar.add(Calendar.HOUR, (-1 * retentionHours - 1));
        dataFile = createDataFile(lessThanExpiredCalendar, dataFileNameFormat);
        expiredFiles.add(dataFile);
        purgeFiles.add(dataFile);

        // create data file newer than purge time and outside of archive time
        // frame
        Calendar newerThanArchiveEnd = (Calendar) end.clone();
        newerThanArchiveEnd.add(Calendar.HOUR, 3);
        createDataFile(newerThanArchiveEnd, dataFileNameFormat);
    }

    private void setupTimes() {
        referenceCalendar = TimeUtil.newGmtCalendar();
        referenceCalendar.set(Calendar.MINUTE, 0);
        referenceCalendar.set(Calendar.SECOND, 0);

        archiveStart = (Calendar) referenceCalendar.clone();
        archiveEnd = (Calendar) referenceCalendar.clone();

        archiveStart.add(Calendar.HOUR, -20);
        archiveEnd.add(Calendar.HOUR, -3);

        yyyyMMFormat.setCalendar(referenceCalendar);
        ddFormat.setCalendar(referenceCalendar);
        hhFormat.setCalendar(referenceCalendar);
        mmFormat.setCalendar(referenceCalendar);
    }

    private File createDataFile(Calendar referenceCalendar,
            MessageFormat fileFormat) throws IOException {
        Date referenceTime = referenceCalendar.getTime();

        String yyyyMM = yyyyMMFormat.format(referenceTime);
        String dd = ddFormat.format(referenceTime);
        String hh = hhFormat.format(referenceTime);
        String mm = mmFormat.format(referenceTime);
        String[] formatArgs = new String[] { yyyyMM, dd, hh, mm };

        String filename = fileFormat.format(formatArgs, new StringBuffer(),
                new FieldPosition(0)).toString();
        File resultFile = new File(archive.getRootDir(), filename);
        String dirname = FilenameUtils
                .getFullPath(resultFile.getAbsolutePath());
        File dir = new File(dirname);

        dir.mkdirs();
        resultFile.createNewFile();
        return resultFile;
    }

    private Collection<String> createFileNameListNoRootDir(File rootDir,
            Collection<File> files) {
        List<String> result = new ArrayList<String>(files.size());
        for (File f : files) {
            String absPath = f.getAbsolutePath();
            String fileRelativePath = absPath.replace(
                    rootDir.getAbsolutePath(), "");
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
        CategoryConfig satCategory = getCategory(archive, SAT_CAT_NAME);
        List<DisplayData> displays = 
                manager.getDisplayInfo(archive.getName(), satCategory.getName());
        List<DisplayData> selectedDisplays = new ArrayList<ArchiveConfigManager.DisplayData>();
        for (DisplayData displayData : displays) {
            if (displayData.getDisplayLabel().equals(satNameForArchive)) {
                selectedDisplays.add(displayData);
            }
        }
        Collection<File> archivedFiles = manager.createArchive(archiveDir,
                selectedDisplays, archiveStart, archiveEnd);
        assertEquals(
                "The expected archive files and the archived files are not the same",
                createFileNameListNoRootDir(new File(archive.getRootDir()),
                        archiveFiles),
                createFileNameListNoRootDir(archiveDir, archivedFiles));
        // check archive directory for files.
        Collection<File> filesFoundInArchive = FileUtils.listFiles(archiveDir,
                FileFilterUtils.trueFileFilter(),
                FileFilterUtils.trueFileFilter());
        assertEquals(
                "The expected archive files in the files found in the archive are not the same",
                archivedFiles, filesFoundInArchive);
    }

    @Test
    public void testArchiveManagerPurge() throws IOException {
        Collection<File> filesFoundInPurge = manager
                .purgeExpiredFromArchive(archive);
        // sort for comparison
        List<File> purgeFilesList = new ArrayList<File>(purgeFiles);
        Collections.sort(purgeFilesList);
        List<File> foundFilesList = new ArrayList<File>(filesFoundInPurge);
        Collections.sort(foundFilesList);
        assertEquals(
                "The expected purge files and the files purged are not the same",
                purgeFilesList, foundFilesList);
    }

}
