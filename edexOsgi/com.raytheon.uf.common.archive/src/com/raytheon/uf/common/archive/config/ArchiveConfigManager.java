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
package com.raytheon.uf.common.archive.config;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.FieldPosition;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXB;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.RegexFileFilter;

import com.raytheon.uf.common.archive.config.ArchiveConstants.Type;
import com.raytheon.uf.common.archive.config.select.ArchiveSelect;
import com.raytheon.uf.common.archive.config.select.CategorySelect;
import com.raytheon.uf.common.archive.exception.ArchiveException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFileInputStream;
import com.raytheon.uf.common.localization.LocalizationFileOutputStream;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Manager for access to archive data information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May  1, 2013 1966       rferrel     Initial creation
 * May 29, 2013 1965       bgonzale    Added archive creation, purge, and save methods.
 *                                     Updated purgeExpiredFromArchive to check time of files in
 *                                     directory before purging them.
 *                                     Added null check for topLevelDirs in purgeExpiredFromArchive.
 *                                     Changed to use File.delete() instead of Apache FileUtil.deleteQuietly().
 *                                     Added warn logging for failure to delete.
 * Jul 24, 2013 2221       rferrel     Changes for select configuration.
 * Aug 06, 2013 2224       rferrel     Changes to use DataSet.
 * Aug 28, 2013 2299       rferrel     purgeExpiredFromArchive now returns the number of files purged.
 * Dec 04, 2013 2603       rferrel     Changes to improve archive purging.
 * Dec 17, 2013 2603       rjpeter     Fix directory purging.
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ArchiveConfigManager {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveConfigManager.class);

    /** The only instance or this class. */
    private final static ArchiveConfigManager instance = new ArchiveConfigManager();

    /** Localize directory for the archive configuration files. */
    public final String ARCHIVE_DIR = "archiver/purger";

    /** Localization manager. */
    protected IPathManager pathMgr;

    private final Map<String, LocalizationFile> archiveNameToLocalizationFileMap = new HashMap<String, LocalizationFile>();

    /** Mapping of archive configuration data keyed to the name. */
    private final Map<String, ArchiveConfig> archiveMap = new HashMap<String, ArchiveConfig>();

    /** Get the singleton. */
    public final static ArchiveConfigManager getInstance() {
        return instance;
    }

    /**
     * Private constructor for singleton.
     */
    private ArchiveConfigManager() {
        pathMgr = PathManagerFactory.getPathManager();
        try {
            reset();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Get list of site's archive data configuration files.
     * 
     * @return archiveConfigList
     */
    private LocalizationFile[] getArchiveConfigFiles() {
        LocalizationFile[] files = pathMgr.listStaticFiles(ARCHIVE_DIR,
                new String[] { ".xml" }, false, true);
        return files;
    }

    /**
     * Get a sorted list of the archive data names.
     * 
     * @return names
     */
    public String[] getArchiveDataNamesList() {
        String[] names = archiveMap.keySet().toArray(new String[0]);
        Arrays.sort(names, 0, names.length, String.CASE_INSENSITIVE_ORDER);

        return names;
    }

    /**
     * Get a sorted array of the archive's categories.
     * 
     * @param archiveConfigName
     * @return categoryNames
     */
    public String[] getCategoryNames(String archiveConfigName) {
        return getCategoryNames(archiveMap.get(archiveConfigName));
    }

    /**
     * Get a sorted array of the archive's categories.
     * 
     * @param archiveConfig
     * @return categoryNames
     */
    public String[] getCategoryNames(ArchiveConfig archiveConfig) {
        List<CategoryConfig> categories = archiveConfig.getCategoryList();
        List<String> nameList = new ArrayList<String>(categories.size());
        for (CategoryConfig category : archiveConfig.getCategoryList()) {
            String name = category.getName();
            nameList.add(name);
        }
        String[] names = nameList.toArray(new String[0]);
        Arrays.sort(names, 0, names.length, String.CASE_INSENSITIVE_ORDER);
        return names;
    }

    /**
     * Obtain the collection of Archives setting the Categories' selections
     * based on the default retention selections.
     * 
     * @return the Collection of Archives.
     */
    public Collection<ArchiveConfig> getArchives() {
        String fileName = ArchiveConstants.selectFileName(Type.Retention, null);
        SelectConfig selections = loadSelection(fileName);
        if ((selections != null) && !selections.isEmpty()) {
            for (ArchiveSelect archiveSelect : selections.getArchiveList()) {
                String archiveName = archiveSelect.getName();
                ArchiveConfig archiveConfig = archiveMap.get(archiveName);
                if (archiveConfig == null) {
                    statusHandler.handle(Priority.WARN,
                            "Archive Configuration [" + archiveName
                                    + "] not found. Skipping selections.");
                    continue;
                }

                for (CategorySelect categorySelect : archiveSelect
                        .getCategorySelectList()) {
                    String categoryname = categorySelect.getName();
                    CategoryConfig categoryConfig = archiveConfig
                            .getCategory(categoryname);
                    if (categoryConfig == null) {
                        statusHandler.handle(Priority.WARN,
                                "Archive Configuration [" + archiveName
                                        + "] Category [" + categoryname
                                        + "] not found. Skipping selections.");
                        continue;
                    }
                    categoryConfig.setSelectedDisplayNames(categorySelect
                            .getSelectSet());
                }
            }
        }
        return archiveMap.values();
    }

    /**
     * Create an archive in the given archive directory from the Files filtered
     * by the ArchiveElements that are within the start and end time frame
     * parameters.
     * 
     * @param archiveDir
     *            Destination directory of the Archive
     * @param displaysSelectedForArchive
     *            List of display elements to filter inclusion into Archive
     * @param start
     *            Start time boundary of the Archive
     * @param end
     *            End time boundary of the Archive
     * @return the list of files added to the created archive.
     * @throws IOException
     * @throws ArchiveException
     */
    public Collection<File> createArchive(File archiveDir,
            Collection<DisplayData> displaysSelectedForArchive, Calendar start,
            Calendar end) throws IOException, ArchiveException {
        Collection<File> archivedFiles = new ArrayList<File>();
        FileUtils.forceMkdir(archiveDir);
        if (archiveDir.exists()) {
            for (DisplayData display : displaysSelectedForArchive) {
                String archiveDirString = archiveDir.getAbsolutePath();
                String rootDirString = display.archiveConfig.getRootDir();

                rootDirString = (rootDirString.endsWith(File.separator) ? rootDirString
                        .substring(0,
                                rootDirString.lastIndexOf(File.separatorChar))
                        : rootDirString);
                for (File srcFile : getDisplayFiles(display, start, end)) {
                    String fileAbsPath = srcFile.getAbsolutePath();
                    File newArchiveDir = getDirRelativeToArchiveDirFromRoot(
                            srcFile.isDirectory(), fileAbsPath, rootDirString,
                            archiveDirString);
                    FileUtils.forceMkdir(newArchiveDir);

                    if (srcFile.isDirectory()) {
                        FileUtils.copyDirectory(srcFile, newArchiveDir);
                        archivedFiles.addAll(Arrays.asList(newArchiveDir
                                .listFiles()));
                    } else {
                        FileUtils.copyFileToDirectory(srcFile, newArchiveDir);
                        String filename = FilenameUtils.getName(fileAbsPath);
                        File dstFile = new File(newArchiveDir, filename);
                        archivedFiles.add(dstFile);
                    }
                }
            }
        } else {
            StringBuilder sbuff = new StringBuilder(
                    "Failed to create archive in: ");
            sbuff.append(archiveDir);
            throw new ArchiveException(sbuff.toString());
        }
        return archivedFiles;
    }

    private File getDirRelativeToArchiveDirFromRoot(boolean fileIsDir,
            String fileAbsPath, String rootDirString, String archiveDirString) {
        String path = null;
        if (fileIsDir) {
            path = fileAbsPath;
        } else {
            path = FilenameUtils.getFullPath(fileAbsPath);
        }
        String newArchivePathString = path.replace(rootDirString,
                archiveDirString);
        return new File(newArchivePathString);
    }

    /**
     * Purge the Files that fall outside of the time frame constraints for the
     * archive. This will always leave the archive's top level directories even
     * when they are empty.
     * 
     * @param archive
     * @return purgeCount
     */
    public int purgeExpiredFromArchive(ArchiveConfig archive) {
        String archiveRootDirPath = archive.getRootDir();
        File archiveRootDir = new File(archiveRootDirPath);
        int purgeCount = 0;

        if (!archiveRootDir.isDirectory()) {
            statusHandler.error(archiveRootDir.getAbsolutePath()
                    + " not a directory.");
            return purgeCount;
        }

        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            statusHandler.info("Purging directory: \""
                    + archiveRootDir.getAbsolutePath() + "\".");
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            String message = String.format(
                    "Start setup of category date helpers for archive: %s.",
                    archive.getName());
            statusHandler.debug(message);
        }

        Map<CategoryConfig, CategoryFileDateHelper> helperMap = new HashMap<CategoryConfig, CategoryFileDateHelper>();
        for (CategoryConfig category : archive.getCategoryList()) {
            CategoryFileDateHelper helper = new CategoryFileDateHelper(category);
            helperMap.put(category, helper);
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            String message = String.format(
                    "End setup of category date helpers for archive: %s.",
                    archive.getName());
            statusHandler.debug(message);
        }

        final Calendar minPurgeTime = calculateExpiration(archive, null);

        IOFileFilter defaultTimeFilter = new IOFileFilter() {

            @Override
            public boolean accept(File dir, String name) {
                File file = new File(dir, name);
                return accept(file);
            }

            @Override
            public boolean accept(File file) {
                Calendar time = TimeUtil.newGmtCalendar();
                time.setTimeInMillis(file.lastModified());
                return time.compareTo(minPurgeTime) < 0;
            }
        };

        File[] topLevelFiles = archiveRootDir.listFiles();
        for (File topFile : topLevelFiles) {
            // In top level directory ignore all hidden files and directories.
            if (!topFile.isHidden()) {
                if (topFile.isDirectory()) {
                    boolean isInCategory = false;
                    for (CategoryConfig category : archive.getCategoryList()) {
                        CategoryFileDateHelper helper = helperMap.get(category);

                        if (helper.isCategoryDirectory(topFile.getName())) {
                            isInCategory = true;
                            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                                String message = String
                                        .format("Start purge of category %s - %s, directory \"%s\".",
                                                archive.getName(),
                                                category.getName(),
                                                topFile.getAbsolutePath());
                                statusHandler.info(message);
                            }

                            final Calendar extPurgeTime = calculateExpiration(
                                    archive, category);
                            int pc = purgeDir(topFile, defaultTimeFilter,
                                    minPurgeTime, extPurgeTime, helper,
                                    category);
                            purgeCount += pc;
                            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                                String message = String
                                        .format("End purge of category %s - %s, directory \"%s\", deleted %d files and directories.",
                                                archive.getName(),
                                                category.getName(),
                                                topFile.getAbsolutePath(), pc);
                                statusHandler.info(message);
                            }
                            break;
                        }
                    }
                    if (isInCategory == false) {
                        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                            String message = String.format(
                                    "Start purge of directory: \"%s\".",
                                    topFile.getAbsolutePath());
                            statusHandler.info(message);
                        }
                        int pc = purgeDir(topFile, defaultTimeFilter);
                        purgeCount += pc;
                        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                            String message = String
                                    .format("End purge of directory: \"%s\", deleted %d files and directories.",
                                            topFile.getAbsolutePath(), pc);
                            statusHandler.info(message);
                        }
                    }
                } else {
                    if (defaultTimeFilter.accept(topFile)) {
                        purgeCount += deleteFile(topFile);
                    }
                }
            }
        }
        return purgeCount;
    }

    /**
     * Purge the contents of a directory of expired data leaving a possibly
     * empty directory.
     * 
     * @param dir
     * @param defaultTimeFilter
     * @param minPurgeTime
     * @param extPurgeTime
     * @param helper
     * @return purgerCount
     */
    private int purgeDir(File dir, IOFileFilter defaultTimeFilter,
            Calendar minPurgeTime, Calendar extPurgeTime,
            CategoryFileDateHelper helper, CategoryConfig category) {
        int purgeCount = 0;

        for (File file : dir.listFiles()) {
            if (!file.isHidden()) {
                DataSetStatus status = helper.getFileDate(file);
                if (status.isInDataSet()) {
                    Collection<String> labels = category
                            .getSelectedDisplayNames();
                    boolean isSelected = false;
                    for (String label : status.getDisplayLabels()) {
                        if (labels.contains(label)) {
                            isSelected = true;
                            break;
                        }
                    }

                    Calendar checkTime = (isSelected ? extPurgeTime
                            : minPurgeTime);
                    Calendar fileTime = status.getTime();
                    boolean purge = fileTime.compareTo(checkTime) < 0;

                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        String message = String
                                .format("%s [%s] category [%s] %s retention [%s] checkTime [%s] = %s.",
                                        (file.isDirectory() ? "Directory"
                                                : "File"), file
                                                .getAbsoluteFile(), category
                                                .getName(), (isSelected ? "ext"
                                                : "min"), TimeUtil
                                                .formatCalendar(checkTime),
                                        TimeUtil.formatCalendar(fileTime),
                                        (purge ? "purge" : "retain"));
                        statusHandler.debug(message);
                    }

                    if (purge) {
                        if (file.isDirectory()) {
                            purgeCount += purgeDir(file,
                                    FileFilterUtils.trueFileFilter());
                            if (file.list().length == 0) {
                                purgeCount += purgeDir(file,
                                        FileFilterUtils.trueFileFilter());
                            }
                        } else {
                            purgeCount += deleteFile(file);
                        }
                    }
                } else if (file.isDirectory()) {
                    purgeCount += purgeDir(file, defaultTimeFilter,
                            minPurgeTime, extPurgeTime, helper, category);
                    if (file.list().length == 0) {
                        purgeCount += deleteFile(file);
                    }
                } else if (defaultTimeFilter.accept(file)) {
                    purgeCount += deleteFile(file);
                }
            }
        }

        return purgeCount;
    }

    /**
     * Recursively purge the contents of a directory based on the filter. The
     * directory in the initial call is not deleted. This may result in an empty
     * directory which is the desired result for top level directories.
     * 
     * 
     * @param dir
     * @param fileDataFilter
     * @return purgeCount
     */
    private int purgeDir(File dir, IOFileFilter fileDataFilter) {
        int purgeCount = 0;
        for (File file : dir.listFiles()) {
            if (!file.isHidden()) {
                if (file.isDirectory()) {
                    purgeCount += purgeDir(file, fileDataFilter);
                    if (file.list().length == 0) {
                        purgeCount += deleteFile(file);
                    }
                } else if (fileDataFilter.accept(file)) {
                    purgeCount += deleteFile(file);
                }
            }
        }
        return purgeCount;
    }

    /**
     * Delete a file or directory.
     * 
     * @param file
     * @return purgeCount
     */
    private int deleteFile(File file) {
        int purgeCount = 0;
        boolean isDir = file.isDirectory();
        if (file.delete()) {
            ++purgeCount;
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler
                        .debug(String.format("Purged %s: \"%s\"",
                                (isDir ? "directory" : "file"),
                                file.getAbsolutePath()));
            }
        } else {
            statusHandler.warn(String.format("Failed to purge %s: \"%s\"",
                    (isDir ? "directory" : "file"), file.getAbsolutePath()));
        }
        return purgeCount;
    }

    /**
     * Get expiration time for the category.
     * 
     * @param archive
     * @param category
     * @return expireCal
     */
    private Calendar calculateExpiration(ArchiveConfig archive,
            CategoryConfig category) {
        Calendar expireCal = TimeUtil.newGmtCalendar();
        int retHours = (category == null)
                || (category.getRetentionHours() == 0) ? archive
                .getRetentionHours() : category.getRetentionHours();
        if (retHours != 0) {
            expireCal.add(Calendar.HOUR, (-1) * retHours);
        }
        return expireCal;
    }

    /**
     * @return the archive with the given name; return null if it does not
     *         exist.
     */
    public ArchiveConfig getArchive(String archiveName) {
        return archiveMap.get(archiveName);
    }

    /**
     * Save the Archive Configuration data to the localized file.
     * 
     * @param lFile
     * @param archiveConfig
     * @throws IOException
     * @throws LocalizationException
     */
    public void saveArchiveConfig(LocalizationFile lFile,
            ArchiveConfig archiveConfig) {
        try {
            marshalArchiveConfigToXmlFile(archiveConfig, lFile);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Clear maps and reloads the maps from the configuration information.
     * 
     * @throws IOException
     * @throws LocalizationException
     */
    public void reset() {
        archiveMap.clear();
        archiveNameToLocalizationFileMap.clear();
        LocalizationFile[] files = getArchiveConfigFiles();
        for (LocalizationFile lFile : files) {
            try {
                ArchiveConfig archiveConfig = unmarshalArhiveConfigFromXmlFile(lFile);
                if ((archiveConfig != null) && archiveConfig.isValid()) {
                    archiveNameToLocalizationFileMap.put(
                            archiveConfig.getName(), lFile);
                    archiveMap.put(archiveConfig.getName(), archiveConfig);
                } else {
                    statusHandler.handle(Priority.ERROR,
                            "Bad Archive configuration file: "
                                    + lFile.getFile().getName());
                }
            } catch (DataBindingException ex) {
                statusHandler.handle(Priority.ERROR,
                        "Bad Archive configuration file \""
                                + lFile.getFile().getName() + "\": ", ex);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Save the cached configuration.
     */
    public void save() {
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        for (String archiveName : archiveMap.keySet()) {
            ArchiveConfig archiveConfig = archiveMap.get(archiveName);
            LocalizationFile lFile = archiveNameToLocalizationFileMap
                    .get(archiveName);
            if (lFile.getContext().getLocalizationLevel() != LocalizationLevel.SITE) {
                // Modify the site not the base file.
                LocalizationFile tlFile = pathMgr.getLocalizationFile(
                        siteContext, lFile.getName());
                lFile = tlFile;
            }
            saveArchiveConfig(lFile, archiveConfig);
        }
    }

    /**
     * Get a list of directories/files for the desired display label bounded by
     * the start and end time inclusive.
     * 
     * @param displayData
     * @param startCal
     * @param endCal
     * @return files
     */
    public List<File> getDisplayFiles(DisplayData displayData,
            Calendar startCal, Calendar endCal) {
        long startTime = 0L;
        if (startCal != null) {
            // Set to beginning of the hour
            Calendar cal = (Calendar) startCal.clone();
            cal.set(Calendar.MILLISECOND, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            startTime = cal.getTimeInMillis();
        }

        Calendar cal = null;
        if (endCal != null) {
            cal = (Calendar) endCal.clone();
        } else {
            cal = TimeUtil.newCalendar();
        }

        // Set to start of the next hour.
        cal.set(Calendar.MILLISECOND, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.add(Calendar.HOUR_OF_DAY, 1);

        long endTime = cal.getTimeInMillis();
        return getDisplayFiles(displayData, startTime, endTime);
    }

    /**
     * Get all of the files/directories associated with the display label.
     * 
     * @param archiveName
     * @param categoryName
     * @param displayLabel
     * @return files
     */
    public List<File> getDisplayFiles(DisplayData displayData) {
        return getDisplayFiles(displayData, null, null);
    }

    /**
     * Get the files/directories associated with the display label with time
     * stamps between inclusive start time and exclusive end time.
     * 
     * @param archiveName
     * @param categoryName
     * @param displayLabel
     * @param startTime
     * @param endTime
     * @return files
     */
    private List<File> getDisplayFiles(DisplayData displayData, long startTime,
            long endTime) {
        List<File> fileList = new LinkedList<File>();
        ArchiveConfig archiveConfig = displayData.archiveConfig;

        for (CategoryDataSet dataSet : displayData.dataSets) {

            int[] timeIndices = dataSet.getTimeIndices();

            String filePatternStr = dataSet.getFilePattern();

            boolean dirOnly = dataSet.isDirOnly();

            List<File> dirs = displayData.dirsMap.get(dataSet);

            int beginIndex = archiveConfig.getRootDir().length();

            Calendar fileCal = TimeUtil.newCalendar();
            fileCal.setTimeZone(TimeZone.getTimeZone("UTC"));

            if (dirOnly) {
                for (String dirPattern : dataSet.getDirPatterns()) {
                    Pattern pattern = dataSet.getPattern(dirPattern);

                    for (File dir : dirs) {
                        String path = dir.getAbsolutePath().substring(
                                beginIndex);
                        Matcher matcher = pattern.matcher(path);
                        if (matcher.matches()) {
                            Long fileTime = dataSet.getMatchTimeInMilliseconds(
                                    timeIndices, matcher);
                            if (fileTime == null) {
                                fileTime = dir.lastModified();
                            }
                            if ((startTime <= fileTime) && (fileTime < endTime)) {
                                fileList.add(dir);
                            }
                        }
                    }
                }
            } else {
                for (String dirPattern : dataSet.getDirPatterns()) {
                    Pattern pattern = dataSet.getPattern(dirPattern);
                    final Pattern filePattern = Pattern.compile("^"
                            + filePatternStr + "$");
                    for (File dir : dirs) {
                        List<File> fList = FileUtil.listDirFiles(dir,
                                new FileFilter() {

                                    @Override
                                    public boolean accept(File pathname) {
                                        return filePattern.matcher(
                                                pathname.getName()).matches();
                                    }
                                }, false);
                        for (File file : fList) {
                            String path = file.getAbsolutePath().substring(
                                    beginIndex);
                            Matcher matcher = pattern.matcher(path);
                            if (matcher.matches()) {
                                Long timestamp = dataSet
                                        .getMatchTimeInMilliseconds(
                                                timeIndices, matcher);
                                long fileTime = timestamp == null ? file
                                        .lastModified() : timestamp.longValue();
                                if ((startTime <= fileTime)
                                        && (fileTime < endTime)) {
                                    fileList.add(file);
                                }
                            }
                        }
                    }
                }
            }
        }
        return fileList;
    }

    /**
     * Get a list of directories matching the categories directory patterns that
     * are sub-directories of the archive's root directory.
     * 
     * @param archiveConfig
     * @param categoryConfig
     * @return dirs
     */
    private Map<CategoryDataSet, List<File>> getDirs(File rootFile,
            CategoryConfig categoryConfig) {
        List<File> resultDirs = null;
        List<File> dirs = new ArrayList<File>();
        List<File> tmpDirs = new ArrayList<File>();
        List<File> swpDirs = null;
        List<CategoryDataSet> dataSets = categoryConfig.getDataSetList();
        Map<CategoryDataSet, List<File>> rval = new HashMap<CategoryDataSet, List<File>>(
                dataSets.size(), 1);

        // keep an in memory map since some of the categories cause the same
        // directories to be listed over and over
        Map<File, List<File>> polledDirs = new HashMap<File, List<File>>();

        for (CategoryDataSet dataSet : dataSets) {
            resultDirs = new LinkedList<File>();

            for (String dirPattern : dataSet.getDirPatterns()) {
                String[] subExpr = dirPattern.split(File.separator);
                dirs.clear();
                dirs.add(rootFile);
                tmpDirs.clear();

                for (String regex : subExpr) {
                    Pattern subPattern = Pattern.compile("^" + regex + "$");
                    IOFileFilter filter = FileFilterUtils
                            .makeDirectoryOnly(new RegexFileFilter(subPattern));

                    for (File dir : dirs) {
                        List<File> dirList = polledDirs.get(dir);
                        if (dirList == null) {
                            File[] list = dir.listFiles();
                            dirList = Arrays.asList(list);
                            polledDirs.put(dir, dirList);
                        }

                        if (dirList != null) {
                            tmpDirs.addAll(FileFilterUtils.filterList(filter,
                                    dirList));
                        }
                    }

                    swpDirs = dirs;
                    dirs = tmpDirs;
                    tmpDirs = swpDirs;
                    tmpDirs.clear();
                }

                resultDirs.addAll(dirs);
            }
            rval.put(dataSet, resultDirs);
        }

        return rval;
    }

    /**
     * Get the Display labels matching the pattern for the archive data's
     * category. Assumes the archive data's root directory is the mount point to
     * start the search.
     * 
     * @param archiveName
     * @param categoryName
     * @param setSelect
     *            - when true set the displayData selection base on category's
     *            selection list
     * @return displayDataList
     */
    public List<DisplayData> getDisplayData(String archiveName,
            String categoryName, boolean setSelect) {
        Map<String, List<File>> displayMap = new HashMap<String, List<File>>();

        ArchiveConfig archiveConfig = archiveMap.get(archiveName);
        String rootDirName = archiveConfig.getRootDir();
        CategoryConfig categoryConfig = findCategory(archiveConfig,
                categoryName);
        File rootFile = new File(rootDirName);
        TreeMap<String, DisplayData> displays = new TreeMap<String, DisplayData>();
        Map<CategoryDataSet, List<File>> dirMap = getDirs(rootFile,
                categoryConfig);
        for (CategoryDataSet dataSet : categoryConfig.getDataSetList()) {
            List<String> dataSetDirPatterns = dataSet.getDirPatterns();
            List<File> dirs = dirMap.get(dataSet);

            int beginIndex = rootFile.getAbsolutePath().length() + 1;
            List<Pattern> patterns = new ArrayList<Pattern>(
                    dataSetDirPatterns.size());

            for (String dirPattern : dataSetDirPatterns) {
                Pattern pattern = Pattern.compile("^" + dirPattern + "$");
                patterns.add(pattern);
            }

            MessageFormat msgfmt = new MessageFormat(dataSet.getDisplayLabel());
            StringBuffer sb = new StringBuffer();
            FieldPosition pos0 = new FieldPosition(0);

            for (File dir : dirs) {
                String path = dir.getAbsolutePath().substring(beginIndex);
                for (Pattern pattern : patterns) {
                    Matcher matcher = pattern.matcher(path);
                    if (matcher.matches()) {
                        sb.setLength(0);
                        String[] args = new String[matcher.groupCount() + 1];
                        args[0] = matcher.group();
                        for (int i = 1; i < args.length; ++i) {
                            args[i] = matcher.group(i);
                        }
                        String displayLabel = msgfmt.format(args, sb, pos0)
                                .toString();
                        List<File> displayDirs = displayMap.get(displayLabel);
                        if (displayDirs == null) {
                            displayDirs = new ArrayList<File>();
                            displayMap.put(displayLabel, displayDirs);
                        }
                        displayDirs.add(dir);
                        DisplayData displayData = displays.get(displayLabel);
                        if (displayData == null) {
                            displayData = new DisplayData(archiveConfig,
                                    categoryConfig, dataSet, displayLabel);
                            displays.put(displayLabel, displayData);
                        } else if (!displayData.dataSets.contains(dataSet)) {
                            displayData.dataSets.add(dataSet);
                        }

                        displayData.dirsMap.put(dataSet, displayDirs);
                        break;
                    }
                }
            }
        }

        List<DisplayData> displayDataList = new ArrayList<DisplayData>(
                displays.size());

        displayDataList.addAll(displays.values());

        return displayDataList;
    }

    /**
     * Obtain the category with the given name.
     * 
     * @param archiveConfig
     * @param categoryName
     * @return categoryConfig
     */
    private CategoryConfig findCategory(ArchiveConfig archiveConfig,
            String categoryName) {
        for (CategoryConfig category : archiveConfig.getCategoryList()) {
            if (category.getName().equals(categoryName)) {
                return category;
            }
        }
        return null;
    }

    /**
     * This does the work of saving an instance of ArchiveConfig to a localized
     * file.
     * 
     * @param archiveConfig
     * @param lFile
     * @throws IOException
     * @throws LocalizationException
     */
    private void marshalArchiveConfigToXmlFile(ArchiveConfig archiveConfig,
            LocalizationFile lFile) throws IOException, LocalizationException {
        LocalizationFileOutputStream stream = null;
        stream = lFile.openOutputStream();
        JAXB.marshal(archiveConfig, stream);
        stream.closeAndSave();
    }

    /**
     * This does the work of taking a localized file and returning a instance of
     * the ArchiveConfig class from the data in the file.
     * 
     * @param lFile
     * @return archiveConfig
     * @throws IOException
     * @throws LocalizationException
     */
    private ArchiveConfig unmarshalArhiveConfigFromXmlFile(
            LocalizationFile lFile) throws IOException, LocalizationException,
            DataBindingException {
        ArchiveConfig archiveConfig = null;
        LocalizationFileInputStream stream = null;
        try {
            stream = lFile.openInputStream();
            archiveConfig = JAXB.unmarshal(stream, ArchiveConfig.class);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
        return archiveConfig;
    }

    /**
     * Delete the selection localized site configuration file.
     * 
     * @param fileName
     * @throws LocalizationOpFailedException
     */
    public void deleteSelection(String fileName)
            throws LocalizationOpFailedException {
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pathMgr.getLocalizationFile(siteContext,
                ARCHIVE_DIR + "/" + fileName);
        lFile.delete();
    }

    /**
     * Load the localized site select configuration file.
     * 
     * @param fileName
     * @return selectConfig
     */
    public SelectConfig loadSelection(String fileName) {
        SelectConfig selections = null;
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pathMgr.getLocalizationFile(siteContext,
                ARCHIVE_DIR + "/" + fileName);
        if (lFile.exists()) {
            FileInputStream stream = null;
            try {
                stream = lFile.openInputStream();
                selections = unmarshallSelectionStream(stream);
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } finally {
                if (stream != null) {
                    try {
                        stream.close();
                    } catch (IOException ex) {
                        // Ignore
                    }
                }
            }
        }
        return selections;
    }

    // TODO possible future methods for supporting importing and exporting
    // select configurations.
    //
    // public SelectConfig importSelections(File selectFile) throws IOException
    // {
    // SelectConfig selections = null;
    // FileInputStream stream = new FileInputStream(selectFile);
    // selections = unmarshallSelectionStream(stream);
    // return selections;
    // }
    //
    // public boolean exportSelections(SelectConfig selections, File selectFile)
    // throws IOException, LocalizationException {
    // FileOutputStream stream = null;
    // try {
    // stream = new FileOutputStream(selectFile);
    // marshalSelectStream(selections, stream);
    // } finally {
    // if (stream != null) {
    // try {
    // stream.close();
    // } catch (IOException ex) {
    // // Ignore
    // }
    // }
    // }
    // return false;
    // }

    /**
     * Get a list of selection names based on the select configuration files in
     * the type's directory.
     * 
     * @param type
     * @return selectNames
     */
    public String[] getSelectionNames(ArchiveConstants.Type type) {
        LocalizationFile[] files = pathMgr.listStaticFiles(ARCHIVE_DIR
                + IPathManager.SEPARATOR + type.selectionDir,
                new String[] { ArchiveConstants.configFileExt }, false, true);
        String[] names = new String[files.length];
        int extLen = ArchiveConstants.configFileExt.length();
        int i = 0;
        StringBuilder sb = new StringBuilder();
        for (LocalizationFile lFile : files) {
            sb.setLength(0);
            sb.append(lFile.getName());
            sb.setLength(sb.length() - extLen);
            names[i] = sb.substring(sb.lastIndexOf(IPathManager.SEPARATOR) + 1);
            ++i;
        }
        Arrays.sort(names, String.CASE_INSENSITIVE_ORDER);
        return names;
    }

    /**
     * Save the selections configuration in the desired file.
     * 
     * @param selections
     * @param fileName
     * @throws LocalizationException
     * @throws IOException
     */
    public void saveSelections(SelectConfig selections, String fileName)
            throws LocalizationException, IOException {
        LocalizationFileOutputStream stream = null;
        LocalizationContext siteContext = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pathMgr.getLocalizationFile(siteContext,
                ARCHIVE_DIR + IPathManager.SEPARATOR + fileName);

        try {
            stream = lFile.openOutputStream();
            marshalSelectStream(selections, stream);
        } finally {
            if (stream != null) {
                try {
                    stream.closeAndSave();
                } catch (Exception ex) {
                    // Ignore
                }
            }
        }
    }

    /**
     * load select configuration from the stream.
     * 
     * @param stream
     * @return selectConfig
     * @throws IOException
     */
    private SelectConfig unmarshallSelectionStream(FileInputStream stream)
            throws IOException {
        SelectConfig selections = null;
        try {
            selections = JAXB.unmarshal(stream, SelectConfig.class);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
        return selections;
    }

    /**
     * Save select configuration to the desired stream.
     * 
     * @param selections
     * @param stream
     * @throws IOException
     * @throws LocalizationException
     */
    private void marshalSelectStream(SelectConfig selections,
            FileOutputStream stream) throws IOException, LocalizationException {
        JAXB.marshal(selections, stream);
    }
}
