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
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.FieldPosition;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.RegexFileFilter;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
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
 * May 1, 2013  1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ArchiveConfigManager {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveConfigManager.class);

    private final static ArchiveConfigManager instance = new ArchiveConfigManager();

    public final String ARCHIVE_DIR = "archive";

    private IPathManager pathMgr;

    private final Map<String, ArchiveConfig> archiveMap = new HashMap<String, ArchiveConfig>();

    private Map<ArchiveConfig, Map<CategoryConfig, Map<String, List<File>>>> displayDirMap = new HashMap<ArchiveConfig, Map<CategoryConfig, Map<String, List<File>>>>();

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
     * Obtain the names of the categories for the named archive data.
     * 
     * @param archiveConfigName
     * @return categoryNames
     */
    public String[] getCategoryNames(String archiveConfigName) {
        return getCategoryNames(archiveMap.get(archiveConfigName));
    }

    /**
     * @param archiveConfig
     * @return
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
     * Load the archiveConfig information form the localized file.
     * 
     * @param lFile
     * @return archiveConfig
     * @throws IOException
     * @throws LocalizationException
     */
    public ArchiveConfig loadArchiveData(LocalizationFile lFile)
            throws IOException, LocalizationException {
        return unmarshalArhiveConfigFromXmlFile(lFile);
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
     * Add the file entry to the desired display map.
     * 
     * @param file
     * @param archiveConfig
     * @param categoryConfig
     * @param displayLabel
     */
    private void addDirToDisplayMap(File file, ArchiveConfig archiveConfig,
            CategoryConfig categoryConfig, String displayLabel) {
        Map<CategoryConfig, Map<String, List<File>>> cMap = displayDirMap
                .get(archiveConfig);
        if (cMap == null) {
            cMap = new HashMap<CategoryConfig, Map<String, List<File>>>();
            displayDirMap.put(archiveConfig, cMap);
        }

        Map<String, List<File>> dirMap = cMap.get(categoryConfig);
        if (dirMap == null) {
            dirMap = new HashMap<String, List<File>>();
            cMap.put(categoryConfig, dirMap);
        }

        List<File> fileList = dirMap.get(displayLabel);
        if (fileList == null) {
            fileList = new ArrayList<File>();
            dirMap.put(displayLabel, fileList);
        }
        fileList.add(file);
    }

    /**
     * Get a list of directories associated with the display label.
     * 
     * @param archiveConfig
     * @param categoryConfig
     * @param displayLabel
     * @return
     */
    private File[] getDirFromDisplayMap(ArchiveConfig archiveConfig,
            CategoryConfig categoryConfig, String displayLabel) {
        Map<CategoryConfig, Map<String, List<File>>> cMap = displayDirMap
                .get(archiveConfig);

        File[] result = new File[0];
        if (cMap == null) {
            return result;
        }

        Map<String, List<File>> dirMap = cMap.get(categoryConfig);
        if (dirMap == null) {
            return result;
        }

        List<File> fileList = dirMap.get(displayLabel);
        if (fileList == null) {
            return result;
        }

        return fileList.toArray(result);
    }

    /**
     * Clear maps and reloads the maps from the configuration information.
     * 
     * @throws IOException
     * @throws LocalizationException
     */
    public void reset() throws IOException, LocalizationException {
        clearDisplayMap();
        archiveMap.clear();
        LocalizationFile[] files = getArchiveConfigFiles();
        for (LocalizationFile lFile : files) {
            ArchiveConfig archiveConfig = unmarshalArhiveConfigFromXmlFile(lFile);
            archiveMap.put(archiveConfig.getName(), archiveConfig);
        }
    }

    /**
     * Walk the display directory maps clearing all entries.
     */
    private void clearDisplayMap() {
        for (ArchiveConfig archiveConfig : displayDirMap.keySet()) {
            Map<CategoryConfig, Map<String, List<File>>> cMap = displayDirMap
                    .get(archiveConfig);
            for (CategoryConfig categoryConfig : cMap.keySet()) {
                Map<String, List<File>> dirMap = cMap.get(categoryConfig);
                dirMap.remove(categoryConfig);
                for (String displayLabel : dirMap.keySet()) {
                    List<File> fileList = dirMap.get(displayLabel);
                    fileList.clear();
                }
                dirMap.clear();
            }
        }
    }

    /**
     * Get a list of directories/files for the desired display label bounded by
     * the start and end time inclusive.
     * 
     * @param archiveName
     *            - name of the ArchiveConfig containing the category
     * @param categoryName
     *            - name of the CategoryConfig containing the display
     * @param displayLabel
     *            - the display label
     * @param startCal
     *            - Start time if null epoch time is used
     * @param endCal
     *            - End time if null current simulated time is used.
     * @return files
     */
    public File[] getDisplayFiles(String archiveName, String categoryName,
            String displayLabel, Calendar startCal, Calendar endCal) {
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
        return getDisplayFiles(archiveName, categoryName, displayLabel,
                startTime, endTime);
    }

    /**
     * Get all of the files/directories associated with the display label.
     * 
     * @param archiveName
     * @param categoryName
     * @param displayLabel
     * @return files
     */
    public File[] getDisplayFiles(String archiveName, String categoryName,
            String displayLabel) {
        return getDisplayFiles(archiveName, categoryName, displayLabel, null,
                null);
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
    private File[] getDisplayFiles(String archiveName, String categoryName,
            String displayLabel, long startTime, long endTime) {
        ArchiveConfig archiveConfig = archiveMap.get(archiveName);
        CategoryConfig categoryConfig = findCategory(archiveConfig,
                categoryName);

        String[] indexValues = categoryConfig.getDateGroupIndices().split(
                "\\s*,\\s*");
        int yearIndex = Integer.parseInt(indexValues[0]);
        int monthIndex = Integer.parseInt(indexValues[1]);
        int dayIndex = Integer.parseInt(indexValues[2]);
        int hourIndex = Integer.parseInt(indexValues[3]);

        String filePatternStr = categoryConfig.getFilePattern();

        boolean dirOnly = (filePatternStr == null)
                || ".*".equals(filePatternStr);

        File dirs[] = getDirFromDisplayMap(archiveConfig, categoryConfig,
                displayLabel);

        int beginIndex = archiveConfig.getRootDir().length();

        Calendar fileCal = TimeUtil.newCalendar();
        fileCal.setTimeZone(TimeZone.getTimeZone("UTC"));

        List<File> fileList = new ArrayList<File>();

        if (dirOnly) {
            Pattern pattern = Pattern.compile(categoryConfig.getDirPattern());

            for (File dir : dirs) {
                String path = dir.getAbsolutePath().substring(beginIndex);
                Matcher matcher = pattern.matcher(path);
                if (matcher.matches()) {
                    int year = Integer.parseInt(matcher.group(yearIndex));
                    // Adjust month value to Calendar's 0 - 11
                    int month = Integer.parseInt(matcher.group(monthIndex)) - 1;
                    int day = Integer.parseInt(matcher.group(dayIndex));
                    int hour = Integer.parseInt(matcher.group(hourIndex));
                    fileCal.set(year, month, day, hour, 0, 0);
                    long fileTime = fileCal.getTimeInMillis();
                    if ((startTime <= fileTime) && (fileTime < endTime)) {
                        fileList.add(dir);
                    }
                }
            }
        } else {
            Pattern pattern = Pattern.compile(categoryConfig.getDirPattern()
                    + File.separator + categoryConfig.getFilePattern());
            final Pattern filePattern = Pattern.compile("^" + filePatternStr
                    + "$");
            for (File dir : dirs) {
                List<File> fList = FileUtil.listDirFiles(dir, new FileFilter() {

                    @Override
                    public boolean accept(File pathname) {
                        return filePattern.matcher(pathname.getName())
                                .matches();
                    }
                }, false);
                for (File file : fList) {
                    String path = file.getAbsolutePath().substring(beginIndex);
                    Matcher matcher = pattern.matcher(path);
                    if (matcher.matches()) {
                        int year = Integer.parseInt(matcher.group(yearIndex));
                        // Adjust month value to Calendar's 0 - 11
                        int month = Integer.parseInt(matcher.group(monthIndex)) - 1;
                        int day = Integer.parseInt(matcher.group(dayIndex));
                        int hour = Integer.parseInt(matcher.group(hourIndex));
                        fileCal.set(year, month, day, hour, 0, 0);
                        long fileTime = fileCal.getTimeInMillis();
                        if ((startTime <= fileTime) && (fileTime < endTime)) {
                            fileList.add(dir);
                        }
                    }
                }
            }
        }

        File[] files = fileList.toArray(new File[0]);
        return files;
    }

    /**
     * Get the Display labels matching the pattern for the archive data's
     * category. Assumes the archive data's root directory is the mount point to
     * start the search.
     * 
     * @param archiveName
     * @param categoryName
     * @return sorted array of display labels
     */
    public String[] getDisplayLabels(String archiveName, String categoryName) {
        ArchiveConfig archiveConfig = archiveMap.get(archiveName);
        CategoryConfig categoryConfig = findCategory(archiveConfig,
                categoryName);
        String dirPattern = categoryConfig.getDirPattern();

        File rootFile = new File(archiveConfig.getRootDir());

        String[] subExpr = dirPattern.split(File.separator);
        List<File> dirs = new ArrayList<File>();
        dirs.add(rootFile);
        List<File> tmpDirs = new ArrayList<File>();
        List<File> swpDirs = null;

        for (String regex : subExpr) {
            Pattern subPattern = Pattern.compile("^" + regex + "$");
            IOFileFilter filter = FileFilterUtils
                    .makeDirectoryOnly(new RegexFileFilter(subPattern));

            for (File dir : dirs) {
                File[] list = dir.listFiles();
                if (list != null) {
                    List<File> dirList = Arrays.asList(list);
                    tmpDirs.addAll(Arrays.asList(FileFilterUtils.filter(filter,
                            dirList)));
                }
            }
            swpDirs = dirs;
            dirs = tmpDirs;
            tmpDirs = swpDirs;
            tmpDirs.clear();
        }

        // index for making directory paths' relative to the root path.
        int beginIndex = rootFile.getAbsolutePath().length() + 1;
        Pattern pattern = Pattern.compile("^" + dirPattern + "$");
        TreeSet<String> displays = new TreeSet<String>(
                String.CASE_INSENSITIVE_ORDER);

        MessageFormat msgfmt = new MessageFormat(categoryConfig.getDisplay());
        StringBuffer sb = new StringBuffer();
        FieldPosition pos0 = new FieldPosition(0);

        for (File file : dirs) {
            String path = file.getAbsolutePath().substring(beginIndex);
            Matcher matcher = pattern.matcher(path);
            if (matcher.matches()) {
                sb.setLength(0);
                String[] args = new String[matcher.groupCount() + 1];
                args[0] = matcher.group();
                for (int i = 1; i < args.length; ++i) {
                    args[i] = matcher.group(i);
                }
                String displayLabel = msgfmt.format(args, sb, pos0).toString();
                addDirToDisplayMap(file, archiveConfig, categoryConfig,
                        displayLabel);
                displays.add(displayLabel);
            }
        }

        return displays.toArray(new String[0]);
    }

    /**
     * Obtain the category with the given name.
     * 
     * @param archiveConfig
     * @param categoryName
     * @return
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
        OutputStream stream = null;
        stream = lFile.openOutputStream();
        JAXB.marshal(archiveConfig, stream);
        stream.close();
        lFile.save();
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
            LocalizationFile lFile) throws IOException, LocalizationException {
        ArchiveConfig archiveConfig = null;
        InputStream stream = null;
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
}
