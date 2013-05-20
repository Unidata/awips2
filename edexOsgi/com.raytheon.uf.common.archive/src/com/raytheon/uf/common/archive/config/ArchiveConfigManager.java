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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
    private final static ArchiveConfigManager instance = new ArchiveConfigManager();

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveConfigManager.class);

    public final String ARCHIVE_DIR = "archive";

    private IPathManager pathMgr;

    private final Map<String, ArchiveConfig> archiveMap = new HashMap<String, ArchiveConfig>();

    private DirectoryFilter directoryFilter = new DirectoryFilter();

    private Pattern subPatterns[] = new Pattern[] { Pattern.compile("\\1"),
            Pattern.compile("\\2"), Pattern.compile("\\3"),
            Pattern.compile("\\4"), Pattern.compile("\\5"),
            Pattern.compile("\\6"), Pattern.compile("\\7"),
            Pattern.compile("\\8"), Pattern.compile("\\9"), };

    private Pattern yearPattern = Pattern.compile("$\\{YYYY}");

    private Pattern monthPattern = Pattern.compile("$\\{MM}");

    private Pattern dayPattern = Pattern.compile("$\\{DD}");

    private Pattern hourPattern = Pattern.compile("$\\{HH}");

    public final static ArchiveConfigManager getInstance() {
        return instance;
    }

    private ArchiveConfigManager() {
        pathMgr = PathManagerFactory.getPathManager();
    }

    /**
     * Get list of site's archive data configuration files.
     * 
     * @return archiveConfigList
     */
    private LocalizationFile[] getArchiveConfigFiles() {

        Map<String, LocalizationFile> confFileMap = new HashMap<String, LocalizationFile>();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        LocalizationFile[] files = pathMgr.listFiles(ctx, ARCHIVE_DIR,
                new String[] { ".xml" }, false, true);

        for (LocalizationFile lf : files) {
            confFileMap.put(lf.getName().trim(), lf);
        }

        ctx = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        files = pathMgr.listFiles(ctx, ARCHIVE_DIR, new String[] { ".xml" },
                false, true);
        for (LocalizationFile lf : files) {
            confFileMap.put(lf.getName().trim(), lf);
        }

        files = new LocalizationFile[confFileMap.size()];
        int i = 0;
        for (String key : confFileMap.keySet()) {
            files[i] = confFileMap.get(key);
            ++i;
        }
        return files;
    }

    public String[] getArchiveDataNamesList() throws IOException,
            LocalizationException {
        if (archiveMap.size() == 0) {
            LocalizationFile[] files = getArchiveConfigFiles();
            for (LocalizationFile lFile : files) {
                ArchiveConfig archiveConfig = unmarshalArhiveConfigFromXmlFile(lFile);
                archiveMap.put(archiveConfig.getName(), archiveConfig);
            }
        }
        String[] names = archiveMap.keySet().toArray(new String[0]);
        Arrays.sort(names, 0, names.length, String.CASE_INSENSITIVE_ORDER);

        return names;
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

    public ArchiveConfig loadArchiveData(LocalizationFile lFile)
            throws IOException, LocalizationException {
        return unmarshalArhiveConfigFromXmlFile(lFile);
    }

    public void saveArchiveConfig(LocalizationFile lFile,
            ArchiveConfig archiveConfig) throws IOException,
            LocalizationException {
        marshalArchiveConfigToXmlFile(archiveConfig, lFile);
    }

    private void marshalArchiveConfigToXmlFile(ArchiveConfig archiveConfig,
            LocalizationFile lFile) throws IOException, LocalizationException {
        OutputStream stream = null;
        try {
            stream = lFile.openOutputStream();
            JAXB.marshal(archiveConfig, stream);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
    }

    public File[] getDisplayFiles(String archiveName, String categoryName,
            List<String> dispalyList, Calendar startTime, Calendar endTime) {
        // TODO implement
        return null;
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
        CategoryConfig category = findCategory(archiveConfig, categoryName);

        File rootFile = new File(archiveConfig.getRootDir());

        List<File> dirList = findDirs(rootFile);

        Pattern pattern = Pattern.compile(category.getDirPattern());
        TreeSet<String> displays = new TreeSet<String>(
                String.CASE_INSENSITIVE_ORDER);

        StringBuffer sb = new StringBuffer();
        for (File dir : dirList) {
            Matcher matcher = pattern.matcher(dir.getAbsolutePath());
            if (matcher.matches()) {
                String display = category.getDisplay();
                int groupCnt = matcher.groupCount();

                for (int i = 0; i < groupCnt; ++i) {
                    Matcher subMatcher = subPatterns[i].matcher(display);
                    sb.setLength(0);
                    while (subMatcher.find()) {
                        subMatcher.appendReplacement(sb, matcher.group(i));
                    }
                    subMatcher.appendTail(sb);
                    display = sb.toString();
                }
                displays.add(display);
            }
        }

        return displays.toArray(new String[0]);
    }

    private List<File> findDirs(File parentDir) {
        return FileUtil.listDirFiles(parentDir, directoryFilter, true);
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

    private class DirectoryFilter implements FileFilter {

        /*
         * (non-Javadoc)
         * 
         * @see java.io.FileFilter#accept(java.io.File)
         */
        @Override
        public boolean accept(File pathname) {
            if (pathname.isDirectory() && !pathname.getName().startsWith(".")) {
                return true;
            }
            return false;
        }
    }
}
