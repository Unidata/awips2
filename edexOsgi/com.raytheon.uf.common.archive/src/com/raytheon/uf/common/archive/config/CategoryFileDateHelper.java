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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.FilenameUtils;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * File date helper for CategoryConfig objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2013 1965       bgonzale    Initial creation
 * Aug 03, 2013 2224       rferrel     Changes for new configuration files.
 * Aug 28, 2013 2299       rferrel     Changes in IFileDateHelper.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class CategoryFileDateHelper implements IFileDateHelper {
    /**
     * Date information derived from each of a Category's dirPatterns.
     */
    private static class CategoryDateInfo {
        private final Pattern datePattern;

        private final Pattern categoryTopLevelDirPattern;

        private final CategoryDataSet.TimeType timeType;

        private final boolean isDirOnly;

        private final int[] timeIndices;

        /**
         * Initialization constructor.
         * 
         * @param datePattern
         * @param categoryTopLevelDirPattern
         * @param yearIndex
         * @param monthIndex
         * @param dayIndex
         * @param hourIndex
         */
        public CategoryDateInfo(Pattern datePattern,
                Pattern categoryTopLevelDirPattern,
                CategoryDataSet.TimeType timeType, boolean isDirOnly,
                int[] timeIndices) {
            this.datePattern = datePattern;
            this.categoryTopLevelDirPattern = categoryTopLevelDirPattern;
            this.timeType = timeType;
            this.isDirOnly = isDirOnly;
            this.timeIndices = timeIndices;
        }

    }

    private final List<CategoryDateInfo> dateInfoList;

    private final String rootDir;

    /**
     * Initialization constructor.
     * 
     * @param config
     * @param rootDirPattern
     *            categoryTopLevelDirPattern
     */
    public CategoryFileDateHelper(CategoryConfig config, String rootDir) {
        this.rootDir = rootDir;
        List<CategoryDataSet> categoryDataSetList = config.getDataSetList();
        int size = 0;
        for (CategoryDataSet dataSet : categoryDataSetList) {
            size += dataSet.getDirPatterns().size();
        }

        this.dateInfoList = new ArrayList<CategoryFileDateHelper.CategoryDateInfo>(
                size);

        boolean isDirOnly;
        CategoryDataSet.TimeType timeType;
        for (CategoryDataSet dataSet : categoryDataSetList) {
            isDirOnly = dataSet.isDirOnly();
            timeType = dataSet.getTimeType();

            for (String patternString : dataSet.getDirPatterns()) {
                Pattern datePattern = dataSet.getPattern(patternString);
                int dirSeparatorIndex = patternString
                        .indexOf(File.separatorChar);
                patternString = dirSeparatorIndex > patternString.length()
                        || dirSeparatorIndex < 0 ? patternString
                        : patternString.substring(0, dirSeparatorIndex);
                Pattern categoryTopLevelDirPattern = Pattern
                        .compile(patternString);
                int[] timeIndices = dataSet.getTimeIndices();

                dateInfoList.add(new CategoryDateInfo(datePattern,
                        categoryTopLevelDirPattern, timeType, isDirOnly,
                        timeIndices));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.archive.config.IFileDateHelper#getFileDate(java
     * .io.File)
     */
    @Override
    public Calendar getFileDate(File file) {
        String filenamePath = file.getAbsolutePath();
        String pathForFilePatternCheck = filenamePath.substring(rootDir
                .length());
        String pathForDirPatternCheck = FilenameUtils
                .getFullPathNoEndSeparator(pathForFilePatternCheck);
        Calendar result = null;
        Long timestamp = null;

        for (CategoryDateInfo dateInfo : dateInfoList) {
            Matcher matcher = null;
            if (dateInfo.isDirOnly) {
                matcher = dateInfo.datePattern.matcher(pathForDirPatternCheck);
            } else {
                matcher = dateInfo.datePattern.matcher(pathForFilePatternCheck);
            }

            if (matcher.matches()) {
                timestamp = CategoryDataSet.getMatchTimeInMilliseconds(
                        dateInfo.timeType, dateInfo.timeIndices, matcher);
                break;
            }
        }

        if (timestamp == null) {
            // no matching pattern, use file last modified date
            timestamp = file.lastModified();
        }

        // TODO future speed improvement refactor IFileDateHelper to have a
        // method that returns a long instead of Calendar. That will prevent
        // converting Calendar to long then back to a Calendar.
        result = TimeUtil.newGmtCalendar();
        result.setTimeInMillis(timestamp);
        return result;
    }

    /**
     * Check if this directory is a category directory. i.e. if the category is
     * satellite, is the directory satellite.
     * 
     * @param dirName
     * @return true if category directory; false otherwise.
     */
    public boolean isCategoryDirectory(String dirName) {
        for (CategoryDateInfo dateInfo : dateInfoList) {
            if (dateInfo.categoryTopLevelDirPattern.matcher(dirName).matches()) {
                return true;
            }
        }
        return false;
    }
}
