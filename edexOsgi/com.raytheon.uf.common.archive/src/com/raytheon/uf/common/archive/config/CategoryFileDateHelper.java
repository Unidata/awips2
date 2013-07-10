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

        private final int yearIndex;

        private final int monthIndex;

        private final int dayIndex;

        private final int hourIndex;

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
                int yearIndex, int monthIndex, int dayIndex, int hourIndex) {
            this.datePattern = datePattern;
            this.categoryTopLevelDirPattern = categoryTopLevelDirPattern;
            this.yearIndex = yearIndex;
            this.monthIndex = monthIndex;
            this.dayIndex = dayIndex;
            this.hourIndex = hourIndex;
        }

    }

    private final List<CategoryDateInfo> dateInfoList;

    private final String rootDir;

    private final boolean isDirOnly;

    /**
     * Initialization constructor.
     * 
     * @param config
     * @param rootDirPattern
     *            categoryTopLevelDirPattern
     */
    public CategoryFileDateHelper(CategoryConfig config, String rootDir) {
        List<String> categoryDirPatternList = config.getDirPatternList();
        this.dateInfoList = new ArrayList<CategoryFileDateHelper.CategoryDateInfo>(
                categoryDirPatternList.size());

        String filePatternStr = config.getFilePattern();
        this.rootDir = rootDir;
        this.isDirOnly = (filePatternStr == null)
                || ".*".equals(filePatternStr);

        for (String patternString : categoryDirPatternList) {
            Pattern datePattern = null;
            if (isDirOnly) {
                datePattern = Pattern.compile(patternString);
            } else {
                datePattern = Pattern.compile(patternString + File.separator
                        + config.getFilePattern());
            }
            int dirSeparatorIndex = patternString.indexOf(File.separatorChar);
            patternString = dirSeparatorIndex > patternString.length()
                    || dirSeparatorIndex < 0 ? patternString : patternString
                    .substring(0, dirSeparatorIndex);
            Pattern categoryTopLevelDirPattern = Pattern.compile(patternString);
            String[] indexValues = config.getDateGroupIndices().split(
                    "\\s*,\\s*");
            int yearIndex = Integer.parseInt(indexValues[0]);
            int monthIndex = Integer.parseInt(indexValues[1]);
            int dayIndex = Integer.parseInt(indexValues[2]);
            int hourIndex = Integer.parseInt(indexValues[3]);

            dateInfoList.add(new CategoryDateInfo(datePattern,
                    categoryTopLevelDirPattern, yearIndex, monthIndex,
                    dayIndex, hourIndex));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.archive.config.IFileDateHelper#getFileDate(java
     * .lang.String)
     */
    @Override
    public Calendar getFileDate(String filenamePath) {
        String pathForPatternCheck = filenamePath.substring(rootDir.length());
        pathForPatternCheck = isDirOnly ? FilenameUtils
                .getFullPathNoEndSeparator(pathForPatternCheck)
                : pathForPatternCheck;
        Calendar result = null;

        for (CategoryDateInfo dateInfo : dateInfoList) {
            Matcher matcher = dateInfo.datePattern.matcher(pathForPatternCheck);

            if (matcher.matches()) {
                int year = Integer.parseInt(matcher.group(dateInfo.yearIndex));
                // Adjust month value to Calendar's 0 - 11
                int month = Integer
                        .parseInt(matcher.group(dateInfo.monthIndex)) - 1;
                int day = Integer.parseInt(matcher.group(dateInfo.dayIndex));
                int hour = Integer.parseInt(matcher.group(dateInfo.hourIndex));

                result = TimeUtil.newGmtCalendar();
                result.set(year, month, day, hour, 0, 0);
                break;
            }
        }
        if (result == null) {
            // no matching pattern, use file last modified date
            File file = new File(filenamePath);
            long lastModifiedMillis = file.lastModified();

            result = TimeUtil.newGmtCalendar();
            result.setTimeInMillis(lastModifiedMillis);
        }
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
