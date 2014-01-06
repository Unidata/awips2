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
import java.text.FieldPosition;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
 * Dec 04, 2013 2603       rferrel     Changes to improve archive purging.
 * Dec 17, 2013 2603       rjpeter     Fix file data pattern matching.
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
        /** Always use the same field postion. */
        private static final FieldPosition pos0 = new FieldPosition(0);

        /** Pattern used to get the date. */
        private final Pattern datePattern;

        /** Pattern for getting top level directories. */
        private final Pattern categoryTopLevelDirPattern;

        /** The type of type stamp being used. */
        private final CategoryDataSet.TimeType timeType;

        /** Indices in the pattern group used to get the time stamp. */
        private final int[] timeIndices;

        /** The format used to get the display label. */
        private final String displayLabelFormat;

        /** Formatter used to get display label. */
        private final MessageFormat msgfmt;

        /**
         * Initialization constructor.
         * 
         * @param datePattern
         * @param categoryTopLevelDirPattern
         * @param yearIndex
         * @param monthIndex
         * @param dayIndex
         * @param hourIndex
         * @param displayLabelFormat
         */
        public CategoryDateInfo(Pattern datePattern,
                Pattern categoryTopLevelDirPattern,
                CategoryDataSet.TimeType timeType, int[] timeIndices,
                String displayLabelFormat) {
            this.datePattern = datePattern;
            this.categoryTopLevelDirPattern = categoryTopLevelDirPattern;
            this.timeType = timeType;
            this.timeIndices = timeIndices;
            this.displayLabelFormat = displayLabelFormat;
            if (displayLabelFormat != null) {
                this.msgfmt = new MessageFormat(this.displayLabelFormat);
            } else {
                this.msgfmt = null;
            }
        }

        /**
         * Get the display label from the matcher. This assumes the matcher is a
         * pattern match for the date pattern.
         * 
         * @param matcher
         * @return label
         */
        public String getDisplayLabel(Matcher matcher) {
            // Unable to use StringBuilder with MessageFormat.
            StringBuffer sb = new StringBuffer();
            String[] args = new String[matcher.groupCount() + 1];
            args[0] = matcher.group();
            for (int i = 1; i < args.length; ++i) {
                args[i] = matcher.group(i);
            }
            String label = msgfmt.format(args, sb, pos0).toString();
            return label;
        }
    }

    private final List<CategoryDateInfo> dateInfoList;

    /**
     * Initialization constructor.
     * 
     * @param config
     * @param rootDirPattern
     *            categoryTopLevelDirPattern
     */
    public CategoryFileDateHelper(CategoryConfig config) {
        List<CategoryDataSet> categoryDataSetList = config.getDataSetList();
        int size = 0;
        for (CategoryDataSet dataSet : categoryDataSetList) {
            size += dataSet.getDirPatterns().size();
        }

        this.dateInfoList = new ArrayList<CategoryFileDateHelper.CategoryDateInfo>(
                size);

        CategoryDataSet.TimeType timeType;
        for (CategoryDataSet dataSet : categoryDataSetList) {
            timeType = dataSet.getTimeType();

            for (String patternString : dataSet.getDirPatterns()) {
                Pattern datePattern = dataSet.getPattern(patternString);
                int dirSeparatorIndex = patternString
                        .indexOf(File.separatorChar);
                patternString = (dirSeparatorIndex > patternString.length())
                        || (dirSeparatorIndex < 0) ? patternString
                        : patternString.substring(0, dirSeparatorIndex);
                Pattern categoryTopLevelDirPattern = Pattern
                        .compile(patternString);
                int[] timeIndices = dataSet.getTimeIndices();

                String displayLabelFormat = dataSet.getDisplayLabel();

                dateInfoList.add(new CategoryDateInfo(datePattern,
                        categoryTopLevelDirPattern, timeType, timeIndices,
                        displayLabelFormat));
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
    public DataSetStatus getFileDate(File file) {
        String filenamePath = file.getAbsolutePath();
        Long timestamp = null;
        DataSetStatus result = new DataSetStatus(file);

        for (CategoryDateInfo dateInfo : dateInfoList) {
            Matcher matcher = dateInfo.datePattern.matcher(filenamePath);

            if (matcher.matches()) {
                timestamp = CategoryDataSet.getMatchTimeInMilliseconds(
                        dateInfo.timeType, dateInfo.timeIndices, matcher);
                result.setInDataSet(true);
                result.addDisplayLabel(dateInfo.getDisplayLabel(matcher));
                break;
            }
        }

        if (timestamp == null) {
            // no matching pattern, use file last modified date
            timestamp = file.lastModified();
        }

        Calendar time = TimeUtil.newGmtCalendar();
        time.setTimeInMillis(timestamp);
        result.setTime(time);
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
