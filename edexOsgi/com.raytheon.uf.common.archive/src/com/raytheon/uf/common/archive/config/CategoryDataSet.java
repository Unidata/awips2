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
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * A grouping of data set information used in a category.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2013  #2224      rferrel     Initial creation
 * Oct 02, 2013 #2147      rferrel     Allow Date to ignore hour in time stamp.
 * Dec 10, 2013 #2624      rferrel     Added Julian date.
 * Dec 17, 2013 2603       rjpeter     Clear low order time fields on time generation.
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "dataSet")
public class CategoryDataSet {
    private static final int YEAR_INDEX = 0;

    private static final int MONTH_INDEX = 1;

    private static final int DAY_OF_YEAR_INDEX = 1;

    private static final int DAY_INDEX = 2;

    private static final int JULIAN_HOUR_INDEX = 2;

    private static final int HOUR_INDEX = 3;

    private static final int TIMESTAMP_INDEX = 0;

    /**
     * Types of times and the number of indices for getting the time stamp from
     * patterns.
     */
    public static enum TimeType {
        Date(4), EpochSec(1), EpochMS(1), File(0), Julian(3);

        private final int numIndices;

        private TimeType(int numIndices) {
            this.numIndices = numIndices;
        }

        public int getNumGroupIndices() {
            return numIndices;
        }
    }

    /**
     * List of directory patterns.
     */
    @XmlElement(name = "dirPattern")
    private List<String> dirPatterns;

    /**
     * Optional file pattern.
     */
    @XmlElement(name = "filePattern")
    private String filePattern;

    @XmlElement(name = "timeType")
    private TimeType timeType = TimeType.Date;

    /**
     * The display label.
     */
    @XmlElement(name = "displayLabel")
    private String displayLabel;

    private int[] timeIndices = null;

    /**
     * The index with group number for getting desired parts of the TimeType.
     */
    @XmlElement(name = "dateGroupIndices")
    private String dateGroupIndices = "";

    public List<String> getDirPatterns() {
        return dirPatterns;
    }

    public void setDirPatterns(List<String> dirPatterns) {
        this.dirPatterns = dirPatterns;
    }

    public String getFilePattern() {
        return filePattern;
    }

    public void setFilePattern(String filePattern) {
        this.filePattern = filePattern;
    }

    public TimeType getTimeType() {
        return timeType;
    }

    public void setTimeType(TimeType timeType) {
        this.timeType = timeType;
    }

    public String getDisplayLabel() {
        return displayLabel;
    }

    public void setDisplayLabel(String displayLabel) {
        this.displayLabel = displayLabel;
    }

    public String getDateGroupIndices() {
        return dateGroupIndices;
    }

    public void setDateGroupIndices(String dateGroupIndices) {
        this.dateGroupIndices = dateGroupIndices;
        this.timeIndices = null;
    }

    /**
     * Get the array of time indices based on time type and date group indices.
     * 
     * @return timeIndices
     */
    public int[] getTimeIndices() {
        if (timeIndices == null) {
            timeIndices = new int[timeType.getNumGroupIndices()];
            if (timeIndices.length > 0) {
                String[] indexValues = getDateGroupIndices().split("\\s*,\\s*");
                for (int index = 0; index < timeIndices.length; ++index) {
                    if (indexValues.length > index) {
                        timeIndices[index] = Integer
                                .parseInt(indexValues[index]);
                    } else {
                        timeIndices[index] = -1;
                    }
                }
            }
        }
        return timeIndices;
    }

    /**
     * Get the Pattern for dirPattern.
     * 
     * @param dirPattern
     * 
     * @return pattern or null if dirPattern not in the list of directory
     *         patterns.
     */
    public Pattern getPattern(String dirPattern) {
        Pattern pattern = null;
        if (dirPatterns.contains(dirPattern)) {
            if (isDirOnly()) {
                pattern = Pattern.compile(dirPattern);
            } else {
                pattern = Pattern.compile(dirPattern + File.separator
                        + getFilePattern());
            }
        }
        return pattern;
    }

    /**
     * 
     * @return true when only the dirPatterns should be used.
     */
    public boolean isDirOnly() {
        return (filePattern == null) || (filePattern.length() == 0)
                || ".*".equals(filePattern);
    }

    /**
     * Get time stamp for file based on time type.
     * 
     * @param timeIndices
     * @param matcher
     * @return fileTime
     */
    public Long getMatchTimeInMilliseconds(int[] timeIndices, Matcher matcher) {
        return CategoryDataSet.getMatchTimeInMilliseconds(timeType,
                timeIndices, matcher);
    }

    /**
     * Get file time based on time type. Assumes the matcher is set up matching
     * a file's path name so the groups in the matcher can be used to get the
     * time stamp.
     * 
     * @param timeType
     * @param timeIndices
     * @param matcher
     * @return fileTime or null if time type does not get time using the
     *         matcher.
     */
    public static Long getMatchTimeInMilliseconds(
            CategoryDataSet.TimeType timeType, int[] timeIndices,
            Matcher matcher) {
        Long fileTime = null;
        switch (timeType) {
        case Date:
            Calendar fileCal = TimeUtil.newGmtCalendar();
            int year = Integer.parseInt(matcher
                    .group(timeIndices[CategoryDataSet.YEAR_INDEX]));
            // Adjust month value to Calendar's 0 - 11
            int month = Integer.parseInt(matcher
                    .group(timeIndices[CategoryDataSet.MONTH_INDEX])) - 1;
            int day = Integer.parseInt(matcher
                    .group(timeIndices[CategoryDataSet.DAY_INDEX]));

            // Default to last hour of the day.
            int hour = 23;

            if (timeIndices[CategoryDataSet.HOUR_INDEX] >= 0) {
                hour = Integer.parseInt(matcher
                        .group(timeIndices[CategoryDataSet.HOUR_INDEX]));
            }

            fileCal.set(year, month, day, hour, 0, 0);
            fileCal.set(Calendar.MILLISECOND, 0);
            fileTime = fileCal.getTimeInMillis();
            break;
        case EpochMS:
            fileTime = Long.parseLong(matcher
                    .group(timeIndices[CategoryDataSet.TIMESTAMP_INDEX]));
            break;
        case EpochSec:
            fileTime = Long.parseLong(matcher
                    .group(timeIndices[CategoryDataSet.TIMESTAMP_INDEX]));
            fileTime *= TimeUtil.MILLIS_PER_SECOND;
            break;
        case File:
            fileTime = null;
            break;
        case Julian:
            Calendar julainCal = TimeUtil.newGmtCalendar();
            int jYear = Integer.parseInt(matcher
                    .group(timeIndices[CategoryDataSet.YEAR_INDEX]));
            int jDay = Integer.parseInt(matcher
                    .group(timeIndices[CategoryDataSet.DAY_OF_YEAR_INDEX]));

            // When two digit year determine century.
            if (jYear < 100) {
                int cYear = julainCal.get(Calendar.YEAR);
                jYear += (cYear - (cYear % 100));
                julainCal.add(Calendar.YEAR, 1);
                int nextYear = julainCal.get(Calendar.YEAR);

                // If date too far into the future back up a century.
                if ((jYear > nextYear) || ((jYear == nextYear) && (jDay > 31))) {
                    jYear -= 100;
                }
            }

            julainCal.set(Calendar.YEAR, jYear);
            julainCal.set(Calendar.DAY_OF_YEAR, jDay);

            // Default to last hour of the day.
            int jHour = 23;
            if (timeIndices[CategoryDataSet.JULIAN_HOUR_INDEX] >= 0) {
                jHour = Integer.parseInt(matcher
                        .group(timeIndices[CategoryDataSet.JULIAN_HOUR_INDEX]));
            }
            julainCal.set(Calendar.HOUR_OF_DAY, jHour);
            julainCal.set(Calendar.MINUTE, 0);
            julainCal.set(Calendar.SECOND, 0);
            julainCal.set(Calendar.MILLISECOND, 0);
            fileTime = julainCal.getTimeInMillis();
            break;

        default:
            fileTime = null;
            break;
        }
        return fileTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("DataSet[ ");
        sb.append("TimeType: ").append(getTimeType());
        sb.append(", dateGroupIndices: ").append(getDateGroupIndices());
        sb.append(", isDirOnly: ").append(isDirOnly());
        sb.append(", displayLabel: ").append(getDisplayLabel());
        sb.append(", dirPatterns[ ");
        for (String dirPattern : getDirPatterns()) {
            sb.append(dirPattern).append(", ");
        }
        sb.append("], filePattern: ").append(
                filePattern == null ? "null" : filePattern);
        sb.append("]");
        return sb.toString();
    }
}
