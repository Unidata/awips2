package com.raytheon.uf.common.datadelivery.registry;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Projection.ProjectionType;
import com.raytheon.uf.common.datadelivery.registry.Provider.ProviderType;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * DD Config Collection object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Sept, 2012   1038      dhladky     Initial creation
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Jan 07, 2013 1451       djohnson    Use TimeUtil.newGmtCalendar().
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "collection")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Collection implements ISerializableObject {

    /**
     * Periodicity
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Oct 4, 2012            dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    @XmlEnum
    public enum Periodicity {
        @XmlEnumValue("day")
        DAY(Calendar.DAY_OF_YEAR), @XmlEnumValue("hour")
        HOUR(Calendar.HOUR), @XmlEnumValue("week")
        WEEK(Calendar.WEEK_OF_YEAR), @XmlEnumValue("month")
        MONTH(Calendar.MONTH), @XmlEnumValue("year")
        YEAR(Calendar.YEAR), @XmlEnumValue("none")
        NONE(0);

        private final int calendarField;

        private Periodicity(int calendarField) {
            this.calendarField = calendarField;
        }

        public int getCalendarField() {
            return calendarField;
        }
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Collection.class);

    @XmlAttribute(name = "name", required = true)
    @DynamicSerializeElement
    private String name;

    @XmlAttribute(name = "ignore")
    @DynamicSerializeElement
    private boolean ignore = true;

    @XmlAttribute
    @DynamicSerializeElement
    private ProviderType dataType;

    @XmlAttribute
    @DynamicSerializeElement
    private ProjectionType projection;

    @XmlElement(name = "parameterLookup")
    @DynamicSerializeElement
    private String parameterLookup;

    @XmlElement(name = "levelLookup")
    @DynamicSerializeElement
    private String levelLookup;

    @XmlElement(name = "seedUrl", required = true)
    @DynamicSerializeElement
    private String seedUrl;

    @XmlElement(name = "urlKey", required = true)
    @DynamicSerializeElement
    private String urlKey;

    @XmlElement(name = "firstDate", required = true)
    @DynamicSerializeElement
    private String firstDate;

    @XmlElement(name = "lastDate", required = true)
    @DynamicSerializeElement
    private String lastDate;

    @XmlElement(name = "dateFormat", required = true)
    @DynamicSerializeElement
    private String dateFormat;

    @XmlElement(name = "periodicity", required = true)
    @DynamicSerializeElement
    private Periodicity periodicity;

    public Collection() {

    }

    /**
     * Used in proto-collection creation from seed scans
     * 
     * @param name
     * @param seedUrl
     * @param dateFormat
     */
    public Collection(String name, String seedUrl, String dateFormat) {
        this.name = name;
        this.seedUrl = seedUrl;
        this.dateFormat = dateFormat;
    }

    /**
     * Gets the last formattable date piece
     * 
     * @return
     */
    private String findDateFormat() {
        String format = null;
        if (getDateFormat() != null) {
            String[] chunks = getDateFormat().split("/");
            if (chunks.length > 1) {
                format = chunks[chunks.length - 1];
            } else {
                format = getDateFormat();
            }
        }

        return format;
    }

    public ProviderType getDataType() {
        return dataType;
    }

    public String getDateFormat() {
        return dateFormat;
    }

    public String getFirstDate() {
        return firstDate;
    }

    /**
     * Gets the first date as Date;
     * 
     * @return
     */
    public Date getFirstDateAsDate() {

        Date date = null;
        if (getFirstDate() != null) {

            SimpleDateFormat lsdf = getSimpleDateFormatter();

            try {
                date = lsdf.parse(getFirstDate());
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        return date;
    }

    /**
     * Fins the formatted Date String
     * 
     * @param date
     * @return
     */
    public String getFormattedDate(Date date) {

        String formattedDate = null;
        SimpleDateFormat sdf = getSimpleDateFormatter();
        formattedDate = sdf.format(date);

        return formattedDate;
    }

    /**
     * Gets the formatted last date;
     * 
     * @return
     */
    public String getFormattedFirstDate() {

        String formattedDate = "";
        if (getFirstDate() != null) {

            SimpleDateFormat lsdf = getSimpleDateFormatter();

            try {
                Date lastDate = lsdf.parse(getFirstDate());
                formattedDate = getFormattedDate(lastDate);
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        return formattedDate;
    }

    /**
     * Gets the formatted last date;
     * 
     * @return
     */
    public String getFormattedLastDate() {

        String formattedDate = "";
        if (getLastDate() != null) {

            SimpleDateFormat lsdf = getSimpleDateFormatter();

            try {
                Date lastDate = lsdf.parse(getLastDate());
                formattedDate = getFormattedDate(lastDate);
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        return formattedDate;
    }

    public String getLastDate() {
        return lastDate;
    }

    /**
     * Gets the last date as Date;
     * 
     * @return
     */
    public Date getLastDateAsDate() {

        Date date = null;
        if (getLastDate() != null) {

            SimpleDateFormat lsdf = getSimpleDateFormatter();

            try {
                date = lsdf.parse(getLastDate());
            } catch (ParseException e) {
                e.printStackTrace();
            }
        }

        return date;
    }

    public String getLevelLookup() {
        return levelLookup;
    }

    public String getName() {
        return name;
    }

    public String getParameterLookup() {
        return parameterLookup;
    }

    public Periodicity getPeriodicity() {
        return periodicity;
    }

    public ProjectionType getProjection() {
        return projection;
    }

    public String getSeedUrl() {
        return seedUrl;
    }

    /**
     * Gets internally used date format
     * 
     * @return
     */
    private SimpleDateFormat getSimpleDateFormatter() {
        SimpleDateFormat lsdf = new SimpleDateFormat();
        lsdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        lsdf.applyLocalizedPattern(findDateFormat());
        return lsdf;
    }

    /**
     * This is the FULL date used by the crawler
     * 
     * @param inDate
     * @return
     */
    public String getUrlDate(Date inDate) {
        // we return a "blank" for collections that don't
        // have dates or date formats
        String urlDate = "";
        if (getDateFormat() != null) {
            SimpleDateFormat sdf = new SimpleDateFormat();
            sdf.applyLocalizedPattern(getDateFormat());
            urlDate = sdf.format(inDate);
        }
        return urlDate;
    }

    public String getUrlKey() {
        return urlKey;
    }

    public boolean isIgnore() {
        return ignore;
    }

    public void setDataType(ProviderType dataType) {
        this.dataType = dataType;
    }

    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    public void setFirstDate(String firstDate) {
        this.firstDate = firstDate;
    }

    public void setFirstDateAsDate(Date date) {
        SimpleDateFormat sdf = getSimpleDateFormatter();
        setFirstDate(sdf.format(date));
    }

    public void setIgnore(boolean ignore) {
        this.ignore = ignore;
    }

    public void setLastDate(String lastDate) {
        this.lastDate = lastDate;
    }

    public void setLastDateAsDate(Date date) {
        SimpleDateFormat sdf = getSimpleDateFormatter();
        setLastDate(sdf.format(date));
    }

    public void setLevelLookup(String levelLookup) {
        this.levelLookup = levelLookup;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setParameterLookup(String parameterLookup) {
        this.parameterLookup = parameterLookup;
    }

    public void setPeriodicity(Periodicity periodicity) {
        this.periodicity = periodicity;
    }

    public void setProjection(ProjectionType projection) {
        this.projection = projection;
    }

    public void setSeedUrl(String seedUrl) {
        this.seedUrl = seedUrl;
    }

    public void setUrlKey(String urlKey) {
        this.urlKey = urlKey;
    }

    /**
     * Update the time objects if necessary
     * 
     * @param currentDate
     */
    public void updateTime(Date currentDate) {

        if (getFirstDate() != null && getLastDate() != null) {

            SimpleDateFormat sdf = getSimpleDateFormatter();
            Periodicity p = getPeriodicity();
            Integer periodInt = p.getCalendarField();

            try {

                // take care of latest date
                Date lastDate = sdf.parse(getLastDate());
                Calendar cal = TimeUtil.newGmtCalendar();
                cal.setTimeInMillis(lastDate.getTime());
                int lastRoll = cal.get(periodInt);

                Calendar curCal = TimeUtil.newGmtCalendar();
                curCal.setTimeInMillis(currentDate.getTime());
                int currRoll = curCal.get(periodInt);
                int diff = Math.abs(currRoll - lastRoll);

                // compare to see if needs to walk forward
                if (diff >= 1) {
                    // walk dates forward
                    setLastDate(sdf.format(currentDate));

                    Date firstDate = sdf.parse(getFirstDate());
                    Calendar calf = TimeUtil.newGmtCalendar();
                    calf.setTimeInMillis(firstDate.getTime());
                    calf.add(periodInt, diff);

                    setFirstDate(sdf.format(calf.getTime()));
                }

            } catch (ParseException e1) {
                statusHandler.error("Un-able to parse and update time: "
                        + e1.getMessage());
            }
        }
    }

}
