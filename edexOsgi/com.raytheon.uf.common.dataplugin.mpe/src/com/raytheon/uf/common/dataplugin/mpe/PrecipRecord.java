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
package com.raytheon.uf.common.dataplugin.mpe;

import java.util.Calendar;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * The MPE precip record data type.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2017 6407       bkowal      Initial creation
 * Oct 06, 2017 6407       bkowal      Added {@link #toString()}.
 *
 * </pre>
 *
 * @author bkowal
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "mpeprecipseq")
@Table(name = "mpe_precip")
@org.hibernate.annotations.Table(appliesTo = "mpe_precip", indexes = {
        @Index(name = "precip_fieldAndDateIndex", columnNames = { "precipField",
                "date", "hour" }) })
@DynamicSerialize
public class PrecipRecord extends PersistablePluginDataObject {

    public static final class Fields {
        public static final String PRECIP_FIELD = "precipField";

        public static final String DATE = "date";

        public static final String HOUR = "hour";

        public static final String HRAP_X = "hrapX";

        public static final String HRAP_Y = "hrapY";

        public static final String HRAP_WIDTH = "hrapWidth";

        public static final String HRAP_HEIGHT = "hrapHeight";
    }

    public static final String PLUGIN_NAME = "mpe_precip";

    private static final long serialVersionUID = -851963980795805444L;

    @Column(nullable = false, length = 15)
    @Enumerated(EnumType.STRING)
    @DynamicSerializeElement
    @DataURI(position = 1)
    private PrecipField precipField;

    @Temporal(TemporalType.DATE)
    @Column(nullable = false)
    @DynamicSerializeElement
    private Date date;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int hour;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int hrapX;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int hrapY;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int hrapWidth;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int hrapHeight;

    public PrecipRecord() {
    }

    public PrecipRecord(String uri) {
        super(uri);
    }

    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }

    @Override
    public void setDataTime(DataTime dataTime) {
        super.setDataTime(dataTime);
        final Calendar calendar = dataTime.getRefTimeAsCalendar();
        setDate(calendar.getTime());
        setHour(calendar.get(Calendar.HOUR_OF_DAY));
    }

    public PrecipField getPrecipField() {
        return precipField;
    }

    public void setPrecipField(PrecipField precipField) {
        this.precipField = precipField;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public int getHour() {
        return hour;
    }

    public void setHour(int hour) {
        this.hour = hour;
    }

    public int getHrapX() {
        return hrapX;
    }

    public void setHrapX(int hrapX) {
        this.hrapX = hrapX;
    }

    public int getHrapY() {
        return hrapY;
    }

    public void setHrapY(int hrapY) {
        this.hrapY = hrapY;
    }

    public int getHrapWidth() {
        return hrapWidth;
    }

    public void setHrapWidth(int hrapWidth) {
        this.hrapWidth = hrapWidth;
    }

    public int getHrapHeight() {
        return hrapHeight;
    }

    public void setHrapHeight(int hrapHeight) {
        this.hrapHeight = hrapHeight;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("PrecipRecord [");
        sb.append("precipField=").append(precipField.name());
        if (date != null) {
            sb.append(", date=").append(date.toString());
        }
        sb.append(", hour=").append(hour);
        sb.append(", hrapX=").append(hrapX);
        sb.append(", hrapY=").append(hrapY);
        sb.append(", hrapWidth=").append(hrapWidth);
        sb.append(", hrapHeight=").append(hrapHeight);
        sb.append("]");

        return sb.toString();
    }
}