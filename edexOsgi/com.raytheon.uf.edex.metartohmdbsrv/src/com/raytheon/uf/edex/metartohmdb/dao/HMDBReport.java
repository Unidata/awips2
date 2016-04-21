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
package com.raytheon.uf.edex.metartohmdb.dao;

import java.util.Calendar;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Data record class for HMDB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009            jkorman     Initial creation
 * May 14, 2014 2536       bclement    removed TimeTools usage and ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class HMDBReport extends PersistableDataObject<String> {

    private static final long serialVersionUID = 1L;
    
    public static final String DTFMT = "'%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS'";

    @DynamicSerializeElement
    @XmlAttribute
    private String report_type;   // not null
    
    @DynamicSerializeElement
    @XmlAttribute
    private Integer mode;
    
    @DynamicSerializeElement
    @XmlAttribute
    private String icao_loc_id;   // not null
    
    @DynamicSerializeElement
    @XmlAttribute
    private String wmo_dd;        // not null
    
    @DynamicSerializeElement
    @XmlAttribute
    private String afos_dd;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar origin;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar date;       // not null
    
    @DynamicSerializeElement
    @XmlAttribute
    private Calendar nominal;    // not null
    
    @DynamicSerializeElement
    @XmlAttribute
    private String report;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Integer prior;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Double lat;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Double lon;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Double elev;
    
    @DynamicSerializeElement
    @XmlAttribute
    private String state;
    
    @DynamicSerializeElement
    @XmlAttribute
    private String country;
    
    @DynamicSerializeElement
    @XmlAttribute
    private String name;
    
    @DynamicSerializeElement
    @XmlAttribute
    private Integer sequence_num;

    @DynamicSerializeElement
    @XmlAttribute
    private Integer status;
    
    public HMDBReport() {
        
    }

    /**
     * @return the report_type
     */
    public String getReport_type() {
        return report_type;
    }

    /**
     * @param report_type the report_type to set
     */
    public void setReport_type(String report_type) {
        this.report_type = report_type;
    }

    /**
     * @return the mode
     */
    public Integer getMode() {
        return mode;
    }

    /**
     * @param mode the mode to set
     */
    public void setMode(Integer mode) {
        this.mode = mode;
    }

    /**
     * @return the icao_loc_id
     */
    public String getIcao_loc_id() {
        return icao_loc_id;
    }

    /**
     * @param icao_loc_id the icao_loc_id to set
     */
    public void setIcao_loc_id(String icao_loc_id) {
        this.icao_loc_id = icao_loc_id;
    }

    /**
     * @return the wmo_dd
     */
    public String getWmo_dd() {
        return wmo_dd;
    }

    /**
     * @param wmo_dd the wmo_dd to set
     */
    public void setWmo_dd(String wmo_dd) {
        this.wmo_dd = wmo_dd;
    }

    /**
     * @return the afos_dd
     */
    public String getAfos_dd() {
        return afos_dd;
    }

    /**
     * @param afos_dd the afos_dd to set
     */
    public void setAfos_dd(String afos_dd) {
        this.afos_dd = afos_dd;
    }

    /**
     * @return the origin
     */
    public Calendar getOrigin() {
        return origin;
    }

    /**
     * @param origin the origin to set
     */
    public void setOrigin(Calendar origin) {
        this.origin = origin;
    }

    /**
     * @return the date
     */
    public Calendar getDate() {
        return date;
    }

    /**
     * @param date the date to set
     */
    public void setDate(Calendar date) {
        this.date = date;
    }

    /**
     * @return the nominal
     */
    public Calendar getNominal() {
        return nominal;
    }

    /**
     * @param nominal the nominal to set
     */
    public void setNominal(Calendar nominal) {
        this.nominal = nominal;
    }

    /**
     * @return the report
     */
    public String getReport() {
        return report;
    }

    /**
     * @param report the report to set
     */
    public void setReport(String report) {
        if(report != null) {
            if(report.length() > 255) {
                report = report.substring(0,255);
            }
        }
        this.report = report;
    }

    /**
     * @return the prior
     */
    public Integer getPrior() {
        return prior;
    }

    /**
     * @param prior the prior to set
     */
    public void setPrior(Integer prior) {
        this.prior = prior;
    }

    /**
     * @return the lat
     */
    public Double getLat() {
        return lat;
    }

    /**
     * @param lat the lat to set
     */
    public void setLat(Double lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public Double getLon() {
        return lon;
    }

    /**
     * @param lon the lon to set
     */
    public void setLon(Double lon) {
        this.lon = lon;
    }

    /**
     * @return the elev
     */
    public Double getElev() {
        return elev;
    }

    /**
     * @param elev the elev to set
     */
    public void setElev(Double elev) {
        this.elev = elev;
    }

    /**
     * @return the state
     */
    public String getState() {
        return state;
    }

    /**
     * @param state the state to set
     */
    public void setState(String state) {
        this.state = state;
    }

    /**
     * @return the country
     */
    public String getCountry() {
        return country;
    }

    /**
     * @param country the country to set
     */
    public void setCountry(String country) {
        this.country = country;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the sequence_num
     */
    public Integer getSequence_num() {
        return sequence_num;
    }

    /**
     * @param sequence_num the sequence_num to set
     */
    public void setSequence_num(Integer sequence_num) {
        this.sequence_num = sequence_num;
    }

    /**
     * @return the status
     */
    public Integer getStatus() {
        return status;
    }

    /**
     * @param status the status to set
     */
    public void setStatus(Integer status) {
        this.status = status;
    }
    
    /**
     * 
     * @return
     */
    public String toInsertSQL() {

        
        String retValue = null;
        
        StringBuilder sb = new StringBuilder("insert into rpt values(");
        if(report_type != null) {
            sb.append("'");
            sb.append(report_type);
            sb.append("',0");
        } else {
            return retValue;
        }
        if(icao_loc_id != null) { // not null
            sb.append(",'");
            sb.append(icao_loc_id);
            sb.append("'");
        } else {
            return retValue;
        }
        if(wmo_dd != null) { // not null
            sb.append(",'");
            sb.append(wmo_dd);
            sb.append("'");
        } else {
            return retValue;
        }
        if(afos_dd != null) {
            sb.append(",'");
            sb.append(afos_dd);
            sb.append("'");
        } else {
            sb.append(",''");
        }
        if(origin != null) {
            sb.append(",");
            sb.append(String.format(DTFMT, date));
        } else {
            sb.append(",");
        }
        if(date != null) { // not null
            sb.append(",");
            sb.append(String.format(DTFMT, date));
        } else {
            return retValue;
        }
        if(nominal != null) { // not null
            sb.append(",");
            sb.append(String.format(DTFMT, date));
        } else {
            return retValue;
        }
        if(report != null) { // not null
            sb.append(",'");
            sb.append(report);
            sb.append("',0");
        } else {
            sb.append(",,0");
        }

        if(lat != null) {
            sb.append(String.format(",%f",lat));
        } else {
            sb.append(",");
        }
        if(lon != null) {
            sb.append(String.format(",%f",lon));
        } else {
            sb.append(",");
        }
        if(elev != null) {
            sb.append(String.format(",%f",elev));
        } else {
            sb.append(",0");
        }
        if(state != null) {
            sb.append(String.format(",'%s'",state));
        } else {
            sb.append(",''");
        }
        if(country != null) {
            sb.append(String.format(",'%s'",country));
        } else {
            sb.append(",''");
        }
        if(name != null) {
            sb.append(String.format(",'%s'",name));
        } else {
            sb.append(",''");
        }
        if(sequence_num != null) {
            sb.append(String.format(",%d",sequence_num));
        } else {
            sb.append(",0");
        }
        if(status != null) {
            sb.append(String.format(",%d",status));
        } else {
            sb.append(",0");
        }
        sb.append(");");
        return sb.toString();
    }

    /**
     * Use to test the toInsertSQL method.
     * @param args
     */
    public static final void main(String [] args) {
        
        HMDBReport rpt = new HMDBReport();
        rpt.setDate(TimeUtil.newGmtCalendar(2009, 06, 30));
        rpt.date.set(Calendar.HOUR_OF_DAY, 14);
        rpt.date.set(Calendar.MINUTE, 56);
        
        rpt.setOrigin(TimeUtil.newGmtCalendar(2009, 06, 30));
        rpt.origin.set(Calendar.HOUR_OF_DAY, 14);
        rpt.date.set(Calendar.MINUTE, 54);

        rpt.setNominal(TimeUtil.newGmtCalendar(2009, 06, 30));
        rpt.nominal.set(Calendar.HOUR_OF_DAY, 15);
        rpt.nominal.set(Calendar.MINUTE,0);
        rpt.nominal.set(Calendar.SECOND,0);
        
        rpt.setReport_type("METAR");
        rpt.setIcao_loc_id("KOMA");
        rpt.setWmo_dd("SAUS70");
        
        rpt.setLat(45.123);
        rpt.setLon(-95.321);
        rpt.setElev(391d);

        //rpt.setCountry("US");
        //rpt.setState("NE");
        //rpt.setName("OMAHA");
        
        rpt.setReport("METAR KOFF 301454 xxxxxxxxxx");
        
        System.out.println(rpt.toInsertSQL());
        
    }
    
}
