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
package com.raytheon.viz.hydrocommon.ratingcurve;

import java.sql.Date;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * This class containing the rating curve shift data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date           Ticket#     Engineer    Description
 * ------------   ----------  ----------- --------------------------
 * 24 Nov 2008    1628        dhladky     First stab.
 * 21 Feb 2010    4167        mpduff      Added TimeZone to Calendar object.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class RatingCurveShiftData implements Comparable<RatingCurveShiftData> {

    private String lid = null;

    private Calendar date = null;

    private double value = 0.0;

    private boolean active = false;

    public RatingCurveShiftData(Object[] objects) {

        if (objects[0] != null) {
            lid = (String) objects[0];
        }
        if (objects[1] != null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTimeInMillis(((Date) objects[1]).getTime());
            date = cal;
        }
        if (objects[2] != null) {
            value = (Double) objects[2];
        }
        if (objects[3] != null) {
            String bool = (String) objects[3];
            if (bool.equals("T")) {
                active = true;
            }
        }
    }

    /**
     * Default constructor for building
     * 
     * @param lid
     * @param date
     * @param value
     * @param active
     */
    public RatingCurveShiftData(String lid, Calendar date, double value,
            boolean active) {
        this.lid = lid;
        this.date = date;
        this.value = value;
        this.active = active;
    }

    public String getLid() {
        return lid;
    }
    
    public void setLid(String lid) {
    	this.lid = lid;
    }

    public Calendar getDate() {
        return date;
    }
    
    public void setDate(Calendar date) {
    	this.date = date;
    }

    public double getValue() {
        return value;
    }

    public void setValue(double value) {
    	this.value = value;
    }
    
    public boolean isActive() {
        return active;
    }
    
    public void setActive(boolean active) {
    	this.active = active;
    }

    /**
     * Get the date in a MM/DD/YYYY format.
     * 
     * @return The date.
     */
    public String getDateString() {
        String format = "%02d/%02d/%4d";
        return String.format(format, date.get(Calendar.MONTH) + 1, date
                .get(Calendar.DAY_OF_MONTH), date.get(Calendar.YEAR));
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("RatingCurveShiftData [ ");
        sb.append(lid + ", " + getDateString() + ", " + value + ", " + active + " ]");
        
        return sb.toString();
    }

    @Override
	public int compareTo(RatingCurveShiftData o) {
		if (this.date.getTime().equals(o.getDate().getTime())) {
			return 0;
		} else if (this.date.getTime().before(o.getDate().getTime())) {
			return 1;
		} else if (this.date.getTime().after(o.getDate().getTime())) {
			return -1;
		}
		return 0;
	}
}
