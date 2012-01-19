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
package com.raytheon.viz.hydrocommon.data;

import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * this class contains the Rejected Data data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Oct 28, 2008				askripsky	Initial creation
 * Nov 10, 2008 1661        askripsky   Changed to extend ForecastData
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class RejectedData extends ForecastData {
    /**
     * Reject Type.
     */
    protected String rejectType = "";

    /**
     * User ID.
     */
    protected String userID = "";

    /**
     * Constructor
     */
    public RejectedData() {
        super();
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public RejectedData(Object[] data) {
        super(data);

        setRejectType(data[16]);
        setUserID(data[17]);
    }

    /**
     * Constructor that casts a Physical Element object to a Rejected Data
     * object. Values set are based on a Physical Element that has been
     * deleted/demoted to Rejected data.
     * 
     * @param oldData
     */
    public RejectedData(PhysicalElementData oldData) {
        setLid(oldData.getLid());
        setPe(oldData.getPe());
        setDur(oldData.getDur());
        setTs(oldData.getTs());
        setExtremum(oldData.getExtremum());

        /* set probability to -1 for observed data */
        probability = -1.0;

        /* set validtime and basistime to obstime for observed data */
        setValidTime(oldData.getObstime());
        setBasisTime(oldData.getObstime());

        /* set postingtime to current time */
        setPostingTime(Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime());

        setValue(oldData.getValue());
        setRevision(oldData.getRevision());
        setShefQualCode(oldData.getShefQualCode());
        setProductID(oldData.getProductID());
        setProductTime(oldData.getProductTime());
        setQualityCode(oldData.getQualityCode());

        /* set reject_type to M for Manual */
        setRejectType("M");

        /* copy userid to rejectedData structure */
        setUserID(LocalizationManager.getInstance().getCurrentUser());
    }

    public String getRejectType() {
        return rejectType;
    }

    public void setRejectType(Object rejectType) {
        this.rejectType = (rejectType != null) ? (String) rejectType : "";
    }

    public String getUserID() {
        return userID;
    }

    public void setUserID(Object userID) {
        this.userID = (userID != null) ? (String) userID : "";
    }
}
