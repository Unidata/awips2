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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Hazard Services Headlines Request
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2021 8331       randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

@DynamicSerialize
public class HazardServicesHeadlinesRequest implements IServerRequest {

    @DynamicSerializeElement
    private String site;

    @DynamicSerializeElement
    private String pil;

    @DynamicSerializeElement
    private List<String> allowedHazards;

    @DynamicSerializeElement
    private List<String> editAreas;

    @DynamicSerializeElement
    private TimeRange timeRange;

    @DynamicSerializeElement
    private String mode;

    /**
     * Nullary constructor for serialization
     */
    public HazardServicesHeadlinesRequest() {

    }

    /**
     * Constructor
     *
     * @param site
     * @param pil
     * @param allowedHazards
     * @param editAreas
     * @param timeRange
     */
    public HazardServicesHeadlinesRequest(String site, String pil,
            List<String> allowedHazards, List<String> editAreas,
            TimeRange timeRange, String mode) {
        this.site = site;
        this.pil = pil;
        this.allowedHazards = allowedHazards;
        this.editAreas = editAreas;
        this.timeRange = timeRange;
        this.mode = mode;
    }

    /**
     * @return the site
     */
    public String getSite() {
        return site;
    }

    /**
     * @param site
     *            the site to set
     */
    public void setSite(String site) {
        this.site = site;
    }

    /**
     * @return the pil
     */
    public String getPil() {
        return pil;
    }

    /**
     * @param pil
     *            the pil to set
     */
    public void setPil(String pil) {
        this.pil = pil;
    }

    /**
     * @return the allowedHazards
     */
    public List<String> getAllowedHazards() {
        return allowedHazards;
    }

    /**
     * @param allowedHazards
     *            the allowedHazards to set
     */
    public void setAllowedHazards(List<String> allowedHazards) {
        this.allowedHazards = allowedHazards;
    }

    /**
     * @return the editAreas
     */
    public List<String> getEditAreas() {
        return editAreas;
    }

    /**
     * @param editAreas
     *            the editAreas to set
     */
    public void setEditAreas(List<String> editAreas) {
        this.editAreas = editAreas;
    }

    /**
     * @return the timeRange
     */
    public TimeRange getTimeRange() {
        return timeRange;
    }

    /**
     * @param timeRange
     *            the timeRange to set
     */
    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    /**
     * @return the mode
     */
    public String getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(String mode) {
        this.mode = mode;
    }
}
