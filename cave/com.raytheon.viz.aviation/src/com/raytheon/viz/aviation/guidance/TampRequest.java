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
package com.raytheon.viz.aviation.guidance;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Request for guidance for TAMP (TAF/LAMP?)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2009            njensen     Initial creation
 * Feb  3, 2011 5795       rferrel     Now handles selected site
 *                                     and routine values.
 * Apr 28, 2011 8065       rferrel     Implement data caching
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TampRequest extends GuidanceRequest {

    protected String selectedSite;

    protected String tafText;

    protected String tafHeader;

    protected boolean cigVisOnly;

    protected boolean routine;

    protected ArrayList<String> siteObjs;

    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("selSite", selectedSite);
        map.put("siteObjs", siteObjs);
        map.put("model", model);
        map.put("format", format);
        map.put("taf", tafText);
        map.put("tafHeader", tafHeader);
        map.put("cvOnly", cigVisOnly);
        map.put("routine", routine);
        return map;
    }

    public String getSelectedSite() {
        return selectedSite;
    }

    public void setSelectedSite(String selectedSite) {
        this.selectedSite = selectedSite;
    }

    public String getTafText() {
        return tafText;
    }

    public void setTafText(String tafText) {
        this.tafText = tafText;
    }

    public String getTafHeader() {
        return tafHeader;
    }

    public void setTafHeader(String tafHeader) {
        this.tafHeader = tafHeader;
    }

    public boolean isCigVisOnly() {
        return cigVisOnly;
    }

    public void setCigVisOnly(boolean cigVisOnly) {
        this.cigVisOnly = cigVisOnly;
    }

    public boolean isRoutine() {
        return routine;
    }

    public void setRoutine(boolean routine) {
        this.routine = routine;
    }

    public void setSiteObjs(ArrayList<String> siteObjs) {
        this.siteObjs = siteObjs;
    }
}
