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
 * Request classed used by the Metar viewer tab.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2009            avarani     Initial creation
 * Apr 28, 2011 8065       rferrel     Implement data caching
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class MetarGuidanceRequest extends GuidanceRequest {
    protected String size;

    protected boolean all;

    protected boolean header;

    protected boolean decoded;

    protected ArrayList<String> siteObjs;

    /**
     * Place in the map arguments need for Metar request.
     */
    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("siteObjs", siteObjs);
        map.put("all", all);
        map.put("header", header);
        map.put("decoded", decoded);
        map.put("size", size);

        return map;
    }

    public void setSiteObjs(ArrayList<String> siteObjs) {
        this.siteObjs = siteObjs;
    }

    public void setSize(String size) {
        this.size = size;
    }

    public void setAll(boolean all) {
        this.all = all;
    }

    public void setHeader(boolean header) {
        this.header = header;
    }

    public void setDecoded(boolean decoded) {
        this.decoded = decoded;
    }

    public ArrayList<String> getSiteIDs() {
        return this.siteIDs;
    }

    public String getSize() {
        return this.size;
    }

    public boolean getAll() {
        return this.all;
    }

    public boolean getHeader() {
        return this.header;
    }

    public boolean getDecoded() {
        return this.decoded;
    }
}
