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

import java.util.HashMap;
import java.util.List;
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
 * Jul 31, 2019 7878       tgurney     Handle pickled Python objects as byte[]
 *
 * </pre>
 *
 * @author avarani
 */

public class MetarGuidanceRequest extends GuidanceRequest {
    private String size;

    private boolean all;

    private boolean header;

    private boolean decoded;

    private List<byte[]> siteObjs;

    /** Place in the map arguments needed for Metar request. */
    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<>();
        map.put("siteObjs", siteObjs);
        map.put("all", all);
        map.put("header", header);
        map.put("decoded", decoded);
        map.put("size", size);

        return map;
    }

    public void setSiteObjs(List<byte[]> siteObjs) {
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

    @Override
    public List<String> getSiteIDs() {
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
