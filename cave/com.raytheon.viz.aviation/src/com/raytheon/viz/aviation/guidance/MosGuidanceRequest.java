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
 * Data request class for the MOS viewer tab.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2011 8065       rferrel     Initial creation
 * Jul 31, 2019  7878      tgurney     Handle pickled Python objects as byte[]
 *
 * </pre>
 *
 * @author avarani
 */

public class MosGuidanceRequest extends GuidanceRequest {
    private List<byte[]> siteObjs;

    private boolean routine;

    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<>();
        map.put("siteObjs", siteObjs);
        map.put("model", model);
        map.put("format", format);
        map.put("routine", routine);
        return map;
    }

    public void setSiteObjs(List<byte[]> siteObjs) {
        this.siteObjs = siteObjs;
    }

    public boolean isRoutine() {
        return routine;
    }

    public void setRoutine(boolean routine) {
        this.routine = routine;
    }
}
