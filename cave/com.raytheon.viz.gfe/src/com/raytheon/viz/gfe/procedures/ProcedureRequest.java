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
package com.raytheon.viz.gfe.procedures;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.jobs.QueueJobRequest;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;

/**
 * Request for a GFE procedure to execute.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureRequest extends QueueJobRequest<Object> {

    private String procedureName;

    private ReferenceData refSet;

    private TimeRange timeRange;

    private String varDict;

    private PreviewInfo preview;

    public String getProcedureName() {
        return procedureName;
    }

    public void setProcedureName(String procedureName) {
        this.procedureName = procedureName;
    }

    public ReferenceData getRefSet() {
        return refSet;
    }

    public void setRefSet(ReferenceData refSet) {
        this.refSet = refSet;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    public String getVarDict() {
        return varDict;
    }

    public void setVarDict(String varDict) {
        this.varDict = varDict;
    }

    public PreviewInfo getPreview() {
        return preview;
    }

    public void setPreview(PreviewInfo preview) {
        this.preview = preview;
    }

}
