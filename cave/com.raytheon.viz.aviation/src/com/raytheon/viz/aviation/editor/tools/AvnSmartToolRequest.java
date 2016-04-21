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
package com.raytheon.viz.aviation.editor.tools;

import java.util.List;

import com.raytheon.uf.viz.core.jobs.QueueJobRequest;

/**
 * Request to run a smart tool off the UI thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2010 3294           rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class AvnSmartToolRequest extends QueueJobRequest<Object> {

    private String toolName;

    private String bbb;

    private List<String> fcsts;

    public void setToolName(String toolName) {
        this.toolName = toolName;
    }

    public String getToolName() {
        return toolName;
    }

    public void setBbb(String bbb) {
        this.bbb = bbb;
    }

    public String getBbb() {
        return bbb;
    }

    public void setFcsts(List<String> fcsts) {
        this.fcsts = fcsts;
    }

    public List<String> getFcsts() {
        return fcsts;
    }
}
