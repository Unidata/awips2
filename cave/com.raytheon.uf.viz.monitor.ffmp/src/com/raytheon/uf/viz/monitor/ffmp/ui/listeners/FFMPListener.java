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
package com.raytheon.uf.viz.monitor.ffmp.ui.listeners;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPGraphData;

/**
 * 
 * FFMPListener
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 06, 2009 2521       dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public interface FFMPListener {

    public void cwaChanged(FFMPCWAChangeEvent fcce);

    public void hucChanged(FFMPHUCChangeEvent fhce);

    public void centerChanged(FFMPScreenCenterEvent fsce) throws VizException;

    public void traceChanged(FFMPStreamTraceEvent fste);

    public void timeChanged(FFMPTimeChangeEvent ftce, FFMPRecord.FIELDS field)
            throws VizException;

    public void fieldChanged(FFMPFieldChangeEvent ffce) throws VizException;

    public void isWorstCaseChanged(FFMPWorstCaseEvent fwce);

    public void refresh();

    public void manualRefresh();

    public void isAutoRefresh(FFMPAutoRefreshEvent fare);

    public void isMaintainLayer(FFMPMaintainLayerEvent fmle);

    public void isParent(FFMPParentBasinEvent fpbe);

    public void restoreTable();

    public void setLinkToFrame(boolean linkToFrame);

    public FFMPGraphData getGraphData(String pfaf) throws VizException;

    public void setBasinToggle(boolean val);

    public void setStreamToggle(boolean val);

}
