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
package com.raytheon.uf.viz.d2d.nsharp.tool;

import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;

import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapMouseHandler;
import gov.noaa.nws.ncep.ui.nsharp.view.AbstractNsharpLoadDialog;

/**
 *
 * The class for D2D NSHARP Map Mouse Handler.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 25, 2020  73172    smanoj   Initial creation
 * Jul 17, 2020  80915    smanoj   Added queryLimit for NSHARP time queries.
 * 
 * </pre>
 *
 * @author smanoj
 */
public class D2DNsharpMapMouseHandler extends AbstractNsharpMapMouseHandler {

    @Override
    public AbstractNsharpLoadDialog getLoadDialog() {
        return D2DNsharpLoadDialog.getAccess();
    }

    @Override
    public int getQueryLimit() {
        // get the number of Frames set to display in D2D.
        return (int) (VizGlobalsManager.getCurrentInstance()
                .getProperty(VizConstants.FRAMES_ID));
    }
}