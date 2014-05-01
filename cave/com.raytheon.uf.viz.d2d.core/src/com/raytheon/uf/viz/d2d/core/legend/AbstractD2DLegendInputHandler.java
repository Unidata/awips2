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
package com.raytheon.uf.viz.d2d.core.legend;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;

/**
 * Abstract legend input handler for d2d. Has a mouse preference manager, and a
 * D2DLegendResource. Extends InputAdapter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AbstractD2DLegendInputHandler extends InputAdapter {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractD2DLegendInputHandler.class);

    protected MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected D2DLegendResource resource;

    protected AbstractD2DLegendInputHandler(D2DLegendResource resource) {
        this.resource = resource;
    }

}
