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
package com.raytheon.uf.viz.collaboration.ui.rsc.rendering;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;

/**
 * An abstract class which contains a reference to the dataManager the
 * subclasses will use for data access, also has convenience methods for
 * accessing current target and paint properties
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class CollaborationRenderingHandler {

    protected CollaborationRenderingDataManager dataManager;

    final void setDataManager(CollaborationRenderingDataManager dataManager) {
        this.dataManager = dataManager;
    }

    protected final IGraphicsTarget getGraphicsTarget() {
        return dataManager.getGraphicsTarget();
    }

    protected final PaintProperties getPaintProperties() {
        return dataManager.getPaintProperties();
    }
}
