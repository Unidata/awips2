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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.uf.viz.drawing.PathDrawingResourceData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationPathDrawingResourceData extends
        PathDrawingResourceData {
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.PathDrawingResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public DrawingLayer construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        CollaborationDrawingLayer layer = new CollaborationDrawingLayer(this,
                loadProperties);
        return layer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.display.AwipsToolsResourceData#setClassT
     * (java.lang.String)
     */
    @Override
    public String getClassT() {
        return this.getClass().getCanonicalName();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.display.AwipsToolsResourceData#setClassT
     * (java.lang.String)
     */
    @Override
    public void setClassT(String classT) {
        System.out.println("setClassT");
    }
}
