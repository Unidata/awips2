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
package com.raytheon.uf.viz.collaboration.ui.rsc;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationRenderingHandler.ColorMapDataCallback;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;

/**
 * Collaboration class responsible for disposing renderable objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDisposingHandler {

    @Subscribe
    public void disposeImage(IImage image) {
        image.dispose();
    }

    @Subscribe
    public void disposeMesh(IMesh mesh) {
        mesh.dispose();
    }

    @Subscribe
    public void disposeColorMapCallback(ColorMapDataCallback callback) {
        callback.setData(null);
    }

    @Subscribe
    public void disposeWireframeShape(IWireframeShape shape) {
        shape.dispose();
    }

    @Subscribe
    public void disposeFont(IFont font) {
        font.dispose();
    }
}
