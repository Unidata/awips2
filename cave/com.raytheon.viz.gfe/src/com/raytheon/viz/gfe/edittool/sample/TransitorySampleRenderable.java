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
package com.raytheon.viz.gfe.edittool.sample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.EditToolPaintProperties;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handles the display for on-demand sampling capability.
 * 
 * Roughly based on AWIPS I class of the same name.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/2008              chammack    Initial Creation.
 * Mar 10, 2016 #5479      randerso    Use improved GFEFonts API
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TransitorySampleRenderable implements IRenderable {

    private Coordinate point;

    private IFont font;

    /**
     * Set the current location of the roaming sample point. Null is an
     * acceptable value for when no value should be displayed.
     * 
     * @param p
     *            the point (lat lon space)
     */
    public void setCoordinate(Coordinate p) {
        this.point = p;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        initFont(target);
        if (point == null) {
            return;
        }

        EditToolPaintProperties eprops = (EditToolPaintProperties) paintProps;

        // TODO: Handle ISC modes
        DataManager dm = eprops.getDataManager();
        ISpatialDisplayManager sdm = dm.getSpatialDisplayManager();
        SamplePainter sp = new SamplePainter(true, dm.getSampleSetManager()
                .isShowLatLon(), sdm.isShowIscSampleUpdateTime(), dm
                .getParmManager().iscMode(), sdm.isShowISCSiteID(),
                sdm.isShowISCOfficialSymbol(), true, font);

        Date date = sdm.getSpatialEditorTime();
        Parm[] parms = sdm.getCurrentlyEnabledParms();
        Arrays.sort(parms);
        Parm imageParm = sdm.getActivatedParm();

        List<GridID> gids = new ArrayList<GridID>(parms.length);

        for (int i = 0; i < parms.length; i++) {
            gids.add(new GridID(parms[i], date));
        }

        GridID editable = new GridID(imageParm, date);

        sp.paint(target, point, gids, editable, paintProps);
    }

    protected void initFont(IGraphicsTarget target) {
        if (font == null) {
            font = GFEFonts.makeGFEIFont(target, "SESample_font", 2);
        }
    }

    public void dispose() {
        if (font != null) {
            font.dispose();
        }
    }
}
