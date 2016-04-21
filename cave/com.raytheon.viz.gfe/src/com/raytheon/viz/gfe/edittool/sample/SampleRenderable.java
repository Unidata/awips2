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
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.edittool.EditToolPaintProperties;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handles the display for on-demand sampling capability.
 * <p>
 * Roughly based on AWIPS I class SampleVisual.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/2008              chammack    Initial Creation.
 * 06/11/2008   #1193      ebabin      Updates for toggling lat/lon for sample set.
 * 07/21/2009              bphillip    Removed the points field
 * 07/23/2012   #936       dgilling    Properly retrieve imageGrid for paintMarkers()
 *                                     and paintSamples().
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SampleRenderable implements IRenderable, IMessageClient {
    private GridID qvGrid;

    private IFont sampleFont;

    private IFont markerFont;

    @SuppressWarnings("unchecked")
    public SampleRenderable() {
        Message.registerInterest(this, ShowQuickViewDataMsg.class);
    }

    @SuppressWarnings("unchecked")
    public void dispose() {
        if (sampleFont != null) {
            sampleFont.dispose();
        }

        if (markerFont != null) {
            markerFont.dispose();
        }
        Message.unregisterInterest(this, ShowQuickViewDataMsg.class);
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
        initFonts(target);

        paintSamples(target, paintProps);

        if (((EditToolPaintProperties) paintProps).getDataManager()
                .getSpatialDisplayManager().isShowISCMarkers()) {
            paintMarkers(target, paintProps);
        }

    }

    /**
     * @param target
     */
    protected void initFonts(IGraphicsTarget target) {
        if (sampleFont == null) {
            int fontNum = 2;
            if (GFEPreference.contains("SESample_font")) {
                fontNum = GFEPreference.getIntPreference("SESample_font");
            }
            sampleFont = GFEFonts.getFont(target, fontNum);
        }

        if (markerFont == null) {
            int fontNum = 3;
            if (GFEPreference.contains("SEMarker_font")) {
                fontNum = GFEPreference.getIntPreference("SEMarker_font");
            }
            markerFont = GFEFonts.getFont(target, fontNum);
        }
    }

    private void paintSamples(IGraphicsTarget target, PaintProperties paintProps) {
        EditToolPaintProperties eprops = (EditToolPaintProperties) paintProps;

        DataManager dm = eprops.getDataManager();

        List<Coordinate> locations = dm.getSampleSetManager().getLocations();
        if (locations.size() == 0) {
            return;
        }

        ISpatialDisplayManager sdm = dm.getSpatialDisplayManager();
        List<GridID> gids;
        GridID imageGrid;
        boolean isc;
        if (qvGrid != null) {
            isc = qvGrid.getParm().isIscParm();
            gids = Arrays.asList(qvGrid);
            imageGrid = qvGrid;
        } else {
            Parm[] parms = sdm.getCurrentlyEnabledParms();
            Parm imageParm = null;
            Arrays.sort(parms);
            gids = new ArrayList<GridID>(parms.length);
            Date date = sdm.getSpatialEditorTime();

            for (Parm p : parms) {
                gids.add(new GridID(p, date));

                if (p.getDisplayAttributes().getVisMode() == VisMode.IMAGE) {
                    imageParm = p;
                }
            }

            imageGrid = new GridID(imageParm, date);
            isc = dm.getParmManager().iscMode();
        }

        SamplePainter sp = new SamplePainter(true, dm.getSampleSetManager()
                .isShowLatLon(), sdm.isShowIscSampleUpdateTime(), isc,
                sdm.isShowISCSiteID(), sdm.isShowISCOfficialSymbol(), true,
                sampleFont);

        for (Coordinate point : locations) {
            sp.paint(target, point, gids, imageGrid, paintProps);
        }
    }

    private void paintMarkers(IGraphicsTarget target, PaintProperties paintProps) {
        EditToolPaintProperties eprops = (EditToolPaintProperties) paintProps;

        DataManager dm = eprops.getDataManager();
        ISpatialDisplayManager sdm = dm.getSpatialDisplayManager();

        SamplePainter spMarker = new SamplePainter(true, false,
                sdm.isShowISCUpdateTimeMarker(), dm.getParmManager().iscMode(),
                sdm.isShowISCSiteIdMarker(),
                sdm.isShowISCOfficialSymbolMarker(), false, markerFont);

        Date date = sdm.getSpatialEditorTime();
        Parm[] parms = sdm.getCurrentlyEnabledParms();
        Parm imageParm = null;

        Arrays.sort(parms);
        GridID[] grids = new GridID[parms.length];
        for (int i = 0; i < parms.length; i++) {
            grids[i] = new GridID(parms[i], date);

            if (parms[i].getDisplayAttributes().getVisMode() == VisMode.IMAGE) {
                imageParm = parms[i];
            }
        }

        GridID imageGrid = new GridID(imageParm, date);

        // only process the "active" sets
        Map<String, List<Coordinate>> markerSets = dm.getSampleSetManager()
                .getMarkerLocations();
        for (Entry<String, List<Coordinate>> entry : markerSets.entrySet()) {
            // extract the locations for the marker set from the dict.
            List<Coordinate> markers;
            int pos = 0;
            String setName = entry.getKey();
            markers = entry.getValue();
            pos = setName.lastIndexOf('_');
            String officeTypeSetName = setName.substring(pos + 1);

            // determine which grids that are visible are appropriate
            // for this marker set
            List<GridID> markerGrids = new ArrayList<GridID>();
            for (GridID grid : grids) {
                if (grid.getParm().getOfficeType().equals(officeTypeSetName)) {
                    markerGrids.add(grid);
                }
            }

            // now draw each marker
            for (int j = 0; j < markers.size(); j++) {
                spMarker.paint(target, markers.get(j), markerGrids, imageGrid,
                        paintProps);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ShowQuickViewDataMsg) {
            qvGrid = ((ShowQuickViewDataMsg) message).getGridId();
        }
    }
}
