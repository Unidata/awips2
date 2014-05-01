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
package com.raytheon.viz.ghg.monitor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.gfe.ui.zoneselector.AbstractZoneSelector;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector.IZoneSelectionListener;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelectorResource;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * GHG Spatial Viewer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GHGSpatialViewer extends AbstractZoneSelector {
    // TODO: This is a start at converting the GHGMonitor to use the classes
    // created for the ZoneSelector. Unforturnately it will require significant
    // refactoring of GHGMonitor to get this to work and we don't have time or a
    // pressing need to do this now.

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GHGSpatialViewer.class);

    private static final Map<String, List<String>> mapConfigure;
    static {
        mapConfigure = new LinkedHashMap<String, List<String>>();
        mapConfigure.put("FIPS", Arrays.asList("Counties", "Marine_Zones"));
        mapConfigure.put("Public", Arrays.asList("Zones"));
        mapConfigure.put("FireWx", Arrays.asList("FireWxZones"));
        mapConfigure.put("Marine", Arrays.asList("Marine_Zones"
        /* , "OffshoreMZones" */));
    }

    IInputHandler theMouseListener = new InputAdapter() {

        /**
         * Primary mouse button
         */
        private static final int MB1 = 1;

        /**
         * Middle (Wheel) mouse button
         */
        private static final int MB2 = 2;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleDoubleClick(int,
         * int)
         */
        @Override
        public boolean handleDoubleClick(int x, int y, int button) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            switch (mouseButton) {
            case MB1:
                button1Press(x, y);
                break;

            default:
                // do nothing
            }

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseHover(int,
         * int)
         */
        @Override
        public boolean handleMouseHover(int x, int y) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
         * int)
         */
        @Override
        public boolean handleMouseMove(int x, int y) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            switch (mouseButton) {
            case MB1:
                button1Release(x, y);
            default:
                // do nothing
            }
            return true;
        }

        @Override
        public boolean handleMouseWheel(Event event, int x, int y) {
            return true;
        }

    };

    private String myWfo;

    private String mapName;

    private RGB fgColor;

    private RGB myWfoOutlineColor;

    private RGB otherWfoOutlineColor;

    private Point pressInfo;

    public GHGSpatialViewer(Composite parent, String myWfo, GridLocation gloc,
            IZoneSelectionListener callback) {
        super(parent, gloc, callback);
        this.myWfo = myWfo;

        this.backColor = RGBColors.getRGBColor("gray90");
        // this.backColor = parent.getShell().getDisplay()
        // .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND).getRGB();
        this.fgColor = RGBColors.getRGBColor("gray40");
        this.myWfoOutlineColor = RGBColors.getRGBColor("yellow");
        this.otherWfoOutlineColor = RGBColors.getRGBColor("black");

        registerMouseHandler(theMouseListener);
    }

    /**
     * @param x
     * @param y
     */
    protected void button1Press(int x, int y) {
        this.pressInfo = new Point(x, y);
    }

    /**
     * @param x
     * @param y
     */
    protected void button1Release(int x, int y) {
        if (this.pressInfo == null) {
            return;
        }
        int diff = Math.abs(x - this.pressInfo.x)
                + Math.abs(y - this.pressInfo.y);
        // click?
        if (diff < 10) {
            List<String> zones = this.selectedZones(x, y);
            if (this.selectCB != null && !zones.isEmpty()) {
                this.selectCB.zoneSelected(zones.get(0));
            }
        }
        this.pressInfo = null;
    }

    public void setMap(String mapName) {
        this.mapName = mapName;
        List<String> mapList = mapConfigure.get(mapName);
        super.setMap(mapList);

        for (ZoneSelectorResource rsc : mapRscList) {
            rsc.setMyWfo(this.myWfo);
            rsc.setDefaultFillColor(this.fgColor);
            rsc.setWfoOutlineColor(this.myWfoOutlineColor);
            rsc.setOutlineColor(otherWfoOutlineColor);
        }

        setMapInternal(mapRscList);
    }

    public List<String> knownMaps() {
        return new ArrayList<String>(mapConfigure.keySet());
    }

    public String getCurrentMap() {
        return this.mapName;
    }
}
