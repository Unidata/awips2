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
package com.raytheon.viz.gfe.colortable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DiscreteColorTable extends ColorTable {
    private static final IUFStatusHandler LOG = UFStatus
            .getHandler(DiscreteColorTable.class);

    private List<String> overlapPatterns;

    private IColorMap colorMap;

    private Parm parm;

    private boolean overlapAllowed;

    private String complexColor;

    private String complexPattern;

    private List<String> keys;

    public DiscreteColorTable(Parm parm, IColorMap colorMap) {
        super();

        this.parm = parm;
        this.colorMap = colorMap;

        String compName = parm.getParmID().getCompositeName();

        // get pattern information from gfe configuration file
        String[] op;
        if (prefs.contains(compName + "_DiscreteOverlapPatterns")) {
            op = prefs.getStringArray(compName + "_DiscreteOverlapPatterns");
        } else {
            op = prefs.getStringArray("DiscreteOverlapPatterns");
        }

        overlapPatterns = new ArrayList<String>();
        overlapPatterns.add("WHOLE"); // 1st is WHOLE
        overlapPatterns.addAll(Arrays.asList(op));

        ParmID parmId = parm.getParmID();
        String siteId = parmId.getDbId().getSiteId();
        String compositeName = parmId.getCompositeName();

        // overlapping parm?
        overlapAllowed = DiscreteKey.discreteDefinition(siteId).overlaps(
                compositeName);

        // discrete keys
        keys = DiscreteKey.discreteDefinition(siteId).symbols(compositeName);

        // DiscreteComplexColor
        if (prefs.contains(compName + "_DiscreteComplexColor")) {
            complexColor = prefs.getString(compName + "_DiscreteComplexColor");
        } else if (prefs.contains("DiscreteComplexColor")) {
            complexColor = prefs.getString("DiscreteComplexColor");
        } else {
            complexColor = "White";
        }

        if (prefs.contains(compName + "_DiscreteComplexPattern")) {
            complexPattern = prefs.getString(compName
                    + "_DiscreteComplexPattern");
        } else if (prefs.contains("DiscreteComplexPattern")) {
            complexPattern = prefs.getString("DiscreteComplexPattern");
        } else {
            complexPattern = "SCATTERED";
        }

        initTable(colorMap);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.colortable.ColorTable#map(com.raytheon.viz.gfe.core
     * .wxvalue.WxValue)
     */
    @Override
    public List<ImageAttr> map(WxValue wxValue) {
        List<ImageAttr> retVal = getEntries().get(wxValue);
        if (retVal != null) {
            return retVal;
        }

        // calc it
        ColorEntry ce = calcDiscreteCTEntry((DiscreteWxValue) wxValue);
        // add to color table
        addEntry(ce);

        // recursive, but will now find it
        // return map(wxValue);

        return ce.getAttributes();
    }

    private void initTable(IColorMap colorMap) {
        ColorEntry[] colorTableEntries = new ColorEntry[keys.size()];

        ParmID parmId = parm.getParmID();
        String siteId = parmId.getDbId().getSiteId();

        // just assign each color on up for overlap and non-overlap cases
        for (int i = 0; i < keys.size(); i++) {
            DiscreteKey dk = new DiscreteKey(siteId, keys.get(i), parmId);
            DiscreteWxValue wx = new DiscreteWxValue(dk, parm);
            colorTableEntries[i] = calcDiscreteCTEntry(wx);
            if (ColorTable.NOT_IN_TABLE_ENTRY == colorTableEntries[i]
                    .getAttributes()) {
                LOG.handle(Priority.PROBLEM, "Not enough colors for " + wx
                        + " in color table.");
            }
        }

        // now for efficiency, add all entries at once
        resetEntries(); // get rid of previous entries
        addEntry(colorTableEntries);
    }

    private ColorEntry calcDiscreteCTEntry(DiscreteWxValue wxValue) {
        List<ImageAttr> imageAttr = new ArrayList<ImageAttr>();

        DiscreteKey key = wxValue.getDiscreteKey();
        int[] keyIndexes = key.keyIndexes();
        boolean outOfPatterns = false;
        boolean outOfColors = false;

        List<Color> colors = colorMap.getColors();
        // do each key
        for (int i = 0; i < keyIndexes.length; i++) {
            // determine fill pattern
            if (i >= overlapPatterns.size()) {
                outOfPatterns = true;
                break;
            }
            String fp = overlapPatterns.get(i);

            // determine color
            int spectrumIndex = keyIndexes[i];
            if (spectrumIndex >= colors.size()) {
                outOfColors = true;
                break;
            }

            int red = (int) (colors.get(spectrumIndex).getRed() * 255);
            int green = (int) (colors.get(spectrumIndex).getGreen() * 255);
            int blue = (int) (colors.get(spectrumIndex).getBlue() * 255);
            imageAttr.add(new ImageAttr(RGBColors.getColorName(new RGB(red,
                    green, blue)), fp));
        }

        // out of patterns?
        if (outOfPatterns) {
            imageAttr.clear();
            imageAttr.add(new ImageAttr(complexColor, complexPattern));
        }

        // out of colors?
        if (outOfColors) {
            imageAttr = ColorTable.NOT_IN_TABLE_ENTRY;
        }

        // now add the entry to the ColorTable
        return new ColorEntry(wxValue, imageAttr);
    }
}
