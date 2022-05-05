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
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * Discrete Color Table
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 23, 2010           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Dec 02, 2019  71896    tjensen   Added default value for defaultPatterns
 *
 * </pre>
 *
 * @author randerso
 */

public class DiscreteColorTable extends ColorTable {
    private static final IUFStatusHandler LOG = UFStatus
            .getHandler(DiscreteColorTable.class);

    private final List<String> overlapPatterns;

    private final IColorMap colorMap;

    private final Parm parm;

    private String complexColor;

    private String complexPattern;

    private final List<String> keys;

    /**
     * Constructor
     *
     * @param parm
     * @param colorMap
     */
    public DiscreteColorTable(Parm parm, IColorMap colorMap) {
        super();

        this.parm = parm;
        this.colorMap = colorMap;

        String compName = parm.getParmID().getCompositeName();

        // get pattern information from gfe configuration file
        String[] defaultPatterns = { "TRANS_25PC_45DEG", "TRANS_25PC_135DEG",
                "CROSS" };
        String[] op = GFEPreference.getStringArray("DiscreteOverlapPatterns",
                defaultPatterns);
        op = GFEPreference.getStringArray(compName + "_DiscreteOverlapPatterns",
                op);

        // 1st is always WHOLE
        overlapPatterns = new ArrayList<>();
        overlapPatterns.add("WHOLE");
        overlapPatterns.addAll(Arrays.asList(op));

        ParmID parmId = parm.getParmID();
        String siteId = parmId.getDbId().getSiteId();
        String compositeName = parmId.getCompositeName();

        // discrete keys
        keys = DiscreteKey.discreteDefinition(siteId).symbols(compositeName);

        // DiscreteComplexColor
        complexColor = GFEPreference.getString("DiscreteComplexColor", "White");
        complexColor = GFEPreference
                .getString(compName + "_DiscreteComplexColor", complexColor);

        complexPattern = GFEPreference.getString("DiscreteComplexPattern",
                "SCATTERED");
        complexPattern = GFEPreference.getString(
                compName + "_DiscreteComplexPattern", complexPattern);

        initTable(colorMap);
    }

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
                LOG.handle(Priority.PROBLEM,
                        "Not enough colors for " + wx + " in color table.");
            }
        }

        // now for efficiency, clear and add all entries at once
        resetEntries();
        addEntry(colorTableEntries);
    }

    private ColorEntry calcDiscreteCTEntry(DiscreteWxValue wxValue) {
        List<ImageAttr> imageAttr = new ArrayList<>();

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
            imageAttr.add(new ImageAttr(
                    RGBColors.getColorName(new RGB(red, green, blue)), fp));
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
