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
package com.raytheon.viz.grid.rsc;

import javax.measure.UnitConverter;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;

/**
 * Grid Resource that does not do simple mapping from data to colors but has an
 * overly complex mapping, usually for something where pixel values are actually
 * enums.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------
 * Oct 13, 2010  4820     bsteffen  Initial creation
 * Feb 07, 2014  2211     bsteffen  Fix sampling
 * Aug 30, 2016  3240     bsteffen  Use Interrogatable for inspect
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DataMappedGridResource extends D2DGridResource {

    public DataMappedGridResource(GridResourceData data, LoadProperties props) {
        super(data, props);
    }

    @Override
    public String getName() {
        String name = this.resourceData.getCustomLegend();
        if (name == null) {
            name = super.getName();
        }
        return name;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        InterrogateMap map = interrogate(coord, getTimeForResource(),
                Interrogator.VALUE, UNIT_STRING_INTERROGATE_KEY);
        if (map == null || map.isEmpty()) {
            return "NO DATA";
        }
        double val = map.get(Interrogator.VALUE).getValue().doubleValue();
        if (Double.isNaN(val) || val <= -9999) {
            return "No Data";
        }

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (params != null) {
            UnitConverter d2cm = params.getDisplayToColorMapConverter();
            if (d2cm != null) {
                val = d2cm.convert(val);
            }

            DataMappingPreferences dataMapping = params.getDataMapping();
            if (dataMapping != null) {
                for (DataMappingEntry entry : dataMapping.getEntries()) {
                    double pixelValue = entry.getPixelValue();
                    double relError = Math.abs((pixelValue - val) / val);
                    String text = entry.getLabel();
                    if (relError < 0.00001 && text != null) {
                        return text;
                    }
                }
            }

            UnitConverter cm2d = params.getColorMapToDisplayConverter();
            if (cm2d != null) {
                val = cm2d.convert(val);
            }
        }
        return ((DataMappedGridResourceData) resourceData).getSampleFormat()
                .format(val) + map.get(UNIT_STRING_INTERROGATE_KEY);
    }

    @Override
    protected ColorMapParameters createColorMapParameters(GeneralGridData data)
            throws VizException {
        ColorMapParameters params = super.createColorMapParameters(data);
        params.setColorMapMin(0.0f);
        params.setColorMapMax(255.0f);
        return params;
    }

}
