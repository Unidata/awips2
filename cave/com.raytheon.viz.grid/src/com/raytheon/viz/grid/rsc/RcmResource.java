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

import java.text.ParsePosition;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;

/**
 * TODO This whole class should be handled from style rules
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 16, 2009           mnash       Initial creation
 * Feb 07, 2014  2211     bsteffen    Fix sampling
 * Feb 28, 2791  2211     bsteffen    Move data conversion to DataSource
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RcmResource extends D2DGridResource {

    private static Unit<?> DBZ;

    /**
     * @param data
     * @param props
     */
    public RcmResource(RcmResourceData data, LoadProperties props) {
        super(data, props);
        if (DBZ == null) {
            DBZ = UnitFormat.getUCUMInstance().parseObject("dBZ",
                    new ParsePosition(0));
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> map = interrogate(coord);
        if (map == null) {
            return "NO DATA";
        }
        float val = ((Number) map.get(INTERROGATE_VALUE)).floatValue();
        String sampleVal = "";
        if (val < 1f) {
            sampleVal = "No Data";
        } else if (val < 48f) {
            sampleVal = "< 15dBZ";
        } else if (val < 96f) {
            sampleVal = "15-29dBZ";
        } else if (val < 128f) {
            sampleVal = "30-39dBZ";
        } else if (val < 144f) {
            sampleVal = "40-44dBZ";
        } else if (val < 160f) {
            sampleVal = "45-49dBZ";
        } else if (val < 176f) {
            sampleVal = "50-54dBZ";
        } else {
            sampleVal = ">= 55dBZ";
        }
        return sampleVal;
    }

    @Override
    protected GeneralGridData getData(GridRecord gridRecord)
            throws VizException {
        GeneralGridData data = super.getData(gridRecord);
        DataSource source = new RcmDataSource(data.getScalarData());
        return GeneralGridData.createScalarData(data.getGridGeometry(), source,
                data.getDataUnit());
    }

    private final class RcmDataSource implements DataSource {

        private final DataSource wrappedSource;

        public RcmDataSource(DataSource wrappedSource) {
            this.wrappedSource = wrappedSource;
        }

        @Override
        public double getDataValue(int x, int y) {
            double value = wrappedSource.getDataValue(x, y);
            if (value < 1) {
                return 1;
            } else if (value < 2) {
                return 48;
            } else if (value < 3) {
                return 96;
            } else if (value < 4) {
                return 128;
            } else if (value < 5) {
                return 144;
            } else if (value < 6) {
                return 160;
            } else if (value < 7) {
                return 176;
            } else {
                return 0;
            }
        }

    }

}
