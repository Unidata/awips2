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

import java.nio.FloatBuffer;
import java.text.ParsePosition;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2009            mnash     Initial creation
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
        float val = (Float) map.get(INTERROGATE_VALUE);
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
        FloatBuffer floatData = data.getScalarData();
        FloatBuffer newFloatData = FloatBuffer.allocate(floatData.capacity());
        floatData.rewind();
        newFloatData.rewind();
        while (floatData.hasRemaining()) {
            float value = floatData.get();
            if (value < 1f) {
                newFloatData.put(1f);
            } else if (value < 2) {
                newFloatData.put(48f);
            } else if (value < 3) {
                newFloatData.put(96f);
            } else if (value < 4) {
                newFloatData.put(128f);
            } else if (value < 5) {
                newFloatData.put(144f);
            } else if (value < 6) {
                newFloatData.put(160f);
            } else if (value < 7) {
                newFloatData.put(176f);
            } else {
                newFloatData.put(0f);
            }
        }
        return GeneralGridData.createScalarData(data.getGridGeometry(),
                newFloatData, data.getDataUnit());
    }

}
