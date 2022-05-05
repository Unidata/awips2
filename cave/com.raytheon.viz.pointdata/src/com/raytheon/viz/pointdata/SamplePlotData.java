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

package com.raytheon.viz.pointdata;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.measure.Unit;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;

/**
 * SamplePlotData
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   -----------------------
 * 10/31/2019   71272      Mark Peters   Initial Creation
 *
 * </pre>
 *
 * @author mpeters
 */

public class SamplePlotData implements IPlotData {

    private final Map<String, String> data = new HashMap<>();

    private final Map<String, Unit<?>> units = new HashMap<>();

    public void addData(String parameter, String value, Unit<?> unit) {
        data.put(parameter, value);
        units.put(parameter, unit);
    }

    @Override
    public int getInt(String parameter) {
        throw new RuntimeException();
    }

    @Override
    public float getFloat(String parameter) {
        throw new RuntimeException();
    }

    @Override
    public long getLong(String parameter) {
        throw new RuntimeException();
    }

    @Override
    public String getString(String parameter) {
        return data.get(parameter);
    }

    @Override
    public String[] getStringAllLevels(String parameter) {
        throw new RuntimeException();
    }

    @Override
    public Type getType(String parameter) {
        return Type.STRING;
    }

    @Override
    public Number getNumber(String parameter) {
        return Double.valueOf(getString(parameter));
    }

    @Override
    public Unit<?> getUnit(String parameter) {
        return units.get(parameter);
    }

    @Override
    public int getDimensions(String parameter) {
        return 1;
    }

    @Override
    public Number[] getNumberAllLevels(String parameter) {
        throw new RuntimeException();
    }

    @Override
    public Set<String> getParameters() {
        return Collections.unmodifiableSet(data.keySet());
    }

}
