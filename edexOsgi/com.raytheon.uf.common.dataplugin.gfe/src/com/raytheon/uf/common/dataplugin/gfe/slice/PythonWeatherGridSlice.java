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
package com.raytheon.uf.common.dataplugin.gfe.slice;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

// TODO: REMOVE THIS CLASS if WxDefinition/WeatherKey class hierarchy is
// ever fully-implemented in Python.

@DynamicSerialize
public class PythonWeatherGridSlice extends AbstractGridSlice {

    @DynamicSerializeElement
    private Grid2DByte weatherGrid;

    @DynamicSerializeElement
    List<String> keys;

    public PythonWeatherGridSlice() {

    }

    public PythonWeatherGridSlice(WeatherGridSlice slice) {
        this.weatherGrid = slice.getWeatherGrid().clone();
        this.keys = new ArrayList<String>();
        for (WeatherKey key : slice.getKeys()) {
            keys.add(key.toString());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpy()
     */
    @Override
    public Object[] getNumpy() {
        Object[] numpy = new Object[2];
        numpy[0] = weatherGrid.getBuffer().array();
        String pyList = PyUtil.listToList(keys);
        numpy[1] = pyList;
        return numpy;
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyX()
     */
    @Override
    public int getNumpyX() {
        return weatherGrid.getXdim();
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyY()
     */
    @Override
    public int getNumpyY() {
        return weatherGrid.getYdim();
    }

    public Grid2DByte getWeatherGrid() {
        return weatherGrid;
    }

    public void setWeatherGrid(Grid2DByte weatherGrid) {
        this.weatherGrid = weatherGrid;
    }

    public List<String> getKeys() {
        return keys;
    }

    public void setKeys(List<String> keys) {
        this.keys = keys;
    }

    @Override
    protected void moveDataToLocalCache() {
        // TODO Auto-generated method stub

    }

    @Override
    protected void moveDataToMem() {
        // TODO Auto-generated method stub

    }
}
