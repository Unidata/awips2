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
package com.raytheon.uf.viz.derivparam.data;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

import com.raytheon.uf.common.dataplugin.level.CompareType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CubeRequestableData extends AbstractRequestableData {
    NavigableMap<Level, CubeLevel<AbstractRequestableData, AbstractRequestableData>> dataMap;

    public CubeRequestableData(AbstractRequestableData paramRecord) {
        super(paramRecord);
        Comparator<Level> levelComparator = new Comparator<Level>() {

            @Override
            public int compare(Level o1, Level o2) {
                int rval = 0;
                CompareType val = o1.compare(o2);
                switch (val) {
                case BELOW: {
                    rval = -1;
                    break;
                }
                case ABOVE: {
                    rval = 1;
                    break;
                }
                }
                return rval;
            }

        };
        dataMap = new TreeMap<Level, CubeLevel<AbstractRequestableData, AbstractRequestableData>>(
                levelComparator);
    }

    public void addParam(AbstractRequestableData paramRecord) {
        CubeLevel<AbstractRequestableData, AbstractRequestableData> cubeLevel = dataMap
                .get(paramRecord.getLevel());
        if (cubeLevel == null) {
            cubeLevel = new CubeLevel<AbstractRequestableData, AbstractRequestableData>();
            dataMap.put(paramRecord.getLevel(), cubeLevel);
        }
        cubeLevel.setParam(paramRecord);
    }

    public void addPressure(AbstractRequestableData pressRecord) {
        CubeLevel<AbstractRequestableData, AbstractRequestableData> cubeLevel = dataMap
                .get(pressRecord.getLevel());
        if (cubeLevel == null) {
            cubeLevel = new CubeLevel<AbstractRequestableData, AbstractRequestableData>();
            dataMap.put(pressRecord.getLevel(), cubeLevel);
        }
        cubeLevel.setPressure(pressRecord);
    }

    public int size() {
        return dataMap.size();
    }

    public void validate() {
        Iterator<CubeLevel<AbstractRequestableData, AbstractRequestableData>> iter = dataMap
                .values().iterator();
        while (iter.hasNext()) {
            CubeLevel<AbstractRequestableData, AbstractRequestableData> cubeLevel = iter
                    .next();
            if (cubeLevel.getParam() == null || cubeLevel.getPressure() == null) {
                iter.remove();
            }
        }
    }

    /**
     * @return the dataMap
     */
    public Map<Level, CubeLevel<AbstractRequestableData, AbstractRequestableData>> getDataMap() {
        return dataMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDataValue
     * ()
     */
    @Override
    public Object getDataValue(Object arg) throws VizException {
        List<CubeLevel<Object, Object>> rval = new ArrayList<CubeLevel<Object, Object>>(
                dataMap.size());
        for (CubeLevel<AbstractRequestableData, AbstractRequestableData> cubeLevel : dataMap
                .values()) {
            if (cubeLevel.getParam() != null && cubeLevel.getPressure() != null) {
                rval.add(new CubeLevel<Object, Object>(cubeLevel.getPressure()
                        .getDataValue(arg), cubeLevel.getParam().getDataValue(
                        arg)));
            }
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.data.AbstractRequestableData#getDependencies
     * ()
     */
    @Override
    public List<AbstractRequestableData> getDependencies() {
        List<AbstractRequestableData> results = new ArrayList<AbstractRequestableData>();
        for (CubeLevel<AbstractRequestableData, AbstractRequestableData> level : dataMap
                .values()) {
            results.add(level.getPressure());
            results.add(level.getParam());
        }
        return results;
    }
}
