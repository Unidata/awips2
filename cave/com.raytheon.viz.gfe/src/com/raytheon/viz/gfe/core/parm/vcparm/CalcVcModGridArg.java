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
package com.raytheon.viz.gfe.core.parm.vcparm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jep.Jep;
import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2012            dgilling     Initial creation
 * Oct 29, 2013  2476       njensen     Renamed numeric methods to numpy
 * Oct 31, 2013     #2508  randerso    Change to use DiscreteGridSlice.getKeys()
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class CalcVcModGridArg implements IVcModuleArgument {

    // simulates the AWIPS1 tuple used, which contained an encoded time
    // range (long[]), the grid data (IGridData), and a bit mask of valid
    // points in the grid data (Grid2DBit).
    private List<Object[]> argTuples;

    public CalcVcModGridArg(List<Object[]> argTuples) {
        this.argTuples = new ArrayList<Object[]>(argTuples);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.vcparm.IVcModuleArgument#evaluateArgument
     * (jep.Jep)
     */
    @Override
    public Collection<String> evaluateArgument(final Jep instance,
            String argName) throws JepException {
        StringBuilder sb = new StringBuilder(768);
        Collection<String> tempNames = new ArrayList<String>(
                (argTuples.size() * 2));

        sb.append(argName + " = [");
        for (int i = 0; i < argTuples.size(); i++) {
            Object[] tuple = argTuples.get(i);
            long[] tr = (long[]) tuple[0];
            IGridData gd = (IGridData) tuple[1];
            Grid2DBit mask = (Grid2DBit) tuple[2];

            sb.append("((");
            sb.append(tr[0]);
            sb.append("L, ");
            sb.append(tr[1]);
            sb.append("L), ");
            tempNames.addAll(encodeGridAndMask(gd, mask, i, sb, instance));
            sb.append(')');

            if (i < (argTuples.size() - 1)) {
                sb.append(',');
            }
        }
        sb.append(']');
        instance.eval(sb.toString());

        return tempNames;
    }

    private Collection<String> encodeGridAndMask(IGridData gd, Grid2DBit mask,
            int offset, StringBuilder sb, Jep jep) throws JepException {
        StringBuilder jepString = new StringBuilder(256);
        String prefix = gd.getParm().getParmID().getParmName()
                + Integer.toHexString(gd.getParm().getParmID().hashCode())
                + "_" + offset + "_";
        Collection<String> tempGridNames = new ArrayList<String>(2);

        if (gd instanceof ScalarGridData) {
            ScalarGridData grid = (ScalarGridData) gd;
            Grid2DFloat f = (grid.getScalarSlice()).getScalarGrid();
            String name = prefix + "grid";
            jep.setNumpy(name, f.getFloats(), f.getXdim(), f.getYdim());
            jepString.append(name);
            jepString.append(", ");
            tempGridNames.add(name);
        } else if (gd instanceof VectorGridData) {
            VectorGridData grid = (VectorGridData) gd;
            Grid2DFloat mag = (grid.getVectorSlice()).getMagGrid();
            Grid2DFloat dir = (grid.getVectorSlice()).getDirGrid();
            String magName = prefix + "Mag";
            String dirName = prefix + "Dir";
            jep.setNumpy(magName, mag.getFloats(), mag.getXdim(),
                    mag.getYdim());
            jep.setNumpy(dirName, dir.getFloats(), dir.getXdim(),
                    dir.getYdim());
            jepString.append('(');
            jepString.append(magName);
            jepString.append(',');
            jepString.append(dirName);
            jepString.append("), ");
            tempGridNames.add(magName);
            tempGridNames.add(dirName);
        } else if (gd instanceof WeatherGridData) {
            WeatherGridData grid = (WeatherGridData) gd;
            Grid2DByte bytes = grid.getWeatherSlice().getWeatherGrid();
            String name = prefix + "grid";
            jep.setNumpy(name, bytes.getBytes(), bytes.getXdim(),
                    bytes.getYdim());
            jepString.append('(');
            jepString.append(name);
            jepString.append(',');
            WeatherKey[] keys = grid.getWeatherSlice().getKeys();
            ArrayList<String> stringKeys = new ArrayList<String>(keys.length);
            for (WeatherKey k : keys) {
                stringKeys.add(k.toString());
            }
            jepString.append(PyUtil.listToTuple(stringKeys));
            jepString.append("), ");
            tempGridNames.add(name);
        } else if (gd instanceof DiscreteGridData) {
            DiscreteGridData grid = (DiscreteGridData) gd;
            Grid2DByte bytes = grid.getDiscreteSlice().getDiscreteGrid();
            String name = prefix + "grid";
            jep.setNumpy(name, bytes.getBytes(), bytes.getXdim(),
                    bytes.getYdim());
            jepString.append('(');
            jepString.append(name);
            jepString.append(',');
            DiscreteKey[] keys = grid.getDiscreteSlice().getKeys();
            ArrayList<String> stringKeys = new ArrayList<String>(keys.length);
            for (DiscreteKey k : keys) {
                stringKeys.add(k.toString());
            }
            jepString.append(PyUtil.listToTuple(stringKeys));
            jepString.append("), ");
            tempGridNames.add(name);
        }

        String maskName = prefix + "mask";
        jep.setNumpy(maskName, mask.getBytes(), mask.getXdim(),
                mask.getYdim());
        jepString.append(maskName);
        sb.append(jepString);
        tempGridNames.add(maskName);

        return tempGridNames;
    }

}
