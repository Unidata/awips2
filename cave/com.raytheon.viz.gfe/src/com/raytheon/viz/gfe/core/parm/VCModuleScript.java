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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.griddata.WeatherGridData;
import com.raytheon.viz.gfe.core.parm.VCModule.CalcGridArg;
import com.raytheon.viz.gfe.core.parm.VCModule.CalcHistoryArg;
import com.raytheon.viz.gfe.core.parm.VCModule.GetInventoryArg;

/**
 * A PythonScript subclass that replaces the Python/C++ interface that was
 * available to AWIPS1's <code>VCModule</code> class. Primarily used for
 * Python->Java data type conversions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class VCModuleScript extends PythonScript {

    private static final String INSTANCE_NAME = "instance";

    private List<String> tempGridNames;

    /**
     * Constructor.
     * 
     * @param aFilePath
     *            the path to the python script
     * @param anIncludePath
     *            the python include path, with multiple directories being
     *            separated by ":"
     * @param aClassLoader
     *            the Java classloader to use for importing Java classes inside
     *            python
     * @throws JepException
     */
    public VCModuleScript(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader) throws JepException {
        super(aFilePath, anIncludePath, aClassLoader, Arrays.asList(
                "import JUtil", "import numpy"));
        instantiatePythonClass(INSTANCE_NAME, "VCParm", null);
        this.tempGridNames = new ArrayList<String>();
    }

    /**
     * Retrieves a list of argument names for the specified Python method.
     * 
     * @param method
     *            The name of the method.
     * @return List of argument names for the specified Python method, except
     *         for "self".
     * @throws JepException
     */
    public List<String> getArgumentNames(String method) throws JepException {
        String[] argNames = getArgumentNames(method, INSTANCE_NAME);
        List<String> retVal = new ArrayList<String>(argNames.length);
        for (String arg : argNames) {
            if (!arg.equals("self")) {
                retVal.add(arg);
            }
        }

        return retVal;
    }

    /**
     * Executes the calcGrid() method.
     * 
     * @param methodArgs
     *            A <code>Map</code> containing argument names and values.
     * @param type
     *            The <code>GridType</code> of the grid being calculated.
     * @return The virtual grid. For SCALARs, this will be a
     *         <code>Grid2DFloat</code>. For VECTORs, this will be an array
     *         containing 2 <code>Grid2DFloat</code> objects (mag, dir). For
     *         WEATHER and DISCRETE, this will be an array containing a
     *         <code>Grid2DByte</code> and a list of keys.
     * @throws JepException
     */
    public Object executeCalcGrid(Map<String, Object> methodArgs, GridType type)
            throws JepException {
        internalExecute("calcGrid", INSTANCE_NAME, methodArgs);
        Object obj = decodeGD(type);
        jep.eval(RESULT + " = None");
        return obj;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.python.PythonScript#execute(java.lang.String,
     * java.util.Map)
     */
    @Override
    public Object execute(String methodName, Map<String, Object> args)
            throws JepException {
        internalExecute(methodName, INSTANCE_NAME, args);
        jep.eval(RESULT + " = JUtil.pyValToJavaObj(" + RESULT + ")");
        Object obj = jep.getValue(RESULT);
        jep.eval(RESULT + " = None");
        return obj;
    }

    /**
     * Override of the evaluateArgument() method which handles the custom
     * argument structures defined in <code>VCModule</code>. Important so that
     * we can convert the Java datatypes to Python native objects (and numpy
     * arrays for the grids).
     * 
     * @param argName
     *            The argument name.
     * @param argValue
     *            the value of the argument
     * 
     * @see com.raytheon.uf.common.python.PythonInterpreter#evaluateArgument(java
     *      .lang.String, java.lang.Object)
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof GetInventoryArg) {
            GetInventoryArg arg = (GetInventoryArg) argValue;
            StringBuilder sb = new StringBuilder(512);
            sb.append(argName + " = [");
            for (int i = 0; i < arg.trs.size(); i++) {
                long[] tuple = arg.trs.get(i);
                sb.append('(');
                sb.append(tuple[0]);
                sb.append(", ");
                sb.append(tuple[1]);
                sb.append(')');
                if (i < arg.trs.size() - 1) {
                    sb.append(',');
                }
            }
            sb.append(']');
            jep.eval(sb.toString());
        } else if (argValue instanceof CalcHistoryArg) {
            CalcHistoryArg castedArg = (CalcHistoryArg) argValue;

            StringBuilder sb = new StringBuilder(1024);
            sb.append(argName + " = [");
            for (int i = 0; i < castedArg.histEntries.size(); i++) {
                Object[] timeAndHistList = castedArg.histEntries.get(i);
                long[] tr = (long[]) timeAndHistList[0];
                List<String> hist = (List<String>) timeAndHistList[1];

                sb.append("((");
                sb.append(tr[0]);
                sb.append(", ");
                sb.append(tr[1]);
                sb.append("), [");
                for (int j = 0; j < hist.size(); j++) {
                    sb.append("\"" + hist.get(j) + "\"");
                    if (j < hist.size() - 1) {
                        sb.append(',');
                    }
                }
                sb.append("])");
                if (i < castedArg.histEntries.size() - 1) {
                    sb.append(',');
                }
            }
            sb.append(']');
            jep.eval(sb.toString());
        } else if (argValue instanceof CalcGridArg) {
            CalcGridArg castedArg = (CalcGridArg) argValue;

            StringBuilder sb = new StringBuilder(768);
            sb.append(argName + " = [");

            for (int i = 0; i < castedArg.argTuples.size(); i++) {
                Object[] tuple = castedArg.argTuples.get(i);
                long[] tr = (long[]) tuple[0];
                IGridData gd = (IGridData) tuple[1];
                Grid2DBit mask = (Grid2DBit) tuple[2];

                sb.append("((");
                sb.append(tr[0]);
                sb.append(", ");
                sb.append(tr[1]);
                sb.append("), ");
                sb.append(encodeGridAndMask(gd, mask, i));
                sb.append(')');

                if (i < castedArg.argTuples.size() - 1) {
                    sb.append(',');
                }
            }
            sb.append(']');
            jep.eval(sb.toString());

        } else {
            super.evaluateArgument(argName, argValue);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.PythonScript#cleanupArgs(java.util.List)
     */
    @Override
    protected void cleanupArgs(List<String> args) throws JepException {
        super.cleanupArgs(args);
        for (String gridName : tempGridNames) {
            jep.eval(gridName + " = None");
        }
        tempGridNames.clear();
    }

    private String encodeGridAndMask(IGridData gd, Grid2DBit mask, int offset)
            throws JepException {
        StringBuilder jepString = new StringBuilder(256);
        String prefix = gd.getParm().getParmID().getParmName()
                + Integer.toHexString(gd.getParm().getParmID().hashCode())
                + "_" + offset + "_";

        if (gd instanceof ScalarGridData) {
            ScalarGridData grid = (ScalarGridData) gd;
            Grid2DFloat f = (grid.getScalarSlice()).getScalarGrid();
            String name = prefix + "grid";
            jep.setNumeric(name, f.getFloats(), f.getXdim(), f.getYdim());
            jepString.append(name);
            jepString.append(", ");
            tempGridNames.add(name);
        } else if (gd instanceof VectorGridData) {
            VectorGridData grid = (VectorGridData) gd;
            Grid2DFloat mag = (grid.getVectorSlice()).getMagGrid();
            Grid2DFloat dir = (grid.getVectorSlice()).getDirGrid();
            String magName = prefix + "Mag";
            String dirName = prefix + "Dir";
            jep.setNumeric(magName, mag.getFloats(), mag.getXdim(),
                    mag.getYdim());
            jep.setNumeric(dirName, dir.getFloats(), dir.getXdim(),
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
            jep.setNumeric(name, bytes.getBytes(), bytes.getXdim(),
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
            jep.setNumeric(name, bytes.getBytes(), bytes.getXdim(),
                    bytes.getYdim());
            jepString.append('(');
            jepString.append(name);
            jepString.append(',');
            DiscreteKey[] keys = grid.getDiscreteSlice().getKey();
            ArrayList<String> stringKeys = new ArrayList<String>(keys.length);
            for (DiscreteKey k : keys) {
                stringKeys.add(k.toString());
            }
            jepString.append(PyUtil.listToTuple(stringKeys));
            jepString.append("), ");
            tempGridNames.add(name);
        }

        String maskName = prefix + "mask";
        jep.setNumeric(maskName, mask.getBytes(), mask.getXdim(),
                mask.getYdim());
        jepString.append(maskName);
        tempGridNames.add(maskName);

        return jepString.toString();
    }

    private Object decodeGD(GridType type) throws JepException {
        Object result = null;
        boolean resultFound = (Boolean) jep.getValue(RESULT + " is not None");

        if (resultFound) {
            int xDim, yDim = 0;
            switch (type) {
            case SCALAR:
                float[] scalarData = (float[]) jep.getValue(RESULT
                        + ".astype(numpy.float32)");
                xDim = (Integer) jep.getValue(RESULT + ".shape[1]");
                yDim = (Integer) jep.getValue(RESULT + ".shape[0]");
                result = new Grid2DFloat(xDim, yDim, scalarData);
                break;
            case VECTOR:
                float[] mag = (float[]) jep.getValue(RESULT
                        + "[0].astype(numpy.float32)");
                float[] dir = (float[]) jep.getValue(RESULT
                        + "[1].astype(numpy.float32)");
                xDim = (Integer) jep.getValue(RESULT + "[0].shape[1]");
                yDim = (Integer) jep.getValue(RESULT + "[0].shape[0]");

                Grid2DFloat magGrid = new Grid2DFloat(xDim, yDim, mag);
                Grid2DFloat dirGrid = new Grid2DFloat(xDim, yDim, dir);
                result = new Grid2DFloat[] { magGrid, dirGrid };
                break;
            case WEATHER:
            case DISCRETE:
                byte[] bytes = (byte[]) jep.getValue(RESULT
                        + "[0].astype(numpy.int8)");
                String[] keys = (String[]) jep.getValue(RESULT + "[1]");
                xDim = (Integer) jep.getValue(RESULT + "[0].shape[1]");
                yDim = (Integer) jep.getValue(RESULT + "[0].shape[0]");

                Grid2DByte grid = new Grid2DByte(xDim, yDim, bytes);
                List<String> keysList = new ArrayList<String>();
                Collections.addAll(keysList, keys);

                result = new Object[] { grid, keysList };
                break;
            }
        }

        return result;
    }

    /**
     * Calls hasattr() against the <code>VCModule</code> instance and returns
     * whether or not the specified attribute exists.
     * 
     * @param attribute
     *            Attribute name.
     * @return Whether or not the specified attribute exists.
     * @throws JepException
     */
    public boolean hasAttr(String attribute) throws JepException {
        jep.eval(RESULT + " = hasattr(" + INSTANCE_NAME + ", \"" + attribute
                + "\")");
        boolean result = ((Boolean) jep.getValue(RESULT)).booleanValue();
        jep.eval(RESULT + " = None");
        return result;
    }

}
