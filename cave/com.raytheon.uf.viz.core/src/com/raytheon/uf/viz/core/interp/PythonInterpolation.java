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
package com.raytheon.uf.viz.core.interp;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PythonInterpolation implements IInterpolation {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(PythonInterpolation.class);

    /** the python function to call */
    protected String function;

    protected String localizationScript = "scripts/interpolation.py";

    protected static PythonScript ps = null;

    protected File interpolationFile = null;

    public PythonInterpolation(ScaleType scale) {
        switch (scale) {
        case LIN: {
            function = "linInterpolate";
            break;
        }
        default: {
            function = "logInterpolate";
        }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.interp.IInterpolation#interpolate(
     * com.raytheon.uf.viz.core.interp.InterpolationRequest,
     * org.geotools.coverage.grid.GridGeometry2D)
     */
    @Override
    public InterpolationResult interpolate(InterpolationRequest request) {
        long t0 = System.currentTimeMillis();

        InterpolationContainer container = new InterpolationContainer(request
                .getXData(), request.getYData(), request.getZData());
        try {
            if (interpolationFile == null) {
                interpolationFile = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(localizationScript)
                        .getFile();
            }
            if (ps == null) {
                String file = interpolationFile.getAbsolutePath();

                ps = new PythonScript(file, "", PythonInterpolation.class
                        .getClassLoader());
            }
            Map<String, Object> args = new HashMap<String, Object>();
            args.put("data", container);
            args.put("gridX", request.getGridX());
            args.put("gridY", request.getGridY());
            args.put("xMin", request.getMinX());
            args.put("xMax", request.getMaxX());
            args.put("yMin", request.getMinY());
            args.put("yMax", request.getMaxY());
            ps.execute(function, args);
            System.out.println("Time to execute interpolation: "
                    + (System.currentTimeMillis() - t0));
            InterpolationResult result = new InterpolationResult();

            result.setValues(container.getInterpValues());

            float[] xspace = container.getXSpace();
            float[] yspace = container.getYSpace();
            double minX = Math.min(xspace[0], xspace[xspace.length - 1]);
            double maxX = Math.max(xspace[0], xspace[xspace.length - 1]);
            double minY = Math.min(yspace[0], yspace[yspace.length - 1]);
            double maxY = Math.max(yspace[0], yspace[yspace.length - 1]);
            GeneralEnvelope env = new GeneralEnvelope(
                    new double[] { minX, minY }, new double[] { maxX, maxY });
            GeneralGridEnvelope range = new GeneralGridEnvelope(new int[] { 0,
                    0 }, new int[] { xspace.length, yspace.length }, false);
            result.setGeometry(new GridGeometry2D(range, env));
            return result;
        } catch (JepException e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error running interpolation", e);
        }
        return null;
    }

}
