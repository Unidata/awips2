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
package com.raytheon.uf.edex.activetable;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.activetable.PracticeProductOfftimeRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2011            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class PracticeProductOfftimeHandler implements
        IRequestHandler<PracticeProductOfftimeRequest> {

    private static String timeUtil;

    private static String includePath;
    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        timeUtil = pathMgr.getFile(commonCx,
                "python" + File.separator + "TimeUtil.py").getPath();
        includePath = pathMgr.getFile(commonCx, "python").getPath();
    }

    @Override
    public Object handleRequest(PracticeProductOfftimeRequest request)
            throws Exception {
        String drtString = request.getDrtString();
        request.setOffsetSeconds(offsetSecs(drtString));
        request.setHeaders(new HashMap<String, Object>());
        EDEXUtil.getMessageProducer().sendAsync("practiceVtecOffsetRoute",
                request);

        return null;

    }

    /**
     * @param request
     * @return The product text of the request
     */
    public String process(PracticeProductOfftimeRequest request) {
        return request.getProductText();
    }

    /**
     * Call determineDrtOffset() (in Python) to determine the offset in seconds
     * for a particular string.
     * 
     * @param drtString
     *            The string to convert
     * @return the offset in seconds
     * @throws Exception
     *             if the Python call fails
     */
    protected int offsetSecs(String drtString) throws Exception {
        int rtnVal = 0;

        if (drtString != null) {

            Map<String, Object> args = new HashMap<String, Object>();
            args.put("timeStr", drtString);

            PythonScript python = null;
            Object obj = null;
            try {
                python = new PythonScript(timeUtil, includePath, getClass()
                        .getClassLoader());
                obj = python.execute("determineDrtOffset", args);
            } catch (JepException e) {
                throw new Exception("Python exception:" + e.getMessage(), e);
            } finally {
                if (python != null) {
                    python.dispose();
                }
            }
            // determineDrtOffset returns a tuple.
            // In the current implementation, it comes back as a String:
            // "( -12345678, ...)", but it might change in the future.
            if (obj instanceof String) {
                String objStr = (String) obj;
                String intStr = objStr.substring(1, objStr.indexOf(","));
                rtnVal = Integer.parseInt(intStr);
            } else if (obj instanceof List) {
                rtnVal = (Integer) ((List<?>) obj).get(0);
            } else if (obj.getClass().isArray()) {
                rtnVal = (Integer) ((Object[]) obj)[0];
            }
        }
        return rtnVal;
    }
}
