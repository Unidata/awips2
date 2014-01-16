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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 17, 2010           bsteffen    Initial creation
 * Jun 04, 2013  2041     bsteffen    Switch derived parameters to use
 *                                    concurrent python for threading.
 * Jan 14, 2014  2661     bsteffen    Make vectors u,v only
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DerivedRequestableData extends AbstractRequestableData {

    private Map<Object, WeakReference<DerivedParameterRequest>> cache = Collections
            .synchronizedMap(new HashMap<Object, WeakReference<DerivedParameterRequest>>());

    private DerivedParameterRequest request;

    public DerivedRequestableData(AbstractRequestableData baseRequester,
            DerivedParameterRequest request) {
        super(baseRequester);
        this.request = request;
    }

    public DerivedRequestableData(DerivedParameterRequest request) {
        this.request = request;
    }

    public DerivedParameterRequest getRequest() {
        return request;
    }

    @Override
    public Object getDataValue(Object arg) throws VizException {
        DerivedParameterRequest request = createDerparRequest(arg);
        try {
            List<IDataRecord> finalResult = DerivedParameterGenerator
                    .calculate(request);
            if (finalResult != null && !finalResult.isEmpty()) {
                if (finalResult.size() == 2 || finalResult.size() == 1) {
                    for (IDataRecord rec : finalResult) {
                        rec.setName(request.getParameterAbbreviation());
                    }
                    return finalResult.toArray(new IDataRecord[0]);
                } else {
                    throw new VizException(
                            "Error processing derived parameter, expecting scalar or vector data.  Vector data must return u and v components.");
                }
            }
        } catch (ExecutionException e) {
            throw new VizException("Error executing Derived Parameter.", e);
        }
        return null;
    }

    /**
     * 
     * @param obj
     *            the pdo which needs to be derived
     * @param cache
     *            a map of data uri's to objects which have already been
     *            retrieved
     * @return
     * @throws VizException
     */
    private synchronized DerivedParameterRequest createDerparRequest(Object arg)
            throws VizException {
        if (cache.containsKey(arg)) {
            DerivedParameterRequest request = cache.get(arg).get();
            if (request != null) {
                return request;
            }
        }
        DerivedParameterRequest request = new DerivedParameterRequest(
                this.request);
        List<Object> baseParams = request.getBaseParams();
        ArrayList<Object> arguments = new ArrayList<Object>(baseParams.size());
        for (Object param : baseParams) {
            arguments.add(getArgument(param, arg));
        }
        request.setArgumentRecords(arguments.toArray(new Object[] {}));
        cache.put(arg, new WeakReference<DerivedParameterRequest>(request));
        return request;
    }

    private Object getArgument(Object param, Object frameworkArg)
            throws VizException {
        if (param instanceof DerivedRequestableData) {
            return ((DerivedRequestableData) param)
                    .createDerparRequest(frameworkArg);
        } else if (param instanceof AggregateRequestableData) {
            List<AbstractRequestableData> recs = ((AggregateRequestableData) param)
                    .getSourceRecords();
            List<Object> arg = new ArrayList<Object>(recs.size());
            for (AbstractRequestableData rec : recs) {
                arg.add(getArgument(rec, frameworkArg));
            }
            return arg;
        } else if (param instanceof CubeRequestableData) {
            Map<Level, CubeLevel<AbstractRequestableData, AbstractRequestableData>> dataMap = ((CubeRequestableData) param)
                    .getDataMap();
            List<CubeLevel<Object, Object>> arg = new ArrayList<CubeLevel<Object, Object>>(
                    dataMap.size());
            for (CubeLevel<AbstractRequestableData, AbstractRequestableData> cubeLevel : dataMap
                    .values()) {
                if (cubeLevel.getParam() != null
                        && cubeLevel.getPressure() != null) {
                    arg.add(new CubeLevel<Object, Object>(getArgument(
                            cubeLevel.getPressure(), frameworkArg),
                            getArgument(cubeLevel.getParam(), frameworkArg)));
                }
            }
            return arg;
        } else if (param instanceof AbstractRequestableData) {
            return ((AbstractRequestableData) param).getDataValue(frameworkArg);
        } else if (param instanceof float[] || param instanceof FloatDataRecord) {
            return param;
        }
        throw new VizException("Unknown BaseParam for DerivedParam of type: "
                + param.getClass().getSimpleName());
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
        for (Object param : request.getBaseParams()) {
            if (param instanceof AbstractRequestableData) {
                results.add((AbstractRequestableData) param);
            }
        }
        return results;
    }

}
