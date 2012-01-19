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
package com.raytheon.uf.common.datastorage.remote.handlers;

import java.io.File;
import java.lang.reflect.Method;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.remote.requests.ThriftDataRequest;
import com.raytheon.uf.common.datastorage.remote.requests.ThriftDataResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Uses reflection to invoke an IDataStore method instead of creating a Handler
 * for each method
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RetrieveDataHandler implements IRequestHandler<ThriftDataRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.thrift.IRequestHandler#handleRequest(com.raytheon
     * .uf.common.thrift.IServerRequest)
     */
    @Override
    public Object handleRequest(ThriftDataRequest req) throws Exception {
        File file = new File(req.getFile());
        if (file.getParentFile().exists() == false) {
            file = new File(new File(System.getProperty("edex.home")
                    + File.separator + "data" + File.separator + "hdf5"), file
                    .getAbsolutePath());
        }
        IDataStore store = DataStoreFactory.getDataStore(file);
        Class<?>[] classes = new Class<?>[req.getParameterTypes().length];
        int i = 0;
        for (String clazz : req.getParameterTypes()) {
            classes[i++] = Class.forName(clazz);
        }
        Method m = store.getClass().getMethod(req.getMethod(), classes);
        Object obj = m.invoke(store, req.getParameters());
        if (obj == null) {
            throw new Exception("datastore returned null");
        } else if (obj instanceof IDataRecord) {
            return new ThriftDataResponse((IDataRecord) obj);
        } else if (obj instanceof IDataRecord[]) {
            return new ThriftDataResponse((IDataRecord[]) obj);
        } else if (obj instanceof String[]) {
            return new ThriftDataResponse((String[]) obj);
        } else {
            throw new Exception("datastore returned invalid response object: "
                    + obj.getClass().getName());
        }
    }
}
