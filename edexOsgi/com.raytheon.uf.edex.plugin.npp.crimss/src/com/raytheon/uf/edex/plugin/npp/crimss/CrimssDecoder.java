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
package com.raytheon.uf.edex.plugin.npp.crimss;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import ucar.ma2.Array;
import ucar.ma2.Index;
import ucar.ma2.Index2D;
import ucar.nc2.Group;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.npp.crimss.CrimssRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.npp.AbstractNPPDecoder;
import com.raytheon.uf.edex.plugin.npp.crimss.dao.CrimssDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CrimssDecoder extends AbstractNPPDecoder {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrimssDecoder.class);

    private final String pluginName;

    private final CrimssDao dao;

    public CrimssDecoder(String pluginName) {
        this.pluginName = pluginName;
        CrimssDao dao = null;
        try {
            dao = new CrimssDao(pluginName);
        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        this.dao = dao;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.npp.AbstractNPPDecoder#decodeNetcdf(ucar.
     * nc2.NetcdfFile, com.raytheon.uf.common.time.DataTime,
     * com.raytheon.edex.esb.Headers)
     */
    @Override
    protected Object decodeNetcdf(NetcdfFile dataFile, DataTime dataTime,
            Headers headers) {
        try {
            Group root = dataFile.getRootGroup();
            CrimssRecord[] records = decodeMetadata(root, dataTime);
            decodeData(records, root);
            return records;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error decoding CrIMSS", e);
        }
        return new PluginDataObject[0];
    }

    private CrimssRecord[] decodeMetadata(Group root, DataTime time)
            throws DecoderException, PluginException, IOException {
        Variable latitude = null;
        Variable longitude = null;
        for (Variable var : root.getVariables()) {
            if (var.getFullName().startsWith("Latitude@")) {
                latitude = var;
            } else if (var.getFullName().startsWith("Longitude@")) {
                longitude = var;
            }
        }
        if (latitude == null || longitude == null) {
            throw new DecoderException("Unable to find lat/lon information");
        }
        Map<File, PointDataContainer> containerMap = new HashMap<File, PointDataContainer>();

        Array latArray = latitude.read();
        Array lonArray = longitude.read();
        int numRecs = latitude.getDimension(0).getLength();
        CrimssRecord[] records = new CrimssRecord[numRecs];
        for (int i = 0; i < numRecs; i++) {
            records[i] = new CrimssRecord();
            records[i].setPluginName(pluginName);
            records[i].setDataTime(time);
            records[i].setLatitude((double) latArray.getFloat(i));
            records[i].setLongitude((double) lonArray.getFloat(i));
            records[i].constructDataURI();
            File storageFile = dao.getFullFilePath(records[i]);
            PointDataContainer pdc = containerMap.get(storageFile);
            if (pdc == null) {
                pdc = PointDataContainer.build(dao
                        .getPointDataDescription(null));
                containerMap.put(storageFile, pdc);
            }
            PointDataView pdv = pdc.append();
            records[i].setPointDataView(pdv);
        }
        return records;
    }

    private void decodeData(CrimssRecord[] records, Group root)
            throws IOException {
        PointDataDescription desc = dao.getPointDataDescription(null);
        for (Variable var : root.getVariables()) {
            String name = var.getFullName().split("@")[0];
            for (ParameterDescription param : desc.parameters) {
                if (name.equals(param.getParameterName())) {
                    int sourceNumDims = var.getShape().length;
                    int destNumDims = param.getNumDims();
                    // TODO we could do some sweet units consistency checking to
                    // make sure that units match what we are expecting
                    if (sourceNumDims == destNumDims) {
                        if (sourceNumDims == 2) {
                            read2D(records, var, name);
                        } else if (sourceNumDims == 1) {
                            read1D(records, var, name);
                        }
                    } else if (sourceNumDims == 1 && destNumDims == 2) {
                        read1Dto2D(records, var, name);
                    }
                }
            }
        }
    }

    private static void read1D(CrimssRecord[] records, Variable var,
            String parameter) throws IOException {
        Array data = var.read();
        for (int i = 0; i < records.length; i++) {
            PointDataView pdv = records[i].getPointDataView();
            pdv.setFloat(parameter, data.getFloat(i));
        }
    }

    /**
     * As an example of when this occurs, in a given file temperature readings
     * are on the same pressure levels for all locations so these pressures are
     * stored in one dimensional data. We need this pressure information stored
     * for each location so it becomes two dimensional in our storage.
     */
    private static void read1Dto2D(CrimssRecord[] records, Variable var,
            String parameter) throws IOException {
        int numLevels = var.getDimension(0).getLength();
        Array data = var.read();
        for (int i = 0; i < records.length; i++) {
            PointDataView pdv = records[i].getPointDataView();
            for (int j = 0; j < numLevels; j++) {
                float value = data.getFloat(j);
                pdv.setFloat(parameter, value, j);
            }
        }
    }

    private static void read2D(CrimssRecord[] records, Variable var,
            String parameter) throws IOException {
        int numLevels = var.getDimension(1).getLength();
        Array data = var.read();
        Index index = new Index2D(new int[] { records.length, numLevels });
        for (int i = 0; i < records.length; i++) {
            PointDataView pdv = records[i].getPointDataView();
            for (int j = 0; j < numLevels; j++) {
                float value = data.getFloat(index.set(i, j));
                pdv.setFloat(parameter, value, j);
            }
        }
    }
}
