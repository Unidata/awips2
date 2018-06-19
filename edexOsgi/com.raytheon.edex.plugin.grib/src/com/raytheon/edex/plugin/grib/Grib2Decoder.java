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
package com.raytheon.edex.plugin.grib;

import java.util.HashMap;
import java.util.Map;

import jep.NDArray;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.python.decoder.PythonDecoder;

/**
 * Grib decoder implementation for decoding grib version 2 files. All the real
 * work is handed off to python.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 04, 2013  2402     bsteffen    Initial creation
 * Apr 29, 2015  4259     njensen     Updated for new JEP API
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class Grib2Decoder extends PythonDecoder {

    public Grib2Decoder() {
        super();
        setPluginName("grib");
        setPluginFQN("com.raytheon.edex.plugin.grib");
        setModuleName("GribDecoder");
        setRecordClassname(GridRecord.class.toString());
        setCache(true);
    }

    public GridRecord[] decode(GribDecodeMessage message) throws GribException {
        Map<String, Object> argMap = new HashMap<String, Object>(4);
        argMap.put("filePath", message.getFileName());
        argMap.put("startPosition", message.getStartPosition());
        argMap.put("messageLength", message.getMessageLength());
        try {
            PluginDataObject[] pdos = decode(argMap);
            GridRecord[] records = new GridRecord[pdos.length];
            for (int i = 0; i < pdos.length; i++) {
                records[i] = (GridRecord) pdos[i];
                if (records[i].getMessageData() instanceof NDArray) {
                    records[i].setMessageData(((NDArray<?>) records[i]
                            .getMessageData()).getData());
                }
            }
            return records;
        } catch (Exception e) {
            throw new GribException("Failed to decode file: ["
                    + message.getFileName() + "]", e);
        }

    }
}
