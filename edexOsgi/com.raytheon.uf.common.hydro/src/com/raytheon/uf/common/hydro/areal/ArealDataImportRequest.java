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
package com.raytheon.uf.common.hydro.areal;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Areal import request object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2018   6979     mduff       Initial creation
 * Mar 11, 2020  19533  mgamazaychikov Added arealImportType
 *
 * </pre>
 *
 * @author mpduff
 */

@DynamicSerialize
public class ArealDataImportRequest implements IServerRequest {

    @DynamicSerializeElement
    private String arealDataFileName;

    @DynamicSerializeElement
    private ArealTypeSelection arealDataType;

    @DynamicSerializeElement
    private String arealImportType;

    public ArealDataImportRequest() {
        // Default
    }

    public ArealDataImportRequest(String arealDataFileName, String arealImportType,
            ArealTypeSelection arealDataType) {
        this.arealDataFileName = arealDataFileName;
        this.arealImportType = arealImportType;
        this.arealDataType = arealDataType;
    }

    public String getArealDataFileName() {
        return arealDataFileName;
    }

    public void setArealDataFileName(String arealDataFileName) {
        this.arealDataFileName = arealDataFileName;
    }

    public ArealTypeSelection getArealDataType() {
        return arealDataType;
    }

    public void setArealDataType(ArealTypeSelection arealDataType) {
        this.arealDataType = arealDataType;
    }

    public String getArealImportType() {
        return arealImportType;
    }

    public void setArealImportType(String arealImportType) {
        this.arealImportType = arealImportType;
    }
}
