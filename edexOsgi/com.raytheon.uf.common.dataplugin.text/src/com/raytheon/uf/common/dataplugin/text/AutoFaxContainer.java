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
package com.raytheon.uf.common.dataplugin.text;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.AutoFaxRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Serialization wrapper for a list of AutoFaxRecords
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2010            bfarmer     Initial creation
 * May 15, 2014 2536       bclement    removed ISerializableObject
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */
@DynamicSerialize
public class AutoFaxContainer {
    @DynamicSerializeElement
    private List<AutoFaxRecord> autoFaxList = new ArrayList<AutoFaxRecord>();

    @DynamicSerializeElement
    private String errorMessage = null;

    public List<AutoFaxRecord> getAutoFaxList() {
        return autoFaxList;
    }

    public void setAutoFaxList(List<AutoFaxRecord> autoFaxList) {
        this.autoFaxList = autoFaxList;
    }

    public void add(AutoFaxRecord id) {
        autoFaxList.add(id);
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

}
