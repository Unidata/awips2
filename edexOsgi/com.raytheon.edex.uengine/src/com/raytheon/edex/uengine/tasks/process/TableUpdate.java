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

package com.raytheon.edex.uengine.tasks.process;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Updates a row in the database by unmarshalling an xml String and then storing
 * the equivalent object in the correct table.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 9/25/2007               njensen     Initial creation.
 * 10/17/2007   482        grichard    Changed method to dao.saveOrUpdate().
 * 12/17/2007   639        grichard    Restored method call dao.saveOrUpdate().
 * 
 * </pre>
 * 
 * @author njensen
 */
public class TableUpdate extends ScriptTask {

    private final String className;

    private final String xml;

    private final String database;

    /**
     * Constructor
     * 
     * @param aClassName
     *            the class name of the object to update
     * @param aXml
     *            the xml representation of the object
     */
    public TableUpdate(String database, String aClassName, String aXml) {
        this.database = database;
        className = aClassName;
        xml = aXml;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        CoreDao dao = null;

        AbstractResponseMessage msg = null;
        try {
            System.out.println(xml);
            PersistableDataObject obj = (PersistableDataObject) SerializationUtil
                    .unmarshalFromXml(xml);
            System.out.println(obj);
            dao = new CoreDao(
                    DaoConfig.forClass(database, className));
            dao.saveOrUpdate(obj);
        } catch (Exception e) {
            msg = ResponseMessageError.generateErrorResponse(
                    "Error updating row in the database.", e);
        }

        if (msg == null) {
            msg = new ResponseMessageGeneric("Row successfully updated.");
        }

        return msg;
    }
}
