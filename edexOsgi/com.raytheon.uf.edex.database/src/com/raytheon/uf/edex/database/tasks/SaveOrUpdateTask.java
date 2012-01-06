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

package com.raytheon.uf.edex.database.tasks;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Task to execute an insert or update to the database
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/11/2008   1777       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SaveOrUpdateTask {

    /** The database name to insert into */
    private String dbName;

    /** The marshalled objects * */
    private List<String> marshalledObjects;

    /**
     * Creates a new SaveOrUpdateTask object
     * 
     * @param dbName
     *            The database name
     */
    public SaveOrUpdateTask(String dbName) {
        this.dbName = dbName;
        marshalledObjects = new ArrayList<String>();
    }

    /**
     * Adds a marshalled object to the list
     * 
     * @param marshalledObject
     *            The marshalled object xml
     */
    public void addObject(String marshalledObject) {
        marshalledObjects.add(marshalledObject);
    }

    public Object execute() throws Exception {

        CoreDao dao = new CoreDao(DaoConfig.forDatabase(dbName));
        PersistableDataObject currentObject = null;
        for (String xml : marshalledObjects) {
            xml = xml.replaceAll("<quote>", "\"");
            currentObject = (PersistableDataObject) SerializationUtil
                    .unmarshalFromXml(xml);
            dao.saveOrUpdate(currentObject);
        }
        return marshalledObjects.size();
    }

}
