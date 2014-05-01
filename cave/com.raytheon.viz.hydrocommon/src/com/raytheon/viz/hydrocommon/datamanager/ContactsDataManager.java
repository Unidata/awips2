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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.ContactsData;

/**
 * This class is the data manager for the contacts data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20 Nov 2008             lvenable    Initial creation
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ContactsDataManager extends HydroDataManager 
{
    /**
     * Instance of this class.
     */
    private static ContactsDataManager manager = null;
    
    /**
     * Select statement.
     */
    private final String SELECT_STATEMENT = "SELECT * FROM contacts";
    
    /**
     * Insert statement.
     */
    private final String INSERT_STATEMENT = "INSERT INTO contacts (lid, contact, phone, email, remark, priority) VALUES ('%s', '%s', '%s', '%s', '%s', %d)";
    
    /**
     * Delete statement.
     */
    private final String DELETE_STATEMENT = "DELETE FROM contacts";
    
    /**
     * Update statement.
     */
    private final String UPDATE_STATEMENT = "UPDATE contacts SET contact='%s', phone='%s', email='%s', remark='%s', priority=%d WHERE lid='%s' AND contact='%s'";
    
    /**
     * Constructor.
     */
    public ContactsDataManager()
    {
    }
    
    /**
     * Get an instance of this class.
     * @return An instance of this class.
     */
    public static synchronized ContactsDataManager getInstance() {
        if (manager == null) {
            manager = new ContactsDataManager();
        }

        return manager;
    }
    
    /**
     * Get the list of contacts from the specified location ID.
     * @param lid Location ID.
     * @return ArrayList of ContactsData.
     * @throws VizException Database exception.
     */
    public ArrayList<ContactsData> getContactData(String lid) throws VizException 
    {
        ArrayList<ContactsData> rval = new ArrayList<ContactsData>();
        
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "' ORDER BY priority ASC ");

        for (QueryResultRow currRow : result.getRows()) {
            rval.add(new ContactsData(currRow, result.getColumnNames()));
        }
        
        return rval;
    }
    
    /**
     * Delete the record from the database the specified location ID and Contact.
     * @param contactsData Contacts data.
     * @throws VizException Database exception.
     */
    public void deleteRecord(ContactsData contactsData) throws VizException
    {
        StringBuilder query = new StringBuilder(DELETE_STATEMENT);
        String whereClaus = String.format(" WHERE lid = '%s' AND contact = '%s' ", 
                contactsData.getLid(), contactsData.getContact());
        query.append(whereClaus);
        
        runStatement(query.toString());
    }
    
    /**
     * Check if a specific record exists in the contacts table.
     * @param lid Location ID.
     * @param contactName Contact name.
     * @return True if the record exists, false otherwise.
     * @throws VizException Database exception.
     */
    public boolean recordExists(String lid, String contactName) throws VizException
    {
        StringBuilder query = new StringBuilder(SELECT_STATEMENT);
        String whereClaus = String.format(" WHERE lid = '%s' AND contact = '%s' ", 
                lid, contactName);
        
        query.append(whereClaus);
        
        QueryResult result = runMappedQuery(query.toString());
        
        if (result.getResultCount() == 0)
        {
            return false;
        }
        
        return true;
    }
    
    /**
     * Insert the new contact data into the database.
     * @param data Contacts data.
     * @throws VizException Database exception.
     */
    public void insertContactData(ContactsData data) throws VizException
    {
        String query = String.format(INSERT_STATEMENT, data.getLid(),
                data.getContact(), data.getPhone(), data.getEmail(),
                data.getRemark(), data.getPriority());
        
        runStatement(query);
    }
    
    /**
     * Update an existing record in the contacts table with the new information.
     * @param data Contacts data.
     * @throws VizException Database exception.
     */
    public void updateContactData(ContactsData data,
            String originalContactName) throws VizException
    {        
        String query = String.format(UPDATE_STATEMENT, data.getContact(),
                data.getPhone(), data.getEmail(), data.getRemark(),
                data.getPriority(), data.getLid(), originalContactName);
        
        runStatement(query);
    }
}
