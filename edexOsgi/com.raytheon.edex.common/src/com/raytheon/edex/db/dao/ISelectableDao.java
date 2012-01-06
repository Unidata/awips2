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


package com.raytheon.edex.db.dao;

import java.util.List;

/**
 * Specifies methods providing a {@code SELECT * FROM table t} style queries.
 * Classes implementing this interface are able to set the {@code Class} to
 * query and perform a {@code SELECT *} style query. Classes implementing this
 * interface may chose to set the DAO class via an initializing constructor. 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18Sep2007    393        MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */
public interface ISelectableDao {
    /**
     * Sets the object associated with this DAO. This
     * version uses the {@code Class} of the object.
     * 
     * @param daoClass the class of the object.
     */
    public void setDaoClass(Class<?> daoClass);
    /**
     * Sets the object associated with this DAO. This
     * version uses the fully qualified class name of 
     * the object.
     *  
     * @param fqn the fully qualified class name of the object
     */
    public void setDaoClass(String fqn);
    /**
     * Performs the {@code SELECT *} query. The result set is
     * returned as a {@code List<?>} of objects of the type
     * set by one of the {@code setDaoClass(...)} methods.
     * 
     * @return the result set
     * 
     * @see #setDaoClass(Class)
     * @see #setDaoClass(String)
     */
    public List<?> selectAll();
    /**
     * Performs the {@code SELECT *} query, limiting the number of
     * objects returned. The result set is returned as a {@code List<?>}
     * of objects of the type set using one of the {@code setDaoClass(...)}
     * methods.
     * 
     * @param count the maximum number of objects to return.
     * 
     * @return the result set
     * 
     * @see #setDaoClass(Class)
     * @see #setDaoClass(String)
     */
    public List<?> selectAll(Integer count);
    /**
     * Performs the basic {@code SELECT ... WHERE ...} using a partially
     * populated object to specify the WHERE clause. The result set is
     * returned as a {@code List<?>} of objects of the type set using one
     * of the {@code setDaoClass(...)} methods.
     * 
     * @param object the object representing the WHERE clause
     * 
     * @return the result set
     * 
     * @see #setDaoClass(Class)
     * @see #setDaoClass(String)
     */
    public List<?> selectByExample(Object object);
}
