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
/**
 * 
 */
package com.raytheon.uf.common.dataplugin.persist;

/**
 * Defines an interface to the base class {@link PersistableDataObject} that
 * represents objects that Hibernate may persist into the database.
 * <P>
 * Provides a read only interface for the Persistable Data Object classes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20Aug2007      393       MW Fegan      Initial creation    
 * Oct 10, 2012 1261        djohnson     Add generic for identifier.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 * 
 */
public interface IPersistableDataObject<IDENTIFIER_TYPE> {
    /**
     * Returns an object representing the identifier.
     *  
     * @return the identifier
     */
    IDENTIFIER_TYPE getIdentifier();
}
