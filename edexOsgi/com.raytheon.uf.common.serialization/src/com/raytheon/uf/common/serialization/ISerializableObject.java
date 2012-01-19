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
package com.raytheon.uf.common.serialization;

/**
 * Empty interface that should be implemented by any class that uses Hibernate,
 * JaxB, or DynamicSerialize annotations so it is detected at runtime.
 * 
 * Implementing this interface in conjunction with adding the class to the
 * com.raytheon.uf.common.serialization.ISerializableObject file in the
 * META-INF/services directory will ensure it is detected at runtime.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 11, 2008				njensen	    Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface ISerializableObject {

}
