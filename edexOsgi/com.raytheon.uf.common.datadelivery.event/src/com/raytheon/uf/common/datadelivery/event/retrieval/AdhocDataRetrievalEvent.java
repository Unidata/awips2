package com.raytheon.uf.common.datadelivery.event.retrieval;

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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * AdhocDataRetrievalEvent.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY  
 * 
 * Date         Ticket#    Engineer    Description  
 * ------------ ---------- ----------- -------------------------- 
 * Aug 02, 2013 1654       bgonzale     Initial implementation.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@DynamicSerialize
public class AdhocDataRetrievalEvent extends DataRetrievalEvent {

    private static final long serialVersionUID = 4929206326118854334L;

    @Override
    public String toString() {
        return "Adhoc" + super.toString();
    }
}