package com.raytheon.uf.edex.datadelivery.retrieval.interfaces;

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
 * further licensing information
 * */

import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;

/**
 * Interface for Provider Retrieval Request Builders
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 16, 2011            dhladky     Initial creation
 * Aug 14, 2012 1022       djohnson    Remove redundant public modifiers.
 * 
 * </pre>
 * 
 * /
 * 
 * @author dhladky
 * @version 1.0
 */
public interface IRetrievalRequestBuilder {

    String processTime(Time prtXML);

    String processCoverage();

    String getRequest();

    RetrievalAttribute getAttribute();

}
