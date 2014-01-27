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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.List;

import com.raytheon.uf.edex.ogc.common.OgcException;


/**
 * Interface for retrieving layer information that may be in storage or cached
 * in memory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sept 11, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 * @param <D>
 * @param <L>
 */
public interface ILayerCache<D extends SimpleDimension, L extends SimpleLayer<D>> {

    /**
     * @return all layers
     * @throws OgcException
     */
    public List<L> getLayers() throws OgcException;

    /**
     * @param name
     * @return layer with matching name or null if none found
     * @throws OgcException
     */
	public L getLayer(String name) throws OgcException;

}
