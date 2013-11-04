/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.edex.plugin.dataset.urn.CFNameLookup;
import com.raytheon.uf.edex.wcs.reg.IFieldAdapted;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridFieldAdapter implements IFieldAdapted<GridRecord> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wcs.reg.FieldAdapted#getCoverageField(java.lang.
     * Object)
     */
    @Override
    public String getCoverageField(GridRecord record) {
		CFNameLookup lookup = CFNameLookup.getInstance();
		String abbr = record.getInfo().getParameter().getAbbreviation();
		return lookup.getCFFromNCEP(abbr);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wcs.reg.FieldAdapted#getSupportedClass()
     */
    @Override
    public Class<GridRecord> getSupportedClass() {
        return GridRecord.class;
    }

}
