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
 * 
*
* SOFTWARE HISTORY
*
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* Mar 29, 2012            bclement     Initial creation
* Aug 18, 2013  #2097     dhladky      renamed for standards
*
*/ 
package com.raytheon.uf.edex.wms.styling;

import java.awt.image.BufferedImage;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.WmsImage;


/**
 *
 * @author bclement
 * @version 1.0	
 */
public interface ICoverageStyleProvider extends StyleLookup {

	public WmsImage styleData(IWmsDataRetriever retriever, WmsStyleChoice style,
			PluginDataObject record, ReferencedEnvelope envelope)
			throws WmsException;
	
    public WmsStyleChoice getStyle(String layer, PluginDataObject record,
            String style) throws WmsException;

    public BufferedImage getLegend(String layer, PluginDataObject record,
            String style, Integer width, Integer height) throws WmsException;

}
