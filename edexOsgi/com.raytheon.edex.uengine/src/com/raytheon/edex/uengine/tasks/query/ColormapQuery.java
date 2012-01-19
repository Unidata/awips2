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

package com.raytheon.edex.uengine.tasks.query;

import java.util.Date;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;

/**
 * Queries for available colormaps.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class ColormapQuery extends ScriptTask {

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.edex.uengine.tasks.ScriptTask#execute()
	 */
	@Override
	public Object execute() {
		ResponseMessageCatalog response = new ResponseMessageCatalog();
		String[] colormaps = ColorMapManager.getInstance().listColorMaps();
		response.setValues(colormaps);
		response.setItems(new CatalogItem[] {});
		response.setFileType("");
		response.setDataURI("");
		response.setValidTime(new Date());

		return response;
	}

}
