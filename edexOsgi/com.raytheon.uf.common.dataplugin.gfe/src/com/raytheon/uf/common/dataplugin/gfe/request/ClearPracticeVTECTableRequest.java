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

package com.raytheon.uf.common.dataplugin.gfe.request;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class ClearPracticeVTECTableRequest extends AbstractGfeRequest {
//	public class GetActiveTableRequest extends AbstractGfeRequest {

	    @DynamicSerializeElement
	    private String requestedSiteId;

	    @DynamicSerializeElement
	    private ActiveTableMode mode;

	    /**
	     * @return the requestedSiteId
	     */
	    public String getRequestedSiteId() {
	        return requestedSiteId;
	    }

	    /**
	     * @param requestedSiteId
	     *            the requestedSiteId to set
	     */
	    public void setRequestedSiteId(String requestedSiteId) {
	        this.requestedSiteId = requestedSiteId;
	    }

	    public ActiveTableMode getMode() {
	        return mode;
	    }

	    public void setMode(ActiveTableMode mode) {
	        this.mode = mode;
	    }

	}
