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
package com.raytheon.uf.edex.ogc.common.feature;

import java.io.OutputStream;
import java.util.List;

import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.edex.ogc.common.OgcResponse;

/**
 * Interface for converting simple features to different output formats
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface SimpleFeatureFormatter {

    /**
     * Format features and return in response wrapper
     * 
     * @param features
     * @return
     * @throws Exception
     */
	public OgcResponse format(List<List<SimpleFeature>> features)
			throws Exception;

    /**
     * Format features and output to stream
     * 
     * @param features
     * @param out
     * @throws Exception
     */
    public void format(List<List<SimpleFeature>> features, OutputStream out)
            throws Exception;

    /**
     * @return mime type supported by this formatter
     */
    public MimeType getMimeType();

    /**
     * @param format
     * @return true if this formatter is suitable for format
     */
	public boolean matchesFormat(MimeType format);
}
