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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import com.raytheon.uf.edex.plugin.mpe.XmrgDateNameFormat;
import com.raytheon.uf.edex.plugin.mpe.apps.ICustomValueConverter;
import com.raytheon.uf.edex.plugin.mpe.apps.ValueConverterException;

/**
 * {@link ICustomValueConverter} implementation to convert an Apps Defaults
 * property value to a {@link XmrgDateNameFormat}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgDateNameConverter implements
        ICustomValueConverter<XmrgDateNameFormat> {

    @Override
    public XmrgDateNameFormat convertValue(String value)
            throws ValueConverterException {
        return XmrgDateNameFormat.lookupDateFormatByText(value);
    }
}