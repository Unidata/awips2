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
package com.raytheon.uf.edex.plugin.goesr.decoder.lookup;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrDecoderException;

/**
 * An {@link AttributeValue} that includes a dateFormat so that dates can be
 * parsed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DateAttributeValue extends AttributeValue {

    @XmlAttribute
    private String dateFormat;

    private transient SimpleDateFormat sdf;

    public Date getDate(NetcdfFile cdfFile) throws GoesrDecoderException {
        String val = getValue(cdfFile, null);
        if (val == null || dateFormat == null) {
            return null;
        }
        if (sdf == null) {
            sdf = new SimpleDateFormat(dateFormat);
        }
        try {
            return sdf.parse(val);
        } catch (ParseException e) {
            throw new GoesrDecoderException("Unable to parse [" + val
                    + "] + using format [" + dateFormat + "]", e);
        }
    }
}
