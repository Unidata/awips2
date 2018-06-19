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
package com.raytheon.uf.edex.netcdf.description.field.date;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.DelegateDescription;

/**
 *
 * Base class to extract a {@link Date} from a {@link NetcdfFile}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Aug 25, 2015  4699     nabowle   Initial creation
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * Apr 07, 2016  5446     skorolev  Added pattern.
 * Apr 19, 2016  5450     nabowle   Add multi-date retrieval.
 * May 19, 2016  5584     nabowle   Updates for consolidation.
 *
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlSeeAlso({ FormattedDateValue.class, EpochOffsetDateValue.class })
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractDateValue extends DelegateDescription {

    public Date getDate(NetcdfFile file) throws InvalidDescriptionException {
        return getDate(file, 0);
    }

    public abstract Date getDate(NetcdfFile file, int index)
            throws InvalidDescriptionException;
}
