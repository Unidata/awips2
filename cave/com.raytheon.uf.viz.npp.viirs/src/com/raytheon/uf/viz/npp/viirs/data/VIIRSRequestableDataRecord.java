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
package com.raytheon.uf.viz.npp.viirs.data;

import java.util.Collection;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2012            mschenke     Initial creation
 * Apr 8, 2013  1293       bkowal       Removed references to hdffileid.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSRequestableDataRecord extends VIIRSDataRecord {

    private static final long serialVersionUID = 482141143796775377L;

    private AbstractRequestableData requestableData;

    public VIIRSRequestableDataRecord(AbstractRequestableData requestableData,
            Collection<VIIRSDataRecord> baseRecords) {
        int i = 0;
        Integer lastChannel = null;
        boolean channelsSame = true;
        Double lastWavelength = null;
        boolean wavelengthsSame = true;
        String lastChannelType = null;
        boolean channelTypeSame = true;
        // We will build ourself from our base records, anything that isn't
        // consistent among all records is not set
        for (VIIRSDataRecord record : baseRecords) {
            if (i == 0) {
                // Set common things from first record
                setRegion(record.getRegion());
                setDataTime(record.getDataTime());
                setLevels(record.getLevels());
                setCoverage(record.getCoverage());

                // Set potentially varying items
                lastChannelType = record.getChannelType();
                lastChannel = record.getChannel();
                lastWavelength = record.getWavelength();
            } else {
                if (channelTypeSame
                        && equals(lastChannelType, record.getChannelType()) == false) {
                    channelTypeSame = false;
                }
                if (channelsSame
                        && equals(lastChannel, record.getChannel()) == false) {
                    channelsSame = false;
                }
                if (wavelengthsSame
                        && equals(lastWavelength, record.getWavelength()) == false) {
                    wavelengthsSame = false;
                }
                if (record.getLevels() < getLevels()) {
                    // We want minimum levels of all base records
                    setLevels(record.getLevels());
                    setCoverage(record.getCoverage());
                }
            }
            ++i;
        }
        if (channelTypeSame) {
            setChannelType(lastChannelType);
        }
        if (channelsSame) {
            setChannel(lastChannel);
        }
        if (wavelengthsSame) {
            setWavelength(lastWavelength);
        }
        setParameter(requestableData.getParameter());
        try {
            constructDataURI();
        } catch (PluginException e) {
            throw new RuntimeException("Error constructing dataURI: "
                    + e.getLocalizedMessage(), e);
        }
        this.requestableData = requestableData;
    }

    public AbstractRequestableData getRequestableData() {
        return requestableData;
    }

    private static boolean equals(Object o1, Object o2) {
        return (o1 == o2) || (o1 != null && o1.equals(o2));
    }
}
