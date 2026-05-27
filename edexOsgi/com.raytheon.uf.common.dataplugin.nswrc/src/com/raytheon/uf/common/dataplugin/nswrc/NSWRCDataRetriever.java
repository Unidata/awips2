/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.common.dataplugin.nswrc;

import java.io.FileNotFoundException;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Handles returning stored populated NSWRC data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 13, 2014            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */

public class NSWRCDataRetriever {

    public static void populateRadarRecord(IDataStore dataStore,
            NSWRCRadialRecord radarRecord) throws StorageException,
            FileNotFoundException {
        getRadarStoredData(dataStore, radarRecord.getDataURI(), radarRecord);
    }

    public static int getRadarStoredData(IDataStore dataStore, String dataURI,
            NSWRCRadialRecord radarRecord) throws FileNotFoundException,
            StorageException {
        int size = 0;
        for (IDataRecord record : dataStore.retrieve(dataURI)) {
            if (record == null || record.getName() == null) {
                continue;
            }
            size += record.getSizeInBytes();
            if (record.getName().equals("Data")) {
                FloatDataRecord floatData = (FloatDataRecord) record;
                radarRecord.setData(floatData.getFloatData());
            } else if (record.getName().equals("AngleData")) {
                FloatDataRecord floatData = (FloatDataRecord) record;
                radarRecord.setAngleData(floatData.getFloatData());
            } else if (record.getName().equals(NSWRCConstants.NOISE_TITLE)) {
                FloatDataRecord floatData = (FloatDataRecord) record;
                radarRecord.setSignal_to_noise(floatData.getFloatData());
            } else if (record.getName().equals(NSWRCConstants.POWER_TITLE)) {
                FloatDataRecord floatData = (FloatDataRecord) record;
                radarRecord.setNormalized_coherent_power(floatData
                        .getFloatData());
            } else {
                size -= record.getSizeInBytes();
            }
        }

        return size;
    }

}
