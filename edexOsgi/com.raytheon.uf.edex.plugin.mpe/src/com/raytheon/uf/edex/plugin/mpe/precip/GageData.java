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
package com.raytheon.uf.edex.plugin.mpe.precip;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

/**
 * POJO for storage of precip data retrieved from the pchourly, pphourly, and
 * pseudogageval tables. The C/C++ code previously used a collection of arrays
 * loosely linked by array index.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2016 5631       bkowal      Initial creation
 * Sep 27, 2016 5631       bkowal      Added gage counting and gage retrieval methods.
 *
 * </pre>
 *
 * @author bkowal
 */

public class GageData {

    private final Calendar dateTime;

    /*
     * PE Precip lists exist to: store all the pseudo gages which are outside
     * the HRAP area (ABRFC requirement). This is just for the p3lmosaic
     * routine. All other mosaics use gages inside the HRAP area.
     */

    private List<PrecipDataRecord> pseudoPrecip = new ArrayList<>(1);

    private List<PrecipDataRecord> pseudoPrecipP3 = new ArrayList<>(1);

    private List<PrecipDataRecord> gagePrecip = new ArrayList<>(1);

    private List<PrecipDataRecord> gagePrecipP3 = new ArrayList<>(1);

    public GageData(final Calendar dateTime) {
        if (dateTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'dateTime' cannot be NULL.");
        }
        this.dateTime = dateTime;
    }

    public void addPseudoPrecip(final PrecipDataRecord precipDataRecord) {
        pseudoPrecip.add(precipDataRecord);
    }

    public void addPseudoPrecipP3(final PrecipDataRecord precipDataRecord) {
        pseudoPrecipP3.add(precipDataRecord);
    }

    public void addGagePrecip(final PrecipDataRecord precipDataRecord) {
        gagePrecip.add(precipDataRecord);
    }

    public void addGagePrecipP3(final PrecipDataRecord precipDataRecord) {
        gagePrecipP3.add(precipDataRecord);
    }

    public int getTotalGageCount() {
        return (pseudoPrecip.size() + gagePrecip.size());
    }

    public int getTotalGageP3Count() {
        return (pseudoPrecipP3.size() + gagePrecipP3.size());
    }

    public List<PrecipDataRecord> getGagePrecipRecords() {
        if (pseudoPrecip.isEmpty() && gagePrecip.isEmpty()) {
            return Collections.emptyList();
        }
        List<PrecipDataRecord> precip = new ArrayList<>(
                pseudoPrecip.size() + gagePrecip.size());
        precip.addAll(pseudoPrecip);
        precip.addAll(gagePrecip);
        return precip;
    }

    public Calendar getDateTime() {
        return dateTime;
    }

    public List<PrecipDataRecord> getPseudoPrecip() {
        return pseudoPrecip;
    }

    public void setPseudoPrecip(List<PrecipDataRecord> pseudoPrecip) {
        this.pseudoPrecip = pseudoPrecip;
    }

    public List<PrecipDataRecord> getPseudoPrecipP3() {
        return pseudoPrecipP3;
    }

    public void setPseudoPrecipP3(List<PrecipDataRecord> pseudoPrecipP3) {
        this.pseudoPrecipP3 = pseudoPrecipP3;
    }

    public List<PrecipDataRecord> getGagePrecip() {
        return gagePrecip;
    }

    public void setGagePrecip(List<PrecipDataRecord> gagePrecip) {
        this.gagePrecip = gagePrecip;
    }

    public List<PrecipDataRecord> getGagePrecipP3() {
        return gagePrecipP3;
    }

    public void setGagePrecipP3(List<PrecipDataRecord> gagePrecipP3) {
        this.gagePrecipP3 = gagePrecipP3;
    }
}