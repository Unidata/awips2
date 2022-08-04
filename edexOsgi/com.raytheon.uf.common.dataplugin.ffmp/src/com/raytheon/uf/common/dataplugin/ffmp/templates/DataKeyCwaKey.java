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
package com.raytheon.uf.common.dataplugin.ffmp.templates;

/**
 * A key for accessing data from the TemplateData maps.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2018 6641       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public class DataKeyCwaKey {

    protected final String dataKey;

    protected final String cwa;

    public DataKeyCwaKey(String dataKey, String cwa) {
        this.dataKey = dataKey;
        this.cwa = cwa;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((cwa == null) ? 0 : cwa.hashCode());
        result = prime * result + ((dataKey == null) ? 0 : dataKey.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        DataKeyCwaKey other = (DataKeyCwaKey) obj;
        if (cwa == null) {
            if (other.cwa != null) {
                return false;
            }
        } else if (!cwa.equals(other.cwa)) {
            return false;
        }
        if (dataKey == null) {
            if (other.dataKey != null) {
                return false;
            }
        } else if (!dataKey.equals(other.dataKey)) {
            return false;
        }
        return true;
    }

    public String getDataKey() {
        return dataKey;
    }

    public String getCwa() {
        return cwa;
    }

    @Override
    public String toString() {
        return "DataKeyCwaKey [dataKey=" + dataKey + ", cwa=" + cwa + "]";
    }

}
