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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.grib;

/**
 * Simple POJO used to keep track of the properties mapped to a HPE Grib File
 * Runner and associated configuration.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPEProcessGribFilesConfigLookupKey {

    public static final String MATCH_ANY = "*";

    private String processIdStartsWith;

    private String xmrgInputStartsWith;

    public HPEProcessGribFilesConfigLookupKey(final String processIdStartsWith,
            final String xmrgInputStartsWith) {
        this.processIdStartsWith = processIdStartsWith;
        this.xmrgInputStartsWith = xmrgInputStartsWith;
    }

    public boolean lookupKeyMatches(final String process,
            final String xmrgInputName) {
        return (process.startsWith(processIdStartsWith.trim())
                && (MATCH_ANY.equals(xmrgInputStartsWith)
                        || xmrgInputName.startsWith(xmrgInputStartsWith)));
    }

    public String getProcessIdStartsWith() {
        return processIdStartsWith;
    }

    public void setProcessIdStartsWith(String processIdStartsWith) {
        this.processIdStartsWith = processIdStartsWith;
    }

    public String getXmrgInputStartsWith() {
        return xmrgInputStartsWith;
    }

    public void setXmrgInputStartsWith(String xmrgInputStartsWith) {
        this.xmrgInputStartsWith = xmrgInputStartsWith;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((processIdStartsWith == null) ? 0
                : processIdStartsWith.hashCode());
        result = prime * result + ((xmrgInputStartsWith == null) ? 0
                : xmrgInputStartsWith.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HPEProcessGribFilesConfigLookupKey other = (HPEProcessGribFilesConfigLookupKey) obj;
        if (processIdStartsWith == null) {
            if (other.processIdStartsWith != null)
                return false;
        } else if (!processIdStartsWith.equals(other.processIdStartsWith))
            return false;
        if (xmrgInputStartsWith == null) {
            if (other.xmrgInputStartsWith != null)
                return false;
        } else if (!xmrgInputStartsWith.equals(other.xmrgInputStartsWith))
            return false;
        return true;
    }
}