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
package com.raytheon.uf.common.monitor.data;

/**
 * Class to encapsulate Guidance data information.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 1, 2018  6720       mpduff      Initial creation.
 *
 * </pre>
 *
 * @author mduff
 */

public class GuidanceData {
    private String sourceName;

    private String displayName;

    private String sourceType;

    private String guidanceType;

    private String sourceFamily;

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getSourceType() {
        return sourceType;
    }

    public void setSourceType(String sourceType) {
        this.sourceType = sourceType;
    }

    public String getGuidanceType() {
        return guidanceType;
    }

    public void setGuidanceType(String guidanceType) {
        this.guidanceType = guidanceType;
    }

    public String getSourceFamily() {
        return sourceFamily;
    }

    public void setSourceFamily(String sourceFamily) {
        this.sourceFamily = sourceFamily;
    }

    @Override
    public String toString() {
        return "GuidanceData [sourceName=" + sourceName + ", displayName="
                + displayName + ", sourceType=" + sourceType + ", guidanceType="
                + guidanceType + ", sourceFamily=" + sourceFamily + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((displayName == null) ? 0 : displayName.hashCode());
        result = prime * result
                + ((guidanceType == null) ? 0 : guidanceType.hashCode());
        result = prime * result
                + ((sourceFamily == null) ? 0 : sourceFamily.hashCode());
        result = prime * result
                + ((sourceName == null) ? 0 : sourceName.hashCode());
        result = prime * result
                + ((sourceType == null) ? 0 : sourceType.hashCode());
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
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        GuidanceData other = (GuidanceData) obj;
        if (displayName == null) {
            if (other.displayName != null) {
                return false;
            }
        } else if (!displayName.equals(other.displayName)) {
            return false;
        }
        if (guidanceType == null) {
            if (other.guidanceType != null) {
                return false;
            }
        } else if (!guidanceType.equals(other.guidanceType)) {
            return false;
        }
        if (sourceFamily == null) {
            if (other.sourceFamily != null) {
                return false;
            }
        } else if (!sourceFamily.equals(other.sourceFamily)) {
            return false;
        }
        if (sourceName == null) {
            if (other.sourceName != null) {
                return false;
            }
        } else if (!sourceName.equals(other.sourceName)) {
            return false;
        }
        if (sourceType == null) {
            if (other.sourceType != null) {
                return false;
            }
        } else if (!sourceType.equals(other.sourceType)) {
            return false;
        }
        return true;
    }
}
