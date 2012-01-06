package com.raytheon.uf.common.dataplugin.grib;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2011            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public class CombinedGribRecord extends GribRecord {

    private static final long serialVersionUID = 1L;

    GribRecord secondaryGribRecord;

    GribRecord primaryGribRecord;

    /**
     * Default Constructor
     * 
     * @param recordToCopy
     * @param secondaryGribRecord
     */
    public CombinedGribRecord(GribRecord recordToCopy,
            GribRecord secondaryGribRecord) {
        super(recordToCopy);
        this.primaryGribRecord = recordToCopy;
        this.secondaryGribRecord = secondaryGribRecord;
    }

    public GribRecord getPrimaryGribRecord() {
        return primaryGribRecord;
    }

    public GribRecord getSecondaryGribRecord() {
        return secondaryGribRecord;
    }

}
