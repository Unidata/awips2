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
package com.raytheon.uf.edex.ogc.common.db;

import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.MappedSuperclass;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer.TimeFormat;

/**
 * Layer metadata storage for point data types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2011            bclement     Initial creation
 * 10/22/2013   2742       dhladky      @Entity made for Db dependency in AWIPS code, changed to @MappedSuperclass
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@MappedSuperclass
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class PointDataLayer extends
        SimpleLayer<DefaultPointDataDimension> {

    private static final long serialVersionUID = 4301480632118555546L;

    public PointDataLayer() {
	}

    public PointDataLayer(SimpleLayer<DefaultPointDataDimension> other) {
		super(other);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.ogc.common.db.SimpleLayer#getTimeEntries()
	 */
	@Override
	public List<String> getTimeEntries() {
		return LayerTransformer.getTimes(this, TimeFormat.HOUR_RANGES);
	}

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.SimpleLayer#getDimensions()
     */
    @Override
    public Set<DefaultPointDataDimension> getDimensions() {
        return new TreeSet<DefaultPointDataDimension>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.SimpleLayer#getDefaultTimeEntry()
     */
	@Override
	public String getDefaultTimeEntry() {
		return LayerTransformer.getTimeRange(getDefaultTime());
	}

    /**
     * Create formatted time range string with range start at the latest time
     * minus milliOffset and the range end at the latest time.
     * 
     * @param milliOffset
     * @return
     */
	protected <T extends PluginDataObject> String getRangeSinceLatest(
			long milliOffset) {
		Date end = getTimes().last();
		long startTime = end.getTime() - milliOffset;
		Date start = new Date(startTime);
		String startStr = LayerTransformer.format(start);
		String endStr = LayerTransformer.format(end);
		return startStr + "/" + endStr;
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.SimpleLayer#updateDates(com.raytheon
     * .uf.edex.ogc.common.db.SimpleLayer)
     */
    @Override
    public void updateDates(SimpleLayer<DefaultPointDataDimension> other) {
        SortedSet<Date> memTimes = other.getTimes();
        if (memTimes == null || memTimes.isEmpty()) {
            return;
        }
        SortedSet<Date> dbtimes = this.getTimes();
        SortedSet<Date> all = new TreeSet<Date>(memTimes);
        all.addAll(dbtimes);
        dbtimes.clear();
        dbtimes.add(all.first());
        dbtimes.add(all.last());
    }

}
