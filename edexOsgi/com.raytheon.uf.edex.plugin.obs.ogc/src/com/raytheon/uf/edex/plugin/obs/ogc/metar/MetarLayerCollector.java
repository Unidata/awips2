package com.raytheon.uf.edex.plugin.obs.ogc.metar;

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

import java.util.Calendar;
import java.util.Date;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.ogc.common.db.LayerCollector;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.vividsolutions.jts.geom.Envelope;
/**
 * 
 * Metar Layer Collector
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   753       dhladky      Modified from a class written by Brian Clements
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MetarLayerCollector extends LayerCollector {

	private MetarLayer layer = newLayer();

	private boolean truncate = true;

	private int roundCutoff = 45;
	
	private static final transient IUFStatusHandler statusHandler = UFStatus
	.getHandler(MetarLayerCollector.class);

	public MetarLayerCollector(LayerTransformer transformer) {
		super(transformer);
		
	}

	/**
	 * Adds these PDO's as foder for this layer
	 * 
	 * @param pdos
	 */
	public void add(PluginDataObject... pdos) {
		synchronized (layer) {
			
			SortedSet<Date> times = layer.getTimes();
			
			for (PluginDataObject pdo : pdos) {
				MetarRecord record = (MetarRecord) pdo;
				Calendar time = getTime(record);
				times.add(time.getTime());
				statusHandler.debug("Adding metar layer: " + record.toString());
			}
		}
	}

	/**
	 * Adds the new times to the layer
	 */
	public void updateDB() {
		synchronized (layer) {
			try {
				if (!layer.getTimes().isEmpty()) {
					updateLayer(layer);
					statusHandler.info("Updating "+layer.getName()+" # times: "+layer.getTimes().size());
					layer.getTimes().clear();
				}
			} catch (DataAccessLayerException e) {
				statusHandler.error("problem updating metar layer. "+e);
				e.printStackTrace();
			}
		}
	}

	private Calendar getTime(MetarRecord record) {
		Calendar timeObs = record.getTimeObs();
		if (truncate) {
			return truncateToHour(timeObs);
		} else {
			return roundToHour(timeObs, roundCutoff);
		}
	}

	private Set<Date> getTimes(Set<Date> dates) {
		Set<Date> rval = new TreeSet<Date>();
		for (Date d : dates) {
			if (truncate) {
				rval.add(truncateToHour(d));
			} else {
				rval.add(roundToHour(d, roundCutoff));
			}
		}
		return rval;
	}

	/**
	 * purges expired times from layer
	 * @param timesToKeep
	 */
	public void purgeExpired(Set<Date> timesToKeep) {
		synchronized (layer) {
			layer.times.clear();
			layer.times.retainAll(getTimes(timesToKeep));
			try {
				replaceTimes(layer);
			} catch (DataAccessLayerException e) {
				statusHandler.error("problem purging expired metars. "+e);
			}
		}
	}

	/**
	 * purge the layer
	 */
	public void purgeAll() {
		synchronized (layer) {
			layer.times.clear();
			try {
				clearLayers(MetarLayer.class);
			} catch (Exception e) {
				statusHandler.error("problem purging metar layers. "+e);
			}
		}
	}

	/**
	 * creates a new layer
	 */
	private MetarLayer newLayer() {
		
		MetarLayer layer = new MetarLayer();
		layer.setName("metar");
		ReferencedEnvelope env = new ReferencedEnvelope(-180, 180, -90, 90,
				MapUtil.LATLON_PROJECTION);
		layer.setCrs84Bounds(JTS.toGeometry((Envelope) env));
		layer.setTargetCrsCode("CRS:84");
		layer.setTargetMaxx(env.getMaxX());
		layer.setTargetMaxy(env.getMaxY());
		layer.setTargetMinx(env.getMinX());
		layer.setTargetMiny(env.getMinY());
		layer.setTimes(new TreeSet<Date>());
		
		return layer;
	}
}
