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
package com.raytheon.uf.edex.wfs.reg;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.db.SingleLayerCollector;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.reg.WfsQueryResults.ResultType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * Abstract implementation for sources backed by plugin data objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class PluginWfsSource extends
        AbstractWfsSource<PluginDataObject> {

    protected CoreDao _dao;

    protected PluginProperties props;

    protected Map<String, IPdoGmlTranslator> translatorMap;

    protected FeatureFactory featFactory;

    protected final SingleLayerCollector<?, SimpleLayer<?>, PluginDataObject> collector;

    /**
     * @param props
     * @param key
     *            unique key for this source
     * @param translators
     * @param featFactory
     */
    public PluginWfsSource(PluginProperties props, String key,
            List<IPdoGmlTranslator> translators, FeatureFactory featFactory,
            SingleLayerCollector<?, SimpleLayer<?>, PluginDataObject> collector) {
        super(key);
        this.props = props;
        this.translatorMap = new HashMap<String, IPdoGmlTranslator>(
                translators.size());
        for (IPdoGmlTranslator trans : translators) {
            this.translatorMap.put(trans.getVersion(), trans);
        }
        this.featFactory = featFactory;
        this.collector = collector;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.AbstractWfsSource#getDao()
     */
    @Override
    protected CoreDao getDao() throws PluginException {
        if (_dao == null) {
            _dao = PluginFactory.getInstance().getPluginDao(
                    props.getPluginName());
        }
        return _dao;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.WfsSource#query(com.raytheon.uf.edex.wfs
     * .request.QualifiedName, com.raytheon.uf.edex.wfs.reg.WfsQuery,
     * com.raytheon.uf.edex.wfs.reg.WfsQueryOptions)
     */
    @Override
    public WfsQueryResults query(QualifiedName feature, WfsQuery query,
            WfsQueryOptions options) throws WfsException {
        WfsQueryResults rval;
        switch (options.getType()) {
        case JAXB:
            rval = jaxbQuery(feature, query, options);
            break;
        case SIMPLE:
            rval = querySimple(feature, query, options);
            break;
        default:
            statusHandler.error("Unsupported query results type: "
                    + options.getType());
            throw new WfsException(Code.OperationProcessingFailed);
        }
        return rval;
    }

    /**
     * @param feature
     * @param query
     * @param options
     * @return
     * @throws WfsException
     */
    protected WfsQueryResults jaxbQuery(QualifiedName feature, WfsQuery query,
            WfsQueryOptions options) throws WfsException {
        String gmlVersion = options.getGmlVersion();
        IPdoGmlTranslator translator = translatorMap.get(gmlVersion);
        if (translator == null) {
            String msg = String.format(
                    "Feature type '%s' does not support GML version %s",
                    feature.getName(), gmlVersion);
            throw new WfsException(Code.InvalidParameterValue, msg);
        }
        WfsQueryResults rval = new WfsQueryResults(ResultType.JAXB);
        List<PluginDataObject> pdos = queryInternal(feature, query);
        PluginDataObject[] arr = extractData(pdos, rval, options);
        try {
            ArrayList<JAXBElement<?>> results = translator.translate(arr);
            rval.setResults(results);
            return rval;
        } catch (Exception e) {
            statusHandler.error("Problem translating pdos to jaxb features", e);
            throw new WfsException(Code.OperationProcessingFailed);
        }
    }

    /**
     * Convert pdo list to array and populate query results with metadata
     * information
     * 
     * @param pdos
     * @param results
     * @return
     */
    protected PluginDataObject[] extractData(
            List<? extends PluginDataObject> pdos, WfsQueryResults results,
            WfsQueryOptions options) {
        if (!options.gatherMetadata || pdos.isEmpty()) {
            return pdos.toArray(new PluginDataObject[pdos.size()]);
        }
        PluginDataObject[] rval = new PluginDataObject[pdos.size()];
        long oldest = Long.MAX_VALUE;
        long latest = Long.MIN_VALUE;
        Iterator<? extends PluginDataObject> iter = pdos.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            PluginDataObject pdo = iter.next();
            long insert = pdo.getInsertTime().getTimeInMillis();
            oldest = Math.min(oldest, insert);
            latest = Math.max(latest, insert);
            rval[i] = pdo;
        }
        results.setLatestResult(new Date(latest));
        results.setOldestResult(new Date(oldest));
        return rval;
    }

    /**
     * @param feature
     * @param q
     * @param options
     * @return
     * @throws WfsException
     */
    public WfsQueryResults querySimple(QualifiedName feature, WfsQuery q,
            WfsQueryOptions options) throws WfsException {
        List<PluginDataObject> pdos = queryInternal(feature, q);
        WfsQueryResults rval = new WfsQueryResults(ResultType.SIMPLE);
        PluginDataObject[] arr = extractData(pdos, rval, options);
        rval.setResults(featFactory.convert(arr));
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.WfsSource#getFeatureIdField(com.raytheon
     * .uf.edex.wfs.request.QualifiedName)
     */
    @Override
    public String getFeatureIdField(QualifiedName feature) {
        return "dataURI";
    }

}
