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

import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.xml.bind.JAXBException;
import javax.xml.parsers.ParserConfigurationException;

import net.opengis.gml.v_3_1_1.ObjectFactory;

import org.apache.commons.lang.ArrayUtils;
import org.w3c.dom.Node;

import com.raytheon.uf.common.serialization.MarshalOptions;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.ogc.common.jaxb.OgcJaxbManager;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;
import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * Wfs registry implementation. Handles wfs sources and the JAXB context
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2011            bclement     Initial creation        
 * May 30, 2013   753      dhladky      reverted to original
 * Jul 15, 2014 3373       bclement     jaxb manager api changes
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsRegistryImpl implements IWfsRegistry {

    protected final Map<String, IWfsSource> byKey = new HashMap<String, IWfsSource>();

	protected final Map<QualifiedName, IWfsSource> byFeature = new HashMap<QualifiedName, IWfsSource>();

	protected final Map<QualifiedName, IWfsSource> byAlias = new HashMap<QualifiedName, IWfsSource>();
	
	protected Class<?>[] jaxbClasses = new Class<?>[] { ObjectFactory.class,
			net.opengis.wfs.v_1_1_0.ObjectFactory.class,
            net.opengis.filter.v_1_1_0.ObjectFactory.class,
            org.w3.xmlschema.ObjectFactory.class, Unique.class,
            net.opengis.wfs.v_2_0_0.ObjectFactory.class,
            net.opengis.filter.v_2_0_0.ObjectFactory.class,
            net.opengis.gml.v_3_2_1.ObjectFactory.class,
            com.eurocontrol.avwx.v_1_1_1.ObjectFactory.class,
            com.eurocontrol.wx.v_1_1_1.ObjectFactory.class,
            net.opengis.sensorml.v_1_0_1_gml32.ObjectFactory.class,
            net.oasis.wsn.b2.ObjectFactory.class,
            net.oasis.wsn.bf2.ObjectFactory.class,
            net.oasis.wsn.br2.ObjectFactory.class,
            net.oasis.wsn.t1.ObjectFactory.class,
            net.opengis.owsnt.v1.ObjectFactory.class,
            org.w3c.ws_addressing.ObjectFactory.class };

    protected volatile OgcJaxbManager jaxbManager;

	protected final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    protected volatile long currentVersion = 0;

	protected volatile long jaxbContextVersion = 0;

	protected String prefix = "wfs";

    protected final ReadWriteLock lock = new ReentrantReadWriteLock();

    public static final Map<String, String> NS_MAP = new ConcurrentHashMap<String, String>();

    protected String port;

    protected String path;

	static {
		NS_MAP.put(OgcNamespace.EDEX, OgcPrefix.EDEX);
		NS_MAP.put(OgcNamespace.GML, OgcPrefix.GML);
		NS_MAP.put(OgcNamespace.OGC, OgcPrefix.OGC);
		NS_MAP.put(OgcNamespace.OWS, OgcPrefix.OWS);
		NS_MAP.put(OgcNamespace.WFS, OgcPrefix.WFS);
		NS_MAP.put(OgcNamespace.XSI, OgcPrefix.XSI);
		NS_MAP.put(OgcNamespace.XLINK, OgcPrefix.XLINK);
        NS_MAP.put(OgcNamespace.SMIL, OgcPrefix.SMIL);
        NS_MAP.put(OgcNamespace.OWS110, OgcPrefix.OWS + "11");
        NS_MAP.put(OgcNamespace.WFS20, OgcPrefix.WFS + "2");
        NS_MAP.put(OgcNamespace.FES20, OgcPrefix.FES);
        NS_MAP.put(OgcNamespace.GML32, OgcPrefix.GML + "32");
        NS_MAP.put(OgcNamespace.SWE_GML32, OgcPrefix.SWE);
        NS_MAP.put(OgcNamespace.AVWX11, OgcPrefix.AVWX);
        NS_MAP.put(OgcNamespace.WX, OgcPrefix.WX);
        NS_MAP.put(OgcNamespace.OM_GML32, OgcPrefix.OM);
        NS_MAP.put(OgcNamespace.SML_GML32, OgcPrefix.SensorML);
        NS_MAP.put(OgcNamespace.ISM, OgcPrefix.ISM);
        NS_MAP.put(OgcNamespace.WSNT, OgcPrefix.WSNT);
        NS_MAP.put(OgcNamespace.WSA, OgcPrefix.WSA);
        NS_MAP.put(OgcNamespace.OWSNT, OgcPrefix.OWSNT);
        NS_MAP.put(OgcNamespace.NAWX15, OgcPrefix.NAWX);
	}

    public WfsRegistryImpl(String port, String path) {
        this.port = port;
        this.path = path;
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.WfsRegistry#register(com.raytheon.uf.edex
     * .wfs.reg.WfsSource)
     */
	@Override
    public IWfsRegistry register(final IWfsSource source)
			throws RegistryException {
        Lock write = lock.writeLock();
        write.lock();
        try {
            addByKey(source);
            addByFeature(source);
            addByAlias(source);
            jaxbClasses = (Class<?>[]) ArrayUtils.addAll(jaxbClasses,
                    source.getJaxbClasses());
            currentVersion++;
        } finally {
            write.unlock();
        }
		return this;
	}

    /**
     * @return
     * @throws JAXBException
     */
    public OgcJaxbManager getManager() throws JAXBException {
        if (jaxbManager == null || jaxbContextVersion < currentVersion) {
            Lock write = lock.writeLock();
            write.lock();
            try {
                jaxbContextVersion = currentVersion;
                NamespacePrefixMapper mapper = new NamespacePrefixMapper() {
                    @Override
                    public String getPreferredPrefix(String uri,
                            String suggestion, boolean requirePrefix) {
                        return NS_MAP.get(uri);
                    }
                };
                jaxbManager = new OgcJaxbManager(mapper, jaxbClasses);
            } finally {
                write.unlock();
            }
        }
		return jaxbManager;
	}

    /**
     * @param xml
     * @return
     * @throws JAXBException
     */
	public Object unmarshal(String xml) throws JAXBException {
        return getManager().unmarshalFromXml(xml);
	}

    public Object unmarshal(Node node) throws JAXBException {
        return getManager().unmarshal(node);
    }

    /**
     * @param in
     * @return
     * @throws JAXBException
     * @throws SerializationException
     */
    public Object unmarshal(InputStream in) throws JAXBException,
            SerializationException {
        return getManager().unmarshalFromInputStream(in);
    }

    /**
     * @param obj
     * @return
     * @throws JAXBException
     */
	public String marshal(Object obj) throws JAXBException {
        return getManager().marshalToXml(obj, MarshalOptions.UNFORMATTED);
	}

    public String marshal(Object obj, boolean fragment) throws JAXBException {
        MarshalOptions options = new MarshalOptions(false, fragment);
        return getManager().marshalToXml(obj, options);
    }

    public Node marshalToNode(Object obj) throws JAXBException,
            ParserConfigurationException {
        return getManager().marshalToNode(obj);
    }

    public String marshal(Object obj, MarshalOptions options) throws JAXBException {
        return getManager().marshalToXml(obj, options);
    }

    /**
     * @param obj
     * @param out
     * @throws JAXBException
     * @throws SerializationException
     */
    public void marshal(Object obj, OutputStream out) throws JAXBException,
            SerializationException {
        getManager().marshalToStream(obj, out, MarshalOptions.UNFORMATTED);
    }

    /**
     * Catalog source by key
     * 
     * @param source
     * @throws RegistryException
     */
	protected void addByKey(IWfsSource source) throws RegistryException {
		String key = source.getKey();
		if (byKey.containsKey(key)) {
			throw new RegistryException("WFS Source already exists with key: "
					+ key);
		}
		byKey.put(source.getKey(), source);
	}

    /**
     * Catalog source by provided feature types
     * 
     * @param source
     * @throws RegistryException
     */
	protected void addByFeature(IWfsSource source) throws RegistryException {
		for (WfsFeatureType f : source.listFeatureTypes()) {
			QualifiedName feature = f.getName();
			if (byFeature.containsKey(feature)) {
				throw new RegistryException(
						"Already providing a feature with name: " + feature);
			}
            String prefix = NS_MAP.get(feature.getNamespace());
            if (prefix == null) {
                prefix = (feature.getPrefix() != null
                        && !feature.getPrefix().isEmpty() ? feature.getPrefix()
                        : feature.getName());
                NS_MAP.put(feature.getNamespace(), prefix);
            }

			byFeature.put(f.getName(), source);
		}
	}
	
	protected void addByAlias(IWfsSource source) throws RegistryException {
        for (WfsFeatureType f : source.getAliases()) {
            QualifiedName feature = f.getName();
            if (byAlias.containsKey(feature)) {
                throw new RegistryException(
                        "Already providing a feature with name: " + feature);
            }
            String prefix = NS_MAP.get(feature.getNamespace());
            if (prefix == null) {
                prefix = (feature.getPrefix() != null
                        && !feature.getPrefix().isEmpty() ? feature.getPrefix()
                        : feature.getName());
                NS_MAP.put(feature.getNamespace(), prefix);
            }

            byAlias.put(f.getName(), source);
        }
    }

    /**
     * Remove source from all maps
     * 
     * @param source
     */
	protected void removeAll(IWfsSource source) {
		IWfsSource removed = byKey.remove(source.getKey());
		if (removed != null) {
			for (WfsFeatureType f : removed.listFeatureTypes()) {
				byFeature.remove(f.getName());
			}
			for (WfsFeatureType f : removed.getAliases()) {
                byAlias.remove(f.getName());
            }
		}
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.reg.WfsRegistry#unregister(com.raytheon.uf.edex
     * .wfs.reg.WfsSource)
     */
	@Override
	public synchronized IWfsRegistry unregister(IWfsSource source) {
        Lock write = lock.writeLock();
        write.lock();
        try {
            removeAll(source);
        } finally {
            write.unlock();
        }
        return this;
	}

    /**
     * Look up source by feature type
     * 
     * @param feature
     * @return
     * @throws WfsException
     */
	public IWfsSource getSource(QualifiedName feature) throws WfsException {
        Lock read = lock.readLock();
        read.lock();
        try {
            IWfsSource source = byFeature.get(feature);
            if(source == null) {
                //If a standard feature is not found see if it is an alias
                source = byAlias.get(feature);
            }
            return source;
        } finally {
            read.unlock();
        }
	}

    /**
     * Get a list of all available feature types, no aliases should be advertised
     * 
     * @return
     */
	public List<WfsFeatureType> getFeatures() {
		List<WfsFeatureType> rval = new LinkedList<WfsFeatureType>();
        Lock read = lock.readLock();
        read.lock();
        try {
            for (IWfsSource source : byKey.values()) {
                rval.addAll(source.listFeatureTypes());
            }
        } finally {
            read.unlock();
        }
        return rval;
	}

	/**
	 * @return the prefix
	 */
	public String getPrefix() {
		return prefix;
	}

	/**
	 * @param prefix
	 *            the prefix to set
	 */
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.WfsRegistry#getHttpServicePort()
     */
    @Override
    public String getHttpServicePort() {
        return port;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wfs.reg.WfsRegistry#getHttpServicePath()
     */
    @Override
    public String getHttpServicePath() {
        return path;
    }

}
