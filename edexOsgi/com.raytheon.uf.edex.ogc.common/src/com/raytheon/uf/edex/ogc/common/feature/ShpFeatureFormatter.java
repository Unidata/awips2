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
package com.raytheon.uf.edex.ogc.common.feature;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.geotools.data.DefaultTransaction;
import org.geotools.data.FeatureSource;
import org.geotools.data.FeatureStore;
import org.geotools.data.Transaction;
import org.geotools.data.memory.MemoryFeatureCollection;
import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.data.shapefile.ShapefileDataStoreFactory;
import org.geotools.feature.FeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import sun.misc.IOUtils;

import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;

/**
 * Convert simple features to shape files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ShpFeatureFormatter implements SimpleFeatureFormatter {

    public static final MimeType mimeType = new MimeType("application/zip");

    public static final MimeType zipType = new MimeType("shape-zip");

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	protected byte[] buffer = new byte[1024 * 4];

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#format
     * (java.util.List, java.io.OutputStream)
     */
    @Override
    public void format(List<List<SimpleFeature>> features, OutputStream out)
            throws Exception {
        FeatureCollection<SimpleFeatureType, SimpleFeature> coll = convert(features);
        if (coll == null) {
            return;
        }
        File tmpDir = createTempDir();
        try {
            writeShape(tmpDir, coll);
            File zip = createZip(tmpDir);
            readFile(zip, out);
        } finally {
            if (tmpDir != null && tmpDir.exists()) {
                deleteDir(tmpDir);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#format
     * (java.util.List)
     */
	@Override
	public OgcResponse format(List<List<SimpleFeature>> features)
			throws Exception {
        FeatureCollection<SimpleFeatureType, SimpleFeature> coll = convert(features);
        if (coll == null) {
            return new OgcResponse(new byte[0], mimeType, TYPE.BYTE);
        }

		File tmpDir = createTempDir();
		try {
            writeShape(tmpDir, coll);
			File zip = createZip(tmpDir);
			byte[] bytes = readFile(zip);
			return new OgcResponse(bytes, mimeType, TYPE.BYTE);
		} finally {
			if (tmpDir != null && tmpDir.exists()) {
				deleteDir(tmpDir);
			}
		}
	}

    protected FeatureCollection<SimpleFeatureType, SimpleFeature> convert(
            List<List<SimpleFeature>> features) {
        List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls = new ArrayList<FeatureCollection<SimpleFeatureType, SimpleFeature>>(
                features.size());
        for (List<SimpleFeature> l : features) {
            if (l != null && !l.isEmpty()) {
                SimpleFeatureType t = l.get(0).getFeatureType();
                MemoryFeatureCollection c = new MemoryFeatureCollection(t);
                c.addAll(l);
                colls.add(c);
            }
        }
        // TODO handle multiple schemas
        if (colls.size() > 1) {
            log.error("Too many feature types sent to shapefile formatter");
        }
        if (colls.isEmpty()) {
            return null;
        }
        return colls.get(0);
    }

	protected File createZip(File dir) throws IOException {
		File rval = new File(dir, "res.zip");
		ZipOutputStream out = null;
		try {
			out = new ZipOutputStream(new FileOutputStream(rval));
			writeDirToZip(dir, rval, out);
		} finally {
			if (out != null) {
				out.close();
			}
		}
		return rval;
	}

	protected void writeDirToZip(File dir, File zip, ZipOutputStream out)
			throws IOException {
		for (File f : dir.listFiles()) {
			if (f.equals(zip)) {
				continue;
			}
			out.putNextEntry(new ZipEntry(f.getName()));
			FileInputStream in = null;
			try {
				in = new FileInputStream(f);
				copy(in, out);
			} finally {
				if (in != null) {
					in.close();
				}
				out.closeEntry();
			}
		}
	}

	protected void copy(InputStream in, OutputStream out) throws IOException {
		int read;
		while ((read = in.read(buffer)) != -1) {
			out.write(buffer, 0, read);
		}
	}

	protected void deleteDir(File dir) {
		File[] files = dir.listFiles();
		for (File f : files) {
			f.delete();
		}
		dir.delete();
	}

	protected static File createTempDir() throws Exception {
		File sysTmp = new File(System.getProperty("java.io.tmpdir"));
		String tstamp = "wfs" + System.currentTimeMillis() + "-";
		for (int i = 0; i < 100; ++i) {
			File rval = new File(sysTmp, tstamp + i);
			if (rval.mkdir()) {
				return rval;
			}
		}
		throw new Exception("Unable to create temp directory");
	}

	protected byte[] readFile(File f) throws FileNotFoundException, IOException {
		return IOUtils.readFully(new FileInputStream(f), -1, true);
	}
	
    protected void readFile(File f, OutputStream out) throws IOException {
        FileInputStream fin = new FileInputStream(f);
        try {
            copy(fin, out);
        } finally {
            fin.close();
        }
    }

	protected void writeShape(File dir,
			FeatureCollection<SimpleFeatureType, SimpleFeature> coll)
			throws Exception {
		SimpleFeatureType schema = coll.getSchema();
		ShapefileDataStoreFactory dataStoreFactory = new ShapefileDataStoreFactory();
		File f = new File(dir, schema.getTypeName() + ".shp");
		Map<String, Serializable> params = new HashMap<String, Serializable>();
		params.put("url", f.toURI().toURL());
		params.put("create spatial index", Boolean.TRUE);
		ShapefileDataStore newDataStore = (ShapefileDataStore) dataStoreFactory
				.createNewDataStore(params);
		newDataStore.createSchema(schema);
		Transaction transaction = new DefaultTransaction("create");
		String typeName = newDataStore.getTypeNames()[0];
		FeatureSource<SimpleFeatureType, SimpleFeature> featureSource = newDataStore
				.getFeatureSource(typeName);
		if (featureSource instanceof FeatureStore) {
			FeatureStore<SimpleFeatureType, SimpleFeature> featureStore = (FeatureStore<SimpleFeatureType, SimpleFeature>) featureSource;

			featureStore.setTransaction(transaction);
			try {
				featureStore.addFeatures(coll);
				transaction.commit();

			} catch (Exception problem) {
				transaction.rollback();
				throw problem;
			} finally {
				transaction.close();
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#getMimeType()
	 */
	@Override
    public MimeType getMimeType() {
		return mimeType;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#matchesFormat
	 * (java.lang.String)
	 */
	@Override
    public boolean matchesFormat(MimeType format) {
        if (mimeType.equalsIgnoreParams(format)) {
			return true;
		}
        if (format.equalsIgnoreParams(zipType)) {
			return true;
		}
		return false;
	}

}
